(ns cryptopals.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.csv :as csv]))

;; Challenge 1

(defn hex-decode
  "Converts a string of hexadecimal numbers to bytes"
  [hs]
  (byte-array (quot (count hs) 2)
              (map (comp unchecked-byte #(Integer/parseInt % 16) #(apply str %))
                   (partition 2 hs))))

(def ^:private ^java.util.Base64$Encoder base64-encoder (java.util.Base64/getEncoder))

(defn base64-encode
  "Converts bytes to base64 string"
  [bs]
  (.encodeToString base64-encoder bs))

(def hex->base64 (comp base64-encode hex-decode))

;; Other useful functions

(defn hex-encode
  "Converts bytes to string of hexadecimal numbers"
  [bs]
  (apply str (map (comp #(Integer/toUnsignedString % 16) #(Byte/toUnsignedInt %)) bs)))

(defn bytes=
  "Compares two byte arrays for equality"
  [bs1 bs2]
  (= (seq bs1) (seq bs2)))

(def ^:private ^java.util.Base64$Decoder base64-decoder (java.util.Base64/getDecoder))

(defn base64-decode
  "Converts base64 string to bytes"
  [bs]
  (.decode base64-decoder (bytes bs)))

;; Challenge 2

(defn byte-xor [b1 b2]
  (unchecked-byte
    (bit-xor (Byte/toUnsignedLong b1)
             (Byte/toUnsignedLong b2))))

(defn fixed-xor
  "xor two same-sized buffers"
  [bs1 bs2]
  (byte-array (map byte-xor bs1 bs2)))


;; Challenge 3

(def alphabet "abcdefghijklmnopqrstuvwxyz") 

(defn- sqr [x] (* x x))

(defn map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-length [m]
  (Math/sqrt (reduce + (map sqr (vals m)))))

(defn map-norm [m]
  (let [l (map-length m)]
    (map-values #(/ % l) m)))

(def english-letter-frequencies 
  (map-norm
    {\a 	8.167
     \b 	1.492
     \c 	2.782
     \d 	4.253
     \e 	12.702
     \f 	2.228
     \g 	2.015
     \h 	6.094
     \i 	6.966
     \j 	0.153
     \k 	0.772
     \l 	4.025
     \m 	2.406
     \n 	6.749
     \o 	7.507
     \p 	1.929
     \q 	0.095
     \r 	5.987
     \s 	6.327
     \t 	9.056
     \u 	2.758
     \v 	0.978
     \w 	2.361
     \x 	0.150
     \y 	1.974
     \z 	0.074}))

(defn- mse
  "Takes base-freqs and freqs maps and provides mean square error.
freqs may have fewer keys than base-freqs"
  [base-freqs freqs]
  (reduce + (for [[k v] base-freqs]
              (sqr (- v (get freqs k 0))))))

(defn- dot-product
  "Takes base-freqs and freqs maps and provides mean square error.
freqs may have fewer keys than base-freqs"
  [base-freqs freqs]
  (reduce + (for [[k v] base-freqs]
              (* v (get freqs k 0)))))

(defn rate-englishness-grams
  "Rates the Englishness of a piece of text by doing a mean square error of character freqs against standard freqs
  Lower is better."
  [text]
  (let [text (filter (set alphabet) (string/lower-case text))
        char-freqs (map-norm (frequencies text))]
    (dot-product english-letter-frequencies char-freqs)))

(def digram-data
  (let [str->long (fn [s] (if (= s "") 0 (Long/parseLong s)))]
    (mapv (partial mapv str->long)
          (with-open [in-file (io/reader "resources/digrams.csv")]
            (doall
              (csv/read-csv in-file))))))

(def english-digram-frequencies
  (map-norm
    (into {} (for [c1-int (range (long \a) (inc (long \z))),
                   c2-int (range (long \a) (inc (long \z)))]
               (let [c1-index (- c1-int (long \a))
                     c2-index (- c2-int (long \a)),
                     c1 (char c1-int),
                     c2 (char c2-int)]
                 [[c1 c2] ((digram-data c1-index) c2-index)])))))
        
(defn valid-digram? [[c1 c2]]
  (and (<= (long \a) (long c1) (long \z))
       (<= (long \a) (long c2) (long \z))))

(defn printable-char? [c]
  (<= 32 (long c) 127))

(defn count-printable-chars [s]
  (count (filter printable-char? s)))

(defn rate-englishness-digrams
  [text]
  (let [digrams (filter valid-digram? (partition 2 1 (string/lower-case text))),
        digram-freqs (map-norm (frequencies digrams))]
    (dot-product english-digram-frequencies digram-freqs)))

(defn rate-englishness [text]
  [(+ (rate-englishness-grams text)
      (rate-englishness-digrams text))
   (count-printable-chars text)])

(defn single-byte-xor [bs b]
  (byte-array (map (partial byte-xor b) bs)))

(defn bytes->string [bs]
  (apply str (map unchecked-char bs)))

(defn single-byte-xor-info [bs b]
  (let [result (bytes->string (single-byte-xor bs b))]
    [(rate-englishness result) b result]))

(defn best-byte-xors [bs]
  (sort-by first #(- (compare %1 %2))
           (map (partial single-byte-xor-info bs)
                (map byte (range -128 128)))))

(defn best-byte-xor [bs]
  (last (first (best-byte-xors bs))))



