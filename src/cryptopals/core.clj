(ns cryptopals.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.csv :as csv]))

(def byte->char unchecked-char)
(def char->byte (comp unchecked-byte int))

(defn bytes->string [bs]
  (apply str (map byte->char bs)))
(defn string->bytes [s]
  (byte-array (map char->byte s)))


(defn hex-decode
  "Converts a string of hexadecimal numbers to bytes"
  [hs]
  (byte-array (quot (count hs) 2)
              (map (comp unchecked-byte #(Integer/parseInt % 16) #(apply str %))
                   (partition 2 hs))))

(defn- left0
  "Adds 0 on the left of a single-digit string, since a byte should always be two hex digits"
  [s]
  (if (= (count s) 1) (str \0 s) s))

(defn hex-encode
  "Converts bytes to string of hexadecimal numbers"
  [bs]
  (apply str (map (comp left0 #(Integer/toUnsignedString % 16) #(Byte/toUnsignedInt %)) bs)))



(def ^:private ^java.util.Base64$Encoder base64-encoder (java.util.Base64/getEncoder))
(def ^:private ^java.util.Base64$Decoder base64-decoder (java.util.Base64/getDecoder))

(defn base64-encode
  "Converts bytes to base64 string"
  [bs]
  (.encodeToString base64-encoder bs))

(defn base64-decode
  "Converts base64 string to bytes"
  [s]
  (.decode base64-decoder s))


(defn bytes=
  "Compares two byte arrays for equality"
  [bs1 bs2]
  (= (seq bs1) (seq bs2)))



;; Challenge 1

(def hex->base64 (comp base64-encode hex-decode))

;; Challenge 2

(defn byte-xor [b1 b2]
  (unchecked-byte
    (bit-xor (Byte/toUnsignedLong b1)
             (Byte/toUnsignedLong b2))))

(defn fixed-xor
  "xor two buffers, |bs1| <= |bs2|"
  [bs1 bs2]
  (byte-array (count bs1) (map byte-xor bs1 bs2)))


;; Challenge 3

(def alphabet "abcdefghijklmnopqrstuvwxyz") 

(defn- sqr [x] (* x x))

(defn- map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn- map-length [m]
  (Math/sqrt (reduce + (map sqr (vals m)))))

(defn- map-norm [m]
  (let [l (map-length m)]
    (map-values #(/ % l) m)))

(def english-letter-frequencies 
  (map-norm
    {\space 22.864
     \a 	8.167
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
  (let [text (filter (set (keys english-letter-frequencies)) 
                     (string/lower-case text))
        char-freqs (frequencies text)]
    (dot-product english-letter-frequencies char-freqs)))

(defn rate-englishness-grams-normalized
  "Rates the Englishness of a piece of text by doing a mean square error of character freqs against standard freqs
  Lower is better."
  [text]
  (let [text (filter (set (keys english-letter-frequencies)) 
                     (string/lower-case text))
        char-freqs (map-norm (frequencies text))]
    (dot-product english-letter-frequencies char-freqs)))

;; Digrams work really well

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
  (or (get (set (map long "\t\n\r")) c)
      (<= 32 (long c) 127)))

(defn count-printable-chars [s]
  (count (filter printable-char? s)))

(defn rate-englishness-digrams
  [text]
  (let [digrams (filter valid-digram? (partition 2 1 (string/lower-case text))),
        digram-freqs (map-norm (frequencies digrams))]
    (dot-product english-digram-frequencies digram-freqs)))

(defn rate-englishness-sum [text]
  [(+ (rate-englishness-grams-normalized text)
      (rate-englishness-digrams text))
   (count-printable-chars text)])

(defn rate-englishness-if-printable-grams [text]
  (if (= (count-printable-chars text) (count text))
    (rate-englishness-grams text)
    0))

;; Back to Challenge 3

(defn single-byte-xor [bs b]
  (byte-array (map (partial byte-xor b) bs)))

(defn single-byte-xor-info [bs b]
  (let [result (bytes->string (single-byte-xor bs b))]
    [(rate-englishness-grams result) b result]))

(defn best-byte-xors [bs]
  (sort-by first >
           (map (partial single-byte-xor-info bs)
                (map byte (range -128 128)))))

(defn best-byte-xor [bs]
  (last (first (best-byte-xors bs))))

(defn best-byte-xor-the-byte [bs]
  (second (first (best-byte-xors bs))))

;; Challenge 4

(def single-char-xor-lines 
  (with-open [in-file (io/reader "resources/single-xor.txt")]
    (map hex-decode (string/split-lines (slurp in-file)))))

(defn best-single-char-xor-lines [byte-lines]
  (sort-by first #(compare %2 %1) (mapcat best-byte-xors byte-lines)))

(defn challenge4 []
  (last (first (best-single-char-xor-lines single-char-xor-lines))))

;; Challenge 5

(defn repeating-key-xor [k bs]
  (fixed-xor bs (cycle k)))

;; Challenge 6

(def challenge6-ciphertext
  (with-open [in-file (io/reader "resources/6.txt")]
    (base64-decode (string/replace (slurp in-file) "\n" ""))))

(def max-key-length 40)

(defn hamming-distance-byte [byte1 byte2]
  (Integer/bitCount (Byte/toUnsignedInt (bit-xor byte1 byte2))))

(defn hamming-distance [bs1 bs2]
  (reduce + (map hamming-distance-byte bs1 bs2)))

(defn average [l]
  (/ (apply + l) (count l)))

(defn keysize-edit-distance [bs keysize]
  (let [ks (vec (take 4 (partition keysize bs))),
        compare-keys (fn [[k1 k2]] (/ (hamming-distance k1 k2) keysize))]        
    (average (map compare-keys (for [i (range 4), j (range (inc i) 4)] [(ks i) (ks j)])))))

(defn most-likely-keysize [bs]
  (apply min-key (partial keysize-edit-distance bs) (range 2 (inc max-key-length))))
  

