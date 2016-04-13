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

(def english-letter-frequencies ; as percentages
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
   \z 	0.074})

(defn- sqr [x] (* x x))

(defn map-values [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-length [m]
  (Math/sqrt (reduce + (map sqr (vals m)))))

(defn map-norm [m]
  (let [l (map-length m)]
    (map-values m #(/ % l))))

(def english-letter-frequencies-normalized (map-norm english-letter-frequencies))

(def digram-data
  (let [str->long (fn [s] (if (= s "") 0 (Long/parseLong s)))]
    (mapv (partial mapv str->long)
          (with-open [in-file (io/reader "resources/digrams.csv")]
            (doall
              (csv/read-csv in-file))))))

(def total-digrams (double (reduce + (map (partial reduce +) digram-data))))

(def english-digram-frequencies ; as percentages
  (mapv (partial mapv #(* 100 (/ % total-digrams))) digram-data))

(defn valid-digram? [[c1 c2]]
  (and (<= (long \a) (long c1) (long \z))
       (<= (long \a) (long c2) (long \z))))

(defn get-digram-frequency [[c1 c2]]
  (let [c1-index (- (long (Character/toLowerCase c1)) (long \a)),
        c2-index (- (long (Character/toLowerCase c2)) (long \a))]
    (if (and (<= 0 c1-index 25) (<= 0 c2-index 25))
      (nth (nth digram-frequencies c1-index) c2-index)
      0)))



(defn- mse-english
  "Takes a map of character freqs and provides mean square error with standard freqs"
  [char-freqs]
  (reduce + (for [[character percentage] english-letter-frequencies]
              (sqr (- percentage (get char-freqs character 0))))))

(defn- mse-digrams-english
  "Takes a map of digram freqs and provides mean square error with standard digram freqs"
  [digram-freqs]
  (println digram-freqs)
  (reduce + (for [c1 (keys english-letter-frequencies),
                  c2 (keys english-letter-frequencies)]
              (sqr (- (digram-freqs [c1 c2] 0)
                      (get-digram-frequency [c1 c2]))))))

(defn rate-englishness-grams
  "Rates the Englishness of a piece of text by doing a mean square error of character freqs against standard freqs
  Lower is better."
  [text]
  (let [text (filter (set (keys english-letter-frequencies)) text)
        char-counts (frequencies (string/lower-case (apply str text))),
        text-length (double (count text)),
        char-freqs (into {} (for [[c n] char-counts] [c (* 100 (/ n text-length))]))]
    (mse-english char-freqs)))

(defn rate-englishness-digrams
  [text]
  (let [digrams (filter valid-digram? (partition 2 1 (string/lower-case text))),
        digram-counts (frequencies digrams),
        num-digrams (double (count digrams)),
        digram-freqs (into {} (for [[d n] digram-counts] [d (* 100 (/ n num-digrams))]))]
    (mse-digrams-english digram-freqs)))

(defn rate-englishness [text] (+ (rate-englishness-grams text)
                                 (rate-englishness-digrams text)))

(defn single-byte-xor [bs b]
  (byte-array (map (partial byte-xor b) bs)))

(defn single-byte-xor-info [bs b]
  (let [result (apply str (map unchecked-char (single-byte-xor bs b)))]
    [(rate-englishness result) b result]))

(defn best-byte-xors [bs]
  (sort-by first
           (map (partial single-byte-xor-info bs)
                (map byte (range -128 128)))))


(def encrypted (hex-decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))

