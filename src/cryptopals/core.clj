(ns cryptopals.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [utils.cond :as u])
  (:import javax.crypto.Cipher javax.crypto.spec.SecretKeySpec))

(def byte->char unchecked-char)
(def char->byte (comp unchecked-byte int))

(defn bytes->string [bs]
  (apply str (map byte->char bs)))
(defn string->bytes [^String s]
  (.getBytes s))

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
  (.decode base64-decoder ^String s))


(defn bytes=
  "Compares two byte arrays for equality"
  [bs1 bs2]
  (= (seq bs1) (seq bs2)))



;; Challenge 1

(def hex->base64 (comp base64-encode hex-decode))

;; Challenge 2

(defn byte-xor 
  ([b1 b2]
    (unchecked-byte
      (bit-xor (Byte/toUnsignedLong b1)
               (Byte/toUnsignedLong b2))))
  ([b1 b2 & bs]
    (reduce byte-xor (cons b1 (cons b2 bs)))))

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
  (let [n 4,
        ks (vec (take n (partition keysize bs))),
        compare-keys (fn [[k1 k2]] (/ (hamming-distance k1 k2) keysize))]        
    (average (map compare-keys (for [i (range n), j (range (inc i) n)] [(ks i) (ks j)])))))

(defn most-likely-keysize [bs]
  (apply min-key (partial keysize-edit-distance bs) (range 2 (inc max-key-length))))
  
(defn partition-into-every-nth [bs n]
  (let [p (partition-all n bs),
        size-of-last-partition (count (last p)),
        long-parts (apply map vector p),
        drop-processed (drop-last (map #(drop size-of-last-partition %) p))
        short-parts (apply map vector drop-processed)]
    (concat long-parts short-parts)))

(defn break-repeating-key "Returns [key message] byte streams" 
  [bs]
  (let [keysize (most-likely-keysize bs),
        p (partition-into-every-nth bs keysize),
        key (map best-byte-xor-the-byte p)]
    [key (repeating-key-xor key bs)]))

(defn crack-challenge6 []
  (map bytes->string (break-repeating-key challenge6-ciphertext)))

;; Challenge 7
                     
(def challenge7-ciphertext
  (with-open [in-file (io/reader "resources/7.txt")]
    (base64-decode (string/replace (slurp in-file) "\n" ""))))

(defn aes-ecb "Takes bs and key as bytes. bs must be byte array when decrypting"
  ([mode bs key] (aes-ecb mode bs key true)) ; pad by default
  ([mode bs key padding?]
    (let [bs (byte-array bs),
          aes-128-ecb-instance (Cipher/getInstance (format "AES/ECB/%sPadding" (if padding? "PKCS5" "No"))),
          secret-key-spec (SecretKeySpec. key "AES") 
          _ (.init aes-128-ecb-instance 
              (get {:encrypt Cipher/ENCRYPT_MODE, :decrypt Cipher/DECRYPT_MODE} mode)
              secret-key-spec)]
      (.doFinal aes-128-ecb-instance bs))))

(def aes-ecb-encode (partial aes-ecb :encrypt))
(def aes-ecb-decode (partial aes-ecb :decrypt))

(defn crack-challenge7 []
  (bytes->string (aes-ecb-decode challenge7-ciphertext (string->bytes "YELLOW SUBMARINE") true)))

;; Challenge 8

(def challenge8-ciphertexts 
  (with-open [in-file (io/reader "resources/8.txt")]
    (map hex-decode (string/split-lines (slurp in-file)))))

(defn aes-ecb? [bs]
  (let [blocks16 (partition 16 bs)]
    (not= (count blocks16) (count (distinct blocks16)))))

(defn crack-challenge8 []
  (hex-encode (first (filter aes-ecb? challenge8-ciphertexts))))

;; Challenge 9

(defn pkcs7-pad [bs block-length]
  (let [bs-length (count bs)]
    (if (>= bs-length block-length) bs
      (let [num-pad-chars (- block-length bs-length),
            num-pad-as-byte (unchecked-byte num-pad-chars)]
        (byte-array block-length (concat bs (repeat num-pad-chars num-pad-as-byte)))))))

(def ^:dynamic *throw-error-if-incorrect-padding* false)
(declare valid-padding?)

(defn pkcs7-unpad [bs]
  (when *throw-error-if-incorrect-padding* (valid-padding? bs))
  (let [bs-length (count bs),
        last-byte (last bs),
        num-pad (Byte/toUnsignedInt last-byte),
        last-bytes (take-last num-pad bs)]
    (if (= (set last-bytes) #{last-byte})
      (drop-last num-pad bs)
      bs)))

;; Challenge 10

(def empty-init-vector (byte-array 16 (repeat 16 (byte 0))))

(defn aes-cbc-encode 
  ([bs key]
    (aes-cbc-encode bs key empty-init-vector))
  ([bs key iv]       
    (when (seq bs)
      (let [block (take 16 bs),
            block (if (seq (drop 16 bs)) block (pkcs7-pad block 16))
            encoded-block (aes-ecb-encode (fixed-xor block iv) key false)]
        (lazy-cat encoded-block (aes-cbc-encode (drop 16 bs) key encoded-block))))))
                            
(defn aes-cbc-decode
  ([bs key]
    (aes-cbc-decode bs key empty-init-vector))
  ([bs key iv]
    (when (seq bs)
      (let [block (take 16 bs),
            decoded-block (fixed-xor iv (aes-ecb-decode block key false))
            decoded-block (if (seq (drop 16 bs)) decoded-block (pkcs7-unpad decoded-block))]
        (lazy-cat decoded-block (aes-cbc-decode (drop 16 bs) key block))))))

(def challenge10-ciphertext
  (with-open [in-file (io/reader "resources/10.txt")]
    (base64-decode (string/replace (slurp in-file) "\n" ""))))

(defn crack-challenge10 []
  (bytes->string (aes-cbc-decode challenge10-ciphertext (string->bytes "YELLOW SUBMARINE"))))
      
;; Challenge 11

(defn rand-bytes [n]
  (byte-array (repeatedly n #(unchecked-byte (rand-int 256)))))

(defn rand-aes-key [] (rand-bytes 16))

(defn aes-cbc-encode-rand-iv "Prepends iv onto ciphertext" [bs key]
  (let [iv (rand-bytes 16)]
    (concat iv (aes-cbc-encode bs key iv)))) 

(defn aes-cbc-decode-rand-iv "Assumes iv is prepended onto ciphertext" [bs key]
  (aes-cbc-decode (drop 16 bs) key (take 16 bs)))

(defn aes-encrypt-data-unknown-key [bs]
  (let [randomly-wrapped-bs (concat (rand-bytes (+ 5 (rand-int 6))) 
                                    bs
                                    (rand-bytes (+ 5 (rand-int 6)))),
        encoder (if (zero? (rand-int 2)) aes-cbc-encode-rand-iv aes-ecb-encode)]
    (if (= encoder aes-cbc-encode-rand-iv) (println "CBC") (println "ECB"))
    (encoder randomly-wrapped-bs (rand-aes-key))))

(defn aes-encryption-mode-detector [encryptor]
  (if (aes-ecb? (encryptor (repeat 64 0)))                  
    :ecb :cbc))

;; Challenge 12

(let [key (rand-aes-key)
      unknown-string (base64-decode "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK")]
  (defn aes-ecb-encrypt-data-unknown-but-consistent-key [bs]
    (let [bs-plus-unknown (concat bs unknown-string)]
      (aes-ecb-encode bs-plus-unknown key))))

(defn discover-block-size [encryptor]
  (let [[x y] (distinct
                (for [n (iterate inc 1)]
                  (count (encryptor (repeat n 0)))))]
    (- y x)))

(defn nth-block "0-based" [bs block-size n]
  (->> bs (drop (* block-size n)) (take block-size)))

(defn discover-byte [encryptor block-size discovered-bytes]
  (let [;print-encryptor (fn [i] (println i) (println (take block-size (encryptor i))) (encryptor i))
        n-discovered-bytes (count discovered-bytes),
        n-discovered-blocks (quot n-discovered-bytes block-size),
        n-leftover-bytes (rem n-discovered-bytes block-size),
        block-with-one-unknown (take-last (dec block-size) 
                                          (concat (repeat block-size 0) discovered-bytes)),
        prepend-bytes (repeat (- (dec block-size) n-leftover-bytes) 0),        
        dict (into {} (for [i (range -128 128) :let [b (byte i)]]
                        [(take block-size (encryptor (concat block-with-one-unknown [b]))) 
                         b]))]
    (-> (encryptor prepend-bytes)
      (nth-block block-size n-discovered-blocks)
      dict)))

(defn decrypt-unknown-string [encryptor]
  (when (= :ecb (aes-encryption-mode-detector encryptor))
    (let [block-size (discover-block-size encryptor)]
      (loop [discovered-bytes []]
        (let [next-byte (discover-byte encryptor block-size discovered-bytes)]
          (if (= next-byte (byte 1)) (bytes->string discovered-bytes)
            (recur (conj discovered-bytes next-byte))))))))

;; Challenge 13

(defn parse-kv [s]
  (let [pairs (str/split s #"&"),
        kv-pairs (for [pair pairs] (str/split pair #"="))]
    (into {} kv-pairs)))

(defn produce-kv [m]
  (str/join \&
    (for [[k v] m] (str k "=" v))))

(defn sanitize [s]
  (string/replace s #"[&=]" ""))

(defn profile-for "Takes email address" [s]
  (produce-kv
    {"email" (sanitize s),
     "uid" 10,
     "role" "user"}))

(let [key (rand-aes-key)]
  (defn encrypt-user-profile-for "Takes email address" [s]
    (aes-ecb-encode (string->bytes (profile-for s)) key))
  
  (defn decrypt-and-parse-profile "Takes encrypted profile" [e]
    (parse-kv (bytes->string (aes-ecb-decode e key)))))  

(defn crack-challenge13 []
  (let [user1 "max@gmail.com",
        user2 (format "mark@gmailadmin%s.com" (apply str (repeat 11 (char 11)))),
        [ct1 ct2] (map encrypt-user-profile-for [user1 user2])],
    (byte-array (concat (drop-last 16 ct1) (nth-block ct2 16 1)))))
      
;; Challenge 14

(let [key (rand-aes-key)
      unknown-target (base64-decode "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK")
      unknown-prefix (rand-bytes (inc (rand-int 100)))]
  (defn aes-ecb-encrypt-data-unknown-prefix [bs]
    (let [unknown-plus-bs (concat unknown-prefix bs unknown-target)]
      (aes-ecb-encode unknown-plus-bs key))))

(defn equal-adjacent-blocks "Returns index of first adjacent block" [bs]
  (first (keep-indexed (fn [index [x y]] (when (= x y) index)) (partition 2 1 (partition 16 bs)))))

(defn prefix-is-n-chars-short-of-a-block? 
  "Returns index of first adjacent block" 
  [encryptor block-size n]
  (let [block-fill 
        (fn [i] (equal-adjacent-blocks
                  (encryptor (concat (repeat n (byte 0)) 
                                     (repeat (* 2 block-size) (byte i))))))]
    ; Try 3 fills so we can't get spurious results by a matching byte on the boundary of the block
    (and (block-fill 1) (block-fill 2) (block-fill 3))))

(defn prefix-is-how-short-of-a-block 
  "Returns ordered pair of how much to pad the prefix and the first-attacker-block"
  [encryptor block-size]
  (first (for [i (range block-size) 
               :let [first-attacker-block (prefix-is-n-chars-short-of-a-block? encryptor block-size i)]
               :when first-attacker-block]               
           [i first-attacker-block])))

(defn discover-byte [encryptor block-size discovered-bytes]
  (let [n-discovered-bytes (count discovered-bytes),
        n-discovered-blocks (quot n-discovered-bytes block-size),
        n-leftover-bytes (rem n-discovered-bytes block-size),
        block-with-one-unknown (take-last (dec block-size) 
                                          (concat (repeat block-size 0) discovered-bytes)),
        prepend-bytes (repeat (- (dec block-size) n-leftover-bytes) 0),
        ;; New things for challenge 14
        [num-prefix-pad first-attacker-block-index] (prefix-is-how-short-of-a-block encryptor block-size) 
        prefix-pad (repeat num-prefix-pad 0)        
        dict (into {} (for [i (range -128 128) :let [b (byte i)]]
                        [(nth-block (encryptor (concat prefix-pad block-with-one-unknown [b]))
                                    block-size
                                    first-attacker-block-index) 
                         b]))]
    (-> (encryptor (concat prefix-pad prepend-bytes))
      (nth-block block-size (+ first-attacker-block-index n-discovered-blocks))
      dict)))

;; Challenge 15

(defn valid-padding? [bs]
  ;(println (take-last 16 bs))
  (let [last-byte (last bs)]
    (if (and last-byte (<= last-byte 16) (= (repeat last-byte last-byte) (take-last last-byte bs)))
      true
      (throw (RuntimeException. "Padding invalid")))))

;; Challenge 16

(let [s1 "comment1=cooking%20MCs;userdata=",
      s2 ";comment2=%20like%20a%20pound%20of%20bacon",
      key (rand-aes-key)]
  (defn encrypt-bitflipping [s]
    (aes-cbc-encode-rand-iv (string->bytes (str s1 (str/replace s #"[;=]" "") s2)) key))
  
  (defn admin-test-bitflipping [bs]
    (println (bytes->string (aes-cbc-decode-rand-iv bs key)))
    (boolean (re-seq #";admin=true;" (bytes->string (aes-cbc-decode-rand-iv bs key))))))

(defn crack-challenge16 []
  (let [ct (encrypt-bitflipping "xxxxxxxxxxxxxxxx"),
        block (nth-block ct 16 3),
        new-block (fixed-xor block (fixed-xor (string->bytes ";comment2=%20lik") (string->bytes ";admin=true;com="))),
        modified-ct (concat (take 48 ct) new-block (drop 64 ct))]
    (admin-test-bitflipping modified-ct)))

;; Challenge 17

(def cbc-padding-oracle-strings
  (map base64-decode
       ["MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
        "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
        "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
        "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
        "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
        "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
        "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
        "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
        "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
        "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]))

(let [key (rand-aes-key)]
  (defn cbc-padding-oracle-encrypt []
    (aes-cbc-encode-rand-iv (rand-nth cbc-padding-oracle-strings) key))
  
  (defn cbc-padding-oracle-decrypt [bs]    
    (binding [*throw-error-if-incorrect-padding* true]
      (try
        (do (doall (aes-cbc-decode-rand-iv bs key)) true)
        (catch RuntimeException e false)))))

(def byte-range (range -128 128))

(defn padding-attack-byte [bs discovered-bytes]
  (let [blocks (map vec (partition 16 bs)),
        num-discovered-bytes (count discovered-bytes),
        num-discovered-blocks (quot num-discovered-bytes 16),
        num-leftover-bytes (rem num-discovered-bytes 16),
        leftover-bytes (take num-leftover-bytes discovered-bytes),
        relevant-blocks (vec (drop-last num-discovered-blocks blocks))
        num-relevant-blocks (count relevant-blocks)
        penultimate-block-index (- num-relevant-blocks 2)
        penultimate-block (nth relevant-blocks penultimate-block-index),
        byte-to-discover-index (- 15 num-leftover-bytes),
        padding-byte (byte (inc num-leftover-bytes)),
        
        new-penultimate-block ; change end of block to padding byte 
        (into [] (for [i (range 16)]
                   (cond
                     (= i (dec byte-to-discover-index))
                     (byte-xor (byte -1) (penultimate-block i)), ; to protect against false matches
                     (<= i byte-to-discover-index)
                     (penultimate-block i),
                     :else
                     (byte-xor padding-byte
                               (penultimate-block i)
                               (nth leftover-bytes (- i (inc byte-to-discover-index))))))),
        
        penultimate-block-alterations ; [i block-alteration] pairs
        (for [i byte-range]
          [i (update new-penultimate-block 
                     byte-to-discover-index #(byte-xor % padding-byte (byte i)))])]
    
        (first (for [[i block-alteration] penultimate-block-alterations
                     :when (cbc-padding-oracle-decrypt 
                             (apply concat 
                                    (assoc relevant-blocks penultimate-block-index block-alteration)))]
                 i))))
                     
        
(defn padding-attack [bs]
  (loop [discovered-bytes ()]
    (if (<= (- (count bs) (count discovered-bytes)) 16)
      discovered-bytes
      (recur (cons (padding-attack-byte bs discovered-bytes) discovered-bytes)))))

(defn crack-challenge17 []
  (let [ct (cbc-padding-oracle-encrypt)]
    (bytes->string (pkcs7-unpad (padding-attack ct)))))
                
                        