(ns cryptopals.core)

;; Challenge 1

(defn hex-decode
  "Converts a string of hexadecimal numbers to bytes"
  [hs]
  (byte-array (map (comp unchecked-byte #(Integer/parseInt % 16) #(apply str %))
                   (partition 2 hs))))

(def ^java.util.Base64$Encoder base64-encoder (java.util.Base64/getEncoder))

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

(def ^java.util.Base64$Decoder base64-decoder (java.util.Base64/getDecoder))

(defn base64-decode
  "Converts base64 string to bytes"
  [bs]
  (.decode base64-decoder (bytes bs)))

;; Challenge 2

(defn fixed-xor
  "xor two same-sized buffers"
  [bs1 bs2]
  (byte-array (map (comp unchecked-byte
                         #(bit-xor (Byte/toUnsignedLong %1)
                                   (Byte/toUnsignedLong %2)))
                   bs1 bs2)))


