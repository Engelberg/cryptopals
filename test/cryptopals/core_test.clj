(ns cryptopals.core-test
  (:require [clojure.test :refer :all]
            [cryptopals.core :refer :all]))

(deftest set1
  (testing "problem1"
    (is (= (hex->base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
           "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")))
  (testing "problem2"
    (is (= (hex-encode (fixed-xor (hex-decode "1c0111001f010100061a024b53535009181c")
                                  (hex-decode "686974207468652062756c6c277320657965")))
           "746865206b696420646f6e277420706c6179"))
    (is (bytes= (fixed-xor (hex-decode "1c0111001f010100061a024b53535009181c")
                           (hex-decode "686974207468652062756c6c277320657965"))
                (hex-decode "746865206b696420646f6e277420706c6179"))))
  (testing "problem3"
           (is (= (best-byte-xor (hex-decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
                  "Cooking MC's like a pound of bacon")))
  (testing "problem5"
           (is (= "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
                  (hex-encode (repeating-key-xor (string->bytes "ICE")
                                                 (string->bytes "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"))))))
  (testing "problem6"
           (is (= 37 (hamming-distance (string->bytes "this is a test") (string->bytes "wokka wokka!!!"))))))
               
