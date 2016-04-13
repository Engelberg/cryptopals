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
                (hex-decode "746865206b696420646f6e277420706c6179")))))
