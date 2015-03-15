(ns vigenere.core)
  (:use 'clojure.string)
; cryptography principle assignments homework-1
; vigenere cipher.
; Author: jasonlvhit

(defn to-num [char] (- (int char) (int \A)))

(defn from-num [num] (char (+ (mod num 26) (int \A))))

(defn to-normalized-seq [str]
  (map #'first (re-seq #"[A-Z]" (clojure.string/upper-case str))))

(defn crypt1 [op text key]
  (from-num (apply op (list (to-num text) (to-num key)))))

(defn crypt [op text key]
  (let [xcrypt1 (partial #'crypt1 op)]
    (apply #'str
           (map xcrypt1 (to-normalized-seq text)
                (cycle (to-normalized-seq key))))))

(defn encrypt [plaintext key] (crypt #'+ plaintext key))

(defn decrypt [ciphertext key] (crypt #'- ciphertext key))

(defn e [ko]
  (/ (- 0.067 0.0385) (- ko 0.0385)))

(defn map-vals [m f]
  (zipmap (keys m) (map f (vals m))))

(defn pow [x]
  (* x x))

(defn freq [text start step]
  (frequencies
    (for [i (range start (count text) step)]
      (get text i))))

(defn get-ko [text step]
  (reduce + (vals (map-vals (freq text 0 step)
                            (fn [x] (pow (/ x (/ (count text) step))))))))

(defn e-iter [text]
  (first (apply max-key second
                (map-indexed vector (for [i (range 1 27)]
                                      (get-ko text i))))))

(defn get-max [M start step]
  (key (apply max-key val (freq M start step))))

(defn get-shifted [x]
  (- (to-num x)
     (to-num (get (char-array "E") 0))))

(defn get-key [M step]
  (for [i (range 0 step)]
    (from-num
      (get-shifted
        (get-max M i step)))))

;(println (e (get-ko (encrypt (slurp "plaintext.txt") "ABC")  1)))
;(println (get-key (encrypt (slurp "plaintext.txt") "ABC") 3))
;(println (e-iter (encrypt (slurp "plaintext.txt") "AAA")))
;(println (for [i (range 1 27)] (get-ko (encrypt (slurp "plaintext.txt") "ABCC") i)))
