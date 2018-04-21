(ns word2prime.core
  (:require [clojure.math.combinatorics :as comb])
  (:require [clojure.core.async :as ac])
  (:require [taoensso.tufte :as tf]))


(defn- gcd
  "最大公約数を返す"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn- coprime?
  "互いに素か？"
  [a b]
  (= 1 (gcd a b)))

(defn- prime?
  "素数か？"
  [n]
  (.isProbablePrime (biginteger n) 1000))

(defn- tail-bytes
  "末尾に追加するバイトの候補列を返す"
  [offset]
  (let [ascii-control-chars (cons 127 (range 32))]
    (if (<= offset 256)
      ascii-control-chars
      (for [a ascii-control-chars, b (tail-bytes (int (/ offset 256)))]
        (+ (* 256 b) a)))))

(defn- find-prime-seq
  "素数の候補シーケンスを返す"
  [data offset]
  (map #(+ (* data offset) %)
       (filter #(tf/p :coprime? (coprime? % data))
               (tail-bytes offset ))))

(defn str->bigint
  "文字列を整数化する"
  [s]
  (reduce #(+ (* %1 256) %2)
          (map #(bigint (int (char %))) s)))

(defn encode
  "指定したテキストをエンコードした素数を返す"
  [word]
  (let [num (str->bigint word)
        shift-seq (iterate #(* % 256N) 256N)
        find-seq  (mapcat #(find-prime-seq num %)
                          shift-seq)]
    (first (filter #(tf/p :prime? (prime? %)) find-seq))))

(defn decode
  "素数を文字列に直す"
  [prime]
  (let [prime-bytes (.toByteArray (biginteger prime))]
    (String. prime-bytes)))

(def alphabet
  "a-zのキャラクタ"
  (map char (range (int \a) (inc (int \z)))))

(defn n-letter-words
  "n文字の単語の組合せをすべてかえす"
  [n]
  (map #(apply str %)
       (comb/selections alphabet n)))

(def all-words
  "アルファベットを使うすべての単語の無限シーケンス"
  (let [nums (iterate inc 1)]
    (mapcat n-letter-words nums)))

(def five-word
  "アルファベット五文字以下の単語"
  (take-while #(<= (.length %) 5) all-words))

(defn make-word-list
  "五文字以下の英単語の素数エンコードリストをファイル出力する"
  [filename encode-words]
  (doseq [word encode-words]
    (let [prime (encode word)]
      (spit filename (str word " " prime "\n") :append true))))

(defn count-sel
  "n個のアルファベットの組み合わせ数をかえす"
  [n]
  (int (Math/pow 26 n)))

(defn count-all-sel
  "１〜n個のアルファベットの組合せ数をかえす"
  [n]
  (dec (/ (dec (int (Math/pow 26 (inc n))))
          (dec 26))))

(def elapsed-time "処理全体に掛かった時間(msec)" (atom 0))

(defn make-word-list-async
  "非同期に単語を素数化したリストを作りファイル出力する"
  [filename encode-words]
  (reset! elapsed-time 0)
  (let [start (System/currentTimeMillis)
        in-ch (ac/chan)
        out-ch (ac/chan)
        encode-xf (map #(let [ret [% (encode %)]]
                          (swap! elapsed-time max (- (System/currentTimeMillis) start))
                          ret))]
    (ac/pipeline 1000 out-ch encode-xf in-ch)
    (ac/go-loop []
      (when-let [[word prime] (ac/<! out-ch)]
        (spit filename (str word " " prime "\n") :append true)
        (recur)))
    (ac/onto-chan in-ch encode-words)))
