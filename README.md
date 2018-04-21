# word2prime

Encode word to prime number by "Illegal Prime" format.

## Usage

on REPL

```clojure:repl
(require '[word2prime.core :as wp])
(wp/encode "hello")
;=> 114784820031263N
(wp/decode 114784820031263N)
;=> "hello"

```

# Reference

[「違法素数」形式でハンドルネームをいい感じに素数化してみる - Qiita](https://qiita.com/deltam/items/c27b55be371803a43e63)


## License

MIT License
