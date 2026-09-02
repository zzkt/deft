# -*- mode: janet; -*-
# deft/pp: pretty-printing
#
# Pretty printer to handle user-defined types, falls back to built-in pp.

(var core-pp pp)

(defn pp-str
  "Pretty-print a value to a string, using :pp for records."
  [v]
  (cond
    (string? v) (string/format "%q" v)
    (keyword? v) (string v)
    (symbol? v) (string v)
    (buffer? v) (string/format "%q" (string v))
    (table? v)
    (let [f (get v :pp)]
      (if (function? f) (f v) (describe v)))
    (struct? v) (describe v)
    (array? v) (string "[" (string/join (map pp-str v) ", ") "]")
    (tuple? v) (string "(" (string/join (map pp-str v) ", ") ")")
    (nil? v) "nil"
    (boolean? v) (string v)
    (number? v) (string v)))

(defn pp :shadow
  "Pretty-print to stdout, using :pp for records."
  [x]
  (if (table? x)
    (let [f (get x :pp)]
      (if (function? f)
        (do (print (f x)) (flush))
        (core-pp x)))
    (core-pp x)))
