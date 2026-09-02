# -*- mode: janet; -*-
# deft tests: function contracts

(import ../deft :prefix "")
(import ./helper :prefix "")

(print "* function contracts")

# basic function contract
(deftfn apply2 [f (:fn [:number -> :number]) x :number] :number
  (f x))

(cassert "apply2 works" (apply2 (fn [x] (+ x 1)) 5) 6)
(cassert-err "apply2 catches bad arg" (apply2 (fn [x] (+ x 1)) "bad"))
(cassert-err "apply2 catches non-fn" (apply2 "not-fn" 5))
(cassert-err "apply2 catches bad ret" (apply2 (fn [] "bad") 5))

# function contract via define
(define apply3 [f (:fn [:string -> :string]) x :string] :string
  (f x))

(cassert "define apply works" (apply3 (fn [s] (string s "!")) "hi") "hi!")
(cassert-err "define catches non-fn" (apply3 "not a function" "x"))

# multi-arg function contract
(define map2 [f (:fn [:number :number -> :number]) a :number b :number] :number
  (f a b))

(cassert "map2 works" (map2 (fn [x y] (+ x y)) 3 4) 7)
(cassert "map2 with short-fn" (map2 |(+ $0 $1) 3 4) 7)
(cassert-err "map2 catches bad arg" (map2 (fn [x y] (+ x y)) "bad" 4))

# zero-arg function contract
(define thunk-fn [f (:fn [-> :number])] :number
  (f))

(cassert "thunk works" (thunk-fn (fn [] 42)) 42)

(print-results)
