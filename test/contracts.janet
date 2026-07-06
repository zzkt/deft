# -*- mode: janet; -*-
# deft tests: function contracts

(import ../deft :prefix "")
(import ./helper :prefix "")

(print "* function contracts")

# basic function contract
(deftfn apply [f (:fn [:number -> :number]) x :number] :number
  (f x))

(assert "apply works" (apply (fn [x] (+ x 1)) 5) 6)
(assert-err "apply catches bad arg" (apply (fn [x] (+ x 1)) "bad"))
(assert-err "apply catches non-fn" (apply "not-fn" 5))
(assert-err "apply catches bad ret" (apply (fn [x] "bad") 5))

# function contract via define
(define apply2 [f (:fn [:string -> :string]) x :string] :string
  (f x))

(assert "define apply works" (apply2 (fn [s] (string s "!")) "hi") "hi!")
(assert-err "define catches non-fn" (apply2 "not a function" "x"))

# multi-arg function contract
(define map2 [f (:fn [:number :number -> :number]) a :number b :number] :number
  (f a b))

(assert "map2 works" (map2 (fn [x y] (+ x y)) 3 4) 7)
(assert "map2 with short-fn" (map2 |(+ $0 $1) 3 4) 7)
(assert-err "map2 catches bad arg" (map2 (fn [x y] (+ x y)) "bad" 4))

# zero-arg function contract
(define thunk-fn [f (:fn [-> :number])] :number
  (f))

(assert "thunk works" (thunk-fn (fn [] 42)) 42)

(print-results)
