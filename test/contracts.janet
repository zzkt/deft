# -*- mode: janet; -*-
# deft tests: function contracts

(import deft :prefix "")
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

(print "\n* keyword args: omitted kwargs default to nil (B6)")

# &keys args must behave like &opt args. when omitted bind nil.
(define kw-fn [x :number &keys {:f (:fn [:number -> :number])}] :number
  (if f (f x) -1))

(define kw-num [x :number &keys {:count :number}] :number
  (+ x (or count 0)))

(define kw-dyn [x :number &keys {:g :dynamic}] :number
  (+ x (or g 0)))

(cassert "fn kw provided" (kw-fn 2 :f |(+ $ 1)) 3)
(cassert "fn kw omitted defaults to nil" (kw-fn 2) -1)
(cassert "kw provided again after omitted" (kw-fn 2 :f |(+ $ 2)) 4)
(cassert-err "fn kw wrong type caught" (kw-fn 2 :f "nope"))
(cassert "num kw provided" (kw-num 2 :count 5) 7)
(cassert "num kw omitted defaults to nil" (kw-num 2) 2)
(cassert-err "num kw wrong type caught" (kw-num 2 :count "x"))
(cassert "dyn kw provided" (kw-dyn 2 :g 9) 11)
(cassert "dyn kw omitted" (kw-dyn 2) 2)

(print-results)
