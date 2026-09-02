# -*- mode: janet; -*-
# deft tests: bidirectional inference

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* bidirectional inference")

(enable-inference true)

# arithmetic inference
(deftn inf-add [x y] (+ x y))
(cassert "inf-add" (inf-add 3 4) 7)
(cassert-err "inf-add catches bad arg" (inf-add "bad" 5))

# multi-arithmetic inference
(deftn inf-ops [a b] (+ (* a 2) (- b 1)))
(cassert "inf-ops" (inf-ops 5 3) 12)

# string inference
(deftn inf-cat [x y] (string x y))
(cassert "inf-cat strs" (inf-cat "a" "b") "ab")

# mixed inference: some typed, some not
(deftn inf-mix [a :string b] (string a b))
(cassert "inf-mix" (inf-mix "hi " "there") "hi there")

# if branches agree — inferred return type is the branch type
(deftn inf-if-num [x] (if (number? x) x 0))
(cassert "inf-if-num" (inf-if-num 5) 5)
(cassert-err "inf-if-num catches string" (inf-if-num "bad"))

# if branches disagree — degrades to :dynamic return
(deftn inf-if-mixed [x] (if x 1 "hello"))
(cassert "inf-if-mixed num" (inf-if-mixed true) 1)
(cassert "inf-if-mixed str" (inf-if-mixed nil) "hello")

# if without else — nil branch
(deftn inf-if-no-else [x] (when (number? x) x))
(cassert "inf-if-no-else nil" (inf-if-no-else nil) nil)
(cassert "inf-if-no-else num" (inf-if-no-else 42) 42)

# let binding inference
(deftn inf-let [x] (let [y (+ x 1)] (string y "!")))
(cassert "inf-let" (inf-let 41) "42!")
(cassert-err "inf-let catches non-num" (inf-let "bad"))

# def inside body
(deftn inf-def-body [a b]
  (def sum (+ a b))
  (string "sum=" sum))
(cassert "inf-def-body" (inf-def-body 3 4) "sum=7")
(cassert-err "inf-def-body catches non-num" (inf-def-body "a" "b"))

# multiple body forms (implicit progn)
(deftn inf-multi-body [x]
  (def doubled (+ x x))
  (string doubled))
(cassert "inf-multi-body" (inf-multi-body 21) "42")
(cassert-err "inf-multi-body catches non-num" (inf-multi-body "bad"))

# function composition via known ops
(deftn inf-comp [a b] (string (string a " ") b))
(cassert "inf-comp" (inf-comp "hello" "world") "hello world")
(cassert "inf-comp non-str" (inf-comp 1 2) "1 2")

# explicit annotation overrides inference
(deftn inf-override [x :number y] (string x " " y))
(cassert "inf-override" (inf-override 42 "ok") "42 ok")
(cassert-err "inf-override catches bad annotated" (inf-override "bad" "x"))

# do block
(deftn inf-do [x]
  (do (def tmp (+ x 1))
      (string tmp)))
(cassert "inf-do" (inf-do 9) "10")

(enable-inference false)

# inference off — no type checking on unannotated params
(deftn no-inf [x y] (+ x y))
(cassert "inf-off works" (no-inf 3 4) 7)

# no inference but still catches if type is explicit
(deftn no-inf-explicit [x :number y] (+ x y))
(cassert-err "explicit still caught" (no-inf-explicit "bad" 5))

(enable-inference true)

(print "\n* gradual degrade")

# param used in both number and string context → :dynamic
(deftn inf-ambiguous [x]
  (if (number? x) (+ x 1) (string x "!")))
(cassert "inf-ambiguous num" (inf-ambiguous 5) 6)
(cassert "inf-ambiguous str" (inf-ambiguous "hi") "hi!")

(print "\n* compose inference")

# inferred fn types through binding
(deftn inf-fn-var [f x]
  (def result (f x))
  result)
(cassert "inf-fn-var str" (inf-fn-var string 42) "42")

(print-results)
