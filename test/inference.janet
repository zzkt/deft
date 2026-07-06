# -*- mode: janet; -*-
# deft tests: bidirectional inference

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* bidirectional inference")

(enable-inference true)

# arithmetic inference
(deftn inf-add [x y] (+ x y))
(assert "inf-add" (inf-add 3 4) 7)
(assert-err "inf-add catches bad arg" (inf-add "bad" 5))

# multi-arithmetic inference
(deftn inf-ops [a b] (+ (* a 2) (- b 1)))
(assert "inf-ops" (inf-ops 5 3) 12)

# string inference
(deftn inf-cat [x y] (string x y))
(assert "inf-cat strs" (inf-cat "a" "b") "ab")

# mixed inference: some typed, some not
(deftn inf-mix [a :string b] (string a b))
(assert "inf-mix" (inf-mix "hi " "there") "hi there")

# if branches agree — inferred return type is the branch type
(deftn inf-if-num [x] (if (number? x) x 0))
(assert "inf-if-num" (inf-if-num 5) 5)
(assert-err "inf-if-num catches string" (inf-if-num "bad"))

# if branches disagree — degrades to :dynamic return
(deftn inf-if-mixed [x] (if x 1 "hello"))
(assert "inf-if-mixed num" (inf-if-mixed true) 1)
(assert "inf-if-mixed str" (inf-if-mixed nil) "hello")

# if without else — nil branch
(deftn inf-if-no-else [x] (when (number? x) x))
(assert "inf-if-no-else nil" (inf-if-no-else nil) nil)
(assert "inf-if-no-else num" (inf-if-no-else 42) 42)

# let binding inference
(deftn inf-let [x] (let [y (+ x 1)] (string y "!")))
(assert "inf-let" (inf-let 41) "42!")
(assert-err "inf-let catches non-num" (inf-let "bad"))

# def inside body
(deftn inf-def-body [a b]
  (def sum (+ a b))
  (string "sum=" sum))
(assert "inf-def-body" (inf-def-body 3 4) "sum=7")
(assert-err "inf-def-body catches non-num" (inf-def-body "a" "b"))

# multiple body forms (implicit progn)
(deftn inf-multi-body [x]
  (def doubled (+ x x))
  (string doubled))
(assert "inf-multi-body" (inf-multi-body 21) "42")
(assert-err "inf-multi-body catches non-num" (inf-multi-body "bad"))

# function composition via known ops
(deftn inf-comp [a b] (string (string a " ") b))
(assert "inf-comp" (inf-comp "hello" "world") "hello world")
(assert "inf-comp non-str" (inf-comp 1 2) "1 2")

# explicit annotation overrides inference
(deftn inf-override [x :number y] (string x " " y))
(assert "inf-override" (inf-override 42 "ok") "42 ok")
(assert-err "inf-override catches bad annotated" (inf-override "bad" "x"))

# do block
(deftn inf-do [x]
  (do (def tmp (+ x 1))
      (string tmp)))
(assert "inf-do" (inf-do 9) "10")

(enable-inference false)

# inference off — no type checking on unannotated params
(deftn no-inf [x y] (+ x y))
(assert "inf-off works" (no-inf 3 4) 7)

# no inference but still catches if type is explicit
(deftn no-inf-explicit [x :number y] (+ x y))
(assert-err "explicit still caught" (no-inf-explicit "bad" 5))

(enable-inference true)

(print "\n* gradual degrade")

# param used in both number and string context → :dynamic
(deftn inf-ambiguous [x]
  (if (number? x) (+ x 1) (string x "!")))
(assert "inf-ambiguous num" (inf-ambiguous 5) 6)
(assert "inf-ambiguous str" (inf-ambiguous "hi") "hi!")

(print "\n* compose inference")

# inferred fn types through binding
(deftn inf-fn-var [f x]
  (def result (f x))
  result)
(assert "inf-fn-var str" (inf-fn-var string 42) "42")

(print-results)
