# -*- mode: janet; -*-
# deft tests: binding and scope checks for typed sequential let

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* lett — basic")

(defn add1 [x] (+ x 1))

(lett [x :number 10
            y :number (add1 x)]
  (cassert "sequential binding sees earlier bindings" y 11))

(lett [a :number 10]
  (cassert "single binding" a 10))

(lett [s :string "hello"]
  (cassert "string binding" s "hello"))

(cassert "empty bindings" (lett [] 42) 42)

(print "\n* lett — type safety")

(cassert-err "catches type mismatch"
  (lett [n :number "not-a-number"] n))

(cassert-err "catches cross-binding type mismatch"
  (lett [x :number 10
              y :string x] y))

(cassert-err "catches inner error in nested lett"
  (lett [] (lett [x :number "bad"] x)))

(cassert-err "catches nested cross-binding type mismatch"
  (lett [x :number 3]
    (lett [y :string x] x)))

(print "\n* lett — binding form validation")

(cassert-err "incomplete triple rejected"
  (lett [a :number] 1))

(cassert-err "dangling name rejected"
  (lett [a :number 10 b] 1))

(print "\n* lett — scoping")

(lett [x :number 5
            y :number (+ x 2)
            z :number (* x y)]
  (cassert "x=5" x 5)
  (cassert "y=7" y 7)
  (cassert "z=35" z 35))

(var shadowed 99)
(lett [shadowed :number 200]
  (cassert "shadows outer var" shadowed 200))
(cassert "outer var unchanged" shadowed 99)

(print "\n* lett — with deftfn")

(deftfn square [n :number] :number (* n n))

(lett [n :number 4
            sq :number (square n)]
  (cassert "calls deftfn inside lett" sq 16))

(print "\n* lett — compound types")

(deftype :positive (fn [v] (and (number? v) (> v 0))))

(cassert "accepts positive" (lett [p :positive 5] p) 5)

(cassert-err "rejects non-positive"
  (lett [p :positive -1] p))

(print "\n* lett — tuple syntax and gradual typing")

(lett [(x :number 10)
            (y "hello")]
  (cassert "tuple syntax typed" x 10)
  (cassert "tuple syntax untyped defaults to :any" y "hello"))

(lett [(a :number 5)
            (b :number (* a 2))]
  (cassert "tuple syntax sequential scoping" b 10))

(cassert-err "tuple syntax catches type error"
  (lett [(x :number "bad")] x))

(print-results)
