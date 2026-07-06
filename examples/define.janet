# -*- mode: janet; -*-

(import deft :prefix "")

# typed function
(define add [x :number y :number] :number (+ x y))
(print "add 2 3: " (add 2 3))

# mixed typed/untyped params (untyped default to :dynamic)
(define flex [a :number b c :string] :string
  (string (+ a b) " " c))
(print "flex 1 2 hi: " (flex 1 2 "hi"))

# untyped function (behaves like defn)
(define greet [name] (string "Hello, " name "!"))
(print "greet: " (greet "Arahant"))

# typed value (mutable)
(define pi :number 3.14159)
(print "pi: " pi)

# typed value (immutable)
(define pi (:and :number :immutable) math/pi)
(print "pi: " pi)

# untyped value (behaves like def)
(define x 42)
(print "x: " x)

# with a docstring
(define factorial
  "Compute n! recursively."
  [n :number] :number
  (if (<= n 1) 1 (* n (factorial (- n 1)))))
(print "factorial 5: " (factorial 5))
