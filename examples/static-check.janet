# -*- mode: janet; -*-
# Static analysis with deftcheck

(import deft :prefix "")

# passes. all types consistent
(deftcheck
  (deftfn safe-div [a :number b :number] :number (/ a b))
  (deftfn greet [name :string] :string (string "Hello, " name)))

(print "consistent forms pass")

# typed, caught: return declared :string, expression yields :number
(print "return mismatch errors: " (deft/check-form '(deftfn bad-ret [x :number] :string (+ x 1))))

# typed. caught: arg s declared :string but used in arithmetic
(print "arg misuse errors: " (deft/check-form '(deftfn bad-add [s :string] :number (+ 3 s))))

# untyped, no annotations, so nothing to check (passes)
(print "untyped form errors: " (deft/check-form '(defn fine [s] (+ 3 s))))
