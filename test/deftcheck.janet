# -*- mode: janet; -*-
# deft tests: checking

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* deftcheck")

# correct code — no errors
(deftcheck
  (deftfn ok [x :number] :number x)
  (deftfn ok2 [s :string] :string (string s "!")))

(cassert "deftcheck clean" true true)

# return type mismatch caught
(def dc-err1 (check-form '(deftfn bad [x :number] :string x)))
(cassert "deftcheck ret mismatch" (> (length dc-err1) 0) true)

# arg type misuse caught
(def dc-err2 (check-form '(deftfn bad [x :number] :number (string x))))
(cassert "deftcheck arg misuse" (> (length dc-err2) 0) true)

# fn contract arg usage
(def dc-err3 (check-form '(deftfn bad [f (:fn [:number -> :number])] :number (f "wrong"))))
(cassert "deftcheck fn contract" (> (length dc-err3) 0) true)

# type narrowing in if branches — caught
(def dc-err4 (check-form '(deftfn bad [x :number] :number (if (string? x) 1 0))))
(cassert "deftcheck narrows conflict" (> (length dc-err4) 0) true)

# type narrowing in if branches — clean
(def dc-err5 (check-form '(deftfn ok [x :number] :number (if (number? x) x 0))))
(cassert "deftcheck narrows clean" (= (length dc-err5) 0) true)

(print-results)
