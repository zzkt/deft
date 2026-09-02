# -*- mode: janet; -*-
# deft tests: values

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* deftval")

(deftval x :number 42)
(cassert "immutable value" x 42)

(deftval s :string "hello")
(cassert "string value" s "hello")

(deftval d :dynamic :anything-goes)
(cassert "dynamic value" d :anything-goes)

(print "\n* deftv")

(deftv counter :number 0)
(++ counter)
(++ counter)
(cassert "mutable counter" counter 2)

(deftv msg :string "")
(set msg "hi")
(cassert "mutable string" msg "hi")

(print-results)
