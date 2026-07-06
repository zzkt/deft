# -*- mode: janet; -*-
# deft tests: values

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* deftval")

(deftval x :number 42)
(assert "immutable value" x 42)

(deftval s :string "hello")
(assert "string value" s "hello")

(deftval d :dynamic :anything-goes)
(assert "dynamic value" d :anything-goes)

(print "\n* deftv")

(deftv counter :number 0)
(++ counter)
(++ counter)
(assert "mutable counter" counter 2)

(deftv msg :string "")
(set msg "hi")
(assert "mutable string" msg "hi")

(print-results)
