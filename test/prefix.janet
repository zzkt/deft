# -*- mode: janet; -*-
# deft tests: import prefix compatibility

(import ./helper :prefix "")
(import deft :as d)

(print "\n* (import deft :prefix \"d/\")")
(d/deftval x :number 42)
(assert "deftval with 'd' prefix" x 42)
(d/deftfn add [a :number b :number] :number (+ a b))
(assert "deftfn with 'd' prefix" (add 2 3) 5)

(print "\n* d sett")
(d/deftv count :number 0)
(d/sett count 10)
(assert "sett under 'd' prefix" count 10)

(print "\n* d lett")
(d/deftfn test-lett-fn [] (+ 0 (d/lett [a :number 1 b :number (+ a 1)] (+ a b))))
(assert "lett under 'd' prefix" (test-lett-fn) 3)

(print "\n* d define")
(d/define z :number 99)
(assert "define typed val under 'd' prefix" z 99)
(d/define mul [a :number b :number] :number (* a b))
(assert "define typed fn under 'd' prefix" (mul 3 4) 12)

(print "\n* d deftype")
(d/deftype :positive (fn [v] (and (number? v) (> v 0))))
(assert "type? under 'd' prefix" (d/type? :positive) true)
(assert "cast under 'd' prefix" (d/cast 5 :positive "test") 5)
(assert "isa? under 'd' prefix" (d/isa? 5 :positive) true)

(print "\n* d deftrecord")
(d/deftrecord :prect (field x :number) (field y :number))
(def p (make-prect 1 2))
(assert "deftrecord accessor under 'd' prefix" (prect-x p) 1)
(assert "deftrecord cast under 'd' prefix" (d/cast p :prect "check") p)

(print "\n* d defenum")
(d/defenum :colour {"red" 1 "blue" 2})
(assert "type? colour under 'd' prefix" (d/type? :colour) true)
(assert "enum-table under 'd' prefix" ((d/enum-table :colour) "red") 1)
(assert "accessor colour under 'd' prefix" (colour "red") 1)
(assert "accessor colour blue" (colour "blue") 2)

(print "\n* d defenum, extend and remove")
(assert "colour-extend exists" (function? colour-extend) true)
(assert "colour-remove exists" (function? colour-remove) true)
(colour-extend "green" 3)
(assert "extend under 'd' prefix" (colour "green") 3)
(colour-remove "red")
(assert "remove under 'd' prefix" (colour "red") nil)

(print-results)
