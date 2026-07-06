# -*- mode: janet; -*-
# deft tests: type, type?

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* type?")

(assert "core :number" (type? :number) true)
(assert "core :string" (type? :string) true)
(assert "core :dynamic" (type? :dynamic) true)
(assert "core :mutable" (type? :mutable) true)
(assert "core :immutable" (type? :immutable) true)
(assert "core :nil" (type? :nil) true)
(assert "core :boolean" (type? :boolean) true)
(assert "core :keyword" (type? :keyword) true)
(assert "core :table" (type? :table) true)
(assert "core :array" (type? :array) true)
(assert "non-type :nonexistent" (type? :nonexistent) false)
(assert "non-type :42" (type? 42) false)
(assert "non-type as string" (type? "hello") false)
(assert "non-type as table" (type? @{}) false)

(deftype :positive (fn [v] (and (number? v) (> v 0))))
(assert "user defined type" (type? :positive) true)

(deftype :nonzero (or :positive (define [v :number] (< v 0))))
(assert "compound with typed predicate" (type? :nonzero) true)

(print "\n* type — deftval/deftv tagging")

(deftval n :number 42)
(assert "deftval :number" (type n) :number)

(deftval s :string "hello")
(assert "deftval :string" (type s) :string)

(deftval d :dynamic "the default")
(assert "deftval :dynamic" (type d) :dynamic)

(deftv counter :number 0)
(++ counter)
(assert "deftv :number" (type counter) :number)

(deftv buf :buffer (buffer ""))
(assert "deftv :buffer" (type buf) :buffer)

(deftval three :positive 3)
(assert "deftval user type" (type three) :positive)

(assert-err "deftval rejects invalid" (deftval bad :positive -1))

(print "\n* type — fallback to core type")

(assert "raw number" (type 99) :number)
(assert "raw string" (type "raw") :string)
(assert "raw keyword" (type :foo) :keyword)
(assert "raw table" (type @{}) :table)
(assert "raw array" (type @[]) :array)
(assert "raw nil" (type nil) :nil)
(assert "raw true" (type true) :boolean)
(assert "raw false" (type false) :boolean)

(print "\n* type — no cross-contamination")

(deftval tagged :number 99)
(assert "same value 99 tagged" (type tagged) :number)

(print "\n* type=")

(assert "type= number true" (type= 42 :number) true)
(assert "type= number false" (type= "hi" :number) false)
(assert "type= string true" (type= "hi" :string) true)
(assert "type= by tag" (type= three :positive) true)
(assert "type= tag mismatch" (type= three :string) false)

(print "\n* isa?")

(assert "isa? number" (isa? 42 :number) true)
(assert "isa? string" (isa? "hi" :string) true)
(assert "isa? number false" (isa? "hi" :number) false)
(assert "isa? positive" (isa? 5 :positive) true)
(assert "isa? positive false" (isa? 0 :positive) false)
(assert "isa? nonzero pos" (isa? 5 :nonzero) true)
(assert "isa? nonzero neg" (isa? -3 :nonzero) true)
(assert "isa? nonzero false" (isa? 0 :nonzero) false)
(assert "isa? :not :string on number" (isa? 33 '(:not :string)) true)
(assert "isa? :not :string on string" (isa? "hi" '(:not :string)) false)

(print "\n* :type as a type")

(assert ":type is a type" (type? :type) true)
(assert "keyword type :number" (isa? :number :type) true)
(assert "keyword type :string" (isa? :string :type) true)
(assert "keyword type :positive" (isa? :positive :type) true)
(assert "keyword type :dynamic" (isa? :dynamic :type) true)
(assert "not a type keyword" (isa? :foobarbaz :type) false)
(assert "not a type 42" (isa? 42 :type) false)
(assert "not a type string" (isa? "hi" :type) false)
(assert "predicate fn is type" (isa? number? :type) true)

(print "\n* type-name")
(assert "number keyword" (type-name :number) :number)
(assert "positive keyword" (type-name :positive) :positive)
(assert "nil for fn" (type-name number?) nil)

(print "\n* registered-types")
(def types (registered-types))
(assert "is table" (table? types) true)
(assert "has :positive" (not= nil (get types :positive)) true)

(print-results)
