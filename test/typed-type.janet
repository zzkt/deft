# -*- mode: janet; -*-
# deft tests: type, type?

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* type?")

(cassert "core :number" (type? :number) true)
(cassert "core :string" (type? :string) true)
(cassert "core :dynamic" (type? :dynamic) true)
(cassert "core :mutable" (type? :mutable) true)
(cassert "core :immutable" (type? :immutable) true)
(cassert "core :nil" (type? :nil) true)
(cassert "core :boolean" (type? :boolean) true)
(cassert "core :keyword" (type? :keyword) true)
(cassert "core :table" (type? :table) true)
(cassert "core :array" (type? :array) true)
(cassert "non-type :nonexistent" (type? :nonexistent) false)
(cassert "non-type :42" (type? 42) false)
(cassert "non-type as string" (type? "hello") false)
(cassert "non-type as table" (type? @{}) false)

(deftype :positive (fn [v] (and (number? v) (> v 0))))
(cassert "user defined type" (type? :positive) true)

(deftype :nonzero (or :positive (define [v :number] (< v 0))))
(cassert "compound with typed predicate" (type? :nonzero) true)

(print "\n* type — deftval/deftv tagging")

(deftval n :number 42)
(cassert "deftval :number" (type n) :number)

(deftval s :string "hello")
(cassert "deftval :string" (type s) :string)

(deftval d :dynamic "the default")
(cassert "deftval :dynamic" (type d) :dynamic)

(deftv counter :number 0)
(++ counter)
(cassert "deftv :number" (type counter) :number)

(deftv buf :buffer (buffer ""))
(cassert "deftv :buffer" (type buf) :buffer)

(deftval three :positive 3)
(cassert "deftval user type" (type three) :positive)

(cassert-err "deftval rejects invalid" (deftval bad :positive -1))

(print "\n* type — fallback to core type")

(cassert "raw number" (type 99) :number)
(cassert "raw string" (type "raw") :string)
(cassert "raw keyword" (type :foo) :keyword)
(cassert "raw table" (type @{}) :table)
(cassert "raw array" (type @[]) :array)
(cassert "raw nil" (type nil) :nil)
(cassert "raw true" (type true) :boolean)
(cassert "raw false" (type false) :boolean)

(print "\n* type — no cross-contamination")

(deftval tagged :number 99)
(cassert "same value 99 tagged" (type tagged) :number)

(print "\n* type=")

(cassert "type= number true" (type= 42 :number) true)
(cassert "type= number false" (type= "hi" :number) false)
(cassert "type= string true" (type= "hi" :string) true)
(cassert "type= by tag" (type= three :positive) true)
(cassert "type= tag mismatch" (type= three :string) false)

(print "\n* isa?")

(cassert "isa? number" (isa? 42 :number) true)
(cassert "isa? string" (isa? "hi" :string) true)
(cassert "isa? number false" (isa? "hi" :number) false)
(cassert "isa? positive" (isa? 5 :positive) true)
(cassert "isa? positive false" (isa? 0 :positive) false)
(cassert "isa? nonzero pos" (isa? 5 :nonzero) true)
(cassert "isa? nonzero neg" (isa? -3 :nonzero) true)
(cassert "isa? nonzero false" (isa? 0 :nonzero) false)
(cassert "isa? :not :string on number" (isa? 33 '(:not :string)) true)
(cassert "isa? :not :string on string" (isa? "hi" '(:not :string)) false)

(print "\n* :type as a type")

(cassert ":type is a type" (type? :type) true)
(cassert "keyword type :number" (isa? :number :type) true)
(cassert "keyword type :string" (isa? :string :type) true)
(cassert "keyword type :positive" (isa? :positive :type) true)
(cassert "keyword type :dynamic" (isa? :dynamic :type) true)
(cassert "not a type keyword" (isa? :foobarbaz :type) false)
(cassert "not a type 42" (isa? 42 :type) false)
(cassert "not a type string" (isa? "hi" :type) false)
(cassert "predicate fn is type" (isa? number? :type) true)

(print "\n* type-name")
(cassert "number keyword" (type-name :number) :number)
(cassert "positive keyword" (type-name :positive) :positive)
(cassert "nil for fn" (type-name number?) nil)

(print "\n* registered-types")
(def types (registered-types))
(cassert "is table" (table? types) true)
(cassert "has :positive" (not= nil (get types :positive)) true)

(print-results)
