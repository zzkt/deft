# -*- mode: janet; -*-
# deft tests: enumeration types

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* defenum — creation")

# basic string→number
(defenum :colour {"red" 1 "green" 2 "blue" 3})

(cassert "type :colour registered" (type? :colour) true)
(cassert "enum-table returns map" (not= nil (enum-table :colour)) true)
(cassert "enum-table has :red" (= 1 (in (enum-table :colour) "red")) true)
(cassert "enum-table has :blue" (= 3 (in (enum-table :colour) "blue")) true)

# string→string map
(defenum :status {"active" "on" "inactive" "off" "unknown" "?"})

(cassert "type :status registered" (type? :status) true)
# Note: status is an unknown symbol at compile time (c.f. defenum)
(cassert "status accessor" (status "active") "on")
(cassert "status accessor fallback" (status "unknown") "?")

# string→any (mixed types)
(defenum :flags {"yes" true "no" false "count" 42})

(cassert "flags accessor bool" (flags "yes") true)
(cassert "flags accessor bool false" (flags "no") false)
(cassert "flags accessor number" (flags "count") 42)

# single entry
(defenum :singleton {"only" 1})

(cassert "singleton accessor" (singleton "only") 1)
(cassert "singleton missing" (singleton "other") nil)
(cassert "singleton predicate" ((type-predicate :singleton) "only") true)

# empty? shouldn't raise an error, just accept nothing
(defenum :empty {})

(cassert "empty accessor nil" (empty "anything") nil)
(cassert "empty predicate rejects" ((type-predicate :empty) "x") false)

(print "\n* defenum — generated accessor")

(cassert "accessor colour exists" (function? colour) true)
(cassert "accessor colour red" (colour "red") 1)
(cassert "accessor colour green" (colour "green") 2)
(cassert "accessor colour blue" (colour "blue") 3)
(cassert "accessor colour missing" (colour "purple") nil)

# accessor as higher-order value
(cassert "accessor map" (deep= @[1 2 3] (map (fn [k] (colour k)) ["red" "green" "blue"])) true)

(cassert "accessor map red" (= 1 (colour (get ["red" "green" "blue"] 0))) true)
(cassert "accessor map green" (= 2 (colour (get ["red" "green" "blue"] 1))) true)
(cassert "accessor map blue" (= 3 (colour (get ["red" "green" "blue"] 2))) true)

# accessor in a function
(defn lookup-colour [k] (colour k))
(cassert "accessor wrapped" (lookup-colour "red") 1)
(cassert "accessor wrapped missing" (lookup-colour "brown") nil)

(print "\n* defenum — type predicate")

(cassert "red accepted" ((type-predicate :colour) "red") true)
(cassert "green accepted" ((type-predicate :colour) "green") true)
(cassert "blue accepted" ((type-predicate :colour) "blue") true)
(cassert "non-colour rejected" ((type-predicate :colour) "yellow") false)
(cassert "number rejected" ((type-predicate :colour) 42) false)
(cassert "nil rejected" ((type-predicate :colour) nil) false)
(cassert "keyword rejected" ((type-predicate :colour) :red) false)

(print "\n* defenum — typed function guard")

(deftfn colour-code [c :colour] :number
  (in (enum-table :colour) c))

(cassert "colour-code red" (colour-code "red") 1)
(cassert "colour-code green" (colour-code "green") 2)
(cassert "colour-code blue" (colour-code "blue") 3)

(cassert-err "colour-code rejects invalid" (colour-code "infrapurple"))

# guard via define
(deftfn ccode-define [c :colour] :number
  (in (enum-table :colour) c))

(cassert "define colour-code red" (ccode-define "red") 1)

# guard via deftval
(deftval got-colour :colour "red")
(cassert "deftval enum guard" got-colour "red")

(cassert-err "deftval enum rejects invalid" (deftval bad-colour :colour "infraviolet"))

(print "\n* defenum — runtime mutation")

# mutation: add new entry to the table
(put (enum-table :colour) "orange" 4)
(cassert "accessor new entry" (colour "orange") 4)
(cassert "type predicate new entry" ((type-predicate :colour) "orange") true)

# mutation: update existing entry
(put (enum-table :colour) "red" 99)
(cassert "accessor updated value" (colour "red") 99)

# mutation: remove entry
(put (enum-table :colour) "orange" nil)
(cassert "accessor removed entry" (colour "orange") nil)
(cassert "predicate removed entry" ((type-predicate :colour) "orange") false)

# mutation: clear entire table
(defenum :temp {"a" 1 "b" 2})
(cassert "temp accessor" (temp "a") 1)
(put (enum-table :temp) "a" nil)
(put (enum-table :temp) "b" nil)
(cassert "temp cleared nil" (temp "a") nil)
(cassert "temp cleared predicate" ((type-predicate :temp) "a") false)

(print "\n* defenum — generated extend and remove")

(defenum :extend-test {"x" 10 "y" 20})

(cassert "extend-test-extend exists" (function? extend-test-extend) true)
(cassert "extend-test-remove exists" (function? extend-test-remove) true)

# extend adds a new entry
(extend-test-extend "z" 30)
(cassert "extend added value" (extend-test "z") 30)
(cassert "extend predicate passes" ((type-predicate :extend-test) "z") true)

# extend returns the new value
(cassert "extend return value" (= 30 (extend-test-extend "z" 30)) true)

# remove deletes an entry
(def removed (extend-test-remove "y"))
(cassert "remove returns old value" (= removed 20) true)
(cassert "remove clears accessor" (extend-test "y") nil)
(cassert "remove clears predicate" ((type-predicate :extend-test) "y") false)

# remove on missing key returns nil
(cassert "remove missing key" (extend-test-remove "nonexistent") nil)

(print "\n* defenum — enum-table identity and sharing")

# enum-table returns the same mutable table each time
(def t1 (enum-table :colour))
(def t2 (enum-table :colour))
(cassert "enum-table identity" (= t1 t2) true)
(put t1 "test-key" "test-val")
(cassert "enum-table mutation visible" (in t2 "test-key") "test-val")

(print "\n* defenum — multiple independent enums")

(defenum :size {"small" 0 "medium" 1 "large" 2})

(cassert "type :size registered" (type? :size) true)
(cassert "enum-table separate" (in (enum-table :size) "medium") 1)
(cassert "colour still has red" (= 99 (in (enum-table :colour) "red")) true)

(print "\n* defenum — generated accessors are independent")

(cassert "accessor size exists" (function? size) true)
(cassert "accessor size medium" (size "medium") 1)
(cassert "accessor colour still works" (colour "red") 99)

(print-results)
