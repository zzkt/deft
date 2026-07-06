# -*- mode: janet; -*-
# deft tests: enumeration types

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* defenum — creation")

# basic string→number
(defenum :colour {"red" 1 "green" 2 "blue" 3})

(assert "type :colour registered" (type? :colour) true)
(assert "enum-table returns map" (not= nil (enum-table :colour)) true)
(assert "enum-table has :red" (= 1 (in (enum-table :colour) "red")) true)
(assert "enum-table has :blue" (= 3 (in (enum-table :colour) "blue")) true)

# string→string map
(defenum :status {"active" "on" "inactive" "off" "unknown" "?"})

(assert "type :status registered" (type? :status) true)
(assert "status accessor" (status "active") "on")
(assert "status accessor fallback" (status "unknown") "?")

# string→any (mixed types)
(defenum :flags {"yes" true "no" false "count" 42})

(assert "flags accessor bool" (flags "yes") true)
(assert "flags accessor bool false" (flags "no") false)
(assert "flags accessor number" (flags "count") 42)

# single entry
(defenum :singleton {"only" 1})

(assert "singleton accessor" (singleton "only") 1)
(assert "singleton missing" (singleton "other") nil)
(assert "singleton predicate" ((type-predicate :singleton) "only") true)

# empty? shouldn't raise an error, just accept nothing
(defenum :empty {})

(assert "empty accessor nil" (empty "anything") nil)
(assert "empty predicate rejects" ((type-predicate :empty) "x") false)

(print "\n* defenum — generated accessor")

(assert "accessor colour exists" (function? colour) true)
(assert "accessor colour red" (colour "red") 1)
(assert "accessor colour green" (colour "green") 2)
(assert "accessor colour blue" (colour "blue") 3)
(assert "accessor colour missing" (colour "purple") nil)

# accessor as higher-order value
(assert "accessor map" (deep= @[1 2 3] (map (fn [k] (colour k)) ["red" "green" "blue"])) true)

(assert "accessor map red" (= 1 (colour (get ["red" "green" "blue"] 0))) true)
(assert "accessor map green" (= 2 (colour (get ["red" "green" "blue"] 1))) true)
(assert "accessor map blue" (= 3 (colour (get ["red" "green" "blue"] 2))) true)

# accessor in a function
(defn lookup-colour [k] (colour k))
(assert "accessor wrapped" (lookup-colour "red") 1)
(assert "accessor wrapped missing" (lookup-colour "brown") nil)

(print "\n* defenum — type predicate")

(assert "red accepted" ((type-predicate :colour) "red") true)
(assert "green accepted" ((type-predicate :colour) "green") true)
(assert "blue accepted" ((type-predicate :colour) "blue") true)
(assert "non-colour rejected" ((type-predicate :colour) "yellow") false)
(assert "number rejected" ((type-predicate :colour) 42) false)
(assert "nil rejected" ((type-predicate :colour) nil) false)
(assert "keyword rejected" ((type-predicate :colour) :red) false)

(print "\n* defenum — typed function guard")

(deftfn colour-code [c :colour] :number
  (in (enum-table :colour) c))

(assert "colour-code red" (colour-code "red") 1)
(assert "colour-code green" (colour-code "green") 2)
(assert "colour-code blue" (colour-code "blue") 3)

(assert-err "colour-code rejects invalid" (colour-code "infrapurple"))

# guard via define
(deftfn ccode-define [c :colour] :number
  (in (enum-table :colour) c))

(assert "define colour-code red" (ccode-define "red") 1)

# guard via deftval
(deftval got-colour :colour "red")
(assert "deftval enum guard" got-colour "red")

(assert-err "deftval enum rejects invalid" (deftval bad-colour :colour "infraviolet"))

(print "\n* defenum — runtime mutation")

# mutation: add new entry to the table
(put (enum-table :colour) "orange" 4)
(assert "accessor new entry" (colour "orange") 4)
(assert "type predicate new entry" ((type-predicate :colour) "orange") true)

# mutation: update existing entry
(put (enum-table :colour) "red" 99)
(assert "accessor updated value" (colour "red") 99)

# mutation: remove entry
(put (enum-table :colour) "orange" nil)
(assert "accessor removed entry" (colour "orange") nil)
(assert "predicate removed entry" ((type-predicate :colour) "orange") false)

# mutation: clear entire table
(defenum :temp {"a" 1 "b" 2})
(assert "temp accessor" (temp "a") 1)
(put (enum-table :temp) "a" nil)
(put (enum-table :temp) "b" nil)
(assert "temp cleared nil" (temp "a") nil)
(assert "temp cleared predicate" ((type-predicate :temp) "a") false)

(print "\n* defenum — generated extend and remove")

(defenum :extend-test {"x" 10 "y" 20})

(assert "extend-test-extend exists" (function? extend-test-extend) true)
(assert "extend-test-remove exists" (function? extend-test-remove) true)

# extend adds a new entry
(extend-test-extend "z" 30)
(assert "extend added value" (extend-test "z") 30)
(assert "extend predicate passes" ((type-predicate :extend-test) "z") true)

# extend returns the new value
(assert "extend return value" (= 30 (extend-test-extend "z" 30)) true)

# remove deletes an entry
(def removed (extend-test-remove "y"))
(assert "remove returns old value" (= removed 20) true)
(assert "remove clears accessor" (extend-test "y") nil)
(assert "remove clears predicate" ((type-predicate :extend-test) "y") false)

# remove on missing key returns nil
(assert "remove missing key" (extend-test-remove "nonexistent") nil)

(print "\n* defenum — enum-table identity and sharing")

# enum-table returns the same mutable table each time
(def t1 (enum-table :colour))
(def t2 (enum-table :colour))
(assert "enum-table identity" (= t1 t2) true)
(put t1 "test-key" "test-val")
(assert "enum-table mutation visible" (in t2 "test-key") "test-val")

(print "\n* defenum — multiple independent enums")

(defenum :size {"small" 0 "medium" 1 "large" 2})

(assert "type :size registered" (type? :size) true)
(assert "enum-table separate" (in (enum-table :size) "medium") 1)
(assert "colour still has red" (= 99 (in (enum-table :colour) "red")) true)

(print "\n* defenum — generated accessors are independent")

(assert "accessor size exists" (function? size) true)
(assert "accessor size medium" (size "medium") 1)
(assert "accessor colour still works" (colour "red") 99)

(print-results)
