# -*- mode: janet; -*-
# function-contracts.janet — function contracts in deft
#
# Three mechanisms
#   deftfn — arg and return types checked at runtime
#   :fn types — function valued arguments carry a contract
#   :dynamic return type — the return type depends on arg types

(import deft :as deft)

#  #  # # #       #   #      #    #              #
#  Basic function contracts
#
#  deftfn generates a contract: each argument is cast to its
#  declared type, and the return value is cast to the declared
#  return type. A mismatch raises a typed error with blame.
# #   # # #      #

(deft/define add [x :number y :number] :number
  (+ x y))

(print "Basic contracts")
(print "  add 2 3: " (add 2 3))            # returns 5

(var err nil)
(try (add 2 "x") ([e] (set err e)))
(print "  add 2 \"x\": " err)
# → Type error (add:y (caller blamed)): expected :number, got "x"


# #  #  # # #       #   #      #    #              #
#  Higher-order contracts (:fn type)
#
#  When a deftfn declares an argument as (:fn [args...] {-> | :->} ret),
#  that argument is wrapped with a contract. The wrapper checks
#  every call: arguments against declared arg-types, return value
#  against declared return type.
#
#  Both -> (bare symbol) and :-> (keyword) are accepted as the
#  separator between arguments and return type inside :fn.
#
##  #  #   #     #  #   #

(print "\nHigher-order contracts")

(deft/define apply-twice
  [f (:fn [:number] -> :number)  #  <- contract
   x :number]                    #  arg type
  :number                        #  return tyoe
  (f (f x)))

(deft/define inc [x] (+ x 1))

# The :fn contract wraps inc which takes and returns :number

(print "  apply-twice inc 5: " (apply-twice inc 5))    # returns 7

# If inc were called with a non-number inside apply-twice,
# the contract wrapper would catch it.

(deft/define wonky [x] (string x "!"))

(set err nil)
(try (apply-twice wonky 5)
     ([e] (set err e)))
(print "  apply-twice wonky 5: " err)

# → Type error (apply-twice:f:ret (function blamed)):
#     expected :number, got "5!"

# The contract also catches argument type mismatches on the
# contracted function. inc expects :number, so passing a :string
# fails at the contract boundary:

(set err nil)
(try (apply-twice inc "x")
     ([e] (set err e)))
(print "  apply-twice inc \"x\": " err)
# → Type error (apply-twice:f:arg0 (caller blamed)):
#     expected :number, got "x"


# # #  #  #    #  #   #      #              #
#  Returning functions with contracts
#
#  Return type can also be :fn. The returned function carries
#  a contract that travels with it.
#
# # #  #  #    #  #   #

(deft/deftfn make-adder
  [n :number]
  (:fn [:number] -> :number)       # using -> (bare symbol)
  (fn [x] (+ x n)))

(def add5 (make-adder 5))

(print "\n=== Part 3: returning contracted functions ===")
(print "  (make-adder 5) 3: " (add5 3))          # → 8

# The contract on add5 rejects non-number arguments
(set err nil)
(try (add5 "x") ([e] (set err e)))
(print "  add5 \"x\": " err)
# → Type error (make-adder return (function blamed):arg0 (caller blamed)):
#     expected :number, got "x"


###  #  #    #          #        #  #           #
#  Dynamic return type (aspirational Π-style)
#
#  Currently deft uses static return type annotations. A more
#  expressive pattern with the return type computed from argument
#  values is the Π type. An approximation could use an :or type
#  with runtime dispatch. Implmenting as a function contract is
#  left as an exercise for the reader...
#
# #  ##    #  #     #    #

(deft/define parse-value
  [s :string kind :keyword]
  (:or :number :string :keyword)
  (case kind
    :int (scan-number s)
    :str s
    :kw (keyword s)
    (error "unknown kind")))


(print "\nDynamic return type")

(set err nil)
(print "  parse-value \"42\" :int: " (parse-value "42" :int))   # → 42
(print "  parse-value \"hi\" :str: " (parse-value "hi" :str))   # → "hi"

# The :or type accepts any of the three so caller must narrow if needed.


# # # #    #       #     #  #     #         #
#  Type computed at runtime
#
#  The return type of a function can be computed from its argument
#  values (not just argument types) by building a type from data
#  and running an explicit cast
#
# # #  #   #    #   #      #

(print "\nRuntime type construction")

(defn predicate-table [data fields]
  (each [k expected-type] (pairs fields)
    (def actual (get data k))
    (deft/cast actual expected-type
      (string "field:" k)))
  data)

(defn check-schema [schema-name data]
  (def schemas
    {:person {:name :string
              :age :number}
     :book   {:title :string
              :author (or :person :people)
              :date :date
              :isbn :string
              :pages :number}})
  (def fields (get schemas schema-name))
  (unless fields (errorf "unknown schema: %q" schema-name))
  (predicate-table data fields))

# The schema is strctured data. A more useful example could
# build the schema dynamically (e.g. from a json file,
# db query, HTTP response, etc) and call cast with the
# resulting type at runtime.

(print "  check-schema :person {:name \"Alice\" :age 48}: "
       (check-schema :person @{:name "Alice" :age 48}))

(set err nil)
(try (check-schema :person @{:name "Bob" :age "x"})
     ([e] (set err e)))
(print "  invalid age: " err)
# → Type error (field:age (caller blamed)): expected :number, got "x"

(set err nil)
(try (check-schema :person @{:name 42 :age 42})
     ([e] (set err e)))
(print "  invalid name: " err)
# → Type error (field:name (caller blamed)): expected :string, got 42
