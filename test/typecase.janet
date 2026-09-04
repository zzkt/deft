# -*- mode: janet; -*-
# deft tests: typecase / typecase-strict

(import ./helper :prefix "")
(import deft :prefix "")

(print "* typecase (using isa?)")

(cassert "typecase number"
         (typecase 42 :number "n" :string "str") "n")
(cassert "typecase string"
         (typecase "hi" :number "n" :string "str") "str")
(cassert "typecase tuple"
         (typecase [1 2] :number "n" :tuple "tup") "tup")

(cassert "typecase no match defaults to nil"
         (typecase true :number "n" :tuple "tup") nil)
(cassert "typecase nil value no match defaults to nil"
         (typecase nil :number "n" :tuple "tup") nil)

(print "\n* typecase-strict (using type=)")

(cassert "strict number"
         (typecase-strict 42 :number "n" :string "str") "n")
(cassert "strict string"
         (typecase-strict "hi" :number "n" :string "str") "str")
(cassert "strict tuple"
         (typecase-strict [1 2] :number "n" :tuple "tup") "tup")

(cassert "strict no match defaults to nil"
         (typecase-strict true :number "n" :tuple "tup") nil)

(print "\n* typecase value evaluated once")

(var counter 0)
(defn bump [] (++ counter) counter)
(cassert "val evaluated once"
         (typecase (bump) :number "n") "n")
(cassert "counter incremented once" counter 1)

(print "\n* typecase dispatch does not mutate receiver")

(def arr [1 2 3])
(typecase arr :string "str" :array "arr")
(cassert "array unchanged after dispatch" arr [1 2 3])

(print "\n* typecase vs typecase-strict: guard interaction")

(register-type :posint (fn [v] (number? v)))
(register-guard :posint (fn [v] (> v 0)))
(cassert "typecase accepts guard-passing value"
         (typecase 5 :posint "pos") "pos")
(cassert "typecase rejects guard-failing value"
         (typecase -5 :posint "pos") nil)
(cassert "strict ignores guard (type= is raw)"
         (typecase-strict -5 :posint "pos") nil)

(print "\n* typecase default finaliser is nil")

(cassert "default finaliser returns nil on no match (typecase)"
         (typecase true :number "n" :tuple "tup") nil)
(cassert "default finaliser returns nil on no match (typecase-strict)"
         (typecase-strict true :number "n" :tuple "tup") nil)

(print "\n* typecase finaliser")

(cassert "finaliser called with unmatched value (typecase)"
         (typecase true :number "n" :tuple "tup" (fn [x] (string "final:" x))) "final:true")
(cassert "finaliser not called on match (typecase)"
         (typecase 42 :number "n" :tuple "tup" (fn [x] (string "final:" x))) "n")

(cassert "finaliser called with unmatched value (typecase-strict)"
         (typecase-strict true :number "n" :tuple "tup" (fn [x] (string "final:" x))) "final:true")
(cassert "finaliser not called on match (typecase-strict)"
         (typecase-strict 42 :number "n" :tuple "tup" (fn [x] (string "final:" x))) "n")

(cassert "finaliser receives the value (typecase)"
         (typecase :nope :number "n" :string "s" (fn [x] [x])) [:nope])
(cassert "finaliser with multiple clauses (typecase-strict)"
         (typecase-strict :nope :number "n" :string "s" (fn [x] [x])) [:nope])
(cassert "single clause with finaliser (typecase)"
         (typecase true :number "n" (fn [x] "off")) "off")
(cassert "single clause with match (typecase)"
         (typecase 1 :number "n" (fn [x] "off")) "n")

(print-results)
