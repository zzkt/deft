# -*- mode: janet; -*-
# typed values, custom types, and checking

(import deft)

(deft/enable-checking true)

# deftval (immutable typed value)
(deft/deftval pi :number math/pi)
(deft/deftval name :string "alice")
(print "pi: " pi)
(print "name: " name)

# deftv (mutable typed value)
(deft/deftv counter :number 0)
(++ counter)
(++ counter)
(print "counter: " counter)

# deftype (custom type predicates)
(deft/deftype :positive (fn [v] (and (number? v) (> v 0))))
(deft/deftype :negative (fn [v] (and (number? v) (< v 0))))
(deft/deftype :nonzero (or :negative :positive))

(deft/deftfn pos-add [a :positive b :positive] :positive
  (+ a b))
(print "pos-add 3 4: " (pos-add 3 4))

(deft/deftfn divide [a :number b :nonzero] :number
  (/ a b))
(print "divide 10 2: " (divide 10 2))

# disable checking
(deft/enable-checking false)
(deft/deftfn unchecked [x :number] :number x)
(print "checking disabled, passes bad arg: " (unchecked "bad"))

# enable checking
(deft/enable-checking true)
(deft/deftfn checked [x :number] :number x)
(var err nil)
(try (checked "bad") ([e] (set err e)))
(print "checking enabled, catches bad arg: " (not= nil err))
