# -*- mode: janet; -*-
# deft/typecase: type-dispatch macros

# Two typecase macros using implicit type matching (isa?) and strict
# matching (type=) for type driven control flow. No simplification
# or rewriting of matching logic.
#
# see also: Strategies for typecase optimization, Jim Newton
# 11th European Lisp Symposium, April 2017.

(use ./core)

(defn- build-case
  ```Shared expansion for `typecase` and `typecase-strict`.
Clauses expand to a `(if (matcher v 'T) branch ...)` chain in source order.
```
  [pred-key name val clauses]
  (let [n (length clauses)
        clause-pairs (if (odd? n) (tuple/slice clauses 0 (- n 1)) clauses)
        finaliser (if (odd? n) (last clauses) nil)]
    (when (odd? (length clause-pairs))
      (error (string name " requires ':type body' clause-pairs")))
    (let [matcher (deft-ref pred-key)]
      (with-syms [v]
        (var acc (if finaliser ~(,finaliser ,v) nil))
        (var k (- (length clause-pairs) 2))
        (while (>= k 0)
          (def T (in clause-pairs k))
          (def branch (in clause-pairs (+ k 1)))
          (set acc ~(if (,matcher ,v ',T) ,branch ,acc))
          (set k (- k 2)))
        (tuple 'do (tuple 'def v val) acc)))))


(defmacro typecase
  ```A conditonal form that matches on type using isa? (implicit type match).

  (typecase value
    :type1  body1
    :type2  body2
    ...
    finaliser)

Each clause is a type followed by a form to evaluate if matched.
If no clause matches the result is `nil` by default. If a finaliser
is present (optional final argument), that function is called with
the value `val`.

Uses `isa?` for matching, which runs the registered predicate function
for `T` using value `val` (i.e. implicit type match).
```
  [val & clauses]
  (build-case 'isa? 'typecase val clauses))


(defmacro typecase-strict
  ```A conditonal form that matches on type using type= (strict type equality).

  (typecase-strict value
    :type1  body1
    :type2  body2
    ...
    finaliser)

Each clause is a type followed by a form to evaluate if matched.
If no clause matches the result is `nil` by default. If a finaliser
is present (optional final argument), that function is called with
the value `val`.

Uses `type=` for matching, which compares `(type val)` to `T`.
```
  [val & clauses]
  (build-case 'type= 'typecase-strict val clauses))
