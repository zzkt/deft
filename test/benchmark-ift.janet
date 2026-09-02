# -*- mode: janet; -*-
# Benchmark: ifT type narrowing benchmark for deft
# https://github.com/utahplt/ifT-benchmark
#
# Tests type-narrowing features against deft's check-form static checker.
# Each item has a success case (should pass) and failure case (should error).
#
# NOTE: Janet/deft narrows :dynamic via type predicates.
# The checker verifies that narrowed types match declared return types.
# :dynamic unifies with everything, so failures test concrete type mismatches
# in branches where narrowing has occurred.
#
# Usage: janet test/benchmark-ift.janet

(import deft :prefix "")

# Helpers

(var passed 0)
(var failed 0)
(var total 0)

(defn check-case
  "Run a single benchmark case. ok=true means no errors expected."
  [item sublabel code ok]
  (prinf "  %-18s %-8s " item sublabel)
  (flush)
  (++ total)
  (def errs (check-form code))
  (def did-pass (= (length errs) 0))
  (if (= did-pass ok)
    (do (++ passed) (printf "\x1b[16;32m OK \x1b[0m"))
    (do (++ failed)
        (printf "FAIL (expected %s, got %s)\n" (if ok "pass" "err")
                (if did-pass "pass" (string (length errs) " error(s)")))
        (each err errs
          (printf "    %s\n" (string err))))))

(defn run-benchmark-item
  "Run both success and failure cases for one benchmark item."
  [item success-code failure-code]
  (check-case item "success" success-code true)
  (check-case item "failure" failure-code false))


# Benchmark items

# 1. positive — refine when condition is true
#    success: narrowed to string, length returns number (matches :number ret)
#    failure: return :string from true branch where x is narrowed to string,
#             but (length x) returns :number, not :string
(run-benchmark-item "positive"
  '(deftfn f [x :dynamic] :number
     (if (string? x) (length x) 0))
  '(deftfn f [x :dynamic] :string
     (if (string? x) (length x) x)))

# 2. negative — refine when condition is false
#    success: else branch returns 0, matches :number
#    failure: else branch returns x (:dynamic) as :string
(run-benchmark-item "negative"
  '(deftfn f [x :dynamic] :number
     (if (string? x) (length x) 0))
  '(deftfn f [x :dynamic] :string
     (if (string? x) (length x) 0)))

# 3. connectives — handle logic connectives (and/or/not)
#    success: not (number? x) → length x returns number
#    failure: return :string from else where x could be number
(run-benchmark-item "connectives"
  '(deftfn f [x :dynamic] :number
     (if (not (number? x)) (length x) 0))
  '(deftfn f [x :dynamic] :string
     (if (not (number? x)) (length x) x)))

# 4. nesting_body — nested conditionals refine intersection
#    success: not string AND not boolean → length x returns number
#    failure: return :string from else where x could be boolean
(run-benchmark-item "nesting_body"
  '(deftfn f [x :dynamic] :number
     (if (not (string? x))
       (if (not (boolean? x)) (length x) 0)
       0))
  '(deftfn f [x :dynamic] :string
     (if (not (string? x))
       (if (not (boolean? x)) (length x) 0)
       x)))

# 5. struct_fields — refine type of a struct field
#    success: field narrowed to number, return as number
#    failure: return :string from branch where field is narrowed to number
(run-benchmark-item "struct_fields"
  '(deftfn f [x :table] :number
     (def a (get x :a))
     (if (number? a) a 0))
  '(deftfn f [x :table] :string
     (def a (get x :a))
     (if (number? a) a (get x :b))))

# 6. tuple_elements — refine types of tuple elements
#    success: element narrowed to number, return as number
#    failure: return :string from branch where element is narrowed to number
(run-benchmark-item "tuple_elements"
  '(deftfn f [x :tuple] :number
     (def a (in x 0))
     (if (number? a) a 0))
  '(deftfn f [x :tuple] :string
     (def a (in x 0))
     (if (number? a) a (in x 1))))

# 7. tuple_length — refine union of tuple types by length
#    success: length 2 → number, else → string via (length (in x 0))
#    failure: return :string from both branches where one returns number
(run-benchmark-item "tuple_length"
  '(deftfn f [x :tuple] :number
     (if (= 2 (length x))
       (length x)
       (length (in x 0))))
  '(deftfn f [x :tuple] :string
     (if (= 2 (length x))
       (length x)
       (in x 0))))

# 8. alias — track test results bound to variables
#    success: y = (string? x), if y → x is string, length ok
#    failure: return :string when y narrows x to string (length returns number)
(run-benchmark-item "alias"
  '(deftfn f [x :dynamic] :number
     (do (def y (string? x)) (if y (length x) 0)))
  '(deftfn f [x :dynamic] :string
     (do (def y (string? x)) (if y (length x) x))))

# 9. nesting_condition — nested conditionals in condition position
#    success: x number AND y string, + returns number
#    failure: return :string from true branch where + returns number
(run-benchmark-item "nesting_condition"
  '(deftfn f [x :dynamic y :dynamic] :number
     (if (if (number? x) (string? y) false)
       (+ x (length y))
       0))
  '(deftfn f [x :dynamic y :dynamic] :string
     (if (if (number? x) (string? y) false)
       (+ x (length y))
       x)))

# 10. merge_with_union — merge refined types with union
#     success: r is number after branches, matches :number
#     failure: return :string from variable that was assigned number
(run-benchmark-item "merge_with_union"
  '(deftfn f [x :dynamic] :number
     (var r 0)
     (if (string? x) (set r (length x))
       (if (number? x) (set r x) (set r 0)))
     r)
  '(deftfn f [x :dynamic] :string
     (var r 0)
     (if (string? x) (set r (length x))
       (if (number? x) (set r x) (set r 0)))
     r))

# 11. predicate_2way — custom predicates refine both ways
(deftfn is-string? [x :dynamic] :boolean (string? x))

(run-benchmark-item "predicate_2way"
  '(deftfn f [x :dynamic] :number
     (if (is-string? x) (length x) 0))
  '(deftfn f [x :dynamic] :string
     (if (is-string? x) (length x) x)))

# 12. predicate_1way — custom predicates refine only positively
(deftfn positive-number? [x :dynamic] :boolean
  (and (number? x) (> x 0)))

(run-benchmark-item "predicate_1way"
  '(deftfn f [x :dynamic] :number
     (if (positive-number? x) x 0))
  '(deftfn f [x :dynamic] :string
     (if (positive-number? x) x (length x))))

# 13. predicate_checked — strict type checking on predicate body
(deftfn is-string-checked? [x :dynamic] :boolean (string? x))

(run-benchmark-item "predicate_checked"
  '(deftfn f [x :dynamic] :boolean
     (if (is-string-checked? x) true false))
  '(deftfn f [x :dynamic] :number
     (if (is-string-checked? x) true x)))


# Improved narrowing tests

# 14. custom_predicate — register-narrowing for user-defined predicates
(register-narrowing "positive-number?" :number)
(deftfn positive-number? [x :dynamic] :boolean (and (number? x) (> x 0)))

(run-benchmark-item "custom_pred"
  '(deftfn f [x :dynamic] :number
     (if (positive-number? x) x 0))
  '(deftfn f [x :dynamic] :string
     (if (positive-number? x) x (length x))))

# 15. nested_access — (pred (in x 0)) narrows through tuple access
(run-benchmark-item "nested_access"
  '(deftfn f [x :tuple] :number
     (def a (in x 0))
     (if (number? a) a 0))
  '(deftfn f [x :tuple] :string
     (def a (in x 0))
     (if (number? a) a (in x 1))))

# 16. negation — (not (pred x)) narrows in else-branch
(run-benchmark-item "negation"
  '(deftfn f [x :dynamic] :number
     (if (not (string? x)) x 0))
  '(deftfn f [x :dynamic] :string
     (if (not (number? x)) x (length x))))

# 17. compound_element — (:tuple T) → (in x 0) returns T
(run-benchmark-item "compound_elem"
  '(deftfn f [x (:tuple :number)] :number (in x 0))
  '(deftfn f [x (:tuple :string)] :number (in x 0)))


# Summary

(print)
(printf "ifT benchmark: %d/%d passed, %d failed\n" passed total failed)
(if (= failed 0)
  (print "All ifT benchmark items passed!")
  (do (print "Some items failed.") (os/exit 1)))
