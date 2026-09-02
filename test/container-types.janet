# -*- mode: janet; -*-
# deft tests: container types (:array, :tuple, :table)

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* container types")

# :array
(deftfn sum-array [xs (:array :number)] :number
   (reduce + 0 xs))

(cassert ":array accepts number array"
        (sum-array @[1 2 3]) 6)
(cassert ":array accepts empty array"
        (sum-array @[]) 0)
(cassert-err ":array rejects string element"
            (sum-array @[1 "bad" 3]))
(cassert-err ":array rejects non-array"
            (sum-array 42))

(cassert "isa? :array number"
        (isa? @[1 2 3] '(:array :number)) true)
(cassert "isa? :array string"
        (isa? @["a" "b"] '(:array :string)) true)
(cassert "isa? :array rejects mixed"
        (isa? @[1 "a"] '(:array :number)) false)
(cassert "isa? :array rejects non-array"
        (isa? 42 '(:array :number)) false)

# (:array :dynamic)
(cassert ":array :dynamic accepts numbers"
        (isa? @[1 2] '(:array :dynamic)) true)
(cassert ":array :dynamic accepts strings"
        (isa? @["a" "b"] '(:array :dynamic)) true)


# :tuple
(deftfn sum-tuple [xs (:tuple :number)] :number
   (reduce + 0 xs))

(cassert ":tuple accepts number tuple"
        (sum-tuple '(1 2 3)) 6)
(cassert ":tuple accepts empty tuple"
        (sum-tuple '()) 0)
(cassert-err ":tuple rejects string element"
            (sum-tuple '(1 "bad" 3)))
(cassert-err ":tuple rejects non-tuple"
            (sum-tuple @[1 2 3]))

(cassert "isa? :tuple number"
        (isa? '(1 2) '(:tuple :number)) true)
(cassert "isa? :tuple string"
        (isa? '("a" "b") '(:tuple :string)) true)
(cassert "isa? :tuple rejects array"
        (isa? @[1 2] '(:tuple :number)) false)


# :table
(deftfn size-of [t (:table :string :number)] :number
   (get t "size" 0))

(cassert ":table string->number accepts"
        (size-of @{"size" 5 "count" 3}) 5)
(cassert ":table string->number key"
        (size-of @{"size" 5}) 5)
(cassert ":table string->number accepts empty"
        (size-of @{}) 0)
(cassert-err ":table string->number rejects number key"
            (size-of @{5 3}))
(cassert-err ":table string->number rejects string val"
            (size-of @{"x" "bad"}))
(cassert-err ":table string->number rejects non-table"
            (size-of 42))

(cassert "isa? :table keyword->number"
        (isa? @{:a 1 :b 2} '(:table :keyword :number)) true)
(cassert "isa? :table keyword->number rejects"
        (isa? @{:a 1 :b "x"} '(:table :keyword :number)) false)


# nested
(deftfn first-elements
  [xs (:array (:tuple :number))] (:array :number)
  (map (fn [t] (get t 0)) xs))

(cassert "nested :array of :tuple"
        (deep= (first-elements @['(1 10) '(2 20) '(3 30)]) @[1 2 3]) true)
(cassert-err "nested :array rejects bad tuple"
            (first-elements @['(1 10) '("x" 20)]))


# define
(define d-sum [xs (:array :number)] :number (reduce + 0 xs))

(cassert "define :array works"
        (d-sum @[1 2 3]) 6)
(cassert-err "define :array rejects"
            (d-sum @[1 "x"]))

(cassert "isa? nested :array of :tuple"
        (isa? @['(1 2)] '(:array (:tuple :number))) true)
(cassert "isa? nested rejects"
        (isa? @['(1 "x")] '(:array (:tuple :number))) false)


# :array with keyword keys
(deftfn count-keys [t (:table :keyword :number)] :number
   (reduce + 0 (values t)))

(cassert ":table keyword->number"
        (count-keys @{:a 1 :b 2}) 3)
(cassert-err ":table keyword->number rejects string key"
            (count-keys @{"a" 1}))


# deftype
(deftype :number-array (:array :number))
(deftype :number-tuple (:tuple :number))
(deftype :string-number-table (:table :string :number))
(deftype :number-array-or-string (or (:array :number) :string))

(cassert "deftype :number-array accepts"
        (isa? @[1 2] :number-array) true)
(cassert "deftype :number-array rejects string element"
        (isa? @[1 "x"] :number-array) false)
(cassert "deftype :number-array rejects non-array"
        (isa? 42 :number-array) false)

(cassert "deftype :number-tuple accepts"
        (isa? '(1 2) :number-tuple) true)
(cassert "deftype :number-tuple rejects string element"
        (isa? '(1 "x") :number-tuple) false)
(cassert "deftype :number-tuple rejects array"
        (isa? @[1 2] :number-tuple) false)

(cassert "deftype :string-number-table accepts"
        (isa? @{"a" 1 "b" 2} :string-number-table) true)
(cassert "deftype :string-number-table rejects number key"
        (isa? @{5 1} :string-number-table) false)
(cassert "deftype :string-number-table rejects string value"
        (isa? @{"a" "x"} :string-number-table) false)

(cassert "deftype :number-array-or-string accepts array"
        (isa? @[1 2] :number-array-or-string) true)
(cassert "deftype :number-array-or-string accepts string"
        (isa? "hello" :number-array-or-string) true)
(cassert "deftype :number-array-or-string rejects keyword"
        (isa? :foo :number-array-or-string) false)

(deftfn sum-num-array
  [xs :number-array] :number
   (reduce + 0 xs))

(cassert "deftfn with deftype :number-array"
        (sum-num-array @[1 2 3]) 6)
(cassert-err "deftfn with deftype :number-array rejects"
            (sum-num-array @[1 "x"]))

(deftfn sum-num-tuple
  [xs :number-tuple] :number
   (reduce + 0 xs))

(cassert "deftfn with deftype :number-tuple"
        (sum-num-tuple '(1 2 3)) 6)
(cassert-err "deftfn with deftype :number-tuple rejects"
            (sum-num-tuple '(1 "x")))


# deftype compound with container types
(deftype :empty-table (:table :keyword :keyword))
(deftype :table-or-array (or (:table :keyword :number) (:array :number)))
(deftype :nested-array-tuple (:array (:tuple :number)))

(cassert "deftype :empty-table"
        (isa? @{:a :b} :empty-table) true)
(cassert "deftype :empty-table rejects"
        (isa? @{:a 1} :empty-table) false)

(cassert "deftype :table-or-array accepts table"
        (isa? @{:a 1} :table-or-array) true)
(cassert "deftype :table-or-array accepts array"
        (isa? @[1 2] :table-or-array) true)
(cassert "deftype :table-or-array rejects string"
        (isa? "hi" :table-or-array) false)

(cassert "deftype :nested-array-tuple"
        (isa? @['(1 2) '(3 4)] :nested-array-tuple) true)
(cassert "deftype :nested-array-tuple rejects"
        (isa? @['(1 "x")] :nested-array-tuple) false)

# define with deftype container types
(define d-sum-array
  [xs :number-array] :number
   (reduce + 0 xs))

(cassert "define with deftype :number-array"
        (d-sum-array @[1 2 3]) 6)
(cassert-err "define with deftype :number-array rejects"
            (d-sum-array @[1 "x"]))

(define d-sum-tuple
  [xs :number-tuple] :number
   (reduce + 0 xs))

(cassert "define with deftype :number-tuple"
        (d-sum-tuple '(1 2 3)) 6)
(cassert-err "define with deftype :number-tuple rejects"
            (d-sum-tuple '(1 "x")))

(define d-count
  [t :string-number-table] :number
   (get t "size" 0))

(cassert "define with deftype :string-number-table"
        (d-count @{"size" 5}) 5)
(cassert-err "define with deftype :string-number-table rejects"
            (d-count @{"size" "x"}))


(print-results)
