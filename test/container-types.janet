# -*- mode: janet; -*-
# deft tests: container types (:array, :tuple, :table)

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* container types")

# :array
(deftfn sum-array [xs (:array :number)] :number
   (reduce + 0 xs))

(assert ":array accepts number array"
        (sum-array @[1 2 3]) 6)
(assert ":array accepts empty array"
        (sum-array @[]) 0)
(assert-err ":array rejects string element"
            (sum-array @[1 "bad" 3]))
(assert-err ":array rejects non-array"
            (sum-array 42))

(assert "isa? :array number"
        (isa? @[1 2 3] '(:array :number)) true)
(assert "isa? :array string"
        (isa? @["a" "b"] '(:array :string)) true)
(assert "isa? :array rejects mixed"
        (isa? @[1 "a"] '(:array :number)) false)
(assert "isa? :array rejects non-array"
        (isa? 42 '(:array :number)) false)

# (:array :dynamic)
(assert ":array :dynamic accepts numbers"
        (isa? @[1 2] '(:array :dynamic)) true)
(assert ":array :dynamic accepts strings"
        (isa? @["a" "b"] '(:array :dynamic)) true)


# :tuple
(deftfn sum-tuple [xs (:tuple :number)] :number
   (reduce + 0 xs))

(assert ":tuple accepts number tuple"
        (sum-tuple '(1 2 3)) 6)
(assert ":tuple accepts empty tuple"
        (sum-tuple '()) 0)
(assert-err ":tuple rejects string element"
            (sum-tuple '(1 "bad" 3)))
(assert-err ":tuple rejects non-tuple"
            (sum-tuple @[1 2 3]))

(assert "isa? :tuple number"
        (isa? '(1 2) '(:tuple :number)) true)
(assert "isa? :tuple string"
        (isa? '("a" "b") '(:tuple :string)) true)
(assert "isa? :tuple rejects array"
        (isa? @[1 2] '(:tuple :number)) false)


# :table
(deftfn size-of [t (:table :string :number)] :number
   (get t "size" 0))

(assert ":table string->number accepts"
        (size-of @{"size" 5 "count" 3}) 5)
(assert ":table string->number key"
        (size-of @{"size" 5}) 5)
(assert ":table string->number accepts empty"
        (size-of @{}) 0)
(assert-err ":table string->number rejects number key"
            (size-of @{5 3}))
(assert-err ":table string->number rejects string val"
            (size-of @{"x" "bad"}))
(assert-err ":table string->number rejects non-table"
            (size-of 42))

(assert "isa? :table keyword->number"
        (isa? @{:a 1 :b 2} '(:table :keyword :number)) true)
(assert "isa? :table keyword->number rejects"
        (isa? @{:a 1 :b "x"} '(:table :keyword :number)) false)


# nested
(deftfn first-elements
  [xs (:array (:tuple :number))] (:array :number)
  (map (fn [t] (get t 0)) xs))

(assert "nested :array of :tuple"
        (deep= (first-elements @['(1 10) '(2 20) '(3 30)]) @[1 2 3]) true)
(assert-err "nested :array rejects bad tuple"
            (first-elements @['(1 10) '("x" 20)]))


# define
(define d-sum [xs (:array :number)] :number (reduce + 0 xs))

(assert "define :array works"
        (d-sum @[1 2 3]) 6)
(assert-err "define :array rejects"
            (d-sum @[1 "x"]))

(assert "isa? nested :array of :tuple"
        (isa? @['(1 2)] '(:array (:tuple :number))) true)
(assert "isa? nested rejects"
        (isa? @['(1 "x")] '(:array (:tuple :number))) false)


# :array with keyword keys
(deftfn count-keys [t (:table :keyword :number)] :number
   (reduce + 0 (values t)))

(assert ":table keyword->number"
        (count-keys @{:a 1 :b 2}) 3)
(assert-err ":table keyword->number rejects string key"
            (count-keys @{"a" 1}))


# deftype
(deftype :number-array (:array :number))
(deftype :number-tuple (:tuple :number))
(deftype :string-number-table (:table :string :number))
(deftype :number-array-or-string (or (:array :number) :string))

(assert "deftype :number-array accepts"
        (isa? @[1 2] :number-array) true)
(assert "deftype :number-array rejects string element"
        (isa? @[1 "x"] :number-array) false)
(assert "deftype :number-array rejects non-array"
        (isa? 42 :number-array) false)

(assert "deftype :number-tuple accepts"
        (isa? '(1 2) :number-tuple) true)
(assert "deftype :number-tuple rejects string element"
        (isa? '(1 "x") :number-tuple) false)
(assert "deftype :number-tuple rejects array"
        (isa? @[1 2] :number-tuple) false)

(assert "deftype :string-number-table accepts"
        (isa? @{"a" 1 "b" 2} :string-number-table) true)
(assert "deftype :string-number-table rejects number key"
        (isa? @{5 1} :string-number-table) false)
(assert "deftype :string-number-table rejects string value"
        (isa? @{"a" "x"} :string-number-table) false)

(assert "deftype :number-array-or-string accepts array"
        (isa? @[1 2] :number-array-or-string) true)
(assert "deftype :number-array-or-string accepts string"
        (isa? "hello" :number-array-or-string) true)
(assert "deftype :number-array-or-string rejects keyword"
        (isa? :foo :number-array-or-string) false)

(deftfn sum-num-array
  [xs :number-array] :number
   (reduce + 0 xs))

(assert "deftfn with deftype :number-array"
        (sum-num-array @[1 2 3]) 6)
(assert-err "deftfn with deftype :number-array rejects"
            (sum-num-array @[1 "x"]))

(deftfn sum-num-tuple
  [xs :number-tuple] :number
   (reduce + 0 xs))

(assert "deftfn with deftype :number-tuple"
        (sum-num-tuple '(1 2 3)) 6)
(assert-err "deftfn with deftype :number-tuple rejects"
            (sum-num-tuple '(1 "x")))


# deftype compound with container types
(deftype :empty-table (:table :keyword :keyword))
(deftype :table-or-array (or (:table :keyword :number) (:array :number)))
(deftype :nested-array-tuple (:array (:tuple :number)))

(assert "deftype :empty-table"
        (isa? @{:a :b} :empty-table) true)
(assert "deftype :empty-table rejects"
        (isa? @{:a 1} :empty-table) false)

(assert "deftype :table-or-array accepts table"
        (isa? @{:a 1} :table-or-array) true)
(assert "deftype :table-or-array accepts array"
        (isa? @[1 2] :table-or-array) true)
(assert "deftype :table-or-array rejects string"
        (isa? "hi" :table-or-array) false)

(assert "deftype :nested-array-tuple"
        (isa? @['(1 2) '(3 4)] :nested-array-tuple) true)
(assert "deftype :nested-array-tuple rejects"
        (isa? @['(1 "x")] :nested-array-tuple) false)

# define with deftype container types
(define d-sum-array
  [xs :number-array] :number
   (reduce + 0 xs))

(assert "define with deftype :number-array"
        (d-sum-array @[1 2 3]) 6)
(assert-err "define with deftype :number-array rejects"
            (d-sum-array @[1 "x"]))

(define d-sum-tuple
  [xs :number-tuple] :number
   (reduce + 0 xs))

(assert "define with deftype :number-tuple"
        (d-sum-tuple '(1 2 3)) 6)
(assert-err "define with deftype :number-tuple rejects"
            (d-sum-tuple '(1 "x")))

(define d-count
  [t :string-number-table] :number
   (get t "size" 0))

(assert "define with deftype :string-number-table"
        (d-count @{"size" 5}) 5)
(assert-err "define with deftype :string-number-table rejects"
            (d-count @{"size" "x"}))


(print-results)
