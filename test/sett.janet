# -*- mode: janet; -*-
# deft tests: sett (typed set) and inference via sett and lett

(import ./helper :prefix "")
(import deft :prefix "")

(print "* sett — basic")

# sett with deftv
(deftv counter :number 0)
(sett counter 10)
(cassert "sett preserves type" counter 10)
(cassert-err "sett rejects type mismatch" (sett counter "bad"))

# sett on var not created with deftv. no type check.
(var plain 0)
(sett plain "now-string")
(cassert "sett on untyped var" plain "now-string")

# sett with nil
(deftv nullable :number 0)
(sett nullable 42)
(cassert "sett nil-type" nullable 42)

# sett on a typed mutable binding with define.
(print "\n* sett — typed bindings made by define")

(define d-num :number 1)
(sett d-num 2)
(cassert "define/sett accepts matching value" d-num 2)
(cassert-err "define/sett rejects mismatch" (sett d-num "bad"))
(cassert "define/sett leaves value intact on failure" d-num 2)
(cassert "define/sett keeps recorded type" (type d-num) :number)

(define d-str :string "hi")
(sett d-str "bye")
(cassert "define/sett :string" d-str "bye")
(cassert-err "define/sett :string mismatch" (sett d-str 99))

# compound declared type is registered just the same
(define d-arr (:array :number) @[1 2])
(cassert-err "define/sett compound mismatch" (sett d-arr "nope"))
(sett d-arr @[1 2 3])
(cassert "define/sett compound ok" (length d-arr) 3)

# mutable container keeps working through sett
(define d-tab (:table :keyword :number) @{:a 1})
(cassert-err "define/sett table mismatch" (sett d-tab @{:a "x"}))
(sett d-tab @{:a 9})
(cassert "define/sett table ok" (get d-tab :a) 9)

# :immutable selects `def`, which is not settable and must not be registered
(define d-imm :immutable "fixed")
(cassert "define :immutable value" d-imm "fixed")
(cassert "define :immutable not in var-types"
         (nil? (get ((deft-ref 'var-types)) 'd-imm)) true)

# untyped `define` is still a bare var and stays unchecked
(define d-plain 5)
(sett d-plain "free")
(cassert "define untyped stays unchecked" d-plain "free")


(print "\n* sett — with inference in deftn")

(enable-inference true)

# sett literal should infer var type and propagate to param via arithmetic
(deftn infer-from-sett [x]
  (var tmp x)
  (sett tmp 10)
  (+ tmp 1))

(cassert "infer-from-sett result" (infer-from-sett 1) 11)

# sett literal helps infer types of args used in arithmetic
(deftn infer-via-sett-param [x y]
  (var tmp x)
  (sett tmp 10)
  (+ tmp y))

(cassert "infer-via-sett-param" (infer-via-sett-param 1 5) 15)

# y is inferred as :number from (+ tmp y), so passing a string errors
(cassert-err "infer-via-sett catches param mismatch" (infer-via-sett-param 1 "bad"))

# sett with mutable type inference
(deftn sett-mutable-infer [init]
  (var data init)
  (sett data @{:a 1})
  (put data :b 2)
  data)

(cassert "sett mutable inference" (get (sett-mutable-infer @{}) :b) 2)

# sett with the same type should be fine
(deftn sett-same-type [init]
  (var tmp init)
  (sett tmp 10)
  (sett tmp 20)
  (+ tmp 1))

(cassert "sett same type ok" (sett-same-type 0) 21)

(print "\n* sett — via deftn with sett of plain var")

# When inference is off, sett on untyped var works regardless
(enable-inference false)

(deftn sett-any [x]
  (var tmp x)
  (sett tmp 42)
  (sett tmp "hello")
  tmp)

(cassert "sett with inference off" (sett-any 1) "hello")

(enable-inference true)

(print "\n* lett — inference contribution")

# lett with explicit type should inform inference in the body
(deftn lett-infer-explicit [x]
  (lett [y :number x]
    (+ y 1)))

(cassert "lett explicit type" (lett-infer-explicit 5) 6)

# lett with literal value should infer type from the value
(deftn lett-infer-from-literal []
  (lett [(y "hello")]
    (string y " world")))

(cassert "lett literal type" (lett-infer-from-literal) "hello world")

# lett sequential bindings — earlier bindings inform later ones
(deftn lett-sequential-infer [x]
  (lett [a :number x
              b :number (+ a 1)]
    (+ b 2)))

(cassert "lett sequential inference" (lett-sequential-infer 5) 8)

# lett tuple syntax with inference
(deftn lett-tuple-infer [x]
  (lett [(y :number x)]
    (+ y 1)))

(cassert "lett tuple syntax" (lett-tuple-infer 10) 11)

# lett with string ops using inference
(deftn lett-string-concat [x]
  (lett [s :string x
              t :string (string s "!")]
    t))

(cassert "lett string op" (lett-string-concat "hi") "hi!")

# lett catches type mismatch at runtime even when binding references another binding
(deftn lett-cross-binding [x]
  (lett [a :number x
              b :string a]
    b))

(cassert-err "lett cross-binding type mismatch" (lett-cross-binding 42))

(print "\n* sett — static checks via check-form")

# clean sett usage
(def dc1 (check-form
  '(deftn sett-clean [x :number]
     (var tmp x)
     (sett tmp 10)
     tmp)))
(cassert "deftcheck sett clean" (= (length dc1) 0) true)

# mixed inference modes
(enable-inference false)
(deftn no-infer-sett [x]
  (var tmp x)
  (sett tmp 42)
  tmp)

(cassert "no inference sett passes" (no-infer-sett 1) 42)
(enable-inference true)

(print-results)
