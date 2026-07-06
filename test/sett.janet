# -*- mode: janet; -*-
# deft tests: sett (typed set) and inference via sett and lett

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* sett — basic")

# sett with deftv
(deftv counter :number 0)
(sett counter 10)
(assert "sett preserves type" counter 10)
(assert-err "sett rejects type mismatch" (sett counter "bad"))

# sett on var not created with deftv. no type check.
(var plain 0)
(sett plain "now-string")
(assert "sett on untyped var" plain "now-string")

# sett with nil
(deftv nullable :number 0)
(sett nullable 42)
(assert "sett nil-type" nullable 42)


(print "\n* sett — with inference in deftn")

(enable-inference true)

# sett literal should infer var type and propagate to param via arithmetic
(deftn infer-from-sett [x]
  (var tmp x)
  (sett tmp 10)
  (+ tmp 1))

(assert "infer-from-sett result" (infer-from-sett 1) 11)

# sett literal helps infer types of args used in arithmetic
(deftn infer-via-sett-param [x y]
  (var tmp x)
  (sett tmp 10)
  (+ tmp y))

(assert "infer-via-sett-param" (infer-via-sett-param 1 5) 15)

# y is inferred as :number from (+ tmp y), so passing a string errors
(assert-err "infer-via-sett catches param mismatch" (infer-via-sett-param 1 "bad"))

# sett with mutable type inference
(deftn sett-mutable-infer [init]
  (var data init)
  (sett data @{:a 1})
  (put data :b 2)
  data)

(assert "sett mutable inference" (get (sett-mutable-infer @{}) :b) 2)

# sett with the same type should be fine
(deftn sett-same-type [init]
  (var tmp init)
  (sett tmp 10)
  (sett tmp 20)
  (+ tmp 1))

(assert "sett same type ok" (sett-same-type 0) 21)

(print "\n* sett — via deftn with sett of plain var")

# When inference is off, sett on untyped var works regardless
(enable-inference false)

(deftn sett-any [x]
  (var tmp x)
  (sett tmp 42)
  (sett tmp "hello")
  tmp)

(assert "sett with inference off" (sett-any 1) "hello")

(enable-inference true)

(print "\n* lett — inference contribution")

# lett with explicit type should inform inference in the body
(deftn lett-infer-explicit [x]
  (lett [y :number x]
    (+ y 1)))

(assert "lett explicit type" (lett-infer-explicit 5) 6)

# lett with literal value should infer type from the value
(deftn lett-infer-from-literal []
  (lett [(y "hello")]
    (string y " world")))

(assert "lett literal type" (lett-infer-from-literal) "hello world")

# lett sequential bindings — earlier bindings inform later ones
(deftn lett-sequential-infer [x]
  (lett [a :number x
              b :number (+ a 1)]
    (+ b 2)))

(assert "lett sequential inference" (lett-sequential-infer 5) 8)

# lett tuple syntax with inference
(deftn lett-tuple-infer [x]
  (lett [(y :number x)]
    (+ y 1)))

(assert "lett tuple syntax" (lett-tuple-infer 10) 11)

# lett with string ops using inference
(deftn lett-string-concat [x]
  (lett [s :string x
              t :string (string s "!")]
    t))

(assert "lett string op" (lett-string-concat "hi") "hi!")

# lett catches type mismatch at runtime even when binding references another binding
(deftn lett-cross-binding [x]
  (lett [a :number x
              b :string a]
    b))

(assert-err "lett cross-binding type mismatch" (lett-cross-binding 42))

(print "\n* sett — static checks via check-form")

# clean sett usage
(def dc1 (check-form
  '(deftn sett-clean [x :number]
     (var tmp x)
     (sett tmp 10)
     tmp)))
(assert "deftcheck sett clean" (= (length dc1) 0) true)

# mixed inference modes
(enable-inference false)
(deftn no-infer-sett [x]
  (var tmp x)
  (sett tmp 42)
  tmp)

(assert "no inference sett passes" (no-infer-sett 1) 42)
(enable-inference true)

(print-results)
