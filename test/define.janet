# -*- mode: janet; -*-
# deft tests: define

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* define")

# docstring with double quotes
(define d-doc1 "adds two numbers" [a :number b :number] :number (+ a b))
(cassert "docstring double-quoted" (d-doc1 3 4) 7)
(cassert "docstring stored" (get (dyn 'd-doc1) :doc) "(d-doc1 a b)\n\nadds two numbers")

# docstring with long-string backticks
(define d-doc2 ``identity function`` [x :dynamic] :dynamic x)
(cassert "docstring long-string" (d-doc2 42) 42)
(cassert "docstring long-string" (get (dyn 'd-doc2) :doc) "(d-doc2 x)\n\nidentity function")

# docstring on untyped function
(define d-doc3 "plain function" [x y] (+ x y))
(cassert "docstring untyped" (d-doc3 3 4) 7)
(cassert "docstring untyped" (get (dyn 'd-doc3) :doc) "(d-doc3 x y)\n\nplain function")

# docstring on mixed typed/untyped fn
(define d-doc4 "mixed params" [a :number b c :string] :string (string (+ a b) c))
(cassert "docstring mixed" (d-doc4 1 2 "x") "3x")
(cassert "docstring mixed" (get (dyn 'd-doc4) :doc) "(d-doc4 a b c)\n\nmixed params")

# typed function
(define d-add [a :number b :number] :number
   (+ a b))
(cassert "typed fn" (d-add 2 3) 5)

# untyped function
(define d-plain [x y]
  (+ x y))
(cassert "untyped fn" (d-plain 3 4) 7)

# mixed typed/untyped params, explicit ret-type
(define d-mix [a :number b c :string] :string
  (string (+ a b) " " c))
(cassert "mixed params" (d-mix 1 2 "hi") "3 hi")

# mixed params, no explicit ret-type (defaults :dynamic)
(define d-mix-dyn [a :number b] (+ a b))
(cassert "mixed params dyn ret" (d-mix-dyn 3 4) 7)

# single :dynamic param
(define d-id [x :dynamic] :dynamic x)
(cassert "dynamic param" (d-id 42) 42)
(cassert "dynamic param str" (d-id "hi") "hi")

# no-arg function (thunk)
(define d-thunk [] :number 99)
(cassert "thunk" (d-thunk) 99)

# multiple body forms
(define d-log-add [a :number b :number] :number
  (def result (+ a b))
  result)
(cassert "multi body" (d-log-add 10 20) 30)

# untyped params with explicit ret-type
(define d-untyped-ret [name] :string
  (string "Hello, " name "!"))
(cassert "untyped params ret" (d-untyped-ret "Alice") "Hello, Alice!")

# typed value
(define d-pi :number 3.14)
(cassert "typed val" d-pi 3.14)

# typed value with :dynamic
(define d-any :dynamic "whatever")
(cassert "dynamic val" d-any "whatever")

# typed string value
(define d-greeting :string "hi")
(cassert "string val" d-greeting "hi")

# untyped value (mutable via var)
(define d-x 99)
(cassert "plain val" d-x 99)
(set d-x 100)
(cassert "plain val mutable" d-x 100)

# typed simple keyword value is mutable (var)
(define d-pi-val :number 3.14)
(cassert "typed val mutable" d-pi-val 3.14)
(set d-pi-val 2.718)
(cassert "typed val set works" d-pi-val 2.718)

# mutable typed compound value (no :immutable, emits var)
(define d-mutable (:or :number :string) 42)
(cassert "compound mutable val" d-mutable 42)
(set d-mutable "hi")
(cassert "compound mutable val set" d-mutable "hi")

# immutable typed compound value (contains :immutable, emits def)
(define d-immutable (:and :number :immutable) 1.618)
(cassert "immutable compound val" d-immutable 1.618)

# immutable compound with :or and :immutable
(define d-imm-or (:or :number :string :immutable) "hello")
(cassert "immutable or compound val" d-imm-or "hello")

# keyword-typed values are mutable (var)
(define d-pi-mut :number 3.14)
(cassert "keyword typed mutable val" d-pi-mut 3.14)
(set d-pi-mut 2.718)
(cassert "keyword typed val can set" d-pi-mut 2.718)

# immutable compound values cannot be set
(cassert-err "immutable compound cannot set" (eval (tuple 'set 'd-immutable 3.14)))
(cassert-err "immutable or cannot set" (eval (tuple 'set 'd-imm-or 99)))

# typed fn catches arg error
(cassert-err "arg error caught" (d-add "bad" 5))

# typed fn catches ret error
(define d-bad-ret [x :number] :string (+ x 1))
(cassert-err "ret error caught" (d-bad-ret 5))

# typed value catches init error
(cassert-err "value init error caught" (define d-bad-val :number "not-a-number"))

# define with custom deftype
(deftype :positive? (fn [v] (and (number? v) (> v 0))))
(define d-pos-add [a :positive? b :positive?] :positive? (+ a b))
(cassert "custom type" (d-pos-add 5 3) 8)

(print-results)
