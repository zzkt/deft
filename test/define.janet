# -*- mode: janet; -*-
# deft tests: define

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* define")

# docstring with double quotes
(define d-doc1 "adds two numbers" [a :number b :number] :number (+ a b))
(assert "docstring double-quoted" (d-doc1 3 4) 7)
(assert "docstring stored" (get (dyn 'd-doc1) :doc) "(d-doc1 a b)\n\nadds two numbers")

# docstring with long-string backticks
(define d-doc2 ``identity function`` [x :dynamic] :dynamic x)
(assert "docstring long-string" (d-doc2 42) 42)
(assert "docstring long-string" (get (dyn 'd-doc2) :doc) "(d-doc2 x)\n\nidentity function")

# docstring on untyped function
(define d-doc3 "plain function" [x y] (+ x y))
(assert "docstring untyped" (d-doc3 3 4) 7)
(assert "docstring untyped" (get (dyn 'd-doc3) :doc) "(d-doc3 x y)\n\nplain function")

# docstring on mixed typed/untyped fn
(define d-doc4 "mixed params" [a :number b c :string] :string (string (+ a b) c))
(assert "docstring mixed" (d-doc4 1 2 "x") "3x")
(assert "docstring mixed" (get (dyn 'd-doc4) :doc) "(d-doc4 a b c)\n\nmixed params")

# typed function
(define d-add [a :number b :number] :number
   (+ a b))
(assert "typed fn" (d-add 2 3) 5)

# untyped function
(define d-plain [x y]
  (+ x y))
(assert "untyped fn" (d-plain 3 4) 7)

# mixed typed/untyped params, explicit ret-type
(define d-mix [a :number b c :string] :string
  (string (+ a b) " " c))
(assert "mixed params" (d-mix 1 2 "hi") "3 hi")

# mixed params, no explicit ret-type (defaults :dynamic)
(define d-mix-dyn [a :number b] (+ a b))
(assert "mixed params dyn ret" (d-mix-dyn 3 4) 7)

# single :dynamic param
(define d-id [x :dynamic] :dynamic x)
(assert "dynamic param" (d-id 42) 42)
(assert "dynamic param str" (d-id "hi") "hi")

# no-arg function (thunk)
(define d-thunk [] :number 99)
(assert "thunk" (d-thunk) 99)

# multiple body forms
(define d-log-add [a :number b :number] :number
  (def result (+ a b))
  result)
(assert "multi body" (d-log-add 10 20) 30)

# untyped params with explicit ret-type
(define d-untyped-ret [name] :string
  (string "Hello, " name "!"))
(assert "untyped params ret" (d-untyped-ret "Alice") "Hello, Alice!")

# typed value
(define d-pi :number 3.14)
(assert "typed val" d-pi 3.14)

# typed value with :dynamic
(define d-any :dynamic "whatever")
(assert "dynamic val" d-any "whatever")

# typed string value
(define d-greeting :string "hi")
(assert "string val" d-greeting "hi")

# untyped value (mutable via var)
(define d-x 99)
(assert "plain val" d-x 99)
(set d-x 100)
(assert "plain val mutable" d-x 100)

# typed simple keyword value is mutable (var)
(define d-pi-val :number 3.14)
(assert "typed val mutable" d-pi-val 3.14)
(set d-pi-val 2.718)
(assert "typed val set works" d-pi-val 2.718)

# mutable typed compound value (no :immutable, emits var)
(define d-mutable (:or :number :string) 42)
(assert "compound mutable val" d-mutable 42)
(set d-mutable "hi")
(assert "compound mutable val set" d-mutable "hi")

# immutable typed compound value (contains :immutable, emits def)
(define d-immutable (:and :number :immutable) 1.618)
(assert "immutable compound val" d-immutable 1.618)

# immutable compound with :or and :immutable
(define d-imm-or (:or :number :string :immutable) "hello")
(assert "immutable or compound val" d-imm-or "hello")

# keyword-typed values are mutable (var)
(define d-pi-mut :number 3.14)
(assert "keyword typed mutable val" d-pi-mut 3.14)
(set d-pi-mut 2.718)
(assert "keyword typed val can set" d-pi-mut 2.718)

# immutable compound values cannot be set
(assert-err "immutable compound cannot set" (eval (tuple 'set 'd-immutable 3.14)))
(assert-err "immutable or cannot set" (eval (tuple 'set 'd-imm-or 99)))

# typed fn catches arg error
(assert-err "arg error caught" (d-add "bad" 5))

# typed fn catches ret error
(define d-bad-ret [x :number] :string (+ x 1))
(assert-err "ret error caught" (d-bad-ret 5))

# typed value catches init error
(assert-err "value init error caught" (define d-bad-val :number "not-a-number"))

# define with custom deftype
(deftype :positive? (fn [v] (and (number? v) (> v 0))))
(define d-pos-add [a :positive? b :positive?] :positive? (+ a b))
(assert "custom type" (d-pos-add 5 3) 8)

(print-results)
