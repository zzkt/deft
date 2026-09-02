# -*- mode: janet; -*-
# deft tests: functions

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* deftfn")

(deftfn add [a :number b :number] :number
  (+ a b))

(cassert "add 2 3" (add 2 3) 5)
(cassert "add -1 1" (add -1 1) 0)

(deftfn identity [x :dynamic] :dynamic x)
(cassert "identity 42" (identity 42) 42)
(cassert "identity :kw" (identity :kw) :kw)

(deftfn greet [g :string n :string] :string
  (string g ", " n "!"))
(cassert "greet" (greet "Hi" "there") "Hi, there!")

(print "\n* deftfn-")

(deftfn- helper [s :string] :string
  (string s "!"))

(cassert "private function" (helper "hi") "hi!")

(print "\n* blame tracking")

(deftfn parse-int [s :string] :number
  (scan-number s))

(cassert-err "return type error blamed on function" (parse-int "not-a-number"))

(print "\n* deftn")

(deftn flex [a :number b c :string] :string
  (string (+ a b) " " c))

(cassert "flex typed+untyped" (flex 1 2 "hi") "3 hi")

(deftn plain [x y]
  (string x y))

(cassert "plain no annotations" (plain "a" "b") "ab")

(deftn incr [i] :number
  (+ i 1))

(cassert "explicit ret-type" (incr 5) 6)

(deftn str-pair [a b]
  (string a b))

(cassert "implicit dynamic ret-type" (str-pair "x" "y") "xy")

(cassert-err "flex typed arg catches error" (flex "bad" 2 "hi"))

(print "\n* consistent?")

(cassert "dynamic ~ number" (consistent? :dynamic :number) true)
(cassert "number ~ dynamic" (consistent? :number :dynamic) true)
(cassert "number ~ number"  (consistent? :number :number) true)
(cassert "number !~ string" (consistent? :number :string) false)

(print "\n* toggle checking")

(enable-checking false)
(deftfn bogus [x :number] :number x)
(cassert "disabled: no error" (bogus "should-work-when-checking-off") "should-work-when-checking-off")

(enable-checking true)
(deftfn strict [x :number] :number x)
(cassert-err "re-enabled: error caught" (strict "boom"))

(print "\n* inline compound types")

(deftype :positive (fn [v] (and (number? v) (> v 0))))
(deftype :negative (fn [v] (and (number? v) (< v 0))))

# :or in param
(deftfn or-param [x (:or :positive :negative)] :number x)
(cassert ":or param positive" (or-param 5) 5)
(cassert ":or param negative" (or-param -3) -3)
(cassert-err ":or param rejects zero" (or-param 0))

# :and in param
(deftfn and-param [x (:and :number :positive)] :number x)
(cassert ":and param positive" (and-param 5) 5)
(cassert-err ":and param rejects negative" (and-param -1))
(cassert-err ":and param rejects string" (and-param "hi"))

# :not in param
(deftfn not-param [x (:not :string)] :dynamic x)
(cassert ":not param number" (not-param 42) 42)
(cassert ":not param keyword" (not-param :foo) :foo)
(cassert-err ":not param rejects string" (not-param "hi"))

# compound return type with deftfn
(deftfn pos-or-neg [x :number] (:or :positive :negative) x)
(cassert ":or return positive" (pos-or-neg 5) 5)
(cassert ":or return negative" (pos-or-neg -3) -3)
(cassert-err ":or return rejects zero" (pos-or-neg 0))

# inline compound with deftn
(deftn flex-or [a :number b (:or :string :keyword)] :string
  (string a " " b))
(cassert "deftn :or param" (flex-or 1 :hi) "1 hi")
(cassert-err "deftn :or param rejects number" (flex-or 1 42))

# inline compound with define
(define d-or [x (:or :string :keyword)] :boolean true)
(cassert "define :or param string" (d-or "hi") true)
(cassert "define :or param keyword" (d-or :ok) true)
(cassert-err "define :or param rejects number" (d-or 99))

(print-results)
