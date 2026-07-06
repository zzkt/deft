# -*- mode: janet; -*-
# deft inference API & trace example

(import deft :prefix "")

(enable-inference true)

(print "\n* infer-expression: ad-hoc type queries")
(printf "  (+ 1 2)         => %p" (infer-expression '(+ 1 2)))
(printf "  (string 42 \"!\") => %p" (infer-expression '(string 42 "!")))
(printf "  (if true 1 \"x\") => %p" (infer-expression '(if true 1 "x")))


(print "\n* infer-expression-full: ")
(def info (infer-expression-full '(+ x 1) @{:x :number}))
(printf "  type: %p" (info :type))
(printf "  subst: %p\n" (info :substitution))


(print "\n  with-inference-trace: step-by-step trace (output goes to stderr)")
(with-inference-trace
  (infer-expression '(+ 1 2)))


(print "  tracing if (when branches disagree):")
(with-inference-trace
  (infer-expression '(if true (string x) 0) @{:x :string}))


(print "\n* deftn definition with full macroexpansion")

(enable-inference-trace true)

(printf "  source:  (deftn add [x y] (+ x y))")
(printf "  expand:\n%p" (macex '(deftn add [x y] (+ x y))))

(printf "  source:  (deftn greet [a :string x] (string a x))")
(printf "  expand:\n%p" (macex '(deftn greet [a :string x] (string a x))))

(enable-inference-trace false)

(deftn add [x y] (+ x y))
(deftn greet [a :string x] (string a " " x))


(print "\n* Resolved type schemes")
(printf "(fn-type-of ... )")
(printf "  'add   -> %p" (fn-type-of 'add))
(printf "  'greet -> %p" (fn-type-of 'greet))


(print "\n* infer-assert-type: compile-time assertions")
(deftn safe-add [x y]
  (infer-assert-type :number (+ x y))
  (+ x y))
(printf "  safe-add expand:\n%p"
  (macex '(deftn safe-add [x y]
            (infer-assert-type :number (+ x y))
            (+ x y))))
(printf "  safe-add type: %p" (fn-type-of 'safe-add))
(printf "  safe-add compiles (assertion passed at macro-expansion time)")
