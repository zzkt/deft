# -*- mode: janet; -*-
# bidirectional type inference examples

(import deft :prefix "")

# Enable type tracking during macro expansion
(enable-inference true)

(print "\n* Basic inference: types flow from operations")
(print "  param types inferred from how they're used in the body")

(deftn add [x y] (+ x y))
(print "  (add 3 4)      => " (add 3 4))
(print "  (add \"bad\" 5)  => " (try (add "bad" 5) ([e] e)))


(print "\n* Mixed annotations + inference")
(print "  annotated :string binds a; inferred x follows from (string a x)")

(deftn greet [a :string x] (string a " " x))
(print "  (greet \"Hello\" \"world\") => " (greet "Hello" "world"))


(print "\n* If branches: agreement vs conflict")
(print "  both branches return :number, so return type is :number")

(deftn pick-num [x] (if (number? x) x 0))
(print "  (pick-num 5) => " (pick-num 5))

(print)
(print "  branches disagree (1 vs \"hello\"), so return is :dynamic")

(deftn pick-any [x] (if x 1 "hello"))
(print "  (pick-any true) => " (pick-any true))
(print "  (pick-any nil)  => " (pick-any nil))


(print "\n* Let bindings: propagate inferred types")

(deftn wrap [x]
  (let [y (+ x 1)]
    (string y "!")))
(print "  (wrap 41) => " (wrap 41))


(print "\n* Def inside body")

(deftn sum-str [a b]
  (def total (+ a b))
  (string "sum=" total))
(print "  (sum-str 3 4) => " (sum-str 3 4))


(print "\n* Function composition")

(deftn shout [a b] (string (string a " ") b "!"))
(print "  (shout \"hello\" \"world\") => " (shout "hello" "world"))


(print "\n* Explicit annotation overrides inference")

(deftn override [x :number y] (string x " " y))
(print "  (override 42 \"ok\")  => " (override 42 "ok"))
(print "  (override \"bad\" \"x\") => " (try (override "bad" "x") ([e] e)))


(print "\n* Predicate narrowing constrains inference")
(print "  number? narrows x to :number in the then branch.")
(print "  string accepts :dynamic, so x stays :dynamic overall.")

(deftn ambi [x]
  (if (number? x) (+ x 1) (string x "!")))
(print "  (ambi 5)    => " (ambi 5))
(print "  (ambi \"hi\") => " (ambi "hi"))


(print "\n* Inference disabled")

(enable-inference false)
(deftn raw [x y] (+ x y))
(print "  (raw 3 4)    => " (raw 3 4))
(print "  (raw \"a\" 1) => " (try (raw "a" 1) ([e] e)))
(print "  (no error — inference off, no type checks)")
