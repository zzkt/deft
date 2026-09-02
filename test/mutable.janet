# -*- mode: janet; -*-
# deft tests: mutability

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* mutable / immutable types")

(enable-checking true)

#  :mutable predicates
(cassert ":mutable on table" ((type-predicate :mutable) @{}) true)
(cassert ":mutable on array" ((type-predicate :mutable) @[]) true)
(cassert ":mutable on buffer" ((type-predicate :mutable) (buffer "")) true)
(cassert ":mutable on string? no" ((type-predicate :mutable) "hi") false)
(cassert ":mutable on number? no" ((type-predicate :mutable) 42) false)

#  :immutable predicates
(cassert ":immutable on string" ((type-predicate :immutable) "hi") true)
(cassert ":immutable on number" ((type-predicate :immutable) 42) true)
(cassert ":immutable on table? no" ((type-predicate :immutable) @{}) false)

#  :mutable inference from put
(deftn putter [t k v]
  (put t k v))

(var pt (putter @{} :a 1))
(cassert "putter infers :mutable" (get pt :a) 1)
(cassert-err "putter catches bad arg" (putter "bad" :a 1))

#  :mutable inference from array/push
(deftn pusher [arr v]
  (array/push arr v))

(cassert "pusher infers :mutable" (last (pusher @[1] 2)) 2)
(cassert-err "pusher catches bad arg" (pusher "bad" 42))

#  :mutable inference from update
(deftn updater [t f]
  (update t :count f))

(var ut (updater @{:count 1} inc))
(cassert "updater infers :mutable" (get ut :count) 2)

#  explicit :mutable
(deftn mut-param [x :mutable] x)
(def mut-result (mut-param @{:a 1}))
(cassert "mut-param accepts table" (get mut-result :a) 1)
(cassert-err "mut-param rejects string" (mut-param "hi"))

#  explicit :immutable
(deftn imm-param [x :immutable] x)
(cassert "imm-param accepts string" (imm-param "hi") "hi")
(cassert-err "imm-param rejects table" (imm-param @{}))

#  deftcheck: mutable ops vs :immutable
(def dc1 (check-form '(deftn clean [t :mutable] :dynamic (put t :a 1))))
(cassert "deftcheck mutable clean" (= (length dc1) 0) true)

(def dc2 (check-form '(deftn bad [s :immutable] :dynamic (put s :a 1))))
(cassert "deftcheck immutable in mutable op flagged" (> (length dc2) 0) true)

(def dc3 (check-form '(deftn ok [s :immutable] :string (string s))))
(cassert "deftcheck immutable no mutable op clean" (= (length dc3) 0) true)

#  deftcheck: immutable arg used in array/push
(def dc4 (check-form '(deftn bad2 [s :immutable] :dynamic (array/push s 1))))
(cassert "deftcheck array/push on immutable" (> (length dc4) 0) true)

(def dc5 (check-form '(deftn bad3 [s :immutable] :dynamic (update s :a inc))))
(cassert "deftcheck update on immutable" (> (length dc5) 0) true)

#  mutable/immutable bindings
(var mv 10)
(cassert "var binding" mv 10)
(set mv 20)
(cassert "var set" mv 20)

(def mc 99)
(cassert "def binding" mc 99)
(cassert-err "def cannot set" (eval (tuple 'set 'mc 0)))

(define d-mut (:or :number :string) 42)
(cassert "define mutable val" d-mut 42)
(set d-mut "hi")
(cassert "define mutable set" d-mut "hi")

(define d-imm (:and :number :immutable) 1.618)
(cassert "define immutable val" d-imm 1.618)
(cassert-err "define immutable cannot set" (eval (tuple 'set 'd-imm 3.14)))

(print-results)
