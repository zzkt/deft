(import ./helper :prefix "")
(import deft :prefix "")

(print "* type redefinition")

(deftype :replaceable (fn [v] (= v 1)))
(cassert-err "caught redefintion"
             (deftype :replaceable (fn [v] (= v 2))))
(replace-type! :replaceable (fn [v] (= v 3)))
(cassert "replace-type! predicate" (isa? 3 :replaceable) true)

(deftrecord :replaceable-record (field a :number))
(replace-type! :replaceable-record (field b :string))
(cassert "replace-type! record" (isa? (make-replaceable-record "ok") :replaceable-record) true)

(deftrecord :replaceable-guard
  (field a :number)
  (guard (fn [v] (> (v :a) 0))))
(replace-type! :replaceable-guard (field a :number))
(cassert "replace-type! clears guard" (isa? (make-replaceable-guard 0) :replaceable-guard) true)

(defenum :replaceable-enum {"old" 1})
(replace-type! :replaceable-enum {"new" 2})
(cassert "replace-type! enum" (isa? "new" :replaceable-enum) true)

(print-results)
