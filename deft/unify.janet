# -*- mode: janet; -*-
# deft/unify: unification with gradual semantics

(var *tvar-counter* 0)

(defn fresh-tvar
  "Create a fresh type variable."
  []
  (set *tvar-counter* (+ 1 *tvar-counter*))
  (symbol (string "?tv" *tvar-counter*)))

(defn type-var?
  "Check if t is a type variable."
  [t]
  (symbol? t))

(defn apply-subst
  "Apply a substitution (table of tv -> type) to a type t."
  [subst t]
  (cond
    (type-var? t) (let [resolved (get subst t)]
                    (if resolved
                      (apply-subst subst resolved)
                      t))
    (tuple? t)
    (let [op (first t)]
      (case op
        :fn (tuple :fn
              (map (partial apply-subst subst) (get t 1))
              (apply-subst subst (get t 2)))
        (apply tuple op (map (partial apply-subst subst) (array/slice t 1)))))
    t))


(defn copy-subst
  "Create a copy of a substitution table."
  [subst]
  (merge-into @{} subst))

(defn compose-subst
  "Compose two substitutions. s1 after s2."
  [s1 s2]
  (def result (copy-subst s2))
  (each [k v] (pairs s1)
    (put result k (apply-subst result v)))
  result)

(defn occurs?
  "Check if type variable tv occurs in type t (occurs check)."
  [tv t]
  (cond
    (= tv t) true
    (type-var? t) false
    (tuple? t) (some (partial occurs? tv) (array/slice t 1))
    false))

(defn unify
  ```Unify two types. Returns a substitution table.
Type :dynamic unifies with anything (gradual semantics).
Raises an error on unification failure.
```
  [a b]
  (cond
    (or (= :dynamic a) (= :dynamic b)) @{}
    (= a b) @{}
    (type-var? a) (if (occurs? a b)
                    (error (string "occurs check failed: " a " in " b))
                    @{a b})
    (type-var? b) (if (occurs? b a)
                    (error (string "occurs check failed: " b " in " a))
                    @{b a})
    (and (tuple? a) (tuple? b) (= (first a) (first b)))
    (let [op (first a)]
      (if (= op :fn)
        (let [args-a (get a 1) args-b (get b 1)]
          (if (not= (length args-a) (length args-b))
            (error (string "arity mismatch"))
            (do (var subst @{})
              (each i (range (length args-a))
                (set subst (compose-subst
                             (unify (apply-subst subst (args-a i))
                                    (apply-subst subst (args-b i)))
                             subst)))
              (let [rs (unify (apply-subst subst (get a 2))
                              (apply-subst subst (get b 2)))]
                (compose-subst rs subst)))))
        (do (var subst @{})
          (each i (range 1 (length a))
            (let [ai (get a i) bi (get b i)]
              (when (and ai bi (not= ai bi))
                (set subst (compose-subst
                             (unify (apply-subst subst ai)
                                    (apply-subst subst bi))
                             subst)))))
          subst)))
    (and (keyword? a) (keyword? b) (= a b)) @{}
    (error (string "cannot unify " a " and " b))))
