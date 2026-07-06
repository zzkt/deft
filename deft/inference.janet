# -*- mode: janet; -*-
# deft/inference: bidirectional type inference

(use ./dispatch)
(use ./core)
(use ./unify)

(var *infer-substitution*
  ```Mutable substitution table used during inference.
Maps type variables (as gensyms) to resolved types.
Reset at the start of each infer-defn call.```
  @{})

# Forward declarations required for mutually recursive functions
# DO NOT change evaluation order of these vars and the functions
# set further on. (c.f. fragile expansion scope)
(var infer-tuple nil)
(var infer-if nil)
(var infer-binding nil)
(var infer-seq nil)
(var infer-fn nil)
(var infer-let nil)
(var infer-call nil)
(var infer-scheme-call nil)
(var infer-dynamic-call nil)

(defn- default-to-dynamic
  "Replace all unbound type variables with :dynamic."
  [t]
  (cond
    (type-var? t) :dynamic
    (tuple? t)
    (let [op (first t)]
      (case op
        :fn (tuple :fn
              (map default-to-dynamic (get t 1))
              (default-to-dynamic (get t 2)))
        (apply tuple op (map default-to-dynamic (array/slice t 1)))))
    t))

(defn- unify-and-record
  ```Unify a and b, record the substitution, and return unified type.
Returns :dynamic on unification failure.
```
  [a b]
  (try
    (let [s (unify a b)]
      (merge-into *infer-substitution* s)
      (apply-subst s a))
    ([_] :dynamic)))

(defn fresh-arg-env
  ```Build an inference env from param names and declared types.
Declared types, enums, and compound types are used as is, unannotated
are given fresh type vars.
```
  [names types]
  (let [env @{}]
    (each i (range (length names))
      (def raw-type (if (< i (length types)) (types i) nil))
      (def declared? (and raw-type
                          (or (keyword? raw-type)
                              (fn-type? raw-type)
                              (compound-type? raw-type))
                          (not (dynamic-type? raw-type))))
      (put env (names i)
           (if declared? raw-type (fresh-tvar))))
    env))

(defn- subst-resolve
  ```Resolve a single type through substitution.
Returns the type variable itself if not (yet) resolved.```
  [t]
  (if (type-var? t)
    (get *infer-substitution* t t)
    t))

(defn- lookup-type
  ```Look up a symbol's inferred type, resolving through substitution.
Returns :dynamic for unresolved variables.
```
  [env sym]
  (or (subst-resolve (get env sym)) :dynamic))

(defn- trace-print
  ```Print a formatted line to stderr when inference tracing is enabled.
fmt is a format string (supports %p); rest are values to format.
```
  [fmt & args]
  (when *inference-trace-enabled*
    (apply eprintf fmt args)))

(defn- infer-syn
  ```Synthesize type of expression form.
env is extended in place by def/var/let.
Prints trace output to stderr when inference tracing is enabled.```
  [env form]
  (def result
    (cond
      (string? form) :string
      (number? form) :number
      (boolean? form) :boolean
      (keyword? form) :keyword
      (nil? form) :nil
      (symbol? form) (lookup-type env form)
      (tuple? form) (infer-tuple env form)
      :dynamic))
  (trace-print "  infer %p => %p" form (default-to-dynamic result))
  result)

(set infer-tuple
  (fn [env form]
    (let [op (first form)
          args (array/slice form 1)]
      (case op
        'if (infer-if env args)
        'def (infer-binding env args)
        'var (infer-binding env args)
        'do (infer-seq env args)
        'fn (infer-fn env args)
        'let (infer-let env args)
        'quote :dynamic
        'while (infer-seq env args)
        'for (let [iv (get form 1)] (put env iv :number) (infer-seq env (array/slice form 4)))
        'each (let [iv (get form 1)] (put env iv :dynamic) (infer-seq env (array/slice form 3)))
        (infer-call env op form args)))))

(defn- merge-substitutions
  ```Merge then-branch and else-branch substitutions into *infer-substitution*.
When a variable resolves to different types in each branch, unify to :dynamic.```
  [then-subst else-subst]
  (each [k v] (pairs then-subst)
    (def ev (get else-subst k :not-found))
    (put *infer-substitution* k
      (if (= :not-found ev) v
        (if (= v ev) v :dynamic))))
  (each [k v] (pairs else-subst)
    (when (= :not-found (get then-subst k :not-found))
      (put *infer-substitution* k v))))

(defn- try-narrow-predicate
  ```Check if cond is a predicate call and if so, return the narrow info.
Returns [sym narrow-type saved-type] or nil.```
  [env cond]
  (when (and (tuple? cond) (symbol? (first cond)) (> (length cond) 1))
    (def narrow (get *predicate-narrowing* (string (first cond))))
    (when narrow
      (def sym (in cond 1))
      (def saved (get env sym))
      (trace-print "  narrow: %p -> %p (was %p)" sym narrow saved)
      [sym narrow saved])))

(defn- narrow-conflict?
  "Check if declared type conflicts with narrowed type."
  [saved-type narrow-type]
  (and (keyword? saved-type) (keyword? narrow-type)
       (not (dynamic-type? saved-type))
       (not= saved-type narrow-type)))

(defn- deep-sym?
  "Check if a symbol appears anywhere in a form (including nested tuples)."
  [form sym]
  (or (and (symbol? form) (= form sym))
      (and (tuple? form) (some |(deep-sym? $ sym) form))))

# see above: forward declarations
(set infer-if
  (fn [env [cond then & else]]
    (def narrow-info (try-narrow-predicate env cond))
    (when narrow-info
      (def [_ narrow-type saved-type] narrow-info)
      (when (narrow-conflict? saved-type narrow-type)
        (trace-print "  NARROWS CONFLICT: %p vs %p" saved-type narrow-type)
        (error (string "narrows conflict: " saved-type " vs " narrow-type))))
    (infer-syn env cond)
    (def saved-subst (copy-subst *infer-substitution*))
    # Apply narrowing for then-branch
    (when narrow-info
      (put env (narrow-info 0) (narrow-info 1)))
    (def then-type (infer-syn env then))
    (def then-subst (copy-subst *infer-substitution*))
    # Restore env and substitution for else-branch
    (when narrow-info
      (put env (narrow-info 0) (narrow-info 2)))
    (set *infer-substitution* saved-subst)
    (def else-type (if (first else)
                     (infer-syn env (first else))
                     :nil))
    (def else-subst (copy-subst *infer-substitution*))
    (merge-substitutions then-subst else-subst)
    (def combined-type (unify-and-record then-type else-type))
    # Propagate narrowing. constrain tvar to narrow-type only if
    # the else-branch left it unconstrained (i.e. tvar is not
    # referenced in the else branch).
    (when (and narrow-info (type-var? (narrow-info 2)))
      (def narrow-sym (narrow-info 0))
      (def var-appears-in-else? (some |(deep-sym? $ narrow-sym) else))
      (unless var-appears-in-else?
        (unify-and-record (narrow-info 2) (narrow-info 1))))
    combined-type))

# see above: forward declarations
(set infer-binding
  (fn [env [sym val]]
    (def val-type (infer-syn env val))
    (put env sym (subst-resolve val-type))
    val-type))

# see above: forward declarations
(set infer-seq
  (fn [env forms]
    (var ret-type :dynamic)
    (each f forms
      (set ret-type (infer-syn env f)))
    ret-type))

# see above: forward declarations
(set infer-fn
  (fn [env [params & body]]
    (def param-names (filter (fn [p] (and (symbol? p) (not= '& p))) params))
    (def has-rest? (some |(= '& $) params))
    (def rest-pos (index-of '& params))
    (def rest-name (when has-rest? (params (+ rest-pos 1))))
    (def fn-env (merge-into @{} env))
    (each p param-names
      (put fn-env p (fresh-tvar)))
    (when rest-name
      (put fn-env rest-name (tuple :array :dynamic)))
    (let [body-type (infer-seq fn-env body)
          arg-types (map (fn [p] (lookup-type fn-env p)) param-names)]
      (tuple :fn arg-types body-type))))

# see above: forward declarations
(set infer-let
  (fn [env [bindings & body]]
    (let [pairs (partition 2 bindings)]
      (each [sym val] pairs
        (put env sym (subst-resolve (infer-syn env val)))))
    (infer-seq env body)))

# see above: forward declarations
(set infer-call
  (fn [env op form args]
    (when form (+ 1 1))  # lint -> compile error: binding form is unused
    (def op-str (when (symbol? op) (string op)))
    (def scheme (if op-str (get *op-type-schemes* op-str)))
    (if scheme
      (infer-scheme-call env args scheme)
      (infer-dynamic-call env op args))))

# see above: forward declarations
(defn- unify-arg-with-param
  ```Unify an inferred arg-type with a scheme param-type.
Returns updated subst or nil on failure.
```
  [subst arg-type param-type]
  (try
    (compose-subst
      (unify (apply-subst subst arg-type)
             (apply-subst subst param-type))
      subst)
    ([_] subst)))

# see above: forward declarations
(set infer-scheme-call
  (fn [env args scheme]
    (let [param-types (get scheme 1)
          ret-type (get scheme 2)
          n-params (length param-types)]
      (var subst @{})
      (each i (range (length args))
        (def arg-type (infer-syn env (args i)))
        (def param-idx (min i (- n-params 1)))
        (def param-type (get param-types param-idx))
        (when (and (= :mutable param-type) (= :immutable arg-type))
          (error (string "expected mutable, got " arg-type)))
        (set subst (unify-arg-with-param subst arg-type param-type)))
      (merge-into *infer-substitution* subst)
      (default-to-dynamic (apply-subst subst ret-type)))))

# see above: forward declarations
(set infer-dynamic-call
  (fn [env op args]
    (if (not (symbol? op)) :dynamic
      (do (def fn-type (lookup-type env op))
        (if (not (and (tuple? fn-type) (= :fn (first fn-type)))) :dynamic
        (let [param-types (get fn-type 1)
              ret-type (get fn-type 2)]
          (var subst @{})
          (each i (range (length args))
            (when (< i (length param-types))
              (def arg-type (infer-syn env (args i)))
              (set subst (unify-arg-with-param subst arg-type (param-types i)))))
          (merge-into *infer-substitution* subst)
          (default-to-dynamic (apply-subst subst ret-type))))))))


(defn infer-syn-form
  "Public: synthesize the type of a form, resolving type vars to :dynamic."
  [env form]
  (default-to-dynamic (infer-syn env form)))

(defn infer-expression
  ```Infer the type of an arbitrary expression using a fresh inference environment.
Returns the resolved type(s) and accepts an optional initial type env table.
Use at runtime (or compile-time in macros) for ad-hoc type queries.```
  [form &opt env]
  (set *infer-substitution* @{})
  (default-to-dynamic (infer-syn (or env @{}) form)))

(defn infer-expression-full
  ```Like infer-expression but also returns full substitution and raw type(s).
Useful for debugging.```
  [form &opt env]
  (set *infer-substitution* @{})
  (def raw (infer-syn (or env @{}) form))
  (def resolved (default-to-dynamic raw))
  (def subst (table ;(mapcat identity (pairs *infer-substitution*))))
  {:type resolved :raw raw :substitution subst})

(defn infer-chk-form
  "Check a form against an expected type. Returns nil or an error string."
  [env form expected]
  (try
    (let [syn-type (infer-syn env form)]
      (unify syn-type expected)
      nil)
    ([e]
      (string "type mismatch: expected " expected ", got: " (string e)))))

(defn infer-defn
  ```Bidirectional inference for deftn/define function definitions.
Resets the global substitution, runs inference, resolves arg types.
Returns [resolved-arg-types, resolved-return-type, errors].
```

  [name arg-names arg-types body ret-declared]
  (trace-print "\n--- infer-defn: %p ---" name)
  (trace-print "  args: %p with types: %p" arg-names arg-types)
  (trace-print "  body: %p" body)
  (set *infer-substitution* @{})
  (def env (fresh-arg-env arg-names arg-types))
  (def errors @[])
  (def body-type
    (try
      (infer-seq env body)
      ([e]
        (array/push errors (string "inference error in " name ": " e))
        :dynamic)))
  (def resolved-args
    (map (fn [n] (default-to-dynamic (lookup-type env n))) arg-names))
  (def final-ret-type
    (if (keyword? ret-declared)
      ret-declared
      (default-to-dynamic body-type)))
  (trace-print "  resolved args: %p -> ret: %p" resolved-args final-ret-type)
  [resolved-args final-ret-type errors])
