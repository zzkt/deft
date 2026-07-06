# -*- mode: janet; -*-
# deft/core: type predicates, casting, function types

(use ./dispatch)

(var *type-registry*
  "Table mapping type labels (keywords) to their predicate functions."
  (table))

(var *value-types*
  "Table mapping values to their declared type label."
  (table))

(var *var-types*
  "Table mapping variable names (symbols) to their declared type."
  (table))

(def deft-refs
  "Reference table for macroexpansion access to runtime declarations (see: init.janet)"
  @{})

(defn deft-ref
  "Look up a deft reference by key."
  [k]
  (get deft-refs k))

(defn register-type
  "Register a named type predicate."
  [name pred]
  (put *type-registry* name pred))

(defn unregister-type
  "Remove a named type predicate."
  [name]
  (var old (get *type-registry* name))
  (put *type-registry* name nil)
  old)

(defn untype
  "Alias for unregister-type."
  [name]
  (unregister-type name))

(defn tag-value
  "Associate a value with its declared type."
  [v type]
  (put *value-types* v type))

# Shadow built-in type form
(def core-type type)

(defn type
  "Return the declared type of a value."
  [v]
  (or (get *value-types* v) (core-type v)))

(defn fn-type?
  "Check if a form is a function. i.e. (:fn [args... -> ret])"
  [t]
  (and (tuple? t)
       (or (= :fn (first t))
           (= 'fn (first t))) (> (length t) 1)))

(defn parse-fn-type
  "Parse (:fn [arg ... -> ret]) into {:args [arg ... ] :ret ret}"
  [t]
  (let [parts (get t 1)]
    (var i 0)
    (var args @[])
    (while (< i (length parts))
      (let [p (parts i)]
        (if (or (= p :->) (= p (quote ->)))
          (break)
          (do (array/push args p) (++ i)))))
    (let [ret (if (< i (length parts)) (parts (+ i 1)) :dynamic)]
      {:args (tuple ;args) :ret ret})))

(def *type-predicates*
  "Built-in type predicates keyed by name (as :keyword)."
  (table
    # explicit dynamic type
    :dynamic (fn [_] true)
    :any (fn [_] true)
    # core Janet types
    :nil nil?
    :boolean boolean?
    :number number?
    :integer int?
    :string string?
    :keyword keyword?
    :symbol symbol?
    :buffer buffer?
    :bytes buffer?
    :table table?
    :struct struct?
    :array array?
    :tuple tuple?
    :function function?
    # as infered from core types
    :callable (fn [v] (or (function? v) (cfunction? v)))
    :mutable (fn [v] (or (table? v) (array? v) (buffer? v) (fiber? v)))
    :immutable (fn [v] (not (or (table? v) (array? v) (buffer? v) (fiber? v))))))

(put *type-predicates* :type
  (fn [v]
    (or (function? v) (cfunction? v) (fn-type? v)
        (and (keyword? v)
             (not (nil? (or (get *type-predicates* v)
                            (get *type-registry* v))))))))

(defn compound-type?
  "Check if T is a compound type form."
  [T]
  (and (tuple? T) (or (= :or (first T)) (= 'or (first T))
                      (= :and (first T)) (= 'and (first T))
                      (= :not (first T)) (= 'not (first T))
                      (= :array (first T))
                      (= :tuple (first T))
                      (= :table (first T))
                      (= :string (first T)))))

(var *predicate-cache*
  "Cache for compound-type-form -> predicate function."
  (table))

# forward declaration required for mutual recursion with build-compound-predicate
(var type-predicate nil)

(defn- build-compound-predicate
  "Build a predicate function for a compound type form (or/and/not/array/tuple/table/string)."
  [T]
  (let [op (keyword (first T))]
    (case op
      :or (let [sub-preds (map type-predicate (array/slice T 1))]
            (fn [v] (some (fn [p] (p v)) sub-preds)))
      :and (let [sub-preds (map type-predicate (array/slice T 1))]
             (fn [v]
               (var ok true)
               (each p sub-preds
                 (when (not (p v))
                   (set ok false) (break)))
               ok))
      :not (let [pred (type-predicate (T 1))]
             (fn [v] (not (pred v))))
      :array (let [elem-pred (type-predicate (T 1))]
               (fn [v] (and (array? v) (all elem-pred v))))
      :tuple (let [elem-pred (type-predicate (T 1))]
               (fn [v] (and (tuple? v) (all elem-pred v))))
      :table (let [key-pred (type-predicate (T 1))
                   val-pred (type-predicate (T 2))]
               (fn [v]
                 (when (table? v)
                   (var ok true)
                   (each [k vv] (pairs v)
                     (when (or (not (key-pred k)) (not (val-pred vv)))
                       (set ok false) (break)))
                   ok)))
      :string (let [elem-pred (type-predicate (T 1))]
                (fn [v]
                  (and (string? v)
                       (all (fn [b] (elem-pred (string/from-bytes b))) v)))))))

# the thankless thunk post declaration of build-compound-predicate
(set type-predicate
  (fn [t]
  (cond
    (compound-type? t)
    (or (get *predicate-cache* t)
        (let [pred (build-compound-predicate t)]
          (put *predicate-cache* t pred)
          pred))
    (fn-type? t) (fn [v] (function? v))
    (keyword? t) (or (get *type-predicates* t)
                     (get *type-registry* t)
                     (errorf "unknown type: %q" t))
    (or (get *type-registry* t)
        (errorf "unknown type: %q" t)))))


(defn registered-types
  "Return the table of registered type predicates."
  []
  *type-registry*)

(defn isa?
  "Check if a value satisfies a type predicate."
  [v T]
  ((type-predicate T) v))

(defn dynamic-type?
  "Check if T is :dynamic or :any type."
  [T]
  (or (= :dynamic T) (= :any T)))

(defn type?
  ```Check if a value is a registered or built-in type keyword.
Returns true if keyword is regestered as a type (e.g. :number)
Returns false for non-type values. (e.g. "number").
```
  [T]
  (if (fn-type? T)
    true
    (and (keyword? T)
         (not= nil (or (get *type-predicates* T)
                        (get *type-registry* T))))))

(defn type=
  "Check if a value's type matches a given type."
  [v T]
  (= (type v) T))

(defn type-name
  "Return the keyword name for a type value, or nil for anonymous predicates."
  [T]
  (when (keyword? T) T))

(defn consistent?
  "Consistency relation from Siek & Taha.
:dynamic is consistent with every type; otherwise requires equality."
  [a b]
  (or (= a b) (dynamic-type? a) (dynamic-type? b)))

(defn fn-type-of
  "Retrieve the inferred or declared type scheme for a named function.
Returns nil when no scheme has been recorded."
  [name]
  (get *inferred-fn-types* name))

(defn type-info
  ```Return a type-info table for a value v.
Keys are :runtime-type (the raw Janet type)
     and  :deft-type (if the value was tagged via deftval/deftv/define)
For info about a typed function use (fn-type-of 'name).
```
  [v]
  (def info @{:runtime-type (core-type v)})
  (let [dt (get *value-types* v)]
    (when dt (put info :deft-type dt)))
  info)

(defn cast
  ```Validate and tag a value with a type at runtime.
When *checking-enabled* is true, raises an error with blame context if
the value does not satisfy target-type predicate. For function types,
wraps the value in a type checking wrapper for args and return on every call.
Returns the (possibly wrapped) value.
```
  [value target-type blame]
  (var result value)
  (when *checking-enabled*
    (unless (dynamic-type? target-type)
      (if (fn-type? target-type)
        (do
          (unless (function? value)
            (errorf "Type error (%s): expected function, got %s"
                    blame (describe value)))
          (let [parsed (parse-fn-type target-type)
                arg-types (parsed :args)
                ret-type (parsed :ret)]
            (set result
              (fn [& args]
                (each i (range (length arg-types))
                  (let [a (get args i)
                        t (get arg-types i)]
                    (when (not= nil a)
                      (cast a t (string blame ":arg" (string i) " (caller blamed)")))))
                (let [result-val (apply value args)]
                  (cast result-val ret-type
                        (string blame ":return (function blamed)")))))))
        (let [pred (type-predicate target-type)]
          (unless (pred value)
            (errorf "Type error (%s): expected %q, got %s"
                    blame target-type (describe value)))))))
  result)
