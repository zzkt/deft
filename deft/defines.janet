# -*- mode: janet; -*-
# deft/defines: typed definition macros (define, deftn, deftrecord, etc.)


(use ./core)
(use ./inference)
(use ./dispatch)
(use ./wolves)

(def *enum-tables*
  "Global table of enum-name -> {key: value} for defenum types."
  @{})

(defn enum-table
  "Retrieve the enumeration map for a named enum type."
  [name]
  (in *enum-tables* name))


(defn- emit-with-fn-type
  ```Register inferred :fn type scheme and return a plain def form.
  The *inferred-fn-types* var is populated during macro expansion
  and persists through jimage serialization.
  When effective-doc is given, sets (get (dyn 'name) :doc).
```
  [name arg-types ret-type fn-form &opt effective-doc]
  (let [fn-scheme (tuple :fn arg-types ret-type)]
    (put *inferred-fn-types* name fn-scheme)
    (if effective-doc
      (tuple 'def name ':doc effective-doc fn-form)
      (tuple 'def name fn-form))))

(defn- infer-and-emit
  "Run inference on a function definition, emit the final form with casts."
  [name arg-names arg-T body-tail return-T docstring &opt kw-map opt-names opt-T rest-info]
  (let [has-kw? (and kw-map (not (empty? kw-map)))
        rest-name (when (and rest-info (rest-info 0)
                             (not= :ignore (rest-info 0)))
                    (rest-info 0))
        rest-ignore (and rest-info (= :ignore (rest-info 0)))
        rest-T (when rest-info (rest-info 1))
        rest-arg-T (if rest-info (or rest-T :dynamic) nil)
        fn-arg-names (build-fn-arg-list arg-names opt-names rest-name rest-ignore kw-map)
        all-names (array/concat (array/slice arg-names) (array/slice opt-names))
        all-declared (array/concat (array/slice arg-T) (array/slice opt-T))]
    (var pos-T arg-T)
    (var opt-resolved-T opt-T)
    (var final-return-T return-T)
    (if *inference-enabled*
      (let [[resolved-args resolved-ret errs]
            (infer-defn name all-names all-declared body-tail return-T)]
        (when (> (length errs) 0)
          (eprintf " type inference: %s:" name)
          (each e errs (eprintf "  %s" e)))
        (set pos-T (array/slice resolved-args 0 (length arg-names)))
        (set opt-resolved-T (array/slice resolved-args (length arg-names)))
        (when (dynamic-type? return-T)
          (set final-return-T resolved-ret)))
      (set final-return-T return-T))
    (let [arg-casts (build-arg-casts
                     name arg-names pos-T
                     opt-names opt-resolved-T
                     rest-name rest-T)
          kw-forms (if has-kw? (build-kw-forms name kw-map) @[])
          return-cast (build-return-cast name final-return-T body-tail)
          fn-body (apply tuple
                         'do (array/concat kw-forms
                                           (array/slice arg-casts)
                                           @[return-cast]))
          kw-arg-T (if has-kw? (map (fn [k] (get kw-map k)) (keys kw-map)) @[])
          all-arg-T (array/concat (array/slice pos-T)
                                      (array/slice opt-resolved-T)
                                      kw-arg-T
                                      (if rest-arg-T @[rest-arg-T] @[]))
          effective-doc (string "(" name " " (string/join (map string arg-names) " ") ")"
                                (if docstring (string "\n\n" docstring) ""))]
      (emit-with-fn-type name all-arg-T final-return-T
        (tuple 'fn name fn-arg-names fn-body)
        effective-doc))))


(defmacro deftn
  ```Define a gradually typed function with optional type annotations.

(deftn name [arg :type ... &opt opt :type ... & rest :type] [:ret] & body)

 Unlike deftfn, type annotations are optional and untyped args are inferred.
 The inferred :fn contract is available at runtime via (fn-type-of 'name).
 ```
  [name args & body-tail]
  (let [has-ret? (or (keyword? (first body-tail)) (compound-type? (first body-tail)))
        raw-ret-type (if has-ret? (first body-tail) :dynamic)
        body (if has-ret? (array/slice body-tail 1) body-tail)
        parsed (parse-flex-args args)
        arg-names (get parsed 0)
        raw-arg-types (get parsed 1)
        kw-map (get parsed 2)
        opt-names (get parsed 3)
        opt-types (get parsed 4)
        rest-info (get parsed 5)]
    (infer-and-emit name arg-names raw-arg-types body raw-ret-type nil kw-map opt-names opt-types rest-info)))


(defn immutable?
  "Check if a type form contains :immutable."
  [T]
  (if (keyword? T)
    (= :immutable T)
    (and (tuple? T)
         (or (= :immutable (first T))
             (some immutable? (array/slice T 1))))))


(defmacro lett
```Typed let* binds (name :type value) or (name value) as mutable typed values.
Supports both tuple [(name :type value) ... ] and array syntax [name type value ... ],
The :type annotation is optional in tuples, but required with arrays.
```
  [bindings & body]
  (if (empty? bindings)
    (tuple 'do ;body)
    (if (and (not (empty? bindings)) (not (tuple? (first bindings))))
      (let [p (partition 3 bindings)]
        (if (not= (length bindings) (* 3 (length p)))
          ~(error ,(string "each binding must be [name type value], got " (describe bindings)))
          (generate-lett-bindings
            (map (fn [t] [(t 0) (t 1) (t 2)]) p) body)))
      (let [p2 (map parse-lett-binding bindings)]
        (if (some nil? p2)
          ~(error ,(string "each binding must be (name [:type] value), got " (describe bindings)))
          (generate-lett-bindings p2 body))))))


(defn- define-fn-form
```Helper for define. Builds a typed/untyped function definition expansion.
Handles typed-params, untyped-params, inference/non-inference cases,
`&opt` optional args, `&` rest args, and `&keys` keyword args.
```
  [name docstring second rest body-start]
  (let [parsed (parse-flex-args second)
        arg-names (get parsed 0)
        raw-arg-types (get parsed 1)
        kw-map (get parsed 2)
        opt-names (get parsed 3)
        opt-types (get parsed 4)
        rest-info (get parsed 5)]
    (if (not (has-typed-params? second))
      (if (not *inference-enabled*)
        (let [out @['defn name]
              has-kw? (not (empty? kw-map))
              rest-name (when (and rest-info (rest-info 0)
                                   (not= :ignore (rest-info 0)))
                          (rest-info 0))
              rest-ignore (and rest-info (= :ignore (rest-info 0)))
              fn-arg-names (build-fn-arg-list arg-names opt-names rest-name rest-ignore kw-map)]
          (when docstring (array/push out docstring))
          (array/push out fn-arg-names)
          (when has-kw?
            (each form (build-kw-forms name kw-map)
              (array/push out form)))
          (each b (array/slice rest body-start)
            (array/push out b))
          (apply tuple out))
        (infer-and-emit name arg-names raw-arg-types
          (array/slice rest body-start)
          :dynamic docstring kw-map opt-names opt-types rest-info))
      (let [candidate (rest body-start)
            ret-is-fn-type (and (tuple? candidate)
                                (= :fn (first candidate))
                                (> (length candidate) 1))
            has-ret? (and (> (length rest) body-start)
                          (or (keyword? candidate)
                              (compound-type? candidate)
                              ret-is-fn-type))
            ret-type (if has-ret? candidate :dynamic)
            body-tail (if has-ret? (array/slice rest (+ body-start 1))
                        (array/slice rest body-start))]
        (infer-and-emit name arg-names raw-arg-types body-tail ret-type docstring kw-map opt-names opt-types rest-info)))))


(defmacro infer-assert-type
 ```Compile-time type assertion. Infers the type of `expr` (using a
clean inference environment) and raises a compile-time error when
the inferred type does not match `expected-type`.

Useful for debugging. place inside a deftn body or after a define
to verify assumptions about inference.
```
  [expected-type expr]
  (let [inferred (infer-expression expr)]
    (if (= inferred expected-type)
      expr
      (errorf "infer-assert-type: expected %s, inferred %s for %q"
              expected-type inferred expr))))


(defmacro with-inference-trace
  ```Execute body with inference tracing enabled.
  Prints every form with inferred type to stderr.
  Restores previous trace state on completion.```
  [& body]
  (with-syms [prev]
    ~(let [,prev *inference-trace-enabled*]
       (enable-inference-trace true)
       (def _result (do ,;body))
       (enable-inference-trace ,prev)
       _result)))


(defmacro define
  ```Unified typed definitions.
     function: (define name doc? [args :types ...] [:ret] & body)
        value: (define name doc? :type expr)
          var: (define name doc? expr)

When args have type annotations, runtime type checks ar inserted.
If :type is :immutable, creates a def, otherwise creates a var.
No type annotation, var.```
  [name & rest]
  (let [has-doc? (string? (rest 0))
        docstring (if has-doc? (rest 0) nil)
        off (if has-doc? 1 0)
        second (rest off)]
    (if (and (not (compound-type? second))
             (or (tuple? second) (array? second)))
      (define-fn-form name docstring second rest (+ off 1))
      (let [typed (or (keyword? second) (compound-type? second))]
        (if typed
          (let [cast-fn (deft-ref 'cast)
                tag-value-fn (deft-ref 'tag-value)
                v (gensym) val (rest (+ off 1))
                def-or-var (if (immutable? second) 'def 'var)
                type-form (if (compound-type? second) (tuple 'quote second) second)
                blame (string name " definition")]
            (tuple def-or-var name
                   (tuple 'let (array v val)
                          (tuple cast-fn v type-form blame)
                          (tuple tag-value-fn v type-form)
                          v)))
          (apply tuple 'var name (array/slice rest off)))))))


(defmacro deftfn
  ```Define a public typed function.
   Every positional arg must have a type spec.```
  [name args ret-type & body]
  (build-typed-fn-form 'defn name args ret-type body))


(defmacro deftfn-
  ```Define a private typed function.```
  [name args ret-type & body]
  (build-typed-fn-form 'defn- name args ret-type body))


(defmacro deftval
  "Define an immutable typed value."
  [name T value]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)
        blame (string name " definition")]
    (with-syms [v]
      ~(def ,name
         (let [,v ,value]
           (,cast-fn ,v ',T ,blame)
           (,tag-value-fn ,v ',T)
           ,v)))))


(defmacro deftv
  "Define a mutable typed variable."
  [name T value]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)
        var-types-ref (deft-ref 'var-types)
        blame (string name " definition")]
    (with-syms [v]
      ~(var ,name
         (let [,v ,value]
           (,cast-fn ,v ',T ,blame)
           (,tag-value-fn ,v ',T)
           (put (,var-types-ref) ',name ',T)
           ,v)))))


(defmacro sett
  "Typed set. assign a new value to a variable with type check."
  [name value]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)
        var-types-ref (deft-ref 'var-types)
        blame (string name " sett")]
    (with-syms [v t]
      ~(let [,v ,value
             ,t (get (,var-types-ref) ',name)]
         (when ,t
           (,cast-fn ,v ,t ,blame)
           (,tag-value-fn ,v ,t))
         (set ,name ,v)))))


(defmacro deftype
 ```Register a new type :keyword with a predicate function or compound form.
If pred is a keyword, creates an alias for an existing type
e.g. (deftype :filepath :string) has same check as :string.```
  [name pred]
  (let [register-type-fn (deft-ref 'register-type)]
    (cond
      (keyword? pred)
        ~(,register-type-fn ',name (fn [v] ,(expand-type-form pred)))
      (and (tuple? pred)
           (find |(= (first pred) $)
                 '(or and not define :array :tuple :table :string)))
        ~(,register-type-fn ',name (fn [v] ,(expand-type-form pred)))
      ~(,register-type-fn ',name ,pred))))


(defmacro deftrecord
  ```Define a typed record with named fields, optional guard, and custom printer.

Generates constructor, accessors, mutators and pp handler based on record/field names
 - make-<name> constructor — accepts required positional args, then optional
   positional args (default nil), then :keyword value pairs for any field.
 - <name>-<field> accessors
 - set-<name>-<field> mutators

Each field clause is either:
  (field name type)         — required positional arg
  (optional name type)      — optional positional arg (default nil)
  (guard pred-fn)           — optional guard predicate
  (print print-fn)          — optional custom printer
```
  [name & clauses]
  (let [register-type-fn (deft-ref 'register-type)
        cast-fn (deft-ref 'cast)
        register-guard-fn (deft-ref 'register-guard)
        pp-str-fn (deft-ref 'pp-str)
        prefix (string/replace ":" "" (string name))
        clause-type? (fn [c tag]
                       (and (tuple? c)
                            (= tag
                               (last (string/split "/" (string (first c)))))))
        req-fields (filter (fn [c] (clause-type? c "field")) clauses)
        opt-fields (filter (fn [c] (clause-type? c "optional")) clauses)
        all-fields (array/concat (array/slice req-fields)
                                 (array/slice opt-fields))
        guard-clauses (filter (fn [c] (clause-type? c "guard"))
                              clauses)
        guard-fn (if (> (length guard-clauses) 0) ((guard-clauses 0) 1) nil)
        print-clauses (filter (fn [c] (clause-type? c "print"))
                              clauses)
        print-fn (if (> (length print-clauses) 0)
                   ((print-clauses 0) 1) nil)
        field-names (map (fn [f] (string/replace ":" "" (string (f 1))))
                         all-fields)
        field-kws (map (fn [f] (keyword (f 1))) all-fields)
        field-types (map (fn [f] (f 2)) all-fields)
        req-kws (map (fn [f] (keyword (f 1))) req-fields)
        make-sym (symbol (string "make-" prefix))
        args-sym (gensym)
        idx-sym (gensym)
        k-sym (gensym)
        pp-sym (symbol (string prefix "-pp-handler"))]

    (with-syms [pv ov env]
      (def do-body @[])
      (array/push do-body
        ~(,register-type-fn ',name
             (fn [,pv] ,(build-struct-pred pv field-kws field-types
                          (map (fn [f] (keyword (f 1))) opt-fields)))))
      (when guard-fn
        (array/push do-body
          ~(,register-guard-fn ',name ,guard-fn)))
      (array/push do-body
        ~(def ,env (fiber/getenv (fiber/current))))
      (each i (range (length field-names))
        (let [fn-name (symbol (string prefix "-" (field-names i)))
              a (gensym)]
          (array/push do-body
            ~(eval ,(struct-eval-form fn-name (tuple a)
                     (tuple 'get a (field-kws i)))
                   ,env))))
      (each i (range (length field-names))
        (let [fn-name (symbol (string "set-" prefix "-" (field-names i)))
              o (gensym)
              v (gensym)]
          (array/push do-body
            ~(eval ,(struct-eval-form fn-name (tuple o v)
                     (tuple 'put o (field-kws i) v)
                     (tuple cast-fn o name (string "set-" prefix "-" (field-names i)))
                     o)
                   ,env))))

      (let [ppv (gensym)
            pp-sym-arg (gensym)
            pushers (map (fn [kw]
                           (tuple 'string kw "="
                                  (tuple pp-str-fn (tuple 'get ppv kw))))
                         field-kws)
            default-pp (tuple 'fn (tuple ppv)
                               (tuple 'string prefix "("
                                      (tuple 'string/join (apply tuple 'array pushers) ", ") ")"))
            pp-handler (if print-fn
                         (tuple 'fn (tuple pp-sym-arg) (tuple print-fn pp-sym-arg))
                         default-pp)
            req-kvs (interleave req-kws
                                (map (fn [i] (tuple 'get args-sym i))
                                     (range (length req-kws))))
            opt-bodies (map (fn [f]
                              (let [kw (keyword (f 1))]
                                (tuple 'if
                                  (tuple 'and
                                    (tuple '< idx-sym
                                            (tuple 'length args-sym))
                                    (tuple 'not (tuple 'keyword?
                                                        (tuple 'get args-sym idx-sym))))
                                  (tuple 'do
                                    (tuple 'put ov kw (tuple 'get args-sym idx-sym))
                                    (tuple '++ idx-sym))
                                  (tuple 'put ov kw nil))))
                            opt-fields)
            kw-loop (tuple 'while (tuple '< idx-sym (tuple 'length args-sym))
                     (tuple 'do
                       (tuple 'def k-sym (tuple 'get args-sym idx-sym))
                       (tuple 'when (tuple 'keyword? k-sym)
                         (tuple 'put ov k-sym
                                 (tuple 'get args-sym (tuple '+ idx-sym 1))))
                       (tuple '+= idx-sym 2)))
            body-parts @[(tuple 'def ov (apply tuple 'table req-kvs))
                         (tuple 'var idx-sym (length req-kws))]
            _ (each b opt-bodies (array/push body-parts b))
            _ (array/push body-parts kw-loop)
            _ (array/push body-parts (tuple 'put ov :pp pp-sym))
            _ (array/push body-parts (tuple cast-fn ov
                                            (tuple 'quote name)
                                            (string "make-" prefix)))
            _ (array/push body-parts ov)
            constructor (tuple 'fn (tuple '& args-sym)
                                (apply tuple 'do body-parts))]
        (array/push do-body
          (tuple 'def pp-sym pp-handler))
        (array/push do-body constructor)
        (tuple 'def make-sym (apply tuple 'do do-body))))))


(defmacro defenum
 ```Define an enumeration type from a key-value table.
Generates `<name>`, `<name>-extend`, and `<name>-remove` helpers.
```
  [name kv-map]
  (let [register-type-fn (deft-ref 'register-type)
        enum-tables-ref (deft-ref '*enum-tables*)
        prefix (string/replace ":" "" (string name))
        accessor-name (symbol prefix)
        extend-name (symbol (string prefix "-extend"))
        remove-name (symbol (string prefix "-remove"))
        # accessor
        eval-accessor
        (let [body (tuple
                    'in
                    (tuple enum-table (tuple 'quote name))
                    'k)
              defn-form (tuple
                         'defn accessor-name
                         "Look up a value in " prefix " by key."
                         '[k]
                         body)]
          (tuple 'quote defn-form))
        # extender
        eval-extend
        (let [et-ref (tuple enum-table (tuple 'quote name))
              body (tuple
                    'do
                    (tuple 'put et-ref 'k 'v)
                    'v)
              defn-form (tuple
                         'defn extend-name
                         "Add or update a key-value pair in "
                         prefix ". Returns the new value."
                         '[k v]
                         body)]
          (tuple 'quote defn-form))
        # remover
        eval-remove
        (let [et-ref (tuple enum-table (tuple 'quote name))
              old-sym (gensym)
              body (tuple
                    'let
                    [old-sym (tuple 'in et-ref 'k)]
                    (tuple 'put et-ref 'k nil)
                    old-sym)
              defn-form (tuple
                         'defn remove-name
                         "Remove a key from "
                         prefix " and return its value."
                         '[k]
                         body)]
          (tuple 'quote defn-form))]
    # macro body
    ~(do (put (,enum-tables-ref) ',name (merge-into @{} ,kv-map))
         (,register-type-fn
           ',name
           (fn [v] (and (string? v)
                        (not= nil (in (in (,enum-tables-ref) ',name) v)))))
        (let [acc-env (fiber/getenv (fiber/current))]
          (eval ,eval-accessor acc-env)
          (eval ,eval-extend acc-env)
          (eval ,eval-remove acc-env)))))


(defmacro deftwrap
  ```Create a typed wrapper around an existing function.

Supports both function and arrow syntax:
    (deftwrap safe-join string/join (:fn [:array :string] :string))
    (deftwrap safe-inc + (:fn [:number -> :number]))
    (deftwrap safe-length length (:fn [:string] :number) "docstring")

Expands to a `defn` with runtime casts.
```
  [wrapper-name wrapped-fn type-scheme &opt docstring]
  (let [parsed (if (= nil (get type-scheme 2))
                 (parse-fn-type type-scheme)
                 {:args (get type-scheme 1) :ret (get type-scheme 2)})
        arg-types (parsed :args)
        ret-type (parsed :ret)
        arg-names (map (fn [i] (symbol (string "a" i)))
                       (range (length arg-types)))
        arg-spec @[]
        _ (for i 0 (length arg-types)
            (array/push arg-spec (arg-names i))
            (array/push arg-spec (arg-types i)))
        call-body (apply tuple wrapped-fn arg-names)
        body (if docstring [docstring call-body] [call-body])]
    (build-typed-fn-form 'defn wrapper-name arg-spec ret-type body)))
