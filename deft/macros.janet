# -*- mode: janet; -*-
# deft/macros: define, deftv, deftype, deftrecord, etc.

(use ./core)
(use ./inference)
(use ./dispatch)

(def *enum-tables*
  "Global table of enum-name -> {key: value} for defenum types."
  @{})

(defn enum-table
  "Retrieve the enumeration map for a named enum type."
  [name]
  (in *enum-tables* name))


(defn- parse-arg-pairs
  "Split flat [name type name type ...] list into (names, types)."
  [args]
  (let [pairs (partition 2 args)
        names (map first pairs)
        types (map last pairs)]
    (when (not= (length args) (* 2 (length names)))
      (errorf "Every argument must be followed by a type spec; got args %q" args))
    (tuple (tuple ;names) types)))


(defn- build-arg-casts
  [name arg-names arg-types]
  (let [cast-fn (deft-ref 'cast)]
    (seq [i :range [0 (length arg-names)]]
      (let [n (arg-names i) t (arg-types i)
            needs-quote? (or (fn-type? t) (compound-type? t))
            type-form (if needs-quote? (tuple 'quote t) t)]
        (if (fn-type? t)
          (tuple 'def n
                 (tuple cast-fn n type-form
                        (string (string name ":" n) " (caller blamed)")))
          (tuple cast-fn n type-form
                 (string (string name ":" n) " (caller blamed)")))))))


(defn- build-return-cast
  [name ret-type body]
  (let [cast-fn (deft-ref 'cast)
        ret-form (if (or (fn-type? ret-type) (compound-type? ret-type))
                   (tuple 'quote ret-type)
                   ret-type)]
    (tuple cast-fn (apply tuple 'do body)
           ret-form
           (string name " return (function blamed)"))))


(defn- type-spec?
  [x]
  (or (keyword? x) (fn-type? x) (compound-type? x)))


(defn- parse-flex-args
  [args]
  (var names @[])
  (var types @[])
  (var i 0)
  (while (< i (length args))
    (let [cur (args i)]
      (if (type-spec? cur)
        (errorf "unexpected type %q without preceding name (position %d)" cur i)
        (do
          (array/push names cur)
          (let [next-idx (+ i 1)]
            (if (and (< next-idx (length args)) (type-spec? (args next-idx)))
              (do (array/push types (args next-idx)) (++ i))
              (array/push types :dynamic))))))
    (++ i))
  (tuple (tuple ;names) types))


(defn- has-typed-params?
  [args]
  (some type-spec? args))


(defn- expand-type-form
  [expr]
  (let [type-pred-fn (deft-ref 'type-predicate)]
    (if (keyword? expr)
      (tuple (tuple type-pred-fn expr) 'v)
      (if (tuple? expr)
        (case (first expr)
          'or  (apply tuple 'or (map expand-type-form (array/slice expr 1)))
          'and (apply tuple 'and (map expand-type-form (array/slice expr 1)))
          'not (tuple 'not (expand-type-form (expr 1)))
           'define (let [define-args (expr 1)
                         body (array/slice expr 2)
                         arg-names (filter |(not (keyword? $)) define-args)
                         arg-types (filter keyword? define-args)
                         checks (map (fn [n t]
                                       (tuple (tuple type-pred-fn t) n))
                                     arg-names arg-types)]
                     ~((fn [,;arg-names] (and ,;checks ,;body)) v))
           ':array (tuple (tuple type-pred-fn (tuple 'quote expr)) 'v)
           ':tuple (tuple (tuple type-pred-fn (tuple 'quote expr)) 'v)
           ':table (tuple (tuple type-pred-fn (tuple 'quote expr)) 'v)
           ':string (tuple (tuple type-pred-fn (tuple 'quote expr)) 'v)
           (tuple expr 'v))
        (errorf "invalid type form: %q" expr)))))


(defn- build-struct-pred
  [pv field-kws field-types guard-fn &opt opt-kws]
  (let [type-pred-fn (deft-ref 'type-predicate)
        opt-set (if opt-kws (apply table (mapcat (fn [k] [k true]) opt-kws)) @{})
        checks @[(tuple 'or (tuple 'table? pv) (tuple 'struct? pv))]]
    (each i (range (length field-kws))
      (let [kw (field-kws i)]
        (array/push checks
          (if (get opt-set kw)
            (tuple 'or
              (tuple '= nil (tuple 'get pv (tuple 'quote kw)))
              (tuple (tuple type-pred-fn (tuple 'quote (field-types i)))
                     (tuple 'get pv (tuple 'quote kw))))
            (tuple (tuple type-pred-fn (tuple 'quote (field-types i)))
                   (tuple 'get pv (tuple 'quote kw)))))))
    (let [body (apply tuple 'and checks)]
      (if guard-fn
        (tuple 'and body (tuple guard-fn pv))
        body))))

(defn- struct-eval-form
  "Build a quoted (defn ...) form for eval inside deftrecord."
  [name params & body]
  (tuple 'quote (apply tuple 'defn name params body)))

(defn- parse-lett-binding
  [b]
  (if (tuple? b)
    (case (length b)
      2 [(b 0) :any (b 1)]
      3 [(b 0) (b 1) (b 2)]
      nil)
    nil))

(defn- generate-lett-bindings
  [pairs body]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)
        names (map first pairs)
        types (map (fn [t] (get t 1)) pairs)
        vals (map last pairs)
        var-forms @[]]
    (for i 0 (length names)
      (let [name (names i)
            t (types i)
            v (vals i)]
        (with-syms [tmp]
          (array/push var-forms
            ~(var ,name
               (let [,tmp ,v]
                 (,cast-fn ,tmp ',t ,(string "lett:" name))
                 (,tag-value-fn ,tmp ',t)
                 ,tmp))))))
    ~(do ,;var-forms ,;body)))

(defn- build-typed-fn-form
  "Parses typed function definitions and returns the macro expansion."
  [defn-kind name args ret-type body]
  (let [has-doc? (string? args)
        actual-args (if has-doc? ret-type args)
        ret-preview (if has-doc? (first body) ret-type)
        has-ret? (or (keyword? ret-preview)
                     (fn-type? ret-preview)
                     (compound-type? ret-preview))
        final-ret-type (if has-ret?
                        (if has-doc? (first body) ret-type)
                        :dynamic)
        body-tail (if has-ret?
                   (if has-doc? (array/slice body 1) body)
                   (if has-doc?
                     (tuple (first body) ;(array/slice body 1))
                     (tuple ret-type ;body)))
        doc (if has-doc? args nil)
        parsed (parse-arg-pairs actual-args)
        arg-names (get parsed 0)
        arg-types (get parsed 1)
        arg-casts (build-arg-casts name arg-names arg-types)
        return-cast (build-return-cast name final-ret-type body-tail)
        out @[defn-kind name arg-names]]
    (when doc (array/push out doc))
    (each c arg-casts (array/push out c))
    (array/push out return-cast)
    (tuple ; out)))

(defmacro deftfn
  "Define a public typed function."
  [name args ret-type & body]
  (build-typed-fn-form 'defn name args ret-type body))

(defmacro deftfn-
  "Define a private typed function."
  [name args ret-type & body]
  (build-typed-fn-form 'defn- name args ret-type body))

(defmacro deftval
  "Define an immutable typed value."
  [name type value]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)]
    (with-syms [v]
      ~(def ,name
         (let [,v ,value]
           (,cast-fn ,v ',type (string ',name " definition"))
           (,tag-value-fn ,v ',type)
           ,v)))))


(defmacro deftv
  "Define a mutable typed variable."
  [name type value]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)
        var-types-ref (deft-ref 'var-types)]
    (with-syms [v]
      ~(var ,name
         (let [,v ,value]
           (,cast-fn ,v ',type (string ',name " definition"))
           (,tag-value-fn ,v ',type)
           (put (,var-types-ref) ',name ',type)
           ,v)))))


(defmacro sett
  "Typed set. assign a new value to a variable with type check."
  [name value]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)
        var-types-ref (deft-ref 'var-types)]
    (with-syms [v t]
      ~(let [,v ,value
             ,t (get (,var-types-ref) ',name)]
         (when ,t
           (,cast-fn ,v ,t (string ',name " sett"))
           (,tag-value-fn ,v ,t))
         (set ,name ,v)))))


(defn- emit-with-fn-type
  ```Register the inferred :fn type scheme at compile-time and return a (def ...) form.
The *inferred-fn-types* var is populated during macro expansion.
```
  [name arg-types ret-type fn-form &opt effective-doc]
  (def fn-scheme (tuple :fn arg-types ret-type))
  (put *inferred-fn-types* name fn-scheme)
  (if effective-doc
    (tuple 'def name ':doc effective-doc fn-form)
    (tuple 'def name fn-form)))

(defn- infer-and-emit
  "Run inference on a function definition, emit the final form with casts."
  [name arg-names arg-types body-tail ret-type doc]
  (let [[final-arg-types final-ret-type]
        (if *inference-enabled*
          (let [[resolved-args resolved-ret errs]
                (infer-defn name arg-names arg-types body-tail ret-type)
                merged-ret (if (dynamic-type? ret-type) resolved-ret ret-type)]
            (when (> (length errs) 0)
              (eprintf " type inference: %s:" name)
              (each e errs (eprintf "  %s" e)))
            [resolved-args merged-ret])
          [arg-types ret-type])]
    (let [arg-casts (build-arg-casts name arg-names final-arg-types)
          return-cast (build-return-cast name final-ret-type body-tail)
          fn-body (apply tuple 'do (array/concat (array/slice arg-casts) @[return-cast]))
          effective-doc (string "(" name " " (string/join (map string arg-names) " ") ")"
                                (if doc (string "\n\n" doc) ""))]
      (emit-with-fn-type name final-arg-types final-ret-type
        (tuple 'fn name arg-names fn-body)
        effective-doc))))

(defmacro deftn
  ```Define a typed function with optional type annotations.
Unlike deftfn, type specs are optional and untyped args are inferred.
The inferred :fn type scheme is available at runtime via (fn-type-of 'name)
```
  [name args & body-tail]
  (let [has-ret? (or (keyword? (first body-tail)) (compound-type? (first body-tail)))
        raw-ret-type (if has-ret? (first body-tail) :dynamic)
        body (if has-ret? (array/slice body-tail 1) body-tail)
        parsed (parse-flex-args args)
        arg-names (get parsed 0)
        raw-arg-types (get parsed 1)]
    (infer-and-emit name arg-names raw-arg-types body raw-ret-type nil)))

(defn- immutable?
  "Check if a type form contains :immutable."
  [type]
  (if (keyword? type)
    (= :immutable type)
    (and (tuple? type)
         (or (= :immutable (first type))
             (some immutable? (array/slice type 1))))))

(defmacro lett
  ```A let* which sequentially binds mutable typed values.
Supports both tuple [(name :type value) ... ] and array syntax [name type value ... ]
The :type annotation is optional in tuples, but required with arrays.
```
  [bindings & body]
  (if (empty? bindings)
    (tuple 'do ;body)
    (if (and (not (empty? bindings)) (not (tuple? (first bindings))))
      (let [p (partition 3 bindings)]
        (if (not= (length bindings) (* 3 (length p)))
          ~(error ,(string "each binding must be [name type value], got "
                           (describe bindings)))
          (generate-lett-bindings
            (map (fn [t] [(t 0) (t 1) (t 2)]) p) body)))
      (let [pairs (map parse-lett-binding bindings)]
        (if (some nil? pairs)
          ~(error ,(string "each binding must be (name [:type] value), got "
                           (describe bindings)))
          (generate-lett-bindings pairs body))))))

(defn- define-fn-form
  ```Build a typed/untyped function definition expansion for use in 'define'.
Handles typed-params, untyped-params, and inference/non-inference cases.
```
  [name doc second rest body-start]
  (def parsed (parse-flex-args second))
  (def arg-names (get parsed 0))
  (def raw-arg-types (get parsed 1))
  (if (not (has-typed-params? second))
    (if (not *inference-enabled*)
      (let [out @['defn name]]
        (when doc (array/push out doc))
        (array/push out arg-names)
        (each b (array/slice rest body-start) (array/push out b))
        (apply tuple out))
      (infer-and-emit name arg-names raw-arg-types
        (array/slice rest body-start) :dynamic doc))
    (let [candidate (rest body-start)
          ret-is-fn-type (and (tuple? candidate)
                              (= :fn (first candidate))
                              (> (length candidate) 1))
          has-ret? (and (> (length rest) body-start)
                        (or (keyword? candidate) (compound-type? candidate) ret-is-fn-type))
          ret-type (if has-ret? candidate :dynamic)
          body-tail (if has-ret? (array/slice rest (+ body-start 1))
                      (array/slice rest body-start))]
      (infer-and-emit name arg-names raw-arg-types body-tail ret-type doc))))


(defmacro infer-assert-type
  ```Compile-time type assertion. Infers the type of 'expr' (in clean env)
or raises a compile-time error when the inferred type does not match 'expected-type'.
```
  [expected-type expr]
  (let [inferred (infer-expression expr)]
    (if (= inferred expected-type)
      expr
      (errorf "infer-assert-type: expected %s, inferred %s for %q"
              expected-type inferred expr))))

(defmacro with-inference-trace
  ```Execute body with inference tracing enabled.
Prints every form and its inferred type to stderr.
Restores previous trace state on completion.
```
  [& body]
  (with-syms [prev]
    ~(let [,prev *inference-trace-enabled*]
       (enable-inference-trace true)
       (def _result (do ,;body))
       (enable-inference-trace ,prev)
       _result)))


(defmacro define
  ```Unified typed definitions
     function: (define name doc? [args :types ...] [:return] & body)
        value: (define name doc? :type expr)
          var: (define name doc? expr)
When args have type annotations, inserts runtime type checks.
When :type is :immutable, creates a def; otherwise creates a var.
No type annotation, var.
```
  [name & rest]
  (let [has-doc? (string? (rest 0))
        doc (if has-doc? (rest 0) nil)
        off (if has-doc? 1 0)
        second (rest off)]
    (if (and (not (compound-type? second))
             (or (tuple? second) (array? second)))
      (define-fn-form name doc second rest (+ off 1))
      (let [typed (or (keyword? second) (compound-type? second))]
        (if typed
          (let [cast-fn (deft-ref 'cast)
                tag-value-fn (deft-ref 'tag-value)
                v (gensym) val (rest (+ off 1))
                def-or-var (if (immutable? second) 'def 'var)
                type-form (if (compound-type? second) (tuple 'quote second) second)]
            (tuple def-or-var name
                   (tuple 'let (array v val)
                          (tuple cast-fn v type-form
                                 (string (string name " definition")))
                          (tuple tag-value-fn v type-form)
                          v)))
          (apply tuple 'var name (array/slice rest off)))))))


(defmacro deftype
  "Register a new type :keyword with a predicate function or compound form."
  [name pred]
  (let [register-type-fn (deft-ref 'register-type)
        type-pred-fn (deft-ref 'type-predicate)]
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

Generates constructor, accessors, mutators based on record/field names
 - make-<name> accepts required positional args, then optional positional args
               then :keyword value pairs for any other field(s).
 - <name>-<field> accessors
 - set-<name>-<field> mutators.

Each field clause is one of the following
  (field name type)         — required positional arg
  (optional name type)      — optional positional arg (default nil)
  (guard pred-fn)           — optional guard predicate
  (print print-fn)          — optional custom printer
```
  [name & clauses]
  (let [register-type-fn (deft-ref 'register-type)
        cast-fn (deft-ref 'cast)
        type-pred-fn (deft-ref 'type-predicate)
        pp-str-fn (deft-ref 'pp-str)
        prefix (string/replace ":" "" (string name))
        clause-type? (fn [c tag]
                       (and (tuple? c)
                            (= tag
                               (last (string/split "/" (string (first c)))))))
        req-fields (filter (fn [c] (clause-type? c "field")) clauses)
        opt-fields (filter (fn [c] (clause-type? c "optional")) clauses)
        all-fields (array/concat (array/slice req-fields) (array/slice opt-fields))
        guard-clauses (filter (fn [c] (clause-type? c "guard")) clauses)
        guard-fn (if (> (length guard-clauses) 0) ((guard-clauses 0) 1) nil)
        print-clauses (filter (fn [c] (clause-type? c "print")) clauses)
        print-fn (if (> (length print-clauses) 0)
                   ((print-clauses 0) 1) nil)
        field-names (map (fn [f] (string/replace ":" "" (string (f 1)))) all-fields)
        field-kws (map (fn [f] (keyword (f 1))) all-fields)
        field-types (map (fn [f] (f 2)) all-fields)
        req-kws (map (fn [f] (keyword (f 1))) req-fields)
        make-sym (symbol (string "make-" prefix))
        args-sym (gensym)
        idx-sym (gensym)
        k-sym (gensym)]
    (with-syms [pv ov obj env]
      (def do-body @[])
      (array/push do-body
        ~(,register-type-fn ',name
             (fn [,pv] ,(build-struct-pred pv field-kws field-types guard-fn
                          (map (fn [f] (keyword (f 1))) opt-fields)))))
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
      (def default-pp
        (let [pv (gensym)
              pushers (map (fn [kw]
                             (tuple 'string kw "="
                                    (tuple pp-str-fn (tuple 'get pv kw))))
                           field-kws)]
          (tuple 'fn (tuple pv)
                 (tuple 'string prefix "("
                        (tuple 'string/join (apply tuple 'array pushers) ", ") ")"))))
      (def pp-handler
        (if print-fn
          (tuple 'fn (tuple (gensym)) (tuple print-fn (gensym)))
          default-pp))
      # Build required-field table init: (:req1 (args 0) :req2 (args 1) ...)
      (def req-kvs
        (interleave req-kws
                    (map (fn [i] (tuple 'get args-sym i))
                         (range (length req-kws)))))
      # Build optional field forms
      (def opt-bodies
        (map (fn [f]
               (let [kw (keyword (f 1))]
                 (tuple 'if
                   (tuple 'and
                     (tuple '< idx-sym
                             (tuple 'length args-sym))
                     (tuple 'not (tuple 'keyword? (tuple 'get args-sym idx-sym))))
                   (tuple 'do
                     (tuple 'put ov kw (tuple 'get args-sym idx-sym))
                     (tuple '++ idx-sym))
                   (tuple 'put ov kw nil))))
             opt-fields))
      # Build keyword-arg processing loop
      (def kw-loop
        (tuple 'while (tuple '< idx-sym (tuple 'length args-sym))
          (tuple 'do
            (tuple 'def k-sym (tuple 'get args-sym idx-sym))
            (tuple 'when (tuple 'keyword? k-sym)
              (tuple 'put ov k-sym (tuple 'get args-sym (tuple '+ idx-sym 1))))
            (tuple '+= idx-sym 2))))
      # Assemble constructor:
      # (fn [& args] (def ov (table ...)) (var idx N) opt-bodies... kw-loop pp cast ov)
      (def constructor
        (do (def body-parts
              @[(tuple 'def ov (apply tuple 'table req-kvs))
                (tuple 'var idx-sym (length req-kws))])
            (each b opt-bodies (array/push body-parts b))
            (array/push body-parts kw-loop)
            (array/push body-parts (tuple 'put ov :pp pp-handler))
            (array/push body-parts (tuple cast-fn ov
                                          (tuple 'quote name)
                                          (string "make-" prefix)))
            (array/push body-parts ov)
            (tuple 'fn (tuple '& args-sym) (apply tuple 'do body-parts))))
      (array/push do-body constructor)
      (tuple 'def make-sym (apply tuple 'do do-body)))))


(defmacro defenum
  ```Define an enumeration type from a key-value table.
Generates <name>, <name>-extend, and <name>-remove helpers.```
  [name kv-map]
  (let [register-type-fn (deft-ref 'register-type)
        enum-tables-ref (deft-ref '*enum-tables*)
        prefix (string/replace ":" "" (string name))
        accessor-name (symbol prefix)
        extend-name (symbol (string prefix "-extend"))
        remove-name (symbol (string prefix "-remove"))
        # accessor form
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
        # extend form
        eval-extend
        (let [et-ref (tuple enum-table (tuple 'quote name))
              body (tuple
                    'do
                    (tuple 'put et-ref 'k 'v)
                    'v)
              defn-form (tuple
                         'defn extend-name
                         "Add or update a key-value pair in " prefix ". Returns the new value."
                         '[k v]
                         body)]
          (tuple 'quote defn-form))
        # remove form
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
                         "Remove a key from " prefix " and return its value."
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
