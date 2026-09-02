# -*- mode: janet; -*-
# deft/wolves: argument parsing, type-form expansion, and cast helpers

(use ./core)

(defn parse-arg-pairs
  ```Split flat [name type name type ...] list into (names, types).
Supports a trailing '&' (to ignore extra args) or '& rest-name'
(named variadic rest). &rest is not typed and is stripped from the
pair parsing but preserved in the returned parameter tuple.```
  [args]
  (var ignore-extra false)
  (var rest-name nil)
  (var a (array/slice args))
  (when (and (> (length a) 0) (= '& (get a (- (length a) 1))))
    (set ignore-extra true)
    (array/pop a))
  (when (and (> (length a) 1) (= '& (get a (- (length a) 2))))
    (set rest-name (get a (- (length a) 1)))
    (array/pop a)
    (array/pop a))
  (let [p2 (partition 2 a)
        names (map first p2)
        types (map last p2)]
    (when (not= (length a) (* 2 (length names)))
      (errorf "Every argument must be followed by a type spec; got args %q" args))
    (var fn-names (tuple ;names))
    (when rest-name (set fn-names (tuple ;fn-names '& rest-name)))
    (when ignore-extra (set fn-names (tuple ;fn-names '&)))
    (tuple (tuple ;names) types fn-names)))


(defn- simple-type-pred
  "Resolve a type keyword to its core Janet predicate symbol at macroexpansion
   time, or nil for complex/user types that need the generic cast."
  [t]
  (get {:number 'number?
        :string 'string?
        :boolean 'boolean?
        :keyword 'keyword?
        :symbol 'symbol?
        :nil 'nil?
        :array 'array?
        :tuple 'tuple?
        :table 'table?
        :struct 'struct?
        :buffer 'buffer?
        :function 'function?}
       t))


(defn- inline-simple-check
  "Build an inline cast check for an argument/keyword value.
   - pred-sym present (simple builtin type): emit
       (when (checking-enabled) (when (not (pred x)) (errorf ... cite)))
   - pred-sym nil: falls through to generic cast (fn/compound/user types),
     or nil for :dynamic/:any (no check emitted).
   Returns the form to splice, or nil if nothing should be emitted."
  [pred-sym n t blame]
  (cond
    (nil? pred-sym)
    (cond
      (dynamic-type? t) nil
      (let [cast-fn (deft-ref 'cast)]
        (tuple cast-fn n
               (if (or (fn-type? t) (compound-type? t))
                 (tuple 'quote t) t)
               blame)))
    (let [checking-fn (deft-ref 'checking-enabled)]
      ~(when (,checking-fn)
         (when (not (,pred-sym ,n))
           (errorf "Type error (%s): expected %q, got %s"
                   ,blame ',t (describe ,n)))))))


(defn build-arg-casts
  "Build cast forms for a deft function's arguments.
   Positional args are cast unconditionally.  Optional (&opt) args are
   cast only when non-nil (Janet defaults them to nil).  A typed rest
   arg is cast as a [:tuple T] value.

   Optimizations over generic cast:
   - :dynamic/:any args emit nothing (zero-cost dynamic)
   - Simple keyword types pre-resolve predicate at expansion, emit
     inline (when (not (pred? x)) (error ...)) — no table lookup per call
   - Compound/fn-type args use generic cast (can't inline)"
  [name arg-names arg-types &opt fopt-names fopt-types rest-name rest-type]
  (def opt-names (or fopt-names @[]))
  (def opt-types (or fopt-types @[]))
  (let [out @[]]
    (each i (range (length arg-names))
      (let [n (arg-names i) t (or (arg-types i) :dynamic)
            blame (tuple 'string (string name ":" n) " (caller blamed)")
            cast-form (inline-simple-check (simple-type-pred t) n t blame)]
        (when cast-form (array/push out cast-form))))
    (each i (range (length opt-names))
      (let [n (opt-names i) t (or (opt-types i) :dynamic)
            blame (tuple 'string (string name ":" n) " (caller blamed)")
            inner (inline-simple-check (simple-type-pred t) n t blame)]
        (when inner (array/push out (tuple 'when (tuple 'not= nil n) inner)))))
    (when (and rest-name rest-type (not (dynamic-type? rest-type)))
      (array/push out
        (tuple (deft-ref 'cast) rest-name
               (tuple 'quote (tuple :tuple rest-type))
               (tuple 'string (string name ":" rest-name) " (caller blamed)"))))
    out))


(defn build-return-cast
  [name ret-type body]
  (let [cast-fn (deft-ref 'cast)
        ret-form (if (or (fn-type? ret-type) (compound-type? ret-type))
                   (tuple 'quote ret-type)
                   ret-type)]
    (tuple cast-fn (apply tuple 'do body)
           ret-form
           (string name " return (function blamed)"))))


(defn build-kw-forms
  ```Generate kw-extraction and cast forms for &keys args.
   Returns a flat array of forms to splice into the fn body.```
  [name kw-map]
  (let [cast-fn (deft-ref 'cast)
        k2 (keys kw-map)
        out @[]
        tbl-sym (gensym)]
    (array/push out (tuple 'def tbl-sym (tuple 'apply 'table '__kw-rest)))
    (each k k2
      (let [t (get kw-map k)
            kw-key (if (keyword? k) k (keyword (string k)))
            sym (symbol (string k))
            needs-quote? (or (fn-type? t) (compound-type? t))
            type-form (if needs-quote? (tuple 'quote t) t)]
        (array/push out (tuple 'def sym (tuple 'get tbl-sym (tuple 'quote kw-key))))
        (if (fn-type? t)
          (array/push out (tuple 'def sym
                            (tuple cast-fn sym type-form
                                   (string (string name ":" sym) " (caller blamed)"))))
          (array/push out (tuple cast-fn sym type-form
                                 (string (string name ":" sym) " (caller blamed)"))))))
    out))


(defn type-spec?
  [x]
  (or (keyword? x) (fn-type? x) (compound-type? x)))


(defn push-name-type
  ```Push args[i] into the current section's name slot, consuming a
   following type spec if present. Returns [next-i rest-name rest-type]
   where rest-name/rest-type are the (possibly updated) rest values.```
  [i args section names types opt-names opt-types rest-name rest-type]
  (var rn rest-name)
  (var rt rest-type)
  (let [next-idx (+ i 1)
        has-type? (and (< next-idx (length args)) (type-spec? (args next-idx)))
        t (if has-type? (args next-idx) :dynamic)]
    (case section
      :opt (do (array/push opt-names (args i)) (array/push opt-types t))
      :rest (do (when rn (errorf "multiple rest args after &"))
                (set rn (args i))
                (set rt t))
      :positional (do (array/push names (args i)) (array/push types t)))
    [(if has-type? (+ i 2) (+ i 1)) rn rt]))

(defn parse-flex-args
  ```Parse a deft argument list.
     [name :type ...]        positional args
     &opt name :type ...     optional args (default nil)
     & name :type            rest arg
     &                       bare rest marker (ignore extra args)
     &keys {:k :type ...}    keyword args

   Return (tuple names types kw opt-names opt-types rest-info)
   where rest-info is [name type] for a named rest arg, [:ignore nil]
   for a bare '&' marker, or nil.
```
  [args]
  (var names @[])
  (var types @[])
  (var kw @{})
  (var opt-names @[])
  (var opt-types @[])
  (var rest-name nil)
  (var rest-type nil)
  (var rest-ignore false)
  (var section :positional)
  (var i 0)
  (while (< i (length args))
    (let [cur (args i)]
      (cond
        (= '&keys cur)
        (let [kt (args (+ i 1))]
          (when (not (or (table? kt) (struct? kt)))
            (errorf "&keys must be followed by a {key :type ...} table, got %q" kt))
          (loop [[k t] :pairs kt] (put kw k t))
          (set i (+ i 2)))
        (= '&opt cur)
        (do
          (when (= section :rest)
            (errorf "&opt must come before & rest args"))
          (set section :opt)
          (++ i))
        (= '& cur)
        (do
          (when (or (= section :rest) rest-name)
            (errorf "multiple & rest args"))
          (set section :rest)
          (++ i)
          (when (>= i (length args))
            (set rest-ignore true)))
        (type-spec? cur)
        (errorf "unexpected type %q without preceding name (position %d)" cur i)
        (let [[new-i new-rn new-rt]
              (push-name-type i args section names types
                              opt-names opt-types rest-name rest-type)]
          (set rest-name new-rn)
          (set rest-type new-rt)
          (set i new-i)))))
  (tuple (tuple ;names) types kw (tuple ;opt-names) opt-types
         (if rest-ignore [:ignore nil]
           (if rest-name [rest-name rest-type] nil))))


(defn build-fn-arg-list
  ```Reconstruct the fn parameter tuple for a deft function from the
   parsed argument components. Order: positional, &opt ..., & rest,
   then & __kw-rest when &keys are present.```
  [arg-names &opt opt-names rest-name rest-ignore kw-map]
  (let [has-kw? (and kw-map (not (empty? kw-map)))
        out @[]]
    (when (and (or rest-name rest-ignore) has-kw?)
      (errorf "cannot combine & rest args with &keys"))
    (each n arg-names (array/push out n))
    (when (and opt-names (> (length opt-names) 0))
      (array/push out '&opt)
      (each n opt-names (array/push out n)))
    (when (or rest-name rest-ignore)
      (array/push out '&)
      (when rest-name (array/push out rest-name)))
    (when has-kw?
      (array/push out '&)
      (array/push out '__kw-rest))
    (tuple ;out)))


(defn has-typed-params?
  [args]
  (some type-spec? args))


(defn expand-type-form
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


(defn build-struct-pred
  ```Build the shape-only predicate form (table/struct + field checks) for a
record type.
The guard is intentionally excluded since it is registered separately via
register-guard so cast can distinguish guard failures from field/shape failures.
```
  [pv field-kws field-types &opt opt-kws]
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
    (apply tuple 'and checks)))


(defn struct-eval-form
  "Build a quoted (defn ...) form for eval inside deftrecord."
  [name params & body]
  (tuple 'quote (apply tuple 'defn name params body)))

(defn parse-lett-binding
  [b]
  (if (tuple? b)
    (case (length b)
      2 [(b 0) :any (b 1)]
      3 [(b 0) (b 1) (b 2)]
      nil)
    nil))

(defn generate-lett-bindings
  [p2 body]
  (let [cast-fn (deft-ref 'cast)
        tag-value-fn (deft-ref 'tag-value)
        names (map first p2)
        types (map (fn [t] (get t 1)) p2)
        vals (map last p2)
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


(defn build-typed-fn-form
 ```Shared helper for deftfn and deftfn- that parses docstring, return type,
body-tail from `(name args ret-type & body)` and returns the macro expansion.
```
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
        docstring (if has-doc? args nil)
        parsed (parse-arg-pairs actual-args)
        arg-names (get parsed 0)
        arg-types (get parsed 1)
        fn-params (get parsed 2)
        arg-casts (build-arg-casts name arg-names arg-types)
        return-cast (build-return-cast name final-ret-type body-tail)
        out @[defn-kind name fn-params]]
    (when docstring (array/push out docstring))
    (each c arg-casts (array/push out c))
    (array/push out return-cast)
    (tuple ; out)))
