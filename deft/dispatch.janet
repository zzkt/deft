# -*- mode: janet; -*-
# deft/dispatch: checking and inference settings

(var *checking-enabled* true)
(var *inference-enabled* true)

(var *inference-trace-enabled*
     "When true, inference steps are printed to stderr for debugging."
     false)

(defn enable-checking
  "Enable or disable runtime type checking."
  [enabled]
  (set *checking-enabled* enabled))

(defn enable-inference
  "Enable or disable local type inference for unannotated names."
  [enabled]
  (set *inference-enabled* enabled))

(defn enable-inference-trace
  "Enable or disable inference tracing to stderr."
  [enabled]
  (set *inference-trace-enabled* enabled))

(var *inferred-fn-types*
  ```Table of function-name to inferred :fn type scheme.
Populated at macroexpansion time by deftn/define and can be read at
runtime using fn-type-of.```
     @{})


(def *predicate-narrowing*
  "Predicate -> narrowed-type map used for branch narrowing."
  (table
     "string?"  :string
     "number?"  :number
     "boolean?" :boolean
     "keyword?" :keyword
     "nil?"     :nil
     "symbol?"  :symbol
     "tuple?"   :tuple
     "array?"   :array
     "table?"   :table
     "struct?"  :struct
     "buffer?"  :buffer
     "fiber?"   :fiber))


(defn register-narrowing
 ``` Register a custom predicate for type narrowing.
`pred-name` is the name of the predicate function as a string.
A type is narrowed to `narrow-type` when the predicate is truthy.

Example: (register-narrowing "positive-number?" :number)
```
  [pred-name narrow-type]
  (put *predicate-narrowing* pred-name narrow-type))


(def *op-type-schemes*
  ```Operator -> type-schemes map used for inference.
  op -> (:fn [arg-types...] ret-type).```
  (table
   "+" '(:fn [:number :number] :number)
   "-" '(:fn [:number :number] :number)
   "*" '(:fn [:number :number] :number)
   "/" '(:fn [:number :number] :number)
   "++" '(:fn [:number] :number)
   "--" '(:fn [:number] :number)
   "<" '(:fn [:number :number] :boolean)
   ">" '(:fn [:number :number] :boolean)
   "=" '(:fn [:number :number] :boolean)
   "not" '(:fn [:boolean] :boolean)
   "string" '(:fn [:dynamic] :string)
   "length" '(:fn [:dynamic] :number)
   "get" '(:fn [:dynamic :dynamic] :dynamic)
   "in" '(:fn [:dynamic :dynamic] :dynamic)
   "put" '(:fn [:mutable :dynamic :dynamic] :dynamic)
   "array/push" '(:fn [:mutable :dynamic] :dynamic)
   "update" '(:fn [:mutable :dynamic :dynamic] :dynamic)
   "array/pop" '(:fn [:array] :dynamic)
   "array/clear" '(:fn [:array] :array)
   "table/clear" '(:fn [:table] :table)
   "string/join" '(:fn [:array :string] :string)
   "string/format" '(:fn [:string] :string)
   "scan-number"  '(:fn [:string] :number)
   "describe" '(:fn [:dynamic] :string)
   "print" '(:fn [:dynamic] :nil)
   "flush" '(:fn [] :nil)
   "error" '(:fn [:string] :nil)
   "tuple" '(:fn [:dynamic] :tuple)
   "array" '(:fn [:dynamic] :array)
   "keys" '(:fn [:dynamic] :array)
   "values" '(:fn [:dynamic] :array)
   "pairs" '(:fn [:dynamic] :array)
   "length" '(:fn [:dynamic] :number)
   "first" '(:fn [:dynamic] :dynamic)
   "last" '(:fn [:dynamic] :dynamic)
   "gensym" '(:fn [] :symbol)
   "keyword" '(:fn [:string] :keyword)
   "symbol" '(:fn [:string] :symbol)
   "string/from-bytes" '(:fn [:number] :string)
   "string?" '(:fn [:dynamic] :boolean)
   "number?" '(:fn [:dynamic] :boolean)
   "boolean?" '(:fn [:dynamic] :boolean)
   "keyword?" '(:fn [:dynamic] :boolean)
   "nil?" '(:fn [:dynamic] :boolean)
   "symbol?" '(:fn [:dynamic] :boolean)))
