# -*- mode: janet; -*-
# deft/check: static type checking via bidirectional inference

(use ./core)
(use ./inference)

(defn- check-form-types
  ```Check a single form using bidirectional inference.
Returns a list of error strings.
```
  [form]
  (let [errors @[]]
    (when (and (tuple? form) (symbol? (first form)))
      (let [name (get form 1)
            args (get form 2)
            body-start (if (and (> (length form) 3) (keyword? (get form 3))) 4 3)
            return-type (if (= body-start 4) (get form 3) :dynamic)
            body (array/slice form body-start)]
        (when (and name args (tuple? args))
          (let [p2 (partition 2 args)
                arg-names (map first p2)
                arg-types (map last p2)
                env (fresh-arg-env arg-names arg-types)]
            (each body-form body
              (let [chk (infer-chk-form env body-form return-type)]
                (when chk
                  (array/push errors
                    (string name ": " chk)))))))))
    errors))


(defmacro deftcheck
  "Ahead-of-time static analysis pass."
  [& forms]
  (let [errors @[]]
    (each f forms
      (each e (check-form-types f) (array/push errors e)))
    (if (> (length errors) 0)
      (do
        (eprintf "deftcheck: %d type error(s)" (length errors))
        (each e errors (eprintf "  %s" e))
        nil)
      (apply tuple 'do forms))))


(defn check-form
  "Runtime function to check a single deft form."
  [form]
  (check-form-types form))
