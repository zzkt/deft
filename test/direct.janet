(import ../deft :prefix "")

(var *tvar-counter* 0)
(defn fresh-tvar []
  (set *tvar-counter* (+ 1 *tvar-counter*))
  (symbol (string "?tv" *tvar-counter*)))

(defn my-fresh-arg-env [names types]
  (let [env @{}]
    (each i (range (length names))
      (put env (names i)
           (if (and (< i (length types)) (keyword? (types i))
                    (not (dynamic-type? (types i))))
             (types i)
             (fresh-tvar))))
    env))

(print "my-fresh-arg-env: " (my-fresh-arg-env '[x y] '[]))
(print "fresh-tvar works: " (fresh-tvar))
(enable-inference true)
(deftn inf-add [x y] (+ x y))
(print (inf-add 3 4))
