# -*- mode: janet; -*-
# deft tests: asserts that keep count

(import ../deft :prefix "")

(var pass-count 0)
(var fail-count 0)

(defn cassert [name got expected]
  (if (= got expected)
    (do (++ pass-count) (print "  ✓ " name))
    (do (++ fail-count)
      (print "  ✗ " name ": expected " (string expected) ", got " (string got)))))

(defmacro cassert-err [name body]
  ~(cassert ,name
           (not= nil
             (try (do ,body nil) ([e] e)))
           true))

(defn print-results []
  (print "\n--- " pass-count " passed, " fail-count " failed ---")
  (if (> fail-count 0) (os/exit 1)))
