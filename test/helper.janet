# -*- mode: janet; -*-
# deft tests: asserts that keep count

(import ../deft :prefix "")

(var pass-count 0)
(var fail-count 0)

(defn assert [label got expected]
  (if (= got expected)
    (do (++ pass-count) (print "  ✓ " label))
    (do (++ fail-count)
      (print "  ✗ " label ": expected " (string expected) ", got " (string got)))))

(defmacro assert-err [label body]
  ~(assert ,label
           (not= nil
             (try (do ,body nil) ([e] e)))
           true))

(defn print-results []
  (print "\n--- " pass-count " passed, " fail-count " failed ---")
  (if (> fail-count 0) (os/exit 1)))
