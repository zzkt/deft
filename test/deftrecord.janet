# -*- mode: janet; -*-
# deft tests: deftrecord

(import ./helper :prefix "")
(import ../deft :prefix "")

(print "* deftrecord: basic construction")

(deftrecord :point
  (field x :number)
  (field y :number))

(def p (make-point 1 2))
(cassert "make-point constructs" (isa? p :point) true)
(cassert "point-x accessor" (point-x p) 1)
(cassert "point-y accessor" (point-y p) 2)

(print "\n* deftrecord: type checking")

(cassert "isa? point" (isa? p :point) true)
(cassert "isa? accepts table with fields" (isa? @{:x 1 :y 2} :point) true)
(cassert "isa? accepts struct with fields" (isa? {:x 1 :y 2} :point) true)
(cassert "isa? rejects number" (isa? 42 :point) false)
(cassert "isa? rejects table missing fields" (isa? @{:x 1} :point) false)

(print "\n* deftrecord: wrong field type")

(cassert-err "rejects string for :number" (make-point "bad" 2))
(cassert-err "rejects nil for :number" (make-point nil 2))

(print "\n* deftrecord: mutators")

(def p2 (make-point 10 20))
(cassert "point-x before" (point-x p2) 10)
(set-point-x p2 99)
(cassert "point-x after" (point-x p2) 99)
(cassert-err "mutator rejects wrong type" (set-point-x p2 "oops"))

(print "\n* deftrecord: optional fields")

(deftrecord :config
  (field name :string)
  (optional host :string)
  (optional port :number)
  (optional verbose :boolean))

(def c1 (make-config "db"))
(cassert "config name" (config-name c1) "db")
(cassert "optional host nil" (config-host c1) nil)
(cassert "optional port nil" (config-port c1) nil)

(def c2 (make-config "web" :host "localhost" :port 8080))
(cassert "optional host set" (config-host c2) "localhost")
(cassert "optional port set" (config-port c2) 8080)

(def c3 (make-config "cache" "redis" 6379))
(cassert "optional positional" (config-host c3) "redis")
(cassert "optional positional port" (config-port c3) 6379)

(print "\n* deftrecord: guard")

(deftrecord :positive-pair
  (field a :number)
  (field b :number)
  (guard (fn [v] (and (> (v :a) 0) (> (v :b) 0)))))

(cassert "valid pair" (isa? (make-positive-pair 1 2) :positive-pair) true)
(cassert-err "guard rejects negative" (make-positive-pair -1 2))
(cassert-err "guard rejects zero" (make-positive-pair 0 0))

(print "\n* deftrecord: guard error message")

(deftrecord :timeframe
  (field start :number)
  (field end :number)
  (guard (fn [v] (> (get v :end) (get v :start)))))

(def timeframe-guard-err
  (try (make-timeframe 101 33)
       ([e] (not= nil (string/find "guard predicate failed" (string e))))))

(cassert "guard failure names guard predicate" timeframe-guard-err true)
(cassert "valid timeframe accepted" (isa? (make-timeframe 10 33) :timeframe) true)
(cassert "guard violation not isa?" (isa? {:start 9 :end 2} :timeframe) false)

(def timeframe-field-err
  (try (make-timeframe 101 "x")
       ([e] (not= nil (string/find "expected :timeframe" (string e))))))

(cassert "field failure names expected type" timeframe-field-err true)

(print "\n* deftrecord: nested records")

(deftrecord :edge
  (field from :dynamic)
  (field to :dynamic))

(deftrecord :graph
  (field nodes :array)
  (field edges :array))

(def g (make-graph @[1 2] @[(make-edge 1 2) (make-edge 2 3)]))
(cassert "graph-nodes" (= (tuple ;(graph-nodes g)) (tuple 1 2)) true)
(cassert "edge-from" (edge-from (first (graph-edges g))) 1)
(cassert "edge-to" (edge-to (first (graph-edges g))) 2)

(print "\n* deftrecord: pp handler")

(def default-pp (pp-str (make-point 3 4)))
(cassert "default pp renders full record" (= default-pp "point(x=3, y=4)") true)
(cassert "default pp shows field x" (string/find "x=3" default-pp) 6)
(cassert "default pp shows field y" (string/find "y=4" default-pp) 11)

(deftrecord :widget
  (field label :string)
  (print (fn [r] (string "WIDGET:" (get r :label)))))

(def custom-pp (pp-str (make-widget "gizmo")))
(cassert "custom printer is invoked" (= custom-pp "WIDGET:gizmo") true)

(print "\n* deftrecord: typed function with record arg")

(deftfn distance [p1 :point p2 :point] :number
  (let [dx (- (point-x p2) (point-x p1))
        dy (- (point-y p2) (point-y p1))]
    (math/sqrt (+ (* dx dx) (* dy dy)))))

(cassert "distance" (distance (make-point 0 0) (make-point 3 4)) 5)

(print "\n* deftrecord: compound field type")

(deftrecord :queue
  (field data (or :array :table))
  (field head :integer))

(def q1 (make-queue @[1 2 3] 0))
(cassert "queue with array" (isa? q1 :queue) true)
(cassert "queue data" (= (tuple ;(queue-data q1)) (tuple 1 2 3)) true)
(cassert "queue head" (queue-head q1) 0)

(def q2 (make-queue @{:a 1} 0))
(cassert "queue with table" (isa? q2 :queue) true)
(cassert "queue data table" (= (get (queue-data q2) :a) 1) true)

(cassert-err "queue rejects string" (make-queue "bad" 0))
(cassert-err "queue requires args" (make-queue))
(cassert-err "queue rejects nil data" (make-queue nil 0))

(deftrecord :opt-queue
  (optional data (or :array :table))
  (optional head :integer))

(def q3 (make-opt-queue))
(cassert "opt-queue empty" (isa? q3 :opt-queue) true)
(cassert "opt-queue data nil" (opt-queue-data q3) nil)

(def q4 (make-opt-queue @[4 5] 2))
(cassert "opt-queue with args" (= (tuple ;(opt-queue-data q4)) (tuple 4 5)) true)
(cassert "opt-queue head" (opt-queue-head q4) 2)

(print-results)
