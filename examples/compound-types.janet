# -*- mode: janet; -*-
# Compound type examples: red-black tree, graph, directed graph, DAG
#
# Demonstrates deftype with structural predicates, compound types
# composing types with and/or, and type-checked functions.

(import deft)

# Red/Black tree

(defn- rb-color [node]
  (get node :color :black))

(defn- rb-no-red-red? [node]
  (if (= (get node :color) :red)
    (and (not= (rb-color (get node :left)) :red)
         (not= (rb-color (get node :right)) :red))
    true))

(defn- rb-valid-tree? [node]
  (if (nil? node) true
      (and (or (table? node) (struct? node))
           (number? (get node :key))
           (or (= :red (get node :color)) (= :black (get node :color)))
           (rb-valid-tree? (get node :left))
           (rb-valid-tree? (get node :right))
           (rb-no-red-red? node))))

(deft/deftype :red-black (fn [v]
                           (and (or (table? v) (struct? v))
                                (rb-valid-tree? v))))

(def leaf nil)
(def n1 {:key 1 :color :black :left leaf :right leaf})
(def n3 {:key 3 :color :black :left leaf :right leaf})
(def n2 {:key 2 :color :red   :left n1 :right n3})
(def bad-node {:key 42 :color :purple :left leaf :right leaf})

(print "Red/Black tree type")
(print "  leaf: " ((deft/type-predicate :red-black) leaf))
(print "  black node: " ((deft/type-predicate :red-black) n3))
(print "  red+blacks: " ((deft/type-predicate :red-black) n2))
(print "  bad color: " ((deft/type-predicate :red-black) bad-node))


# Graph

(defn- in? [s k] (not= nil (in s k)))

(defn- edge-valid? [edge node-set]
  (and (or (table? edge) (struct? edge))
       (in? node-set (get edge :from))
       (in? node-set (get edge :to))))

(defn- graph-valid? [g]
  (and (or (table? g) (struct? g))
       (array? (get g :nodes))
       (array? (get g :edges))
       (let [ns (frequencies (get g :nodes))]
         (all (fn [e] (edge-valid? e ns)) (get g :edges)))))

(deft/deftype :graph graph-valid?)

(def g-ok {:nodes @[1 2 3]
           :edges @[{:from 1 :to 2}
                    {:from 2 :to 3}]})

(def g-bad {:nodes @[1 2 3]
            :edges @[{:from 1 :to 99}]})

(def g-empty {:nodes @[] :edges @[]})

(print "\nGraph type")
(print "  valid: " ((deft/type-pred :graph) g-ok))
(print "  bad edges: " ((deft/type-pred :graph) g-bad))
(print "  empty: " ((deft/type-pred :graph) g-empty))
(print "  string: " ((deft/type-pred :graph) "hi"))
(print "  bare table: " ((deft/type-pred :graph) @{:a 1}))


# Directed graph

(defn- directed-edge? [e]
  (and (or (table? e) (struct? e))
       (not= nil (get e :from))
       (not= nil (get e :to))))

(deft/deftype :directed-graph (fn [v]
  (and (graph-valid? v)
       (all directed-edge? (get v :edges)))))

(def dg-ok {:nodes @[1 2 3]
            :edges @[{:from 1 :to 2}
                     {:from 2 :to 3}]})

(def dg-bad {:nodes @[1 2 3]
             :edges @[{:from 1 :to 2} {:x 1 :y 2}]})

(print "\nDirected graph type")
(print "  valid: " ((deft/type-pred :directed-graph) dg-ok))
(print "  bad edge shape: " ((deft/type-pred :directed-graph) dg-bad))


# Directed acyclic graph (DAG)

# Note: cycle detection can be potentially expensive
(defn- has-cycle? [nodes edges]
  (let [adj (table)
        visited (table)
        in-stack (table)]
    (each n nodes
      (put adj n @[])
      (put visited n false)
      (put in-stack n false))
    (each e edges
      (when (and (get e :from) (get e :to))
        (array/push (get adj (get e :from)) (get e :to))))
    (var cycle? false)
    (defn dfs [n]
      (unless (get visited n)
        (put visited n true)
        (put in-stack n true)
        (each m (get adj n)
          (if (get in-stack m) (set cycle? true) (dfs m)))
        (put in-stack n false)))
    (each n nodes (unless cycle? (dfs n)))
    cycle?))


(deft/deftype :dag (fn [v]
  (and (deft/isa? v :directed-graph)
       (not (has-cycle? (get v :nodes) (get v :edges))))))

# valid DAG
(def dag-ok {:nodes @[1 2 3]
             :edges @[{:from 1 :to 2}
                      {:from 2 :to 3}]})

# syntactically correct but fails predicate (contains a cycle)
(def dag-cycle {:nodes @[1 2 3]
                :edges @[{:from 1 :to 2}
                         {:from 2 :to 3}
                         {:from 3 :to 1}]})

# syntactically correct but fails predicate (contains a self loop)
(def dag-self {:nodes @[1 2]
               :edges @[{:from 1 :to 2}
                        {:from 2 :to 2}]})

(print "\nDAG type")
(print "  valid DAG: " ((deft/type-pred :dag) dag-ok))
(print "  cycle: " ((deft/type-pred :dag) dag-cycle))
(print "  self loop: " ((deft/type-pred :dag) dag-self))


# DAG as compound type (using 'and')

(deft/deftype :no-self-loops (fn [v]
  (all (fn [e] (not= (get e :from) (get e :to))) (get v :edges))))

(deft/deftype :dag-2 (and :directed-graph :no-self-loops
                          (fn [v] (not (has-cycle? (get v :nodes) (get v :edges))))))

(print "\nDAG defined as compound type")
(print "  valid: " ((deft/type-pred :dag-2) dag-ok))
(print "  cycle: " ((deft/type-pred :dag-2) dag-cycle))


# Typed functions

(deft/deftfn traverse [g :dag] :array
  (def result @[])
  (def visited (table))
  (defn visit [node]
    (unless (get visited node)
      (put visited node true)
      (each e (get g :edges)
        (when (= (get e :from) node)
          (visit (get e :to))))
      (array/push result node)))
  (each n (get g :nodes) (visit n))
  result)

(print "\nTyped function using :dag")
(def toposorted (traverse dag-ok))

(printf "  topological sort: %p" toposorted)

(try (traverse dag-cycle)
     ([e] (printf "  cycle caught: %s" e)))


# custom graph op with cast

(defn add-edge [g from to]
  (deft/cast g :dag "add-edge")
  (array/push (g :edges) {:from from :to to})
  g)

(printf "\n  add-edge (pre-validated): %p" (in (add-edge dag-ok 3 1) :edges))
