# -*- mode: janet; -*-
# Compound types using deftrecord. red-black tree, graph, digraph, DAG
#
# Demonstrates building data structures as typed records
#   - A red-black tree using predicate helpers
#   - A graph requiring edge references to match nodes
#   - A digraph inheriting graph invariants
#   - A DAG combining digraph with cycle detection
#   - Typed functions and mutation
#   - Generated accessors and mutators

(import deft)

#  # #    ##   # #   # #   #  #
#  Red/Black tree
#
#  A red-black tree node has :key, :color, :left, :right.
#  Leaves are nil. deft/define creates typed helper predicates;
#  deftrecord declares the shape; the guard recurses into children
#  for full-tree validation; deftype aliases the predicate so nil
#  is admissible as a value without changing the constructor.
#
# #  #  #

(deft/define rb-color
  "Get the :color of a node, defaulting to :black for nil."
  [node :dynamic] :keyword
  (get node :color :black))

(deft/define rb-no-red-red?
  "No red node may have a red child."
  [node :dynamic] :boolean
  (if (= (get node :color) :red)
    (and (not= (rb-color (get node :left)) :red)
         (not= (rb-color (get node :right)) :red))
    true))

(defn- rb-valid-tree?
  ```Recursive validity: every node must have a number :key,
a valid :color, and children that are also valid.
```
  [node]
  (if (nil? node) true
    (and (or (table? node) (struct? node))
         (number? (get node :key))
         (or (= :red (get node :color)) (= :black (get node :color)))
         (rb-valid-tree? (get node :left))
         (rb-valid-tree? (get node :right))
         (rb-no-red-red? node))))

(deft/deftrecord :rb-node
  ```A red-black tree node. Fields are type-checked on construction;
the guard recurses into children for full-tree validation.
```
  (field key :number)
  (field color :keyword)
  (field left :dynamic)
  (field right :dynamic)
  (guard rb-valid-tree?))

# :red-black is a compound type alias. it applies the :rb-node
# predicate without a constructor. This allows nil (leaf) as an
# admissible value for the type without having to change
# make-rb-node to accept nil.

(deft/deftype :red-black (fn [v] (deft/isa? v :rb-node)))

(def leaf nil)
(def n1 (make-rb-node 1 :black leaf leaf))
(def n3 (make-rb-node 3 :black leaf leaf))
(def n2 (make-rb-node 2 :red n1 n3))
(print "Red/Black tree type (deftrecord)")
(print "  leaf: " (deft/isa? leaf :red-black))
(print "  black node: " (deft/isa? n3 :red-black))
(print "  red+blacks: " (deft/isa? n2 :red-black))


# # #  #  ##   #   #  #
#  Graph
#
#  Two deftrecord types where one nests the other.
#  The guard cross-references edges against the node set and uses
#  type-safe accessors (edge-from, edge-to) from :edge.
#
# # #   #   #

(deft/deftrecord :edge
  "A directed edge between two nodes (values compared by identity)."
  (field from :dynamic)
  (field to :dynamic))

(deft/define in?
  "True when key `k` is present in table `s`."
  [s :table k :dynamic] :boolean
  (not= nil (in s k)))

(deft/deftrecord :graph
  ```A graph with a node set and edge list.
The guard predicate ensures all edge end-points exist in the node set.```
  (field nodes :array)
  (field edges :array)
  (guard (fn [v]
    (let [ns (frequencies (get v :nodes))]
      (all (fn [e] (and (in? ns (edge-from e))
                        (in? ns (edge-to e))))
           (get v :edges))))))

(def g-ok (make-graph @[1 2 3] @[(make-edge 1 2) (make-edge 2 3)]))
(def g-empty (make-graph @[] @[]))

(print "\nGraph type (deftrecord)")
(print "  valid: " (deft/isa? g-ok :graph))
(print "  empty: " (deft/isa? g-empty :graph))


# #   # # #   #    #  #     #
#  Digraph
#
#  A stricter type over :graph where the guard extends the predicate
#  by adding an extra condition that every edge of the graph must
#  have both :from and :to nodes.
#
# #   #    # #

(deft/define directed-edge?
  "True when edge `e` has both :from and :to."
  [e :dynamic] :boolean
  (and (not= nil (get e :from)) (not= nil (get e :to))))

(deft/deftrecord :digraph
  "A graph where every edge declares both endpoints."
  (field nodes :array)
  (field edges :array)
  (guard (fn [v]
    (and (deft/isa? v :graph)
         (all directed-edge? (get v :edges))))))

(def dg-ok (make-digraph @[1 2 3] @[(make-edge 1 2) (make-edge 2 3)]))

(print "\nDigraph type (deftrecord)")
(print "  valid: " (deft/isa? dg-ok :digraph))


##   #   # #    #   #
#  Directed Acyclic Graph (DAG)
#
#  A dag is a digraph with no cycles. Cycle detection requires
#  a function to check the graph, so deftype is used to
#  compose the :digraph predicate with a cycle check.
#
# #    #  #    #

(deft/define has-cycle?
  "Depth-first cycle detection over adjacency list."
  [nodes :array edges :array] :boolean
  (let [adj (table)
        visited (table)
        in-stack (table)]
    (each n nodes
      (put adj n @[])
      (put visited n false)
      (put in-stack n false))
    (each e edges
      (when (and (edge-from e) (edge-to e))
        (array/push (get adj (edge-from e)) (edge-to e))))
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


(deft/deftype :dag
  (fn [v]
    (and (deft/isa? v :digraph)
         (not (has-cycle? (digraph-nodes v) (digraph-edges v))))))


(def dag-ok (make-digraph @[1 2 3]
               @[(make-edge 1 2) (make-edge 2 3)]))


(print "\nDAG type (deftype over deftrecord)")
(print "  valid DAG: " (deft/isa? dag-ok :dag))


# #   #  #    #   ##   #      #         #
#  Typed functions
#
#  deftfn validates arguments at the call site and casts the return.
#  define with a :dynamic return type skips return validation, which
#  can be useful when mutation may temporarily violate invariants.
#
# #   # #  #

(deft/define traverse
  [g :dag] :array
  (def result @[])
  (def visited (table))
  (defn visit [node]
    (unless (get visited node)
      (put visited node true)
      (each e (digraph-edges g)
        (when (= (edge-from e) node)
          (visit (edge-to e))))
      (array/push result node)))
  (each n (digraph-nodes g) (visit n))
  result)

(print "\nTyped function using :dag")
(printf "  topological sort: %p" (traverse dag-ok))

# define with :dynamic return
# input :dag is validated; the return is not re-validated,
# so we can mutate and return without triggering a cycle check.

(deft/define add-edge
  [g :dag from :dynamic to :dynamic] :dynamic
  (array/push (digraph-edges g) (make-edge from to)) g)

(printf "  add-edge (pre-validated): %p" (in (add-edge dag-ok 3 1) :edges))


## #   #    #
#  Generated accessors and mutators
#
#  deftrecord generates {name}-{field} accessors and
#  set-{name}-{field}! mutators that re-validate via cast.
#
##   #   #

(print "\nAccessors and mutators")
(printf "  edge-from: %p" (edge-from (make-edge 10 20)))
(printf "  edge-to: %p" (edge-to (make-edge 10 20)))

(def g (make-graph @[1 2 3] @[]))
(printf "  nodes (before): %p" (graph-nodes g))

(set-graph-nodes g @[4 5 6])
(printf "  nodes (after mutator): %p" (graph-nodes g))

# Mutator re-validates: wrong type for :nodes triggers a cast error.
(try (set-graph-nodes g "oops")
     ([e] (printf "  mutator type error: %s" (describe e))))


## #   #    #
#  Optional fields
#
#  deftrecord supports (optional name type) clauses alongside (field name type).
#  Optional fields default to nil and can be set positionally or via :keyword.
#
#   #  #   #

(print "\nThe Labours of Heracles (and optional fields)")

(deft/deftrecord :labour
  "One of the labours of Heracles."
  (field number :number)
  (field name :string)
  (optional creature :string)
  (optional slain :boolean)
  (optional location :string))

(def t1 (make-labour 1 "Nemean Lion"
                     :creature "Lion"
                     :slain true
                     :location "Nemea"))

(prin "\n  Labour 1 (all optional): ") (deft/pp t1)

(def t2 (make-labour 2 "Lernaean Hydra"
                     :creature "Hydra"
                     :slain true))

(prin "\n  Labour 2 (partial optional): ") (deft/pp t2)

(def t3 (make-labour 3 "Ceryneian Hind"
                     :creature "Hind"))

(prin "\n  Labour 3 (minimal optional): ") (deft/pp t3)

(def t4 (make-labour 4 "Erymanthian Boar"
                     :creature "Boar"
                     :location "Erymanthos"))

(prin "\n  Labour 4: ") (deft/pp t4)

(def t5 (make-labour 5 "Augean Stables"
                     :location "Elis"
                     :creature "Cattle"))

(prin "\n  Labour 5: ") (deft/pp t5)

(def t6 (make-labour 6 "Stymphalian Birds"
                     :creature "Birds"
                     :slain true
                     :location "Stymphalos"))

(prin "\n  Labour 6: ") (deft/pp t6)

(def t7 (make-labour 7 "Cretan Bull"
                     :creature "Bull"
                     :location "Crete"))

(prin "\n  Labour 7: ") (deft/pp t7)

(def t8 (make-labour 8 "Mares of Diomedes"
                     :creature "Mares"
                     :location "Thrace"))

(prin "\n  Labour 8: ") (deft/pp t8)

(def t9 (make-labour 9 "Girdle of Hippolyta"
                     :creature "Amazon"
                     :location "Themiscyra"))

(prin "\n  Labour 9: ") (deft/pp t9)

(def t10 (make-labour 10 "Cattle of Geryon"
                      :creature "Cattle"
                      :location "Erytheia"))

(prin "\n  Labour 10: ") (deft/pp t10)

(def t11 (make-labour 11 "Apples of Hesperides"
                      :slain false
                      :creature "Dragon"
                      :location "Garden of Hesperides"))

(prin "\n  Labour 11: ") (deft/pp t11)

(def t12 (make-labour 12 "Capture Cerberus"
                      :creature "Cerberus" :slain false
                      :location "Underworld"))

(prin "\n  Labour 12: ") (deft/pp t12)

(def all-labours @[t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12])

# Accessors work for all fields (including optional)
(printf "\n  accessors (labour 1): \n    name=%s\n    creature=%s\n    slain=%p\n    location=%s\n"
        (labour-name t1)
        (labour-creature t1)
        (labour-slain t1)
        (labour-location t1))

(defn completed?
  "A labour is completed (slain/captured) when :slain is true or missing."
  [t]
  (let [s (labour-slain t)]
    (or (nil? s) s)))

(deft/define count-completed
  "How many labours are marked completed?"
  [labours :array] :number
   (length (filter completed? labours)))

(printf "  completed: %d / %d" (count-completed all-labours) (length all-labours))


# # #   # # #   #    #  #     #
#  Pretty-printing with deft/pp
#
#  Every deftrecord gets an auto-generated :pp handler that shows
#  field=value pairs. Use deft/pp to invoke it, or provide a custom
#  (print ...) clause to override.
#
# #    #  #

(print "\nPretty-printing with deft/pp")
(prin "  graph ->\n    pp: ")
(deft/pp (make-graph @[1 2] @[(make-edge 1 2)]))
(printf "    describe: %s" (describe (make-graph @[] @[])))
