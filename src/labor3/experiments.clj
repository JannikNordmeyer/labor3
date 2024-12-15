(ns labor3.experiments
  (:require [clojure.test :refer :all]
            [labor3.core :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.lattices :refer :all]
            [conexp.io.contexts :refer :all]
            [conexp.gui.draw :refer :all]))

;Exercise 3:

;The card game from exercise one contained no binary attribute, so I could not run this experiment.

;In general, clarifying a context replaces with one representative all objects and attributes that share their object / attribute concept.
;If an object is removed from the card game this way, the card game contained a card that is essentially a copy of another card.
;If an attribute is removed from the card game this way, the card game contained an attribute that has the same value as another attribute on any card.

;Reducing a context removes all objects, whose object concepts are join-reducible and all attributes, whose attribute concepts are meet-reducible.
;If an object is removed from the card game this way, this means that there is a set of other cards, so that the intersection of their attributes are precisely the attributes of the removed card.
;If an attribute is removed from the card game this way, this means that there is a set of other attributes, so that for each card, the removed attribute can be computed as the conjunction of this set of attributes.


;Exercise 9:
(def ctx (read-context "resources/bodiesofwater.ctx"))

;(draw-lattice (concept-lattice ctx))
;clarifying or reducing the context in general has no effect on the structure of the concept lattice.
(draw-lattice (concept-lattice (reduce-context ctx)))

;(def ctx1 (make-context (disj (objects (reduce-context ctx)) "puddle") (attributes (reduce-context ctx)) (incidence-relation (reduce-context ctx))))
;(draw-lattice (concept-lattice ctx1))
;Removing an object from the reduced lattice collapses all concepts whose extent only differs in the removed object into single vertices.
;The same analogously applies for attributes.
(def ctx2 (make-context (objects (reduce-context ctx)) (disj (attributes (reduce-context ctx)) "natural") (incidence-relation (reduce-context ctx))))
(draw-lattice (concept-lattice ctx2))
