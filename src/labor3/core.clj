(ns labor3.core
  (:require [clojure.set :as set]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.lattices :refer :all]
            [conexp.io.contexts :refer :all]))

(defn- powerset [a]
  "Computes the Powerset of the Supplied Set."
  (apply set/union #{a} (map #(powerset (disj a %)) a)))

(defn- equivalent-objs [ctx obj]
  "Returns all Objects that have the same Incidence as the Supplied Object in the Supplied Context."
  (let [objs (objects ctx)
        incident-attrs (object-derivation ctx #{obj})]
    (filter #(= incident-attrs (object-derivation ctx #{%})) objs))
  )

(defn- equivalent-attrs [ctx attr]
  "Returns all Attributes that have the same Incidence as the Supplied Attribute in the Supplied Context."
  (let [attrs (attributes ctx)
        incident-objs (attribute-derivation ctx #{attr})]
    (filter #(= incident-objs (attribute-derivation ctx #{%})) attrs))
  )

(defn obj-clarifiable? [ctx obj]
  "Verifies whether the Supplied Object has the same Incidence as another Object in the Supplied Context."
  (not= (equivalent-objs ctx obj) #{obj})
  )

(defn attr-clarifiable? [ctx attr]
  "Verifies whether the Supplied Attribute has the same Incidence as another Attribute in the Supplied Context."
  (not= (equivalent-attrs ctx attr) #{attr})
  )

(defn- clarify-objs [ctx]
  "Returns the Set of Objects resulting from Clarifying the Supplied Context."
    (loop [clarified-objs #{}
           remaining-objs (objects ctx)]
      (if (empty? remaining-objs)
        clarified-objs
        (recur (conj clarified-objs (first remaining-objs))
               (set/difference remaining-objs (equivalent-objs ctx (first remaining-objs))))))
  )

(defn- clarify-attrs [ctx]
  "Returns the Set of Objects resulting from Clarifying the Supplied Context."
  (loop [clarified-attrs #{}
         remaining-attrs (attributes ctx)]
    (if (empty? remaining-attrs)
      clarified-attrs
      (recur (conj clarified-attrs (first remaining-attrs))
             (set/difference remaining-attrs (equivalent-attrs ctx (first remaining-attrs))))))
  )

(defn ctx-clarify [ctx]
  "Returns a Clarified Version of the Supplied Context."
  (make-context (clarify-objs ctx) (clarify-attrs ctx) (incidence-relation ctx))
  )

(defn obj-reducible? [ctx obj]
  "Verifies whether the Supplied Object is Reducible in the Supplied Context."
  (let [object-concept [(attribute-derivation ctx (object-derivation ctx #{obj})) (object-derivation ctx #{obj})]
        join-irreducibles (lattice-sup-irreducibles (concept-lattice ctx))]
    (not (.contains join-irreducibles object-concept)))
  )

(defn attr-reducible? [ctx attr]
  "Verifies whether the Supplied Attribute is Reducible in the Supplied Context."
  (let [attribute-concept [(attribute-derivation ctx #{attr}) (object-derivation ctx (attribute-derivation ctx #{attr}))]
        meet-irreducibles (lattice-inf-irreducibles (concept-lattice ctx))]
    (not (.contains meet-irreducibles attribute-concept)))
  )

(defn ctx-reduce [ctx]
  "Returns a Reduced Version of the Supplied Context."
  (let [clarified-ctx (ctx-clarify ctx)
        irreducible-objs (filter #(not (obj-reducible? clarified-ctx %)) (objects clarified-ctx))
        irreducible-attrs (filter #(not (attr-reducible? clarified-ctx %)) (attributes clarified-ctx))]
    (make-context irreducible-objs irreducible-attrs (incidence-relation clarified-ctx)))
  )

(defn increasing? [base-set operator]
  (let [pset (powerset base-set)]
    (reduce #(and %1 %2) (for [x pset y pset] (or (not (set/subset? x y)) (set/subset? (operator x) (operator y))))))
  )

(defn idempotent? [base-set operator]
  (reduce #(and %1 %2) (for [x base-set] (= (operator x) (operator (operator x)))))
  )


(defn extensive? [base-set operation]
  (reduce #(and %1 %2) (for [x (powerset base-set)] (set/subset? x (operation x))))
  )

(defn intensive? [base-set operation]
  (reduce #(and %1 %2) (for [x (powerset base-set)] (set/subset? (operation x) x)))
  )

(defn closure-operator? [base-set operation]
  (and (increasing? base-set operation)
       (idempotent? base-set operation)
       (extensive? base-set operation))
  )

(defn core-operator? [base-set operation]
  (and (increasing? base-set operation)
       (idempotent? base-set operation)
       (intensive? base-set operation))
  )

(defn closure-system? [base-set subsets]
    (and (every? #(set/subset? % base-set) subsets)
         (every? #(.contains subsets (reduce set/intersection %)) (disj (powerset subsets) #{})))
  )

(defn minimal-closure-system [base-set subsets]
  "Returns the Minimal Closure System that Contains the Supplied Subsets."
  (into #{} (for [x (disj (powerset subsets) #{})] (reduce set/intersection x)))
  )

(defn maximal-closure-system [base-set subsets]
  "Returns a Maximal Closure System that is Contained in the Supplied Subsets."
  (loop [candidates (reverse (sort set/subset? (powerset subsets)))]
    (if (closure-system? base-set (first candidates))
      (first candidates)
      (recur (rest candidates))))
  )

(defn- first-closure [ctx]
  (object-derivation ctx (attribute-derivation ctx #{}))
  )

(defn- next-closure [closure-operator old-closure lectic-order]
  (loop [remaining (reverse lectic-order)
         current-closure old-closure]
      (if (.contains current-closure (first remaining))
        (recur (rest remaining)
               (conj current-closure (first remaining)))

        (if (every? #(< % (first remaining)) (closure-operator (conj current-closure (first remaining))))
          (closure-operator (conj current-closure (first remaining)))
          (recur (rest remaining)
                 current-closure))))
  )

(defn all-closures [ctx]
  (let [closure-operator #(object-derivation ctx (attribute-derivation ctx %))
        lectic-order (into [] (attributes ctx))]
    (loop [current-closure (first-closure ctx)
           closures #{}]
      (if current-closure
        (recur (next-closure closure-operator current-closure lectic-order)
               (conj closures current-closure))
        (conj closures current-closure))))
  )


(def ctx (read-context "resources/bodiesofwater.ctx"))
(def rctx (ctx-reduce ctx))


(println (all-closures ctx))

;(println (reverse (sort set/subset? (powerset #{1 2 3 4}))))
;(println (maximal-closure-system #{1 2 3 4} #{#{1 2 3} #{2 3 4} #{1 2}}))


