(ns labor3.test
  (:require [clojure.test :refer :all]
            [labor3.core :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.lattices :refer :all]
            [conexp.io.contexts :refer :all]))

(def ctx1 (read-context "resources/Living-Beings-and-Water.ctx"))
(def ctx2 (read-context "resources/bodiesofwater.ctx"))
(def ctx3 (read-context "resources/ben-and-jerrys-flavors.ctx"))
(def ctx4 (read-context "resources/Diagnosis.ctx"))

(deftest test-clarify
  (is (= (objects (ctx-clarify ctx1)) #{"dog" "fish leech" "corn" "bream" "water weeds" "bean" "frog" "reed"}))
  (is (= (attributes (ctx-clarify ctx1)) #{"has limbs" "breast feeds" "needs chlorophyll" "needs water to live" "lives on land" "lives in water" "can move" "monocotyledon" "dicotyledon"}))

  (is (= (objects (ctx-clarify ctx2)) #{"river" "reservoir" "puddle" "sea" "maar" "canal"}))
  (is (= (attributes (ctx-clarify ctx2)) #{"running" "maritime" "constant" "natural" "stagnant"}))

  (is (= (objects (ctx-clarify ctx3)) #{"Peanut Butter Cup" "Fudge Brownie" "Caramel Sutra" "Salted Caramel Brownie" "Caramel Chew Chew" "Half Baked" "Cookie Dough"}))
  (is (= (attributes (ctx-clarify ctx3)) #{"Choco Ice" "Peanut Ice" "Choco Pieces" "Brownie" "Dough" "Caramel Ice" "Vanilla" "Caramel"}))
  )

(deftest test-reduce
  (is (= (objects (ctx-reduce ctx1)) #{"dog" "fish leech" "corn" "bream" "water weeds" "bean" "frog" "reed"}))
  (is (= (attributes (ctx-reduce ctx1)) #{"has limbs" "breast feeds" "needs chlorophyll" "lives on land" "lives in water" "can move" "monocotyledon" "dicotyledon"}))

  (is (= (objects (ctx-reduce ctx2)) #{"river" "reservoir" "puddle" "sea" "maar" "canal"}))
  (is (= (attributes (ctx-reduce ctx2)) #{"running" "maritime" "constant" "natural" "stagnant"}))

  (is (= (objects (ctx-reduce ctx4)) #{"Patient#111" "Patient#119" "Patient#31" "Patient#32" "Patient#17" "Patient#27" "Patient#105" "Patient#58" "Patient#65" "Patient#103" "Patient#56" "Patient#98" "Patient#43" "Patient#50"}))
  (is (= (attributes (ctx-reduce ctx4)) #{"[Lumbar pain yes]" "[Bladder inflammation? yes]" "[Burning no]" "[Lumbar pain no]" "[Nausea no]" "[Burning yes]" "[Temperatur [∈ [40.0 42.0]]]" "[Micturition pains no]" "[Temperatur [∈ [35.0 37.5]]]" "[Pelvis nephritis? no]" "[Micturition pains yes]" "[Pelvis nephritis? yes]" "[Urine pushing yes]" "[Temperatur [∈ [37.5 40.0]]]" "[Bladder inflammation? no]"}))
  )

(deftest test-operators
  (is (closure-operator? (into #{} (objects ctx1)) #(attribute-derivation ctx1 (object-derivation ctx1 %))))
  (is (closure-operator? (into #{} (attributes ctx1)) #(object-derivation ctx1 (attribute-derivation ctx1 %))))
  (is (not (core-operator? (into #{} (objects ctx1)) #(attribute-derivation ctx1 (object-derivation ctx1 %)))))
  (is (not (core-operator? (into #{} (attributes ctx1)) #(object-derivation ctx1 (attribute-derivation ctx1 %)))))

  (is (closure-operator? #{1 2 3 4 5} identity))
  (is (core-operator? #{1 2 3 4 5} identity))

  (is (not (closure-operator? #{1 2 3 4 5} (constantly #{}))))
  (is (core-operator? #{1 2 3 4 5} (constantly #{})))
  )

(deftest test-closure-system
  (is (closure-system? #{1 2 3 4 5} #{#{1 2 3} #{2 3 4} #{2 3}}))
  (is (closure-system? #{1 2 3 4 5} #{#{1 2 3} #{3 4 5} #{2 3} #{3} #{} #{1 2 3 4 5}}))

  (is (not (closure-system? #{1 2 3 4 5} #{#{1 2 3} #{2 3 4}})))
  (is (not (closure-system? #{1 2 3 4 5} #{#{1 2 3} #{3 4 5} #{2 3} #{3} #{} #{1 2 3 4 5} #{1 2}})))

  (is (= (minimal-closure-system #{1 2 3 4 5} #{#{1 2 3} #{3 4 5}}) #{#{1 2 3} #{3 4 5} #{3}}))
  (is (= (minimal-closure-system #{1 2 3 4 5} #{#{1 2} #{3 4} #{4 5}}) #{#{} #{4} #{1 2} #{3 4} #{4 5}}))

  (is (= (maximal-closure-system #{1 2 3 4 5} #{#{1 2 3} #{3 4 5}}) #{#{3 4 5}}))
  (is (= (maximal-closure-system #{1 2 3 4 5} #{#{1} #{1 2} #{3 4} #{4 5}}) #{#{1} #{1 2}}))
  )

(deftest test-next-closure
  (is (= (into #{} (all-closures ctx1)) (into #{} (concepts ctx1))))
  (is (= (into #{} (all-closures ctx2)) (into #{} (concepts ctx2))))
  (is (= (into #{} (all-closures ctx3)) (into #{} (concepts ctx3))))
  )
