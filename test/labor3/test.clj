(ns labor3.test
  (:require [clojure.test :refer :all]))

(def ctx (read-context "resources/Living-Beings-and-Water.ctx"))
(def rctx (ctx-reduce ctx))

(def operator #(attribute-derivation rctx (object-derivation rctx %)))
(def base-set (into #{} (objects rctx)))
(def pset (powerset base-set))



(println (intensive? (into #{} (objects rctx)) #(attribute-derivation rctx (object-derivation rctx %))))
