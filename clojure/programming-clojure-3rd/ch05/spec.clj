(ns spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

;; Specs describing an ingredient
(s/def ::ingredient (s/keys :req [::name ::quantity ::unit]))
(s/def ::name string?)
(s/def ::quantity number?)
(s/def ::unit keyword?)

;; Function spec for scale-ingredient
(s/fdef scale-ingredient
  :args (s/cat :ingredient ::ingredient :factor number?)
  :ret ::ingredient)

(defn scale-ingredient [ingredient factor]
  (update ingredient :quantity * factor))
