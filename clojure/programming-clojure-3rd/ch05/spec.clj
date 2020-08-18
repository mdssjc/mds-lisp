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



(create-ns 'demo.music.release)
(alias 'music 'demo.music.release)

(s/def ::music/id uuid?)
(s/def ::music/artist string?)
(s/def ::music/title string?)
(s/def ::music/date inst?)

(s/def ::music/release
  (s/keys :req[::music/id]
          :opt [::music/artist
                ::music/title
                ::music/date]))


(s/def ::music/release-unqualified
  (s/keys :req-un[::music/id]
          :opt-un [::music/artist
                   ::music/title
                   ::music/date]))
