(ns ch1.modeling)

;; Map
(def earth {:name       "Earth"
            :moons      1
            :volume     1.08321e12 ;; km^3
            :mass       5.97219e24 ;; kg
            :aphelion   152098232  ;; km, farthest from sun
            :perihelion 147098290  ;; km, closest to sun
            })

(def earth {:name       "Earth"
            :moons      1
            :volume     1.08321e12 ;; km^3
            :mass       5.97219e24 ;; kg
            :aphelion   152098232  ;; km, farthest from sun
            :perihelion 147098290  ;; km, closest to sun
            :type       :Planet    ;; entity type
            })

;; Record
(defrecord Planet [name
                   moons
                   volume     ;; km^3
                   mass       ;; kg
                   aphelion   ;; km, farthest from sun
                   perihelion ;; km, closest to sun
                   ])

(def earth
     (->Planet "Earth" 1 1.08321e12 5.97219e24 152098232 147098290))

(def earth
     (map->Planet {:name       "Earth"
                   :moons      1
                   :volume     1.08321e12
                   :mass       5.97219e24
                   :aphelion   152098232
                   :perihelion 147098290}))
