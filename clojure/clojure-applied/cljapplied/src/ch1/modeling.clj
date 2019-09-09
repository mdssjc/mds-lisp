(ns ch1.modeling)

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
