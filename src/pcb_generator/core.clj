(ns pcb-generator.core
  (:use [clojure.string :only [join]]))

;Sets the wire bend to be point-to-point
(def set-wire-bend-line
  "set wire_bend 2")

(defn set-layer
  "Sets the layer to the given layer"
  [layer]
  (str "layer " layer))

(defn draw-polygon
  "Draws a polygon going through
  the given seq of [x y] pairs.
  The first and last pair must be the same."
  [pairs]
  (when-not (= (first pairs) (last pairs))
    (throw (RuntimeException. "First and last pairs must be the same to close polygon")))
  (str "polygon " (join \  (map (fn [[x y]]
                                  (str \( x \  y \)))
                                pairs))))


