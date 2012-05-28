(ns pcb-generator.core
  (:use [seesaw core graphics color])
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

(def shapes (atom []))

(defn- make-drawing-fn
  [shapes]
  (fn [c g]
    (doseq [s shapes]
      (let [xs (map first s)
            ys (map second s)
            n (count s)] 
        (.drawPolygon g
                      (int-array xs)
                      (int-array ys)
                      n)))))

(defn create-canvas-frame
  []
  (native!)
  (let [canvas (canvas :id :c)
        f (frame :title "Shape"
                 :width 500
                 :height 500
                 :content canvas)]
    (show! f)
    (fn show [& shapes]
      (config! canvas
               :paint (make-drawing-fn shapes)))))

(defn center-on
  [shape w h]
  (let [w2 (/ w 2)
        h2 (/ h 2)] 
    (map (fn [[x y]] [(+ w2 x) (+ h2 y)]) shape)))

(defn make-n-gon
  [radius n]
  (let [angle (/ (* 2 java.lang.Math/PI) n)]
    (letfn [(scale [x] (int (* (/ radius
                                  (java.lang.Math/cos (/ angle 2))) x)))
            (angle->point
              [a] 
              [(scale (java.lang.Math/cos a))
               (scale (java.lang.Math/sin a))])]
      (let [pairs (->> (repeat (inc n) angle)
                    (reductions + 0))] 
        (map angle->point pairs)))))

(defn make-rect
  [w h]
  (let [w2 (/ w 2)
        h2 (/ h 2)]
    [[(- w2) (- h2)]
     [(- w2) h2]
     [w2 h2]
     [w2 (- h2)]]))

(defn trans
  [shape xoff yoff]
  (map (fn [[x y]] [(+ x xoff) (+ y yoff)]) shape))

(defn quad-comb
  [w h spacing]
  (let [teeth (repeat 4 (make-rect w h))
        step (+ w spacing)
        woff (* 1.5 step)
        offs (reductions + (- woff) (repeat 4 step))]
    (map #(trans %1 %2 0) teeth offs)))

(defn rot
  [angle x y]
  (let [t (java.lang.Math/atan2 y x)
        r (java.lang.Math/sqrt (+ (* x x) (* y y)))
        t' (+ t angle)
        x' (* r (java.lang.Math/cos t'))   
        y' (* r (java.lang.Math/sin t'))]
    [x' y']))

(defn rot-shape
  [angle shape]
  (map #(apply rot angle %) shape))

(defn radiate
  [angle radius shape]
  (rot-shape angle (trans shape 0 radius)))

(defn center-all-on
  [shapes w h]
  (for [s shapes]
    (center-on s w h)))

(defn make-radiated-comb
  [angle radius w h spacing]
  (map #(radiate angle radius %) (quad-comb w h spacing)))

(defn make-radiated-combs
  [n radius w h spacing]
  (let [angle (/ (* 2 java.lang.Math/PI) n)
        angles (reductions + 0 (repeat n angle))]
    (mapcat #(make-radiated-comb % (- radius (/ h 2)) w h spacing) angles)))

(defn fix-polygon
  "Adds a new endpoint to duplicate the beginning if needed. Idempotent."
  [poly]
  (if-not (= (last poly) (first poly))
    (conj poly (last poly))
    poly))

;This makes a toothy hexagon
;(apply f (center-all-on (conj (make-radiated-combs 6 76 10 20 5) (make-n-gon 100 6)) 500 500))
;(clojure.string/join "\n" (map (comp draw-polygon fix-polygon) (center-all-on (conj (make-radiated-combs 6 100 10 20 5) (make-n-gon 100 6)) 500 500)))
