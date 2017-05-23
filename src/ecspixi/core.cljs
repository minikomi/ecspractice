(ns ecspixi.core
  (:require [cljsjs.pixi]
            [ecspixi.ecs :as ecs]
            [reagent.core :as r]))

(def P js/PIXI)

(enable-console-print!)

;; components

(defn new-position [x y]
  (ecs/c {:name :position
          :properties {:x x
                       :y y}}))

(defn new-velocity [dx dy]
  (ecs/c {:name :velocity
          :properties {:dx dx
                       :dy dy}}))

;; entities

(def ball
  (ecs/e
   [(new-position 200 200)
    (new-velocity (- (rand-int 5) 10) (- (rand-int 5) 10))
    (ecs/c {:name :renderable
            :properties {:type :ball}})]))

;; systems

(def bounce
  (ecs/s
   {:name :bounce
    :priority 0
    :entity-filters
    [(ecs/has-component? :position)
     (ecs/has-component? :velocity)]
    :update-fn
    (fn [s es]
      (mapv
       (fn [e]
         (let [{:keys [w h]} (-> s :globals)
               {:keys [x y]} (ecs/getc e :position)
               {:keys [dx dy]} (ecs/getc e :velocity)
               new-dx (if (or (>= 0 x) (< w x)) (- dx) dx)
               new-dy (if (or (>= 0 y) (< h y)) (- dy) dy)]
           (println x y new-dx new-dy)
           (ecs/update-components e [(new-velocity new-dx new-dy)])))
       es))}))

(def move
  (ecs/s
   {:name :move
    :priority 1
    :entity-filters
    [(ecs/has-component? :position)
     (ecs/has-component? :velocity)]
    :update-fn
    (fn [_ es]
      (mapv
       (fn [e]
         (let [{:keys [x y]} (ecs/getc e :position)
               {:keys [dx dy]} (ecs/getc e :velocity)]
           (ecs/update-components e [(new-position (+ x dx) (+ y dy))])))
       es))}))

(defn prop-set! [prop k v]
  (aset prop k v)
  prop)

(defn set-position [item {:keys [x y]}]
  (-> item
      (.-position)
      (prop-set! "x" x)
      (prop-set! "y" y))
  item)

(def render
  (ecs/s
   {:name :render
    :update-fn
    (fn [s es]
      (let [stage (-> s :globals :stage)
            renderer (-> s :globals :renderer)
            c (-> (P.Graphics.)
                  (.beginFill 0xff0000)
                  (.drawRect 0 0 10 10)
                  (.endFill))]
        (set-position c (ecs/getc (first es) :position))
        (.removeChildren stage)
        (.addChild stage c)
        (.render renderer stage))
      es)}))

(def test-engine
  (ecs/create-engine {:entities [ball]
                      :systems [bounce move render]
                      :globals {:w 400
                                :h 400}}))

(defn game []
  (let [dom-node (atom false)]
    (r/create-class
     {:display-name "game"
      :component-did-mount
      (fn [this]
        (reset! dom-node (r/dom-node this))
        (let [renderer (.autoDetectRenderer P 400 400)
              stage (P.Container.)
              eng (ecs/create-engine {:entities [ball]
                                      :systems [bounce move render]
                                      :globals {:renderer renderer
                                                :stage stage
                                                :w 400
                                                :h 400}})
              loop-fn (fn loop-fn [engine]
                        (when @dom-node
                          (js/requestAnimationFrame
                           #(loop-fn (ecs/tick-engine engine)))))]
          (loop-fn eng)
          (.appendChild @dom-node (.-view renderer))))
      :component-will-unmount
      (fn [_]
        (reset! dom-node false))
      :reagent-render
      (fn []
        [:div {:id "game"}])})))

(defn init []
  (r/render [game]
            (.getElementById js/document "app")))
