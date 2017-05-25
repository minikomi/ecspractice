(ns ecspixi.core
  (:require [cljsjs.pixi]
            [ecspixi.ecs :as ecs]
            [reagent.core :as r]
            [goog.object :as gobj]))

(def P js/PIXI)

(enable-console-print!)

;; components
;; ----------------------------------------------------------------

(defrecord Position [x y])

(defn new-position [x y]
  (ecs/c {:id :position
          :properties (clj->js
                       {:x x
                        :y y})}))

(defrecord Velocity [dx dy])

(defn new-velocity [dx dy]
  (ecs/c {:id :velocity
          :properties (clj->js
                       {:dx dx
                        :dy dy})}))

;; entities
;; ----------------------------------------------------------------

(defn rand-col []
  (P.utils.rgb2hex (clj->js
                    [(+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5))])))

(defn new-ball [x y]
  (ecs/e
   [(new-position x y)
    (new-velocity (- (inc (rand-int 10)) 5) (- (inc (rand-int 10)) 5))
    (ecs/c {:id :renderable
            :properties {:type :ball
                         :graph-obj (-> (P.Graphics.)
                                        (.beginFill (rand-col))
                                        (.drawRect -2 -2 4 4)
                                        (.endFill))}})]))

;; systems
;; ----------------------------------------------------------------

(def bounce
  (ecs/s
   {:id :bounce
    :priority 0
    :required-components #{:position :velocity}
    :update-fn
    (fn [s es]
      (doseq [e es]
        (let [{:keys [w h]} (-> s :globals)
              pos (ecs/get-component e :position)
              vel (ecs/get-component e :velocity)
              x (gobj/get pos "x")
              y (gobj/get pos "y")
              dx (gobj/get vel "dx")
              dy (gobj/get vel "dy")
              new-dx (if (or (>= 0 x) (< w x)) (- dx) dx)
              new-dy (if (or (>= 0 y) (< h y)) (- dy) dy)]
          (gobj/set vel "dx" new-dx)
          (gobj/set vel "dy" new-dy))))}))

(def move
  (ecs/s
   {:id :move
    :priority 1
    :required-components #{:position :velocity}
    :update-fn
    (fn [_ es]
      (doseq [e es]
        (let [pos (ecs/get-component e :position)
              vel (ecs/get-component e :velocity)
              x (.-x pos)
              y (.-y pos)
              dx (gobj/get vel "dx")
              dy (gobj/get vel "dy")]
          (gobj/set pos "x" (+ x dx))
          (gobj/set pos "y" (+ y dy)))))}))


(defn make-input-system [stage eng]
  (set! (.-interactive stage) true)
  (set! (.-hitArea stage) (P.Rectangle. 0 0 400 400))
  (.on stage "mousedown"
       (fn [ev]
         (ecs/event! eng
                     :mouse-down)))
  (.on stage "mouseup"
       (fn [ev]
         (ecs/event! eng
                     :mouse-up))))

(defn prop-set! [obj k v]
  (gobj/set obj k v)
  obj)

(defn set-position! [graph-obj pos]
  (set! (.-position graph-obj) pos))

(defn ensure-staged! [graph-obj stage]
  (when-not (gobj/get graph-obj "staged")
    (do (gobj/set graph-obj "staged" true)
        (.addChild stage graph-obj)))
  graph-obj)

(def render
  (ecs/s
   {:id :render-graph-obj
    :required-components #{:renderable :position}
    :update-fn
    (fn update-render [eng es]
      (let [{:keys [stage renderer mouse]} (:globals eng)]
        (doseq [e es]
          (-> (ecs/get-component e :renderable)
              :graph-obj
              (ensure-staged! stage)
              (set-position! (ecs/get-component e :position))))
        (if (= :down mouse)
          (set! (.-backgroundColor renderer) 0x00ffff)
          (set! (.-backgroundColor renderer) 0x000000))
        (.render renderer stage)))}))

;; Scaffolding
;; ----------------------------------------------------------------

(defn mouse-down-handler [engine _]
  (assoc-in engine [:globals :mouse] :down))

(defn mouse-up-handler [engine _]
  (assoc-in engine [:globals :mouse] :up))

(defn make-engine [renderer stage]
  (ecs/engine {:entities
               (vec (repeatedly 2000 #(new-ball (rand-int 400) (rand-int 400))))
               :systems [bounce
                         move
                         render]
               :event-handlers {:mouse-down mouse-down-handler
                                :mouse-up mouse-up-handler}
               :globals {:renderer renderer
                         :stage stage
                         :mouse :up
                         :w 400
                         :h 400}}))

(defn game []
  (let [dom-node (atom false)
        mouse-state (atom {:mousedown false})]
    (r/create-class
     {:display-name "game"
      :component-did-mount
      (fn [this]
        (reset! dom-node (r/dom-node this))
        (let [renderer (.autoDetectRenderer P 400 400)
              stage (P.Container.)
              eng (make-engine renderer stage)]
          (make-input-system stage eng)
          (ecs/run-engine! dom-node eng)
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
