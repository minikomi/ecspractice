(ns ecspixi.core
  (:require [cljsjs.pixi]
            [ecspixi.ecs :as ecs]
            [reagent.core :as r]
            [goog.object :as gobj]))

(def P js/PIXI)

(enable-console-print!)

;; components
;; ----------------------------------------------------------------

(defn new-position [x y]
  (ecs/c {:id :position
          :properties {:x x
                       :y y}}))

(defn new-velocity [dx dy]
  (ecs/c {:id :velocity
          :properties {:dx dx
                       :dy dy}}))

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
      (mapv
       (fn [e]
         (let [{:keys [w h]} (-> s :globals)
               {:keys [x y]} (ecs/get-component e :position)
               {:keys [dx dy]} (ecs/get-component e :velocity)
               new-dx (if (or (>= 0 x) (< w x)) (- dx) dx)
               new-dy (if (or (>= 0 y) (< h y)) (- dy) dy)]
           (ecs/set-component e :velocity {:dx new-dx :dy new-dy})))
       es))}))

(def move
  (ecs/s
   {:id :move
    :priority 1
    :required-components #{:position :velocity}
    :update-fn
    (fn [_ es]
      (mapv
       (fn [e]
         (let [{:keys [x y]} (ecs/get-component e :position)
               {:keys [dx dy]} (ecs/get-component e :velocity)]
           (ecs/set-component e :position {:x (+ x dx) :y (+ y dy)})))
       es))}))

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

(defn set-position! [graph-obj {:keys [x y]}]
  (-> graph-obj
      (.-position)
      (prop-set! "x" x)
      (prop-set! "y" y))
  graph-obj)

(defn ensure-staged! [graph-obj stage]
  (when-not (gobj/get graph-obj "staged")
    (do (gobj/set graph-obj "staged" true)
        (.addChild stage graph-obj)))
  graph-obj)

(def render
  (ecs/s
   {:id :render-graph-obj
    :required-components
    #{:renderable :position}
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

(defn mouse-down-handler [engine]
  (println "down")
  (assoc-in engine [:globals :mouse] :down))

(defn mouse-up-handler [engine]
  (assoc-in engine [:globals :mouse] :up))

(defn make-engine [renderer stage]
  (ecs/engine {:entities [(new-ball (rand-int 400) (rand-int 400))]
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
