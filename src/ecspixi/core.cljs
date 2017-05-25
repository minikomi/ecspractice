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
          :properties
          (map->Position
           {:x x
            :y y})}))

(defrecord Velocity [dx dy])

(defn new-velocity [dx dy]
  (ecs/c {:id :velocity
          :properties (map->Velocity
                       {:dx dx
                        :dy dy})}))

;; entities
;; ----------------------------------------------------------------

(defn rand-col []
  (P.utils.rgb2hex (clj->js
                    [(+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5))])))

(defn set-attr [obj k v]
  (gobj/set obj (name k) v)
  obj)

(defn new-ball [x y]
  (ecs/e
   [(new-position x y)
    (new-velocity (- (inc (rand-int 10)) 5) (- (inc (rand-int 10)) 5))
    (ecs/c {:id :renderable
            :properties {:type :ball
                         :graph-obj (-> (P.Graphics.)
                                        (.beginFill (rand-col))
                                        (.drawCircle 0 0 4)
                                        (.endFill))}})]))

;; systems
;; ----------------------------------------------------------------

(def bounce
  (ecs/s
   {:id :bounce
    :priority 0
    :required-components #{:position :velocity}
    :update-fn
    (fn bounce-update [s es]
      (doseq [e es]
        (let [{:keys [w h]} (-> s :globals)
              {:keys [x y]} @(ecs/e->c e :position)
              vel (ecs/e->c e :velocity)
              {:keys [dx dy]} @vel
              new-dx (if (or (>= 0 x) (< w x)) (- dx) dx)
              new-dy (if (or (>= 0 y) (< h y)) (- dy) dy)]
          (when (or (not= new-dx dx)
                    (not= new-dy dy))
              (vreset! vel
                       (Velocity. new-dx new-dy))))))}))

(def move
  (ecs/s
   {:id :move
    :priority 1
    :required-components #{:position :velocity}
    :update-fn
    (fn move-update [_ es]
      (doseq [e es]
        (let [pos (ecs/e->c e :position)
              {:keys [x y]} @pos
              {:keys [dx dy]} @(ecs/e->c e :velocity)]
          (vreset! pos (Position. (+ x dx)
                                  (+ y dy))))))}))

(defn make-input-system [stage eng]
  (set! (.-interactive stage) true)
  (set! (.-hitArea stage) (P.Rectangle. 0 0 (-> eng :globals :w) (-> eng :globals :h)))
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
  (set! (.-position graph-obj) #js{:x (:x pos)
                                   :y (:y pos)}))

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
      (let [{:keys [stage renderer mouse spr textures]} (:globals eng)]
        (doseq [e es]
          (-> @(ecs/e->c e :renderable)
              :graph-obj
              (ensure-staged! stage)
              (set-position! @(ecs/e->c e :position))))
        (if (= :down mouse)
          (do
            (.render renderer stage (first @textures))
            (set! (.-texture spr) (first @textures))
            (vreset! textures [(second @textures) (first @textures)]))
          (do
            (.clearRenderTexture renderer (first @textures) 0x000000)
            (.clearRenderTexture renderer (second @textures) 0x000000)))
        (.render renderer stage)))}))

;; Scaffolding
;; ----------------------------------------------------------------

(defn mouse-down-handler [engine _]
  (assoc-in engine [:globals :mouse] :down))

(defn mouse-up-handler [engine _]
  (assoc-in engine [:globals :mouse] :up))

(def W (.. js/window -document -body -clientWidth))
(def H (.. js/window -document -body -clientHeight))

(defn make-engine [renderer stage]

  (let [rt1 (.create P.RenderTexture
              (.-width renderer)
              (.-height renderer))
        rt2 (.create P.RenderTexture
             (.-width renderer)
             (.-height renderer))
        spr (P.Sprite. rt1)]
      (.addChild stage spr)

    (ecs/engine {:entities
                  (vec (repeatedly 600 #(new-ball (rand-int W) (rand-int H))))
                  :systems [bounce
                            move
                            render]
                  :event-handlers {:mouse-down mouse-down-handler
                                    :mouse-up mouse-up-handler}
                  :globals {:renderer renderer
                            :stage stage
                            :mouse :up
                            :w (.-width renderer)
                            :h (.-height renderer)
                            :textures (volatile! [rt1 rt2])
                            :spr spr}})))




(defn game []
  (let [dom-node (atom false)
        mouse-state (atom {:mousedown false})]
    (r/create-class
     {:display-name "game"
      :component-did-mount
      (fn [this]
        (reset! dom-node (r/dom-node this))
        (let [renderer (.autoDetectRenderer P W H)
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
