(ns ecspixi.core
  (:require [cljsjs.pixi]
            [ecspixi.ecs :as ecs]
            [reagent.core :as r]
            [goog.object :as gobj]))

(def P js/PIXI)

(enable-console-print!)

;; components
;; ----------------------------------------------------------------

(deftype Position [x y])

(defn new-position [x y]
  (ecs/c {:id :position
          :properties (Position. x y)}))

(deftype Velocity [dx dy])

(defn new-velocity [dx dy]
  (ecs/c {:id :velocity
          :properties (Velocity. dx dy)}))

;; entities
;; ----------------------------------------------------------------

(defn rand-col []
  (P.utils.rgb2hex (clj->js
                    [(+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5))])))

(defn set-attr [obj k v]
  (gobj/set obj (name k) v)
  obj)

(deftype Renderable [type graph-obj])

(defn new-bunny [stage x y]
  (let [bunny (.fromImage P.Sprite "https://pixijs.github.io/examples/required/assets/basics/bunny.png")]
    (.addChild stage bunny)
    (ecs/e
     [(new-position x y)
      (new-velocity (- (inc (rand-int 10)) 5) (- (inc (rand-int 10))))
      (ecs/c {:id :renderable
              :properties (Renderable. :bunny bunny)})])))

;; systems
;; ----------------------------------------------------------------

(def bounce
  (ecs/s
   {:id :bounce
    :priority 0
    :required-components #{:position :velocity}
    :update-fn
    (fn bounce-update [engine es]
      (.forEach es
                (fn bounce-inner [e]
                  (let [w (:w (.-globals engine) 0)
                        h (:h (.-globals engine) 0)
                        pos @(gobj/get (.-components e)  "position")
                        x (aget pos "x")
                        y (aget pos "y")
                        vel-at (gobj/get (.-components e) "velocity")
                        vel @vel-at
                        dx (aget vel "dx")
                        dy (aget vel "dy")
                        new-dx (if (or (>= 0 x) (< w x)) (- dx) dx)
                        new-dy (if (or (>= 0 y) (< h y)) (- dy) (+ dy 2))]
                    (when (or (not (identical? new-dx dx))
                              (not (identical? new-dy dy)))
                      (vreset! vel-at (Velocity. new-dx new-dy)))))))}))

(def move
  (ecs/s
   {:id :move
    :priority 1
    :required-components #{:position :velocity}
    :update-fn
    (fn move-update [_ es]
      (.forEach es
                (fn [e]
                  (let [pos-at (ecs/get-component e :position)
                        pos @pos-at
                        x (aget pos "x")
                        y (aget pos "y")
                        vel @(ecs/get-component e :velocity)
                        dx (aget vel "dx")
                        dy (aget vel "dy")]
                    (vreset! pos-at (Position. (+ x dx)
                                               (+ y dy)))))))}))

(defn make-input-system [stage eng]
  (set! (.-interactive stage) true)
  (set! (.-hitArea stage)
        (P.Rectangle. 0 0 (-> eng :globals :w) (-> eng :globals :h)))
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
      (let [{:keys [stage renderer mouse spr]} (.-globals eng)]
        (.forEach es
                  (fn [e]
                    (let [pos @(aget (.-components e) "position")
                          go (.-graph-obj @(aget (.-components e) "renderable"))]
                      (.set (.-position go)
                            (.-x pos)
                            (.-y pos)))))
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
  (ecs/engine {:entities
               (vec (repeatedly 12000 #(new-bunny stage (rand-int W) (rand-int H))))
               :systems [bounce move render]
               :event-handlers {:mouse-down mouse-down-handler
                                :mouse-up mouse-up-handler}
               :globals {:renderer renderer
                         :stage stage
                         :mouse :up
                         :w (.-width renderer)
                         :h (.-height renderer)}}))

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
