(ns ecspixi.core
  (:require [cljsjs.pixi]
            [ecspixi.ecs :as ecs]
            [reagent.core :as r]
            [goog.object :as gobj]))

(def P js/PIXI)

(enable-console-print!)

;; entities
;; ----------------------------------------------------------------

(defn rand-col []
  (P.utils.rgb2hex (clj->js
                    [(+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5))])))

(defn get-sprite []
  (.fromImage P.Sprite "https://pixijs.github.io/examples/required/assets/basics/bunny.png"))

(deftype Velocity [^:mutable dx ^:mutable dy]
  Object
  (set [_ dx' dy']
    (set! dx dx')
    (set! dy dy')))

(defn make-bunny [^not-native eng x y]
  (let [bunny (ecs/e)
        spr (get-sprite)]
    (.set (.-position spr) x y)
    (.addChild (:stage eng) spr)
    (ecs/c eng bunny :bunny nil)
    (ecs/c eng bunny :position (.-position spr))
    (ecs/c eng bunny :velocity (Velocity. (rand-int 10) (rand-int 10)))
    (ecs/c eng bunny :renderable spr)))

(defn delete-bunny [eng entity-id]
  (let [spr (:spr (ecs/get-component eng entity-id :renderable))]
    (.removeChild (:stage eng) spr)
    (ecs/remove-entity eng entity-id)))

;; systems
;; ----------------------------------------------------------------

(def bounce
  (ecs/s :bounce 0
         (fn bounce-update [^not-native eng]
           (let [w (:w eng)
                 h (:h eng)
                 update-fn
                 (fn [_ e]
                  (let [pos (ecs/get-component eng e :position)
                        x (.-x pos)
                        y (.-y pos)
                        vel (ecs/get-component eng e :velocity)
                        new-dx (if (or (>= 0 x) (< w x))
                                 (- (.-dx vel))
                                 (.-dx vel))
                        new-dy (if (or (>= 0 y) (< h y))
                                 (- (.-dy vel))
                                 (+ 1 (.-dy vel)))]
                    (.set vel new-dx new-dy)))]
              (ecs/run-entities eng :bunny update-fn)))))

(def move
  (ecs/s :move 1
         (fn move-update [^not-native eng]
           (ecs/run-entities eng :bunny
                             (fn [_ e]
                                (let [pos (ecs/get-component eng e :position)
                                      vel (ecs/get-component eng e :velocity)]
                                  (.set pos
                                        (+ (.-x pos) (.-dx vel))
                                        (+ (.-y pos) (.-dy vel)))))))))

(def render
  (ecs/s :render
         (fn update-render [^not-native eng]
           (.render (:renderer eng) (:stage eng)))))

;; Scaffolding
;; ----------------------------------------------------------------

(defn make-input-system [eng]
  (set! (.-interactive (:stage eng)) true)
  (set! (.-hitArea (:stage eng))
        (P.Rectangle. 0 0 (:w eng) (:h eng)))
  ;; mouse down broadcast
  (.on (:stage eng) "mousedown"
       (fn [ev]
         (ecs/event! eng :mouse-down {:x (.. ev -data -global -x)
                                      :y (.. ev -data -global -y)})))
  ;; mouse up broadcast
  (.on (:stage eng) "mouseup"
       (fn [ev] (ecs/event! eng :mouse-up))))

(defn mouse-down-handler [engine pos]
  (dotimes [_ 10000]
    (make-bunny engine (+ (rand-int 500) (:x pos)) (+ (rand-int 500) (:y pos)))))

(defn mouse-up-handler [engine _])

(defn make-engine [renderer stage]
  (ecs/engine {:systems [bounce move render]
               :event-handlers {:mouse-down mouse-down-handler
                                :mouse-up mouse-up-handler}
               :globals {:renderer renderer
                         :stage stage
                         :mouse :up
                         :w (.-width renderer)
                         :h (.-height renderer)}}))

(defn game []
  (let [dom-node (atom false)
        mouse-state (atom {:mousedown false})
        W (.. js/window -document -body -clientWidth)
        H (.. js/window -document -body -clientHeight)]
    (r/create-class
     {:display-name "game"
      :component-did-mount
      (fn [this]
        (reset! dom-node (r/dom-node this))
        (let [renderer (.autoDetectRenderer P W H)
              stage (P.Container.)
              eng (make-engine renderer stage)]
          (make-input-system eng)
          (dotimes [_ 10000] (make-bunny eng (rand-int W) (rand-int H)))
          (time (dotimes [n 100]
                  (ecs/run-engine eng)))
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
