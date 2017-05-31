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
          :properties {:x x :y y}}))

(defn new-velocity [dx dy]
  (ecs/c {:id :velocity
          :properties {:dx dx :dy dy}}))


;; entities
;; ----------------------------------------------------------------

(defn rand-col []
  (P.utils.rgb2hex (clj->js
                    [(+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5)) (+ 0.5 (rand 0.5))])))

(defn set-attr [obj k v]
  (gobj/set obj (name k) v)
  obj)

(defn new-bunny [stage x y]
  (let [bunny (.fromImage P.Sprite
                          "https://pixijs.github.io/examples/required/assets/basics/bunny.png")]
    (.addChild stage bunny)
    (ecs/e
     [(new-position x y)
      (new-velocity (- (inc (rand-int 10)) 5) (- (inc (rand-int 10))))
      (ecs/c {:id :renderable
              :properties {:type :bunny :graph-obj bunny}})])))

;; systems
;; ----------------------------------------------------------------

(def bounce
  (ecs/s
   {:id :bounce
    :priority 0
    :required-components #{:position :velocity}
    :update-fn
    (fn bounce-update [engine es]
      (let [w (:w (.-globals engine))
            h (:h (.-globals engine))]
        (doseq [e es]
          (let [{:keys [x y] :as pos} (:position e)
                {:keys [dx dy] :as pos} (:velocity e)
                new-dx (if (or (>= 0 x) (< w x)) (- dx) dx)
                new-dy (if (or (>= 0 y) (< h y)) (- dy) (+ 1 dy))]
            (vreset! (:velocity e) {:dx new-dx :dy new-dy})))))}))

(def move
  (ecs/s
   {:id :move
    :priority 1
    :required-components #{:position :velocity}
    :update-fn
    (fn move-update [_ es]
      (doseq [e es]
        (let [{:keys [x y] :as pos} (:position e)
              {:keys [dx dy]} (:velocity e)]
          (vreset! (:position e)
                   {:x (+ x dx)
                    :y (+ y dy)}))))}))

(def render
  (ecs/s
   {:id :render-graph-obj
    :required-components #{:renderable :position}
    :update-fn
    (fn update-render [eng es]
      (let [{:keys [stage renderer mouse spr]} (.-globals eng)]
        (doseq [e es]
          (let [{:keys [x y]} (:position e)
                go (:graph-obj (:renderable e))]
            (.set (.-position go) x y)))
        (.render renderer stage)))}))

;; Scaffolding
;; ----------------------------------------------------------------

(defn make-input-system [stage eng]
  (set! (.-interactive stage) true)
  (set! (.-hitArea stage)
        (P.Rectangle. 0 0 ((.-globals eng) :w) ((.-globals eng) :h)))
  (.on stage "mousedown"
       (fn [ev]
         (ecs/event! eng :mouse-down {:x (.. ev -data -global -x)
                                      :y (.. ev -data -global -y)})))
  (.on stage "mouseup"
       (fn [ev] (ecs/event! eng :mouse-up))))

(defn mouse-down-handler [engine pos]
  (let [w (:w (.-globals engine))
        h (:h (.-globals engine))
        stage (:stage (.-globals engine))
        entities (.-entities engine)]
    (doseq [_ (range 50)]
      (.push (.-entities engine)
            (new-bunny stage (:x pos) (:y pos))))
    (when (< 2000 (.-length entities))
      (let [n (- (.-length entities) 2000)
            removed (.slice entities 0 n)
            remain (.slice entities n)]
        (doseq [e removed]
          (.removeChild stage
                        (:graph-obj (:renderable e))))
        (set! (.-entities engine) remain)))))

(defn mouse-up-handler [engine _])

(defn make-engine [renderer stage]
   (ecs/engine {:entities
                (vec (repeatedly 5
                                 #(new-bunny stage
                                             (rand-int (.-width renderer))
                                             (rand-int (.-height renderer)))))
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
