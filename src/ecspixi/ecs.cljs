(ns ecspixi.ecs
  (:require-macros [ecspixi.ecs :refer [c-swap!]])
  (:require [clojure.set :as cs]
            [goog.object :as o]))

(def MAX_PRIORITY (.-Infinity js/window))

(defn shallow-clj->obj [m]
  (let [obj (js-obj)
        ks (.keys m)]
    (loop []
      (let [v (.next ks)]
        (when (not (.-done v))
          (do (aset obj (name (.-value v)) (get m (.-value v) nil))
              (recur)))))
    obj))

(defn shallow-clj->arr [coll]
  (let [arr (array)]
    (doseq [v coll]
      (.push arr v))
    arr))

;; [E]ntity
;; ----------------------------------------------------------------

(defn e []
  (str (random-uuid)))

;; [C]omponent
;; ----------------------------------------------------------------

(deftype Component [id ^:mutable properties]
  Object
  (toString [this]
    (str (.-properties this)))
  ILookup
  (-lookup [this k] (-lookup this k nil))
  (-lookup [this k not-found]
    (o/get properties (name k) nil))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  IVolatile
  (-vreset! [_ new-properties]
    (set! properties (shallow-clj->obj new-properties)))
  IDeref
  (-deref [_] properties))

;; [S]ystem
;; ----------------------------------------------------------------

(deftype System [id priority sys-fn])

(defn s
  ([id sys-fn]
   (s id MAX_PRIORITY sys-fn))
  ([id priority sys-fn]
   (System. id priority sys-fn)))

(defn priority-sort [systems]
  (sort-by #(.-priority %) systems))

;; event bus
;; ----------------------------------------------------------------

(defn event!
  ([eng event-name]
   (event! eng event-name nil))
  ([eng event-name data]
   (.push (.-event-bus eng) [event-name data])))

;; Engine
;; ----------------------------------------------------------------

(defprotocol IEngine
  (c
    [eng entity id]
    [eng entity id properties])
  (get-entities [eng component-id])
  (get-component [eng entity-id component-id])
  (run-engine [eng]))

(defn frame-inc [engine]
  (set! (.-frame engine) (+ 1 (.-frame engine))))

(defn run-events [engine]
  (let [current-events (.-event-bus engine)]
    (doseq [[ev-type data] current-events]
      (when-let [h (ev-type (.-event-handlers engine))]
        (h engine data)))
    (set! (.-event-bus engine) #js[])))

(defn run-systems [engine]
  (doseq [s (.-systems engine)]
    ((.-sys-fn s) engine))
  nil)

(deftype ECSEngine [event-handlers
                    ^:mutable event-bus
                    ^:mutable entity->component
                    ^:mutable component->entities
                    ^:mutable globals
                    ^:mutable systems
                    ^:mutable frame]
  Object
  (toString [this]
    (str
     (.-globals this)))
  ILookup
  (-lookup [this k] (-lookup this k nil))
  (-lookup [this k not-found]
    (o/get globals (name k) nil))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  IVolatile
  (-vreset! [_ new-properties]
    (set! globals (shallow-clj->obj new-properties)))
  IDeref
  (-deref [_] globals)
  IEngine
  (c [eng entity component-id]
    (c eng entity component-id nil))
  (c [eng entity component-id properties]
    ;; add component -> entity
    (let [current-entities (o/get component->entities
                                  component-id
                                  #js[])]
      (.push current-entities entity)
      (o/set component->entities component-id
             current-entities))

    (let [current-components (o/get entity->component entity #js{})]
      (o/set current-components (name component-id)
             (Component. component-id
                         (shallow-clj->obj (or properties {}))))
      (o/set entity->component entity current-components)))

  (get-entities [eng component-id]
    (o/get component->entities component-id))

  (get-component [eng entity-id component-id]
    (aget (o/get entity->component entity-id)
          (name component-id)))

  (run-engine [this]
    (doto this
      (frame-inc)
      (run-events)
      (run-systems))))

(defn engine [{:keys [event-handlers globals systems]}]
  (ECSEngine. (or event-handlers {})
              #js[]
              #js{}
              #js{}
              (or (shallow-clj->obj globals) #js{})
              (-> (or systems [])
                  priority-sort
                  shallow-clj->arr)
              0))

(defn run-engine! [running! eng]
  (let [loop-fn
        (fn loop-fn []
          (when @running!
            (run-engine eng)
            (js/requestAnimationFrame loop-fn)))]
    (loop-fn)))
