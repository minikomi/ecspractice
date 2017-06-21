(ns ecspixi.ecs
  (:require-macros [ecspixi.ecs :refer [c-swap!]])
  (:require [clojure.set :as cs]
            [goog.object :as o]
            [goog.array :as arr]))

(def MAX_PRIORITY (.-Infinity js/window))

(defn shallow-clj->obj [ m]
  (let [obj (js-obj)
        ks (.keys m)]
    (.forEach m
              (fn [v k]
                (aset obj (name k) v)))
    obj))

(defn shallow-clj->arr [ coll]
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
  (-lookup [this  k] (-lookup this k nil))
  (-lookup [this  k not-found]
    (aget properties (name k)))
  IFn
  (-invoke [this k]
    (or (aget properties (name k)) nil))
  (-invoke [this k not-found]
    (or (aget properties (name k)) not-found))
  IVolatile
  (-vreset! [_ ^:not-native new-properties]
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
  (get-component [eng entity-id component-id])
  (get-entities [eng component-id])
  (run-entities [eng component-id f])
  (remove-entity [eng entity-id])
  (frame-inc [eng])
  (run-events [eng])
  (run-systems [eng]))

(defn c [eng entity component-id properties]

  (let [current-entities (o/get (.-component->entities eng)
                                (name component-id)
                                #js[])]
    (.push current-entities entity)
    (o/set (.-component->entities eng)
           (name component-id)
           current-entities))

  (let [current-components (or (aget (.-entity->components eng) entity) #js{})]
    (o/set current-components
           (name component-id)
           (Component. component-id
                       properties))
    (o/set (.-entity->components eng) entity current-components)))

(deftype ECSEngine [event-handlers
                    ^:mutable event-bus
                    ^:mutable entity->components
                    ^:mutable component->entities
                    ^:mutable globals
                    ^:mutable systems
                    ^:mutable frame]
  Object
  (toString [this]
    (str
     (.-globals this)))
  ILookup
  (-lookup [this k]
    (or
     (aget globals (name k))
     nil))
  (-lookup [this k not-found]
    (aget globals (name k)))
  IFn
  (-invoke [this k]
    (or
     (aget globals (name k))
     nil))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  IVolatile
  (-vreset! [_  new-properties]
    (set! globals (shallow-clj->obj new-properties)))
  IDeref
  (-deref [_] globals)
  IEngine
  (get-component [eng entity-id component-id]
    (.-properties
     (aget (aget entity->components entity-id)
           (.-name component-id))))
  (get-entities [eng component-id]
    (aget component->entities (name component-id)))
  (run-entities [eng component-id  f]
    (.forEach (aget component->entities (name component-id)) (fn [ent] (f eng ent))))
  (frame-inc [engine]
    (set! frame (inc frame)))
  (run-events [engine]
    (doseq [[ev-type data] event-bus]
      (when-let [h (ev-type event-handlers)]
        (h engine data)))
    (set! event-bus #js[]))
  (run-systems [engine]
    (.forEach systems
              (fn [s]
                ((.-sys-fn s) engine))))
  (remove-entity [eng entity-id]
    (let [components (aget entity->components entity-id)]
      (.forEach (o/getKeys components)
                (fn [k]
                  (let [es (aget component->entities k)]
                    (arr/remove es entity-id))))
      (o/remove entity->components entity-id))))

(defn run-engine [eng]
  (doto eng
    (frame-inc)
    (run-events)
    (run-systems)))

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

(defn run-engine! [running!  eng]
  (let [loop-fn
        (fn loop-fn []
          (when @running!
            (do
              (run-engine eng)
              (js/requestAnimationFrame loop-fn))))]
    (loop-fn)))
