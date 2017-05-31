(ns ecspixi.ecs
  (:require-macros [ecspixi.ecs :refer [c-swap!]])
  (:require [clojure.set :as cs]
            [goog.object :as o]))

(def MAX_PRIORITY (.-Infinity js/window))

(defn assoc-by-id
  ([items] (assoc-by-id {} items))
  ([item-map items]
   (reduce #(assoc % (.-id %2) %2) item-map items)))

(defn shallow-clj->js [m]
  (let [obj (js-obj)
        v (vec m)
        c (count v)]
    (dotimes [n c]
      (aset obj (name (nth (nth v n) 0)) (nth (nth v n) 1)))
    obj))

;; [E]ntity
;; ----------------------------------------------------------------

(defn has-component? [c-name]
  (fn component-find [entity]
    (-> entity
        :components
        (get c-name false))))

(defn e->c [entity c-name]
  (get-in entity [:components c-name]))

(defprotocol IECSEntity
  (get-component
    [this k]
    [this k not-found]))

(deftype Entity [id components component-set]
  ILookup
  (-lookup [this k] (-lookup this k nil))
  (-lookup [this k not-found]
    (get-component this k not-found))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  IECSEntity
  (get-component [this k]
    (get-component this k nil))
  (get-component [this k not-found]
    (o/get components (name k) not-found)))

(defn e [components]
  (Entity.
   (random-uuid)
   (shallow-clj->js (assoc-by-id {} (or components [])))
   (->> components
        (map #(vector (o/get % "id") true))
        (into {})
        shallow-clj->js)))

;; [C]omponent
;; ----------------------------------------------------------------

(deftype Component [id ^:mutable properties]
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
    (set! properties (shallow-clj->js new-properties)))
  IDeref
  (-deref [_] properties))

(defn c [{:keys [id properties]}]
  (Component. id (shallow-clj->js (or properties {}))))

;; [S]ystem
;; ----------------------------------------------------------------

(defprotocol IECSSystem
  (get-should-run [_])
  (get-update-fn [_])
  (get-required-components [_])
  (get-priority [this]))

(deftype System [id priority update-fn should-run required-components entity-filter]
  IECSSystem
  (get-priority [this]
    priority)
  (get-should-run [_]
    should-run)
  (get-update-fn [_]
    update-fn)
  (get-required-components [_]
    required-components))

(defn s [{:keys [id priority update-fn should-run required-components]}]
  (let [rc-arr (clj->js (or (vec required-components) []))]
    (System. id
             (or priority MAX_PRIORITY)
             update-fn
             (or should-run (constantly true))
             required-components
             (fn [e]
               (.every
                rc-arr
                #(o/get (.-component-set e) %))))))

(defn priority-sort [systems]
  (sort-by get-priority systems))

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
  (run-engine [o]))

(defn frame-inc [engine]
  (set! (.-frame engine) (+ 1 (.-frame engine))))

(defn run-events [engine]
  (let [current-events (.-event-bus engine)]
    (doseq [[ev-type data] current-events]
      (when-let [h (ev-type (.-event-handlers engine))]
        (h engine data)))
    (set! (.-event-bus engine) #js[])))

(defn run-systems [engine]
  (.forEach (.-systems engine)
            (fn [system]
              ((.-update-fn system) engine
               (.filter
                (.-entities engine)
                (.-entity-filter system)))))

  nil)

(deftype ECSEngine [event-handlers
                    ^:mutable event-bus
                    ^:mutable globals
                    ^:mutable entities
                    ^:mutable systems
                    ^:mutable frame]
  Object
  (toString [this]
            (str
             (.-globals this)))
  IEngine
  (run-engine [this]
              (doto this
                (frame-inc)
                (run-events)
                (run-systems))))

(defn shallow-clj->arr [coll]
  (let [arr (array)]
    (doseq [v coll]
      (.push arr v))
    arr))

(deftype User [firstname lastname]
  Object
  (toString [this]
            (str
             (.-firstname this)
             " "
             (.-lastname this))))

(defn engine [{:keys [event-handlers globals entities systems]}]
  (let [eng (ECSEngine. (or event-handlers {})
                        #js[]
                        (or globals {})
                        (or (shallow-clj->arr entities) #js[])
                        (clj->js (or systems []))
                        0)] eng))

(defn run-engine! [running! eng]
  (let [loop-fn
        (fn loop-fn []
          (when @running!
            (run-engine eng)
            (js/requestAnimationFrame loop-fn)))]
    (loop-fn)))
