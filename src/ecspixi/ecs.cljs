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
        mv (vec m)]
    (dotimes [n (count mv)]
      (aset obj (name (nth (nth mv n) 0)) (nth (nth mv n) 1)))
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
    (get-component this (name k) nil))
  (-invoke [this k not-found]
    (get-component this (name k) not-found))
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
               (let [cs (.-component-set e)
                     rc-c (.-length rc-arr)]
                 (loop [n 0]
                   (if (<= rc-c n)
                     true
                     (if (o/get cs (aget rc-arr n) false)
                       (recur (inc n))
                       false))))))))


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

  (let [sys-n (count (.-systems engine))
        ents (.-entities engine)
        ent-n (count ents)]
   (loop [sys-n sys-n]
     (when (< 0 sys-n)
       (let [sys (aget (.-systems engine) (dec sys-n))
             sys-fil (.-entity-filter sys)
             sys-fn (.-update-fn sys)
             ok-ents #js[]]
        (loop [ent-n ent-n]
          (when (< 0 ent-n)
            (when (sys-fil (aget ents (dec ent-n)))
              (.push ok-ents (aget ents (dec ent-n))))
            (recur (dec ent-n))))
        (sys-fn engine ents)
        (recur (dec sys-n))))))
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
