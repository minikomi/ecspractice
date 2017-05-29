(ns ecspixi.ecs
  (:require-macros [ecspixi.ecs :refer [c-swap!]])
  (:require [clojure.set :as cs]
            [goog.object :as o]))

(def MAX_PRIORITY (.-Infinity js/window))

(defn assoc-by-id
  ([items] (assoc-by-id {} items))
  ([item-map items]
   (reduce #(assoc % (.-id %2) %2) item-map items)))

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
  (get-component-set [_])
  (get-component [_ k]))

(deftype Entity [id components component-set]
  IECSEntity
  (get-component-set [_]
    component-set)
  (get-component [_ k]
    (o/get components (name k))))

(defn e [components]
  (Entity.
   (random-uuid)
   (clj->js (assoc-by-id {} (or components [])))
   (->> components
        (map #(vector (.-id %) true))
        (into {}) clj->js)))

;; [C]omponent
;; ----------------------------------------------------------------

(deftype Component [id ^:mutable properties]
  IVolatile
  (-vreset! [_ new-properties]
    (set! properties new-properties))
  IDeref
  (-deref [_] properties))

(defn c [{:keys [id properties]}]
  (Component. id (or properties {})))

;; [S]ystem
;; ----------------------------------------------------------------

(defprotocol IECSSystem
  (get-should-run [_])
  (get-update-fn [_])
  (get-required-components [_])
  (get-priority [this]))

(deftype System [id priority update-fn should-run required-components]
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
  (System. id
           (or priority MAX_PRIORITY)
           update-fn
           (or should-run (constantly true))
           (->>
            (or required-components [])
            clj->js)))

(defn priority-sort [systems]
  (sort-by get-priority systems))

;; event bus
;; ----------------------------------------------------------------

(defn event!
  ([eng event-name]
   (event! eng event-name nil))
  ([eng event-name data]
   (vswap! (:event-bus eng) conj [event-name data])))

;; Engine
;; ----------------------------------------------------------------

(defprotocol IEngine
  (get-systems [o])
  (get-globals [o])
  (get-entities [o])
  (run-engine [o]))

(defn frame-inc [engine]
  (set! (.-frame engine) (+ 1 (.-frame engine))))

(defn run-events [engine]
  (let [current-events (o/get engine "event-bus")]
    ;;; TODO EVENT HANDLING
    (o/set engine "event-bus" #js[])))

(defn run-systems [engine]
  (.forEach (get-systems engine)
            (fn [system]
              ((get-update-fn system) engine
               (.filter
                (get-entities engine)
                (fn [e]
                  (let [ecs (get-component-set e)]
                    (.every
                     (get-required-components system)
                     #(o/get ecs %))))))))
  nil)


(deftype ECSEngine [event-handlers
                    ^:mutable globals
                    ^:mutable entities
                    ^:mutable systems
                    ^:mutable frame]
  Object
  (toString [this]
    (str
     (.-globals this)))
  IEngine
  (get-systems [this]
    systems)
  (get-entities [this]
    entities)
  (get-globals [this]
    globals)
  (run-engine [this]
    (doto this
      (frame-inc)
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
  (let [eng (ECSEngine. (clj->js (or event-handlers {}))
                        (or globals {})
                        (or (shallow-clj->arr entities) #js[])
                        (clj->js (or systems []))
                        0)]


    eng))

(defn run-engine! [running! eng]
  (let [loop-fn
        (fn loop-fn []
          (when @running!
            (run-engine eng)
            (js/requestAnimationFrame loop-fn)))]
    (loop-fn)))
