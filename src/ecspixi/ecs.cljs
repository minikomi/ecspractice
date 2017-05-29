(ns ecspixi.ecs
  (:require-macros [ecspixi.ecs :refer [c-swap!]])
  (:require [clojure.set :as cs]
            [goog.object :as o]))

(def MAX_PRIORITY (.-Infinity js/window))

(defn assoc-by-id
  ([items] (assoc-by-id {} items))
  ([item-map items]
   (reduce #(assoc % (:id %2) %2) item-map items)))

;; [E]ntity
;; ----------------------------------------------------------------

(defn has-component? [c-name]
  (fn component-find [entity]
    (-> entity
        :components
        (get c-name false))))

(defn e->c [entity c-name]
  (get-in entity [:components c-name :properties]))

(defrecord Entity [id components component-set])

(defn e [components]
  (map->Entity
   {:id (random-uuid)
    :components (assoc-by-id {} (or components []))
    :component-set (->> components (map :id) sort vec clj->js)}))

;; [C]omponent
;; ----------------------------------------------------------------

(defrecord Component [id properties])

(defn c [{:keys [id properties]}]
  (map->Component
   {:id id
    :properties (or properties {})}))

;; [S]ystem
;; ----------------------------------------------------------------

(defrecord System [id priority update-fn should-run required-components])

(defn s [{:keys [id priority update-fn should-run required-components]}]
  (map->System
   {:id id
    :priority (or priority MAX_PRIORITY)
    :update-fn update-fn
    :should-run (or should-run (constantly true))
    :required-components (->> (or required-components [])
                              sort
                              clj->js)}))

(defn priority-sort [systems]
  (sort-by #(compare (:priority %2) (:priority %1))
           systems))

(defn add-system [engine system]
  (update engine :systems
          #(-> % (conj system) priority-sort)))

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
  (init [o])
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
              ((o/get system "update-fn") engine
               (.filter
                (get-entities engine)
                (fn [e]
                  (let [ecs (-> e :component-set)]
                    (when (.every
                           (o/get system "required-components")
                           #(<= 0 (.indexOf ecs %)))
                      e))))))))


(deftype ECSEngine [event-handlers globals entities systems]
  Object
  (toString [this]
    (str
     (.-globals this)))
  IEngine
  (init [this]
    (set! (.-frame this) 0)
    (set! (.-event-bus #js[])))
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
                        (clj->js (or systems [])))]
    (init eng)
    eng))

(defn run-engine! [running! eng]
  (let [loop-fn
        (fn loop-fn []
          (when @running!
            (run-engine eng)
            (js/requestAnimationFrame loop-fn)))]
       (loop-fn)))
