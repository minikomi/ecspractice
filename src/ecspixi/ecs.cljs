(ns ecspixi.ecs
  (:require-macros [ecspixi.ecs :refer [c-swap!]])
  (:require [clojure.set :as cs]))

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

(defrecord Entity [id components])

(defn e [components]
  (map->Entity
   {:id (random-uuid)
    :components (assoc-by-id {} (or components []))}))

;; [C]omponent
;; ----------------------------------------------------------------

(defrecord Component [id properties])

(defn c [{:keys [id properties]}]
  (map->Component
   {:id id
    :properties (volatile! (or properties {}))}))

;; [S]ystem
;; ----------------------------------------------------------------

(defrecord System [id priority update-fn should-run required-components])

(defn s [{:keys [id priority update-fn should-run required-components]}]
  (map->System
   {:id id
    :priority (or priority MAX_PRIORITY)
    :update-fn update-fn
    :should-run (or should-run (constantly true))
    :required-components (or required-components (constantly true))}))

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

(defrecord Engine [frame globals event-bus event-handlers entities systems])

(defn engine [{:keys [entities event-handlers systems globals]}]
  (map->Engine
   {:frame (volatile! 0)
    :event-bus (volatile! [])
    :event-handlers (or event-handlers {})
    :globals (or globals nil)
    :entities (assoc-by-id {} (or entities []))
    :systems (or (priority-sort systems) [])}))

(defn frame-inc [engine]
  (vswap! (:frame engine) inc)
  engine)

(defn run-events [engine]
  (let [current-events @(:event-bus engine)]
    (vreset! (:event-bus engine) [])
    (reduce
     (fn [{:keys [event-handlers] :as eng} [event-type data]]
       (if-let [handler (get event-handlers event-type nil)]
         (handler eng data)
         eng))
     engine
     current-events)))

(defn filter-entities [required-components entities]
  (filterv
   (fn [e]
     (cs/superset?
      (-> e :components keys set)
      required-components))
   (->> entities (mapv second))))

(defn run-systems [{:keys [entities systems] :as eng}]
  (doseq [system systems]
    (->> entities
         (into []
               (comp (map second)
                     (filter
                      (fn [e]
                        (cs/superset?
                         (-> e :components keys set)
                         (:required-components system))))))
         ((:update-fn system) eng)))
  eng)

(defn tick-engine [engine]
  (-> engine frame-inc run-events run-systems))

(defn run-engine! [running! engine]
  (let [loop-fn
        (fn loop-fn [eng]
          (when @running!
            (js/requestAnimationFrame #(loop-fn (tick-engine eng)))))]
    (loop-fn engine)))
