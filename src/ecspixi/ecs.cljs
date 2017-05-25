(ns ecspixi.ecs
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

(defn get-component [entity c-name]
  (get-in entity [:components c-name :properties]))

(defn update-component [entity c-name & new]
  (reduce
   (fn [e [k v]]
     (assoc-in e [:components c-name :properties k] v))
   entity
   (partition 2 new)))

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
   (swap! (:event-bus eng) conj [event-name data])))

;; Engine
;; ----------------------------------------------------------------

(defrecord Engine [frame globals event-bus event-handlers entities systems])

(defn engine [{:keys [entities event-handlers systems globals]}]
  (map->Engine
   {:frame 0
    :event-bus (atom [])
    :event-handlers (or event-handlers {})
    :globals (or globals nil)
    :entities (assoc-by-id {} (or entities []))
    :systems (or (priority-sort systems) [])}))

(defn frame-inc [engine]
  (update engine :frame inc))

(defn run-events [engine]
  (let [current-events @(:event-bus engine)]
    (reset! (:event-bus engine) [])
    (reduce
     (fn [eng [event-type data]]
       (if-let [handler (get-in eng [:event-handlers event-type])]
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
    (let [xs (comp (map second)
                   (filter
                     (fn [e]
                       (cs/superset?
                         (-> e :components keys set)
                         (:required-components system)))))]
      ((:update-fn system) eng (into [] xs entities))))
  eng)


(defn tick-engine [engine]
  (-> engine frame-inc run-events run-systems))

(defn run-engine! [running! engine]
  (let [loop-fn
        (fn loop-fn [engine]
          (when @running!
            (js/requestAnimationFrame #(loop-fn (tick-engine engine)))))]
    (loop-fn engine)))
