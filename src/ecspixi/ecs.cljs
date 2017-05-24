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

(defn filter-entities [required-components entities]
  (filterv
   (fn [e]
     (cs/superset?
      (-> e :components keys set)
      required-components))
   (->> entities (mapv second))))

(defn get-component [entity c-name]
  (-> entity :components c-name :properties))

(defn set-component [entity c-name properties]
  (assoc-in entity
            [:components c-name :properties]
            properties))

(defn update-components [e cs]
  (update e :components assoc-by-id cs))

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

;; Engine
;; ----------------------------------------------------------------

(defrecord Engine [frame globals entities systems])

(defn engine [{:keys [entities systems globals]}]
  (map->Engine
   {:frame 0
    :globals (or globals nil)
    :entities (assoc-by-id {} (or entities []))
    :systems (or (priority-sort systems) [])}))

(defn tick-engine [{:keys [systems] :as engine}]
  (reduce
   (fn [{:keys [entities] :as eng} system]
     (if-not ((:should-run system) eng) eng
             (let [filtered-entities
                   (filter-entities (:required-components system) entities)
                   updated-entities ((:update-fn system) eng filtered-entities)]
               (update eng :entities assoc-by-id updated-entities))))
   (update engine :frame inc)
   systems))

(defn run-engine! [running! engine]
  (let [loop-fn
        (fn loop-fn [engine]
          (when @running!
            (js/requestAnimationFrame #(loop-fn (tick-engine engine)))))]
    (loop-fn engine)))
