(ns ecspixi.ecs)

(def MAX_PRIORITY (.-Infinity js/window))

(defn assoc-by-name
  ([items] (assoc-by-name {} items))
  ([item-map items]
   (reduce #(assoc % (:name %2) %2) item-map items)))

;; [E]ntity

(defn has-component? [c-name]
  (fn [entity]
    (-> entity
       :components
       (get c-name false))))

(defn getc [entity c-name]
  (-> entity :components c-name :properties))

(defn setc [entity c-name properties]
  (assoc-in entity
             [:components c-name :properties]
             properties))

(defn update-components [e cs]
  (update e :components assoc-by-name cs))

(defn e [components]
  {:name (random-uuid)
   :components (assoc-by-name (or components []))})

;; [C]omponent

(defn c [{:keys [name properties]}]
  {:id (random-uuid)
   :name name
   :properties (or properties {})})

;; [S]ystem

(defn s [{:keys [priority update-fn should-run entity-filters]}]
  {:id (random-uuid)
   :priority (or priority MAX_PRIORITY)
   :update-fn update-fn
   :should-run (or should-run (constantly true))
   :entity-filters (or entity-filters [(constantly true)])})

(defn priority-sort [systems]
  (sort-by #(compare (:priority %2) (:priority %1))
           systems))

(defn add-system [engine system]
  (update engine :systems
          #(-> % (conj system) priority-sort)))

;; Engine

(defn create-engine [{:keys [entities systems globals]}]
  {:frame 0
   :globals (or globals nil)
   :entities (assoc-by-name (or entities []))
   :systems (or (priority-sort systems) [])})

(defn tick-engine [{:keys [systems] :as engine}]
  (reduce
    (fn [{:keys [entities] :as eng} system]
      (if-not ((:should-run system) eng)
        eng
        (let [filtered-entities (->> entities
                                     vals
                                     (filter
                                      (fn [e]
                                        ((apply every-pred (:entity-filters system)) e)))
                                     vec)
              updated-entities ((:update-fn system) eng filtered-entities)]
          (update eng :entities assoc-by-name updated-entities))))
    (update engine :frame inc)
    systems))
