(ns ecspixi.ecs)

(defmacro c-swap!
  [entity c-name f & args]
  `(vreset! (get-in ~entity [:components ~c-name :properties])
            (~f (deref (get-in ~entity [:components ~c-name :properties])) ~@args)))
