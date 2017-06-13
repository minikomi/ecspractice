(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :target-path "./target/"
 :dependencies '[;; pin deps
                 [org.clojure/clojure "1.9.0-alpha17" :scope "provided"]
                 [org.clojure/clojurescript "1.9.562"]
                 ;; util
                 [boot-deps "0.1.6"]
                 ;; cljs
                 [adzerk/boot-cljs "2.0.0" :scope "test"]
                 ;; dev
                 [adzerk/boot-cljs-repl "0.3.3"]
                 [adzerk/boot-reload "0.5.1" :scope "test"]
                 [com.cemerick/piggieback "0.2.1" :scope "test"]
                 [org.clojure/tools.nrepl "0.2.13" :scope "test"]
                 [pandeiro/boot-http "0.8.3" :scope "test"]
                 [powerlaces/boot-cljs-devtools "0.2.0" :scope "test"]
                 [ring-logger "0.7.7"]
                 [ring/ring-defaults "0.3.0"]
                 [weasel "0.7.0" :scope "test"]
                 ;; Frontend
                 [reagent "0.6.2"]
                 [binaryage/oops "0.5.5"]
                 [cljsjs/pixi "4.4.3-0"]])


(require
 'boot.repl
 '[adzerk.boot-cljs      :refer [cljs]]
 '[adzerk.boot-reload    :refer [reload]]
 '[pandeiro.boot-http    :refer [serve]]
 '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]])


(deftask build []
  (comp
   (cljs)))

(deftask cider "CIDER profile" []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.15.0-SNAPSHOT"]
                  [refactor-nrepl "2.3.0-SNAPSHOT"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  identity)

(deftask development []
  (task-options! cljs {:optimizations :none
                       :compiler-options {:closure-defines {'goog.DEBUG true}}})
  identity)

(deftask production []
  (task-options! cljs {:optimizations :advanced
                       :source-map false
                       :compiler-options {:elide-asserts true
                                          :pretty-print false
                                          :closure-defines {'goog.DEBUG false}}})
  identity)

(deftask optimized []
  (task-options! cljs {:optimizations :advanced
                       :source-map false
                       :compiler-options {:elide-asserts true
                                          :pretty-print true
                                          :pseudo-names true
                                          :closure-defines {'goog.DEBUG false}}})
  identity)

(deftask dev []
  (comp
   (development)
   (cider)
   (serve)
   (watch)
   (cljs-repl)
   (reload :on-jsload 'ecspixi.core/init
           :ws-host "0.0.0.0"
           :asset-path "/public")
   (build)))

(deftask pseudo []
  (comp
   (optimized)
   (cider)
   (serve)
   (watch)
   (reload :on-jsload 'ecspixi.core/init
           :ws-host "0.0.0.0"
           :asset-path "/public")
   (build)
   (sift :include #{#"\.out" #"\.cljs\.edn$" #"^\." #"/\."} :invert true)
   (target)))


(deftask prod []
  (comp
   (production)
   (build)
   (sift :include #{#"\.out" #"\.cljs\.edn$" #"^\." #"/\."} :invert true)
   (target)))
