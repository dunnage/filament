(ns build
  (:refer-clojure :exclude [test compile])
  (:require [clojure.tools.build.api :as b] ; for b/git-count-revs
            [org.corfield.build :as bb]))

(def lib 'net.clojars.dunnage/filament)
#_ (def version "0.1.0-SNAPSHOT")
; alternatively, use MAJOR.MINOR.COMMITS:
(def version (format "1.0.%s" (b/git-count-revs nil)))

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"
                            :aliases [:build-config]
                            }))


(defn clean [_]
      (b/delete {:path "target"}))

(defn compile [_]
      (b/javac {:src-dirs   ["src-java"]
                :class-dir  class-dir
                :basis      basis
                :javac-opts ["-source" "11" "-target" "11"]}))
