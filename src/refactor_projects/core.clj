(ns refactor-projects.core
  (:import [java.io File])
  (:require [clojure.java.io :as io]
            [clojure.string :as cljstr]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as spec]))

(spec/def ::paths (spec/coll-of string?))

(spec/def ::rename (spec/cat :prefix #{:rename}
                             :from-ns string?
                             :to-ns string?))

(spec/def ::op (spec/alt :rename ::rename))
(spec/def ::ops (spec/coll-of ::op))
(spec/def ::mode #{:preview :execute})

(spec/def ::refactor-spec (spec/keys :req-un [::paths
                                              ::ops
                                              ::mode]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(spec/def ::clj-file (spec/alt :src-file (spec/cat :ns (partial = 'ns)
                                                   :namespace symbol?
                                                   :rest (spec/* any?))
                               :leiningen-project (spec/cat :dp (partial = 'defproject)
                                                            :name symbol?
                                                            :rest (spec/* any?))
                               :unknown (spec/* any?)))

(defn parts-to-filename-suffix [parts]
  (str (cljstr/join
        "/"
        (map
         (fn [part]
           (cljstr/replace part #"-" "_"))
         parts)) ".clj"))

(defn find-subvec [a b]
  (last
   (filter
    (fn [x]
      (= (:data x) a))
    (map
     (fn [data i]
       {:index i
        :data (take (count a) data)})
     (take-while (complement empty?)
                 (iterate rest b))
     (range)))))

(defn replace-subvec [dst f new-vec]
  (let [dst (vec dst)
        pre (subvec dst 0 (:index f))
        suf (subvec dst (+ (:index f) (count (:data f))))]
    (reduce into [] [pre new-vec suf])))

(defn file? [x]
  (instance? File x))

(defn clojure-file? [file]
  {:pre [(file? file)]}
  (.endsWith (cljstr/lower-case (str file)) ".clj"))

(defn file-contains-subpath? [file p]
  {:pre [(or (instance? File file)
             (nil? file))]}
  (cond
    (nil? file) false
    (= p (.getName file)) true
    :default (file-contains-subpath? (.getParentFile file) p)))

(defn tokenize [s]
  {:pre [(string? s)]}
  (transduce
   (comp (map (fn [s] (cljstr/split s #" ")))
         cat
         (filter (complement empty?))
         (map cljstr/trim))
   conj
   []
   (cljstr/split-lines s)))

(defn namespace-to-parts [namespace-str]
  {:pre [(string? namespace-str)]}
  (cljstr/split namespace-str #"\."))

(defn parts-to-ns [parts]
  (cljstr/join "." parts))

(defn parse-src-file-tokens [tokens]
  {:type :src-file
   :namespace (second tokens)})

(defn parse-project-tokens [tokens]
  {:type :leiningen-project
   :name (second tokens)})

(defn parse-tokens [tokens]
  ((case (first tokens)
      "(ns" parse-src-file-tokens
      "(defproject" parse-project-tokens
      (constantly {}))  tokens))

(defn parse-edn [edn-data]
  (let [parsed (spec/conform ::clj-file edn-data)]
    (if (= parsed ::spec/invalid)
      {}
      (merge {:type (first parsed)}
             (into {}
                   (map (fn [[k v]]
                          [k (str v)])
                        (select-keys
                         (second parsed) [(case (first parsed)
                                            :src-file :namespace
                                            :leiningen-project :name)])))))))

(defn analyze-clojure-file [file]
  (try
    (let [data (slurp file)
          edn-data (read-string data)]
      (merge {:data data
              :edn-data edn-data
              :file file}
             (parse-edn edn-data)))
    (catch Exception e
      (println "  ERROR: Failed to analyze" (.getAbsolutePath file))
      {})))

(defn append-test [s]
  (str s "-test"))



(defn rename-settings-with-test [rename-settings]
  (-> rename-settings
      (update :from-ns append-test)
      (update :to-ns append-test)))

(defn rename-settings-to-parts [rename-settings]
  (-> rename-settings
      (update :from-ns namespace-to-parts)
      (update :to-ns namespace-to-parts)))

(defn compute-new-data [src-data src-settings]
  (cljstr/replace src-data
                  (parts-to-ns (:from-ns src-settings))
                  (parts-to-ns (:to-ns src-settings))))

(defn compute-new-file [file settings]
  (let [parts (namespace-to-parts (:namespace file))
        lc-file (-> file
                    :file
                    .getAbsolutePath)]
    (when-let [f (find-subvec (:from-ns settings) parts)]
      (let [expected-suffix (parts-to-filename-suffix parts)]
        (if (cljstr/ends-with? lc-file expected-suffix)
          (let [new-parts (replace-subvec parts f (:to-ns settings))
                filename-prefix (subs lc-file 0 (- (count lc-file) (count expected-suffix)))]
            (io/file filename-prefix (parts-to-filename-suffix new-parts)))
          (println "--------> WARNING: The current filename '"
                   lc-file "' is not consistent with namespace '" (:from-ns settings) "'"))))))

(defn perform-renaming [file rename-settings]
  (let [test-settings (:test rename-settings)
        src-settings (:src rename-settings)
        old-data (:data file)
        old-file (:file file)
        new-data (compute-new-data old-data src-settings)
        new-file (or (compute-new-file file test-settings)
                     (compute-new-file file src-settings))
        updated-data? (not= new-data old-data)
        updated-file? (boolean new-file)
        updated? (or updated-data? updated-file?)]
    (when updated?
      (println (str "\n -> Update '" (.getAbsolutePath old-file) "'"))
      (when updated-data?
        (println "     - new data"))
      (when updated-file?
        (println "     - new name: " (.getAbsolutePath new-file))))
    (case (:mode rename-settings)
      :preview nil
      :execute (when updated?
                 (println "  -- Execute! --")
                 (when new-file
                   (io/make-parents new-file))
                 (cond
                   (and updated-data? updated-file?) (do (io/delete-file old-file)
                                                         (spit new-file new-data))
                   updated-data? (spit old-file new-data)
                   updated-file? (.renameTo old-file new-file))))))

(defn rename-namespace-for-file [file rename-settings]
  (and (= (:type file) :src-file)
       (perform-renaming file rename-settings)))

(defn empty-directories [paths]
  (transduce
   (comp  (map (comp file-seq io/file))
          cat
          (filter #(and (not (file-contains-subpath? % "checkouts"))
                        (not (file-contains-subpath? % ".git"))
                        (.isDirectory %)
                        (< 3 (count (file-seq %))))))
   conj
   []
   paths))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clojure-files [directories-to-scan]
  {:pre [(coll? directories-to-scan)
         (every? string? directories-to-scan)]}
  (transduce
   (comp (map (comp file-seq io/file))
         cat
         (filter #(and (clojure-file? %)
                       (not (file-contains-subpath? % "checkouts"))))
         (map analyze-clojure-file))
   conj
   []
   directories-to-scan))

(defn rename-namespace [refactor-spec clojure-files from-ns to-ns]
  (let [basic-rename-settings {:from-ns from-ns
                               :to-ns to-ns}
        test-rename-settings (rename-settings-with-test basic-rename-settings)
        full-rename-settings (merge refactor-spec
                                    {:src (rename-settings-to-parts basic-rename-settings)
                                     :test (rename-settings-to-parts test-rename-settings)})]
    (doseq [file clojure-files]
      (rename-namespace-for-file file full-rename-settings))))

(defn refactor [refactor-spec]
  {:pre [(spec/valid? ::refactor-spec refactor-spec)]}
  (doseq [op (:ops refactor-spec)]
    (println "-------> Performing op" op)
    (let [files (clojure-files (:paths refactor-spec))]
      (println "  " (count files) " clojure files")
      (let [[op-type op-data] (spec/conform ::op op)]
        (case op-type
          :rename (rename-namespace refactor-spec
                                    files
                                    (:from-ns op-data)
                                    (:to-ns op-data)))))))



#_ (refactor {:paths ["/home/jonas/prog/clojure"]
           :ops [[:rename "bluebell.utils.wip" "bluebell.utils.wip.wip"]]
              :mode :preview})

#_(refactor {:paths ["/home/jonas/prog/clojure"]
           :ops [[:rename "bluebell.utils.wip.specs" "bluebell.utils.wip.specs"]]
             :mode :execute})

#_(refactor {:paths ["/home/jonas/prog/clojure/geex"]
           :ops [[:rename "geex.jcore" "geex.core"]]
           :mode :preview})

#_(refactor {:paths ["/home/jonas/prog/clojure"]
           :ops [[:rename "bluebell.utils.dsl" "bluebell.utils.dsl"]
                 [:rename "bluebell.utils.render-text" "bluebell.utils.render-text"]
                 [:rename "bluebell.utils.ebmd" "bluebell.utils.ebmd"]]
           :mode :execute})

#_(refactor {:paths ["/home/jonas/prog/clojure/test-project"]
           :ops [[:rename "test-project" "kattskit.wip"]]
             :mode :preview})

#_(refactor {:paths ["/home/jonas/prog/clojure/geex"
                   "/home/jonas/prog/clojure/geex-benchmarks/clojure-benchmarks"]
             :ops [[:rename "geex.lib" "geex.base"]]
             :mode :preview})

#_(refactor {:paths ["/home/jonas/prog/clojure/pres-clojured"]
             :ops [[:rename "exsampler.sample" "exampler.sample"]]
             :mode :preview})

#_(refactor {:paths ["/home/jonas/prog/clojure/geex"
                   "/home/jonas/prog/clojure/geex-benchmarks/clojure-benchmarks"
                   "/home/jonas/prog/clojure/geex-benchmarks/pres-clojured"]
             :ops [[:rename "geex.base" "geex.common"]]
             :mode :execute})
