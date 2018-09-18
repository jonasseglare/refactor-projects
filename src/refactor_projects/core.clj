(ns refactor-projects.core
  (:import [java.io File])
  (:require [clojure.java.io :as io]
            [clojure.string :as cljstr]))

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

(defn analyze-clojure-file [file]
  (let [data (slurp file)
        tokens (tokenize data)]
    (merge {:data data
            :file file}
           (parse-tokens tokens))))

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
    (println "  from-ns=" (:from-ns settings))
    (println "  parts=" parts)
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
        new-data (compute-new-data (:data file) src-settings)
        new-file (or (compute-new-file file test-settings)
                     (compute-new-file file src-settings))]
    (when (not= new-data (:data file))
      (println "NEW DATA!!!")
      #_(println "'" new-data "'"))
    (println "new-file=" new-file)))

(defn rename-namespace-for-file [file rename-settings]
  (and (= (:type file) :src-file)
       (perform-renaming file rename-settings)))

(defn append-test [s]
  (str s "-test"))

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
         (filter clojure-file?)
         (map analyze-clojure-file))
   conj
   []
   directories-to-scan))

(defn rename-namespace [clojure-files from-ns to-ns]
  (let [basic-rename-settings {:from-ns from-ns
                               :to-ns to-ns}
        test-rename-settings (rename-settings-with-test basic-rename-settings)
        full-rename-settings {:src (rename-settings-to-parts basic-rename-settings)
                              :test (rename-settings-to-parts test-rename-settings)}]
    (doseq [file clojure-files]
      (println "\n* Rename namespace for" (-> file :file .getAbsolutePath))
      (rename-namespace-for-file file full-rename-settings))))
