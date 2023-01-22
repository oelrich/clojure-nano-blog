(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [hiccup2.core :as hc]))

(def edn-readers
  {:readers {'site-title (fn [_] "My FANTASTIC blog!")
             'site-content (fn [content-type]
                             (cond (= content-type "main") ':main-content))
             'link (fn [target] (cond (= target "stylesheet") "./style.css"))}})

(defn load-edn [source]
  (when-not (.isDirectory (io/file source))
    (try
      (with-open [r (io/reader source)]
        {:name (str/replace (.getName source) ".edn" "")
         :content (edn/read edn-readers (java.io.PushbackReader. r))})
      (catch java.io.IOException e
        (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
      (catch RuntimeException e
        (printf "Error parsing edn file '%s': %s\n" source (.getMessage e))))))

(defn write-html [target-dir input]
  (let [out-file (str target-dir
                      (when-not (empty? target-dir) "/")
                      (:name input)
                      ".html")]
    (print (str "writing to: " out-file "\n"))
    (spit out-file (hc/html (:content input)))))

(defn find-in [hiccup-data path default]
  (cond (empty? hiccup-data) default
        (empty? path) hiccup-data
        (= (first hiccup-data) (first path)) (find-in (second hiccup-data) (rest path) default)
        :else (find-in (rest hiccup-data) path default)))

(defn replace-in [hiccup-data target-key target-value]
  (cond (empty? hiccup-data) []
        (= (first hiccup-data) target-key)
        (into [target-value]
              (replace-in (rest hiccup-data)
                          target-key target-value))
        (vector? (first hiccup-data))
        (into [(replace-in (first hiccup-data)
                           target-key target-value)]
              (replace-in (rest hiccup-data)
                          target-key target-value))
        :else
        (into [(first hiccup-data)]
              (replace-in (rest hiccup-data) target-key target-value))))

(defn get-dl [doc]
  (let [name (:name doc)
        title (find-in (:content doc) [:article :h1] "Unnamed document")]
    [:a {:href (str name ".html")} [:dt name] [:dd title]]))

(defn add-index [inputs]
  (conj inputs
        {:name "index"
         :content (into [] (concat [:dl] (map get-dl inputs)))}))

(defn write-all [target-dir inputs]
  (dorun
   (map (partial write-html target-dir) inputs)))

(defn apply-template [template document]
  (let [document-content (:content document)
        template-document (replace-in template :main-content document-content)]
    (pp/pprint document)
    #_(pp/pprint template-document)
    (assoc document :content template-document)))

(defn blog [{:keys [source-dir
                    target-dir
                    template-dir]
             :or {source-dir "entries"
                  target-dir ""
                  template-dir "templates"}}]
  (let [page-template (load-edn (io/file (str template-dir "/" "page.edn")))]
    (->> (file-seq (io/file (str source-dir)))
         (map load-edn)
         (filter some?)
         (add-index)
         (map (partial apply-template (:content page-template)))
         (write-all (str target-dir)))))
