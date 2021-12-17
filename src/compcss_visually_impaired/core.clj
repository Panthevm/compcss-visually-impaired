(ns compcss-visually-impaired.core
  (:require
   [clojure.string]
   [com.evocomputing.colors]))


(defmulti declaration :property)
(def regex-value-unit #"(\d+\.?\d*)px")
(def regex-color-rgb  #"rgb(|a)\((.*?)\)")

(defmethod declaration
  "font-size"
  [value]
  (let [old-val (->> value :expression (re-seq regex-value-unit) first)
        new-val (some-> old-val second Integer/parseInt (+ 5) str)]
    (update value :expression
            (fn [expression]
              (or (some->
                   value
                   (clojure.string/replace
                    (str (second old-val) "px")
                    (str new-val "px")))
                  value)))))

(defmethod declaration
  "background"
  [value]
  (let [rgb-match (re-find regex-color-rgb (:expression value))]
    (cond
      (first rgb-match)
      (update value :expression
              (fn [expression]
                (clojure.string/replace
                 expression
                 (first rgb-match)
                 "white")))
      :else value)))

(defmethod declaration
  "background-color"
  [value]
  (assoc value :expression "white"))


(defmethod declaration :default
  [value]
  value)

(defn declarations-update
  [declarations]
  (let [declarations (map declaration declarations)]
    (cond-> declarations
      (some (comp
             (partial contains? #{"background" "background-color"})
             :property)
            declarations)
      (conj {:property   "border"
             :important? false
             :expression "2px solid black"
             :type       :declaration}))))

(defn stylesheets-update
  [stylesheets]
  (map
   (fn [stylesheet]
     (case (:type stylesheet)
       :style-rule
       (update stylesheet :declarations declarations-update)
       :media-rule
       (update stylesheet :rules stylesheets-update)
       stylesheet))
   stylesheets))


(defn middleware
  [handler]
  (fn [configuration db]
    (->>
     (update db :compcss.core/output-stylesheets stylesheets-update)
     (handler configuration))))
