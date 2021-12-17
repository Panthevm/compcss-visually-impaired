(ns compcss-visually-impaired.core
  (:require
   [clojure.string]
   [com.evocomputing.colors]))


(defmulti declaration :property)
(def regex-value-unit #"(\d+\.?\d*)px")
(def regex-color-rgb  #"rgb(|a)\((.*?)\)")

(def db
  {:compcss.core/output-stylesheets
   [{:selectors
     [{:members
       [{:value "foo" :group :type :type :member-simple}
        {:name  " " :type :selector-combinator}
        {:value "bar" :group :type :type :member-simple}]
       :type :selector}]
     :declarations
     [{:property   "font"
       :expression "16px 16 fam"
       :important? false
       :type       :declaration}
      {:property "color"
       :expression "rgba(160, 160, 160, .5)"
       :important? false
       :type       :declaration}]
     :type :style-rule}]})


(defmethod declaration
  "font-size"
  [value]
  (let [old-val (->> value :expression (re-seq regex-value-unit) first)
        new-val (some-> old-val second Integer/parseInt (+ 7) str)]
    (update value :expression
            (fn [expression]
              (str 
               (or (some->
                    expression
                    (clojure.string/replace
                     (str (second old-val) "px")
                     (str new-val "px")))
                   expression)
               "!important")))))

(defn color-resolve
  [hsl-lightness]
  (cond (> hsl-lightness 80)
        "#ffffff"
        (<= hsl-lightness 70)
        "#000000"))

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
                 "white !important")))
      :else value)))

(defmethod declaration
  "color"
  [value]
  (assoc value :expression "black"))

(defmethod declaration
  "background-color"
  [value]
  (assoc value :expression "white"))


(defmethod declaration :default
  [value]
  value)

(defn declarations-update
  [declarations selectors]
  (let [declarations (map declaration declarations)]
    (cond-> declarations
      (and
       (some (comp
              (partial contains? #{"background" "background-color"})
              :property)
             declarations)
       )
      (conj {:property   "border"
             :important? true
             :expression "2px solid black"
             :type       :declaration})
      (and (some (fn [{members :members}]
                   (some #(and (-> % :group #{:pseudo})
                               (-> % :value (= ":hover"))) members))
                 selectors)
           (some (comp (partial contains? #{"color" "background-color" "background"}) :property)
                 declarations))
      (-> (->> (remove (comp #{"color" "background-color" "background"} :property)))
          (conj {:property   "color"
                 :important? true
                 :expression "white"
                 :type       :declaration}
                {:property   "background-color"
                 :important? true
                 :expression "black"
                 :type       :declaration})))))

(defn stylesheets-update
  [stylesheets]
  (map
   (fn [stylesheet]
     (case (:type stylesheet)
       :style-rule
       (update stylesheet :declarations declarations-update (:selectors stylesheet))
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
