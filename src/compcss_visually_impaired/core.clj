(ns compcss-visually-impaired.core
  (:require [clojure.string]
            [com.evocomputing.colors]))

(def regex-value-unit #"(\d+\.?\d*)px")

(def regex-color-rgba #"rgb(|a)\((.*?)\)")

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

;; формула для расчета яркости (0.2126*R + 0.7152*G + 0.0722*B)

(defmulti style-obr
  (fn [declaration]
    (cond (#{"font-size" "font"} (:property declaration))
          "font-size"
          (#{"color" "background-color" "background"} (:property declaration))
          "colors")))

(defmethod style-obr
  "font-size"
  [declaration]
  (update declaration :expression
          (fn [value]
            (let [old-val (->> value (re-seq regex-value-unit) first)
                  new-val (some-> old-val second Integer/parseInt (+ 5) str)]
              (or (some-> value (clojure.string/replace (str (second old-val) "px") (str new-val "px"))) value)))))


(defn color-resolve
  [hsl-lightness]
  (cond (> hsl-lightness 70)
        "#ffffff"
        (<= hsl-lightness 70)
        "#000000"))

(defmethod style-obr
  "colors"
  [declaration]
  (update declaration :expression
          (fn [value]
            (let [rgb     (-> (re-seq regex-color-rgba value) first last
                              (->> (format "[%s]") read-string
                                   (map #(if (clojure.string/starts-with? (str %) ".")
                                           (Float/parseFloat (str "0" '.5))
                                           %))
                                   vec))
                  new-color (some->> (try (com.evocomputing.colors/create-color rgb)
                                          (catch Exception _ nil))
                                     :hsl last color-resolve)]
              (or new-color value)))))

(defmethod style-obr
  :default
  [declaration]
  declaration)



(defn style-update
  [data]
  (update data :declarations
          (partial map style-obr)))

(defn --middleware
  [handler]
  (fn [configuration _]
    (map
     (fn [data]
       (if (= (data :type) :style-rule)
         (style-update data)
         data))
     (:compcss.core/output-stylesheets db))))
