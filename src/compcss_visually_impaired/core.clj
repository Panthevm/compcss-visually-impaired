(ns compcss-visually-impaired.core
  (:require [clojure.string]
            [com.evocomputing.colors]))

(def regex-value-unit #"(\d+\.?\d*)px")

(def regex-color-rgba #"rgb(|a)\((.*?)\)")

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


(defn middleware
  [handler]
  (fn [configuration db]
    (handler
     configuration
     (update db :compcss.core/output-stylesheets
             (fn [stylesheets]
               (map
                (fn [data]
                  (if (= (data :type) :style-rule)
                    (style-update data)
                    data))
                stylesheets))))))
