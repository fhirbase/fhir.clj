(ns fhir.todo)

(def header
  {:js {:ng "http://cdn" :jq "http://cdn"}
   :css {:main "http://" :butstrap "http" }
   :encoding "utf8"
   :title "Live without XML"})

(def body
  {:header {:new-item {:placeholder "print the title"}
            :create {:text "Create"}}
   :items []})

(def btn {:border {:width 1 :color "gray"}})

(def style
  {:header {:new-item {:editable true}
            :create 'btn}})

(defn click [& args])

(click
  [:header :search :create]
  (fn [doc ev]
    (if-let [txt (get-in doc [:header :new-item :text])]
      (-> do
          (dissoc :alert)
          (update-in [:items] #(conj % {:title txt})))
      (assoc doc :alert {:text "Please fill the field"}))))

(fn update-items [items]
  (assoc doc :items
         (map (fn [x] (select-keys :title :desc :done)) items)))

(fn run [doc]
  (xhr "url"
       (fn [items]
         (update-items items))))

