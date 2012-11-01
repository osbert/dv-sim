(ns ^{:doc "Render the views for the application."}
  one.sample.view
  (:use [domina :only [set-html! set-styles! styles by-id set-style!
                       by-class value set-value! set-text! nodes single-node set-attr!]]
        [domina.xpath :only [xpath]]
        [one.browser.animation :only [play]]
        [one.logging :only [info, get-logger]]
        [goog.dom.selection :only [getStart setStart setEnd getEnd]])
  (:require-macros [one.sample.snippets :as snippets])
  (:require [goog.events.KeyCodes :as key-codes]
            [goog.events.KeyHandler :as key-handler]
            [clojure.browser.event :as event]
            [one.dispatch :as dispatch]
            [one.sample.animation :as fx]
            [one.sample.dvorak :as dvorak]))

(def ^{:doc "A map which contains chunks of HTML which may be used
  when rendering views."}
  snippets (snippets/snippets))

(defmulti render-button
  "Render the submit button based on the current state of the
  form. The button is disabled while the user is editing the form and
  becomes enabled when the form is complete."
  identity)

(defmethod render-button :default [_])

(defmulti render-form-field
  "Render a form field based on the current state transition. Form
  fields are validated as soon as they lose focus. There are six
  transitions and each one has its own animation."
  :transition)

(defmethod render-form-field :default [_])

(defn- label-xpath
  "Accepts an element id for an input field and return the xpath
  string to the label for that field."
  [id]
  (str "//label[@id='" id "-label']/span"))

(defmethod render-form-field [:empty :editing] [{:keys [id]}]
  (fx/label-move-up (label-xpath id)))

(defmethod render-form-field [:editing :empty] [{:keys [id]}]
  (fx/label-move-down (label-xpath id)))

(defmethod render-form-field [:valid :editing-valid] [{:keys [id]}]
  (play (label-xpath id) fx/fade-in))

(defmethod render-form-field [:editing :error] [{:keys [id error]}]
  (let [error-element (by-id (str id "-error"))]
    (set-style! error-element "opacity" "0")
    (set-html! error-element error)
    (play error-element fx/fade-in)))

(defn- swap-error-messages
  "Accepts an id and an error message and fades the old error message
  out and the new one in."
  [id error]
  (let [error-element (by-id (str id "-error"))]
    (play error-element fx/fade-out
             {:name "fade out error"})
    (play error-element fx/fade-in {:before #(set-html! error-element error)})))

(defmethod render-form-field [:error :editing-error] [{:keys [id error]}]
  (swap-error-messages id error))

(defmethod render-form-field [:editing-error :error] [{:keys [id error]}]
  (swap-error-messages id error))

(defmethod render-form-field [:editing-error :editing-valid] [{:keys [id]}]
  (let [error-element (by-id (str id "-error"))]
    (play error-element (assoc fx/fade-out :time 200))))

(defmethod render-form-field [:editing-error :empty] [{:keys [id]}]
  (let [error-element (by-id (str id "-error"))]
    (play error-element (assoc fx/fade-out :time 200))
    (fx/label-move-down (label-xpath id))))

(defn- add-input-event-listeners
  "Accepts a field-id and creates listeners for blur and focus events which will then fire
  `:field-changed` and `:editing-field` events."
  [field-id]
  (let [field (by-id field-id)
        keyboard (goog.events.KeyHandler. (by-id "form"))]
    (event/listen field
                  "blur"
                  #(dispatch/fire [:field-finished field-id] (value field)))
    (event/listen field
                  "focus"
                  #(dispatch/fire [:editing-field field-id]))
    (event/listen field
                  "keyup"
                  #(dispatch/fire [:field-changed field-id] (value field)))))

(defmulti render
  "Accepts a map which represents the current state of the application
  and renders a view based on the value of the `:state` key."
  :state)

(defn relevant-keypress-event
  [event]
  (not (or (.-ctrlKey event)
           (.-metaKey event)
           (= (.-keyCode event) key-codes/BACKSPACE)
           (= (.-keyCode event) key-codes/TAB)
           (= (.-keyCode event) key-codes/ENTER)
           (not (key-codes/isTextModifyingKeyEvent event)))))

(defn translate-keypress-event
  [text-box event]
  (let [start (getStart text-box)
        end (count (value text-box))
        prefix (apply str (take start (value text-box)))
        suffix (apply str (take-last (- end (getEnd text-box)) (value text-box)))]
    (set-value! (single-node text-box)
                (clojure.string/join [prefix
                                      (dvorak/simulate-dvorak (.fromCharCode js/String (.-charCode event)))
                                      suffix]))
    (setStart text-box (+ start 1))
    (setEnd text-box (+ start 1)))
  )

(defmethod render :init [_]
  (fx/initialize-views (:form snippets) (:greeting snippets))
  (add-input-event-listeners "text-input")

  (set-value! (by-id "text-input") (rand-nth [
                                              "The quick brown fox jumped over the lazy dog.",
                                              "Hello, world!",
                                              "Live long and prosper.",
                                              "Hey, I just met you,\nAnd this is crazy, \nBut here's my number, \nSo call me, maybe?",
                                              "Elementary, my dear Watson.",
                                              "Toto, I've got a feeling we're not in Kansas anymore.",
                                              "Release the kraken!",
                                              "I'll make him an offer he can't refuse.",
                                              "May the Force be with you.",
                                              "Houston, we have a problem."
                                              ]))
  (dispatch/fire [:field-changed "text-input"] (value (by-id "text-input")))
  
  (let [text-box (by-id "text-dvorak-input")]
      (event/listen (goog.events.KeyHandler. text-box)
                    "key"
                    (fn [e]
                      (when (relevant-keypress-event e)
                        (translate-keypress-event text-box e)
                        (dispatch/fire :dvorak-input {
                                                      :charcode (.-charCode e)
                                                      :region-start (getStart text-box)
                                                      :region-end (getEnd text-box)
                                                      :target text-box
                                                      })
                        (.preventDefault e))))))

(defmethod render :form [{:keys [state error name]}]
  (fx/show-form)
  (set-value! (by-id "text-input") "")
  (dispatch/fire [:field-finished "text-input"] ""))

(defmethod render :greeting [{:keys [state name exists]}]
  (set-text! (single-node (by-class "name")) name)
  (set-text! (single-node (by-class "again")) (if exists "again" ""))
  (fx/show-greeting))

(dispatch/react-to #{:state-change} (fn [_ m] (render m)))

(defn- form-fields-status
  "Given a map of old and new form states, generate a map with `:id`,
  `:transition` and `:error` keys which can be passed to
  `render-form-field`."
  [m]
  (map #(hash-map :id %
                  :transition [(or (get-in m [:old :fields % :status]) :empty)
                               (get-in m [:new :fields % :status])]
                  :error (get-in m [:new :fields % :error]))
       (keys (get-in m [:new :fields]))))

(dispatch/react-to #{:form-change}
                   (fn [_ m]
                     (doseq [s (form-fields-status m)]
                       (render-form-field s))
                     (set-value! (single-node (by-id "text-output")) (get-in m [:new :dvorak-string]))
                     (render-button [(get-in m [:old :status])
                                     (get-in m [:new :status])] )))
