;   Copyright (c) Pavel Prokopenko. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns formar.core)

(def ^{:private true}
  empty-form {:data {} :data-errors {} :form-errors []})

(def ^{:private true}
  email-regexp #"^[_A-Za-z0-9-\+]+(\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\.[A-Za-z0-9]+)*(\.[A-Za-z]{2,})$")

(defn- data-error
  [m attribute error]
  (assoc-in m [:data-errors attribute] error))

(defn- get-value
  ([m attribute]
   (get-in m [:data attribute]))
  ([m attribute not-found]
   (get-in m [:data attribute] not-found)))

;; Helpers

(defn valid?
  ([form]
   (and
     (empty? (:data-errors form))
     (empty? (:form-errors form))))
  ([form attribute]
   (nil? (get-in form [:data-errors attribute]))))

;; Transformers

(defn number
  "Creates a transformer that converts the value assigned to attribute key to a number.
   It silently ignores non-existing keys."
  [attribute & {:keys [message msg-fn] :or {message "should be a number"}}]
  (let [msg-fn (or msg-fn (constantly message))]
    (fn [m]
      (let [value (get-value m attribute ::not-found)]
        (if-not (= value ::not-found)
          (try
            (assoc-in m [:data attribute] (Long/parseLong value))
            (catch NumberFormatException e
              (data-error m attribute (msg-fn attribute :number))))
          m)))))

(defn required
  "Creates a transformer that checks if the value assigned to attribute key
   is not nil or an empty string. It leaves the original map untouched."
  [attribute & {:keys [message msg-fn] :or {message "is required"}}]
  (let [msg-fn (or msg-fn (constantly message))]
    (fn [m]
      (let [value (get-value m attribute)]
        (if (or (nil? value)
                (if (string? value)
                  (empty? (clojure.string/trim value)) false))
          (data-error m attribute (msg-fn attribute :required))
          m)))))

(defn pattern
  [attribute regexp & {:keys [message msg-fn allow-nil] :or {message "has incorrect format" allow-nil true}}]
  (let [msg-fn (or msg-fn (constantly message))]
    (fn [m]
      (let [value (get-value m attribute)]
        (if (nil? value)
          (if (false? allow-nil)
            (data-error m attribute (msg-fn attribute :required))
            m)
          (if-not (re-matches regexp value)
            (data-error m attribute (msg-fn attribute :format regexp))
            m))))))

(defn email
  [attribute & {:keys [message msg-fn allow-nil] :or {message "is not a valid email" allow-nil false}}]
  (pattern attribute email-regexp :message message :msg-fn msg-fn :allow-nil allow-nil))

;; Forms

(defn- fetch-attribute
  [source dest [attribute transformers]]
  (let [value (get source (name attribute))]
    (loop [res (assoc-in dest [:data attribute] value)
           ts transformers]
      (if-not (empty? ts)
        (let [t (first ts)
              res (t res)]
          (if (valid? res attribute)
            (recur res (rest ts))
            res))
        res))))

(defn- fetch-form
  [fields source]
  (reduce (partial fetch-attribute source) empty-form fields))

;; ----------------------

(defmacro expand-field
  ([x] [])
  ([x forms]
   (vec
     (for [form forms]
       (if (seq? form)
         (with-meta `(~(first form) ~x ~@(rest form)) (meta form))
         (list form x))))))

(defmacro form-fields
  [fields]
  (vec
    (for [[field & forms] fields]
      `[~field (expand-field ~field ~forms)])))

(defmacro defform
  [name body]
  `(defn ~name
     [m#]
     (#'fetch-form (form-fields ~(first body)) m#)))
