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
  "Creates a transformer that check if the value assigned to the given attribute key
   matches the given regexp. It does not allow nils by default."
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
  "Creates a transformer that checks if the value assigned to the given attribute key
   is a valid email address. By default it does not allow nils. It does not modify the original map."
  [attribute & {:keys [message msg-fn allow-nil] :or {message "is not a valid email" allow-nil false}}]
  (pattern attribute email-regexp :message message :msg-fn msg-fn :allow-nil allow-nil))

;; Forms

(defn- transform-field
  [source dest [field transformers]]
  (let [value (get source (name field))]
    (loop [res (assoc-in dest [:data field] value)
           ts  transformers]
      (if-not (empty? ts)
        (let [t (first ts)
              res (t res)]
          (if (valid? res field)
            (recur res (rest ts))
            res))
        res))))

(defn transform
  "Transforms form data from the source map based on the rules defined in fields.

   Accepts the following arguments:
     source  - an orginal map with string keys
     fields  - a list of field definitions:
               - the first item in the list is a keyword - the name of the field
               - the seconds items is a list of transformers to be applied to the field
     form-fn - a function (usually a composite function) to be appled to the result of field
               transformations if the result is valid

   Returns a map with the following keys:
     :data        - all data produced by transformers
     :data-errors - all data errors detected by field transformers
     :form-errors - a vector of errors detected by form transformers

   Notes:
     1. The field transformation stops if any of the field transformers detects an error.
     2. If any of field transformers detects a problem, non of the form transformers is triggered.
     3. If a form transformer detects an issue, the transformation ends."
  [source fields form-fn]
  (let [r (reduce (partial transform-field source) empty-form fields)]
    (if (valid? r) (form-fn r) r)))

;; ----------------------

(defmacro expand-field
  ([x] [])
  ([x forms]
   (mapv (fn [form]
           (if (seq? form)
             (with-meta `(~(first form) ~x ~@(next form)) (meta form))
             (list form x))) forms)))

(defmacro expand-fields
  [fields-spec]
  (mapv (fn [[field & forms]]
          `[~field (expand-field ~field ~forms)]) fields-spec))

(defmacro expand-form-fn
  [spec]
  (let [g (gensym)
        step (fn [trans-fn] `(if (valid? ~g) (-> ~g ~trans-fn) ~g))]
    `(fn [m#]
      (let [~g m#
            ~@(interleave (repeat g) (map step spec))]
         ~g))))

(defmacro defform
  "A simple macro that defines a function that takes a map (presumably a http-form) to be
   transformed and validated with a set of rules defined in the body. It returns a map -
   the result of this transfromation.

   Example usage:
     (defform registration-form
       [[[:username required (pattern #\"^[a-zA-Z0-9_]+$\")]
         [:email required email]
         [:password required]
         [:repeat-password required]]
       [password-match]])"
  [name body]
  `(defn ~name
     [m#]
     (transform m# (expand-fields ~(first body)) (expand-form-fn ~(second body)))))
