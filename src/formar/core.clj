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
  "Checks if the given form is valid."
  ([form]
   (and
     (empty? (:data-errors form))
     (empty? (:form-errors form))))
  ([form attribute]
   (nil? (get-in form [:data-errors attribute]))))

(defn- coerce-number
  "Coerces a value to an number otherwise returns nil."
  [val]
  (if-not (integer? val)
    (try
      (Long/parseLong val)
      (catch NumberFormatException e
        nil))
    val))

;; Transformers

(defn number
  "Creates a transformer that converts the value assigned to the given 'attribute' key to a number.
   It silently ignores non-existing keys.

   Optional parameters:
       :message (default: \"should be a number\") - the triggered error message
       :msg-fn  (default: nil) - a function (fn [attribute type]) to retrieve the error message"
  [attribute & {:keys [message msg-fn] :or {message "should be a number"}}]
  (let [msg-fn (or msg-fn (constantly message))]
    (fn [m]
      (if-let [value (get-value m attribute)]
        (if-let [value (coerce-number value)]
          (assoc-in m [:data attribute] value)
          (data-error m attribute (msg-fn attribute :number)))
        m))))

(defn range-of
  "Creates a transformer that converts the value assigned to the given 'attribute' key to a number
   and checks that it belongs to the given range.

   Optional parameters:
       :max (default: nil) - the maximum allowed value
       :min (default: nil) - the minimum allowed value
       :number-message (default: \"should be a number\") - the 'number' error message
       :min-message (default: \"should be greater than %d\") - the 'min' error message
       :max-message (default: \"should be less than %d\") - the 'max' error message
       :range-message (default: \"should be between %d and %d\") - the 'range' error message
       :msg-fn  (default: nil) - a function (fn [attribute type & args]) to retrieve the error message"
  [attribute & {:keys [max min msg-fn number-message min-message max-message range-message]
                :or {number-message "should be a number"
                     min-message    "should be greater than %d"
                     max-message    "should be less than %d"
                     range-message  "should be between %d and %d"}}]
  (let [number-fn (or msg-fn (constantly number-message))
        min-fn    (or msg-fn (constantly (format min-message min)))
        max-fn    (or msg-fn (constantly (format max-message max)))
        range-fn  (or msg-fn (constantly (format range-message min max)))]
    (fn [m]
      (if-let [value (get-value m attribute)]
        (if-let [value (coerce-number value)]
          (cond
           (and (not (nil? min))
                (not (nil? max))
                (not (and (>= value min)
                          (<= value max)))) (data-error m attribute (range-fn attribute :range min max))

           (and (not (nil? min))
                (nil? max)
                (not (>= value min)))       (data-error m attribute (min-fn attribute :min min))

           (and (nil? min)
                (not (nil? max))
                (not (<= value max)))       (data-error m attribute (max-fn attribute :max max))

           :else                            (assoc-in m [:data attribute] value))
          (data-error m attribute (number-fn attribute :number)))
        m))))

(defn length
  "Creates a validator for the length of the value assigned to the 'attribute' key.

   Optional parameters:
       :is (default: nil) - the exact allowed length (if given, :min and :max are ignored)
       :max (default: nil) - the maximum allowed length
       :min (default: nil) - the minimum allowed length
       :is-message (default: \"should be exactly %d character(s)\") - the 'is' error message
       :min-message (default: \"should be at least %d character(s)\") - the 'min' error message
       :max-message (default: \"should be at most %d character(s)\") - the 'max' error message
       :range-message (default: \"should be between %d and %d characters long\") - the 'range' error message
       :msg-fn  (default: nil) - a function (fn [attribute type & args]) to retrieve the error message"
  [attribute & {:keys [is min max msg-fn is-message min-message max-message range-message]
                :or {is-message    "should be exactly %d character(s)"
                     min-message   "should be at least %d character(s)"
                     max-message   "should be at most %d character(s)"
                     range-message "should be between %d and %d characters long"}}]
  (let [is-fn    (or msg-fn (constantly (format is-message is)))
        min-fn   (or msg-fn (constantly (format min-message min)))
        max-fn   (or msg-fn (constantly (format max-message max)))
        range-fn (or msg-fn (constantly (format range-message min max)))]
    (fn [m]
      (if-let [value (get-value m attribute)]
        (let [len (count value)]
          (cond
           (and (not (nil? is))
                (not (= is len)))         (data-error m attribute (is-fn attribute :is is))

           (and (not (nil? min))
                (not (nil? max))
                (not (and (>= len min)
                          (<= len max)))) (data-error m attribute (range-fn attribute :range min max))

           (and (not (nil? min))
                (nil? max)
                (not (>= len min)))       (data-error m attribute (min-fn attribute :min min))

           (and (nil? min)
                (not (nil? max))
                (not (<= len max)))       (data-error m attribute (max-fn attribute :max max))

           :else                          m))
        m))))

(defn choice
  "Creates a validator that checks that the value assigned to the given 'attribute' key
   is in the set of allowed values. Nil-values are not allowed.

   Optional parameters:
       :required-message (default: \"is required\") - the 'required' error message
       :not-allowed-message (default: \"is not allowed\") - the 'not-allowed-value' message
       :msg-fn (default: nil) - a function (fn [attribute type]) to retrieve the error message"
  [attribute options & {:keys [msg-fn required-message not-allowed-message]
                        :or {required-message    "is required"
                             not-allowed-message "is not allowed"}}]
  (let [required-fn    (or msg-fn (constantly required-message))
        not-allowed-fn (or msg-fn (constantly not-allowed-message))]
    (fn [m]
      (if-let [value (get-value m attribute)]
        (if-not (contains? options value)
          (data-error m attribute (not-allowed-fn attribute :not-allowed))
          m)
        (data-error m attribute (required-fn attribute :required))))))

(defn required
  "Creates a validator that check whether the value assigned to the 'attribute' key
   is not nil or an empty string.

   Optional parameters:
       :message (default: \"is required\") - the error message
       :msg-fn (default: nil) - a function (fn [attribute type]) to retrieve the error message"
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
  "Creates a validator that check if the value assigned to the given 'attribute' key
   matches the given regexp. It does not allow nils by default.

   Optional parameters:
       :allow-nil (default: true) - allow nils or not
       :message (default: \"has incorrect format\") - the error message
       :msg-fn (default: nil) - a function (fn [attribute type]) to retrieve the error message"
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
  "Creates a validator that checks if the value assigned to the given 'attribute' key
   is a valid email address. By default it does not allow nils.

   Optional parameters:
       :allow-nil (default: true) - allow nils or not
       :message (default: \"is not a valid email\") - the error message
       :msg-fn (default: nil) - a function (fn [attribute type]) to retrieve the error message"
  [attribute & {:keys [message msg-fn allow-nil] :or {message "is not a valid email" allow-nil false}}]
  (pattern attribute email-regexp :message message :msg-fn msg-fn :allow-nil allow-nil))

(defn keywordize
  "Creates a transformer that changes the value assigned to the given 'attribute' key to a keyword."
  [attribute]
  (fn [m]
    (if-let [value (get-value m attribute)]
      (assoc-in m [:data attribute] (keyword value))
      m)))

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
