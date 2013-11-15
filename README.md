# formar

A Clojure library that helps to transform and validate http forms.

## Usage

Include the library in your leiningen project dependencies:

```clojure
[formar "0.1.2"]
```

## Examples

```clojure
(ns your-namespace
  (:require [formar.core :refer [defform email pattern required]]))

(defform registration-form
         [[[:username required (pattern #"^[a-zA-Z0-9_]+$")]
           [:email required email]
           [:password required]]])

(print (registration-form {"username" "dale" "email" "email" "bad-data" "bad-value"}))

;; {:data {:password nil, :email email, :username dale},
;;  :data-errors {:password is required, :email is not a valid email},
;;  :form-errors []}

(defform login-form
         [[[:username required (pattern #"[a-zA-Z]+" :msg-fn (constantly "Only [a-zA-Z] characters are allowed!"))]
           [:password (required :message "Password is required!")]]])

(print (login-form {"username" "da-le" "email" "email" "bad-data" "bad-value"}))

;; {:data {:password nil, :username da-le},
;;  :data-errors {:password Password is required!, :username Only [a-zA-Z] characters are allowed!},
;;  :form-errors []}
```

### Validation of dependant fields

If you need to check that a few dependant fields comply with certain rules, you can define a function that takes
the result of all field transformations and returns a result of it's own checks/transformations. You can apply
as many "form" transformers as you like.

Important:
  - Form transformers are applied only if previous field transformations detected no errors!
  - Execution of multiple form transformers is interrupted when any of them triggers error state.

```clojure
(ns your-namespace
  (:require [formar.core :refer [defform required]]))

(defn passwords-match
  [m]
  (let [{:keys [password repeat-password]} (:data m)]
    (if-not (= password repeat-password)
      (update-in m [:form-errors] conj "Passwords don't match!")
      m)))

(defform registration-form
  [[[:username required (pattern #"^[a-zA-Z0-9_]+$")]
    [:password required]
    [:repeat-password required]]
   [passwords-match]])

(print (registration-form {"username" "dale" "password" "pass" "repeat-password" "word"}))

;; {:data {:repeat-password word, :password pass, :username bob}, :data-errors {}, :form-errors [Passwords don't match!]}
```

You can also use functions that take more than one argument, but the first argument have to be the map to be transformed.

```clojure
(defform registration-form
  [[[:username required (pattern #"^[a-zA-Z0-9_]+$")]
    [:password required]
    [:repeat-password required]]
   [(update-in [:form-errors] conj "Error message!")]])

(print (registration-form {"username" "dale" "password" "pass" "repeat-password" "word"}))

;; {:data {:repeat-password word, :password pass, :username bob}, :data-errors {}, :form-errors [Error message!]}
```

## License

Copyright © 2013 Pavel Prokopenko

Distributed under the Eclipse Public License.
