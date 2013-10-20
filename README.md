# formar

A Clojure library that helps to transform and validate http forms.

## Usage

Include the library in your leiningen project dependencies:

```clojure
[formar "0.1.0"]
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
           [:password (required :message "Passwords is required!")]]])

(print (login-form {"username" "da-le" "email" "email" "bad-data" "bad-value"}))

;; {:data {:password nil, :username da-le},
;;  :data-errors {:password Passwords is required!, :username Only [a-zA-Z] characters are allowed!},
;;  :form-errors []}
```

## License

Copyright © 2013 Pavel Prokopenko

Distributed under the Eclipse Public License.