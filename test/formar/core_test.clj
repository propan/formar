;   Copyright (c) Pavel Prokopenko. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns formar.core-test
  (:require [clojure.test :refer :all]
            [formar.core :refer :all]))

(deftest test-number
  (testing "Ignores non-existing keys"
    (let [tran-fn (number :age)
          result  (tran-fn {:data {}})]
      (is (nil? (get-in result [:data-errors :age])))
      (is (nil? (get-in result [:data :age])))))

  (testing "Transfroms values to numbers"
    (let [tran-fn (number :age)
          result  (tran-fn {:data {:age "30"}})]
      (is (nil? (get-in result [:data-errors :age])))
      (is (= 30 (get-in result [:data :age])))))

  (testing "Allows custom error message"
    (let [tran-fn (number :age :message "Your value is incorrect")
          result  (tran-fn {:data {:age "bad-value"}})]
      (is (= "Your value is incorrect" (get-in result [:data-errors :age])))
      (is (= "bad-value" (get-in result [:data :age])))))

  (testing "Allows custom error message function"
    (let [tran-fn (number :age :msg-fn (constantly "Your value is incorrect"))
          result  (tran-fn {:data {:age "bad-value"}})]
      (is (= "Your value is incorrect" (get-in result [:data-errors :age])))
      (is (= "bad-value" (get-in result [:data :age]))))))

(deftest test-range-of
  (testing "Ignores non-existing keys"
    (let [tran-fn (range-of :age :min 18)
          result  (tran-fn {:data {}})]
      (is (nil? (get-in result [:data-errors :age])))
      (is (nil? (get-in result [:data :age])))))

  (testing "Converts values to numbers"
    (let [tran-fn (range-of :age)
          result  (tran-fn {:data {:age "30"}})]
      (is (nil? (get-in result [:data-errors :age])))
      (is (= 30 (get-in result [:data :age])))))
  
  (let [tran-fn (range-of :age :min 18)]
    (testing "Only minimum is checked [positive]"
      (let [result (tran-fn {:data {:age 42}})]
        (is (nil? (get-in result [:data-errors :age])))
        (is (= 42 (get-in result [:data :age])))))

    (testing "Only minimum is checked [negative]"
      (let [result (tran-fn {:data {:age 12}})]
        (is (= "should be greater than 18" (get-in result [:data-errors :age])))
        (is (= 12 (get-in result [:data :age]))))))

  (let [tran-fn (range-of :age :max 18)]
    (testing "Only maximum is checked [positive]"
      (let [result (tran-fn {:data {:age 12}})]
        (is (nil? (get-in result [:data-errors :age])))
        (is (= 12 (get-in result [:data :age])))))

    (testing "Only maximum is checked [negative]"
      (let [result (tran-fn {:data {:age 42}})]
        (is (= "should be less than 18" (get-in result [:data-errors :age])))
        (is (= 42 (get-in result [:data :age]))))))

  (let [tran-fn (range-of :age :min 10 :max 16)]
    (testing "Range is checked [positive]"
      (let [result (tran-fn {:data {:age 12}})]
        (is (nil? (get-in result [:data-errors :age])))
        (is (= 12 (get-in result [:data :age])))))

    (testing "Range is checked [negative]"
      (let [result (tran-fn {:data {:age 42}})]
        (is (= "should be between 10 and 16" (get-in result [:data-errors :age])))
        (is (= 42 (get-in result [:data :age])))))))

(deftest test-choice
  (let [tran-fn (choice :color #{"red" "green" "blue"})]

    (testing "Catches nil values"
      (let [result (tran-fn {})]
        (is (= "is required" (get-in result [:data-errors :color])))))

    (testing "Catches wrong values"
      (let [result (tran-fn {:data {:color "yellow"}})]
        (is (= "is not allowed" (get-in result [:data-errors :color])))))

    (testing "Allowes correct values"
      (let [result (tran-fn {:data {:color "red"}})]
        (is (= "red" (get-in result [:data :color])))
        (is (nil? (get-in result [:data-errors :color])))))))

(deftest test-required
  (testing "Catches nil values"
    (let [tran-fn (required :age)
          result  (tran-fn {:data {}})]
      (is (= "is required" (get-in result [:data-errors :age])))))

  (testing "Catches empty strings"
    (let [tran-fn (required :age)
          result  (tran-fn {:data {:age ""}})]
      (is (= "is required" (get-in result [:data-errors :age])))))

  (testing "Allows custom error message"
    (let [tran-fn (required :age :message "Your value is incorrect")
          result  (tran-fn {})]
      (is (= "Your value is incorrect" (get-in result [:data-errors :age])))))

  (testing "Allows custom error message function"
    (let [tran-fn (required :age :msg-fn (constantly "Your value is incorrect"))
          result  (tran-fn {})]
      (is (= "Your value is incorrect" (get-in result [:data-errors :age]))))))

(deftest test-pattern
  (testing "Ignores matches and nils"
    (let [tran-fn (pattern :age #"\d+")]
      (is (nil? (get-in (tran-fn {:data {:age "18"}}) [:data-errors :age])))
      (is (nil? (get-in (tran-fn {}) [:data-errors :age])))))

  (testing "Catches wrong format & nils"
    (let [tran-fn (pattern :age #"\d+" :allow-nil false)]
      (is (= "has incorrect format" (get-in (tran-fn {:data {:age "18 years"}}) [:data-errors :age])))
      (is (= "has incorrect format" (get-in (tran-fn {}) [:data-errors :age])))))

  (testing "Allows custom error message"
    (let [tran-fn (pattern :age #"\d+" :message "Your value is incorrect" :allow-nil false)
          result  (tran-fn {})]
      (is (= "Your value is incorrect" (get-in result [:data-errors :age])))))

  (testing "Allows custom error message function"
    (let [tran-fn (pattern :age #"\d+" :msg-fn (constantly "Your value is incorrect") :allow-nil false)
          result  (tran-fn {})]
      (is (= "Your value is incorrect" (get-in result [:data-errors :age]))))))

(deftest test-email
  (testing "Ignores matches and nils"
    (let [tran-fn (email :email :allow-nil true)]
      (is (nil? (get-in (tran-fn {:data {:email "dale.cooper@thecoopers.com"}}) [:data-errors :email])))
      (is (nil? (get-in (tran-fn {}) [:data-errors :email])))))

  (testing "Catches wrong format & nils"
    (let [tran-fn (email :email)]
      (is (= "is not a valid email" (get-in (tran-fn {:data {:email "email"}}) [:data-errors :email])))
      (is (= "is not a valid email" (get-in (tran-fn {}) [:data-errors :email]))))))

(defn passwords-match
  [m]
  (let [{:keys [password repeat-password]} (:data m)]
    (if-not (= password repeat-password)
      (update-in m [:form-errors] conj "Passwords don't match!")
      m)))

(defform simple-registration-form
  [[[:username required (pattern #"^[a-zA-Z0-9_]+$")]
    [:email required email]
    [:password required]]])

(defform registration-form
  [[[:username required (pattern #"^[a-zA-Z0-9_]+$")]
    [:email required email]
    [:password required]
    [:repeat-password required]]
   [passwords-match]])

(deftest form-test
  (testing "Field validation"
    (let [result (simple-registration-form {"username"    "bob"
                                            "email"       "email"
                                            "password"    ""
                                            "extra-field" "bad-data"})]
      (is (= "bob" (get-in result [:data :username])))
      (is (= "email" (get-in result [:data :email])))
      (is (= "" (get-in result [:data :password])))
      (is (nil? (get-in result [:data :extra-field])))
      (is (nil? (get-in result [:data-errors :username])))
      (is (= "is not a valid email" (get-in result [:data-errors :email])))
      (is (= "is required" (get-in result [:data-errors :password])))))

  (testing "Form validation"
    (let [result (registration-form {"username"        "bob"
                                     "email"           "bob@thebobs.com"
                                     "password"        "pass"
                                     "repeat-password" "word"})]
      (is (empty? (:data-errors result)))
      (is (= 1 (count (:form-errors result))))
      (is (true? (some #(= "Passwords don't match!" %) (:form-errors result)))))))
