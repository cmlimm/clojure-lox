(ns clojure-lox.error
  (:require [clojure.tools.trace :as trace]))

(defn report [{line :line message :message symbol :symbol}]
  (do
    (println (format "[line %s] %s" line message))
    (flush)))

(defn report-errors [errors]
  (doseq [error errors] (report error)))