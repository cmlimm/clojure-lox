(ns clojure-lox.scanner
  (:require [clojure-lox.token :as token])
  (:require [clojure.tools.trace :as trace]))


(defn get-scanner [source]
  {
   :source source
   :tokens []
   :errors []
   :start 0
   :current 0
   :line 1
   })


(defn add-token 
  ([scanner token-type] (add-token scanner token-type nil))
  ([scanner token-type literal] (assoc scanner 
                                  :tokens 
                                  (conj (scanner :tokens)
                                    (token/get-token 
                                      token-type
                                      (subs (scanner :source) (scanner :start) (scanner :current))
                                      literal
                                      (scanner :line))))))


(defn add-error [scanner message]
  (assoc scanner :errors (conj (scanner :errors) {:message message
                                                  :line (scanner :line)})))


(defn advance [scanner]
  (assoc scanner :current (inc (scanner :current))))


(defn scan-token [scanner]
  (let [char (get (scanner :source) (scanner :current))
        scanner (advance scanner)]
    (case char
   	  \( (add-token scanner :token/left-paren)
   	  \) (add-token scanner :token/right-paren) 
   	  \{ (add-token scanner :token/left-brace)
   	  \} (add-token scanner :token/right-brace)
   	  \, (add-token scanner :token/comma)
   	  \. (add-token scanner :token/dot)
   	  \- (add-token scanner :token/minus)
   	  \+ (add-token scanner :token/plus)
   	  \; (add-token scanner :token/semicolon)
   	  \* (add-token scanner :token/star)
      (add-error scanner "Invalid character."))))


(defn next-token [scanner]
  (assoc scanner :start (scanner :current)))


(defn end? [scanner]
  (>= (scanner :current) (count (scanner :source))))


(defn scan [source]
  (loop [scanner (get-scanner source)]
    (if (end? scanner)
      (add-token scanner :token/eof)
      (recur (next-token (scan-token scanner))))))
