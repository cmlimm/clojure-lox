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

(defn end? [scanner]
  (>= (scanner :current) (count (scanner :source))))

(defn get-current [scanner]
  (nth (scanner :source) (scanner :current)))

(defn get-current-or-end [scanner]
  "Formerly known as peek"
  (if (end? scanner) nil (get-current scanner)))

(defn advance [scanner]
  (assoc scanner :current (inc (scanner :current))))

(defn get-next-or-end [scanner]
  "Formerly known as peek-next"
  (let [advanced-scanner (advance scanner)]
    (if (end? advanced-scanner) nil (get-current advanced-scanner))))

(defn advance-line [scanner]
  (assoc scanner :line (inc (scanner :line))))

(defn next-token [scanner]
  (assoc scanner :start (scanner :current)))

(defn match [scanner expected-symbol]
  (cond
    (end? scanner) false
    (not= expected-symbol (get-current scanner)) false
    :else true))

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

(defn add-string [scanner]
  (cond
    (end? scanner) (add-error scanner "Unterminated string.")
    (= (get-current scanner) \") (let [-scanner (advance scanner)]
                                   (add-token -scanner :token/string
                                     (subs (-scanner :source)
                                       (inc (-scanner :start))
                                       (dec (-scanner :current)))))
  		:else (recur (advance (if (= (get-current scanner) \newline)
                            (advance-line scanner)
                            scanner)))))

(defn digit? [symbol] 
  (and 
    (>= 0 (compare \0 symbol)) 
    (<= 0 (compare \9 symbol))))

(defn add-number [scanner]
  ; Because we only call add-number when currently looking at a digit,
  ; we will never start parsing anything like '.4' (which will be incorrectly
  ; parsed by add-number as 0.4).
  (cond
    (digit? (get-current-or-end scanner)) (recur (advance scanner))
    (and
      (= \. (get-current-or-end scanner))
      (digit? (get-next-or-end scanner))) (recur (advance scanner))
    :else (add-token scanner :token/number
            (parse-double (subs (scanner :source) (scanner :start) (scanner :current))))))

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
      \! (if (match scanner \=)
           (add-token (advance scanner) :token/bang-equal)
           (add-token scanner :token/bang))
      \= (if (match scanner \=)
           (add-token (advance scanner) :token/equal-equal)
           (add-token scanner :token/equal))
      \< (if (match scanner \=)
           (add-token (advance scanner) :token/less-equal)
           (add-token scanner :token/less))
      \> (if (match scanner \=)
           (add-token (advance scanner) :token/greater-equal)
           (add-token scanner :token/greater))
      \/ (if (match scanner \/)
           (loop [-scanner scanner]
             (if (and
                   (not (end? -scanner))
                   (not= (get-current -scanner) \newline))
               (recur (advance -scanner))
               -scanner))
           (add-token scanner :token/slash))
      (\space \return \tab) scanner
      \newline (advance-line scanner)
      \" (add-string scanner)
      (cond
        (digit? char) (add-number scanner)
        :else (add-error scanner (format "Invalid character \"%s\"." char))))))

(defn scan [source]
  (loop [scanner (get-scanner source)]
    (if (end? scanner)
      (add-token scanner :token/eof)
      (recur (next-token (scan-token scanner))))))
