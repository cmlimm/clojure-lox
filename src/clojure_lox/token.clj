(ns clojure-lox.token)

(defn get-token [type lexeme literal line]
  {
   :type type
   :lexeme lexeme
   :literal literal
   :line line
   })