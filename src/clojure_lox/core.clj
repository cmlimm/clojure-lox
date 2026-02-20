(ns clojure-lox.core
  (:gen-class)
  (:require [clojure-lox.error :as error])
  (:require [clojure-lox.scanner :as scanner])
  (:require [clojure.tools.trace :as trace]))

(defn run 
  ([string] (run string false))
  ([string interrupt-on-error?]
   (let [{tokens :tokens token-errors :errors} (scanner/scan string)]
     (if (seq token-errors)
       (do
         (error/report-errors token-errors)
         ; (when interrupt-on-error? (java.lang.System/exit 65))
         )
       (println string)))))

(def interrupt-on-error true)

(defn run-prompt
  ([] (do
        (print "> ")
        (flush)
        (run-prompt (read-line))))
  ([line] (when (not (= line "")) 
            (do
              (run line)
              (recur (do
                       (print "> ")
                       (flush)
                       (read-line)))))))

(defn run-file [filename]
  (run (slurp filename) interrupt-on-error))

(defn -main 
  ([] (run-prompt))
  ([filename] (run-file filename))
  ([filename & args] (do
                       (println "Usage: clojure-lox [script]")
                       (java.lang.System/exit 64))))

; (-main "test.lox")
; (trace/untrace-ns lex-interpreter.core)
; (trace/untrace-ns lex-interpreter.scanner)
; (trace/untrace-ns lex-interpreter.error)
; (trace/untrace-ns lex-interpreter.token)