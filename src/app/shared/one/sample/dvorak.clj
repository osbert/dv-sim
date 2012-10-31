(ns one.sample.dvorak
  (:use [clojure.string :only [lower-case upper-case]]))

(def tr-table
  {
   "a" "a"
   "b" "n"
   "c" "i"
   "d" "h"
   "e" "d"
   "f" "y"
   "g" "u"
   "h" "j"
   "i" "g"
   "j" "c"
   "k" "v"
   "l" "p"
   "m" "m"
   "n" "l"
   "o" "s"
   "p" "r"
   "q" "x"
   "r" "o"
   "s" ";"
   "t" "k"
   "u" "f"
   "v" "."
   "w" ","
   "x" "b"
   "y" "t"
   "z" "/"
   "." "e"
   "," "w"
   "'" "q"
   "\"" "Q"
   ";" "z"
   "<" "W"
   ">" "E"
   })

(def reverse-tr-table
  (zipmap (vals tr-table) (keys tr-table)))

(defn char-replace
  [table char]
  (let [s (str char)]
    (or (table s)
        (let [table-lower-s (table (lower-case s))]
          (and table-lower-s (upper-case table-lower-s)))
        s)))

(defn convert-to-dvorak
  [s]
  (clojure.string/join (map (partial char-replace tr-table) s)))

(defn simulate-dvorak
  [s]
  (clojure.string/join (map (partial char-replace reverse-tr-table) s)))
