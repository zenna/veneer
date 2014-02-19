(ns veneer.core
  (import [java.io PushbackReader FileReader]))

(defn parse-clj [fname]
  (with-open [reader (PushbackReader. (FileReader. fname))]
    (read reader)))