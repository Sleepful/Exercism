(ns matching-brackets)
(require '[clojure.string :as str])

(def ^:private open-paren
  #{"(" "{" "["})

(def ^:private close-paren
  #{")" "}" "]"})

(def ^:private paren-dict
  {"(" ")"
   "{" "}"
   "[" "]"})

(defn matches-paren [open-paren close-paren]
  (= (paren-dict open-paren) close-paren))

(defn- pop-open-paren [stack-atom]
  ; (prn "pre" @stack-atom)
  (swap! stack-atom pop))
  ; (prn "post" @stack-atom))

(defn- append-open-paren [stack-atom char]
  (swap! stack-atom conj char))

; if matching open paren in the stack, then pop it, otherwise fail
(defn handle-close-paren [stack-atom close-paren fail-atom]
  (cond
    (matches-paren (peek @stack-atom) close-paren)  (do #_(prn "closing: " close-paren)
                                                     (pop-open-paren stack-atom))

    :else (reset! fail-atom true)))

(defn valid?
  "Returns true if the given string has properly matched brackets; otherwise, returns false."
  [s]
  (let [chars (str/split s #"")
        stack (atom [])
        fail (atom false)]
    ; (prn chars)
    (doseq [char chars] ; :while (true? @fail)]
      (do (cond
               ; case:
            (contains? open-paren char)
            (append-open-paren stack char)
               ; case:
            (contains? close-paren char)
            (handle-close-paren stack char fail)
              ; ignore non-parens, the list comprehension will move to the next char
            :else (prn "nothin" char))))
          ;(prn char)))

    ; stack must be empty to denote all matched params
    ; (prn "stack" @stack)
    ; (prn "fail" @fail)
    (and (not @fail) (empty? @stack))))

(valid? "()a")
(valid? "(()]")
(valid? "{[]}")

