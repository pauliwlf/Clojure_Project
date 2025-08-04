(ns mini.playground)

; 2-adic function
; n-adic function
; variadic function

; partial
(defn custom-partial [func & args]
  (fn [& rest-args]
    (apply func (concat args rest-args))))

((custom-partial + 5 4 5) 2 3)

(defn add [a b] (+ a b))
((custom-partial add 2 3) 4)

; curry 2-adic
(defn curry [func]
  (fn [arg1]
    (fn [arg2]
      (func arg1 arg2))))

; curry variadic
(defn curry-variadic [func]
  (letfn [(collect-args [args]
            (fn [& next-args]
              (if (some #{:done} next-args)
                (apply func (concat args (remove #{:done} next-args)))
                (collect-args (concat args next-args)))))]
    (collect-args [])))

(((((curry-variadic +) 2) 3) 4) 4 5 :done)
(((curry-variadic +) 2 3) :done)

; composition
(defn composition [& funcs]
  (fn [& args]
    ((fn [funcs, args]
       (if (empty? funcs)
         (first args)
         (recur (butlast funcs) (list (apply (last funcs) args)))))
     funcs args)))

;; (defn composition-loop [& funcs]
;;   (fn [& args]
;;     (loop [current-funcs funcs current-args args]
;;       (if (empty? current-funcs)
;;         current-args
;;         (recur (butlast current-funcs) (list (apply (last current-funcs) current-args)))))))

((composition str dec +) 2 3 4 5)

; reduce
(defn custom-reduce [func, seq]
  ((fn [func, seq, acc]
     (if (empty? seq)
       acc
       (recur func (rest seq) (func acc (first seq)))))
   func (rest seq) (first seq)))
(custom-reduce + '())