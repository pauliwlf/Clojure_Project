(ns mini.playground)



; Immutability -> Objekte können nicht verändert werden
(def testVariable 5)
(str testVariable)
(+ testVariable 4)
(str testVariable)

; Commutability -> Reihenfolge ist bei manchen Funktionen egal

(= (+ 1 2 3 4 5) (+ 2 4 3 1 5))

; Homoiconicity -> Program Code is based on Data structures of program language
; e.g. Function invocation = list




(def testVariable (+ testVariable 4))
(str testVariable)


let a = 1
a += 2
print a









(def test ((fn [& x] (apply + x)) 2 3 4 5 6))
test

(defn dom [] '(1 2 3 4 5))
(defn olli [] '(1 2 3 4 5 6 7))
(apply + (concat (olli) (dom)))
(+ '(1 2 3 4 5) 1 2 3 4 5)

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

;closure

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




(custom-reduce + '(2 3 4 5))

(custom-reduce - '(1 2 3))

(custom-reduce str '("Hallo " "Welt" "!"))

(custom-reduce max '(5 1 23 15))

(custom-reduce inc '(1 2))