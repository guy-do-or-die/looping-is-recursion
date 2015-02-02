(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [res n]
                 (cond
                   (= n 0) 1
                   (= n 1) res
                   :else (recur (* res base) (dec n))))]
    (if (zero? base)
      0
      (helper base exp))))

(defn last-element [a-seq]
  (let [helper (fn [[f & r]]
                 (if (empty? r)
                   f
                   (recur r)))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [[f1 & r1] [f2 & r2]]
               (cond 
                 (not= f1 f2) false
                 (empty? r1) (= f1 f2)
                 :else (recur r1 r2) ))]
    (if (= (count seq1) (count seq2))
      (helper seq1 seq2)
      false)))

(defn find-first-index [pred a-seq]
  (loop [[f & r :as s] a-seq
         i 0 ]
    (cond 
      (empty? s) nil
      (pred f) i
      :else (recur r (inc i)))))

(defn avg [a-seq]
  (loop [sum 0 n 0 [f & r :as s] a-seq]
    (if (empty? s)
      (/ sum n)
      (recur (+ sum f) (inc n) r))))

(defn parity [a-seq]
  (let [toggle #(if (contains? % %2)
                 (disj % %2)
                 (conj % %2))] 
    (loop [[f & r :as s] a-seq
         res #{}]
    (if (empty? s)
      res
      (recur r (toggle res f))))))

(defn fast-fibo [n]
  (loop [nx-1 0 nx 1 i 1]
    (cond 
      (zero? n) 0
      (= i n) nx
      :else (recur nx (+ nx nx-1) (inc i)))
    ))

(defn cut-at-repetition [a-seq]
  (loop [[f & r :as s] a-seq
         res []
         els #{}]
    (if (or (empty? s) (contains? els f))
      res
      (recur r (conj res f) (conj els f)))))
