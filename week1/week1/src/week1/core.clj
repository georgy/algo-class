(ns week1.core)

(def merge-and-count)

(defn sort-and-count
  [seq]
  (let
      [size (count seq)
       mid (/ size 2)]
    (cond
     (= size 0) [seq 0]
     (= size 1) [seq 0]
     :else
     (let
         [l-result (sort-and-count (take mid seq))
          r-result (sort-and-count (drop mid seq))
          left (l-result 0)
          right (r-result 0)
          result (merge-and-count left right)]
       [(result 0) (+ (l-result 1)
                      (r-result 1)
                      (result 1))]))))

(defn merge-and-count
  [left right]
  (loop
      [left left
       right right
       result []
       cnt 0]
    (let
        [l (first left)
         r (first right)]
      (cond
       (and (nil? l) (nil? r)) [result cnt] ; done
       (nil? l) (recur left (rest right) (conj result r) cnt) ; left is empty
       (nil? r) (recur (rest left) right (conj result l) cnt) ; right is empty
       :else (if (< l r)
               (recur (rest left) right (conj result l) cnt)
               (recur left (rest right) (conj result r) (+ cnt (count left))))))))

(defn int-seq
  []
  (with-open [rdr (clojure.java.io/reader "resources/IntegerArray.txt")]
    (doall (map #(Integer/valueOf %) (line-seq rdr)))))

(defn answer
  []
  ((sort-and-count (int-seq)) 1))

; Mac book Air: "Elapsed time: 132645.824 msecs" ~ 2 mins