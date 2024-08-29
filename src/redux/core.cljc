(ns redux.core
  (:require [redux.utils :refer [project complete-triangular-matrix pairs]])
  (:refer-clojure :exclude [juxt]))

(defn pre-step [rf f]
  (fn
    ([]      (rf))
    ([acc]   (rf acc))
    ([acc x] (rf acc (f x)))))

(defn post-complete [rf f]
  (fn
    ([]      (rf))
    ([acc]   (f (rf acc)))
    ([acc x] (rf acc x))))

(defn with-xform [rf xform]
  (let [rfv (volatile! nil)]
    (fn
      ([]
       (vreset! rfv (xform rf))
       (@rfv))
      ([acc]
       (@rfv acc))
      ([acc x]
       (@rfv acc x)))))

(defn juxt*
  "Take a sequence of reducing functions `rfns` and return a reducing function
  that separately applies each of `rfns` on each incoming element. As the
  completion, return a vector of fully accumulated values, one per `rfns`.

  This is a stateful \"transducer\". The returned value of this function is not
  safe for sharing and reuse. Use only once."
  [rfns]
  (let [rfns (object-array rfns)
        n (alength rfns)
        all-reduced? (object-array 1)]
    (fn
      ([] (amap rfns i _ ((aget rfns i))))
      ([^objects acc]
       (dotimes [i n]
         (aset acc i ((aget rfns i) (unreduced (aget acc i)))))
       (vec acc))
      ([^objects acc x]
       (aset all-reduced? 0 true)
       (dotimes [i n]
         (let [a (aget acc i)]
           (when-not (reduced? a)
             (aset all-reduced? 0 false)
             (aset acc i ((aget rfns i) a x)))))
       (if (aget all-reduced? 0) (reduced acc) acc)))))

(defn juxt
  "Like `juxt*`, but accepts `rfns` as varargs."
  [& rfns]
  (juxt* rfns))

(defn facet [rf fns]
  (->> (map (fn [f] (pre-step rf f)) fns)
       (apply juxt)))

(defn fuse [kvs]
  (post-complete (juxt* (vals kvs))
                 (fn [acc]
                   (zipmap (keys kvs) acc))))

(defn fuse-matrix [rf kvs]
  (-> (fuse (->> (pairs (keys kvs))
                 (map (fn [[k1 k2]]
                        [[k1 k2] (rf #(get % k1) #(get % k2))]))
                 (into {})))
      (pre-step (project kvs))
      (post-complete complete-triangular-matrix)))
