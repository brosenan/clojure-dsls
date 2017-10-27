(ns dsls.core-test
  (:require [midje.sweet :refer :all]
            [dsls.core :as d]))
(fact
 (d/eval 3) => 3
 (d/eval [+ 2 3 4]) => 9
 (d/eval [d/if [> 2 1] "foo" "bar"]) => "foo"
 (d/eval [d/if [> 1 2] "foo" "bar"]) => "bar"
 (d/eval [d/fact 5]) => 120)

(fact
 (d/in #{1 2 3} 2) => true
 (d/in #{1 2 3} 4) => false
 (set (d/enum #{1 2 3})) => #{1 2 3}
 (d/in [d/intersect #{1}  #{3}] 2) => false
 (d/enum [d/intersect #{1 2} #{2 3}]) => [2]
 (d/enum [d/intersect #{1 2} #{2 3} #{1 3}]) => []
 (d/in sequential? [1 2 3]) => true
 (set (d/enum [d/intersect (set (range 100)) odd?])) => (set (->> (range 100)
                                                                  (filter odd?)))
 (d/in [d/universe] "foo") => true
 (d/enum [d/intersect #{1}]) => [1])

(defn factorial [n]
  (cond (<= n 1) 1
        :else (* n (factorial (dec n)))))

(fact ;; two orders of magnitude difference in performance
 (time (d/eval [d/fact 150.0]))
 (time (d/eval [d/fact 150.0]))
 (time (d/eval [d/fact 150.0]))
 (time (factorial 150.0))
 (time (factorial 150.0))
 (time (factorial 150.0)))
