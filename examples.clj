(ns clojure-syntax)
(println "Hello, World")
(println "Hello, World") (println "Hello, World")

(ns math-operations)
(println (+ 5 (* 5 (- 7 2 1))))
(println (+ 1 2 3))
(println (= (- 10 (* 2 3) 4)))
;--(println(= [1 2 3 4] (conj '(2 3 4) 1)))
(println (conj '(3 4) 2 1))
(println (= [1 2 3 4] (conj '(2 3 4) 1)))

(ns let-fn-example)
(let [width     10
      height    20
      thickness 2]
  (println "hello from inside the `let`.")
  (println * width height thickness)
  (println (let [x 2
                 x (* x x)
                 x (+ x 1)]
             x)))

(ns scalars-example)
(println "Scalars: \n")
(println "Type of 1 is: " (type 1) "\n")
(println "Type of 1.2 is: " (type 1.2) "\n")
(println "Type of 1N is: " (type 1N) "\n")
(println "Type of 1.5M is: " (type 1.5M) "\n")
(println "Type of 1.2e4 is: " (type 1.2e4) "\n")
(println "Type of 1/3 is: " (type 1/3) "\n")
(println "Type of \"HEY!\" is: " (type "HEY!") "\n")
(println "Type of \\a is: " (type \a) "\n")
(println "Type of :my-keyword is: " (type :my-keyword) "\n")
(println "Type of 'my-s is: " (type 'my-s) "\n")

(ns data-structures-example)
;; Vectors
(println (def v [:a :b :c]))
(println (nth v 1))             ; ⇒ :b
(println (v 1))                 ; ⇒ :b  (same)
(println (first v))             ; ⇒ :a
(println (rest v))              ; ⇒ (:b :c)
(println (next v))              ; ⇒ (:b :c)
(println (last v))              ; ⇒ :c
;; Lists
;; Same as vectors, but can't index.
;; Maps
(println (def m {:a 1 :b 2}))
(println (get m :a))            ; ⇒ 1
(println (m :a))                ; ⇒ 1       (same)
(println (:a m))                ; ⇒ 1       (same!)
(println (get m :x 44))         ; ⇒ 44      (if no :x, 44 is the default)
(println (keys m))              ; ⇒ (:a :b)
(println (vals m))              ; ⇒ (1 2)
;; Grab a key or a val from a single map entry:
(println (key (first m)))       ; ⇒ :a
(println (val (first m)))       ; ⇒ 1
;; Of course, note that maps are not ordered.
;; Sets
(println (def s #{:a :b :c}))
(println (s :a))                ; ⇒ :a
(println (s :z))                ; ⇒ nil
(println (clojure.set/union #{:a :b :c} #{:b :c :d}))
(println ((hash-map :a 10, :b 20, :c 30) :b))

(ns data-structures-example2)
;; Vectors
(println (def v   [:a :b :c]))
(println (def li '(:a :b :c)))
(println (conj v  :d))          ; ⇒ [:a :b :c :d]
(println (conj li :d))          ; ⇒ (:d :a :b :c)
(println v)   ; ⇒ is still [:a :b :c]
(println li)  ; ⇒ is still (:a :b :c)
;; Maps
(println (def m {:a 1 :b 2}))
(println (assoc m :c 3))        ; ⇒ {:a 1 :c 3 :b 2}
(println (dissoc m :b))         ; ⇒ {:a 1}
(println m)   ; ⇒ is still {:a 1 :b 2}
;; Sets
(println (def s #{:a :b}))
(println (conj s :c))           ; ⇒ #{:a :c :b}
(println (disj s :a))           ; ⇒ #{:b}
(println s)   ; ⇒ is still #{:a :b}

(ns string-example
(:require [clojure.string :as str]))
(println (str/join " :caraio " [1 2 3]))

(ns control-structures)
(def my-stuff [1 2 3])
(println (if (seq my-stuff)
           "still has stuff left"
           "all gone"))

(defn my-subtraction
  "Docstring goes here."
  [a b]
  (println "adding them!")
  (- a b))
(println (my-subtraction 10 20))   ; Returns/evaluates-to 30
(defn my-multi
  "Docstring goes here."
  [a b]
  (println "adding them!")
  (* a b))
(println (my-multi 10 20))
(println (- 10 (* 2 3)))
(println (map #(+ % 5) '(1 2 3)))
(println (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))
(println ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
(println (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last)))
(println (loop [x 5
                result []]
           (if (> x 0)
             (recur (dec x) (conj result (+ 2 x)))
             result)))
(println (take 3 (drop 2 [2 5 4 1 3 6])))
(println (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
(take 4 [2 5 4 1 3 6])


;1.Enter only enough to fill in the blank (in this case, a boolean true/false) - do not retype the whole problem.
;(= #_blank true)
(ns nothing-but-the-truth)
(= true true)

;2 Enter only enough to fill in the blank (in this case, a single number) - do not retype the whole problem.
;(= (- 10 (* 2 3)) #_blank)
(ns simple-math)
4

;3 Enter only enough to fill in the blank (in this case, a string) - do not retype the whole problem.
;(= #_blank (.toUpperCase "hello world"))
(ns intro-to-strings)
"HELLO WORLD"

;4 (= #_blank '(:a :b :c))
(ns intro-to-lists)
[:a :b :c]

;5 (= #_blank (conj '(2 3 4) 1))
; (= #_blank (conj '(3 4) 2 1))
(ns lists-conj)
[1 2 3 4]
[1 2 3 4]

; 6 (= #_blank (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))
(ns intro-to-vectors)
[:a :b :c]

; 7 (= #_blank (conj [1 2 3] 4))
; (= #_blank (conj [1 2] 3 4))
(ns vector-conj)
[1 2 3 4]
[1 2 3 4]

;8 (= #_blank (set '(:a :a :b :c :c :c :c :d :d)))
; (= #_blank (clojure.set/union #{:a :b :c} #{:b :c :d}))
(ns intro-to-sets)
#{:a :b :c :d}

;9 (= #{1 2 3 4} (conj #{1 4 3} #_blank))
(ns sets-conj)
2

;10 (= #_blank ((hash-map :a 10, :b 20, :c 30) :b))
; (= #_blank (:b {:a 10, :b 20, :c 30}))
(ns intro-to-maps)
20

;11 (= {:a 1, :b 2, :c 3} (conj {:a 1} #_blank [:c 3]))
(ns maps-conj)
[:b 2]

;12 (= #_blank (first '(3 2 1)))
;(= #_blank (second [2 3 4]))
;(= #_blank (last (list 1 2 3)))
(ns intro-to-seqs)
3

;13 (= #_blank (rest [10 20 30 40]))
(ns sequences-rest)
[20 30 40]

;14 (= #_blank ((fn add-five [x] (+ x 5)) 3))
; (= #_blank ((fn [x] (+ x 5)) 3))
; (= #_blank (#(+ % 5) 3))
; (= #_blank ((partial + 5) 3))
(ns intro-to-functions)
((defn add-five [x] (+ x 5)) 3)

;15 (= 4 (#_blank 2)) (= 6 (#_blank 3)) (= 22 (#_blank 11)) (= 14 (#_blank 7))
(ns double-down)
(fn [arg-1] (+ arg-1 arg-1))

;16 (= (#_blank "Dave") "Hello, Dave!") (= (#_blank "Jenn") "Hello, Jenn!") (= (#_blank "Rhea") "Hello, Rhea!")
(ns hello-world)
(fn [name]  (str "Hello, " name "!"))

; 17 (= #_blank (map #(+ % 5) '(1 2 3)))
(ns sequences-map)
'(6 7 8)

;18 (= #_blank (filter #(> % 5) '(3 4 5 6 7)))
(ns sequences-filter)
'(6 7)

;19 (= #_blank (let [x 5] (+ 2 x))) (= #_blank (let [x 3, y 10] (- y x))) (= #_blank (let [x 21] (let [y 3] (/ x y))))
(ns local-bindings)
7

;20 (= #_blank (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))
(ns regular-expressions)
"ABC"

;21 (= 15 (reduce #_blank [1 2 3 4 5])) (=  0 (reduce #_blank [])) (=  6 (reduce #_blank 1 [2 3]))
(ns intro-to-reduce)
+

;22 (= #_blank ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
(ns simple-recursion)
'(5 4 3 2 1)

;23 (= (#_blank (sort (rest (reverse [2 5 4 1 3 6]))))(-> [2 5 4 1 3 6] (reverse) (rest) (sort) (#_blank))5)
(ns rearranging-code)
last

;24 (= #_blank (loop [x 5 result []] (if (> x 0) (recur (dec x) (conj result (+ 2 x))) result)))
(ns recurring-theme)
[7 6 5 4 3]

;25 (= (#_blank + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
;    (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (#_blank +))
;    11)
(ns rearranging-code-2)
reduce

;26 (true?  (#_blank :a {:a nil :b 2})) (false? (#_blank :b {:a nil :b 2})) (false? (#_blank :c {:a nil :b 2}))
(ns a-nil-key)
(fn [key coll] (and (contains? coll key) (nil? (get coll key))))

;27 (= #_blank (for [x (range 40)
;                     :when (= 1 (rem x 4))]
;                 x))
(ns for-the-win)
(for [x (iterate #(+ 4 %) 1)
      :while (< x 40)]
  x)

;28 (= #_blank (if-not false 1 0)) (= #_blank (if-not nil 1 0))
(ns logical-falsity-and-truth)
1

;29 (clojure.set/superset? #_blank #{2}) (clojure.set/subset? #{1} #_blank) (clojure.set/superset? #_blank #{1 2})
(ns subset-and-superset)
#{1 2}

;30 (= (let [[a b c d e] [0 1 2 3 4]] [c e]) #_blank)
(ns intro-to-destructuring)
[2 4]

;31 (= (#_blank 0 [:a :b :c]) {:a 0 :b 0 :c 0}) (= (#_blank "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})  (= (#_blank [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})
(ns map-defaults)
;;Write your code under this comment, it will fill the blank.
;(fn [x y] mapcat #((vector x) y))
;(= 2 (:foo {:bar 0, :baz 1} 2))
;(map #(str "Hello " % "!" ) ["Ford" "Arthur" "Tricia"])
;((fn [x y] (map #(vector x %)y))0 [:a :b :c])
;((fn [x key y] (zipmap keys y x))0 [:a :b :c])
;(hash map "x" [1 2 3] ) 
(map (zipmap [:a :b :c] (repeat [:foo :bar])))
;(fn [x y](zipmap y (repeat x)))

;32 (= #_blank (some #{2 7 6} [5 6 7 8])) (= #_blank (some #(when (even? %) %) [5 6 7 8]))
(ns intro-to-some)
6

;2 (= (my-last [1 2 3 4 5]) 5) (= (my-last '(5 4 3)) 3)
(ns last-element)
;(defn my-last [x] (first(reverse(x))))
;(peek ["b" "c" "d"])
;(first(reverse["b" "c" "d"]))
(defn my-last [x] (nth x (- (count x) 1)))

;3 (= (second-last (list 1 2 3 4 5)) 4) (= (second-last ["a" "b" "c"]) "b") (= (second-last [[1 2] [3 4]]) [1 2])
(ns penultimate-element)
;(defn second-last  [x] (nth x (- (count x) 2)))
;(defn second-last  [list_nums n] (last (take (inc n) list_nums)))
;((fn [list_nums n] (last (take(inc n)list_nums)))["a" "b" "c"] "b")
;((fn [list_nums n] (last(take (+ n 1) list_nums)))["a" "b" "c"] "b")
;((fn [list_nums n] (first (drop n list_nums)))(list 1 2 3 4 5) 4)
(defn second-last [s x]
  (last (take (inc x) s)))

;4 (= (my-nth '(4 5 6 7) 2) 6) (= (my-nth [:a :b :c] 0) :a)
(ns nth-element)
(defn my-nth [s x]
  (last (take (inc x) s)))

;5 (= (count-seq '(1 2 3 3 1)) 5) (= (count-seq "Hello World") 11) (= (count-seq [[1 2] [3 4] [5 6]]) 3)
(ns count-a-sequence)
(defn count-seq
  [list]
  (if (empty? list) 0
      (+ 1 (count-seq (rest list)))))

;6 (= (sum [1 2 3]) 6) (= (sum (list 0 -2 5 5)) 8) (= (sum #{1 4 2}) 7)
(ns sum-it-all-up)
(defn sum 
 [list]
  (reduce + list))

;7 (= (just-odds #{1 2 3 4 5}) '(1 3 5)) (= (just-odds [4 2 1 6]) '(1))
(ns find-the-odd-numbers)

(defn just-odds [input]
  (def output [])
  (loop [x input]
    (if (not= (count x) 0)
      (do
        (if (> (mod (first x) 2) 0)
          (do
            (def output (conj output (first x)))))
        (println x)
        (recur (rest x)))))
  output)

(just-odds #{1 2 3 4 5})





