(ns tic.test.core
  (:use [tic.core])
  (:use [clojure.test]))

(defn make-dummy-player [moves]
  (let [remaining-moves (ref moves)]
    (fn [state]
      (dosync
        (let [move (first @remaining-moves)]
          (alter remaining-moves rest)
          move)))))

(deftest t-make-state
  (is (= [[[:e :e :e] [:e :e :e] [:e :e :e]] :success] (make-state))))

(deftest t-play-valid-move
  (is (= [[[:e :x :e] [:e :e :e] [:e :e :e]] :success] (play-move [[:e :e :e] [:e :e :e] [:e :e :e]] [0 1]))))

(deftest t-play-invalid-move
  (is (= [[[:e :x :e] [:e :e :e] [:e :e :e]] :invalid] (play-move [[:e :x :e] [:e :e :e] [:e :e :e]]  [0 1]))))

(deftest t-play-winning-move-x
  (is (= [[[:x :x :x] [:o :o :e] [:e :e :e]] :xwins] (play-move [[:x :x :e] [:o :o :e] [:e :e :e]] [0 2]))))

(deftest t-play-winning-move-o
  (is (= [[[:x :x :e] [:o :o :o] [:x :e :e]] :owins] (play-move [[:x :x :e] [:o :o :e] [:x :e :e]] [1 2]))))

(deftest t-try-to-play-on-won-board
  (is (= [[[:x :x :e] [:o :o :o] [:x :e :e]] :completed] (play-move [[:x :x :e] [:o :o :o] [:x :e :e]] [0 2]))))

(deftest t-run-canned-play
  (is (=
    [[[:x :x :x] [:o :o :e] [:e :e :e]] :xwins]
    (play-a-game (make-dummy-player [[0 0] [0 1] [0 2]]) (make-dummy-player [[1 0] [1 1] [1 2]]) []))))

(deftest t-score-won-board
  (is (= 2 (score-board [[:x :x :x] [:o :o :e] [:o :e :e]] :x))))

(deftest t-score-lost-board
  (is (= 0 (score-board [[:x :x :x] [:o :o :e] [:e :e :e]] :o))))

(deftest t-score-tie-board
  (is (= 1 (score-board [[:x :x :o] [:o :o :x] [:x :o :x]] :o))))

(deftest t-score-about-to-win-board
  (is (= 2 (score-board [[:x :x :e] [:o :o :e] [:e :e :e]] :x))))

(deftest t-score-should-tie-board
  (is (= 1 (score-board [[:e :e :e] [:e :x :e] [:e :e :e]] :o))))

(deftest t-score-should-win-board
  (is (= 2 (score-board [[:e :e :e] [:o :x :e] [:e :e :e]] :x))))

(deftest t-play-perfect-players-from-start
  (is (= :tie (second (play-a-game (make-perfect-ai :x) (make-perfect-ai :o) [])))))

(deftest t-play-perfect-players-from-tieable (is true))

(deftest t-play-perfect-players-from-winnable (is true))

