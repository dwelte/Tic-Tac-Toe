(ns tic.core
  (:use seesaw.core))

(defmacro debug [& stmts]
  (let [has-message (string? (first stmts))
        message (if has-message (list (first stmts)) '())
        exp (if has-message (rest stmts) stmts)]
   `(let [val# (~@exp)] (println ~@message val#) val#)))

(defn make-state [] [[[:e :e :e] [:e :e :e] [:e :e :e]] :start])

(defn board-status [board]
  (let [
    lines [
      [[0 0] [0 1] [0 2]]
      [[1 0] [1 1] [1 2]]
      [[2 0] [2 1] [2 2]]
      [[0 0] [1 0] [2 0]]
      [[0 1] [1 1] [2 1]]
      [[0 2] [1 2] [2 2]]
      [[0 0] [1 1] [2 2]]
      [[0 2] [1 1] [2 0]]]
    get-line (fn [board line]
      (let [vals (map #(get-in board %) line)]
        (if (apply = vals)
          (if (= (first vals) :e)
            nil
            (first vals))
          nil)))
    winner (some identity (map #(get-line board %) lines))
    full? (every? #(not= :e %) (flatten board))]
    (cond
      (= winner :x) :xwins
      (= winner :o) :owins
      full? :tie
      :else :success)))

(defn apply-move [board [a b]]
  (let [count-player #(count (filter #{%} (flatten board)))
        xcount (count-player :x)
        ocount (count-player :o)
        player (if (< ocount xcount) :o :x)
        new-board (assoc-in board [a b] player)]
    [new-board (board-status new-board)]))

(defn play-move [board [a b]]
  (cond
    (#{:xwins :owins :tie} (board-status board)) [board :completed]
    (not= :e (get-in board [a b])) [board :invalid]
    :else (apply-move board [a b])))

(defn previous-token [board]
  (let [tcount #(count (filter #{%} (flatten board)))]
    (if (= (tcount :x) (tcount :o)) :o :x)))

(defn play-a-game
  ([xplayer oplayer viewers]
   (let [[next-board outcome :as next-state] (make-state)]
     (doseq [viewer viewers] (viewer nil nil outcome next-board))
     (play-a-game xplayer oplayer viewers next-board)))
  ([current-player next-player viewers current-board]
   (let [move (current-player current-board)
         [next-board outcome :as next-state] (play-move current-board move)]
     (doseq [viewer viewers] (viewer (previous-token next-board) move outcome next-board))
     (cond
       (#{:success :start} outcome) (recur next-player current-player viewers next-board)
       (= outcome :invalid) (recur current-player next-player viewers next-board)
       :else (do (Thread/sleep 2000) outcome)))))

(defn print-board [board]
  (let [str-row (fn [row] (apply str (interpose \| (map {:x \x, :o \o, :e \space} row))))
        lines (interpose "-+-+-" (map str-row board))]
    (doall (map println lines))))

(defn make-cl-player [player-name]
  (fn [board]
    (println "Player" player-name "please enter a move.")
    (let [move (read-line)
          digits (map #(Long/parseLong %) (re-seq #"[012]" move))]
      (if (not= 2 (count digits))
        (do
          (println "Invalid input, please enter pair of numbers between 0 and 2 inclusive.")
          (println)
          (recur board))
        (do
          (println)
          (vec digits))))))

(defn get-empty-positions [board]
  (for [a [0 1 2]
        b [0 1 2]
        :let [pos [a b]]
        :when (= :e (get-in board pos))]
    pos))

(defn make-random-ai []
  (fn [board]
    (rand-nth (get-empty-positions board))))

(defn score-board [board token]
  (let [outcome (board-status board)
        possible-boards (map #(assoc-in board % token) (get-empty-positions board))
        flipped-token (if (= token :x) :o :x)
        flip-score #(- 2 %)
        scorer #(if (= token :x) % (flip-score %))
        opponent-score-board #(flip-score (score-board % flipped-token))]
    (cond
      (= outcome :xwins) (scorer 2)
      (= outcome :owins) (scorer 0)
      (= outcome :tie) 1
      :else (apply max (map opponent-score-board possible-boards)))))

(defn make-perfect-ai [token]
  (fn [board]
    (let [possible-pos (get-empty-positions board)
          flipped-token (if (= token :x) :o :x)
          make-score-to-pos (fn [p] {(- 2 (score-board (assoc-in board p token) flipped-token)) [p]})
          score-position-maps (map make-score-to-pos possible-pos)
          scores (apply merge-with concat score-position-maps)
          max-positions (scores (apply max (keys scores)))]
      (rand-nth max-positions))))

(defn print-board-and-outcome  [token play outcome board]
  (print-board board)
  (cond
    (= outcome :invalid) (println "Move was on an occupied space.  Please choose an empty space.")
    (= outcome :xwins) (println "X wins!")
    (= outcome :owins) (println "O wins!")
    (= outcome :tie) (println "It's a tie")
    :else nil))

(defn make-one-player-cl-viewer [player-token]
  (fn [token play outcome board]
    (when-not (and (= token player-token) (= outcome :success))
      (print-board-and-outcome token play outcome board))))

(def router-destination (ref nil))

(defn set-destination [f] (dosync (ref-set router-destination f)))

(defn router [x] (dosync (when-let [f (deref router-destination)] (ref-set router-destination nil) (f x))))

(defn select-mode []
  (let [return-promise (promise)
        select-frame (frame :title "Tic-Tac-Toe")
        make-return-function (fn [value] (fn [e] (dispose! select-frame) (deliver return-promise value)))
        make-button (fn [message value] (let [b (button :text message)] (listen b :action (make-return-function value)) b))
        perfect-button (make-button "Perfect AI" :perfect)
        random-button (make-button "Random AI" :random)
        two-player-button (make-button "Two Player" :two)
        exit-button (make-button "Exit" :exit)
        grid (grid-panel :rows 1 :columns 4 :items [perfect-button random-button two-player-button exit-button])]
    (-> select-frame
      (config! :content grid)
      pack!
      show!)
    @return-promise))

(defn make-graphic-player-and-viewer []
  (let [make-box (fn [] (button :text " " :font "ARIAL-BOLD-42"))
        make-row (fn [] [(make-box) (make-box) (make-box)])
        boxes [(make-row) (make-row) (make-row)]
        j (doall (for [a (range 3) b (range 3) :let [pos [a b]]] (listen (get-in boxes pos) :action (fn [e] (router pos))))) 
        box (get-in boxes [1 1])
        grid (grid-panel :rows 3
                         :columns 3
                         :hgap 0
                         :vgap 0
                         :items (flatten boxes))
        message (label :text "Player X please enter a move" :halign :center)
        outer (top-bottom-split grid message)
        viewer (fn [token play outcome board]
                 (config! message :text
                   (case outcome
                     :xwins "X wins!"
                     :owins "O wins!"
                     :tie "It's a tie."
                     (str "Player " ({:x "O" :o "X"} token "X")  " please enter a move")))
                 (doall (map (fn [b v] (config! b :text ({:x "X" :o "O" :e " "} v))) (flatten boxes) (flatten board))))
        player (fn [board]
                 (let [result (promise)]
                   (set-destination #(deliver result %))
                   @result))]
    (native!)
    (-> (frame :title "Hello" 
               :content outer
               :on-close :dispose)
      pack!
      show!)
    [player viewer]))

(defn make-two-player-cl-viewer [] print-board-and-outcome)

(defn make-graphical-player-vs-rand []
  (let [[xplayer viewer] (make-graphic-player-and-viewer)]
    [xplayer (make-random-ai) [viewer]]))

(defn make-graphical-player-vs-perfect []
  (let [[xplayer viewer] (make-graphic-player-and-viewer)]
    [xplayer (make-perfect-ai :o) [viewer]]))

(defn make-graphical-player-vs-player []
  (let [[player viewer] (make-graphic-player-and-viewer)]
    [player player [viewer]]))

(defn choose-players []
  (println "Choose play mode")
  (println "a - Against random AI")
  (println "b - Against perfect AI")
  (println "c - Against a human")
  (if-let [choice (re-find #"[abc]" (read-line))]
    (do (println) (cond
                    (= choice "a") (make-graphical-player-vs-rand) 
                    (= choice "b") (make-graphical-player-vs-perfect)
                    :else          (make-graphical-player-vs-player)))
    (do (println "Illegal choice.  Please choose a or b.") (println) (recur))))

(defn choose-players-graphical [mode]
  (case mode
    :perfect (make-graphical-player-vs-perfect)
    :random (make-graphical-player-vs-rand)
    :two (make-graphical-player-vs-player)
    nil))

(defn -main [& args]
  (let [mode (select-mode)]
    (if (not= mode :exit)
      (let [[xplayer oplayer viewers] (choose-players-graphical mode)]
        (play-a-game xplayer oplayer viewers)
        (recur args))
      (System/exit 0))))


