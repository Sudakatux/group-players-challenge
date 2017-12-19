(ns player-exercise.core)

; Total number of score cant exceed 100 by player
; team cant exceed 175
; cant repeat total score attributes

(def maxTeamPoints 175)
(def amountOfPlayers 10)
(def maxPlayerPoints 100)
(def amountOfLetters 3)
(def withDecimals? false)

(defn myRandom
  [arg]
  (if withDecimals?
    (rand arg)
    (rand-int arg)
  )
)


(defn sumplayer
  [player]
  (let [speed (:speed player)
        agility (:agility player)
        strength (:strength player)]
        (+ speed agility strength)
  )
)

(defn conditionalRandom
  "Will return a random number following the conditions"
  [maxNum conditions]
  (loop [randomNumber (myRandom maxNum)]
    (if (every? true? (map #(% randomNumber) conditions))
      randomNumber
      (recur (myRandom maxNum))
    )
  )
)

(defn containsElement
  "Will return true if list contains element"
  [elem collection]
  (some #(= elem %) collection)
)

(defn randomSeq
  "Generate a random sequence, given a number an amount of elements to generate an a condition"
  [max amount condition?]
  (loop [ maxNum max
          finalSeq []] ; TODO missing adding to sequence 
      (if  (or (= (count finalSeq) amount) (<= maxNum 0)) ;If we have the amount
        finalSeq ; Return sequence
        (let [divided (int (Math/ceil (/ maxNum ( - amount (count finalSeq))))) 
          newNumber ( conditionalRandom divided ; We want to find a number where the max possible number is less or equal than
                        [ #(not= % 0) ;Is not zero
                          #(not (containsElement % finalSeq)) ;Is not part of our current list totalScore
                          #(condition? %)
                        ]
                    )
          ]
            (recur (- max (+ (reduce + finalSeq) newNumber))
              (into finalSeq (concat [newNumber]))
            )
        )
      )
  )   
)


(defn assignPoints
  "Simple point assignment to symbols"
  [points]
  (zipmap [:speed :agility :strength] (vec points))
)

(defn calculateName
  "Will generate a name for the player"
  [idx]
    (let [prefix (apply str (flatten (take amountOfLetters (partition 1 (seq "abcdefghijklmnopqrstuwvxyz")))))]
      (str prefix (inc idx))
    )
)

(defn sums-to 
  "Returns a list of sequence of every possible value that sums to total"
  [n total]
  (case n
    1 [[total]]
    (for [i (range (inc total))
          solution (sums-to (dec n) (- total i))]
      (cons i solution))))

(defn assignPlayerAttributes
 "Given a list of max player points, Will create players"
 [number points]
  (
    (comp 
      (fn [elm]
              (assoc elm :name (calculateName number))
      ) 
      assignPoints 
      (fn [el] (flatten (rand-nth (filter #(not (containsElement 0 %)) (sums-to 3 el)))))
    ) 
    points)
)

(defn createPlayer
  "Will convert a list of points to a list of players"
  [playerPoints]
    (map-indexed (fn [idx points]
        ( assignPlayerAttributes idx points)
      )
      playerPoints) 
)
  

; (randomSeq 175 5 (fn [n] (< n 100)) Generates amount for each player
;(randomSeq 175 5 (fn [_] true)) Generates amount of 
(defn generate
  "Main function will generate the team"
  []
  (let [maxPlayerPoints 
          (randomSeq maxTeamPoints amountOfPlayers #(> (/ % 3) 1))
        ] ; Generate groups that sum less than 175 each group summing less than 100
    (createPlayer maxPlayerPoints)
  )
)

(defn -main 
  []
  (generate)
)
