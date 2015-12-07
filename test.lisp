
;(setq players '())

;(setq players (append players '(p1)))

#|list( cons( player, list( cons(card, prob))  )  )

players = [
  p1: [ (Duke, 0.5), (Contessa, 0.4)],
  p2: [ (Captain, 0.17), (Ambassador, 0.5)]
]

players = [p1, p2, p3]
hands = [
[ (Duke, 0.5), (Contessa, 0.4)],
[ (Captain, 0.17), (Ambassador, 0.5)]
]

(defun positionFunc()
  (setq players '(p1 p2 p3))
  (setq hands '(
	       ((Duke . 4) (Contessa . 1))
	        ((Captain . 2) (Duke . 1))
  	      ((Ambassador . 3) (Assassin . 2))))


  (print players)


  (setq playerIndex (position 'p2 players))
  (print playerIndex)
  (setq playerCards (nth (position 'p2 players) hands))
  (print playerCards)

  (setq cardFreq (position 'Duke playerCards))
  (print cardFreq)


  (if (null cardFreq)
      (append playerCards '(Duke . 1))
      (nth cardFreq playerCards)
  )

  (print (assoc 'p1 '((p2 . 3) (p1 . 2) (p3 . 4))))
)

(defun get-element (list name)
    (cadr (assoc name list :test #'string=)))
|#



; Updates the card's probability of occuring with the new value.
; If the hand is empty, then the player, card, and value are added.
; If the card is not in the hand, then the card and value are added.
; If the card is already in the hand, then the current value is replaced.
(defun updateCardProbability(playerName card value)

  ; Get the player's possible cards & probabilities
  (setq player (assoc playerName players))

  ; If the player's hand is empty, add the card and its value
  (if (null player)
    (setq players (append players (list (list playerName (cons card value)))))

    ; Add the card and its value or Update the card's value
    (progn
      ; Gets the card and probability of the card they just played (Duke . 0.4)
      (setq cardFreq (assoc card (cdr player)))

      ; If the card is not in their possible hand, add it.
      ; If the card is in their possible hand, update the value
      (if (not (null cardFreq))
        ; Updates the players probability of having that card
        (setf (cdr cardFreq) value) ; (+ (cdr cardFreq) value))

        ; Add the card to their possible hand
        (progn
          (setq players (remove player players))
          (setq player (append player (list (cons card value))))
          (setq players (append players (list player)))
        )
      )
    )
  )

  ; Sorts the cards for that player by their occurence
  (sort (cdr player) #'sortCardsByMaxOccurence)
)

; Sorts the list of cards by their occurence. For example:
; Input: '((Duke . 10) (Assassin . 5) (Contessa . 15))
; Output: '((Contessa . 15) (Duke . 10) (Assassin . 5))
(defun sortCardsByMaxOccurence(a b)
  (> (cdr a) (cdr b))
)


(setq players nil)
(updateCardProbability 'p1 'contessa 10)
(format t "~a~%" players)
(updateCardProbability 'p1 'duke 15)
(format t "~a~%" players)
(updateCardProbability 'p1 'ambassador 20)
(format t "~a~%" players)

(updateCardProbability 'p2 'captain 30)
(format t "~a~%" players)
(updateCardProbability 'p2 'captain 300)
(format t "~a~%" players)

(updateCardProbability 'p3 'ambassador 25)
(format t "~a~%" players)
(updateCardProbability 'p3 'assassin 76)
(format t "~a~%" players)
(updateCardProbability 'p3 'captain 34)
(format t "~a~%" players)





(format t "~%~%~a~%~%" (sort '((duke . 7) (contessa . 5) (assassin . 10)) #'sortCardsByMaxOccurence))

(format t "Sorting: ~a~%" (assoc 'p1 players))
(print (sort (cdr (assoc 'p1 players)) #'sortCardsByMaxOccurence))
(print players)






















