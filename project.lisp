;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; project.lisp - default (and rather dumb) Coup player for
;; CMSC471 project. Will select relatively arbitrary actions.
;; Coups if 7 coins available, will otherwise select a random action
;;
;; Adapted from:
;; (c) Shawn Squire 2015
;; Version 1.0 - Distributed 10/6/2015
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PACKAGE DEFINITION
;; You MUST include these two lines, with YOUR OWN TEAM NAME
;; for the defined package.  All of your code "lives" in this
;; package, and it must be the same as the team name that you
;; will use for the tournament!!

(defpackage :pro_ject)
(in-package :pro_ject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; IMPORTS - symbols (function names) to import from the main
;; game engine package.  Add imports of utility functions here
;; if you choose to use any.
;;

(import '(coup::Moves
					coup::Characters
					coup::CardsPerCharacter coup::CardsPerPlayer
					coup::game-players coup::game-rounds
					coup::player-name coup::player-hand coup::player-faceup
					coup::player-exchange coup::player-handcount
					coup::player-numrounds coup::player-coins
					coup::card-move coup::card-block coup::move-card coup::block-cards
					))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REQUIRED FUNCTIONS

(defun perform-move (player game)
	; Play as the duke on the first round
	(if (= (game-rounds game) 1)
		'(coup::Tax)

		; If we get to 7, coup someone
		(if (< (player-coins player) 7)

			; If we are the duke, play as the duke
			(if (eq 'coup::Duke (find 'coup::Duke (player-hand player)))
				'(coup::Tax)
				'(coup::Income)
			)
			'(coup::Coup)
		)
	)
)

(defun reveal-card (player game)
	(+ (random 2) 1))

(defun select-exchange (player game)
	'((1 . 1) (2 . 2)))

(defun block-move (move player game source &optional target))

(defun challenge-card (card player game source &optional target))

(defun event (e game arguments)
	(cond
		((string= e "MOVE") (case (car arguments)
							('Income "Player (cadr arguments) is using income")
							('ForeignAid "Player (cadr arguments) is using foreign aid")
							('Coup "Player (cadr arguments) is couping (caddr arguments)!")
							('Tax "Player (cadr arguments) is using tax")
							('Assassinate "Player (cadr arguments) is assassinating (caddr arguments)")
							('Exchange "Player (cadr arguments) is using exchange")
							('Steal "Player (cadr arguments) is stealing from (caddr arguments)!")))
		((string= e "SHUFFLE") "Deck is shuffled")
		((string= e "REVEAL") "(car arguments) has shown they have (cadr arguments)")
		((string= e "ELIMINATED") "(car arguments) is totally out!")
		((string= e "CHALLENGE-LOST") "(car arguments) lost that challenge against (cadr arguments) having a (caddr arguments)")
		((string= e "CHALLENGE-WON") "(car arguments) won that challenge against (cadr arguments) having a (caddr arguments)")
		((string= e "BLOCK") "(car arguments) blocked (cadr arguments) (caddr arguments) using their (cadddr arguments)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MEMORY FUNCTIONS

; Keeps track of the players, what cards they have played,
; and the probability that they actually have that card.
; The data structure format is as follows:
#|
'(
	(p1 ( (Duke . 0.5) (Contessa . 0.4) ) )
	(p2 ( (Captain . 0.17) (Ambassador . 0.5) ) )
)
players = [
	p1: [ (Duke, 0.5), (Contessa, 0.4)],
	p2: [ (Captain, 0.17), (Ambassador, 0.5)]
]
|#
(setq players nil)


; Updates the card's probability of occuring with the new value.
; If the hand is empty, then the player, card, and value are added.
; If the card is not in the hand, then the card and value are added.
; If the card is already in the hand, then the current value is replaced.
(defun updateCardProbability(playerName card value)

  ; Get the player's possible cards & probabilities
  (setq player (assoc playerName players))

  ; If the player's hand is empty, add the card and its value
  (if (null player)
    (setq players (append players (list (list playerName (cons card 1))))) ; 1 -> value

    ; Add the card and its value or Update the card's value
    (progn
      ; Gets the card and probability of the card they just played (Duke . 0.4)
      (setq cardFreq (assoc card (cdr player)))

      ; If the card is not in their possible hand, add it.
      ; If the card is in their possible hand, update the value
      (if (not (null cardFreq))
        ; Updates the players probability of having that card
        (setf (cdr cardFreq) (+ (cdr cardFreq) 1) ; (+ (cdr cardFreq) value)) -> value

        ; Add the card to their possible hand
        (progn
          (setq players (remove player players))
          (setq player (append player (list (cons card 1)))) ; 1 -> value
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





; Probably need the number of times the player said they are that character and
; the probability that they are that player

; Could add memory for the cards in the deck





