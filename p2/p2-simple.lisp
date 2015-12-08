;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; p2-simple.lisp - default (and rather dumb) Coup player for
;; CMSC471 project. Will select relatively arbitrary actions.
;; Coups if 7 coins available, will otherwise select a random action
;;
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

(defpackage :p2)
(in-package :p2)

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

