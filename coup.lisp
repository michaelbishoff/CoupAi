;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; coup.lisp - game engine for CMSC 471 project
;; (c) Shawn Squire 2015
;; Version 1.0 - Distributed 10/6/2015
;; Version 1.1 - Distributed 10/9/2015 -- Critical argument fix
;; Version 1.2 - Distributed 10/31/2015 -- Added events
;;
;; In the tournament, the game will be played with 3 players

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LISP CONFIGURATION
;; This prevents Lisp from complaining when the file is reloaded
(setf *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* t)

(defpackage :coup)
(in-package :coup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CONSTANT DEFINITIONS
;;

;; Create a new random state to vary up runs
(setf *random-state* (make-random-state t))

;; Will be set to current game; useful for debugging and post-game analysis
(defvar *GAME* nil)

;; Maximum number of rounds to run reasonably before calling the end of game
(defconstant MaxRounds 100)

;; List of possible moves by any player (although they may have to bluff)
(defconstant Moves '(Income ForeignAid Coup Tax Assassinate Exchange Steal))
;; If no move is supplied, what to do instead
(defconstant DefaultMove 'Income)

;; List of possible characters
(defconstant Characters '(Duke Assassin Ambassador Captain Contessa))

;; Number of cards per character
(defconstant CardsPerCharacter 3)

;; Number of cards per player
(defconstant CardsPerPlayer 2)

;; Number of coins required for a coup
(defconstant CoinsForCoup 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CLASS DEFINITIONS
;;
;; Data marked as PRIVATE is off-limits to players -- if your code accesses
;;  this data structure, your player will be disqualified from the tournament
;; Data marked as SELF is accessible only to that player -- if your code
;;  accesses SELF data for another player, your player will be disqualified
;; Data marked as PUBLIC is accessible to any player at any time
;; All fields in the GAME and ALL PLAYER data structures, including your own
;;  player, is to be treated as READ ONLY -- you should not directly change
;;  any values of any of these fields; you may only call the specified functions
;;  in the API at the appropriate times. If your code edits any field in any
;;  GAME or PLAYER data structure, your player will be disqualified.

;; GAME -- basic data structures for playing a game

(defclass game ()
	(
		;; PRIVATE -- deck of cards, next card to be dealt is car()
		(deck :accessor game-deck :initarg :deck)

		;; PUBLIC -- list of players (each is an instance of type PLAYER)
		(players :accessor game-players :initarg :players)

		;; PUBLIC -- list of players who have been eliminated from the game
		(eliminated :accessor game-eliminated :initform NIL :initarg :eliminated)

		;; PUBLIC -- how many rounds (one turn per player) have been played
		(rounds :accessor game-rounds :initform 0 :initarg :rounds)
	)
)

;; PLAYER -- data associated with a given player

(defclass player ()
	(
		;; PUBLIC -- player's name, must be name of their package
		(name :accessor player-name :initarg :name)

		;; SELF -- Cards in players hand that are not visible
		(hand :accessor player-hand :initform NIL :initarg :hand)

		;; PUBLIC -- Cards visible after a player lost them in a coup
		(faceup :accessor player-faceup :initform NIL :initarg :faceup)

		;; SELF -- Cards that may be selected in addition to hand during Exchange
		(exchange :accessor player-exchange :initform NIL :initarg :exchange)

		;; PUBLIC -- Number of cards remaining in players hand
		;; A player is "out" if they have 0 cards remaining
		(handcount :accessor player-handcount :initform 0 :initarg :handcount)

		;; PUBLIC -- Number of rounds played so far
		(numrounds :accessor player-numrounds :initform 0 :initarg :numrounds)

		;; PUBLIC -- Number of coins a player has
		(coins :accessor player-coins :initform 0 :initarg :coins)
	)
)

;; Calls a specified function in a player's package
(defmacro player-fn (p fn)
	`(symbol-function (intern ,fn (player-name ,p))))

;; Check if a player has a function defined
(defmacro player-fn-exists (p fn)
	`(fboundp (intern ,fn (player-name ,p))))

;; Get players name, or Nobody if not exist (like if removed)
(defun get-name (player) (if player (player-name player) "Nobody"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PUBLIC GAME FUNCTIONS
;;
;; These are utility functions specific to coup that may be called
;; by players and the tournament manager. Useful for gathering set
;; information about the game and rules

;; Given a card, what action is available
(defun card-move (card)
	(case card
		('Duke 'Tax)
		('Assassin 'Assassinate)
		('Ambassador 'Exchange)
		('Captain 'Steal)
		(otherwise NIL)))

;; Given a card, what actions are able to be blocked
(defun card-block (card)
	(case card
		('Duke 'ForeignAid)
		('Ambassador 'Steal)
		('Captain 'Steal)
		('Contessa 'Assassinate)))

;; Given an action, what card is required
(defun move-card (move)
	(case move
		('Tax 'Duke)
		('Assassinate 'Assassin)
		('Exchange 'Ambassador)
		('Steal 'Captain)
		(otherwise NIL)))

;; Given an action, what cards can block (returned as list)
(defun block-cards (move)
	(case move
		('ForeignAid '(Duke))
		('Steal '(Ambassador Captain))
		('Assassinate '(Contessa))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GAME ENGINE FUNCTIONS
;;
;; Functions to be called ONLY by the tournament manager
;; (Of course, you can use any of these functions for testing,
;; but your submitted code MUST NOT call any of these functions,
;; or you will be disqualified from the tournament.)

(defun play (&optional (players '(p1 p2 p3)))
	(let ((game (make-instance 'game
														 :deck (generate-and-shuffle)
														 :players (mapcar #'make-player players)
														 :rounds 0
														 )))
	(setf *GAME* game)

	(format t "Here's the initial deck: ~%~s~%" (game-deck game))

	(format t "Initial deal: two cards per player ~%")
	(loop for p in (game-players game)
		do (progn
				 (loop for i from 1 to CardsPerPlayer do (deal-to-player p game 'hand))
				 (setf (player-coins p) 2)))
	(print-round-summary game)

	(loop named gameloop do (progn
		(incf (game-rounds game))
		(setq active-players 0)
		(format t "-> BEGINNING ROUND ~d~%" (game-rounds game))
		(loop for p in (game-players game)
					do (progn
						(if (and (player-hand p) (> (length (game-players game)) 0)) (progn
							;; We are not out of the game, and there are still players
							(let ((player-request (funcall (player-fn p "PERFORM-MOVE") p game)))
								(let ((move (car player-request))
										(target (get-target game p (cdr player-request))))
									(if (member move Moves)
										;; Use preferred action (if failed, just default to Income)
										(if (null (perform-move p move game target))
											(progn (format t "~s can not ~s...~%" (get-name p) move)
														 (perform-move p DefaultMove game)))
										;; Otherwise if no action available, default to Income
										(progn
											(format t "No action selected... ")
											(perform-move p DefaultMove game)))))))))

		;; Quit if there's only one player remaining
		(if (= (length (game-players game)) 1)
			(progn
				(format t "Only one player remains! Congratulations!")
				(return-from gameloop)))

		;; Quit if we've been running too long
		(if (> (game-rounds game) MaxRounds)
			(progn
				(format t "Took over ~s turns ... quitting early" MaxRounds)
				(return-from gameloop)))

		(print-round-summary game)
	))

	(print-game-summary game)
))

;; Perform a given move
(defun perform-move (player move game &optional target)
	(cond ((eq move 'Income)
				 (trigger game "MOVE" (list 'Income player))
				 (progn
					 (incf (player-coins player))
					 T))

				((eq move 'ForeignAid)
				 (trigger game "MOVE" (list 'ForeignAid player))
				 (if (not (request-block 'ForeignAid player game))
					 (progn
						 (incf (player-coins player) 2)
						 T) T))

				((and (eq move 'Coup) (>= (player-coins player) 7) target)
				 (trigger game "MOVE" (list 'Coup player target))
				 (progn
					 (decf (player-coins player) 7)
					 (reveal-card target game (funcall (player-fn target "REVEAL-CARD") target game))
					 T))

				((eq move 'Tax)
				 (trigger game "MOVE" (list 'Tax player))
				 (if (not (request-challenge (move-card 'Tax) player game))
					 (progn
						 (incf (player-coins player) 3)
						 T) T))

				((and (eq move 'Assassinate) (>= (player-coins player) 3) target)
				 (trigger game "MOVE" (list 'Assassinate player target))
				 (progn (decf (player-coins player) 3)
								(if (not (or (request-challenge (move-card 'Assassinate) player game target) (request-block 'Assassinate player game)))
									(progn
										(reveal-card target game (funcall (player-fn target "REVEAL-CARD") target game))
										T) T)))

				((eq move 'Exchange)
				 (trigger game "MOVE" (list 'Exchange player))
				 (if (not (request-challenge (move-card 'Exchange) player game))
					 (progn
						 (loop for i from 1 to CardsPerPlayer do (deal-to-player player game 'exchange))
						 (loop for s in (select-exchange player game)
									 do (progn
												(cond ((and (= (car s) 1) (= (cdr s) 1))
															 ;; Swap first card in hand with first card in exchange
															 (let ((tmp (car (player-hand player))))
																		 (setf (car (player-hand player)) (car (player-exchange player)))
																		 (setf (car (player-exchange player)) tmp)))
															((and (= (car s) 1) (= (cdr s) 2))
															 ;; Swap first card in hand with second card in exchange
															 (let ((tmp (car (player-hand player))))
																		 (setf (car (player-hand player)) (cadr (player-exchange player)))
																		 (setf (cadr (player-exchange player)) tmp)))
															((and (= (car s) 2) (not (null (cadr (player-hand player)))) (= (cdr s) 1))
															 ;; Swap second card in hand (if available) with first card in exchange
															 (let ((tmp (cadr (player-hand player))))
																		 (setf (cadr (player-hand player)) (car (player-exchange player)))
																		 (setf (car (player-exchange player)) tmp)))
															((and (= (car s) 2) (not (null (cadr (player-hand player)))) (= (cdr s) 2))
															 ;; Swap second card in hand (if available) with second card in exchange
															 (let ((tmp (cadr (player-hand player))))
																		 (setf (cadr (player-hand player)) (cadr (player-exchange player)))
																		 (setf (cadr (player-exchange player)) tmp))))))
						 (nconc (game-deck game) (player-exchange player))
						 (trigger game "SHUFFLE" '())
						 (setf (game-deck game) (randomize (game-deck game)))
						 (setf (player-exchange player) NIL)
						 T) T))

				((and (eq move 'Steal) target)
				 (trigger game "MOVE" (list 'Steal player target))
				 (if (not (or (request-challenge (move-card 'Steal) player game target) (request-block 'Steal player game target)))
					 (progn
							 ;; Take at most two coins from target, or whatever they have left
							 (setf (player-coins player) (+ (player-coins player) (min (player-coins target) 2)))
							 (setf (player-coins target) (- (player-coins target) (min (player-coins target) 2)))
							 T) T))

				(T (format t "Can not perform action ~s on ~s~%" move (if target (get-name target))))
		))

;; Ask all players if they can block a move
(defun request-block (move player game &optional target)
	;;(format t "~~ Checking if anyone wishes to block ~s's ~s~%" (get-name player) move)
	(loop for p in (remove player (game-players game)) do
				(let ((result (funcall (player-fn p "BLOCK-MOVE") move p game player target)))
					(if (and result)
						(progn
							(trigger game "BLOCK" (list p player move result))
							(if (not (request-challenge result p game player))
								;; If player wishes to block and there are no successful challenges, block works
								(return-from request-block T)))))))

;; Ask all players if they wish to challenge a card
;; card -- The card being used
;; player -- The player using the card
;; game -- current game state
;; target -- If applicable, the target the player is using the card on
(defun request-challenge (card player game &optional target)
	;;(format t "~~ Checking if anyone wishes to challenge ~s's ~s~%" (get-name player) card)
	(loop for p in (remove player (game-players game)) do
				(if (funcall (player-fn p "CHALLENGE-CARD") card p game player target)
					;; Player *p* wishes to challenge the card
					(if (member card (player-hand player))
						;; Accused player has card and wins challenge
						(progn
							(trigger game "CHALLENGE-LOST" (list p player card))
							(reveal-card p game (funcall (player-fn p "REVEAL-CARD") p game))
							(return-from request-challenge NIL))

						;; Accused player does not have card and lost challenge
						(progn
							(trigger game "CHALLENGE-WON" (list p player card))
							(reveal-card player game (funcall (player-fn player "REVEAL-CARD") player game))
							(return-from request-challenge T))))))

;; Ask player which cards they would like to keep from hand / exchange
(defun select-exchange (player game)
	(funcall (player-fn player "SELECT-EXCHANGE") player game))

;; Get a target or some random if none given
(defun get-target (game player target)
	(if (and target (member target (game-players game)))
		target
		(random-elem (remove player (game-players game)))))

;; Create a player with the given name
(defun make-player (player)
	(make-instance 'player :name player))

;; Deal a single card to a player
;; where: hand, exchange
(defun deal-to-player (player game where)
	(let ((card (pop (game-deck game))))
		(format t "Dealing card ~s to player ~s's ~s~%"
						card (get-name player) where)
		(cond ((eq where 'hand)
					 (setf (player-hand player)
								 (append (player-hand player) (list card))))
					((eq where 'exchange)
					 (setf (player-exchange player)
								 (push card (player-exchange player))))
					(t (error "Don't know how to deal to ~s in DEAL-TO-PLAYER~%" where)))
		))

;; Fire and event to all the players
(defun trigger (game event arguments)
	(cond
		((string= event "MOVE") (case (car arguments)
							('Income (format t "! ~s: Performing Income~%" (get-name (cadr arguments))))
							('ForeignAid (format t "! ~s: Performing Forign Aid~%" (get-name (cadr arguments))))
							('Coup (format t "! ~s: Performing Coup against ~s~%" (get-name (cadr arguments)) (get-name (caddr arguments))))
							('Tax (format t "! ~s: Performing Tax~%" (get-name (cadr arguments))))
							('Assassinate (format t "! ~s: Performing Assassinate against ~s~%" (get-name (cadr arguments)) (get-name (caddr arguments))))
							('Exchange (format t "! ~s: Performing Exchange~%" (get-name (cadr arguments))))
							('Steal (format t "! ~s: Performing Steal against ~s~%" (get-name (cadr arguments)) (get-name (caddr arguments))))))
		((string= event "SHUFFLE") (format t "! Shuffling Deck~%"))
		((string= event "REVEAL") (format t "! ~s: Revealing Card ~s~%" (get-name (car arguments)) (cadr arguments)))
		((string= event "ELIMINATED") (format t "!!! ~s: ELIMINATED~%" (get-name (car arguments))))
		((string= event "CHALLENGE-LOST") (format t "!! ~s: Challenged ~s's ~s and lost~%" (get-name (car arguments)) (get-name (cadr arguments)) (caddr arguments)))
		((string= event "CHALLENGE-WON") (format t "!! ~s: Challenged ~s's ~s and won~%" (get-name (car arguments)) (get-name (cadr arguments)) (caddr arguments)))
		((string= event "BLOCK") (format t "!! ~s: Blocking ~s's ~s with ~s~%" (get-name (car arguments)) (get-name (cadr arguments)) (caddr arguments) (caddr arguments)))
		(T (format t "Event ~s Occurred~%" event)))
	(loop for p in (game-players game) do
				(if (player-fn-exists p "EVENT")
					(funcall (player-fn p "EVENT") event game arguments))))

;; Flip a card to reveal it and remove from hand
(defun reveal-card (player game cardnum) (progn
	(if (or (null cardnum) (not (integerp cardnum)) (> cardnum (length (player-hand player)))) (setf cardnum 1))
	(trigger game "REVEAL" (list player (if (= cardnum 1) (car (player-hand player)) (cdr (player-hand player)))))
	(if (= cardnum 1)
		(setf (player-faceup player) (append (player-faceup player) (list (pop (player-hand player)))))
		(setf (player-faceup player) (append (player-faceup player) (list (pop (cdr (player-hand player)))))))
	(if (= (length (player-hand player)) 0) (progn
		(trigger game "ELIMINATED" (list player))
		(setf (game-eliminated game) (append (game-eliminated game) (list player)))
		(setf (game-players game) (remove player (game-players game)))))))

;; Generate a new deck of cards (using Characters) and shuffle it
(defun generate-and-shuffle ()
	(randomize (mapcan #'(lambda (character)
												 (generate-n-cards character CardsPerCharacter))
										 Characters)))


(defun print-game-summary (game)
	(format t "~%=== END OF GAME! ===~%~%")
	(format t "Final deck: ~s~%" (game-deck game))
	(loop for p in (append (game-players game) (reverse (game-eliminated game)))
				do (progn
						 (format t "Player ~s has ~s coins~%" 
										 (get-name p) (player-coins p))
						 (format t "  Player ~s's hand: ~s~%"
										 (get-name p) (player-hand p)))
						 (format t "  Player ~s's faceup: ~s~%" 
										 (get-name p) (player-faceup p)))
	)

;; Print a summary of the game at the end of a round
(defun print-round-summary (game)
	(format t "~%<- END OF ROUND ~s~%" (game-rounds game))
	(loop for p in (game-players game)
				do (progn
						 (format t "Player ~s: coins ~s, hand ~s, faceup ~s~%"
										 (get-name p) (player-coins p) (player-hand p)
										 (player-faceup p))))
	(format t "~d eliminated players~%" (length (game-eliminated game)))
	(format t "~%"))

;; Generate N cards of type character (used by generate-and-shuffle())
(defun generate-n-cards (character number)
	(loop for i from 1 to number collect character))

;; Return a random copy of a list of items (used by generate-and-shuffle())
;; Knuth shuffle -- http://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp
(defun randomize (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UTILITY FUNCTIONS

;; Return a random element from list l
(defun random-elem (l)
	(unless (null l)
		(nth (random (length l)) l)))

;; Randomly return T or Nil 
(defun random-bool ()
	(= (random 2) 1))
