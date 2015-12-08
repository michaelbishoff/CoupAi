;+;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;CUSTOM FUNCTIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;states class
(defclass state () 
	((coins :initarg :coins)
	(player :initarg :player)
	(reward :initarg :reward)))

;;Initialization of all states
(setq state_0_a (make-instance 'state :coins 0 :player "a" :reward 0))
(setq state_1_a (make-instance 'state :coins 1 :player "a" :reward 10))
(setq state_2_a (make-instance 'state :coins 2 :player "a" :reward 20))
(setq state_3_a (make-instance 'state :coins 3 :player "a" :reward 30))
(setq state_4_a (make-instance 'state :coins 4 :player "a" :reward 40))
(setq state_5_a (make-instance 'state :coins 5 :player "a" :reward 50))
(setq state_6_a (make-instance 'state :coins 6 :player "a" :reward 60))
(setq state_7_a (make-instance 'state :coins 7 :player "a" :reward 300))
(setq state_8_a (make-instance 'state :coins 8 :player "a" :reward 305))
(setq state_9_a (make-instance 'state :coins 9 :player "a" :reward 310))
(setq state_10_a (make-instance 'state :coins 10 :player "a" :reward 315))
(setq state_11_a (make-instance 'state :coins 10 :player "a" :reward 320))
(setq state_12_a (make-instance 'state :coins 10 :player "a" :reward 325))
(setq state_0_d (make-instance 'state :coins 0 :player "d" :reward 600))
(setq state_1_d (make-instance 'state :coins 1 :player "d" :reward 605))
(setq state_2_d (make-instance 'state :coins 2 :player "d" :reward 610))
(setq state_3_d (make-instance 'state :coins 3 :player "d" :reward 615))
(setq state_4_d (make-instance 'state :coins 4 :player "d" :reward 620))
(setq state_5_d (make-instance 'state :coins 5 :player "d" :reward 625))
(setq state_6_d (make-instance 'state :coins 6 :player "d" :reward 630))
(setq state_7_d (make-instance 'state :coins 7 :player "d" :reward 635))
(setq state_dead (make-instance 'state :coins 0 :player "a" :reward -3000))

;;initial probability values
(setq prob_tax_dead 1)
(setq prob_steal_dead 1)
(setq prob_exchange_dead 1)
(setq prob_assassinate_dead 1)
(setq prob_income_dead 1)
(setq prob_aid_dead 1)
(setq prob_tax_plus 0)
(setq prob_steal_plus 0)
(setq prob_income_plus 0)
(setq prob_aid_plus 0)
(setq prob_assassinate_kill 0)
(setq prob_exchange_no 0)
(setq prob_assassinate_no 0)
(setq prob_aid_no 0)
(setq prob_steal_no 0)
(setq prob_coup_dead 1)
(setq prob_one 1)


;;InitialProbability function - given current state and action computes 
;;the probability of various final states.
;;Retruns the probability and the final state depending on the cards in hand

(defun set_initial (cards)
	(progn
		(setq card1_act nil)
		(if (eq 'coup::Duke (find 'coup::Duke cards))
			(progn	
				(if (< 0 prob_tax_dead)(setf prob_tax_dead 0))
				(if (< 0.5 prob_steal_dead)(setf prob_steal_dead 0.5))
				(if (< 0.5 prob_exchange_dead)(setf prob_exchange_dead 0.5))
				(if (< 0.5 prob_assassinate_dead)(setf prob_assassinate_dead 0.5))
				(if (< 0 prob_income_dead)(setf prob_income_dead 0))
				(if (< 0 prob_aid_dead)(setf prob_aid_dead 0))
				(if (> 1 prob_tax_plus)(setf prob_tax_plus 1))
				(if (> 0.2 prob_steal_plus)(setf prob_steal_plus 0.2))
				(if (> 1 prob_income_plus)(setf prob_income_plus 1))
				(if (> 0.4 prob_aid_plus)(setf prob_aid_plus 0.4))
				(if (> 0 prob_assassinate_kill)(setf prob_assassinate_kill 0))
				(if (> 1 prob_assassinate_no)(setf prob_assassinate_no 1))
				(if (> 0.5 prob_exchange_no)(setf prob_exchange_no 0.5))
				(if (> 0.6 prob_aid_no)(setf prob_aid_no 0.6))
				(if (> 0.3 prob_steal_no)(setf prob_steal_no 0.3))
				(if (< 0 prob_coup_dead)(setf prob_coup_dead 0))
			)
		)
		(if (eq 'coup::Captain (find 'coup::Captain cards))
			(progn	
				(if (< 0.6 prob_tax_dead)(setf prob_tax_dead 0.6))
				(if (< 0 prob_steal_dead)(setf prob_steal_dead 0))
				(if (< 0.5 prob_exchange_dead)(setf prob_exchange_dead 0.5))
				(if (< 0.5 prob_assassinate_dead)(setf prob_assassinate_dead 0.5))
				(if (< 0 prob_income_dead)(setf prob_income_dead 0))
				(if (< 0 prob_aid_dead)(setf prob_aid_dead 0))
				(if (> 0.4 prob_tax_plus)(setf prob_tax_plus 0.4))
				(if (> 0.5 prob_steal_plus)(setf prob_steal_plus 0.5))
				(if (> 1 prob_income_plus)(setf prob_income_plus 1))
				(if (> 0.4 prob_aid_plus)(setf prob_aid_plus 0.4))
				(if (> 0 prob_assassinate_kill)(setf prob_assassinate_kill 0))
				(if (> 1 prob_assassinate_no)(setf prob_assassinate_no 1))
				(if (> 0.5 prob_exchange_no)(setf prob_exchange_no 0.5))
				(if (> 0.6 prob_aid_no)(setf prob_aid_no 0.6))
				(if (> 0.5 prob_steal_no)(setf prob_steal_no 0.5))
				(if (< 0 prob_coup_dead)(setf prob_coup_dead 0))			)
		)
		(if (eq 'coup::Contessa (find 'coup::Contessa cards))
			(progn	
				(if (< 0.6 prob_tax_dead)(setf prob_tax_dead 0.6))
				(if (< 0.5 prob_steal_dead)(setf prob_steal_dead 0.5))
				(if (< 0.5 prob_exchange_dead)(setf prob_exchange_dead 0.5))
				(if (< 0.5 prob_assassinate_dead)(setf prob_assassinate_dead 0.5))
				(if (< 0 prob_income_dead)(setf prob_income_dead 0))
				(if (< 0 prob_aid_dead)(setf prob_aid_dead 0))
				(if (> 0.4 prob_tax_plus)(setf prob_tax_plus 0.4))
				(if (> 0.2 prob_steal_plus)(setf prob_steal_plus 0.2))
				(if (> 1 prob_income_plus)(setf prob_income_plus 1))
				(if (> 0.4 prob_aid_plus)(setf prob_aid_plus 0.4))
				(if (> 0 prob_assassinate_kill)(setf prob_assassinate_kill 0))
				(if (> 1 prob_assassinate_no)(setf prob_assassinate_no 1))
				(if (> 0.5 prob_exchange_no)(setf prob_exchange_no 0.5))
				(if (> 0.6 prob_aid_no)(setf prob_aid_no 0.6))
				(if (> 0.3 prob_steal_no)(setf prob_steal_no 0.3))
				(if (< 0 prob_coup_dead)(setf prob_coup_dead 0))
			)
		)
		(if (eq 'coup::Assassin (find 'coup::Assassin cards))
			(progn	
				(if (< 0.6 prob_tax_dead)(setf prob_tax_dead 0.6))
				(if (< 0.5 prob_steal_dead)(setf prob_steal_dead 0.5))
				(if (< 0.5 prob_exchange_dead)(setf prob_exchange_dead 0.5))
				(if (< 0.5 prob_assassinate_dead)(setf prob_assassinate_dead 0.5))
				(if (< 0 prob_income_dead)(setf prob_income_dead 0))
				(if (< 0 prob_aid_dead)(setf prob_aid_dead 0))
				(if (> 0.5 prob_tax_plus)(setf prob_tax_plus 0.5))
				(if (> 0.2 prob_steal_plus)(setf prob_steal_plus 0.2))
				(if (> 1 prob_income_plus)(setf prob_income_plus 1))
				(if (> 0.6 prob_aid_plus)(setf prob_aid_plus 0.6))
				(if (> 0 prob_assassinate_kill)(setf prob_assassinate_kill 0))
				(if (> 1 prob_assassinate_no)(setf prob_assassinate_no 1))
				(if (> 0.5 prob_exchange_no)(setf prob_exchange_no 0.5))
				(if (> 0.4 prob_aid_no)(setf prob_aid_no 0.4))
				(if (> 0.3 prob_steal_no)(setf prob_steal_no 0.3))
				(if (< 0 prob_coup_dead)(setf prob_coup_dead 0))
			)
		)
		(if (eq 'coup::Ambassador (find 'coup::Ambassador cards))
			(progn	
				(if (< 0.6 prob_tax_dead)(setf prob_tax_dead 0.6))
				(if (< 0.5 prob_steal_dead)(setf prob_steal_dead 0.5))
				(if (< 0.5 prob_exchange_dead)(setf prob_exchange_dead 0.5))
				(if (< 0.5 prob_assassinate_dead)(setf prob_assassinate_dead 0.5))
				(if (< 0 prob_income_dead)(setf prob_income_dead 0))
				(if (< 0 prob_aid_dead)(setf prob_aid_dead 0))
				(if (> 0.5 prob_tax_plus)(setf prob_tax_plus 0.5))
				(if (> 0.2 prob_steal_plus)(setf prob_steal_plus 0.2))
				(if (> 1 prob_income_plus)(setf prob_income_plus 1))
				(if (> 0.6 prob_aid_plus)(setf prob_aid_plus 0.6))
				(if (> 0 prob_assassinate_kill)(setf prob_assassinate_kill 0))
				(if (> 1 prob_assassinate_no)(setf prob_assassinate_no 1))
				(if (> 0.5 prob_exchange_no)(setf prob_exchange_no 0.5))
				(if (> 0.4 prob_aid_no)(setf prob_aid_no 0.4))
				(if (> 0.3 prob_steal_no)(setf prob_steal_no 0.3))
				(if (< 0 prob_coup_dead)(setf prob_coup_dead 0))
			)
		)
	)
)

;;states list used for computational in functions below
(setq states_a (list state_0_a state_1_a state_2_a state_3_a state_4_a state_5_a state_6_a state_7_a state_8_a state_9_a state_10_a))
(setq states_d (list state_1_d state_2_d state_3_d state_4_d state_5_d state_6_d state_7_d state_dead))
(setq states_all (append states_a states_d))
;(setq actions '(coup::Tax coup::Steal coup::Income coup::ForeignAid coup::Assassinate))
(setq actions '(coup::Tax coup::Steal coup::Income coup::ForeignAid coup::Coup coup::Assassinate))

;;Transition function - given current state and action, computes 
;;the probability of various final states.
;;Retruns the probability and the final state depending on the cards in hand
(defun transition (state_init action)
	(progn
;		(print (position state_init states_a))
		(if (eq 'coup::Tax action)
			(return-from transition 
				(if (< (position state_init states_a :test #'eq) 8) 
					(progn
						(list (list prob_tax_plus (nth (+ (position state_init states_a :test #'eq) 3) states_a))
						(list prob_tax_dead state_dead)))
					(list (list prob_tax_plus state_init) (list prob_tax_dead state_dead))
		)))
		(if (eq 'coup::Steal action)	
			(return-from transition 
				(if (< (position state_init states_a :test #'eq) 9) 
					(progn
						(list (list prob_steal_plus (nth (+ (position state_init states_a :test #'eq) 2) states_a))
						(list prob_steal_dead state_dead)
						(list prob_steal_no state_init)))
					(list (list prob_steal_plus state_init)
						(list prob_steal_dead state_dead)
						(list prob_steal_no state_init))
		)))
		(if (eq 'coup::Income action)	
			(return-from transition 
				(if (< (position state_init states_a :test #'eq) 10) 
					(progn
						(list (list prob_income_plus (nth (+ (position state_init states_a :test #'eq) 1) states_a))
						(list prob_income_dead state_dead)))
					(list (list prob_income_plus state_init)
						(list prob_income_dead state_dead))
		)))
		(if (eq 'coup::ForeignAid action)
			(return-from transition 
				(if (< (position state_init states_a :test #'eq) 9) 
					(progn
						(list (list prob_aid_plus (nth (+ (position state_init states_a :test #'eq) 2) states_a))
						(list prob_aid_dead state_dead)
						(list prob_aid_no state_init)))
					(list (list prob_aid_plus state_init)
						(list prob_aid_dead state_dead)
						(list prob_aid_no state_init))
		)))	
		(if (eq 'coup::Assassinate action)	
			(return-from transition 
				(if (> (position state_init states_a :test #'eq) 2) 
					(progn
						(list (list prob_assassinate_kill (nth (- (position state_init states_a :test #'eq) 3) states_d))
						(list prob_assassinate_dead state_dead)
						(list prob_assassinate_no state_init)))
					(list (list prob_assassinate_kill state_init)
						(list prob_assassinate_dead state_dead)
						(list prob_assassinate_no state_init))
		)))
		(if (eq 'coup::Exchange action)	
			()
		)
		(if (eq 'coup::Coup action)	
			(return-from transition 
				(if (> (position state_init states_a :test #'eq) 6) 
					(progn
						(list (list prob_one (nth (- (position state_init states_a :test #'eq) 7) states_d))))
					(list 
						(list prob_coup_dead state_dead))
		))
		)		
	)	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REQUIRED FUNCTIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sets the initial probability values for the transition function
(defun init (cards game)
	(set_initial cards)
	(init-opponents game)
)

;;Policy iteraction finds the best policy to take to get to a state with higher rewards
(defun policy_iteration ()
	(progn
		(setq pi (make-hash-table))
		(loop for s in states_a
			do (progn 
				(setf (gethash s pi) (nth (random 1) actions))))

		(loop for s in states_d
			do (progn (
				setf (gethash s pi) nil)))

		(setq U (make-hash-table))
		
		(loop for s in states_all
			do (setf (gethash s U) 0))

		 (loop for i from 0 to 15
		 	do
		 	(progn  
		 		(setq U (policy_eval pi U))
		 		(setq unchanged T)
		 		(loop for s in states_all
		 			do (progn
;		 				(print (slot-value s 'coins))
		 				(setf updated_act (get_actions s))
		 				(setq temp_a (gethash s pi))
		 				(loop for a in updated_act
		 					do (progn
		 						(if (> (expected_utility U s a) (expected_utility U s temp_a))
		 							(setf temp_a a)))
		 					(if (not (eq temp_a (gethash s pi)))
		 						(progn
		 							(setf (gethash s pi) a)
		 							(setf unchanged Nil))))))
		 		(if (eq unchanged T)
		 			(progn
		 			(return-from policy_iteration (list pi))))
		 		)
		 	)
		 (return-from policy_iteration (list pi))
	)
	)

;;Calculates the expected utility of each state, action pair which depends
;;the probability of the action being successful as well as the reward for each state
(defun expected_utility (U state action)
	(progn 
		(setq sum 0)
;		(print action)
		(loop for val in (transition state action)
			do(progn
;				(print val)
;				(print (gethash (nth 1 val) U))
				(setf sum (+ sum (* (car val)(gethash (nth 1 val) U)))))
		)
		(return-from expected_utility sum)
)
)

;;Policy evaluation function evaluates the policy. In other words checks the current policy
;; and determines if a better state can be reached and updates the policy hash-table as a result
(defun policy_eval (pi U)
	(progn
		(setq k 30)
		(loop for i from 0 to k
			do (progn
				(loop for s in states_all
					do (progn
;						(print "STATE")
;						(print (slot-value s 'coins))
;						(print (gethash s pi))
						(setq sum 0)
						(loop for val in (transition s (gethash s pi))
							do(progn
;								(print val)
;								(print (gethash (nth 1 val) U))
								(setf sum (+ sum (* (car val)(gethash (nth 1 val) U))))))
;						(print (gethash s U))
;						(print (slot-value s 'reward))
						(setf (gethash s U) (+ (slot-value s 'reward) (* 0.3 sum)))) 
				))))
	U
	)

;; returs all the actions that are possible. Not implemented
(defun get_actions(state)
		(if (numberp (position state states_a))
			(return-from get_actions actions)
		(return-from get_actions nil)))

;;identifies current state based on number of coins 
;;used to determine the best action
(defun current_state (num_coins) (
	nth num_coins states_a))
 
 ;;initlizes policy to nil
(setq policy nil)

;;perform-move uses policy iteration to create a hash-table of the best policies for each state
;;based on the current state the move is chosen
;;if the move is coup, a function is called to determine who to coup
;;if the move is steal, a function is called to determine who to steal
;;if the move is assassinate, a function is called to determine who to assassinate

(defun perform-move (player game)
  (progn
  	(if (= (game-rounds game) 1)
  		(progn
	  		(init (player-hand player) game)
    	)
    )
    (update_prob (player-hand player) (player-coins player))
    (format t "%%%%%%%%%%% Strongest Player is: ~a~%" (player-name (getStrongestPlayer game)))

    (format t "%%%%%%%%%%% Does ~a have ~a: ~a~%" 
    	(player-name (getStrongestPlayer game)) 
    	'coup::Contessa
    	;XX This is how you call player-has card
    	;(player-has-card player card)
    	(player-has-card (getStrongestPlayer game) 'coup::Contessa))

;    (print players)
;	(if (eq 'coup::Ambassador (find 'coup::Ambassador (player-hand player)))
;		(return-from perform-move '(coup::Exchange))
;	)
    (setq policy (nth 0 (policy_iteration)))   
;    (list (gethash (current_state (player-coins player)) (nth 0 (policy_iteration))))
	(setq move (gethash (current_state (player-coins player)) policy))

	(if (eq move 'coup::Coup)
		(return-from perform-move (list move (getStrongestPlayer game))))

	(if (eq move 'coup::Steal)
			(progn
				(loop for val in (init-opponents game)
					do (progn
						(print (player-has-card val 'coup::Captain))
;							(if (not (and (player-has-card val 'coup::Captain) (player-has-card val 'coup::Ambassador)))
;								(list move val))
						
					)
				)
				
			; for each player:
;			(if (or (player-has-card player 'coup::Captain) (player-has-card player 'coup::Ambassador))
				; Don't steal or check other players
				; Steal
;			)
;			(list move)
;			(list move)
		))
	(return-from perform-move (list move))
	)
 )

#|   ( progn (					; Play as the duke on the first round
  (print actions)
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
  ))
 |#

(defun reveal-card (player game)
	(+ (random 2) 1))

(defun select-exchange (player game)
	(format t "SWAP~%")
	(format t "Exchange hand: ~s~%" (player-exchange player))

	(if (eq (game-numplayers game) 1)
		(if (not (null (find 'coup::Captain (player-exchange player))))

		)
	)
	
	; Swap card 1 in the hand with card 1 in the exchange
	; Swap card 2 in the hand with card 2 in the exchange
	'((1 . 1) (2 . 2))
)

(defun block-move (move player game source &optional target)
	(format t "$$$$$$$$$$$ player: ~a has ~a cards. source: ~a $$$$$$$$$$$$$$$" (player-name player) (player-handcount player) (player-name source))

	(if (and (eq (player-handcount player) 1)  (eq move 'coup::Assassinate))
		'coup::Contessa
		(if (and (eq move 'coup::ForeignAid) (not (null (find 'coup::Duke (player-hand player)))))
			'coup::Duke
			(if (and (eq move 'coup::Steal) (or (not (null (find 'coup::Captain (player-hand player)))) (not (null (find 'coup::Ambassador (player-hand player))))))
				'coup::Captain
			)
		)
	)
)


; Challenges a player if their two most likely cards aren't the character
; they are claiming to be
; If the player claims to be a card that we know they lost, then challenge
; (it's not likely that they have double)
; TODO: Set the probability of them having the card to
; (given lost the card, p(card))
; If the player has 1 card left and they claim to be something other then
; their most likely card, challenge
(defun challenge-card (card player game source &optional target)
	; source blocks the move
	; player is the player that is to be challenged
;	(format t "%%%%%%%%%%%%%%%%%%%%%%% Challenge ~a ~a source: ~a %%%%%%%%%%%%%%%%~%" (player-name player) card (player-name source))

	; Check if the player's top two cards are what they are claiming to be.
	; If the top two are not the card they claim to be, call them out.
	; If the top two cards are negative, they don't have the card.

	(setq playerHand (cdr (assoc source players)))
;	(format t "~a probably has: ~a~%" source playerHand)

	(setq mostLikelyCard (nth 0 playerHand))
	(setq secondMostLikelyCard (nth 1 playerHand))

	; Gets the card and probability of that card from their hand
	(setq probabilityOfClaimedCard (assoc card playerHand))

	; If the card they are claiming to be is negative, then they don't have the card
	(if (and (not (null probabilityOfClaimedCard)) (< (cdr probabilityOfClaimedCard) 0))
		; TODO: from comment
		t

		; They have a most likely card
		(if (not (null mostLikelyCard))

			; They have some cards that are possible
			; and the most likely equals the card they are claiming to be
			(if (and (> (cdr mostLikelyCard) 0) (eq (car mostLikelyCard) card))
				nil

				; If they have two cards that are possible
				(if (not (null secondMostLikelyCard))

					; If the second most likely card equals the card they are claiming to be
					(if (and (> (cdr secondMostLikelyCard) 0) (eq (car secondMostLikelyCard) card))
						nil

						; The top two cards are not the card they are claiming to be, so call them out
						t
					)

					; If they have one card left challenge because they have a
					; different card that is likely to be in their hand
					(if (eq (- 2 (list-length (player-faceup player))) 1)
						t
					)
				)
			)
		)
	)
)

(defun event (e game arguments)
;	(format t "~%Event: ~a~%" arguments)
	; TODO: Increase probability of dying if someone gets 7 coins
	(cond
		((string= e "MOVE") (case (car arguments)
							('coup::Income "Player (cadr arguments) is using income")
							('coup::ForeignAid "Player (cadr arguments) is using foreign aid")
							('coup::Coup "Player (cadr arguments) is couping (caddr arguments)!")
							('coup::Tax "Player (cadr arguments) is using tax"
								(updateCardProbability (cadr arguments) 'coup::Duke 1))
							('coup::Assassinate "Player (cadr arguments) is assassinating (caddr arguments)"
								(updateCardProbability (cadr arguments) 'coup::Assassin 1))
							('coup::Exchange "Player (cadr arguments) is using exchange"
								(updateCardProbability (cadr arguments) 'coup::Ambassador 1))
							('coup::Steal "Player (cadr arguments) is stealing from (caddr arguments)!"
								(updateCardProbability (cadr arguments) 'coup::Captain 1))))
		((string= e "SHUFFLE") "Deck is shuffled")
		((string= e "REVEAL") "(car arguments) has shown they have (cadr arguments)")
		((string= e "ELIMINATED") "(car arguments) is totally out!"
			(setq opponents (remove (car arguments) opponents)))
		((string= e "CHALLENGE-LOST") "(car arguments) lost that challenge against (cadr arguments) having a (caddr arguments)"
			(updateCardProbability (cadr arguments) (caddr arguments) 0))
		((string= e "CHALLENGE-WON") "(car arguments) won that challenge against (cadr arguments) having a (caddr arguments)"
			(updateCardProbability (cadr arguments) (caddr arguments) 0))
			; TODO: Change this to removing the card from the possible card list for that player
		((string= e "BLOCK") "(car arguments) blocked (cadr arguments) (caddr arguments) using their (cadddr arguments)"
			(updateCardProbability (car arguments) (cadddr arguments) 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MEMORY FUNCTIONS

; Keeps track of the players, what cards they have played,
; and the probability that they actually have that card.
; The data structure format is as follows:
;#|
;'(
;	(p1 ( (Duke . 0.5) (Contessa . 0.4) ) )
;	(p2 ( (Captain . 0.17) (Ambassador . 0.5) ) )
;)
;players = [
;	p1: [ (Duke, 0.5), (Contessa, 0.4)],
;	p2: [ (Captain, 0.17), (Ambassador, 0.5)]
;]
;|#
(setq players nil)


; Updates the card's probability of occuring with the new value.
; If the hand is empty, then the player, card, and value are added.
; If the card is not in the hand, then the card and value are added.
; If the card is already in the hand, then the current value is replaced.
(defun updateCardProbability(playerName card value)
;  (format t "************** Updating ~a with card ~a . ~a ********~%~%" playerName card value)
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
        (setf (cdr cardFreq) (+ (cdr cardFreq) value)) ; (+ (cdr cardFreq) value)) -> value

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


(defun player-has-card (player card)
	(setq player-prob-cards (assoc (player-name player) players))

	; The player has played a character card
	(if (not (null player-prob-cards))
		(progn
			(if (eq card 'coup::Contessa)
				(progn 
					; Gets the card and probability of the card they just played (Duke . 0.4)
					(setq cardFreq (assoc card (cdr player-prob-cards)))

				    ; If the card is not in their possible hand
				    ; and it has a probability > 0
				    (if (and (not (null cardFreq)) (> (cdr cardFreq) 0))
				    	(return-from player-has-card t)
				    	nil
			    	)
		    	)

		    	; In all other cases, check if the card is in their top two most played
		    	(most-likely-cards player-prob-cards card)
			)
;			(if (or (eq card 'coup::Captain) (eq card 'coup::Ambassador) (eq card 'coup::Duke) (eq card 'coup::Assassin))
;				(most-likely-cards player-prob-cards card)
;			)
		)
	)
)

(defun most-likely-cards(player-prob-cards card)
	; Gets the card and probability of the card they just played (Duke . 0.4)
	(setq cardFreqs (cdr player-prob-cards))

    ; If the card is not in their possible hand
    ; and it has a probability > 0
    (if (and (not (null cardFreqs)) (or (eq (car (first cardFreqs)) card) (eq (car (second cardFreqs)) card)))
    	(return-from player-has-card t)
    	nil
	)
)




; This is set in (defun init())
(setq opponents nil)

(defun init-opponents (game)
	; Gets the list of players in the game and removes
	; PRO_JECT from this list
	(setq opponents (game-players game))
	(loop for player in (game-players game) do 
		(progn 
			;(print (player-name player))
			(if (string-equal (player-name player) 'PRO_JECT)
				(setf opponents (remove player opponents))
			)
		)
	)
	(return-from init-opponents opponents)

)

(defun getStrongestPlayer (game)
	(setq strongestPlayer nil)
	(setq strongestPlayerCoins nil)

	(loop for player in opponents
		do (progn
				(setq numCoins (+ (player-coins player) (* (player-handcount player) 7)))

				(if (null strongestPlayer)
					(progn
						(setq strongestPlayer player)
						(setq strongestPlayerCoins numCoins))

					(if (> numCoins strongestPlayerCoins)
						(progn
							(setq strongestPlayer player)
							(setq strongestPlayerCoins numCoins))
					)
				)
			)
	)
	strongestPlayer
)



;;;;;;;;NEEDS TO BE FIXED;;;;;;;;;;;;;;;;;;;;;

(defun update_prob(cards coins) 
	(progn
		(if (and (eq 'coup::Assassin (find 'coup::Assassin cards))(> coins 3))
			(progn	
				(setf prob_assassinate_dead 0.2)
				(setf prob_assassinate_kill 0.5)
				(setf prob_assassinate_no 0.3)		
			)
			(if (> coins 3)
				(progn	
					(setf prob_assassinate_dead 0.5)
					(setf prob_assassinate_kill 0.1)
					(setf prob_assassinate_no 0.9)		
				)
				(progn	
					(setf prob_assassinate_dead 0.5)
					(setf prob_assassinate_kill 0)
					(setf prob_assassinate_no 1))
			)
		)
	)
)

; |#
; (defun update_prob (cards) 
; 	(progn
; 		(if )
; 		; (if (or (eq (nth 0 players) coup::Assassin)(eq (nth 1 players) coup::Assassin))
; 		; 	(progn 
; 		; 		(setf prob_income_dead 0.5) 
; 		; 		(setf prob_aid_dead 0.5)
; 		; 		(setf prob_tax_dead 0.5)
; 		; 		(setf prob_steal_dead 0.5)
; 		; 		(setf prob_assassinate_dead 0.5)
; 		; 	))
; 		(if (any player has more that 7 coins
; 			(progn 
; 				(setf prob_income_dead 0.9) 
; 				(setf prob_aid_dead 0.9)
; 				(setf prob_tax_dead 0.9)
; 				(setf prob_steal_dead 0.9)
; 				(setf prob_assassinate_dead 0.9)
; 			))
; 		(if (or (eq (nth 0 players) coup::Duke)(eq (nth 1 players) coup::Duke))
; 			(progn 
; 				(setf prob_aid_no 0.9)
; 			))
; 		(if (or (eq (nth 0 players) coup::Captain)(eq (nth 1 players) coup::Captain))
; 			(progn 
; 				(setf prob_steal_no 0.9)
; 			))
; 	)))
; Probably need the number of times the player said they are that character and
; the probability that they are that player

; Could add memory for the cards in the deck

