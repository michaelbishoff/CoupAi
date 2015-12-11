;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; project.lisp - Coup player for CMSC471 project. Uses Policy
;; Evaulation to select actions based on probability of success
;; Coups if 7 coins available, and kills the strongest player
;; as of our turn.
;;
;; Adapted from:
;; (c) Shawn Squire 2015
;; Version 1.0 - Distributed 10/6/2015
;;
;; Gaurav Luthria
;; Rajan Patel
;; Michael Bishoff

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

;; Initialization of all states
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

;; set_initial - initializes the probabilies of actions succeeding and
;; failing based on the cards we have
(defun set_initial (cards)
	(progn
		(setq card1_act nil)

		;; Default probabilities
		(setf prob_tax_dead 0.5)
		(setf prob_steal_dead 0.5)
		(setf prob_exchange_dead 0.5)
		(setf prob_assassinate_dead 0.5)
		(setf prob_income_dead 0)
		(setf prob_aid_dead 0)
		(setf prob_tax_plus 0.5)
		(setf prob_steal_plus 0.3)
		(setf prob_income_plus 1)
		(setf prob_aid_plus 0.5)
		(setf prob_assassinate_kill 0)
		(setf prob_assassinate_no 1)
		(setf prob_exchange_no 0.5)
		(setf prob_aid_no 0.5)
		(setf prob_steal_no 0.2)
		(setf prob_coup_dead 0)

		;; If we have a Duke, increase the probability of a successful tax
		(if (eq 'coup::Duke (find 'coup::Duke cards))
			(progn	
				(if (< 0 prob_tax_dead)(setf prob_tax_dead 0))
				(if (> 1 prob_tax_plus)(setf prob_tax_plus 1))
			)
		)
		;; If we have a Captain, increase the probability of a successful steal
		(if (eq 'coup::Captain (find 'coup::Captain cards))
			(progn	
				(if (< 0 prob_steal_dead)(setf prob_steal_dead 0))
				(if (> 0.6 prob_steal_plus)(setf prob_steal_plus 0.6))
				(if (> 0.4 prob_steal_no)(setf prob_steal_no 0.4)))
		)
		;; If we have an Assassin, increase the probability
		;; of a successful assinatiion
		(if (eq 'coup::Assassin (find 'coup::Assassin cards))
			(progn	
				(if (< 0 prob_assassinate_dead)(setf prob_assassinate_dead 0))
				(if (> 0 prob_assassinate_kill)(setf prob_assassinate_kill 0))
				(if (> 1 prob_assassinate_no)(setf prob_assassinate_no 1))
			)
		)
		;; Don't exchange if we have the ambassador because it's a
		;; waste of a turn for us
		(if (eq 'coup::Ambassador (find 'coup::Ambassador cards))
			(progn	
				(if (< 0 prob_exchange_dead)(setf prob_exchange_dead 0))
				(if (> 0 prob_exchange_no)(setf prob_exchange_no 0))
			)
		)
	)
)

;; states list used for computational in functions below
(setq states_a (list state_0_a state_1_a state_2_a state_3_a state_4_a state_5_a state_6_a state_7_a state_8_a state_9_a state_10_a))
(setq states_d (list state_1_d state_2_d state_3_d state_4_d state_5_d state_6_d state_7_d state_dead))
(setq states_all (append states_a states_d))
(setq actions '(coup::Tax coup::Steal coup::Income coup::ForeignAid coup::Coup coup::Assassinate))

;; transition - given current state and action, computes the
;; probability of various final states.
;; Returns the probability and the final state depending on the cards in hand
(defun transition (state_init action)
	(progn
		;;if action is tax, then return a list as shown:
		;;((probability of state_1, state_1), (probability of state_2, state_2) 
		(if (eq 'coup::Tax action)
			(return-from transition 
				(if (< (position state_init states_a :test #'eq) 8) 
					(progn
						(list (list prob_tax_plus (nth (+ (position state_init states_a :test #'eq) 3) states_a))
						(list prob_tax_dead state_dead)))
					(list (list prob_tax_plus state_init) (list prob_tax_dead state_dead))
		)))
		;;does similar thing with tax but with steal
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
		;;similar with income as with tax and steal
		(if (eq 'coup::Income action)	
			(return-from transition 
				(if (< (position state_init states_a :test #'eq) 10) 
					(progn
						(list (list prob_income_plus (nth (+ (position state_init states_a :test #'eq) 1) states_a))
						(list prob_income_dead state_dead)))
					(list (list prob_income_plus state_init)
						(list prob_income_dead state_dead))
		)))
		;;similar with ForeignAid as with tax and steal
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
		;;similar with assassinate as with tax and steal	
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
		;;similar with coup as with tax and steal
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

;; init - sets the initial probability values for the transition function given
;; the cards we are dealt. Create memory of each opponent with the gam object.
(defun init (cards game)
	(set_initial cards)
	(init-opponents game)
)

;; policy_iteraction - finds the best policy to take to get to a state
;; with higher rewards
(defun policy_iteration ()
	(progn
		;;Initializes policy hashtable
		(setq pi (make-hash-table))
		;;Creates an initial action for each state
		(loop for s in states_a
			do (progn 
				(setf (gethash s pi) (nth (random 1) actions))))
		(loop for s in states_d
			do (progn (
				setf (gethash s pi) nil)))

		;;Makes a Utility hashtable that maps utilities to each state
		(setq U (make-hash-table))
		;;Initializes the utility hashtable
		(loop for s in states_all
			do (setf (gethash s U) 0))
		;;Policy Evaluation, loops through every state and 
		;;if there is another action which leads to a higher utility
		;;replace the action in PI with the action that yields a higher
		;;utility
		 (loop for i from 0 to 15
		 	do
		 	(progn  
		 		(setq U (policy_eval pi U))
		 		(setq unchanged T)
		 		(loop for s in states_all
		 			do (progn
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

;; expected_utility - Calculates the expected utility of each state, action
;; pair which depends the probability of the action being successful as well
;; as the reward for each state
(defun expected_utility (U state action)
	(progn 
		(setq sum 0)
		(loop for val in (transition state action)
			do(progn
				(setf sum (+ sum (* (car val)(gethash (nth 1 val) U)))))
		)
		(return-from expected_utility sum)
	)
)

;; policy_eval - Checks the current policy and determines if a better state can
;; be reached and updates the policy hash-table as a result
(defun policy_eval (pi U)
	(progn
		(setq k 30)
		;;Loops through all the states 30 times (evaluation converges before)
		(loop for i from 0 to k
			do (progn
				(loop for s in states_all
					do (progn
						(setq sum 0)
						(loop for val in (transition s (gethash s pi))
							do(progn
								;;computes the utlity of the state which is the
								;;probability of each state and the rewards + the future rewards
								;;the discount value is 0.3 which means that the earlier rewards 
								;;are worth more than later rewards
								(setf sum (+ sum (* (car val)(gethash (nth 1 val) U))))))
						(setf (gethash s U) (+ (slot-value s 'reward) (* 0.3 sum)))) 
				))
		)
	)
	U
)

;; get_actions - Returns all the actions that are possible.
(defun get_actions(state)
		(if (numberp (position state states_a))
			(return-from get_actions actions)
		(return-from get_actions nil)))

;; current_state - Identifies current state based on number of coins.
;; This is used to determine the best action.
(defun current_state (num_coins) (
	nth num_coins states_a))
 
;; Initlizes policy to nil
(setq policy nil)

;; perform-move - Uses policy iteration to create a hash-table of the best
;; policies for each state based on the current state the move is chosen.
;; If the move is coup, a function is called to determine who to coup
;; If the move is steal, a function is called to determine who to steal.
;; If the move is assassinate, a function is called to
;; determine who to assassinate.
(defun perform-move (player game)
  (progn
  	;; On the first round, init the probabilities based the cards we are dealt
  	;; and create a memory of the opponents
  	(if (= (game-rounds game) 1)
  		(progn
	  		(init (player-hand player) game)
    	)
    )

    ;; Updates the probability of Assinating a player based on the number of
    ;; coins we have and if we're the Assassin
    (update_prob (player-hand player) (player-coins player))

    (setq policy (nth 0 (policy_iteration)))

    ;; Gets the selects the policy based on the current state 
	(setq move (gethash (current_state (player-coins player)) policy))

	;; If we chose to Coup, coup the strongest player
	(if (eq move 'coup::Coup)
		(return-from perform-move (list move (getStrongestPlayer game))))

	;; If we chose to Steal, steal from someone who doesn't
	;; have a Captain or Ambassador
	(if (eq move 'coup::Steal)
		(progn
			(loop for player in opponents
				do (progn
					; If a player doesn't have the Captain or the Ambassador, steal from them
					(if (not (or (player-has-card player 'coup::Captain) (player-has-card player 'coup::Ambassador)))
						(return-from perform-move (list move player)))
				)
			)
			; Take Income instead of stealing because they
			;; have a Captain or Ambassador
			(return-from perform-move (list 'coup::Income))
		)
	)

	;; If we chose to Assassinate, assassinate someone who doesn't have the contessa
	(if (eq move 'coup::Assassinate)
		(progn
			(loop for player in opponents
				do (progn
					; If a player doesn't have the captain or the ambassador, steal from them
					(if (not (player-has-card player 'coup::Contessa))
						(return-from perform-move (list move player)))
				)
			)
			; Do Income instead of assassinating because they have a Contessa
			(return-from perform-move (list 'coup::Income))
		)
	)

	;; If we chose to do ForeignAid, take ForeignAid if no one has the Duke.
	(if (eq move 'coup::ForeignAid)
		(progn
			(loop for player in opponents
				do (progn
					(print (player-has-card player 'coup::Duke))
					; If a player doesn't have the captain or the ambassador, steal from them
					(if (player-has-card player 'coup::Duke)
						(return-from perform-move (list 'coup::Income)))
				)
			)
			; Do Income instead of stealing because they have a captain or ambassador
			(return-from perform-move (list move))
		)
	)

	(return-from perform-move (list move))
  )
)

;; reveal-card - Reveals the cards we have in the order:
;; Ambassador, Contessa, Assassin, Captain, Duke
(defun reveal-card (player game)
	(setq reveal-order '('coup::Ambassador  'coup::Contessa  'coup::Assassin 'coup::Captain 'coup::Duke))

	(loop for card in reveal-order do
		(progn
			; If the card is our first card, reveal it
			(if (eq card (first (player-hand player)))
				(return-from reveal-card 1)

				; If we have a second card and the card is
				; our second card, reveal it
				(if (and (eq (player-handcount player) 2) (eq card (second (player-hand player))))
					(return-from reveal-card 2)
				)
			)
		)
	)
)

;; select-exchange - If there is only one opponent,
;; and we drew the Captian, take the Captain
(defun select-exchange (player game)
	(setq swap-list nil)

	;; If there's one other player
	(if (eq (game-numplayers game) 2)
		;; If the first card in the exchange is a Captain, take it
		(if (eq (first (player-exchange player)) 'coup::Captain)
			(setq swap-list (append swap-list '(1.1)))
		)
		;; If the second card in the exchange is a Captain, take it
		(if (eq (second (player-exchange player)) 'coup::Captain)
			(setq swap-list (append swap-list '(2.2)))
		)
	)
	swap-list
)

;; block-move - Blocks various moves depending on the situation
(defun block-move (move player game source &optional target)

	;; If we have one card left, and they are assassinating us,
	;; claim to be the Contessa
	(if (and (eq (player-handcount player) 1)  (eq move 'coup::Assassinate))
		'coup::Contessa

		;; If they try to take ForeignAid and we are the Duke, block it
		(if (and (eq move 'coup::ForeignAid) (not (null (find 'coup::Duke (player-hand player)))))
			'coup::Duke

			;; If someone tries to steal and we have the Captain, block it
			(if (and (eq move 'coup::Steal) (not (null (find 'coup::Captain (player-hand player)))))
				'coup::Captain

				;; If someone tries to steal and we have the Ambassador, block it
				(if (and (eq move 'coup::Steal) (not (null (find 'coup::Ambassador (player-hand player)))))
					'coup::Ambassador

					;; If we are being assassinated and we have the contessa, block it
					(if (and (eq move 'coup::Assassinate) (not (null (find 'coup::Contessa (player-hand player)))))
						'coup::Contessa
					)
				)
			)
		)
	)
)


;; challenge-card - Challenges a player if their two most likely cards are not
;; the character they are claiming to be.
;; If the player claims to be a card that we know they lost, then challenge
;; (it's not likely that they have double)
;; If the player has 1 card left and they claim to be something other then
;; their most likely card, challenge
(defun challenge-card (card player game source &optional target)

	;; Don't challenge if it's not us who's being attacked
	(if (not (eq target player))
		(return-from challenge-card nil)
	)

	;; Get the player's hand probability
	(setq playerHand (cdr (assoc source players)))

	;; Get the first and second likely card
	(setq mostLikelyCard (nth 0 playerHand))
	(setq secondMostLikelyCard (nth 1 playerHand))

	;; Gets the card and probability of that card from their hand
	(setq probabilityOfClaimedCard (assoc card playerHand))

	;; If the card they are claiming to be is negative, then they don't have the card
	(if (and (not (null probabilityOfClaimedCard)) (< (cdr probabilityOfClaimedCard) 0))
		t

		;; They have a most likely card
		(if (not (null mostLikelyCard))

			;; The most likely equals the card they are claiming to be
			(if (and (> (cdr mostLikelyCard) 0) (eq (car mostLikelyCard) card))
				nil

				;; If they have two cards that are possible
				(if (not (null secondMostLikelyCard))

					;; If the second most likely card equals the card they are claiming to be
					(if (and (> (cdr secondMostLikelyCard) 0) (eq (car secondMostLikelyCard) card))
						nil

						;; The top two cards are not the card they are
						;; claiming to be, so call them out
						t
					)

					;; If they have one card left challenge because they have a
					;; different card that is likely to be in their hand
					(if (eq (- 2 (list-length (player-faceup player))) 1)
						t
					)
				)
			)
		)
	)
)

;; event - handles various events that occur in the game
(defun event (e game arguments)
	(cond
		((string= e "MOVE") (case (car arguments)
							('coup::Income "Player (cadr arguments) is using income")
							('coup::ForeignAid "Player (cadr arguments) is using foreign aid")
							('coup::Coup "Player (cadr arguments) is couping (caddr arguments)!")
							;; Increase the probabliity of Duke if they Tax
							('coup::Tax "Player (cadr arguments) is using tax"
								(updateCardProbability (cadr arguments) 'coup::Duke 1))
							;; Increase the probabliity of Assassin if they Assassinate
							('coup::Assassinate "Player (cadr arguments) is assassinating (caddr arguments)"
								(updateCardProbability (cadr arguments) 'coup::Assassin 1))
							;; Increase the probabliity of Ambassador if they exchange
							('coup::Exchange "Player (cadr arguments) is using exchange"
								(updateCardProbability (cadr arguments) 'coup::Ambassador 1))
							;; Increase the probabliity of Captain if they Steal
							('coup::Steal "Player (cadr arguments) is stealing from (caddr arguments)!"
								(updateCardProbability (cadr arguments) 'coup::Captain 1))))
		((string= e "SHUFFLE") "Deck is shuffled")
		((string= e "REVEAL") "(car arguments) has shown they have (cadr arguments)")
		;; Remove a player from the list of opponents if they are eliminated
		((string= e "ELIMINATED") "(car arguments) is totally out!"
			(setq opponents (remove (car arguments) opponents)))
		;; Reset the probability of them having the card if they lost the challenge since they lost the card
		((string= e "CHALLENGE-LOST") "(car arguments) lost that challenge against (cadr arguments) having a (caddr arguments)"
			(updateCardProbability (cadr arguments) (caddr arguments) 0))
		;; Reset the probability of them have the card if they won the challenge since they get a new card
		((string= e "CHALLENGE-WON") "(car arguments) won that challenge against (cadr arguments) having a (caddr arguments)"
			(updateCardProbability (cadr arguments) (caddr arguments) 0))
		;; Increase the probability of them having the card they are claiming to block with
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

;; updateCardProbability - Updates the card's probability of occuring by adding
;; the value to the running count of times they've played that card.
;; If the hand is empty, then the player, card, and value are added.
;; If the card is not in the hand, then the card and value are added.
;; If the card is already in the hand, then the new value is added.
(defun updateCardProbability(playerName card value)
  ;; Get the player's possible cards & probabilities
  (setq player (assoc playerName players))

  ;; If the player's hand is empty, add the card and its value
  (if (null player)
    (setq players (append players (list (list playerName (cons card 1))))) ; 1 -> value

    ;; Add the card and its value or Update the card's value
    (progn
      ;; Gets the card and probability of the card they just played (Duke . 0.4)
      (setq cardFreq (assoc card (cdr player)))

      ;; If the card is not in their possible hand, add it.
      ;; If the card is in their possible hand, update the value
      (if (not (null cardFreq))
        ;; Updates the players probability of having that card
        (setf (cdr cardFreq) (+ (cdr cardFreq) value))

        ;; Add the card to their possible hand
        (progn
          (setq players (remove player players))
          (setq player (append player (list (cons card 1))))
          (setq players (append players (list player)))
        )
      )
    )
  )

  ;; Sorts the cards for that player by their occurence
  (sort (cdr player) #'sortCardsByMaxOccurence)
)

;; sortCardsByMaxOccurence - Sorts the list of cards by their occurence
;; Input:  '((Duke . 10) (Assassin . 5) (Contessa . 15))
;; Output: '((Contessa . 15) (Duke . 10) (Assassin . 5))
(defun sortCardsByMaxOccurence(a b)
  (> (cdr a) (cdr b))
)

;; player-has-card - Returns true if the player is likely to have a card.
(defun player-has-card (player card)
	(setq player-prob-cards (assoc (player-name player) players))

	; The player has played a character card
	(if (not (null player-prob-cards))
		(progn
			;; Special case for Contessa
			;; If they've claimed to be the contessa once, assume they have it
			;; because the contessa's action is rarely used
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
		)
	)
)

;; most-likely-cards - Returns true if the card is the
;; player's first or second most likely card
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




;; The opponents in the game. This is set in (defun init())
(setq opponents nil)

;; init-opponents - Initilizes the list of opponents in the game
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

;; getStrongestPlayer - Returns the 'strongest player' at the current state in
;; the game. The strongest player is the player who has the most coins and we
;; treat each card as 7 coins
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

;; update_prob - Changes the probability of a successful or blocked
;; assassination, or us getting called out and losing a card if we try to
;; assassinate based on if we have the assassin and the number of coins we have
(defun update_prob(cards coins) 
	(progn
		;; If we have the assassin and if we hve more than 3 coins
		(if (and (eq 'coup::Assassin (find 'coup::Assassin cards))(> coins 3))
			(progn	
				(setf prob_assassinate_dead 0)
				(setf prob_assassinate_kill 0.5)
				(setf prob_assassinate_no 0.3)		
			)
			;; If we have more than 3 coins
			(if (> coins 3)
				(progn	
					(setf prob_assassinate_dead 0.3)
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