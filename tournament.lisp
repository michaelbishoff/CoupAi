(load 'coup.lisp)

(setf *random-state* (make-random-state t))

(defclass result () (
		(player :accessor result-player :initarg :player)
		(games :accessor result-games :initarg :games :initform 0)
		(wins :accessor result-wins :initarg :wins :initform 0)
		(losses :accessor result-losses :initarg :losses :initform 0)
		(crashes :accessor result-crashes :initarg :crashes :initform '())
		(points :accessor result-points :initarg :points :initform 0)
		(history :accessor result-history :initarg :history :initform '())))

;; Initiation
(defun load-players (rosterf)
	(setq players '())
	(with-open-file (stream rosterf :direction :input :if-does-not-exist :error)
		(loop for player in (mapcar #'split-str
																(loop for line = (read-line stream nil 'eof)
																			until (eq line 'eof)
																			collect line))
					do (if (not (char-equal #\# (char (car player) 0))) (progn
							 (load (cadr player))
							 (setf *results* (append
																 *results*
																 (list (make-instance 'result
																											:player
																											(intern (car player))))))
							 (setq players (append players (list (intern (car player)))))))))
	players)

;; PLAY
(defun play-roster (num)
	(progn
		(loop for i upto (- num 1) do (comb 3 players #'play-match))
		(format t "~%~%===== Tournament Results =====~%" num)
		(loop for p in (sort *results* #'> :key #'result-points)
					do (print-result p))))

(defun play-match (players)
	(let ((game (coup::play players)))
		(if (valid-game game)
			(handle-results game)
			(format t "Invalid game result (~S) -- discarding~%~%"
							(mapcar #'coup::player-name (all-players game))))))

(defun valid-game (game)
	(and (= (coup::game-numplayers game) (length (all-players game)))
			 (= (length (list-dupes (all-players game))) 0)))

(defun handle-results (game)
	(loop for p in (all-players game) do
				(add-points (get-points p game) (find-result (coup::player-name p)))
				(add-history game (find-result (coup::player-name p)))
				(if (coup::player-crashed p)
					(add-crash (coup::player-error p)
										 (find-result (coup::player-name p))))
				(if (find p (coup::game-players game))
					(add-win (find-result (coup::player-name p)))
					(add-lose (find-result (coup::player-name p))))))

(defun add-history (game player)
	(setf (result-history player) (append (result-history player) (list game))))

(defun add-points (points player)
	(setf (result-points player) (+ (result-points player) points)))

(defun get-points (player game)
	(* 50 (- (coup::game-numplayers game)
					 (get-position (coup::player-name player) game))))

(defun print-result (r)
	(let ((rankings (map-history-positions r))
				(player (map-history
									#'(lambda (game)
											(find (result-player r)
														(all-players game)
														:key #'coup::player-name))
									r)))
		(format t "~s~%" (result-player r))
		(format t "    ~1$ points per game~%"
						(if (> (length (result-history r)) 0)
							(/ (result-points r) (length (result-history r)))
							0))
		(format t "    ~d matches~%" (length (result-history r)))
		(format t "    ~d first place~%" (count 0 rankings))
		(format t "    ~d second place~%" (count 1 rankings))
		(format t "    ~d third place~%" (count 2 rankings))
		(if (> (count-if #'(lambda (x) (> x 2)) rankings) 0)
			(format t "    ~d lower places~%"
							(count-if #'(lambda (x) (> x 2)) rankings)))

		(format t "    ~d bluffs~%"
						(apply '+ (mapcar #'coup::player-bluffs player)))
		(format t "    ~d times you challenged and won~%"
						(apply '+ (mapcar #'coup::player-you-challenged-won player)))
		(format t "    ~d times you challenged and lost~%"
						(apply '+ (mapcar #'coup::player-you-challenged-lost player)))
		(format t "    ~d times you were challenged and won~%"
						(apply '+ (mapcar #'coup::player-were-challenged-won player)))
		(format t "    ~d times you were challenged and lost~%"
						(apply '+ (mapcar #'coup::player-were-challenged-lost player)))
		(format t "    ~d times you blocked~%"
						(apply '+ (mapcar #'coup::player-you-blocked player)))
		(format t "    ~d times you were blocked~%"
						(apply '+ (mapcar #'coup::player-were-blocked player)))

		(format t "    ~d crashes~%" (length (result-crashes r)))
		(format t "~%")))

(defun find-result (p)
	(find p *results* :key #'result-player))

(defun add-win (p) (progn
	(setf (result-wins p) (incf (result-wins p)))
	(setf (result-games p) (incf (result-games p)))))

(defun add-lose (p) (progn
	(setf (result-losses p) (incf (result-losses p)))
	(setf (result-games p) (incf (result-games p)))))

(defun add-crash (e p) (progn
	(setf (result-crashes p) (append (result-crashes p) (list e)))))

(defun select-players (n l)
	(if (< n 1)
		nil
		(let ((p (coup::random-elem l)))
			(append (list p) (select-players (decf n) (remove p l))))))

(defun map-history (f r)
	(mapcar f (result-history r)))

(defun map-history-positions (r)
	(map-history #'(lambda (game) (get-position (result-player r) game)) r))

(defun get-position (p game)
	(position p (mapcar #'coup::player-name (all-players game))))

(defun all-players (game)
	(append (coup::game-players game) (reverse (coup::game-eliminated game))))


;; Helpers
(defun split-str (string &optional (separator " "))
	(split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
	(let ((n (position separator string
										 :from-end t
										 :test #'(lambda (x y)
															 (find y x :test #'string=)))))
		(if n
			(split-str-1 (subseq string 0 n)
									 separator (cons (subseq string (1+ n)) r))
			(cons string r))))

(defun comb (m list fn)
	(labels ((comb1 (l c m)
									(when (>= (length l) m)
										(if (zerop m) (return-from comb1 (funcall fn c)))
										(comb1 (cdr l) c m)
										(comb1 (cdr l) (cons (first l) c) (1- m)))))
		(comb1 list nil m)))


;; http://www.lee-mac.com/uniqueduplicate.html#listdupes
(defun list-dupes (l)
	(if l
		(if (member (car l) (cdr l))
			(cons (car l) (list-dupes (remove (car l) (cdr l))))
			(list-dupes (remove (car l) (cdr l))))))

;; Running from command line
(setq *results* '())
(if (> (length *args*) 0)
	(setq players (load-players (car *args*))))
(if (> (length *args*) 1)
	(play-roster (parse-integer (cadr *args*))))