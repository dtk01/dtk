;;;
;;; dtk.el: access SWORD via diatheke
;;;
(defvar *dtk-books*
  '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "I Samuel" "II Samuel" "I Kings" "II Kings" "I Chronicles" "II Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea"  "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi"
    "Matthew" "Mark" "Luke" "John" "Acts" "Romans" "I Corinthians" "II Corinthians" "Galations" "Ephesians" "Philippians" "Colossians" "I Thessalonians" "II Thessalonians" "I Timothy" "II Timothy" "Titus" "Philemon" "Hebrews" "James" "I Peter" "II Peter" "I John" "II John" "III John" "Jude" "Revelations"))

(defvar *dtk-books-regexp* nil)
(setq *dtk-books-regexp*
  (let ((raw-regexp "")) 
    (mapc #'(lambda (book)
	      (setq raw-regexp
		    (concat raw-regexp "\\(" book "\\)\\|")))
	 *dtk-books*) 
    (substring raw-regexp 0 (- (length raw-regexp) 2))))

(defvar *dtk-buffer-name* "*dtk*")

(defvar *dtk-compact-view-p* t
  "If a true value, do not use full citation for each verse. Rather, show only verse number(s) in a compact form.")

(defvar *dtk-module* "KJV"
  "Module currently in use.")

;;
;; interact with diatheke 
;;
(defun dtk ()
  "If dtk buffer already exists, move to it. Otherwise, generate the buffer and insert, into the dtk buffer, some of the content from the module. If the module is a Bible module (a member of \"Biblical Texts\"), facilitate the selection of one or more verses."
  (interactive)
  (if (dtk-buffer-exists-p)
      (dtk-switch-to-dtk-buffer) 
    (dtk-go-to)))

(defun dtk-follow ()
  "Look for full citation under point. If point is indeed at a full citation, insert corresponding verse into dtk buffer directly after citation. If point is not at a full citation, do nothing."
  (interactive)
  (dtk-to-start-of-full-citation)
  (multiple-value-bind (bk ch vs)
      (dtk-parse-citation-at-point)
    (dtk-go-to bk ch vs)))

(defun dtk-go-to (&optional bk ch vs)
"Facilitate the selection of one or more verses via BK CH and VS. If BK is NIL, query user to determine value to use for BK, CH, and VS."
  (interactive)
  (if (dtk-bible-module-p *dtk-module*)      
      (dtk-bible bk ch vs)
    (dtk-other)))

(defun dtk-bible (&optional bk ch vs)
  "BK is a string. CH is an integer. VS is an integer."
  (let ((book (or bk
		  (minibuffer-with-setup-hook 'minibuffer-complete
		    (let ((completion-ignore-case t))
		      (completing-read (concat "Book: ")
				       *dtk-books*))))))
    (let* ((ch (if bk
		   (number-to-string ch)
		 (read-from-minibuffer "Ch: ")))
	   (vs (if bk
		   (number-to-string vs)
		 (read-from-minibuffer "Vs: "))))
      (let ((ch-vs (if ch
		      (if vs
			  (concat ch ":" vs)
			ch)
		    ""))
	    (dtk-buffer (dtk-ensure-dtk-buffer-exists)))
       (dtk-switch-to-dtk-buffer)
       (dtk-mode)
       (let ((start-point (point))) 
	 (call-process "diatheke" nil
		       dtk-buffer	; insert content in dtk-buffer
		       t		; redisplay buffer as output is inserted
		       ;; arguments: -b KJV k John
		       "-b" *dtk-module* "-k" book ch-vs) 
	 (if *dtk-compact-view-p*
	     (dtk-compact-region start-point (point))))))))

(defun dtk-other ()
  ;; FIXME: this will fail except for Bible and commentary
  (dtk-commentary))

(defun dtk-commentary ()
  ;; if it's a commentary, we can get away with the same approach...
  (let* ((book
	  (or bk
	      (minibuffer-with-setup-hook 'minibuffer-complete
	 	(let ((completion-ignore-case t))
	 	  (completing-read (concat "Book: ")
	 			   *dtk-books*)))))
	 (ch (if bk
	 	 ch
	       (read-from-minibuffer "Ch: ")))
	 (vs (if bk
	 	 vs
	       (read-from-minibuffer "Vs: ")))
	 (ch-vs (if ch
	 	    (if vs
	 		(concat ch ":" vs)
	 	      ch)
	 	  ""))
	 (dtk-buffer (dtk-ensure-dtk-buffer-exists)))
    (dtk-switch-to-dtk-buffer)
    (dtk-mode)
    (let ((start-point (point)))
      (call-process "diatheke" nil
		    dtk-buffer
		    t "-b" *dtk-module* 
		    ;"-m" "100"		; sanity cap
		    "-k" book ch-vs)
      ;; (if *dtk-compact-view-p*
      ;; 	  (let ((end-point (point)))
      ;; 	    (goto-char start-point)
      ;; 	    ;; if succeeding line is blank, delete it
      ;; 	    (delete-blank-lines)
      ;; 	    (dtk-forward-to-verse-number-end)
      ;; 	    (while (and (< (point) end-point)
      ;; 			(condition-case nil
      ;; 			    (dtk-forward-to-verse-number-end)
      ;; 			  (error nil)))
      ;; 	      (unless (dtk-preceding-citation-is-chapter-start-p)
      ;; 		;; sometimes diatheke inserts a newline after the verse number
      ;; 		(dtk-snug-text-to-citation)
      ;; 		(dtk-compact-preceding-citation)
      ;; 		;; if succeeding line is blank, delete it
      ;; 		(delete-blank-lines)
      ;; 		;; what to do here? could...
      ;; 		;; 1. indent line
      ;; 		;; ;;(insert-char 32 2)
      ;; 		;; 2. compact further by merging w/preceding line
      ;; 		(join-line)		; ? (delete-indentation)
      ;; 		))))
      )))

(defun dtk-search (&optional word-or-phrase)
  (interactive)
  (let ((word-or-phrase (or word-or-phrase (read-from-minibuffer "Search: ")))
	 (dtk-buffer (dtk-ensure-dtk-buffer-exists)))
    (dtk-clear-dtk-buffer)
    (dtk-switch-to-dtk-buffer)
    (dtk-mode)
    (call-process "diatheke" nil
		  dtk-buffer
		  t "-b" *dtk-module* "-s" "phrase" "-k" word-or-phrase)))

;;;
;;; dtk modules/books
;;;

;; MODULE: a string, e.g., "KJV"
(defun dtk-bible-module-p (module)
  (member module (dtk-modules-in-category "Biblical Texts")))

;; CATEGORY: e.g., "Biblical Texts"
(defun dtk-module-category (category)
  (assoc (concat category ":") (dtk-modulelist)))

;; return a list of module names
(defun dtk-module-names () 
  (process-lines "diatheke" "-b" "system" "-k" "modulelistnames"))

(defun dtk-modulelist () 
  ;; list where each member is a sublist representing a category
  ;; - each sublist has the form (<category> <module1> <module2> ... <moduleN>)
  ;; - category of modules looks like "Biblical Texts:"
  ;; - each category string is succeeded by one or more strings, each describing a module and with the form "ESV : English Standard Version"
  (let ((modulelist-strings
	 (process-lines "diatheke" "-b" "system" "-k" "modulelist"))
	(modules-by-category nil))
    ;; construct list with the form ((category1 module11 ...) ... (categoryN moduleN1 ...))
    (dolist (x modulelist-strings)
      ;; if last character in string is colon (:), assume X represents a category
      (if (= (aref x (1- (length x))) 58)
	  (push (list x) modules-by-category) 
	(setf (first modules-by-category) (append (first modules-by-category) (list x)))))
    modules-by-category))

(defun dtk-modules-in-category (category) 
  (let ((biblical-text-modules 
	 (rest (dtk-module-category category))))
    (mapcar 
     #'(lambda (module-string)
	 (dtk-string-trim-whitespace (substring module-string 0 (position 58 module-string))))
     biblical-text-modules)))

(defun dtk-select-module ()
  (interactive)
  (let ((module 
	 (minibuffer-with-setup-hook 'minibuffer-complete
	   (let ((completion-ignore-case t))
	     (completing-read (concat "Module: ")
			      ;; FIXME: polish up dtk-modulelist and select between 'Generic books', 'Commentaries', 'Biblical Texts', etc. first
			      (dtk-module-names))))))
    (if module (setf *dtk-module* module))))

;;;
;;; dtk buffer
;;; 
(defun dtk-buffer-exists-p ()
  (get-buffer *dtk-buffer-name*))

(defun dtk-clear-dtk-buffer ()
  (interactive)
  (with-current-buffer *dtk-buffer-name*
    (delete-region (progn (beginning-of-buffer) (point))
		   (progn (end-of-buffer) (point)))))

;; assume a single buffer named '*dtk*'
(defun dtk-ensure-dtk-buffer-exists ()
  (get-buffer-create *dtk-buffer-name*))

(defun dtk-switch-to-dtk-buffer ()
  (switch-to-buffer "*dtk*"))

;;;
;;; interact with dtk buffers
;;;
(defvar dtk-verse-raw-citation-verse-number-regexp
  ":[[:digit:]]+:")

;; put point directly before number
(defun dtk-back-to-verse-full-citation-verse-number ()
  (interactive)
  (search-backward-regexp dtk-verse-raw-citation-verse-number-regexp))

(defun dtk-compact-region (&optional start-point end-point)
  "Helper for DTK-BIBLE."
  (interactive)
  (let ((end-point (or end-point (point)))
	(start-point (or start-point (region-beginning)))) 
    (goto-char start-point)
    ;; if succeeding line is blank, delete it
    (delete-blank-lines)
    ;; if we can't find verse number, no point in proceeding
    (when (dtk-forward-to-verse-number-end)
      ;; clean up ugly trailing colon
      (delete-char -1)

      ;; the above leaves the cursor on the space succeeding the colon
      (while (and (< (point) end-point)
		  (condition-case nil
		      (dtk-forward-to-verse-number-end)
		    (error nil)))
	(cond ((dtk-preceding-citation-is-chapter-start-p)
	       ;; clean up ugly trailing colon
	       (delete-char -1))
	      (t 
	       ;; sometimes diatheke inserts a newline after the verse number
	       (dtk-snug-text-to-citation)
	       (dtk-compact-preceding-citation)
	       ;; if succeeding line is blank, delete it
	       (delete-blank-lines)
	       ;; what to do here? could...
	       ;; 1. indent line
	       ;; ;;(insert-char 32 2)
	       ;; 2. compact further by merging w/preceding line
	       (join-line)		; ? (delete-indentation)
	       ))))))

(defun dtk-compact-preceding-citation ()
  (search-backward-regexp dtk-verse-raw-citation-verse-number-regexp)
  ;; put point on top of first numeral of verse
  (forward-char 1)
  (delete-region (point) (line-beginning-position))
  ;; point is at verse number
  (let ((start (point)))
    ;; verse number is preceded by space or start of line
    (search-forward-regexp "[0-9]+")
    ;; change verse number font appearance
    (dtk-to-verse-number-font start (point)))  
  ;; delete colon succeeding verse number
  (search-forward ":")
  (delete-region (1- (point)) (point)))

(defun dtk-forward-to-verse-number-end ()
  "Look for the next occurence of a verse number in a verse citation. Return point or, if unable to find verse number, return NIL."
  ;; search for :N: or :NN:
  ;; FIXME: what about chapters with > 99 verses?
  (search-forward-regexp dtk-verse-raw-citation-verse-number-regexp
			 nil
			 t))

(defun dtk-parse-citation-at-point () 
  "Assume point is at the start of a full verse citation."
  (let ((book-start-position (point))
	(book-end-position nil)
	(colon1-position nil)
	;; CITATION-END-POSITION: last position in the full citation
	(citation-end-position nil))
    ;; move to start of chapter component of citation
    (search-forward ":")
    (setf colon1-position (point))
    (search-backward " ")
    (setf book-end-position (1- (point)))
    (forward-char)
    (setf chapter-start-position (point))
    ;; move to end of of citation
    (search-forward ":")
    ;; - if citation is end start of buffer, searching for non-word character will fail
    (condition-case nil
	(progn (search-forward-regexp "\\W")
	       (backward-char))
      (error nil
	     (progn (end-of-buffer)
		    (backward-char)
		    (message "end-of-buffer"))))
    (setf citation-end-position (point))
    (values
     (buffer-substring-no-properties book-start-position (1+ book-end-position))
     (string-to-number (buffer-substring-no-properties chapter-start-position (1- colon1-position)))
     (string-to-number 
      (buffer-substring-no-properties colon1-position citation-end-position)))))

(defun dtk-preceding-citation-is-chapter-start-p ()
  "Return a true value if preceding citation corresponds to the start of a chapter."
  (let ((start-point (point)))
    (search-backward-regexp dtk-verse-raw-citation-verse-number-regexp)
    (beginning-of-line)
    (multiple-value-bind (book chapter verse-number)
	(dtk-parse-citation-at-point)
      (goto-char start-point)
      (= verse-number 1))))

(defun dtk-quit ()
  (interactive)
  (kill-buffer *dtk-buffer-name*))

(defun dtk-snug-text-to-citation (&optional gap)
  "If the verse citation verse number is not succeeded by the verse text, bring the text of the next line onto the current line."
  (let ((gap (or gap 1)))
      (if (looking-at "[ \t]*$")		; (dtk-rest-of-line-blank-p)
	  (progn
	    (kill-line)
	    (insert-char ?\u0020  	; space (ascii 32)
			 1)))))

(defun dtk-to-start-of-full-citation ()
  "If point is within a full citation, move the point to the start of the full citation."
  (interactive)
  (let ((full-citation-component (dtk-at-verse-full-citation?)))
    (when full-citation-component
	;; place point at space before chapter number
	(cond ((eq full-citation-component :space-or-book)
	       (search-forward ":")
	       (search-backward " "))
	      ((member full-citation-component
		       '(:chapter :colon :verse))
	       (message "memb")
	       (search-backward " "))) 
	;; move to start of chapter name
	(search-backward-regexp *dtk-books-regexp*)	
	;; kludge to anticipate any order in *dtk-books-regexp*
	;; - if citation is at start of buffer, searching for non-word character will fail
	(if (condition-case nil
		(progn (search-backward-regexp "\\W")
		       t)
		(error nil
		       (progn (beginning-of-line-text)
			      nil)))
	    (forward-char)))))

;;
;; dtk major mode
;;

;;
;; font lock
(defvar *dtk-books-font-lock-variable-name-face-string*
  (concat "^\\("
	  (mapconcat #'(lambda (book)
			 book)
		     ;; treat last book differently
		     *dtk-books* ; (butlast *dtk-books* 1)
		     "\\|")
	  ;(car (last *dtk-books*))
	  "\\)"))

;; these could use some TLC/refinement
(defvar dtk-font-lock-keywords nil)
(setq dtk-font-lock-keywords
      (list
       ;; book names
       (cons *dtk-books-font-lock-variable-name-face-string* 
	     ;;(find-face 'dtk-full-book)
	     font-lock-variable-name-face  ; Foreground: LightGoldenrod
	     )
       ;; chapter and verse numbers
       (cons "\\([0-9]*\\)" 
	     ;;(find-face 'dtk-full-verse-number)
	     font-lock-constant-face	; Foreground: Aquamarine
	     )
       ;; translation/source
       (list *dtk-module*)))

(defface dtk-full-book 
  '((t ()))
  "Face for book component of a full citation.")
(set-face-background 'dtk-full-book "gray50")
(set-face-foreground 'dtk-full-book "red")
(set-face-attribute 'dtk-full-book nil 
		    :height 1.2)

(defface dtk-full-verse-number 
  '((t ()))
  "Face for marking verse number component of a full citation.")
(set-face-background 'dtk-full-verse-number nil)
(set-face-attribute 'dtk-full-verse-number nil 
		    :height 1.2)

(defface dtk-compact-verse-number 
  '((t ()))
  "Face for marking verse number.")
(set-face-background 'dtk-compact-verse-number nil)
(set-face-attribute 'dtk-compact-verse-number nil 
		    :height 0.8)

;;
;; misc dtk mode stuff
(defvar dtk-mode-abbrev-table nil
  "Abbrev table used while in dtk mode.")

;; place where users can add stuff
(defvar dtk-mode-hook nil)

(defvar dtk-mode-map nil
  "Major mode keymap for `dtk-mode'.")
(setq dtk-mode-map 
      (let ((map (make-sparse-keymap))) 
	(define-key map "c" 'dtk-clear-dtk-buffer)
	(define-key map "b" 'dtk-backward-verse)
	(define-key map "g" 'dtk-go-to)
	(define-key map "f" 'dtk-forward-verse)
	(define-key map "m" 'dtk-select-module)
	(define-key map "s" 'dtk-search)
	(define-key map "q" 'dtk-quit)
	(define-key map "x" 'dtk-follow)
	map))

(defun dtk-make-overlay-verse-number (beg end) 
  (let ((ov (make-overlay beg end
			  (get-buffer *dtk-buffer-name*)
			  t t)))
    (overlay-put ov 'face 'dtk-verse-number)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'dtk-overlay t)
    ov))

(defun dtk-mode ()
  "Major mode for displaying dtk text
\\{dtk-mode-map}
Turning on dtk mode runs `text-mode-hook', then `dtk-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map dtk-mode-map)
  (setq mode-name "dtk")
  (setq major-mode 'dtk-mode)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table dtk-mode-abbrev-table)
  ;; indent with #\Tab
  ;;(setq indent-line-function 'dtk-indent-line)
  ;; syntax highlighting/font lock
  (setq font-lock-defaults '(dtk-font-lock-keywords))
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (run-hooks 'text-mode-hook 'dtk-mode-hook))

(defun dtk-to-verse-number-font (beg end)
  (with-current-buffer *dtk-buffer-name*
    (dtk-make-overlay-verse-number beg end)
    (add-text-properties
     beg end 
     '(display (raise 0.2)))))


;;;
;;; navigating (by book, chapter, and verse)
;;;
(defun dtk-at-verse-citation? ()
  ;; could be at a compact citation or at a full citation (e.g., 'John 1:1')

  ;; if at a numeral character, assume at a verse or chapter number
  (or (dtk-number-at-point (point)) 
      (dtk-at-verse-full-citation?)))

(defun dtk-at-verse-full-citation? ()
  ;; could be at a verse number or at a full citation (e.g., 'John 1:1')
  (cond ((dtk-at-verse-full-citation-chapter?)
	 :chapter)
	((dtk-at-verse-full-citation-verse?)
	 :verse)
	((dtk-at-verse-full-citation-colon?)
	 :colon)
	((dtk-at-verse-full-citation-space-or-book?)
	 :space-or-book)))

(defun dtk-at-verse-full-citation-colon? ()
  ;; if at a colon, is it between two numerals?
  (and (= 58 (char-after (point)))	; colon character?
       (dtk-number-at-point (1+ (point)))
       (dtk-number-at-point (1- (point)))))

(defun dtk-at-verse-full-citation-chapter? ()
  (looking-at "[[:digit:]]+:[[:digit:]]"))

(defun dtk-at-verse-full-citation-verse? ()
  (and (dtk-number-at-point (point))
       ;; might be at third digit of a verse...
       (looking-back "[[:digit:]]:[[:digit:]]?[[:digit:]]?" 5)))

(defun dtk-at-verse-full-citation-space-or-book? ()
  (or (and (= 32 (char-after (point)))	; space character? 
	   (or 
	    ;; if at a space, ask if it precedes ch:v
	    (looking-at " [[:digit:]]+:[[:digit:]]")
	    ;; if at a space, ask if it precedes something like 'John 3:5'
	    ;; FIXME: this is lazy since it will include space that really isn't part of citation but is adjacent to citation (it's here because this catches the space in things like 'I John')
	    (looking-at " \\w+ [[:digit:]]+:[[:digit:]]")))
      ;; if at a character, ask if it is book component of a citation
      ;; A = 65, Z = 90, a = 97, z = 122
      (and (dtk-alpha-at-point (point))
	   (or
	    ;; this misses any book that isn't a single word (e.g., 'I John')
	    (looking-at "\\w+ [[:digit:]]+:[[:digit:]]") 
	    ;; catch the possibility that we're at the 'I' in something like 'I John 1:1'
	    ;; FIXME: this will erroneously identify the end of the previous verse as part of the succeeding citation if the previous verse ends with an 'I' (seems unlikely but...)
	    (looking-at "\\(\\(I \\)\\|\\(II \\)\\)\\w+ [[:digit:]]+:[[:digit:]]")))))

(defun dtk-backward-verse ()
  "Move backward to the start of the previous verse."
  (interactive)
  (dtk-to-start-of-current-verse)
  (search-backward-regexp "[0-9]"))

(defun dtk-forward-verse ()
  "Move to the numeric component of the verse citation for the next verse."
  (interactive)
  ;; 1. check if at a citation, if so move downstream
  ;; 2. move to next citation

  ;; in the middle of a full citation?
  ;; - performing this test first allows test for numeral later to specifically test for compact citation
  (let ((at-full-citation-p (dtk-at-verse-full-citation?)))
   (cond (at-full-citation-p
	  ;; move to end of citation
	  (cond ((member at-full-citation-p 
			 '(:chapter :colon :verse))
		 (search-forward " ")) 
		;; :space-or-book
	     	((eq at-full-citation-p :space-or-book)
		 (search-forward ":")
		 (search-forward " "))))
	 ;; if at a compact citation,
	 ((dtk-number-at-point (point))
	  (search-forward " "))
	 ;; assume that if we're not at a citation, then we're in the middle of a verse
	 (t
	  nil)))
  ;; move to next verse citation
  (search-forward-regexp "[0-9]"))

(defun dtk-to-start-of-current-verse ()
  "Move to the start (beginning of the citation) of the current verse."
  (interactive)
  ;; in the middle of a full citation?
  ;; - performing this test first allows test for numeral later to specifically test for compact citation
  (let ((at-full-citation-p (dtk-at-verse-full-citation?)))
   (cond (at-full-citation-p
	  ;; move to start of citation
	  ;; FIXME: this only guarantees move close to start, not to exact start
	  (cond ((member at-full-citation-p 
			 '(:chapter :colon :verse))
		 (search-backward " "))
		;; do nothing for :space-or-book
	     	((t nil))))
	 ;; if at a compact citation,
	 ((dtk-number-at-point (point))
	  ;; back up to a character which isn't a numeral
	  (search-backward-regexp "[^0-9]")
	  ;; then forward to a numeral
	  (search-forward-regexp "[0-9]")
	  ;; put point on the numeral
	  (backward-char))
	 ;; assume that if we're not at a citation, then we're in the middle of a verse -> move back to verse number and then follow steps above
	 (t
	  (search-backward-regexp "[0-9]")
	  (dtk-to-start-of-current-verse)))))

;;;
;;; utility functions
;;;
(defun dtk-alpha-at-point (&optional point)
  "Match A-Z or a-z."
  (let ((char-code (char-after (or point (point)))))
    (or
     (and (>= char-code 65) 
	  (<= char-code 90))
     (and (>= char-code 97) 
	  (<= char-code 122)))))

(defun dtk-number-at-point (&optional point)
  "More flexible version of NUMBER-AT-POINT."
  (let ((char-code (char-after (or point (point))))) 
    (and (>= char-code 48) 
	 (<= char-code 57))))

;; several emacs libraries contain a trim function (e.g., org-trim slime-trim-whitespace bbdb-string-trim)

;; modified bbdb-string-trim
(defun dtk-string-trim-whitespace (string)
  "Lose leading and trailing whitespace."
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)