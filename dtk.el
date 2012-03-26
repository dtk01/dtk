;;;
;;; dtk.el: access diatheke from emacs
;;;
(defvar *dtk-books*
  '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "I Samuel" "II Samuel" "I Kings" "II Kings" "I Chronicles" "II Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea"  "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi"
    "Matthew" "Mark" "Luke" "John" "Acts" "Romans" "I Corinthians" "II Corinthians" "Galations" "Ephesians" "Phillipians" "Colossians" "I Thessalonians" "II Thessalonians" "I Timothy" "II Timothy" "Titus" "Philemon" "Hebrews" "James" "I Peter" "II Peter" "I John" "II John" "III John" "Jude" "Revelations"))

(defvar *dtk-buffer-name* "*dtk*")

(defvar *dtk-compact-view-p* t
  "If a true value, do not use full citation for each verse. Rather, show only verse number(s) in a compact form.")

;;
;; interact with diatheke 
;;
(defun dtk (&optional bk ch vs )
  "Insert, into the dtk buffer, one or more verses. If BK is NIL, query user to determine value to use for BK, CH, and VS."
  (interactive)
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
		   dtk-buffer  ; (current-buffer)
		   t "-b" "KJV" "-k" book ch-vs)
     (if *dtk-compact-view-p*
	 (let ((end-point (point)))
	   (goto-char start-point)
	   ;; if succeeding line is blank, delete it
	   (delete-blank-lines)
	   (dtk-forward-to-verse-number-end)
	   (while (and (< (point) end-point)
		       (condition-case nil
			   (dtk-forward-to-verse-number-end)
			 (error nil)))
	     (unless (dtk-preceding-citation-is-chapter-start-p)
	       ;; sometimes diatheke inserts a newline after the verse number
	       (dtk-snug-text-to-citation)
	       (dtk-compact-preceding-citation)
	       ;; if succeeding line is blank, delete it
	       (delete-blank-lines)
	       ;; what to do here? could...
	       ;; 1. indent line
	       ;; ;;(insert-char 32 2)
	       ;; 2. compact further by merging w/preceding line
	       (join-line) ; ? (delete-indentation)
	       )))))))

(defun dtk-search (&optional word-or-phrase)
  (interactive)
  (let ((word-or-phrase (or word-or-phrase (read-from-minibuffer "Search: ")))
	 (dtk-buffer (dtk-ensure-dtk-buffer-exists)))
    (dtk-clear-dtk-buffer)
    (dtk-switch-to-dtk-buffer)
    (dtk-mode)
    (call-process "diatheke" nil
		  dtk-buffer
		  t "-b" "KJV" "-s" "phrase" "-k" word-or-phrase)))

;;;
;;; interact with dtk buffers
;;;

;; put point directly before number
(defun dtk-back-to-verse-citation-verse-number ()
  (interactive)
  (search-backward-regexp dtk-verse-citation-verse-number-regexp))

(defun dtk-clear-buffer ()
  (interactive)
  (with-current-buffer *dtk-buffer-name*
    (delete-region (progn (beginning-of-buffer) (point))
		   (progn (end-of-buffer) (point)))))

(defun dtk-compact-preceding-citation ()
  (interactive)
  (search-backward-regexp dtk-verse-citation-verse-number-regexp)
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

;; assume a single buffer named '*dtk*'
(defun dtk-ensure-dtk-buffer-exists ()
  (get-buffer-create "*dtk*"))

;; a diatheke verse citation looks like 'I Peter 1:1' succeeded by a colon
;; - the 'verse citation' is composed of BOOK:CHAPTER:VERSE-NUMBER
(defun dtk-forward-to-verse-number-end ()
  "Look for the next occurence of a verse number in a verse citation."
  ;; search for :N: or :NN:
  (search-forward-regexp dtk-verse-citation-verse-number-regexp))

(defun dtk-parse-citation-at-point ()
  "Assume point is at the start of a verse citation."
  (dtk-switch-to-dtk-buffer)
  (let ((book-start-position (point))
	(book-end-position nil)
	(colon1-position nil)
	(citation-end-position (point)))
    ;; book may look like "Amos" or "I John"
    (search-forward-regexp "[[:digit:]]")
    (setf book-end-position (- (point) 2))
    (setf chapter-start-position (point))
    (search-forward ":")
    (setf colon1-position (point))
    (search-forward ":")
    (setf citation-end-position (point))
    (values
     (buffer-substring-no-properties book-start-position book-end-position)
     (string-to-number (buffer-substring-no-properties chapter-start-position (1- colon1-position)))
     (string-to-number (buffer-substring-no-properties colon1-position (1- citation-end-position))))))

(defun dtk-preceding-citation-is-chapter-start-p ()
  "Return a true value if preceding citation corresponds to the start of a chapter."
  (let ((start-point (point)))
    (search-backward-regexp dtk-verse-citation-verse-number-regexp)
    (beginning-of-line)
    (multiple-value-bind (book chapter verse-number)
	(dtk-parse-citation-at-point)
      (goto-char start-point)
      (= verse-number 1))))

(defun dtk-snug-text-to-citation (&optional gap)
  "If the verse citation verse number is not succeeded by the verse text, bring the text of the next line onto the current line."
  (let ((gap (or gap 1)))
      (if (looking-at "[ \t]*$")		; (dtk-rest-of-line-blank-p)
	  (progn
	    (kill-line)
	    (insert-char ?\u0020  	; space (ascii 32)
			 1))
	)))

(defun dtk-switch-to-dtk-buffer ()
  (switch-to-buffer "*dtk*"))

;;
;; dtk major mode
;;
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
       (cons *dtk-books-font-lock-variable-name-face-string* font-lock-variable-name-face)
       ;;("^\\(Genesis\\|Exodus\\|Leviticus\\|Numbers\\|Deuteronomy\\|Psalms\\|Zechariah\\|Matthew\\|Mark\\|Luke\\|John\\|Acts\\|Romans\\|I Peter\\|II Peter\\|I Timothy\\|II Timothy\\)" . font-lock-variable-name-face)
       ;; chapter and verse numbers
       (cons "\\([0-9]*\\)" font-lock-constant-face)
       ;; translation/source
       (list "^\(KJV\)")))

(defvar dtk-verse-citation-verse-number-regexp
  ":[[:digit:]]+:")

(defface dtk-verse-number 
  '((t ()))
  "Face for marking verse number.")
(set-face-background 'dtk-verse-number nil)
(set-face-attribute 'dtk-verse-number nil 
		    ;; 'tex-suscript-height nice but no guarantee it will be defined
		    :height 0.8)

(defvar dtk-mode-abbrev-table nil
  "Abbrev table used while in dtk mode.")

;; place where users can add stuff
(defvar dtk-mode-hook nil)

(defvar dtk-mode-map nil
  "Major mode keymap for `dtk-mode'.")

(if (not dtk-mode-map)
    (progn
      (setq dtl-mode-map (make-sparse-keymap))))

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
