;;;
;;; dtk.el: access diatheke from emacs
;;;
(defvar *dtk-books*
  '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "I Samuel" "II Samuel" "I Kings" "II Kings" "I Chronicles" "II Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea"  "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi"
    "Matthew" "Mark" "Luke" "John" "Acts" "Romans" "I Corinthians" "II Corinthians" "Galations" "Ephesians" "Phillipians" "Colossians" "I Thessalonians" "II Thessalonians" "I Timothy" "II Timothy" "Titus" "Philemon" "Hebrews" "James" "I Peter" "II Peter" "I John" "II John" "III John" "Jude" "Revelations"))

(defvar *dtk-books-font-lock-variable-name-face-string*
  (concat "^\\("
	  (mapconcat #'(lambda (book)
			 book)
		     ;; treat last book differently
		     *dtk-books* ; (butlast *dtk-books* 1)
		     "\\|")
	  ;(car (last *dtk-books*))
	  "\\)"))

(defvar *dtk-compact-view-p* t)

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
	   ;;(message "DTK: start-point")
	   ;;(message (number-to-string start-point))
	   ;;(message "DTK: end-point")
	   ;;(message (number-to-string end-point))
	   (goto-char start-point)
	   (dtk-forward-verse-citation)
	   (while (and (< (point) end-point)
		       (condition-case nil
			   (dtk-forward-verse-citation)
			 (error nil)))
	     ;;(message "DTK: current point")
	     ;;(message (number-to-string (point)))
	     ;; FIXME: this is broken - I John is good test case
	     (unless (dtk-preceding-citation-is-chapter-start-p)
	       (dtk-compact-preceding-citation))))))))

(defun dtk-clear-buffer ()
  (interactive)
  (dtk-switch-to-dtk-buffer)
  (delete-region (progn (beginning-of-buffer) (point))
		 (progn (end-of-buffer) (point))))

(defun dtk-compact-preceding-citation ()
  ;;(interactive)
  (search-backward-regexp dtk-verse-citation-verse-number-regexp)
  (forward-char 1)
  (delete-region (point) (line-beginning-position))
  (insert-char 32 2)			; indent line
  )

(defun dtk-clear-dtk-buffer ()
  (dtk-switch-to-dtk-buffer)
  (beginning-of-buffer)
  (let ((start (point)))
    (end-of-buffer)
    (let ((end (point)))
      (delete-region start end))))

;; assume a single buffer named '*dtk*'
(defun dtk-ensure-dtk-buffer-exists ()
  (get-buffer-create "*dtk*"))

;; a diatheke verse citation looks like 'I Peter 1:1' succeeded by a colon
;; - the 'verse citation' is composed of BOOK:CHAPTER:VERSE-NUMBER
(defun dtk-forward-to-verse-number-end ()
  "Move forward to the end of the citation for the next verse."
  ;; search for :N: or :NN:
  (search-forward-regexp dtk-verse-citation-verse-number-regexp)
  ;; then move to next non-whitespace character
  ;; (goto-non-whitespace)
  )

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
  ;;(interactive)
  (let ((start-point (point)))
    (search-backward-regexp dtk-verse-citation-verse-number-regexp)
    (beginning-of-line)
    (multiple-value-bind (book chapter verse-number)
	(dtk-parse-citation-at-point)
      (goto-char start-point)
      (= verse-number 1))))

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

(defun dtk-switch-to-dtk-buffer ()
  (switch-to-buffer "*dtk*"))


(defvar dtk-verse-citation-verse-number-regexp
  ":[[:digit:]]+:")



;;
;; dtk major mode
;;
(defvar dtk-mode-abbrev-table nil
  "Abbrev table used while in dtk mode.")

;; place where users can add stuff
(defvar dtk-mode-hook nil)

(defvar dtk-mode-map nil
  "Major mode keymap for `dtk-mode'.")

(if (not dtk-mode-map)
    (progn
      (setq dtl-mode-map (make-sparse-keymap))))

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
  ;;(setq paragraph-start (concat "^[.']\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  ;;(setq paragraph-separate (concat "^[.']\\|" paragraph-separate))

  (run-hooks 'text-mode-hook 'dtk-mode-hook))
