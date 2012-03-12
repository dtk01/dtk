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

(defun dtk (&optional bk ch vs)
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
    ;(dtk-clear-dtk-buffer)
    (dtk-switch-to-dtk-buffer)
    (dtk-mode)
    (call-process "diatheke" nil 
		  dtk-buffer  ; (current-buffer)
		  t "-b" "KJV" "-k" book ch-vs)))

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


;;
;; dtk major mode
;;
(defvar dtk-mode-abbrev-table nil
  "Abbrev table used while in dtk mode.")

;; place where users can add stuff
(defvar dtk-mode-hook nil)

(defvar dtk-mode-map
  nil "Major mode keymap for `dtk-mode'.")

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
  "Major mode for display dtk text
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
  (setq indent-line-function 'dtk-indent-line)
  ;; syntax highlighting/font lock
  (setq font-lock-defaults '(dtk-font-lock-keywords))

  (make-local-variable 'paragraph-start)
  ;;(setq paragraph-start (concat "^[.']\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  ;;(setq paragraph-separate (concat "^[.']\\|" paragraph-separate))
  
  (run-hooks 'text-mode-hook 'dtk-mode-hook))
