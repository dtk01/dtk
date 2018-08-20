;;; dtk.el --- access SWORD content via diatheke

;; Copyright (C) 2017-2018 David Thompson

;; Author: David Thompson
;; Keywords: hypermedia
;; Package-Requires: ((emacs "24") (seq "1.9"))
;; Version: 0.2
;; URL: https://github.com/dtk01/dtk.el

;;; Commentary:

;; This package provides access to SWORD content via diatheke, facilitating
;; reading a Biblical text, or other diatheke-accessible material, in Emacs.

;; To browse to a particular text, use `dtk`.

;;; Code:

(defvar dtk-books nil)
(setq dtk-books
      '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "I Samuel" "II Samuel" "I Kings" "II Kings" "I Chronicles" "II Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea"  "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi"
	"Matthew" "Mark" "Luke" "John" "Acts" "Romans" "I Corinthians" "II Corinthians"
	"Galatians" ; "Galations"
	"Ephesians" "Philippians" "Colossians" "I Thessalonians" "II Thessalonians" "I Timothy" "II Timothy" "Titus" "Philemon" "Hebrews" "James" "I Peter" "II Peter" "I John" "II John" "III John" "Jude"
	"Revelation of John" ;"Revelations"
	))

(defvar dtk-books-regexp nil)
(setq dtk-books-regexp
      (let ((raw-regexp ""))
	(mapc #'(lambda (book)
		  (setq raw-regexp
			(concat raw-regexp "\\(" book "\\)\\|")))
	      dtk-books)
	(substring raw-regexp 0 (- (length raw-regexp) 2))))

(defvar dtk-buffer-name "*dtk*")

(defvar dtk-dict-buffer-name "*dtk-dict*")

(defvar dtk-search-buffer-name "*dtk-search*")

(defvar dtk-compact-view-p t
  "If a true value, do not use full citation for each verse. Rather, show only verse number(s) in a compact form.")

(defvar dtk-word-wrap t
  "The value of this variable should satisfy the predicate booleanp. If its value is true, wrap continuation lines at word boundaries (space or tab character) nearest to right window edge.")

(defvar dtk-module nil
  "Module currently in use.")

(defvar dtk-module-category nil
  "Module category last selected by the user.")

;;
;; dictionary
;;
(defvar dtk-dict-crossrefs nil
  "Cross-references for the most recent dictionary lookup.")

(defvar dtk-dict-def nil
  "Definition and notes for the most recent dictionary lookup.")

(defvar dtk-dict-word nil
  "The word (raw string) for the most recent dictionary lookup.")

;;
;; interact with diatheke
;;

;;;###autoload
(defun dtk ()
  "If dtk buffer already exists, move to it. Otherwise, generate the buffer and insert, into the dtk buffer, some of the content from the module. If the module is a Bible module (a member of \"Biblical Texts\"), facilitate the selection of one or more verses."
  (interactive)
  (if (dtk-buffer-exists-p)
      (switch-to-buffer-other-window dtk-buffer-name)
    (if (dtk-biblical-texts)
	(if (not (dtk-go-to))
	    (let ((dtk-buffer (dtk-ensure-dtk-buffer-exists)))
	      (dtk-switch-to-dtk-buffer)
	      (dtk-mode)))
      (message "Biblical texts are not presently available via diatheke. Consider installing the desired texts."))))

(defun dtk-dictionary (key module)
  "Set DTK-DICT-WORD, DTK-DICT-DEF, and DTK-DICT-CROSSREFS using the dictionary module MODULE. KEY is a string, the query key for the dictionary lookup."
  (dtk-dict-handle-raw-lines (dtk-dict-raw-lines key module) module))

(defun dtk-dict-raw-lines (key module)
  "Perform a dictionary lookup using the dictionary module MODULE with query key KEY (a string). Return a list of lines, each corresponding to a line of output from invocation of diatheke."
  ;; $ diatheke -b "StrongsGreek" -k 3
  (process-lines "diatheke" "-b" module "-k" key))

(defun dtk-dict-handle-raw-lines (lines module)
  "Helper function for DTK-DICTIONARY. Handles list of strings, LINES, corresponding to lines of diatheke output associated with a dictionary query in diatheke module MODULE."
  ;; The first line begins with an integer succeeded by a colon character. Example:
  ;; 0358803588:  3588  ho   ho, including the feminine
  (let ((raw-first-line (pop lines)))
    ;; trim text up to colon character
    (setf dtk-dict-word (seq-subseq raw-first-line (1+ (seq-position raw-first-line ?:)))))
  (while (not (and (string= (elt lines 0) "")
		   (string= (elt lines 1) "")))
    (setf dtk-dict-word (concat dtk-dict-word (pop lines))))
  ;; two empty lines seem to denote boundary between the word/number/etymology and the description/definition/notes
  (pop lines)
  (pop lines)
  ;; set definition/notes component
  (setf dtk-dict-def "")
  (while (and (not (and
		    (>= (length (elt lines 0))
			4)
		    (string= (seq-subseq (dtk-string-trim-whitespace
					  (elt lines 0))
					 0 4)
			     "see ")))
	      ;; See note below regarding end of entire diatheke response
	      (not (and (>= (length (elt lines 0))
			    (+ 2 (length module)))
			(string= (seq-subseq (elt lines 0) 1 (1+ (length module)))
				 module))))
    (setf dtk-dict-def (concat dtk-dict-def (pop lines))))
  ;; set cross-references
  (setf dtk-dict-crossrefs nil)
  (while (and lines
	      ;; We expect the entire diatheke response to end with a
	      ;; line with the parenthesized module name (e.g.,
	      ;; "(StrongsHebrew)")
	      (and (>= (length (elt lines 0))
		       (+ 2 (length module)))
		   (not (string= (seq-subseq (elt lines 0) 1 (1+ (length module)))
				 module))))
    ;; FIXME: string may end with module name in parentheses; should clean that up
    (setf dtk-dict-crossrefs (push (pop lines)
				   dtk-dict-crossrefs)))
  t)

(defun dtk-follow ()
  "Look for a full citation under point. If point is indeed at a full citation, insert the corresponding verse into dtk buffer directly after citation. If point is not at a full citation, do nothing."
  (interactive)
  (dtk-to-start-of-full-citation)
  (let ((bk-ch-vs (dtk-parse-citation-at-point)))
    (dtk-go-to (elt bk-ch-vs 0)
	       (elt bk-ch-vs 1)
	       (elt bk-ch-vs 2))))

(defun dtk-go-to (&optional bk ch vs)
  "Facilitate the selection of one or more verses via book (BK), chapter number (CH), and verse number (VS). If BK is NIL, query user to determine value to use for BK, CH, and VS. Return NIL if specified module is not available."
  (interactive)
  (if (dtk-module-available-p dtk-module)
      ;; Both `Commentaries` and `Biblical Texts` are references by book, chapter, and verse
      (if (or (dtk-bible-module-available-p dtk-module)
	      (dtk-commentary-module-available-p dtk-module))
	  (dtk-bible bk ch vs)
	(dtk-other))
    (progn
      (message "Module %s is not available. Use dtk-select-module (bound to '%s' in dtk mode) to select a different module. Available modules include %s"
	       dtk-module
	       (key-description (elt (where-is-internal 'dtk-select-module dtk-mode-map) 0))
	       (dtk-module-names dtk-module-category))
      nil)))

(defun dtk-bible (&optional bk ch vs)
  "BK is a string. CH is an integer. VS is an integer. If BK is not specified, rely on interacting via the minibuffer to obtain book, chapter, and verse."
  (if (not (dtk-biblical-texts))
      (warn "One or more Biblical texts must be installed first")
    (let ((book (or bk
		    (minibuffer-with-setup-hook 'minibuffer-complete
		      (let ((completion-ignore-case t))
			(completing-read "Book: "
					 dtk-books))))))
      (let* ((ch (if bk
		     (if ch
			 (number-to-string ch)
		       "")
		   (read-from-minibuffer "Ch: ")))
	     (vs (if bk
		     (if vs
			 (number-to-string vs)
		       "")
		   (read-from-minibuffer "Vs: "))))
	(let ((ch-vs (if (not (dtk-empty-sequence-p ch))
			 (if vs
			     (concat ch ":" vs)
			   ch)
		       ""))
	      (dtk-buffer (dtk-ensure-dtk-buffer-exists)))
	  ;; if dtk buffer is already established, just move point to it
	  (switch-to-buffer-other-window dtk-buffer-name)
	  (dtk-mode)
	  (setq word-wrap dtk-word-wrap)
	  (let ((start-point (point)))
	    (dtk-bible--insert-using-diatheke book ch-vs)
	    (let ((insert-end (point)))
	      (if dtk-compact-view-p
		  (dtk-compact-region--sto start-point insert-end))
	      ;; Remove dictionary support until this is thought through.
	      (if (not dtk-show-dict-numbers)
	      	    (while (dtk-handle-next-dict-number-in-buffer start-point)
	      	      t)))))))))

(defun dtk-bible--insert-using-diatheke (book ch-vs)
  "Insert content specified by BOOK and CH-VS into the current buffer."
  (if (executable-find "diatheke")
      (progn
	;; sanity check
	(if (not dtk-module)
	    (warn "Define dtk-module ('m') first")
	  (progn
	    (call-process "diatheke" nil t
			  t     ; redisplay buffer as output is inserted
			  ;; arguments: -b KJV k John
			  "-o" "n"
			  "-b" dtk-module "-k" book ch-vs)
	    ;; diatheke outputs verses and then outputs
	    ;; - a single line with the last verse w/o reference followed by
	    ;; - a single line with the module followed by
	    ;; - a newline
	    (kill-whole-line -3)
	    t)))
    (message (concat "diatheke not found found; please verify diatheke is installed"))))

(defun dtk-other ()
  (error "Unsupported"))

;;;###autoload
(defun dtk-search (&optional word-or-phrase)
  "Search for the text string WORD-OR-PHRASE. If WORD-OR-PHRASE is NIL, prompt the user for the search string."
  (interactive)
  (let ((word-or-phrase (or word-or-phrase (read-from-minibuffer "Search: ")))
	(search-buffer (dtk-ensure-search-buffer-exists)))
    (dtk-clear-search-buffer)
    (dtk-switch-to-search-buffer)
    (dtk-search-mode)
    (call-process "diatheke" nil
		  search-buffer
		  t "-b" dtk-module "-s" "phrase" "-k" word-or-phrase)))

;;;
;;; dtk modules/books
;;;

;; MODULE: a string, e.g., "KJV"
(defun dtk-bible-module-available-p (module)
  (dtk-module-available-p dtk-module "Biblical Texts"))

(defun dtk-biblical-texts ()
  (dtk-modules-in-category "Biblical Texts"))

(defun dtk-commentary-module-available-p (module)
  (dtk-module-available-p dtk-module "Commentaries"))

(defun dtk-module-available-p (module-name &optional module-category)
  "Test whether the module specified by MODULE-NAME is locally available. MODULE-CATEGORY is either NIL or a string such as 'Biblical Texts' or 'Commentaries'. If NIL, test across all modules (don't limit by module category)."
  (member module-name (dtk-module-names (or module-category :all))))

(defun dtk-module-category (category)
  "CATEGORY is a string such as 'Biblical Texts' or 'Commentaries'."
  (assoc category (dtk-modulelist)))

(defun dtk-module-names (module-category)
  "Return a list of strings, each corresponding to a module name within the module category specified by MODULE-CATEGORY. If MODULE-CATEGORY is :all, return all module names across all categories."
  (cond ((eq module-category :all)
	 (let ((shortnames nil))
	   (mapc #'(lambda (category-data)
		     (mapc #'(lambda (shortname-description)
			       (push (elt shortname-description 0) shortnames))
			   (cdr category-data)))
		 (dtk-modulelist))
	   shortnames))
	((stringp module-category)
	 (mapcar #'(lambda (shortname-description)
		     (elt shortname-description 0))
		 (cdr (assoc (or module-category dtk-module-category)
			     (dtk-modulelist)))))))

(defun dtk-modulelist ()
  "Return an alist where each key is a string corresponding to a category and each value is a list of strings, each corresponding to a modules. A string describing a category has the form `Biblical Texts:`. A string describing a module has the form `ESV : English Standard Version`."
  (let ((modulelist-strings
	 (process-lines "diatheke" "-b" "system" "-k" "modulelist"))
	(modules-by-category nil))
    ;; construct list with the form ((category1 module11 ...) ... (categoryN moduleN1 ...))
    (dolist (x modulelist-strings)
      ;; if last character in string is colon (:), assume X represents a category
      (if (= (aref x (1- (length x))) 58)
	  (push (list (seq-subseq x 0 (1- (length x)))) modules-by-category)
	;; handle "modulename : moduledescription"
	(let ((colon-position (seq-position x 58)))
	  (let ((modulename (seq-subseq x 0 (1- colon-position)))
		(module-description (seq-subseq x (+ 2 colon-position))))
	    (setf (elt modules-by-category 0)
		  (append (elt modules-by-category 0)
			  (list (list modulename module-description))))))))
    modules-by-category))

(defun dtk-modules-in-category (category)
  (let ((biblical-text-modules
	 (cdr (dtk-module-category category))))
    (mapcar
     #'(lambda (modulename-description)
	 (elt modulename-description 0))
     biblical-text-modules)))

;;;###autoload
(defun dtk-select-module-category ()
  "Prompt the user to select a module category."
  (interactive)
  (let ((module-category
	 (minibuffer-with-setup-hook 'minibuffer-complete
	   (let ((completion-ignore-case t))
	     (completing-read "Module type: "
			      (dtk-modulelist))))))
    (if (and module-category
	     (not (string= module-category "")))
	(setf dtk-module-category module-category))))

;;;###autoload
(defun dtk-select-module ()
  "Prompt the user to select a module."
  (interactive)
  (let ((module
	 (minibuffer-with-setup-hook 'minibuffer-complete
	   (let ((completion-ignore-case t))
	     (completing-read "Module: "
			      (dtk-module-names dtk-module-category))))))
    (if (and module
	     (not (string= module "")))
	(setf dtk-module module))))

;;;
;;; dtk buffers
;;; 
(defun dtk-buffer-exists-p ()
  (get-buffer dtk-buffer-name))

(defun dtk-clear-dtk-buffer ()
  "Clear the dtk buffer."
  (interactive)
  (with-current-buffer dtk-buffer-name
    (delete-region (progn (goto-char (point-min)) (point))
		   (progn (goto-char (point-max)) (point)))))

(defun dtk-clear-search-buffer ()
  "Clear the search buffer."
  (with-current-buffer dtk-search-buffer-name
    (delete-region (progn (goto-char (point-min)) (point))
		   (progn (goto-char (point-max)) (point)))))

(defun dtk-ensure-dtk-buffer-exists ()
  (get-buffer-create dtk-buffer-name))

(defun dtk-ensure-search-buffer-exists ()
  (get-buffer-create dtk-search-buffer-name))

(defun dtk-switch-to-dtk-buffer ()
  "Switch to the dtk buffer using SWITCH-TO-BUFFER."
  (switch-to-buffer dtk-buffer-name))

(defun dtk-switch-to-search-buffer ()
  (switch-to-buffer dtk-search-buffer-name))

;;;
;;; interact with dtk buffers
;;;
(defvar dtk-verse-raw-citation-verse-number-regexp
  ":[[:digit:]]+:")

;; put point directly before number
(defun dtk-back-to-verse-full-citation-verse-number ()
  "Navigate back to the start of the verse."
  (interactive)
  (search-backward-regexp dtk-verse-raw-citation-verse-number-regexp))

(defun dtk-compact-region (&optional start-point end-point)
  "Helper for DTK-BIBLE. START-POINT and END-POINT specify the region under consideration."
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

;; This parses the specified text using sword-to-org parsing. It
;; assumes the text specified by START-POINT and END-POINT is raw
;; diatheke output. After parsing, that text is removed and replaced
;; with formatted text.
(defun dtk-compact-region--sto (start-point end-point)
  "Helper for DTK-BIBLE. START-POINT and END-POINT specify the region under consideration."
  (let ((verse-plists (dtk-sto--diatheke-parse-text (buffer-substring start-point end-point))))
    (goto-char start-point)
    (delete-region start-point end-point)
    (dtk-insert-verses verse-plists)))

(defun dtk-verse-inserter (bk ch vs text)
  (when bk
    (insert bk)
    (insert-char 32))
  (when ch
    (insert (int-to-string ch))
    (if vs
	(insert-char 58)
      (insert-char 32)))
  (when vs
    (insert (int-to-string vs))
    (insert-char 32))
  (when text
    (insert text)))

(defun dtk-insert-verses (verse-plists)
  "Insert formatted text described by VERSE-PLISTS."
  (cl-flet ()
    (let ((this-chapter nil))
      ;; handle first verse
      (-let (((&plist :book book :chapter chapter :verse verse :text text) (pop verse-plists)))
	(dtk-verse-inserter book chapter verse text)
	(setf this-chapter chapter))
      ;; Format the remaining verses, anticipating changes in chapter
      ;; number. Assume that book will not change.
      (cl-loop 
       for verse-plist in verse-plists
       do (-let (((&plist :book book :chapter chapter :verse verse :text text) verse-plist))
	    (if (equal chapter this-chapter)
		(progn
		  (unless (= (char-before) 32)
		    (insert-char 32))
		  (dtk-verse-inserter nil nil verse text))
	      ;; new chapter
	      (progn
		(insert-char 10) (insert-char 10)
		(dtk-verse-inserter book chapter verse text)
		(setf this-chapter chapter))))))))
 
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
  "Assume point is at the start of a full verse citation. Return a list where the first member specifies the book, the second member specifies the chapter, and the third member specifies the verse by number."
  (let ((book-start-position (point))
	(book-end-position nil)
	(chapter-start-position nil)
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
	     (progn (goto-char (point-max))
		    (backward-char)
		    (message "end-of-buffer"))))
    (setf citation-end-position (point))
    (list
     (buffer-substring-no-properties book-start-position (1+ book-end-position))
     (string-to-number (buffer-substring-no-properties chapter-start-position (1- colon1-position)))
     (string-to-number
      (buffer-substring-no-properties colon1-position citation-end-position)))))

(defun dtk-preceding-citation-is-chapter-start-p ()
  "Return a true value if preceding citation corresponds to the start of a chapter."
  (let ((start-point (point)))
    (search-backward-regexp dtk-verse-raw-citation-verse-number-regexp)
    (beginning-of-line)
    (let ((bk-ch-vs (dtk-parse-citation-at-point)))
      (goto-char start-point)
      (= (elt bk-ch-vs 2)		; verse number
	 1))))

(defun dtk-preview-citation ()
  "Preview citation at point."
  (interactive)
  ;; lazy man's preview -- append at end of *dtk* buffer so at least it's readable
  (with-current-buffer dtk-buffer-name
    ;; (move-point-to-end-of-dtk-buffer)
    (goto-char (point-max))
    ;; (add-vertical-line-at-end-of-dtk-buffer)
    (insert-char 10))
  ;; (back-to-point-in-search/current-buffer)
  (dtk-follow))

(defun dtk-quit ()
  "Quit."
  (interactive)
  (kill-buffer dtk-buffer-name))

(defun dtk-snug-text-to-citation ()
  "If the verse citation verse number is not succeeded by the verse text, bring the text of the next line onto the current line."
  (let ((gap 1))
    (if (looking-at "[ \t]*$")		; (dtk-rest-of-line-blank-p)
	(progn
	  (kill-line)
	  (insert-char ?\u0020  	; space (ascii 32)
		       gap)))))

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
	(search-backward-regexp dtk-books-regexp)
	;; kludge to anticipate any order in dtk-books-regexp
	;; - if citation is at start of buffer, searching for non-word character will fail
	(if (condition-case nil
		(progn (search-backward-regexp "\\W")
		       t)
		(error nil
		       (progn (beginning-of-line-text)
			      nil)))
	    (forward-char)))))

;;
;; parse diatheke raw text from a "Biblical Texts" text
;;

;; The `dtk-sto` prefix indicates code derived from alphapapa's
;; sword-to-org project.
(defconst dtk-sto--diatheke-parse-line-regexp
  (rx bol
      ;; Book name
      (group-n 1 (minimal-match (1+ anything)))
      space
      ;; chapter:verse
      (group-n 2 (1+ digit)) ":" (group-n 3 (1+ digit)) ":"
      ;; Passage text (which may start with a newline, in which case
      ;; no text will be on the same line after chapter:verse)
      (optional (1+ space)
                (group-n 4 (1+ anything))))
  "Regexp to parse each line of output from `diatheke'.")

(defun dtk-sto--diatheke-get-modules ()
  "Return list of Sword modules from diatheke. The list is an alist
where the key is the module category and the value is an alist where
the key is the module abbreviation and the value is the corresponding
string description."
  (let ((modules-by-category nil)
	(module-category nil))
    (let ((abbrevs-descriptions nil))
      (cl-loop for line in (s-lines (with-temp-buffer
                                      (call-process "diatheke" nil '(t nil) nil
                                                    "-b" "system" "-k" "modulelist")
                                      (buffer-string)))
	       when (string-match (rx (group-n 1 (minimal-match (1+ (not (any ":")))))
				      ":"
				      string-end)
				  line)
	       do (if module-category
		      (progn
			(push (list module-category abbrevs-descriptions) modules-by-category)
			(setf module-category nil)
			(setf abbrevs-descriptions nil))
		    (setf module-category (match-string 1 line)))
               when (string-match (rx (group-n 1 (minimal-match (1+ (not (any ":")))))
				      " : "
				      (group-n 2 (zero-or-more anything)))
				  line)
	       do (push (cons (match-string 1 line)
			      (match-string 2 line))
			abbrevs-descriptions)))
    modules-by-category))


(defun dtk-sto--diatheke-parse-text (text &optional &key keep-newlines)
  "Parse TEXT line-by-line, returning list of verse plists.
When KEEP-NEWLINES is non-nil, keep blank lines in text.

Plists are in format (:book \"Genesis\" :chapter 1 :verse 1
                      :text \"In the beginning...\").

Example:

\(sword-to-org--diatheke-parse-text
  (sword-to-org--diatheke-get-text \"ESV\" \"Philemon 1:1-3\")
  :keep-newlines t)"
  (cl-loop with result
           with new-verse
           for line in (s-lines text)
           for parsed = (dtk-sto--diatheke-parse-line line)
           if parsed
           do (progn
                (push new-verse result)
                (setq new-verse parsed))
           else do (let* ((text (plist-get new-verse :text))
                          (new-text (concat text
                                            (if (s-present? line)
                                                line
                                              (when keep-newlines "\n")))))
                     (plist-put new-verse :text new-text))
           finally return (cdr (progn
                                 (push new-verse result)
                                 (nreverse result)))))

(defun dtk-sto--diatheke-parse-line (line)
  "Return plist from LINE.  If LINE is not the beginning of a verse, return nil.
You generally don't want to use this directly.  Instead use
`sword-to-org--diatheke-parse-text'.

Plist is in format (:book \"Genesis\" :chapter 1 :verse 1
                    :text \"In the beginning...\").

For a complete example, see how
`sword-to-org--diatheke-parse-text' calls this function."
  (if (s-present? line)
      (when (string-match dtk-sto--diatheke-parse-line-regexp line)
        (let ((book (match-string 1 line))
              (chapter (string-to-number (match-string 2 line)))
              (verse (string-to-number (match-string 3 line)))
              ;; Ensure text is present, which may not be the case if
              ;; a verse starts with a newline.  See
              ;; <https://github.com/alphapapa/sword-to-org/issues/2>
              (text (when (s-present? (match-string 4 line))
                      (s-trim (match-string 4 line)))))
          (list :book book :chapter chapter :verse verse :text text)))))

;;
;; dictionary: handle dictionary entries and references
;;
(defface dtk-dict-word
  '((t ()))
  "Face for a word or phrase with a corresponding dictionary entry."
  :group 'dtk-faces)

(defun dtk-handle-next-dict-number-in-buffer (&optional beg)
  "Return NIL if a dictionary entry isn't specified at a position succeeding BEG. Otherwise, return a true value."
  (when beg (goto-char beg))
  (if (search-forward "<" nil t 1)
      (if (> (point) beg)
	  ;; Grab the dictionary key/number, assuming a string of the form
	  ;; <XN...N> where X is a single upper-case character and N...N
	  ;; is some integer value.
	  (let ((<-position (point))
		(>-position (search-forward ">")))
	    ;; DICT-N is the string representation of the dictionary key/number
	    (let ((dict-n (buffer-substring-no-properties (1+ <-position) (1- >-position)))
		  ;; G - Strong's Greek
		  ;; H - Strong's Hebrew
		  (module-spec (buffer-substring-no-properties <-position (1+ <-position))))
	      ;; delete <XN...N> from the buffer
	      (goto-char >-position)
	      (delete-char (1- (- <-position >-position)))
	      ;; Make an overlay for preceding word or phrase. (Just grab
	      ;; word for now; worry about phrases later)
	      (let* ((word-end (progn
				 (backward-to-word 1)
				 (point)))
		     (word-start (progn (backward-word 1)
					(point)))
		     (ov (make-overlay word-start word-end)))
		(setf (overlay-get ov 'dtk-dict-overlay) t)
		(setf (overlay-get ov 'dtk-dict-number) dict-n)
		(setf (overlay-get ov 'dtk-dict-module-spec) module-spec)
		(overlay-put ov 'help-echo dict-n)
		(overlay-put ov 'face 'dtk-dict-word)
		(goto-char word-end)))
	    t)
	nil)))

(defun dtk-dict-overlay-at-point ()
  "Return an overlay."
  (let ((overlays (overlays-at (point)))
	(dtk-dict-overlay nil))
    (if overlays
	(catch 'overlays-loop
	  (dolist (ol overlays)
	    (when (overlay-get ol 'dtk-dict-overlay)
	      (setf dtk-dict-overlay ol)
	      (throw 'overlays-loop nil)))))
    dtk-dict-overlay))

(defun dtk-dict-show-current-dict ()
  (get-buffer-create dtk-dict-buffer-name) ;(dtk-ensure-dict-buffer-exists)
  ;;(dtk-clear-dict-buffer)
  (with-current-buffer dtk-dict-buffer-name
    (delete-region (progn (goto-char (point-min)) (point))
		   (progn (goto-char (point-max)) (point))))
  (switch-to-buffer dtk-dict-buffer-name) ;(dtk-switch-to-dict-buffer)
  (dtk-dict-mode)
  ;; insert dtk-dict-content
  (insert dtk-dict-word)
  (insert-char 10)
  (insert dtk-dict-def)
  (insert-char 10)
  (mapc #'(lambda (cr)
	    (insert cr)
	    (insert-char 10))
	dtk-dict-crossrefs))

(defun dtk-show-dict-entry ()
  "Show Strong's dictionary data for word at point, if possible."
  (interactive)
  (let ((dtk-dict-overlay (dtk-dict-overlay-at-point)))
    (if dtk-dict-overlay
	(progn
	  ;; set dict data
	  (let ((dtk-dict-n (overlay-get dtk-dict-overlay 'dtk-dict-number))
		(module-spec (overlay-get dtk-dict-overlay 'dtk-dict-module-spec)))
	    (dtk-dictionary dtk-dict-n
			    (pcase module-spec
			      ("G" "StrongsGreek")
			      ("H" "StrongsHebrew"))))
	  (dtk-dict-show-current-dict))
      (message "%s" "No dictionary data"))))

(defvar dtk-show-dict-numbers nil
  "If true, show dictionary numbers, if available. Otherwise, ensure dictionary information is not visible.")

;;
;; dtk major mode
;;

;;
;; font lock
(defvar dtk-books-font-lock-variable-name-face-string
  (concat "^\\("
	  (mapconcat #'(lambda (book)
			 book)
		     ;; treat last book differently
		     dtk-books ; (butlast dtk-books 1)
		     "\\|")
	  ;(car (last dtk-books))
	  "\\)"))

;; these could use some TLC/refinement
(defvar dtk-font-lock-keywords nil)
(setq dtk-font-lock-keywords
      (list
       ;; book names
       (cons dtk-books-font-lock-variable-name-face-string
	     ;;(find-face 'dtk-full-book)
	     font-lock-variable-name-face  ; Foreground: LightGoldenrod
	     )
       ;; chapter and verse numbers
       (cons "\\([0-9]*\\)"
	     ;;(find-face 'dtk-full-verse-number)
	     font-lock-constant-face	; Foreground: Aquamarine
	     )
       ;; translation/source
       (list dtk-module)))

(defface dtk-full-book
  '((t ()))
  "Face for book component of a full citation."
  :group 'dtk-faces)
(set-face-background 'dtk-full-book "gray50")
(set-face-foreground 'dtk-full-book "red")
(set-face-attribute 'dtk-full-book nil
		    :height 1.2)

(defface dtk-full-verse-number
  '((t ()))
  "Face for marking verse number component of a full citation."
  :group 'dtk-faces)
(set-face-background 'dtk-full-verse-number nil)
(set-face-attribute 'dtk-full-verse-number nil
		    :height 1.2)

(defface dtk-compact-verse-number
  '((t ()))
  "Face for marking verse number."
  :group 'dtk-faces)
(set-face-background 'dtk-compact-verse-number nil)
(set-face-attribute 'dtk-compact-verse-number nil
		    :height 0.8)

;;
;; misc dtk mode stuff
;; (defvar dtk-mode-abbrev-table nil
;;   "Abbrev table used while in dtk mode.")

;; place where users can add stuff
;(defvar dtk-mode-hook nil)

;; (defvar dtk-mode-map nil
;;   "Major mode keymap for `dtk-mode'.")

(defun dtk-make-overlay-verse-number (beg end)
  (let ((ov (make-overlay beg end
			  (get-buffer dtk-buffer-name)
			  t t)))
    (overlay-put ov 'face 'dtk-verse-number)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'dtk-overlay t)
    ov))

;;;###autoload
(define-derived-mode dtk-mode text-mode "dtk"
  "Major mode for displaying dtk text
\\{dtk-mode-map}
Turning on dtk mode runs `text-mode-hook', then `dtk-mode-hook'."
  ;(kill-all-local-variables)
  ;(use-local-map dtk-mode-map)
  ;(setq mode-name "dtk")
  ;(setq major-mode 'dtk-mode)
  ;(set-syntax-table text-mode-syntax-table)
  ;(setq local-abbrev-table dtk-mode-abbrev-table)
  ;; indent with #\Tab
  ;;(setq indent-line-function 'dtk-indent-line)
  ;; syntax highlighting/font lock
  (setq font-lock-defaults '(dtk-font-lock-keywords))
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  ;(run-hooks 'text-mode-hook 'dtk-mode-hook)
  )

(define-key dtk-mode-map "c" 'dtk-clear-dtk-buffer)
(define-key dtk-mode-map "b" 'dtk-backward-verse)
(define-key dtk-mode-map "g" 'dtk-go-to)
(define-key dtk-mode-map "f" 'dtk-forward-verse)
(define-key dtk-mode-map "m" 'dtk-select-module)
(define-key dtk-mode-map "M" 'dtk-select-module-category)
(define-key dtk-mode-map "s" 'dtk-search)
(define-key dtk-mode-map "S" 'dtk-show-dict-entry)
(define-key dtk-mode-map "q" 'dtk-quit)
(define-key dtk-mode-map "x" 'dtk-follow)

(defun dtk-to-verse-number-font (beg end)
  (with-current-buffer dtk-buffer-name
    (dtk-make-overlay-verse-number beg end)
    (add-text-properties
     beg end
     '(display (raise 0.2)))))

;;;###autoload
(define-derived-mode dtk-search-mode dtk-mode "dtk-search"
  "Major mode for interacting with dtk search results.")

(define-key dtk-search-mode-map
  [return] 'dtk-preview-citation)

;;;###autoload
(define-derived-mode dtk-dict-mode dtk-mode "dtk-dict"
  "Major mode for interacting with dtk dict results.")

(defun dtk-dict-quit ()
  "Leave the *dtk-dict* buffer."
  (interactive)
  (kill-buffer dtk-dict-buffer-name))

(define-key dtk-mode-map "q" 'dtk-dict-quit)

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
  (or (and (= 32 (char-after (point)))
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
	     	(t nil)))
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
;;; miscellany
;;;
(defun dtk-random-point ()
  "Choose a book, at random, in DTK-BOOKS, and then navigate to a random point within that book."
  (interactive)
  (let ((book (elt dtk-books (random (length dtk-books)))))
    (dtk-go-to book nil nil)
    (goto-char (random (point-max)))))

;;;
;;; utility functions
;;;
(defun dtk-alpha-at-point (&optional point)
  "Match if the character at point POINT (defaults to current point) is an upper-case or lower-case alphabetical letter character (i.e., in the range A through Z or the range a through z)."
  (let ((char-code (char-after (or point (point)))))
    (or
     (and (>= char-code 65)
	  (<= char-code 90))
     (and (>= char-code 97)
	  (<= char-code 122)))))

(defun dtk-empty-sequence-p (x)
  (or (not x)
      (= 0 (length x))))

(defun dtk-number-at-point (&optional point)
  "A more flexible version of NUMBER-AT-POINT. POINT optionally specifies a point."
  (let ((char-code (char-after (or point (point)))))
    (and (>= char-code 48)
	 (<= char-code 57))))

;; several emacs libraries contain a trim function (e.g., org-trim slime-trim-whitespace bbdb-string-trim)

;; modified bbdb-string-trim
(defun dtk-string-trim-whitespace (string)
  "Lose leading and trailing whitespace in string STRING."
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

;;;
;;; establish defaults (relying on dtk code)
;;;
(unless dtk-module-category
  (setf dtk-module-category "Biblical Texts"))
(unless dtk-module
  (setf dtk-module (elt (dtk-module-names dtk-module-category) 0)))

(provide 'dtk)
;;; dtk.el ends here
