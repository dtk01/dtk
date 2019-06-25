;;; dtk.el --- access SWORD content via diatheke
;;
;; Copyright (C) 2017-2019 David Thompson
;;
;; Author: David Thompson
;; Keywords: hypermedia
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6.1") (dash "2.12.0") (seq "1.9") (s "1.9"))
;; Version: 0.2
;; URL: https://github.com/dtk01/dtk.el

;;; Commentary:
;;
;; This package provides access to SWORD content via diatheke, facilitating
;; reading Biblical text or other diatheke-accessible material in Emacs.
;;
;; To browse a particular text in a dedicated buffer, use `dtk'. To insert text
;; directly, use `dtk-bible'.

;;; Code:


;;;; Dependencies
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'seq)
(require 'subr-x)

;;;; Customization
;; User configurable variables:
(defgroup dtk nil
  "Read Biblical text and other SWORD resources through diatheke."
  :prefix "dtk-"
  :group 'convenience)

;;;;; General Settings
(defcustom dtk-program "diatheke"
  "Front-end to SWORD library.
Only diatheke is supported at the moment."
  :type 'string)

(defcustom dtk-word-wrap t
  "Non-nil means to use word-wrapping for continuation lines."
  :type 'boolean)

(defcustom dtk-compact-view t
  "Show verses in compact view.
If nil, display all verses as if they're retrieved independently, e.g:

John 1:1: In the beginning was the Word, and the Word was with God, and the Word was God.
John 1:2: The same was in the beginning with God.
John 1:3: All things were made by him; and without him was not any thing made that was made.

If non-nil, hide repeated \"chapter\" for all verses except the first one, e.g:

John 1:1 In the beginning was the Word, and the Word was with
God, and the Word was God. 2 The same was in the beginning with
God. 3 All things were made by him; and without him was not any
thing made that was made."
  :type 'boolean)

(defcustom dtk-buffer-name "*dtk*"
  "Name of buffer for displaying text.")

(defcustom dtk-dict-buffer-name "*dtk-dict*"
  "Name of buffer for displaying dictionary entries and references.")

(defcustom dtk-search-buffer-name "*dtk-search*"
  "Name of buffer for displaying search results.")

(defvar dtk-diatheke-output-format nil
  "Opportunity for user to specify desired the output format when
  calling diatheke. Intended to be used in conjunction with
  dtk-preserve-diatheke-output-p.")

(defvar dtk-preserve-diatheke-output-p nil
  "When true, do not attempt to parse or format, but preserve diatheke
  output ``as-is''.")

;;;;; Biblical Text defaults
;; TODO: "module" is a more general term. Rename it properly.
(defcustom dtk-module nil
  "Module currently in use.")

(defcustom dtk-module-category nil
  "Module category last selected by the user.")

;;;;; Dictionary settings
(defcustom dtk-dict-crossrefs nil
  "Cross-references for the most recent dictionary lookup.")

(defcustom dtk-dict-def nil
  "Definition and notes for the most recent dictionary lookup.")

(defcustom dtk-dict-word nil
  "The word (raw string) for the most recent dictionary lookup.")

;;;;; Internal variables
(defcustom dtk--recent-book nil
  "Most recently used book when reading user's completion."
  ;; Normally we read the same book during a short period of time, so save
  ;; latest input as default. On the contrary, chapter and verses are short
  ;; numeric input, so we skip them.
  )

;;;;; Constants
(defconst dtk-books
  '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "I Samuel" "II Samuel" "I Kings" "II Kings" "I Chronicles" "II Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea"  "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi"
    "Matthew" "Mark" "Luke" "John" "Acts" "Romans" "I Corinthians" "II Corinthians" "Galatians" "Ephesians" "Philippians" "Colossians" "I Thessalonians" "II Thessalonians" "I Timothy" "II Timothy" "Titus" "Philemon" "Hebrews" "James" "I Peter" "II Peter" "I John" "II John" "III John" "Jude"
    "Revelation of John" ;"Revelations"
    )
  "List of strings representing books of the Bible.")

(defconst dtk-books-regexp
  (regexp-opt dtk-books)
  "Regular expression aiming to match a member of DTK-BOOKS.")

;;; Functions
;;;###autoload
(defun dtk ()
  "If dtk buffer already exists, move to it. Otherwise, generate the buffer and insert, into the dtk buffer, some of the content from the module. If the module is a Bible module (a member of \"Biblical Texts\"), facilitate the selection of one or more verses."
  (interactive)
  (if (not (dtk-biblical-texts))
      (message "Biblical texts are not presently available via diatheke. Consider installing the desired texts.")
    (dtk-init)
    (dtk-go-to)))

(defun dtk-dictionary (key module)
  "Set DTK-DICT-WORD, DTK-DICT-DEF, and DTK-DICT-CROSSREFS using the dictionary module MODULE. KEY is a string, the query key for the dictionary lookup."
  (dtk-dict-handle-raw-lines (dtk-dict-raw-lines key module) module))

(defun dtk-dict-raw-lines (key module)
  "Perform a dictionary lookup using the dictionary module MODULE with query key KEY (a string). Return a list of lines, each corresponding to a line of output from invocation of diatheke."
  ;; $ diatheke -b "StrongsGreek" -k 3
  (process-lines dtk-program "-b" module "-k" key))

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
		    (string= (seq-subseq (string-trim (elt lines 0))
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
  (let ((book-chapter-verse (dtk-parse-citation-at-point)))
    (dtk-go-to (elt book-chapter-verse 0)
	       (elt book-chapter-verse 1)
	       (elt book-chapter-verse 2))))

(defun dtk-go-to (&optional book chapter verse)
  "Facilitate the selection of one or more verses via book (BOOK), chapter number (CHAPTER), and verse number (VERSE). If BOOK is NIL, query user to determine value to use for BOOK, CHAPTER, and VERSE. Return NIL if specified module is not available."
  (interactive)
  (if (dtk-module-available-p dtk-module)
      ;; Both `Commentaries` and `Biblical Texts` are references by book, chapter, and verse
      (if (or (dtk-bible-module-available-p dtk-module)
	      (dtk-commentary-module-available-p dtk-module))
	  (dtk-bible book chapter verse t)
	(dtk-other))
    (progn
      (message "Module %s is not available. Use dtk-select-module (bound to '%s' in dtk mode) to select a different module. Available modules include %s"
	       dtk-module
	       (key-description (elt (where-is-internal 'dtk-select-module dtk-mode-map) 0))
	       (dtk-module-names dtk-module-category))
      nil)))

(defun dtk-bible (&optional book chapter verse dtk-buffer-p)
  "Query diatheke and insert text.
With `C-u' prefix arg, change module temporarily.

Text is inserted in place, unless DTK-BUFFER-P is true.

BOOK is a string. CHAPTER is an integer. VERSE is an integer. If
BOOK is not specified, rely on interacting via the minibuffer to
obtain book, chapter, and verse."
  (interactive)
  (when (not (dtk-biblical-texts))
    (error "One or more Biblical texts must be installed first"))
  (let* ((completion-ignore-case t)
         (final-module  (or (if current-prefix-arg ;; Called with prefix argument
                                (completing-read "Module: " (dtk-module-names
                                                             dtk-module-category)
                                                 nil t nil nil '(nil)))
                            dtk-module))
         (final-book    (or book
                            (setq dtk--recent-book
                                  (completing-read "Book: " dtk-books nil nil nil nil dtk--recent-book))))
         (final-chapter (or (when chapter (number-to-string chapter))
                            (read-from-minibuffer "Chapter: ")))
         (final-verse   (or (when verse (number-to-string verse))
                            (read-from-minibuffer "Verse: ")))
         (chapter-verse (concat final-chapter ":" final-verse)))
    ;; If dtk-buffer-p is true, insert text in the default dtk buffer
    (when dtk-buffer-p
      (cond ((not (dtk-buffer-exists-p))
	     (dtk-init))
	    (t
	     (switch-to-buffer dtk-buffer-name))))
    ;; User can specify output format, overriding default
    (let ((output-format (or dtk-diatheke-output-format :osis)))
      ;; Insert text directly
      (condition-case nil
	  (dtk-bible--insert-using-diatheke final-book chapter-verse final-module output-format)
	(error
	 ;; at this point, consider the game up if XML parsing triggered an error;
	 ;; attempt to degrade gracefully and try simple/plain format
	 (dtk-bible--insert-using-diatheke final-book chapter-verse final-module :plain))))
    )
  )

(defun dtk-bible--insert-using-diatheke (book chapter-verse &optional module diatheke-output-format)
  "Insert content specified by BOOK and CHAPTER-VERSE into the current buffer. CHAPTER-VERSE is a string of the form CC:VV (chapter number and verse number separated by the colon character)."
  (unless diatheke-output-format
    (setq diatheke-output-format :plain))
  (let ((module (or module dtk-module)))
    (insert
     (with-temp-buffer
       (call-process dtk-program nil t
                     t     ; redisplay buffer as output is inserted
                     ;; arguments: -b KJV k John
                     "-o" (case diatheke-output-format
			    (:osis "nfmslx")
			    (:plain "n"))
		     ;; FIXME: perform a sanity check to determine if OSIS is available for the given text
		     "-f" (case diatheke-output-format
			    (:osis "OSIS")
			    (:plain "plain"))
                     "-b" module "-k" book chapter-verse)
       ;; Provides user the option to work with "raw" diatheke output
       (unless dtk-preserve-diatheke-output-p
	 ;; Assume diatheke emits text and then emits
	 ;; - zero or more empty lines followed by
	 ;; - a line beginning with the colon character succeeded by the text of last verse (w/o reference) followed by
	 ;; - a single line beginning with the ( character indicating the module (e.g., "(ESV2011)")
	 ;; - followed by a zero or more newlines

	 ;; Post-process texts
	 ;; Search back and remove (<module name>)
	 (let ((end-point (point)))
           (re-search-backward "^(.*)" nil t 1)
           (delete-region (point) end-point))
	 ;; Search back and remove duplicate text of last verse and the preceding colon
	 (let ((end-point (point)))
           (re-search-backward "^:" nil t 1)
           (delete-region (point) end-point))
	 ;; Parse text
	 (let ((raw-diatheke-text (buffer-substring (point-min) (point-max))))
	   ;; PARSED-LINES is a list where each member has the form
	   ;; (:book "John" :chapter 1 :verse 1 :text (...))
	   (let ((parsed-lines (dtk--parse-osis-xml-lines raw-diatheke-text)))
	     ;; replace diatheke output w/text from parsed-lines
	     (delete-region (point-min) (point-max))
	     (dtk-insert-verses parsed-lines))))
       ;; Return contents of the temporary buffer
       (buffer-string)
       )))
  t)

(defun dtk-other ()
  "Placeholder anticipating possibility of using diatheke to access content distinct from Biblical texts."
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
    (call-process dtk-program nil
		  search-buffer
		  t "-b" dtk-module "-s" "phrase" "-k" word-or-phrase)))

;;;
;;; dtk modules/books
;;;

(defun dtk-bible-module-available-p (module)
  "Indicate whether the module MODULE is available. MODULE is a string."
  (dtk-module-available-p module "Biblical Texts"))

(defun dtk-biblical-texts ()
  "Return a list of module names associated with the 'Biblical Texts' category."
  (dtk-modules-in-category "Biblical Texts"))

(defun dtk-commentary-module-available-p (module)
  "Return an indication of whether module MODULE is both available and associated with the 'Commentaries' category."
  (dtk-module-available-p module "Commentaries"))

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
	 (process-lines dtk-program "-b" "system" "-k" "modulelist"))
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
  "Return a list of module names associated with module category CATEGORY."
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
         (let ((completion-ignore-case t))
           (completing-read "Module type: "
                            (dtk-modulelist)))))
    (if (and module-category
	     (not (string= module-category "")))
	(setf dtk-module-category module-category))))

;;;###autoload
(defun dtk-select-module ()
  "Prompt the user to select a module."
  (interactive)
  (let ((module
         (let ((completion-ignore-case t))
           (completing-read "Module: "
                            (dtk-module-names dtk-module-category)
                            nil
                            t
                            nil
                            nil
                            '(nil)))))
    (if module
	(setf dtk-module module)
      (message "Module not selected"))))

;;;
;;; dtk buffers
;;; 
(defun dtk-buffer-exists-p ()
  "Return an indication of whether the default dtk buffer exists."
  (get-buffer dtk-buffer-name))

(defun dtk-clear-dtk-buffer ()
  "Clear the dtk buffer."
  (interactive)
  (with-current-buffer dtk-buffer-name
    (delete-region (point-min) (point-max))))

(defun dtk-clear-search-buffer ()
  "Clear the search buffer."
  (with-current-buffer dtk-search-buffer-name
    (delete-region (point-min) (point-max))))

(defun dtk-init ()
  "Initialize dtk buffer and switch to it."
  (when (not (dtk-buffer-exists-p))
    (get-buffer-create dtk-buffer-name)
    ;; Switch window only when we're not already in *dtk*
    (if (not (string= (buffer-name) dtk-buffer-name))
        (switch-to-buffer-other-window dtk-buffer-name)
      (switch-to-buffer dtk-buffer-name))
    (dtk-mode))
  )

(defun dtk-ensure-search-buffer-exists ()
  "Ensure the default dtk buffer exists for conducting a search."
  (get-buffer-create dtk-search-buffer-name))

(defun dtk-switch-to-search-buffer ()
  "Switch to the dtk search buffer using SWITCH-TO-BUFFER."
  (switch-to-buffer dtk-search-buffer-name))

;;;
;;; interact with dtk buffers
;;;
(defcustom dtk-verse-raw-citation-verse-number-regexp
  ":[[:digit:]]+:"
  "A regular expression used to match verse number(s).")

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

(defun dtk-verse-inserter (book ch verse text new-bk-p new-ch-p)
  "Insert a verse associated book BOOK, chapter CH, verse number VERSE, and text TEXT. If this function is being invoked in the context of a change to a new book or a new chapter, indicate this with NEW-BK-P or NEW-CH-P, respectively."
  (let ((book-start (point)))
    (when (or (not dtk-compact-view) new-bk-p)
      (insert book #x20)
      (set-text-properties book-start (point) (list 'book book)))
    (when (or (not dtk-compact-view) new-ch-p)
      (let ((chapter-start (point)))
	(insert (int-to-string chapter)
		(if verse #x3a #x20))
	(add-text-properties (if new-ch-p
				 chapter-start
			       book-start)
			     (point)
			     (list 'book book 'chapter chapter)))))
  (when verse
    (let ((verse-start (point)))
      (insert (int-to-string verse) #x20)
      (set-text-properties verse-start (point) (list 'book book 'chapter chapter 'verse verse))))
  (when text
    (let ((text-start (point)))
      (funcall dtk-verse-text-inserter text)
      ;; verse text inserter may set text properties
      (add-text-properties text-start (point) (list 'book book 'chapter chapter 'verse verse))))
  (unless dtk-compact-view
    (insert #xa)))

(defun dtk-insert-osis-string (string)
  ;; Ensure some form of whitespace precedes a word. OSIS-ELT may be a word, a set of words (e.g., "And" or "the longsuffering"), or a bundle of punctuation and whitespace (e.g., "; ").
  (when (string-match "^[a-zA-Z]" string)
    (when (not (member (char-before) '(32 9 10 11 12 13)))
      (insert #x20)))
  (insert string))

(defun dtk-insert-osis-elt (osis-elt)
  (let* ((tag (pop osis-elt))
	 (attributes (pop osis-elt))
	 (children osis-elt))
    (case tag
      (w
       (when children
	 (let ((lemma (let ((lemma-pair (assoc 'lemma attributes)))
			(if lemma-pair
			    (cdr lemma-pair)))))
	   (let ((beg (point)))
	     (dtk-simple-osis-inserter children)
	     ;; add text properties
	     (when lemma
	       (add-text-properties beg (point) (list 'lemma lemma)))))))
      (divineName
       (dtk-simple-osis-inserter children))
      (transChange
       (when children
	 (let ((beg (point)))
	   (dtk-simple-osis-inserter children)
	   ;;(add-text-properties beg (point) (list 'transChange t))
	   ;;(add-text-properties beg (point) '(font-lock-face dtk-translChange-face))
	   )))
      (q				; quote
       (dtk-simple-osis-inserter children))
      ;; containers
      (div
       (let ((type (let ((type-pair (assoc 'type attributes)))
		     (if type-pair
			 (cdr type-pair)))))
	 (cond ((and
		 t		       ;dtk-honor-osis-div-paragraph-p
		 (equalp type "paragraph"))
		(insert #xa)))
	 (dtk-simple-osis-inserter children)))
      (chapter
       (dtk-simple-osis-inserter children))
      (verse
       (dtk-simple-osis-inserter children))
      (l				; poetic line(s)
       (dtk-simple-osis-inserter children))
      (lg				; poetic line(s)
       (dtk-simple-osis-inserter children))
      (note
       (when nil			;dtk-show-notes-p
	 (dtk-simple-osis-inserter children)))
      ;; formatting
      (milestone
       (let ((type (let ((type-pair (assoc 'type attributes)))
		     (if type-pair
			 (cdr type-pair)))))
	 (cond ((and nil	      ;dtk-honor-osis-milestone-line-p
		     (equalp type "line"))
		(insert #xa)))))
      (lb
       (when (and t			;dtk-honor-osis-lb-p
		  (insert #xa))))
      ;; indicate inability to handle this OSIS element
      (t (when nil		   ;dtk-flag-unhandled-osis-elements-p
	   (insert "!" (prin1-to-string tag) "!"))))))

(defun dtk-insert-osis-thing (osis-thing)
  "Insert verse text represented by OSIS-THING."
  (cond ((stringp osis-thing)
	 (dtk-insert-osis-string osis-thing))
	((consp osis-thing)
	 (dtk-insert-osis-elt osis-thing))
	;; indicate inability to handle this elt
	(t (insert "*" (prin1-to-string osis-thing) "*"))))

(defvar dtk-verse-text-inserter
  'dtk-simple-osis-inserter
  "Specifies function used to insert verse text.

The function is called with a single argument, CHILDREN, a list where
each member is either a string or a list representing a child element
permissible within an OSIS XML document. Consider this example
representation of a W element:

  (w ((lemma . \"strong:G1722\") (wn . \"001\")) \"In\")")

(defun dtk-simple-osis-inserter (children)
  (dolist (osis-elt children)
    (dtk-insert-osis-thing osis-elt)))

(defun dtk-insert-verses (verse-plists)
  "Insert formatted text described by VERSE-PLISTS."
  (cl-flet ()
    (let ((this-chapter nil))
      ;; handle first verse
      (-let (((&plist :book book :chapter chapter :verse verse :text text) (pop verse-plists)))
	(dtk-verse-inserter book chapter verse text t t)
	(setf this-chapter chapter))
      ;; Format the remaining verses, anticipating changes in chapter
      ;; number. Assume that book will not change.
      (cl-loop 
       for verse-plist in verse-plists
       do (-let (((&plist :book book :chapter chapter :verse verse :text text) verse-plist))
	    (if (equal chapter this-chapter)
		(progn
		  (unless (member (char-before) '(#x20 #x0a #x0d))
		    (insert #x20))
		  (dtk-verse-inserter book chapter verse text nil nil))
	      ;; new chapter
	      (progn
		(insert #xa #xa)
		(setf this-chapter chapter)
		(dtk-verse-inserter book chapter verse text nil t))))))))
 
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

(defun dtk-preview-citation ()
  "Preview citation at point."
  (interactive)
  ;; lazy man's preview -- append at end of *dtk* buffer so at least it's readable
  (with-current-buffer dtk-buffer-name
    ;; (move-point-to-end-of-dtk-buffer)
    (goto-char (point-max))
    ;; (add-vertical-line-at-end-of-dtk-buffer)
    (insert #xa))
  ;; (back-to-point-in-search/current-buffer)
  (dtk-follow))

(defun dtk-quit ()
  "Quit."
  (interactive)
  (when (member (buffer-name (current-buffer))
		(list dtk-buffer-name dtk-dict-buffer-name dtk-search-buffer-name))
    (kill-buffer nil)))

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
  "Return a list of Sword modules from diatheke. The list is an alist where the key is the module category and the value is an alist where the key is the module abbreviation and the value is the corresponding string description."
  (let ((modules-by-category nil)
	(module-category nil))
    (let ((abbrevs-descriptions nil))
      (cl-loop for line in (s-lines (with-temp-buffer
                                      (call-process dtk-program nil '(t nil) nil
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


(defun dtk-sto--diatheke-parse-text (text &optional keep-newlines-p)
  "Parse TEXT line-by-line, returning list of verse plists.
When KEEP-NEWLINES-P is non-nil, keep blank lines in text.

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
						;; If TEXT is already defined and LINE isn't empty, add a space to avoid directly concatenating two words. This assumes diatheke only breaks at puncutation or word boundaries.
                                                (concat " " line)
                                              (when keep-newlines-p "\n")))))
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

(defun dtk--parse-osis-xml-lines (text)
  "Parse the string TEXT, assuming it is a set of lines in the OSIS output format generated by diatheke."
  (let ((lines (s-lines text)))
    (let ((lines-n (length lines))
	  (parsed nil)
	  (current-line-n 0))
      (while (< current-line-n lines-n)
	;; consume lines associated with a single verse
	(multiple-value-bind (last-line-parsed-n parsed-verse)
	    (dtk--diatheke-parse-osis-xml-for-verse lines current-line-n)
	  (if parsed-verse
	      (push parsed-verse parsed))
	  (setf current-line-n (1+ last-line-parsed-n))))
      (nreverse parsed))))

(defvar dtk-parse-osis-ignore-regexp
  (regexp-opt '("^Unprocessed Token:"))
  "Regular expression describing lines to be ignored in diatheke OSIS output.")

(defun dtk--diatheke-parse-osis-xml-for-verse (lines n)
  "Consume lines associated with a single verse. Return multiple values where where the first value is the index of the last line consumed in parsing a single verse and the second value is a plist associated with a single verse. Use list of strings, LINES, starting at list element N. If an indication of the beginning of a verse is not encountered at element N, return nil."
  ;; Anticipate LINES to correspond to what diatheke coughs up. For a single verse of a "Bible text", this is typically one or more lines of the form
  ;; II Peter 3:15: <w lemma=\"strong:G3588 lemma.TR:την\" morph=\"robinson:T-ASF\" src=\"2\" wn=\"001\"/><w lemma=\"strong:G2532 lemma.TR:και\" morph=\"robinson:CONJ\" src=\"1\" wn=\"002\">And</w> <w lemma=\"strong:G2233 lemma.TR:ηγεισθε\" morph=\"robinson:V-PNM-2P\" src=\"8\" wn=\"003\">account</w> ...
  ;;
  ;; Note that diatheke may emit, for a single verse, a set of lines of the form
  ;; II Peter 3:15: Unprocessed Token: <br /> in key II Peter 3:15
  ;; Unprocessed Token: <br /> in key II Peter 3:15
  ;; ...
  (let ((line (elt lines n))
	(current-line-n n)
	(last-line-n (1- (length lines))))
    (when (s-present? line)
      (when (string-match dtk-sto--diatheke-parse-line-regexp line)
	(let ((book (match-string 1 line))
	      (chapter (string-to-number (match-string 2 line)))
	      (verse (string-to-number (match-string 3 line)))
	      ;; Ensure text is present, which may not be the case if
	      ;; a verse starts with a newline.  See
	      ;; <https://github.com/alphapapa/sword-to-org/issues/2>
	      (first-line-raw-text (when (s-present? (match-string 4 line))
				     (s-trim (match-string 4 line))))
	      (text-raw ""))
	  ;; Once initial line associated with verse has been dealt
	  ;; with, modify the initial line so that it, along with
	  ;; every subsequent line can be handled in the same manner.
	  (when book
	    (setf (elt lines n) first-line-raw-text))
	  ;; per-line processing
	  (do ((ignorep
		;; discard/ignore some classes of diatheke OSIS output
		(string-match dtk-parse-osis-ignore-regexp (elt lines current-line-n))
		(string-match dtk-parse-osis-ignore-regexp (elt lines current-line-n))))
	      (nil nil)
	    (unless ignorep
	      (setf text-raw
		    (concatenate 'string text-raw (elt lines current-line-n))))
	    (when (or (>= current-line-n last-line-n)
		      ;; check if next line corresponds to start of a new verse
		      (string-match dtk-sto--diatheke-parse-line-regexp (elt lines (1+ current-line-n))))
	      (return))
	    (incf current-line-n))
	  ;; Add root element and parse text as a single piece of XML
	  (let ((text-structured (with-temp-buffer
				   (insert "<r>" text-raw "</r>")
				   (xml-parse-region))))
            (values current-line-n
		    (list
		     :book book :chapter chapter :verse verse
		     :text (subseq (car text-structured) 2)))))))))

;;
;; dictionary: handle dictionary entries and references
;;
(defface dtk-dict-word
  '((t ()))
  "Face for a word or phrase with a corresponding dictionary entry."
  :group 'dtk-faces)

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
  "Show the current dictionary entry."
  (get-buffer-create dtk-dict-buffer-name) ;(dtk-ensure-dict-buffer-exists)
  ;;(dtk-clear-dict-buffer)
  (with-current-buffer dtk-dict-buffer-name
    (delete-region (progn (goto-char (point-min)) (point))
		   (progn (goto-char (point-max)) (point))))
  (switch-to-buffer dtk-dict-buffer-name) ;(dtk-switch-to-dict-buffer)
  (dtk-dict-mode)
  ;; insert dtk-dict-content
  (insert dtk-dict-word #xa dtk-dict-def #xa)
  (mapc #'(lambda (cr) (insert cr #xa))
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

(defcustom dtk-show-dict-numbers nil
  "If true, show dictionary numbers, if available. Otherwise, ensure dictionary information is not visible.")

;;
;; dtk major mode
;;

;;
;; font lock
(defcustom dtk-books-font-lock-variable-name-face-string
  (concat "^\\("
	  (mapconcat #'(lambda (book)
			 book)
		     ;; treat last book differently
		     dtk-books ; (butlast dtk-books 1)
		     "\\|")          
	  "\\)")
  "Facilitate font lock in dtk major mode for books in DTK-BOOKS.")

;; these could use some TLC/refinement
(defconst dtk-font-lock-keywords
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
   (list dtk-module))
  "List of font lock keywords for dtk major mode.")

(defface dtk-full-book
  '((t (:background "gray50" :foreground "red" :height 1.2)))
  "Face for book component of a full citation."
  :group 'dtk-faces)

(defface dtk-full-verse-number
  '((t (:background nil :height 1.2)))
  "Face for marking verse number component of a full citation."
  :group 'dtk-faces)

(defface dtk-compact-verse-number
  '((t (:background nil :height 0.8)))
  "Face for marking verse number."
  :group 'dtk-faces)

(defun dtk-make-overlay-verse-number (beg end)
  "Make an overlay for the verse number beginning at point BEG and ending at point END."
  (let ((ov (make-overlay beg end
			  (get-buffer dtk-buffer-name)
			  t t)))
    (overlay-put ov 'face 'dtk-verse-number)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'dtk-overlay t)
    ov))

(defvar dtk-mode-map
  (let ((map (make-keymap)))
    (define-key map "c" 'dtk-clear-dtk-buffer)
    (define-key map "b" 'dtk-backward-verse)
    (define-key map "g" 'dtk-go-to)
    (define-key map "f" 'dtk-forward-verse)
    (define-key map "m" 'dtk-select-module)
    (define-key map "M" 'dtk-select-module-category)
    (define-key map "s" 'dtk-search)
    (define-key map "S" 'dtk-show-dict-entry)
    (define-key map "q" 'dtk-quit)
    (define-key map "x" 'dtk-follow)
    (define-key map (kbd "C-M-b") 'dtk-backward-chapter)
    (define-key map (kbd "C-M-f") 'dtk-forward-chapter)
    map)
  "Keymap for in dtk buffer.")

;;;###autoload
(define-derived-mode dtk-mode text-mode "dtk"
  "Major mode for displaying dtk text
\\{dtk-mode-map}
Turning on dtk mode runs `text-mode-hook', then `dtk-mode-hook'."
  (setq font-lock-defaults '(dtk-font-lock-keywords))
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq word-wrap dtk-word-wrap)
  )

(defun dtk-to-verse-number-font (beg end)
  "Make an overlay for the verse number beginning at point BEG and ending at point END. Modify the text properties of the verse number to enhance readability."
  (with-current-buffer dtk-buffer-name
    (dtk-make-overlay-verse-number beg end)
    (add-text-properties
     beg end
     '(display (raise 0.2)))))

;;;###autoload
(define-derived-mode dtk-search-mode dtk-mode "dtk-search"
  "Major mode for interacting with dtk search results.")

;;;###autoload
(define-derived-mode dtk-dict-mode dtk-mode "dtk-dict"
  "Major mode for interacting with dtk dict results.")

(defvar dtk-search-mode-map
  (let ((map (make-keymap)))
    (define-key map [return] 'dtk-preview-citation)
    map)
  "Keymap for dtk search buffer.")

;;; navigating (by book, chapter, and verse)
(defun dtk-at-verse-citation? ()
  "Return a true value if point is at a verse citation."
  ;; could be at a compact citation or at a full citation (e.g., 'John 1:1')

  ;; if at a numeral character, assume at a verse or chapter number
  (or (dtk-number-at-point (point))
      (dtk-at-verse-full-citation?)))

(defun dtk-at-verse-full-citation? ()
  "Return a true value if point is at a full verse citation."
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
  "Return a true value if point is at the colon character in a full verse citation."
  ;; if at a colon, is it between two numerals?
  (and (= 58 (char-after (point)))	; colon character?
       (dtk-number-at-point (1+ (point)))
       (dtk-number-at-point (1- (point)))))

(defun dtk-at-verse-full-citation-chapter? ()
  "Return a true value if point appears to be at the numeral(s) indicating the chapter number in a full verse citation."
  (looking-at "[[:digit:]]+:[[:digit:]]"))

(defun dtk-at-verse-full-citation-verse? ()
  "Return a true value if point appears to be at the numeral(s) indicating the verse number in a full verse citation."
  (and (dtk-number-at-point (point))
       ;; might be at third digit of a verse...
       (looking-back "[[:digit:]]:[[:digit:]]?[[:digit:]]?" 5)))

(defun dtk-at-verse-full-citation-space-or-book? ()
  "Return a true value if point appears to be at the space preceding a full verse citation or at the start of a full verse citation."
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

(defun dtk-backward-chapter ()
  "Move to the previous chapter. Behavior is undefined if the current chapter is not
preceded by a different chapter."
  (interactive)
  ;; If at whitespace w/o chapter property, or if at end of buffer, move backward until chapter defined
  (dtk-backward-until-chapter-defined)
  (let ((current-chapter (get-text-property (point) 'chapter)))
    (dtk-backward-until-defined-chapter-not-equal current-chapter)))

(defun dtk-backward-until-chapter-defined ()
  "If the chapter text property is not defined at point, move backward
until at a position where the chapter text property is defined. Return
NIL if a position does not exist forward of point where the chapter
text property changes."
  (interactive)
  (cond ((bobp)
	 nil)
	((not (get-text-property (point) 'chapter))
	 (let ((changes-at-point (previous-single-property-change (1+ (point)) 'chapter)))
	   (if changes-at-point
	       (goto-char changes-at-point))))))

(defun dtk-backward-until-defined-chapter-not-equal (x &optional start-point)
  "Move forward past the either START-POINT (if non-nil) or current
point if, at some point before the current point, the chapter text
property value is defined and the test for equality between the
chapter text property value and X does not return a true value."
  (let ((changes-at-point (previous-single-property-change (or start-point
							       (point))
							   'chapter)))
    (when changes-at-point
      (let ((new-chapter-text-property (get-text-property changes-at-point 'chapter)))
	(cond ((and new-chapter-text-property
		    (not (equal (get-text-property changes-at-point 'chapter)
				x)))
	       (goto-char changes-at-point))
	      ;; Possibly at whitespace
	      (t (dtk-backward-until-defined-chapter-not-equal x changes-at-point)))))))

(defun dtk-backward-verse ()
  "Move to the numeric component of the verse citation for the previous verse."
  (interactive)
  (dtk-previous-verse)
  (dtk-to-start-of-current-verse))

(defun dtk-previous-verse ()
  "Move to the previous verse. No assurance is offered with respect to the exact location of point within the preceding verse after invoking DTK-PREVIOUS-VERSE."
  (interactive)
  ;; It is possible that point is currently at whitespace not
  ;; associated with a verse; if so, move until the 'verse property is
  ;; defined and treat that verse value as the current verse.
  (dtk-back-until-verse-defined)
  (dtk-previous-verse-change)
  ;; As above, it's possible point is at a position where
  ;; the 'verse property is not defined.
  (dtk-back-until-verse-defined))

(defun dtk-back-until-verse-defined ()
  "If the verse text property is not defined at point, back up until at a position where the verse text property is defined."
  (interactive)
  (if (not (get-text-property (point) 'verse))
      (goto-char
       (1- (previous-single-property-change
	    (if (eobp)
		(point)
	      (1+ (point)))
	    'verse)))))

(defun dtk-previous-verse-change ()
  "Move to the point at which the 'verse text property assumes a different value (relative to the 'verse text property at the current point). Return the point at which the 'verse text property changed or, if the property does not change prior to the current point, return NIL."
  (interactive)
  (let ((verse-changes-at (previous-single-property-change (1+ (point)) 'verse)))
    (when verse-changes-at
      (goto-char (1- verse-changes-at)))
    verse-changes-at))

(defun dtk-forward-chapter ()
  "Move to the next chapter (the point at which the chapter text
property changes to a new value distinct from the current chapter text
property value). Behavior is undefined if the current chapter is not
succeeded by a different chapter."
  (interactive)
  ;; If at whitespace w/o chapter property, move forward until chapter defined
  (dtk-forward-until-chapter-defined)
  (let ((current-chapter (get-text-property (point) 'chapter)))
    (dtk-forward-until-defined-chapter-not-equal current-chapter)))

(defun dtk-forward-until-chapter-defined ()
  "If the chapter text property is not defined at point, move forward
until at a position where the chapter text property is defined. Return
NIL if a position does not exist forward of point where the chapter
text property changes."
  (interactive)
  (cond ((eobp)
	 nil)
	((not (get-text-property (point) 'chapter))
	 (let ((changes-at-point (next-single-property-change (1+ (point)) 'chapter)))
	   (if changes-at-point
	       (goto-char changes-at-point))))))

(defun dtk-forward-until-defined-chapter-not-equal (x &optional start-point)
  "Move forward past the either START-POINT (if non-nil) or current
point if, at some point past the current point, the chapter text
property value is defined and the test for equality between the
chapter text property value and X does not return a true value."
  (let ((changes-at-point (next-single-property-change (1+ (or start-point
							       (point)))
						       'chapter)))
    (when changes-at-point
      (let ((new-chapter-text-property (get-text-property changes-at-point 'chapter)))
	(cond ((and new-chapter-text-property
		    (not (equal (get-text-property changes-at-point 'chapter)
				x)))
	       (goto-char changes-at-point))
	      ;; Possibly at whitespace
	      (t (dtk-forward-until-defined-chapter-not-equal x changes-at-point)))))))

(defun dtk-forward-verse ()
  "Move to the numeric component of the verse citation for the next verse."
  (interactive)
  (goto-char (next-single-property-change (point) 'verse))
  ;; it is possible that whitespace is present not associated with a verse; if that's the case move forward until the 'verse property is defined
  (if (not (get-text-property (point) 'text))
      (goto-char (next-single-property-change (point) 'verse))))

(defun dtk-to-start-of-current-verse ()
  "Move to the start (beginning of the citation) of the current verse."
  (interactive)
  (let ((verse-changes-at (dtk-previous-verse-change)))
    (cond (verse-changes-at
	   (forward-char))
	  ;; In less-than-ideal circumstances, VERSE-CHANGES-AT assumes a value of NIL when at the first verse. Ugly kludges include moving to start of buffer or searching back for the first numeric character encountered.
	  (t
	   (beginning-of-buffer)))))

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

(defun dtk-number-at-point (&optional point)
  "A more flexible version of NUMBER-AT-POINT. POINT optionally specifies a point."
  (let ((char-code (char-after (or point (point)))))
    (and (>= char-code 48)
	 (<= char-code 57))))

;;;
;;; establish defaults (relying on dtk code)
;;;
(unless dtk-module-category
  (setf dtk-module-category "Biblical Texts"))
(unless dtk-module
  (setf dtk-module (elt (dtk-module-names dtk-module-category) 0)))

(provide 'dtk)
;;; dtk.el ends here
