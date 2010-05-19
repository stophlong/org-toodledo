;; w3m requires many parts.  This excerpt only contains material needed for w3m-url-encode-string.
;; This is sufficient for org-toodledo
;; Excerpt by Stoph Long 2010.

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>


(defvar w3m-type nil
  "Type of the w3m command.
The valid values include `w3m', `w3mmee', and `w3m-m17n'.")

(defcustom w3m-language
  (if (or (and (boundp 'current-language-environment)
	       (string= "Japanese"
			(symbol-value 'current-language-environment)))
	  (boundp 'MULE))
      "Japanese")
  "*Your preferred language used in emacs-w3m sessions."
  :group 'w3m
  :type '(radio (const :format "%v " "Japanese")
		(const :tag "Other" nil))
  :get (lambda (symbol)
	 (let ((value (format "%s" (default-value symbol)))
	       (case-fold-search t))
	   (prog1
	       (setq value (if (string-match "\\`japan" value) "Japanese"))
	     (set-default symbol value))))
  :set (lambda (symbol value)
	 (set-default symbol (if (equal value "Japanese") "Japanese"))))

(defcustom w3m-coding-system (if (featurep 'mule)
				 (if (eq w3m-type 'w3mmee)
				     'iso-2022-7bit-ss2
				   'iso-2022-7bit)
			       'iso-8859-1)
  "*Default coding system used to communicate with the w3m command."
  :group 'w3m
  :type '(coding-system :size 0))

(defcustom w3m-default-coding-system
  (if (equal "Japanese" w3m-language) 'shift_jis 'iso-8859-1)
  "*Default coding system used to encode url strings and post-data."
  :group 'w3m
  :type '(coding-system :size 0))

(defun w3m-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "+")
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string into a list of chars.
	  (append (encode-coding-string (or str "")
					(or coding
					    w3m-default-coding-system
					    w3m-coding-system
					    'iso-2022-7bit))
		  nil))))
