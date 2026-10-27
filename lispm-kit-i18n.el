;;; lispm-kit-i18n.el --- Programs' words, in the language spoken  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-kit
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A program's words are written in English in its code and shown in
;; the language it speaks, from catalogs of its own, one a language.
;; What finds that language, reads the catalogs and translates is here,
;; once for every program: each keeps only its catalogs, and says once
;; where they are and how it chooses its language, as a domain, a
;; symbol naming its words:
;;
;;   (lispm-kit-i18n-define 'lispm-mail
;;     :directory (expand-file-name "i18n" lispm-mail-directory)
;;     :language 'lispm-mail-language
;;     :language-functions 'lispm-mail-language-functions
;;     :changed-hook 'lispm-mail-language-changed-hook)
;;
;; A domain speaks the language its :language variable names, else that
;; of the program it is shown by, which a function of its
;; :language-functions says, else the computer's own; once that
;; changes, its :changed-hook runs, to draw again what shows words.
;; Every word a person reads passes through one of these, which a
;; program calls through short names of its own, as LispM Agent's
;; `lispm-tr' calls `lispm-kit-tr' with its domain, `lispm':
;;
;;   (lispm-kit-tr DOMAIN MESSAGE ARGS...)
;;                            MESSAGE translated, then formatted with
;;                            ARGS, if any
;;   (lispm-kit-tr-n DOMAIN SINGULAR PLURAL COUNT ARGS...)
;;                            the form for COUNT, formatted with ARGS,
;;                            or with COUNT
;;   (lispm-kit-tr-in DOMAIN CONTEXT MESSAGE ARGS...)
;;                            MESSAGE as meant in CONTEXT, where the
;;                            same English says two things
;;   (lispm-kit-format-time DOMAIN FORMAT TIME)
;;                            TIME as FORMAT, translated, says it, the
;;                            names of months and days in the language
;;
;; Words kept in a list made as a file loads stay English there, each
;; marked with `lispm-kit-tr-noop', and are translated where shown, so
;; that a change of language shows at once; `(lispm-kit-tr-noop MESSAGE
;; CONTEXT)' keeps its context with it.  A translation may take the
;; arguments in another order with `%1$s'.  What models read stays
;; English, see `lispm-kit-in-english'.
;;
;; A domain's catalogs are the files of its :directory, one per
;; language, named by its tag, as zh-Hans.eld:
;;
;;   (:name "NAME" :messages ((MESSAGE . TRANSLATION) ...))
;;
;; NAME is the language's name in itself.  A MESSAGE said in a context is
;; (CONTEXT . MESSAGE); a plural's TRANSLATION is a list of its forms, as
;; many as the language has, see `lispm-kit-i18n-plural-forms'.  A
;; message missing from the catalog shows as written.  The English
;; catalogs hold only what their spelling changes: LispM's English
;; spells as Britain does.  lispm-kit-i18n-check.el checks a domain's
;; catalogs against its code.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun lispm-kit-tr-noop (message &optional context)
  "Return MESSAGE as it is, marking it as words to translate where shown.
For words kept as a file loads, see lispm-kit-i18n.el.  MESSAGE as meant
in CONTEXT carries it, and `lispm-kit-tr' translates it so."
  (if context (propertize message 'lispm-kit-i18n-context context) message))

(defconst lispm-kit-languages
  `((en-US ,(lispm-kit-tr-noop "English (US)")) (en-GB ,(lispm-kit-tr-noop "English (UK)"))
    (de ,(lispm-kit-tr-noop "German")) (fr ,(lispm-kit-tr-noop "French"))
    (ja ,(lispm-kit-tr-noop "Japanese"))
    (zh-Hans ,(lispm-kit-tr-noop "Chinese, Simplified"))
    (zh-Hant ,(lispm-kit-tr-noop "Chinese, Traditional")))
  "The languages LispM speaks: (TAG ENGLISH-NAME).  TAG names a domain's
catalog; a program showing ENGLISH-NAME translates it as its own words.")

(defconst lispm-kit-i18n-plural-forms
  '((de . lispm-kit-i18n--one-else-other)
    (en-GB . lispm-kit-i18n--one-else-other)
    (en-US . lispm-kit-i18n--one-else-other)
    (fr . lispm-kit-i18n--up-to-one-else-other))
  "How each language picks the plural form of a count: (TAG . FUNCTION),
FUNCTION returning the index of the form for a count.  A language not
listed has one form for every count, as Japanese and Chinese.")

(defun lispm-kit-i18n--one-else-other (count)
  "Return the plural form for COUNT where one is singular: 0 or 1."
  (if (= count 1) 0 1))

(defun lispm-kit-i18n--up-to-one-else-other (count)
  "Return the plural form for COUNT where 0 and 1 are singular, as in
French: 0 or 1."
  (if (< (abs count) 2) 0 1))

(defconst lispm-kit-i18n-locales
  '((en-US . "en_US.UTF-8") (en-GB . "en_GB.UTF-8") (de . "de_DE.UTF-8") (fr . "fr_FR.UTF-8")
    (ja . "ja_JP.UTF-8") (zh-Hans . "zh_CN.UTF-8") (zh-Hant . "zh_TW.UTF-8"))
  "The locale each language names months and days in: (TAG . LOCALE).")

;;;; Domains

(defvar lispm-kit-i18n--catalogs nil
  "The catalogs read so far: (DOMAIN (TAG . CATALOG) ...), CATALOG the
file's plist with its :messages in a hash table.")

(defvar lispm-kit-i18n--spoken nil
  "The domains whose :language variable was set, while that waits for the
command setting it to end: (DOMAIN . LANGUAGE), LANGUAGE the one spoken
before.")

(defun lispm-kit-i18n--get (domain property)
  "Return PROPERTY of DOMAIN, as `lispm-kit-i18n-define' gave it."
  (plist-get (get domain 'lispm-kit-i18n) property))

(defun lispm-kit-i18n-define (domain &rest properties)
  "Define DOMAIN, a symbol, as a program's words, and return it.
PROPERTIES:

  :directory DIRECTORY   where its catalogs are, one per language
  :language VARIABLE     a variable naming the language chosen for it,
                         a tag of `lispm-kit-languages', or nil to
                         follow the program it is shown by, else the
                         computer
  :language-functions VARIABLE
                         a hook variable: functions of no arguments
                         returning the language of the program it is
                         shown by, a tag, or nil
  :changed-hook VARIABLE a hook variable run once it speaks another
                         language, to draw again what shows words
  :watch WATCH           t, the default, follows its :language variable
                         as it is set; nil waits for
                         `lispm-kit-i18n-watch'

Defined again, it takes the new PROPERTIES and reads its catalogs anew."
  (unless (plist-get properties :directory)
    (error "Domain %s says no :directory" domain))
  (put domain 'lispm-kit-i18n properties)
  (setf (alist-get domain lispm-kit-i18n--catalogs nil t) nil)
  (when (if (plist-member properties :watch) (plist-get properties :watch) t)
    (lispm-kit-i18n-watch domain))
  domain)

(defun lispm-kit-i18n-watch (domain)
  "Follow DOMAIN's :language variable as it is set: once the command
setting it ends, DOMAIN's :changed-hook runs, if the language it speaks
has changed."
  (when-let* ((variable (lispm-kit-i18n--get domain :language)))
    (unless (memq domain (get variable 'lispm-kit-i18n-domains))
      (put variable 'lispm-kit-i18n-domains
           (cons domain (get variable 'lispm-kit-i18n-domains))))
    (add-variable-watcher variable #'lispm-kit-i18n--changed)))

;;;; The computer's language

(defvar lispm-kit-i18n--system-languages 'unread
  "The computer's preferred languages, most preferred first, once read.")

(defvar lispm-kit-i18n--system-language '(unread)
  "The computer's language once found: (LANGUAGES . LANGUAGE), LANGUAGE
the one LispM speaks of the preferred LANGUAGES it was found among.")

(defun lispm-kit-i18n--read-system-languages ()
  "Return the computer's preferred languages, most preferred first, as
language tags or locale names: macOS's AppleLanguages, else the
LANGUAGE, LC_ALL, LC_MESSAGES and LANG variables."
  (or (when (eq system-type 'darwin)
        (with-temp-buffer
          (when (eq 0 (ignore-errors (call-process "defaults" nil t nil
                                                   "read" "-g" "AppleLanguages")))
            (goto-char (point-min))
            (let ((tags nil))
              (while (re-search-forward "\"?\\([A-Za-z]+\\(?:[-_][A-Za-z0-9]+\\)*\\)\"?[,\n]" nil t)
                (push (match-string 1) tags))
              (nreverse tags)))))
      (seq-mapcat (lambda (variable)
                    (when-let* ((value (getenv variable)))
                      (split-string value ":" t)))
                  '("LANGUAGE" "LC_ALL" "LC_MESSAGES" "LANG"))))

(defun lispm-kit-i18n-language-of (tag)
  "Return the language of `lispm-kit-languages' TAG asks for, or nil.
TAG is a language tag, as zh-Hant-HK, or a locale, as zh_TW.UTF-8.
Chinese without a script is Simplified, except as written in Hong Kong,
Macao and Taiwan; English is American in the United States and without
a region, else British."
  (save-match-data
    (lispm-kit-i18n--language-of tag)))

(defun lispm-kit-i18n--language-of (tag)
  "Return the language of `lispm-kit-languages' TAG asks for, or nil, the
match data changed; see `lispm-kit-i18n-language-of'."
  (let* ((parts (split-string (car (split-string tag "[.@]")) "[-_]" t))
         (language (downcase (or (car parts) "")))
         (rest (mapcar (lambda (part) (if (= (length part) 4) (capitalize part) (upcase part)))
                       (cdr parts)))
         (script (seq-find (lambda (part) (= (length part) 4)) rest))
         (region (seq-find (lambda (part) (/= (length part) 4)) rest)))
    (pcase language
      ("zh" (cond ((equal script "Hant") 'zh-Hant)
                  ((equal script "Hans") 'zh-Hans)
                  ((member region '("HK" "MO" "TW")) 'zh-Hant)
                  (t 'zh-Hans)))
      ("en" (if (member region '(nil "US")) 'en-US 'en-GB))
      ("ja" 'ja)
      ("fr" 'fr)
      ("de" 'de))))

(defun lispm-kit-system-language ()
  "Return the computer's language, the first of its preferred languages
that LispM speaks, else American English.  It is found once, as every
word shown asks for it while no language is chosen; the caller's match
data is kept."
  (when (eq lispm-kit-i18n--system-languages 'unread)
    (setq lispm-kit-i18n--system-languages
          (save-match-data (lispm-kit-i18n--read-system-languages))))
  (let ((languages lispm-kit-i18n--system-languages))
    (unless (eq (car lispm-kit-i18n--system-language) languages)
      (setq lispm-kit-i18n--system-language
            (cons languages (or (seq-some #'lispm-kit-i18n-language-of languages) 'en-US)))))
  (cdr lispm-kit-i18n--system-language))

(defun lispm-kit-i18n--chosen (domain)
  "Return the language DOMAIN's :language variable names, or nil."
  (when-let* ((variable (lispm-kit-i18n--get domain :language)))
    (and (boundp variable) (symbol-value variable))))

(defun lispm-kit-current-language (domain)
  "Return the language DOMAIN speaks now, a tag: that its :language
variable names, else the first of `lispm-kit-languages' a function of
its :language-functions returns, else the computer's."
  (or (lispm-kit-i18n--chosen domain)
      (when-let* ((functions (lispm-kit-i18n--get domain :language-functions)))
        (and (boundp functions)
             (run-hook-wrapped functions
                               (lambda (function) (car (assq (funcall function) lispm-kit-languages))))))
      (lispm-kit-system-language)))

;;;; Catalogs

(defun lispm-kit-i18n-catalog-file (domain language)
  "Return the file of DOMAIN's catalog of LANGUAGE."
  (expand-file-name (format "%s.eld" language)
                    (or (lispm-kit-i18n--get domain :directory)
                        (error "No words are defined as %s" domain))))

(defun lispm-kit-i18n-key (message context)
  "Return the key MESSAGE in CONTEXT, a string or nil, has in a catalog's
table."
  (if context (concat context "\4" message) message))

(defun lispm-kit-i18n-read-catalog (domain language)
  "Read DOMAIN's catalog of LANGUAGE from its file: its plist, with its
:messages, (MESSAGE . TRANSLATION), in a hash table keyed by
`lispm-kit-i18n-key'.  A language without a file has an empty one."
  (let ((file (lispm-kit-i18n-catalog-file domain language))
        (table (make-hash-table :test #'equal)))
    (let ((catalog (when (file-readable-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (read (current-buffer))))))
      (dolist (entry (plist-get catalog :messages))
        (let ((message (car entry)))
          (puthash (if (consp message)
                       (lispm-kit-i18n-key (cdr message) (car message))
                     message)
                   (cdr entry) table)))
      (plist-put (copy-sequence catalog) :messages table))))

(defun lispm-kit-i18n-catalog (domain &optional language)
  "Return DOMAIN's catalog of LANGUAGE, by default of the language it
speaks, read once."
  (let* ((language (or language (lispm-kit-current-language domain)))
         (catalogs (assq domain lispm-kit-i18n--catalogs)))
    (or (alist-get language (cdr catalogs))
        (let ((catalog (lispm-kit-i18n-read-catalog domain language)))
          (if catalogs
              (push (cons language catalog) (cdr catalogs))
            (push (list domain (cons language catalog)) lispm-kit-i18n--catalogs))
          catalog))))

(defun lispm-kit-language-name (domain language)
  "Return the name of LANGUAGE in itself, as DOMAIN's catalog gives it."
  (or (plist-get (lispm-kit-i18n-catalog domain language) :name)
      (cadr (assq language lispm-kit-languages))
      (symbol-name language)))

(defun lispm-kit-i18n-forget ()
  "Forget the catalogs read and the computer's languages, so they are read
again when next needed."
  (setq lispm-kit-i18n--catalogs nil
        lispm-kit-i18n--system-languages 'unread
        lispm-kit-i18n--system-language '(unread)))

;;;; Translating

(defvar lispm-kit-i18n--english nil
  "Non-nil while words are said as written, in English; see
`lispm-kit-in-english'.")

(defmacro lispm-kit-in-english (&rest body)
  "Run BODY saying every word as written, in English, and return its value.
What models read is said so: a tool's result, and the error of a
function the tool shares with the user's commands."
  (declare (indent 0) (debug t))
  `(let ((lispm-kit-i18n--english t)) ,@body))

(defun lispm-kit-i18n--translation (domain message context)
  "Return the translation of MESSAGE in CONTEXT in DOMAIN's catalog, or
nil."
  (unless lispm-kit-i18n--english
    (gethash (lispm-kit-i18n-key message context)
             (plist-get (lispm-kit-i18n-catalog domain) :messages))))

(defun lispm-kit-tr-in (domain context message &rest args)
  "Return MESSAGE, as meant in CONTEXT, a string or nil, in the language
DOMAIN speaks, formatted with ARGS when there are any."
  (let ((translation (lispm-kit-i18n--translation domain message context)))
    (let ((text (if (stringp translation) translation message)))
      (if args (apply #'format text args) text))))

(defun lispm-kit-tr (domain message &rest args)
  "Return MESSAGE in the language DOMAIN speaks, formatted with ARGS when
there are any.  MESSAGE is the English written in the code; see
lispm-kit-i18n.el.  A MESSAGE marked with a context by
`lispm-kit-tr-noop' is translated as meant in it."
  (apply #'lispm-kit-tr-in domain
         (and (> (length message) 0) (get-text-property 0 'lispm-kit-i18n-context message))
         message args))

(defun lispm-kit-tr-n (domain singular plural count &rest args)
  "Return the form of SINGULAR or PLURAL for COUNT in the language DOMAIN
speaks, formatted with ARGS, or with COUNT when there are none."
  (let* ((translation (lispm-kit-i18n--translation domain singular nil))
         (rule (alist-get (lispm-kit-current-language domain) lispm-kit-i18n-plural-forms))
         (text (cond ((stringp translation) translation)
                     (translation (or (nth (if rule (funcall rule count) 0) translation)
                                      (car (last translation))))
                     ((= count 1) singular)
                     (t plural))))
    (apply #'format text (or args (list count)))))

(defun lispm-kit-format-time (domain format &optional time)
  "Return TIME, by default now, as FORMAT, translated, says it, the names
of months and days in the language DOMAIN speaks; see
`format-time-string'."
  (let ((system-time-locale (alist-get (lispm-kit-current-language domain) lispm-kit-i18n-locales)))
    (format-time-string (lispm-kit-tr domain format) time)))

;;;; Changing the language

(defun lispm-kit-i18n--changed-hook (domain)
  "Run DOMAIN's :changed-hook, if it has one."
  (when-let* ((hook (lispm-kit-i18n--get domain :changed-hook)))
    (run-hooks hook)))

(defun lispm-kit-i18n--changed (variable _value operation where)
  "Run the :changed-hook of each domain whose :language variable is
VARIABLE once it is set, as the command setting it ends, when the
language the domain speaks changes: once for all the settings it set.
This is a variable watcher; OPERATION and WHERE are those of
`add-variable-watcher'."
  (when (and (eq operation 'set) (null where))
    (dolist (domain (get variable 'lispm-kit-i18n-domains))
      (unless (assq domain lispm-kit-i18n--spoken)
        (push (cons domain (lispm-kit-current-language domain)) lispm-kit-i18n--spoken)
        (run-at-time 0 nil #'lispm-kit-i18n--speak domain)))))

(defun lispm-kit-i18n--speak (domain)
  "Run DOMAIN's :changed-hook if the language it speaks is another than
the one it spoke before its :language variable was set."
  (when-let* ((spoken (assq domain lispm-kit-i18n--spoken)))
    (setq lispm-kit-i18n--spoken (delq spoken lispm-kit-i18n--spoken))
    (unless (eq (cdr spoken) (lispm-kit-current-language domain))
      (lispm-kit-i18n--changed-hook domain))))

(defun lispm-kit-language-changed (domain)
  "Draw again what shows DOMAIN's words, now that the language a function
of its :language-functions returns has changed, unless its :language
variable chooses one: a program saying its language calls this once it
speaks another."
  (unless (or (lispm-kit-i18n--chosen domain) (assq domain lispm-kit-i18n--spoken))
    (lispm-kit-i18n--changed-hook domain)))

(defun lispm-kit-i18n-set (domain language &optional save)
  "Make DOMAIN speak LANGUAGE now: set its :language variable to it,
saved for every later start too when SAVE is non-nil, and run its
:changed-hook at once, if the language it speaks changes."
  (let ((variable (or (lispm-kit-i18n--get domain :language)
                      (error "The words of %s say no :language variable" domain)))
        (from (lispm-kit-current-language domain)))
    ;; Shown here, not by the watcher a second time.
    (let ((lispm-kit-i18n--spoken (cons (cons domain from) lispm-kit-i18n--spoken)))
      (if save
          (customize-save-variable variable language)
        (set-default variable language)))
    (unless (eq from (lispm-kit-current-language domain))
      (lispm-kit-i18n--changed-hook domain))))

(provide 'lispm-kit-i18n)
;;; lispm-kit-i18n.el ends here
