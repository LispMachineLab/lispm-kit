;;; lispm-kit-i18n-check.el --- A program's catalogs checked against its code  -*- lexical-binding: t; -*-

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

;; What a program's tests check of its words, and what its `make i18n'
;; reports.  `lispm-kit-i18n-messages' finds the messages its code gives
;; to translate, by the names it translates with:
;;
;;   (lispm-kit-i18n-messages (directory-files "lisp" t "\\.el\\'")
;;     :tr 'lispm-tr :tr-n 'lispm-tr-n :tr-in 'lispm-tr-in
;;     :tr-noop 'lispm-tr-noop :format-time 'lispm-format-time)
;;
;; `lispm-kit-i18n-problems' says what its catalogs lack, or hold no
;; longer, against them, and `lispm-kit-i18n-report' prints it.  A
;; program's code needs nothing of this file: its tests require it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-kit-i18n)

(defconst lispm-kit-i18n-translated-languages '(de fr ja zh-Hans zh-Hant)
  "The languages whose catalogs translate every message.  The English
ones hold only what their spelling changes.")

(defconst lispm-kit-i18n-spellings
  '(("colour" . "color") ("centre" . "center") ("grey" . "gray") ("favourite" . "favorite")
    ("behaviour" . "behavior") ("cancelled" . "canceled") ("labelled" . "labeled"))
  "Words Britain and the United States spell apart: (BRITISH . AMERICAN),
the start of a word.  Customize stays, Emacs's name for its settings.")

(defun lispm-kit-i18n--time-format-p (text)
  "Return non-nil when TEXT is a `format-time-string' format: it has a
conversion `format' has not, as %H or %B."
  (let ((case-fold-search nil))
    (string-match-p "%[-_0^#]*[0-9]*[HMIpABbYymZz]" text)))

(defun lispm-kit-i18n-messages (files &rest names)
  "Return the messages FILES give to translate: a hash table from a
catalog key (see `lispm-kit-i18n-key') to a plist: :message, :context,
:plural, the plural's English, :time for a time format, and :places,
where the message is written, as FILE:LINE.  NAMES says what the
program translates with, each a function or a list of them, taking
what the function of lispm-kit-i18n.el does but the domain:

  :tr          as `lispm-kit-tr'
  :tr-n        as `lispm-kit-tr-n'
  :tr-in       as `lispm-kit-tr-in'
  :tr-noop     as `lispm-kit-tr-noop', which takes no domain anyway
  :format-time as `lispm-kit-format-time'

A message counts where a call says it as a string."
  (let* ((roles (cl-loop for (role functions) on names by #'cddr
                         append (mapcar (lambda (function) (cons function role))
                                        (ensure-list functions))))
         (pattern (concat "(" (regexp-opt (mapcar (lambda (role) (symbol-name (car role))) roles) t)
                          "[ \t\n]+\""))
         (messages (make-hash-table :test #'equal)))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (emacs-lisp-mode)
        (let ((name (file-name-nondirectory file)))
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (let ((role (alist-get (intern (match-string 1)) roles))
                  (start (match-beginning 0)))
              (unless (nth 8 (save-excursion (syntax-ppss start)))
                (let* ((line (line-number-at-pos start))
                       (form (save-excursion (goto-char start) (read (current-buffer))))
                       (args (cdr form))
                       (context (pcase role
                                  (:tr-in (pop args))
                                  (:tr-noop (nth 1 args))))
                       (message (car args))
                       (key (lispm-kit-i18n-key message context))
                       (entry (gethash key messages)))
                  (puthash key
                           (list :message message :context context
                                 :plural (or (plist-get entry :plural)
                                             (and (eq role :tr-n) (nth 1 args)))
                                 :time (or (plist-get entry :time) (eq role :format-time)
                                           (lispm-kit-i18n--time-format-p message))
                                 :places (append (plist-get entry :places)
                                                 (list (format "%s:%d" name line))))
                           messages))))))))
    messages))

(defun lispm-kit-i18n--format-arguments (text)
  "Return the arguments format string TEXT takes, as (INDEX . CONVERSION),
sorted: `%1$s' takes argument 1, and a plain `%s' the one after the
last taken."
  (let ((arguments nil) (next 1) (start 0))
    (while (string-match "%\\(?:\\([0-9]+\\)\\$\\)?[-+ #0]*[0-9]*\\(?:\\.[0-9]+\\)?\\([a-zA-Z%]\\)" text start)
      (setq start (match-end 0))
      (unless (equal (match-string 2 text) "%")
        (let ((index (if (match-string 1 text) (string-to-number (match-string 1 text)) next)))
          (push (cons index (match-string 2 text)) arguments)
          (setq next (1+ index)))))
    (sort arguments (lambda (a b) (< (car a) (car b))))))

(defun lispm-kit-i18n--spelled-p (text spellings)
  "Return non-nil when TEXT has a word starting as one of SPELLINGS."
  (let ((case-fold-search t))
    (seq-some (lambda (spelling) (string-match-p (concat "\\b" spelling) text)) spellings)))

(defun lispm-kit-i18n--stale (catalog messages)
  "Return the keys of CATALOG's table that MESSAGES lacks."
  (let ((stale nil))
    (maphash (lambda (key _) (unless (gethash key messages) (push key stale))) catalog)
    (nreverse stale)))

(defun lispm-kit-i18n--missing (language catalog messages)
  "Return the keys of MESSAGES that CATALOG's table, LANGUAGE's, lacks, or
whose plural it gives in another number of forms than LANGUAGE has."
  (let ((forms (if (alist-get language lispm-kit-i18n-plural-forms) 2 1))
        (missing nil))
    (maphash (lambda (key entry)
               (let ((translation (gethash key catalog)))
                 (unless (and translation
                              (if (plist-get entry :plural)
                                  (or (and (stringp translation) (= forms 1))
                                      (and (consp translation) (= (length translation) forms)))
                                (stringp translation)))
                   (push key missing))))
             messages)
    (nreverse missing)))

(defun lispm-kit-i18n--wrong-arguments (catalog messages)
  "Return (KEY TEXT) for each translation in CATALOG's table taking other
arguments than its message in MESSAGES, in any order; a time format may
say the time its own way, and a plural's form leave the count unsaid, as
\"one item\" does."
  (let ((wrong nil))
    (maphash (lambda (key translation)
               (when-let* ((entry (gethash key messages))
                           ((not (plist-get entry :time))))
                 (let ((expected (lispm-kit-i18n--format-arguments (plist-get entry :message))))
                   (dolist (text (ensure-list translation))
                     (let ((arguments (lispm-kit-i18n--format-arguments text)))
                       (unless (or (equal arguments expected)
                                   (and (plist-get entry :plural)
                                        (equal arguments (cl-remove-if (lambda (a) (equal (cdr a) "d"))
                                                                       expected))))
                         (push (list key text) wrong)))))))
             catalog)
    (nreverse wrong)))

(defun lispm-kit-i18n--misspelled (language catalog messages)
  "Return the spelling problems of CATALOG's table, LANGUAGE's, against
MESSAGES: (spelled-as-written KEY) for a message spelled the other
English way that it does not change, and (spelling KEY TEXT) for a
translation keeping the other English's spelling.  Only American and
British English have them."
  (when-let* ((from (pcase language
                      ('en-US (mapcar #'car lispm-kit-i18n-spellings))
                      ('en-GB (mapcar #'cdr lispm-kit-i18n-spellings)))))
    (let ((problems nil))
      (maphash (lambda (key entry)
                 (dolist (text (cons (plist-get entry :message) (ensure-list (plist-get entry :plural))))
                   (when (and (lispm-kit-i18n--spelled-p text from) (not (gethash key catalog)))
                     (push (list 'spelled-as-written key) problems)))
                 (dolist (text (ensure-list (gethash key catalog)))
                   (when (lispm-kit-i18n--spelled-p text from)
                     (push (list 'spelling key text) problems))))
               messages)
      (nreverse (delete-dups problems)))))

(defun lispm-kit-i18n-problems (domain messages &optional kinds)
  "Return what is wrong with DOMAIN's catalogs, against MESSAGES its code
gives (see `lispm-kit-i18n-messages'): a list of problems, each a list
of the language, the kind, and what it is about:

  (LANGUAGE nameless)            the catalog says no :name
  (LANGUAGE stale KEY)           it holds a message the code no longer
                                 gives
  (LANGUAGE missing KEY)         a language of
                                 `lispm-kit-i18n-translated-languages'
                                 lacks a message, or a plural's forms
  (LANGUAGE arguments KEY TEXT)  a translation takes other arguments
                                 than its message
  (LANGUAGE spelled-as-written KEY)
                                 American or British English keeps a
                                 message spelled the other way
  (LANGUAGE spelling KEY TEXT)   ... or spells a translation so

KINDS, when given, is a list of the kinds wanted."
  (let ((problems nil))
    (dolist (language (mapcar #'car lispm-kit-languages))
      (let* ((catalog (lispm-kit-i18n-read-catalog domain language))
             (table (plist-get catalog :messages)))
        (unless (plist-get catalog :name)
          (push (list language 'nameless) problems))
        (dolist (key (lispm-kit-i18n--stale table messages))
          (push (list language 'stale key) problems))
        (when (memq language lispm-kit-i18n-translated-languages)
          (dolist (key (lispm-kit-i18n--missing language table messages))
            (push (list language 'missing key) problems)))
        (dolist (wrong (lispm-kit-i18n--wrong-arguments table messages))
          (push (cons language (cons 'arguments wrong)) problems))
        (dolist (problem (lispm-kit-i18n--misspelled language table messages))
          (push (cons language problem) problems))))
    (seq-filter (lambda (problem) (or (null kinds) (memq (nth 1 problem) kinds)))
                (nreverse problems))))

(defun lispm-kit-i18n-report (domain messages)
  "Print, for each language, the messages DOMAIN's catalog lacks and those
it holds no longer, against MESSAGES its code gives (see
`lispm-kit-i18n-messages'), as a program's `make i18n' shows them."
  (dolist (language (mapcar #'car lispm-kit-languages))
    (let ((table (plist-get (lispm-kit-i18n-read-catalog domain language) :messages))
          (missing 0) (stale 0))
      (when (memq language lispm-kit-i18n-translated-languages)
        (dolist (key (lispm-kit-i18n--missing language table messages))
          (setq missing (1+ missing))
          (princ (format "%s: missing %S (%s)\n" language key
                         (string-join (plist-get (gethash key messages) :places) " ")))))
      (dolist (key (lispm-kit-i18n--stale table messages))
        (setq stale (1+ stale))
        (princ (format "%s: stale %S\n" language key)))
      (princ (format "%s: %d missing, %d stale\n" language missing stale)))))

(provide 'lispm-kit-i18n-check)
;;; lispm-kit-i18n-check.el ends here
