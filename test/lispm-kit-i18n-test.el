;;; lispm-kit-i18n-test.el --- Tests of lispm-kit-i18n  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
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

;; The tests define words of their own, a domain whose catalogs they
;; write in a scratch directory, and speak them as a program would.

;;; Code:

(require 'ert)
(require 'lispm-kit-i18n)
(require 'lispm-kit-i18n-check)

(defvar lispm-kit-i18n-test--language nil
  "The language chosen for the tests' words.")

(defvar lispm-kit-i18n-test--language-functions nil
  "Functions saying the language of the program around the tests' words.")

(defvar lispm-kit-i18n-test--changed-hook nil
  "Hook run once the tests' words are spoken in another language.")

(defvar lispm-kit-i18n-test--other-language nil
  "The language chosen for the tests' other words.")

(defconst lispm-kit-i18n-test--catalogs
  '((fr :name "Français"
        :messages (("Step %d of %d" . "Étape %1$d sur %2$d")
                   ("%d item" . ("%d élément" "%d éléments"))
                   (("state" . "Open") . "Ouvert")
                   ("Open" . "Ouvrir")
                   ("%-m/%-d/%y" . "%d.%m.%y")))
    (de :name "Deutsch" :messages (("Open" . "Öffnen"))))
  "The catalogs of the tests' words: (TAG . CATALOG).")

(defun lispm-kit-i18n-test--write (directory catalogs)
  "Write CATALOGS, (TAG . CATALOG), as the files of DIRECTORY."
  (pcase-dolist (`(,tag . ,catalog) catalogs)
    (with-temp-file (expand-file-name (format "%s.eld" tag) directory)
      (prin1 catalog (current-buffer)))))

(defmacro lispm-kit-i18n-test--with-words (&rest body)
  "Run BODY with the tests' words defined as the domain
`lispm-kit-i18n-test', their catalogs written in a scratch directory,
bound to `directory', no language chosen, the computer's language
Japanese, and nothing read yet."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "lispm-kit-i18n-test-" t))
          (lispm-kit-i18n--catalogs nil)
          (lispm-kit-i18n--spoken nil)
          (lispm-kit-i18n--system-languages '("ja-JP"))
          (lispm-kit-i18n--system-language '(unread))
          (lispm-kit-i18n-test--language nil)
          (lispm-kit-i18n-test--language-functions nil)
          (lispm-kit-i18n-test--changed-hook nil))
     (unwind-protect
         (progn
           (lispm-kit-i18n-test--write directory lispm-kit-i18n-test--catalogs)
           (lispm-kit-i18n-define 'lispm-kit-i18n-test
             :directory directory
             :language 'lispm-kit-i18n-test--language
             :language-functions 'lispm-kit-i18n-test--language-functions
             :changed-hook 'lispm-kit-i18n-test--changed-hook)
           ,@body)
       (remove-variable-watcher 'lispm-kit-i18n-test--language #'lispm-kit-i18n--changed)
       (put 'lispm-kit-i18n-test--language 'lispm-kit-i18n-domains nil)
       (delete-directory directory t))))

(defun lispm-kit-i18n-test--wait (predicate seconds)
  "Wait at most SECONDS for PREDICATE to return non-nil, running timers;
return its value."
  (let ((end (+ (float-time) seconds)) (value nil))
    (while (and (not (setq value (funcall predicate))) (< (float-time) end))
      (accept-process-output nil 0.01))
    value))

(ert-deftest lispm-kit-i18n-test-language-of ()
  "A language tag or locale asks for one of LispM's languages: Chinese by
script, else by region, English American in the United States and
without a region, else British."
  (should (equal (mapcar #'lispm-kit-i18n-language-of
                         '("zh-Hant-HK" "zh-Hans-HK" "zh-TW" "zh_HK.UTF-8" "zh-CN" "zh" "en" "en-US"
                           "en_US.UTF-8" "en-HK" "en-GB" "fr-CA" "de_CH" "ja-JP" "ko-KR" "C"))
                 '(zh-Hant zh-Hans zh-Hant zh-Hant zh-Hans zh-Hans en-US en-US
                   en-US en-GB en-GB fr de ja nil nil)))
  (let ((lispm-kit-i18n--system-language '(unread)))
    (let ((lispm-kit-i18n--system-languages '("yue-Hant-HK" "ja-HK" "en-US")))
      (should (eq (lispm-kit-system-language) 'ja)))
    (let ((lispm-kit-i18n--system-languages '("ko-KR")))
      (should (eq (lispm-kit-system-language) 'en-US)))))

(ert-deftest lispm-kit-i18n-test-current-language ()
  "Words are spoken in the language chosen for them, else that of the
program around, else the computer's; a program's language LispM does not
speak is not followed."
  (lispm-kit-i18n-test--with-words
    (should (eq (lispm-kit-current-language 'lispm-kit-i18n-test) 'ja))
    (let ((lispm-kit-i18n-test--language-functions (list #'ignore (lambda () 'de))))
      (should (eq (lispm-kit-current-language 'lispm-kit-i18n-test) 'de))
      (let ((lispm-kit-i18n-test--language 'fr))
        (should (eq (lispm-kit-current-language 'lispm-kit-i18n-test) 'fr))))
    (let ((lispm-kit-i18n-test--language-functions (list (lambda () 'en))))
      (should (eq (lispm-kit-current-language 'lispm-kit-i18n-test) 'ja)))))

(ert-deftest lispm-kit-i18n-test-translates ()
  "A message shows in the language spoken, formatted, as written where the
catalog lacks it; a plural takes the language's form for the count; a
message said in a context is looked up in it; in English, as written;
the caller's match data is kept."
  (lispm-kit-i18n-test--with-words
    (let ((lispm-kit-i18n-test--language 'fr))
      (should (equal (lispm-kit-tr 'lispm-kit-i18n-test "Step %d of %d" 2 3) "Étape 2 sur 3"))
      (should (equal (lispm-kit-tr 'lispm-kit-i18n-test "Not in the catalog") "Not in the catalog"))
      (should (equal (lispm-kit-tr 'lispm-kit-i18n-test "%d%% done" 5) "5% done"))
      (should (equal (lispm-kit-tr 'lispm-kit-i18n-test "Open") "Ouvrir"))
      (should (equal (lispm-kit-tr-in 'lispm-kit-i18n-test "state" "Open") "Ouvert"))
      (should (equal (lispm-kit-tr 'lispm-kit-i18n-test (lispm-kit-tr-noop "Open" "state")) "Ouvert"))
      (should (equal (mapcar (lambda (n) (lispm-kit-tr-n 'lispm-kit-i18n-test "%d item" "%d items" n))
                             '(0 1 2))
                     '("0 élément" "1 élément" "2 éléments")))
      (should (equal (lispm-kit-tr-n 'lispm-kit-i18n-test "%d page" "%d pages" 0) "0 pages"))
      (should (equal (lispm-kit-format-time 'lispm-kit-i18n-test "%-m/%-d/%y"
                                            (encode-time '(0 0 12 27 10 2026 nil -1 nil)))
                     "27.10.26"))
      (should (equal (lispm-kit-language-name 'lispm-kit-i18n-test 'fr) "Français"))
      (should (equal (lispm-kit-language-name 'lispm-kit-i18n-test 'ja) "Japanese"))
      (should (equal (lispm-kit-in-english (lispm-kit-tr 'lispm-kit-i18n-test "Open")) "Open"))
      (should (equal (string-match "b" "abc") 1))
      (lispm-kit-tr 'lispm-kit-i18n-test "Open")
      (should (equal (match-beginning 0) 1)))))

(ert-deftest lispm-kit-i18n-test-domains-apart ()
  "Each program's words are read from its own catalogs, in the language
it speaks."
  (lispm-kit-i18n-test--with-words
    (let ((other (make-temp-file "lispm-kit-i18n-test-other-" t))
          (lispm-kit-i18n-test--other-language 'de))
      (unwind-protect
          (progn
            (lispm-kit-i18n-test--write other '((de :name "Deutsch" :messages (("Open" . "Auf")))
                                                (fr :name "Français" :messages (("Open" . "Ouvre")))))
            (lispm-kit-i18n-define 'lispm-kit-i18n-test-other
              :directory other :language 'lispm-kit-i18n-test--other-language :watch nil)
            (let ((lispm-kit-i18n-test--language 'fr))
              (should (equal (lispm-kit-tr 'lispm-kit-i18n-test "Open") "Ouvrir"))
              (should (equal (lispm-kit-tr 'lispm-kit-i18n-test-other "Open") "Auf"))
              (let ((lispm-kit-i18n-test--other-language 'fr))
                (should (equal (lispm-kit-tr 'lispm-kit-i18n-test-other "Open") "Ouvre")))))
        (put 'lispm-kit-i18n-test-other 'lispm-kit-i18n nil)
        (delete-directory other t)))))

(ert-deftest lispm-kit-i18n-test-language-changed ()
  "Setting the language chosen runs the changed hook once the command
ends, when the language spoken changes; so does a program saying its
language changed, unless one is chosen; setting it with
`lispm-kit-i18n-set' runs it at once, once."
  (lispm-kit-i18n-test--with-words
    (let ((drawn 0))
      (add-hook 'lispm-kit-i18n-test--changed-hook (lambda () (setq drawn (1+ drawn))))
      (setq lispm-kit-i18n-test--language 'de)
      (should (lispm-kit-i18n-test--wait (lambda () (= drawn 1)) 1))
      (setq lispm-kit-i18n-test--language 'de)
      (accept-process-output nil 0.05)
      (should (= drawn 1))
      (lispm-kit-language-changed 'lispm-kit-i18n-test)
      (should (= drawn 1))
      (setq lispm-kit-i18n-test--language nil)
      (should (lispm-kit-i18n-test--wait (lambda () (= drawn 2)) 1))
      (lispm-kit-language-changed 'lispm-kit-i18n-test)
      (should (= drawn 3))
      (lispm-kit-i18n-set 'lispm-kit-i18n-test 'fr)
      (should (= drawn 4))
      (accept-process-output nil 0.05)
      (should (= drawn 4))
      (lispm-kit-i18n-set 'lispm-kit-i18n-test 'fr)
      (should (= drawn 4)))))

(ert-deftest lispm-kit-i18n-test-watch-later ()
  "Words defined not to watch their language variable follow it only once
`lispm-kit-i18n-watch' says so."
  (lispm-kit-i18n-test--with-words
    (let ((drawn 0))
      (remove-variable-watcher 'lispm-kit-i18n-test--language #'lispm-kit-i18n--changed)
      (put 'lispm-kit-i18n-test--language 'lispm-kit-i18n-domains nil)
      (lispm-kit-i18n-define 'lispm-kit-i18n-test
        :directory directory :language 'lispm-kit-i18n-test--language
        :changed-hook 'lispm-kit-i18n-test--changed-hook :watch nil)
      (add-hook 'lispm-kit-i18n-test--changed-hook (lambda () (setq drawn (1+ drawn))))
      (setq lispm-kit-i18n-test--language 'de)
      (accept-process-output nil 0.05)
      (should (= drawn 0))
      (lispm-kit-i18n-watch 'lispm-kit-i18n-test)
      (setq lispm-kit-i18n-test--language 'fr)
      (should (lispm-kit-i18n-test--wait (lambda () (= drawn 1)) 1)))))

(ert-deftest lispm-kit-i18n-test-check ()
  "The messages a program's code gives are found where they are written,
and its catalogs are checked against them: stale and missing messages,
a catalog without a name, other arguments, and the other English's
spelling."
  (lispm-kit-i18n-test--with-words
    (let ((source (expand-file-name "words.el" directory)))
      (with-temp-file source
        (insert "(defun f (n)\n"
                "  ;; (my-tr \"In a comment\")\n"
                "  (list (my-tr \"Open\") (my-tr-in \"state\" \"Open\")\n"
                "        (my-tr \"Step %d of %d\" 1 2) (my-tr-n \"%d item\" \"%d items\" n)\n"
                "        (my-tr-noop \"Grey\") (my-time \"%-m/%-d/%y\") \"Not a message\"))\n"))
      (lispm-kit-i18n-test--write
       directory
       '((fr :name "Français"
             :messages (("Open" . "Ouvrir") (("state" . "Open") . "Ouvert")
                        ("Step %d of %d" . "Étape %d")
                        ("%d item" . ("%d élément"))
                        ("%-m/%-d/%y" . "%d.%m.%y")
                        ("Gone" . "Parti")))
         (en-US :name "English (US)" :messages (("Grey" . "Grey")))
         (en-GB :messages ())))
      (let ((messages (lispm-kit-i18n-messages (list source)
                                              :tr 'my-tr :tr-n 'my-tr-n :tr-in 'my-tr-in
                                              :tr-noop 'my-tr-noop :format-time 'my-time)))
        (should (equal (sort (hash-table-keys messages) #'string<)
                       (sort (list "Open" (lispm-kit-i18n-key "Open" "state") "Step %d of %d"
                                   "%d item" "Grey" "%-m/%-d/%y")
                             #'string<)))
        (should (equal (plist-get (gethash "Open" messages) :places) '("words.el:3")))
        (should (equal (plist-get (gethash "%d item" messages) :plural) "%d items"))
        (should (plist-get (gethash "%-m/%-d/%y" messages) :time))
        (let ((problems (lispm-kit-i18n-problems 'lispm-kit-i18n-test messages)))
          (should (member '(fr stale "Gone") problems))
          (should (member '(fr missing "%d item") problems))
          (should (member '(fr missing "Grey") problems))
          (should (member '(fr arguments "Step %d of %d" "Étape %d") problems))
          (should-not (seq-find (lambda (problem) (equal (cdr problem) '(arguments "%-m/%-d/%y" "%d.%m.%y")))
                                problems))
          (should (member '(en-GB nameless) problems))
          (should (member '(en-US spelling "Grey" "Grey") problems))
          (should (member '(de missing "Grey") problems))
          (should-not (member '(de missing "Open") problems))
          (should (equal (lispm-kit-i18n-problems 'lispm-kit-i18n-test messages '(nameless))
                         '((en-GB nameless) (ja nameless) (zh-Hans nameless) (zh-Hant nameless)))))))))

(provide 'lispm-kit-i18n-test)
;;; lispm-kit-i18n-test.el ends here
