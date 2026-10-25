;;; lispm-kit-test.el --- Tests of lispm-kit  -*- lexical-binding: t; -*-

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

;; The tests declare the commands of a mode of their own, then read its
;; keymap, its menus and its symbol as Emacs and an interface read them.

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'lispm-kit)

(defconst lispm-kit-test--directory
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The directory of lispm-kit's sources.")

;;;; A mode of the tests

(defvar lispm-kit-test--running nil "Non-nil while the tests' daemon runs.")
(defvar lispm-kit-test--busy nil "Non-nil while the tests' mode is busy.")
(defvar lispm-kit-test--checked nil "Non-nil while the tests' item is checked.")

(defun lispm-kit-test--running-p ()
  "Return non-nil while the tests' daemon runs."
  lispm-kit-test--running)

(dolist (name '(act back start-or-stop restart find check view))
  (defalias (intern (format "lispm-kit-test-%s" name)) (lambda () (interactive))
    "A command of the tests' mode."))

;; A keymap of its own already, as an application may have one.
(defvar-keymap lispm-kit-test-mode-map
  :doc "Keys of the tests' mode."
  "z" #'ignore)

(lispm-kit-define-commands lispm-kit-test-mode
  (lispm-kit-test-act :key "RET" :help "Act on the line")
  (lispm-kit-test-back :key ("q" "l") :label "Back to the List" :button "Back")
  :menu "Test"
  (lispm-kit-test-start-or-stop
   :key "s" :help "Start or stop the daemon"
   :label (if lispm-kit-test--running "Stop" "Start")
   :enabled (not lispm-kit-test--busy)
   :button t)
  (lispm-kit-test-restart :key "r" :label "Restart" :button "Again" :enabled #'lispm-kit-test--running-p)
  ;; Among the buttons after Restart, in the menu in View.
  (lispm-kit-test-find :key "f" :label "Find…" :button t)
  "---"
  ("More"
   (lispm-kit-test-check :key "c" :label "Checked" :style toggle :selected lispm-kit-test--checked))
  ["Raw" ignore]
  lispm-kit-test-back
  :menu "View"
  (lispm-kit-test-view :key "v" :label "View All")
  lispm-kit-test-find)

(define-derived-mode lispm-kit-test-mode special-mode "Kit Test"
  "The tests' mode.
\\{lispm-kit-test-mode-map}")

(defun lispm-kit-test--items (map)
  "Return the items of menu keymap MAP as its menu shows them in the
current buffer: (LABEL COMMAND ENABLED CHECKED), `separator', or (TITLE
ITEM ...) for a submenu."
  (let ((items nil))
    (map-keymap
     (lambda (_key binding)
       (pcase binding
         (`(menu-item ,name ,real . ,properties)
          (let ((name (if (stringp name) name (eval name t))))
            (push (cond ((string-prefix-p "--" name) 'separator)
                        ((keymapp real) (cons name (lispm-kit-test--items real)))
                        (t (list name real
                                 (or (not (plist-member properties :enable))
                                     (and (eval (plist-get properties :enable) t) t))
                                 (and (eval (cdr (plist-get properties :button)) t) t))))
                  items)))
         (`(,(and (pred stringp) name) . ,_)
          (when (string-prefix-p "--" name)
            (push 'separator items)))))
     map)
    (nreverse items)))

(defun lispm-kit-test--menus ()
  "Return the menus the tests' mode has in the menu bar, as
`lispm-kit-test--items' reads them."
  (lispm-kit-test--items (keymap-lookup lispm-kit-test-mode-map "<menu-bar>")))

;;;; The tests

(ert-deftest lispm-kit-test-binds-keys ()
  "The commands' keys are bound in the mode's keymap, beside those it had."
  (with-temp-buffer
    (lispm-kit-test-mode)
    (should (eq (key-binding (kbd "s")) #'lispm-kit-test-start-or-stop))
    (should (eq (key-binding (kbd "RET")) #'lispm-kit-test-act))
    (should (eq (key-binding (kbd "q")) #'lispm-kit-test-back))
    (should (eq (key-binding (kbd "l")) #'lispm-kit-test-back))
    (should (eq (key-binding (kbd "c")) #'lispm-kit-test-check))
    (should (eq (key-binding (kbd "z")) #'ignore))
    ;; Its parent's keys stay.
    (should (eq (key-binding (kbd "g")) #'revert-buffer))))

(ert-deftest lispm-kit-test-puts-the-commands-on-the-mode ()
  "The mode's symbol holds the commands as declared, in their order,
those of submenus too, their forms made functions."
  (let ((commands (get 'lispm-kit-test-mode 'lispm-kit-commands)))
    (should (equal (mapcar (lambda (command) (plist-get command :command)) commands)
                   '(lispm-kit-test-act lispm-kit-test-back lispm-kit-test-start-or-stop
                     lispm-kit-test-restart lispm-kit-test-find lispm-kit-test-check lispm-kit-test-view)))
    (should (equal (seq-keep (lambda (command) (plist-get command :button)) (cdr commands))
                   (list "Back" (plist-get (nth 2 commands) :label) "Again" "Find…")))
    (pcase-let ((`(,act ,back ,start ,restart ,_find ,check ,view) commands))
      (should (equal act '(:command lispm-kit-test-act :key ("RET") :help "Act on the line")))
      (should (equal (plist-get back :key) '("q" "l")))
      (should (equal (plist-get back :button) "Back"))
      (should (equal (plist-get start :help) "Start or stop the daemon"))
      (should (functionp (plist-get start :label)))
      ;; :button t takes the words of the menu item.
      (should (eq (plist-get start :button) (plist-get start :label)))
      (let ((lispm-kit-test--running nil)) (should (equal (funcall (plist-get start :button)) "Start")))
      (let ((lispm-kit-test--running t)) (should (equal (funcall (plist-get start :button)) "Stop")))
      (let ((lispm-kit-test--busy nil)) (should (funcall (plist-get start :enabled))))
      (let ((lispm-kit-test--busy t)) (should-not (funcall (plist-get start :enabled))))
      (should (eq (plist-get restart :enabled) #'lispm-kit-test--running-p))
      (should (eq (plist-get check :style) 'toggle))
      (let ((lispm-kit-test--checked t)) (should (funcall (plist-get check :selected))))
      (should (equal view '(:command lispm-kit-test-view :key ("v") :label "View All"))))))

(ert-deftest lispm-kit-test-makes-the-menus ()
  "The menus hold the commands with words, as declared, in the order of
`easy-menu-define', the later menu first, a command named alone there
rather than where it is declared; their words, enabling and checks
follow the mode's state."
  (with-temp-buffer
    (lispm-kit-test-mode)
    (let ((lispm-kit-test--running nil) (lispm-kit-test--busy nil) (lispm-kit-test--checked nil))
      (should (equal (lispm-kit-test--menus)
                     '(("View" ("View All" lispm-kit-test-view t nil) ("Find…" lispm-kit-test-find t nil))
                       ("Test"
                        ("Start" lispm-kit-test-start-or-stop t nil)
                        ("Restart" lispm-kit-test-restart nil nil)
                        separator
                        ("More" ("Checked" lispm-kit-test-check t nil))
                        ("Raw" ignore t nil)
                        ("Back to the List" lispm-kit-test-back t nil))))))
    (let ((lispm-kit-test--running t) (lispm-kit-test--busy t) (lispm-kit-test--checked t))
      (should (equal (seq-take (cdr (assoc "Test" (lispm-kit-test--menus))) 4)
                     '(("Stop" lispm-kit-test-start-or-stop nil nil)
                       ("Restart" lispm-kit-test-restart t nil)
                       separator
                       ("More" ("Checked" lispm-kit-test-check t t))))))))

(ert-deftest lispm-kit-test-describe-mode ()
  "C-h m lists the commands' keys."
  (with-temp-buffer
    (lispm-kit-test-mode)
    (let ((help-window-select nil))
      (save-window-excursion
        (describe-mode)
        (with-current-buffer (help-buffer)
          (should (string-match-p "lispm-kit-test-start-or-stop" (buffer-string)))
          (should (string-match-p "lispm-kit-test-back" (buffer-string))))))))

(ert-deftest lispm-kit-test-refuses-mistakes ()
  "A property no command has, a command declared twice and one in a menu
but not declared are errors."
  (should-error (macroexpand-all '(lispm-kit-define-commands lispm-kit-test-other-mode
                                    (ignore :keys "a"))))
  (should-error (eval '(lispm-kit-define-commands lispm-kit-test-other-mode
                         (ignore :key "a") (ignore :key "b"))
                      t))
  (should-error (eval '(lispm-kit-define-commands lispm-kit-test-other-mode
                         :menu "Other" ignore)
                      t)))

(ert-deftest lispm-kit-test-compiles-clean ()
  "A mode declaring its commands byte-compiles without a warning."
  (let* ((directory (make-temp-file "lispm-kit-test" t))
         (file (expand-file-name "lispm-kit-test-compiled.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; lispm-kit-test-compiled.el --- A test  -*- lexical-binding: t; -*-\n")
            (dolist (form '((require 'lispm-kit-commands)
                            (defvar lispm-kit-test-compiled--on nil "Non-nil when on.")
                            (defun lispm-kit-test-compiled-toggle ()
                              "Toggle."
                              (interactive)
                              (setq lispm-kit-test-compiled--on (not lispm-kit-test-compiled--on)))
                            (lispm-kit-define-commands lispm-kit-test-compiled-mode
                              :menu "Compiled"
                              (lispm-kit-test-compiled-toggle
                               :key "t" :label (if lispm-kit-test-compiled--on "Off" "On")
                               :enabled (not buffer-read-only) :button t))
                            (define-derived-mode lispm-kit-test-compiled-mode special-mode "Compiled"
                              "A mode.")
                            (provide 'lispm-kit-test-compiled)))
              (prin1 form (current-buffer))
              (insert "\n")))
          (let ((byte-compile-error-on-warn t)
                (load-path (cons lispm-kit-test--directory load-path)))
            (should (byte-compile-file file))))
      (delete-directory directory t))))

(ert-deftest lispm-kit-test-knows-no-interface ()
  "Its sources name no interface of their own: an interface reads what an
application declares."
  (dolist (file (directory-files lispm-kit-test--directory t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((case-fold-search t))
        (should-not (and (re-search-forward "lispm-desktop\\|platinum" nil t)
                         (list (file-name-nondirectory file) (line-number-at-pos))))))))

(provide 'lispm-kit-test)
;;; lispm-kit-test.el ends here
