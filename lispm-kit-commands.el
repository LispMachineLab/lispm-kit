;;; lispm-kit-commands.el --- An application's commands, each declared once  -*- lexical-binding: t; -*-

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

;; An application's mode says what each of its commands is, once, with
;; `lispm-kit-define-commands': its keys, its menu item, what it does,
;; when it can be used, and whether it is one of the main commands an
;; interface may show as buttons.  The mode's keymap gets the keys and
;; the menu bar the menus, as `easy-menu-define' makes them, so plain
;; Emacs shows them as it shows any mode's, C-h m included; and what
;; was declared stays on the mode's symbol as plain data, its
;; `lispm-kit-commands' property, for an interface of its own to read:
;;
;;   (lispm-kit-define-commands lispm-rotom-mode
;;     :menu "Rotom"
;;     (lispm-rotom-start-or-stop
;;      :key "s" :help "Start or stop the daemon"
;;      :label (if (lispm-rotom-running-p) "Stop Daemon" "Start Daemon")
;;      :button (if (lispm-rotom-running-p) "Stop" "Start")
;;      :enabled (and (lispm-rotom-installed-p) (not lispm-rotom-busy)))
;;     ...)
;;
;; The property is a list of property lists, one a command, in the
;; order declared, those in submenus too:
;;
;;   :command   the command
;;   :key       the keys it is bound to, strings as `keymap-set' takes them
;;   :label     its menu item's words, a string or a function returning one
;;   :help      what it does, in a few words
;;   :enabled   a function returning non-nil while it can be used; without
;;              it, it always can
;;   :button    for one of the main commands: the button's words, a
;;              string or a function returning one
;;   :style     `toggle' or `radio', for an item checked or not
;;   :selected  a function returning non-nil while its item is checked
;;
;; The functions take no arguments and are called in the mode's buffer.
;; The words are in English; an interface may translate them where it
;; shows them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'easymenu)

(defconst lispm-kit--properties '(:key :label :help :enabled :button :style :selected)
  "The properties a command is declared with, see `lispm-kit-define-commands'.")

(defun lispm-kit--function (form)
  "Return code making FORM a function of no arguments, unless it is one."
  (if (memq (car-safe form) '(lambda function))
      form
    `(lambda () ,form)))

(defun lispm-kit--command (command properties)
  "Return code making the property list of COMMAND declared with PROPERTIES."
  (let ((code nil))
    (while properties
      (let ((property (pop properties))
            (value (pop properties)))
        (unless (memq property lispm-kit--properties)
          (error "%s: %S is not a property of a command" command property))
        (push property code)
        (push (pcase property
                (:key `',(ensure-list value))
                (:style `',value)
                ((or :label :button) (if (or (stringp value) (eq value t)) value (lispm-kit--function value)))
                ((or :enabled :selected) (lispm-kit--function value))
                (_ value))
              code)))
    `(list :command ',command ,@(nreverse code))))

(defun lispm-kit--item (item)
  "Return code making ITEM of the body of `lispm-kit-define-commands'."
  (pcase item
    (`(,(and command (pred symbolp)) . ,properties) (lispm-kit--command command properties))
    (`(,(and title (pred stringp)) . ,items)
     (let ((keywords nil))
       (while (keywordp (car items))
         (push (pop items) keywords)
         (push (pop items) keywords))
       `(list :submenu ,title :keywords ',(nreverse keywords)
              :items (list ,@(mapcar #'lispm-kit--item items)))))
    (_ `',item)))

(defmacro lispm-kit-define-commands (mode &rest body)
  "Declare the commands of MODE, a major mode, each once.
BODY holds them, each (COMMAND PROPERTY VALUE ...), in the order the
menus and an interface's buttons show them.  `:menu TITLE' in BODY
starts a menu of the menu bar, holding what follows up to the next
one; commands before the first are in no menu.  The menus are made one
after another, as `easy-menu-define' makes them, so the menu bar shows
the later ones first.  A menu holds besides:

  \"---\"             a separator
  (TITLE ITEM ...)  a submenu; easy-menu's keywords, as :filter, may
                    follow its TITLE
  COMMAND           the item of COMMAND, declared elsewhere in BODY: here
                    rather than where it is declared, which keeps its
                    place among the buttons
  [...]             an item as `easy-menu-define' takes it

A command's properties:

  :key KEY          a key, or a list of them, as `keymap-set' takes them
  :label LABEL      the words of its menu item; without them it has none
  :help STRING      what it does, in a few words
  :enabled FORM     non-nil while it can be used
  :button LABEL     makes it one of the main commands, LABEL the words of
                    its button; t takes those of :label
  :style STYLE      `toggle' or `radio'
  :selected FORM    non-nil while its item is checked

A LABEL is a string or a form returning one; any form may be a
function instead, as (lambda () ...) or #\\='NAME.  They are evaluated
in MODE's buffer.  The keys are bound in `MODE-map', defined here if it
is not yet; what is declared is MODE's `lispm-kit-commands' property,
see the commentary of lispm-kit-commands.el."
  (declare (indent 1) (debug (symbolp &rest sexp)))
  (let ((groups (list (list nil))))
    (while body
      (let ((item (pop body)))
        (if (eq item :menu)
            (push (list (pop body)) groups)
          (push item (cdar groups)))))
    (let ((map (intern (format "%s-map" mode))))
      `(progn
         (defvar ,map (make-sparse-keymap))
         (lispm-kit--install
          ',mode ,map
          (list ,@(mapcar (pcase-lambda (`(,title . ,items))
                            `(list ,title ,@(mapcar #'lispm-kit--item (reverse items))))
                          (reverse groups))))))))

(defun lispm-kit--commands (items)
  "Return the commands' property lists among ITEMS, those of submenus too."
  (mapcan (lambda (item)
            (pcase (car-safe item)
              (:command (list item))
              (:submenu (lispm-kit--commands (plist-get item :items)))))
          items))

(defun lispm-kit--named (items)
  "Return the commands named alone among ITEMS, those of submenus too."
  (mapcan (lambda (item)
            (cond ((and item (symbolp item)) (list item))
                  ((eq (car-safe item) :submenu) (lispm-kit--named (plist-get item :items)))))
          items))

(defun lispm-kit--menu-item (command)
  "Return the easy-menu item of COMMAND's property list, or nil.
A command without :label has none."
  (when-let* ((label (plist-get command :label)))
    (cl-flet ((call (property) (when-let* ((function (plist-get command property)))
                                 `(funcall ',function))))
      ;; Easy-menu merges items of one name: one changing its words is
      ;; named after its command.
      (vconcat (list (if (stringp label) label (symbol-name (plist-get command :command)))
                     (plist-get command :command))
               (unless (stringp label) `(:label ,(call :label)))
               (when (plist-get command :enabled) `(:active ,(call :enabled)))
               (when-let* ((help (plist-get command :help))) `(:help ,help))
               (when-let* ((style (plist-get command :style))) `(:style ,style))
               (when (plist-get command :selected) `(:selected ,(call :selected)))))))

(defun lispm-kit--menu (items commands named)
  "Return ITEMS of a menu as `easy-menu-define' takes them.
A command named alone is found in COMMANDS; a command NAMED alone
somewhere has its item there, not where it is declared."
  (delq nil
        (mapcar (lambda (item)
                  (pcase item
                    ((and (pred symbolp) name)
                     (lispm-kit--menu-item
                      (or (seq-find (lambda (command) (eq (plist-get command :command) name)) commands)
                          (error "%s is in a menu, but not declared" name))))
                    (`(:command ,name . ,_) (unless (memq name named) (lispm-kit--menu-item item)))
                    (`(:submenu ,title :keywords ,keywords :items ,items)
                     `(,title ,@keywords ,@(lispm-kit--menu items commands named)))
                    (_ item)))
                items)))

(defun lispm-kit--install (mode map groups)
  "Make what `lispm-kit-define-commands' declares of MODE; return MODE.
Bind the keys of the commands in GROUPS, ((TITLE . ITEMS) ...), in MAP,
make their menus, and put the commands on MODE."
  (let* ((items (mapcan (lambda (group) (copy-sequence (cdr group))) groups))
         (commands (lispm-kit--commands items))
         (named (lispm-kit--named items))
         (seen nil))
    (dolist (command commands)
      (let ((name (plist-get command :command)))
        (when (memq name seen)
          (error "%s is declared twice" name))
        (push name seen)
        (when (eq (plist-get command :button) t)
          (plist-put command :button (plist-get command :label)))
        (dolist (key (plist-get command :key))
          (keymap-set map key name))))
    (put mode 'lispm-kit-commands commands)
    (pcase-dolist (`(,title . ,items) groups)
      (when title
        (easy-menu-do-define nil map nil (cons title (lispm-kit--menu items commands named)))))
    mode))

(provide 'lispm-kit-commands)
;;; lispm-kit-commands.el ends here
