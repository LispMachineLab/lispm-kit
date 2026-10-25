;;; lispm-kit.el --- What LispM's applications are made with  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-kit
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp, extensions
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

;; LispM Kit is what the applications of LispM's App Store are made
;; with, in any Emacs 30.1 or later:
;;
;;   lispm-kit-commands.el  an application's commands, each declared
;;                          once: its keys, its menus, its main commands
;;
;; It is an application's to require, and knows of no interface:
;; Emacs's own shows what an application declares as it shows any
;; mode's, and another interface reads it from the mode's symbol.

;;; Code:

(require 'lispm-kit-commands)

(provide 'lispm-kit)
;;; lispm-kit.el ends here
