;;; eshell-better-prompt.el --- refined better eshell prompt -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/eshell-better-prompt
;; Version: 0.0.1
;; Package-Requires: ((package "version-number"))
;; Keywords: eshell

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;;; Code:
;; TODO: apply some environment variables like REMOTEHOST, SSH_CONNECTION
;; TODO: implement command colorize whether the command is exist or not
;;       (memo use eshell-self-insert-command)
;; TODO: Apply pwd

(require 'em-prompt)
(require 'vc-git)
(require 'magit)
(require 'rx)
(require 'cl-lib)

;; Cool prompt character candidates:
;; §  ᘓ  ᚖ  ៙  ៚  ⃝  ⃠  ⍒  ⍊  ⍉  ⍭  ⏣
;; ☠  ☣  ☀  ☁  ☂  ☃  ⚡  ☮  ★  ☆  ⚡  ⚝
;; ✗ ☑ ☒ ☓ ☄ ☯ ∉ ø ∰ ℣ ℤ ☤ ☥ ☦ ♠ ♣ ♥ ♦ ¿ � λ
;; See also: http://csbruce.com/software/utf-8.html
(defvar eshell-better-prompt-head "λ ")
(defvar eshell-better-prompt-section "§")
(defvar eshell-better-prompt-time-format "%x %X")
(defvar eshell-better-prompt-functions
  '(eshell-better-prompt-status-section ; $?
    eshell-better-prompt-user-section   ; user@hostname
    eshell-better-prompt-vcs-section    ; git info
    eshell-better-prompt-time-section   ; time
    eshell-better-prompt-pwd-section    ; current directory
    ))

(defvar eshell-better-prompt-ordinary-prompt-regex
  (rx (and (1+ (not (in ">#$\t\n")))
           (0+ space) (in ">#$") space)))
;; TODO: colorize specific internal shell of eshell.
;; Example:
;;    sudo -i   : root@machine:~#
;;    adb shell : shell@mako:/ $

;; BUFFER LOCAL VARIABLE
(defvar-local eshell-better-prompt-limit 42)

;; Faces
(defface eshell-better-prompt-user-face
  '((t :inherit font-lock-function-name-face))
  "Face of user information section."
  :group 'eshell-better-prompt)

(defface eshell-better-prompt-vcs-face
  '((t :inherit font-lock-keyword-face))
  "Face of VCS information section."
  :group 'eshell-better-prompt)

(defface eshell-better-prompt-time-face
  '((t :inherit font-lock-negation-char-face))
  "Face of time information section."
  :group 'eshell-better-prompt)

(defface eshell-better-prompt-status-error-face
  '((t :inherit font-lock-warning-face))
  "Face of status information section."
  :group 'eshell-better-prompt)

(defface eshell-better-prompt-direct-input-face
  '((t :inherit font-lock-builtin-face))
  "Face when direct input."
  :group 'eshell-better-prompt)

(defface eshell-better-prompt-delay-input-face
  '((t :inherit eshell-prompt-face))
  "Face when delay input."
  :group 'eshell-better-prompt)

;; FUNCTIONS
(defun eshell-better-prompt-initialize ()
  "Initialize prompt configuration."
  (let ((prompt-regex
         (apply `((lambda ()
                    (rx (and line-start
                             (or ,eshell-better-prompt-head
                                 ,eshell-better-prompt-ordinary-prompt-regex))))))))
    ;; Even user didn't turn on `eshell-highlight-prompt', this text
    ;; property should be read-only and sticky property to prevent
    ;; deleting prompt.
    (setq eshell-better-prompt-head
          (propertize eshell-better-prompt-head
                      'read-only t 'rear-nonsticky t 'front-sticky t))
    (setq eshell-prompt-regexp prompt-regex)))

;; Fallback function if eshell-bol is failed.
;; This is for irregular prompt like "prelude> ".
(advice-add 'eshell-bol :after
            (lambda ()
              (when (eq (point-at-bol) (point))
                (while (and (eq 'eshell-prompt (face-at-point))
                            (not (eq (point-at-eol) (point))))
                  (forward-char 1)))))

(defun eshell-better-prompt-set-window-limit ()
  "Set window width."
  ;; Calculate length of `eshell-better-prompt-section'.
  (let ((ratio (char-width (string-to-char eshell-better-prompt-section)))
        (sum-of-section (1+ (length eshell-better-prompt-functions))))
    (setq-local eshell-better-prompt-limit
                (max (- (window-width)
                        (+ (* ratio sum-of-section) 0))
                     30))))

(defun eshell-better-prompt-user-section ()
  "Return string for prompt.
The format is like [user name][$?][system name]."
  (let ((user (eshell-user-name)))
    (propertize (format "%s@%s" user
                        (car (split-string (system-name) "\\.")))
                'face 'eshell-better-prompt-user-face)))

(defun eshell-better-prompt-vcs-section ()
  "Return vcs string."
  (let ((branch (car (vc-git-branches))))
    (when branch
      (propertize
       (mapconcat 'identity
                  (delq nil
                        `(,(eshell-better-prompt-git-weather (eshell/pwd))
                          ,(mapconcat
                            'identity
                            (delq nil `(,(magit-get-remote) ,branch))
                            "/")
                          ,(magit-get-current-tag)))
                  "|")
       'face 'eshell-better-prompt-vcs-face))))

(defun eshell-better-prompt-time-section ()
  "Return time string."
  (propertize
   (format-time-string eshell-better-prompt-time-format)
   'face 'eshell-better-prompt-time-face))

(defun eshell-better-prompt-status-section ()
  "Return status code."
  (unless (zerop eshell-last-command-status)
    (propertize (format " %i " eshell-last-command-status)
                'face 'eshell-better-prompt-status-error-face)))

(defun eshell-better-prompt-pwd-section ()
  "Return pwd string."
  (propertize
   (eshell-better-prompt-shortened-path
    (abbreviate-file-name (eshell/pwd)) eshell-better-prompt-limit)
   'face 'eshell-ls-directory-face))

(defconst eshell-prompt-function
  '(lambda ()
     (eshell-better-prompt-format
      eshell-better-prompt-head
      (eshell-better-prompt-split
       (mapcar (lambda (func) (funcall func))
               eshell-better-prompt-functions)))))

(defun eshell-better-prompt-split (strings)
  "Add newline to the STRINGS if it's longer than `eshell-better-prompt-limit'."
  (cl-loop
   with last = (1- (length strings))
   for i from 0 to last
   for str  = (nth i strings)
   for next = (nth (1+ i) strings)
   collect str into newlist
   if (<= 1 i) ;; Calculate length of current line
   for len = (cl-loop
              for str in (reverse newlist)
              until (string= "\n" str)
              collect str into tmplist
              finally return (+ (* 2 (1+ (length tmplist)))
                                (cl-reduce '+ (append tmplist (list next)) :key 'length)))
   if (and (< eshell-better-prompt-limit (or len 0))
           (not (= last i)))
   collect "\n" into newlist
   finally return newlist))

(defun eshell-better-prompt-format (head args)
  "Make string from HEAD and ARGS."
  (let ((section
         (propertize eshell-better-prompt-section 'face
                     (if eshell-send-direct-to-subprocesses
                         'eshell-better-prompt-direct-input-face
                       'eshell-better-prompt-delay-input-face))))
    (format "%s%s%s\n%s"
            section (mapconcat 'identity (delq nil args) section) section head)))

(defun eshell-better-prompt-git-weather (directory)
  "Return the weather that represents current git state with DIRECTORY."
  (cond
   ((vc-git-conflicted-files directory)
    "⚡")
   ((magit-rebase-info)
    "☂")
   ((magit-anything-modified-p)
    "☁")
   (t "☀")))

(defun eshell-better-prompt-fontify-irregular-prompt ()
  "Fontify irregular prompt string."
  (let ((p (point)))
    (when (and (not (member 'read-only (text-properties-at (1- (point)))))
               (string-match eshell-better-prompt-ordinary-prompt-regex
                             (thing-at-point 'line)))
      (add-text-properties
       (point-at-bol) p
       '(read-only t
                   font-lock-face eshell-prompt
                   front-sticky (font-lock-face read-only)
                   rear-nonsticky (font-lock-face read-only))))))

(advice-add 'eshell-run-output-filters :after
            'eshell-better-prompt-fontify-irregular-prompt)

;; I stole from here: http://www.emacswiki.org/emacs/EshellPrompt
;; Thank you EmacsWiki!
(defun eshell-better-prompt-shortened-path (path max-len)
  "Return a modified version of `PATH'.
replacing some components with single characters starting from the
left to try and get the path down to `MAX-LEN'"
  (let* ((components (split-string path "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

;; Hooks
(add-hook 'eshell-before-prompt-hook 'eshell-better-prompt-set-window-limit)
(add-hook 'eshell-post-command-hook  'eshell-better-prompt-set-window-limit)

;; Initialize
(eshell-better-prompt-initialize)

(provide 'eshell-better-prompt)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; eshell-better-prompt.el ends here
