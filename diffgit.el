;;; diffgit.el --- Diff git repo with difftastic

;; Filename: diffgit.el
;; Description: diff git repo with diffstatic
;; Author: Bitnut <940095072@qq.com>
;; Maintainer: Bitnut <940095072@qq.com>
;; Copyright (C) 2018, Bitnut, all rights reserved.
;; Created: 2022-10-05 19:08:02
;; Version: 0.1.0
;; Last-Updated: 2022-10-05 19:08:02
;;           By: Bitnut
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;

;;; Commentary:
;;
;; diffgit is a git diff tool depends on diffstatic.

;;; Code:

(define-derived-mode diffgit-mode text-mode "diffgit"
  (kill-all-local-variables)
  (setq major-mode 'diffgit-mode)
  (setq mode-name "diffgit")
  (read-only-mode 1)
  (run-hooks 'diffgit-mode-hook))

(defgroup diffgit nil
  "Git diff using diffstatic"
  :group 'diffgit)

(defcustom diffgit-buffer "*diffgit*"
  "Diff result genrated by diffstatic."
  :type 'string
  :group 'diffgit)

(defcustom diffgit-tmp-buffer "*diffgit-tmp*"
  "The buffer name of tmp buffer."
  :type 'string
  :group 'diffgit)

(defcustom diffgit-mode-hook '()
  "Diffgit mode hook."
  :type 'hook
  :group 'diffgit)

(defun diffgit-create-or-erase-buffer ()
  "Erase or create buffer for result display."
  (if (get-buffer diffgit-buffer)
      (with-current-buffer diffgit-buffer
        (let ((inhibit-read-only t))
          ;; Switch to `diffgit-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
          (diffgit-mode)
          ;; Erase buffer content
          (read-only-mode -1)
          (erase-buffer)))
    (generate-new-buffer diffgit-buffer)))

(defun diffgit-build-command (&optional commit)
  "Build command for invoing difftastic through git."
  (if (not commit)
      (list "git" "difftool")
    (list "git" "difftool" (concat commit "^.." commit)))
  )

(defun diffgit-colorize-output (proc)
  "Colorize terminal output based on user config, `PROC' is the difft program run by git."
  (if (require 'xterm-color nil 'noerror)
      (let ((inhibit-read-only t))
        (message "using xterm-color")
        (with-current-buffer (process-buffer proc)
          (xterm-color-colorize-buffer)))
    (let ((inhibit-read-only t))
      (message "Cannot find xterm-color using default ansi-color")
      (with-current-buffer (process-buffer proc)
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun git-diff-guess-commit-at-point ()
  "Try guess commit at point."
  (thing-at-point 'word))

(defun diffgit-gen-diff (command)
  "Run difft and get output."
  (let ((inhibit-read-only t))
    (diffgit-create-or-erase-buffer)
    ;; run command
    (with-current-buffer diffgit-buffer
      (make-process
       :name "diffgit"
       :buffer diffgit-buffer
       :command command
       :connection-type nil
       :sentinel (lambda (p _m)
                   (when (eq 0 (process-exit-status p))
                     (diffgit-colorize-output p)
                     (pop-to-buffer diffgit-buffer)
                     (goto-char (point-min))
                     (read-only-mode t)))
       ))))

(defun diffgit-gen-work-tree-diff ()
  "Run difft to get diff of work tree."
  (interactive)
  (let (command)
    (setq command (diffgit-build-command))
    (diffgit-gen-diff command)))

(defun diffgit-gen-diff-by-commit ()
  "Run difft through by commit."
  (interactive)
  ;; TODO: Not consider non git repo condition
  (let (commit command)
    (setq commit (read-string "Enter commit hash or skip:"))
    (if (eq (length commit) 0)
        (setq commit (git-diff-guess-commit-at-point)))
    (setq command (diffgit-build-command commit))
    (diffgit-gen-diff command)))

(provide 'diffgit)

;;; diffgit.el ends here
