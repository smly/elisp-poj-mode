;;; poj-mode.el --- minor mode of peking university online judge

;; Copyright (C) 2009 Kohei Ozaki

;; Author: Kohei Ozaki <eowner@gmail.com>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(global-set-key "\C-c\C-g" 'poj-mode)

(defvar poj-mode nil)

(defvar poj-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ct" 'poj-get-timeline)
    (define-key map "\C-cu" 'poj-get-timeline-ask)
    (define-key map "\C-cs" 'poj-submit-current)
    map))

(defconst poj-mode-version
  "0.1" "Version number of poj-mode.el")

(defgroup poj nil
  "minor mode for Peking University Online Judge"
  :prefix "poj-"
  :group 'hypermedia)

(defcustom poj-url "http://acm.pku.edu.cn/JudgeOnline/"
  "poj address"
  :type 'string
  :group 'poj)

(defcustom poj-proxy ""
  "proxy for curl"
  :type 'symbol
  :group 'poj)

(defcustom poj-usrid nil
  "poj user name"
  :type 'string
  :group 'poj)

(defvar poj-curl-command
  "curl" "curl command")

(defcustom poj-directory
  (expand-file-name "~/.poj/")
  "poj directory"
  :type 'directory
  :group 'poj)

(defvar poj-tmpfile
  (expand-file-name
   (concat poj-directory ".temp")))

(defcustom poj-cookie
  (expand-file-name
   (concat poj-directory "Cookie@poj"))
  "cookie name"
  :type 'file
  :group 'poj)

;;
;; mode line
(if (not (assq 'poj-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(poj-mode " POJ")
                minor-mode-alist)))

;;
;; mode func
(defun poj-mode (&optional arg)
  "poj mode"
  (interactive)
  ;; mode variable settings
  (cond
   ((< (prefix-numeric-value arg) 0)
    (setq poj-mode nil))
   (arg
    (setq poj-mode t))
   (t
    (setq poj-mode (not poj-mode))))
  ;; content
  (if poj-mode
      (progn
        (unless (file-exists-p poj-directory)
          (make-directory poj-directory t))
        (setq minor-mode-map-alist
              (cons (cons 'poj-mode poj-mode-map)
                    minor-mode-map-alist))
        )
    nil)
  (recenter)
  )
;;
;; other func

(defun poj-url-encode-string (str &optional coding)
  "copy from w3m-url-encode-string"
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (cond
             ((eq ch ?\n)
              "%0D%0A")
             ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
              (char-to-string ch)) ; printable
             ((char-equal ch ?\x20) ; space
              "+")
             (t
              (format "%%%02x" ch)))) ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string (or str "")
                                        (or coding
                                            buffer-file-coding-system
                                            'iso-2022-7bit))
                  nil))))


(defun poj-submit-post-request (pid lid)
  (let*
      ((body (poj-url-encode-string (buffer-string) buffer-file-coding-system))
       (post-data
        (concat "problem_id=" pid
                "&language=" lid
                "&source=" body)))
    (with-temp-file poj-tmpfile
      (insert post-data))
    (call-process poj-curl-command nil nil nil
                  "-b" poj-cookie
                  "-x" poj-proxy
                  "--data" (concat "@" poj-tmpfile)
                  (concat poj-url "submit"))
))

(defun poj-submit-current ()
  (interactive)
  (poj-login)
  (let*
      ((problemid (read-string "problem-id ? : "))
       (languagetype (read-string "language ? : ")))
    (message "submitting...")
    (poj-submit-post-request problemid languagetype))
  (message "submitted...")
  (poj-logout)
  )

(defun poj-rregex (pttr str)
  (goto-char (point-min))
  (while (re-search-forward pttr nil t)
    (replace-match str)))

(defun poj-get-timeline ()
  (interactive)
  (message "checking timeline of %s ..." poj-usrid)
  (poj-get-timeline-user poj-usrid))

(defun poj-get-timeline-ask ()
  (interactive)
  (let (usrid str)
    (setq usrid (read-string "userid ? : "))
    (poj-get-timeline-user usrid)))

(defvar poj-status-buffer "*poj-status*" "Buffer Name for poj-status.")

(defun poj-get-timeline-user (usrid)
  (let ((urldata (concat poj-url
                         "status?problem_id=&user_id="
                         usrid)))
    (call-process poj-curl-command nil nil nil
                  "-o" poj-tmpfile
;                  "-b" poj-cookie
                  urldata))
  (if (string= poj-status-buffer (buffer-name))
      (if (one-window-p)
          (kill-buffer poj-status-buffer)
        (progn
          (kill-buffer poj-status-buffer)
          (delete-window)
          )) nil)

  (if (get-buffer poj-status-buffer)
      (kill-buffer poj-status-buffer))

  (if (one-window-p)
      (split-window-vertically))

  (switch-to-buffer-other-window poj-status-buffer)

  (insert (with-temp-buffer
            (insert (with-temp-buffer
                      "*poj-status*"
                      (insert-file-contents poj-tmpfile)
                      (if (string-match "<tr align=center>\\(\n\\|.\\)+</table>" (buffer-string))
                          (match-string 0 (buffer-string)) nil)))
            (poj-rregex "<tr align=center><td>\\([^<]*\\)</td><td><a href=userstatus[^>]*>\\([^<]*\\)</a></td>" "\\1\t\\2\t")
            (poj-rregex "<td><a href=problem[^>]*>\\([^<]*\\)</a></td>" "\\1\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Compiling</font>\\(</a>\\)?</td>" "--\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Running & Judging</font>\\(</a>\\)?</td>" "--\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Accepted</font>\\(</a>\\)?</td>" "AC\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Presentation Error</font>\\(</a>\\)?</td>" "PR\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Time Limit Exceeded</font>\\(</a>\\)?</td>" "TLE\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Memory Limit Exceeded</font>\\(</a>\\)?</td>" "MLE\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Wrong Answer</font>\\(</a>\\)?</td>" "WA\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Runtime Error</font>\\(</a>\\)?</td>" "RE\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Output Limit Exceeded</font>\\(</a>\\)?</td>" "OLE\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Compile Error</font>\\(</a>\\)?</td>" "CE\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>System Error</font>\\(</a>\\)?</td>" "SE\t")
            (poj-rregex "<td>\\(<a[^>]*>\\)?<font color=[^>]*>Validator Error</font>\\(</a>\\)?</td>" "VE\t")
            (poj-rregex "<td>\\([^<]*\\)</td>" "\\1\t")
            (poj-rregex "\t</tr>" "")
            (while (search-forward "</table>" nil t)
              (replace-match ""))
            (buffer-string)
            ))
  (goto-char (point-min))
;  (insert-file-contents poj-tmpfile)
  (message "finished"))

(defun poj-login ()
  (message (concat "logging in to \"" poj-url "\" as \"" poj-usrid "\""))
  (let (pass str)
    (setq pass (read-passwd "password ? : "))
    (call-process poj-curl-command nil nil nil
                  "-k" "-c" poj-cookie
                  "-x" poj-proxy
                  "-d" (concat "user_id1=" poj-usrid)
                  "-d" (concat "password1=" pass)
                  "-d" "url=/JudgeOnline/"
                  (concat poj-url "/login"))))

(defun poj-logout ()
  (call-process poj-curl-command nil nil nil
                "-b" poj-cookie
                "-x" poj-proxy
                (concat poj-url "login?action=logout&url=/JudgeOnline/"))
  (message "logged out from acm.pku.edu.cn"))

;; mode provider
(provide 'poj-mode)
;;; poj-mode.el ends here
