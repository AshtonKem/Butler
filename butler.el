;;; butler.el --- Client for Jenkins

;; Copyright © 2012-2013 Ashton Kemerling
;;
;; Author: Ashton Kemerling <ashtonkemerling@gmail.com>
;; URL: http://www.github.comc/AshtonKem/Butler.git
;; Version: 0.0.1
;; Keywords: Jenkins, Hudson, CI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides an interface to connect to the Jenkins CI server

;;; Installation

;; Will eventually be available in elpa or marmalade


;;; Code:

(defvar butler-servers nil)
(defun butler-buffer ()
  (get-buffer-create "*butler-status*"))
(define-derived-mode butler-mode fundamental-mode "Butler"
  "A major mode for interacting with various CI servers")
(require 'json)
(defun get-server (name)
  (car (delq nil (mapcar #'(lambda (obj)
			     (if (string= name (car (cdr obj)))
				 obj
			       nil))
			 butler-servers))))


(defvar butler-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "g") 'magit-status)
    map))


(defun parse-jobs ()
  (goto-char (point-min))
  (search-forward "{")
  (backward-char)
  (let* ((raw (buffer-substring (point) (point-max)))
	 (parsed (json-read-from-string raw))
	 (jobs (cdr (assoc 'jobs parsed))))
    jobs))


(defun update-butler-status (unused target-buffer callback)
  (let ((jobs (parse-jobs)))
    (with-current-buffer target-buffer
      (mapcar (lambda (job)
		(let ((name (cdr (assoc 'name job)))
		      (inhibit-read-only t)
		      (color (cdr (assoc 'color job))))
		  (insert "    ")
		  (cond
		   ((string= color  "red")
		    (insert (propertize "● " 'face `(:foreground ,color))))
		   ((string= color "yellow")
		    (insert (propertize "● " 'face `(:foreground ,color))))
		   ((string= color  "blue")
		    (insert (propertize "● " 'face `(:foreground ,color))))
		   ((string= color  "grey")
		    (insert (propertize "● " 'face `(:foreground ,color))))
                   ((string= color  "aborted")
		    (insert (propertize "● " 'face `(:foreground "grey"))))
		   ((string= color "disabled")
		    (insert (propertize "● " 'face `(:foreground "black"))))
		   ((string= (subseq color -6) "_anime")
		    (insert (propertize "● " 'face `(:foreground ,(subseq color 0 -6)))))
		   (t (insert (concat "Unknown: " "'" color "' "))))
		  (insert name)
		  (insert "\n")))
	      jobs)
      (funcall callback))))


(defun get-jobs (server buffer callback)
  (let* ((url-request-method "GET")
	 (args (cdr (cdr server)))
	 (username (cdr (assoc 'server-user args)))
	 (password (cdr (assoc 'server-password args)))
	 (url (cdr (assoc 'server-address args)))
	 (url-request-extra-headers
	  `(("Authorization" . ,(concat "Basic "
					(base64-encode-string
					 (concat username ":" password)))))))
    (url-retrieve (concat url "/api/json?pretty=true")
                  #'update-butler-status
                  (list buffer callback))))

(defun draw-butler (buffer callback)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)))
    (dolist (server butler-servers)
      (let ((name (car (cdr server)))
	    (inhibit-read-only t)
	    (address (cdr (assoc 'server-address (cdr (cdr server))))))
        (goto-char (point-max))
	(insert (concat name " (" address "):\n"))
	(get-jobs server buffer
                  (if (equal server (car (last butler-servers)))
                      callback
                    (lambda ())))))))


(defun butler-status ()
  (interactive)
  (lexical-let ((target-point nil))
    (with-current-buffer (butler-buffer)
      (setq target-point (or (point) 0)))
    (erase-buffer)
    (draw-butler (butler-buffer) (lambda ()
                                   (switch-to-buffer (butler-buffer))
                                   (goto-char target-point)))
    (switch-to-buffer (butler-buffer))
    (setq buffer-read-only t)))

(defun butler-refresh ()
  (interactive)
  (lexical-let ((target-point nil)
                (target-buffer (generate-new-buffer "temp")))
    (with-current-buffer (butler-buffer)
      (setq target-point (or (point) 0)))
    (draw-butler target-buffer (lambda ()
                                 (let ((results (buffer-string))
                                       (inhibit-read-only t))
                                   (with-current-buffer (butler-buffer)
                                     (erase-buffer)
                                     (insert results)
                                     (goto-char target-point))
                                   (switch-to-buffer (butler-buffer))
                                   (kill-buffer target-buffer))))))
