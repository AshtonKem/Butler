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

(defvar jenkins-servers nil)
(require 'json)
(defun get-server (name)
  (car (delq nil (mapcar #'(lambda (obj)
			     (if (string= name (car (cdr obj)))
				 obj
			       nil))
			 jenkins-servers))))

(defun parse-jobs (status)
  (goto-char (point-min))
  (search-forward "{")
  (backward-char)
  (let* ((raw (buffer-substring (point) (point-max)))
	 (parsed (json-read-from-string raw))
	 (jobs (cdr (assoc 'jobs parsed))))
    jobs))


(defun update-butler-status (status)
  (let ((jobs (parse-jobs status))
	(buffer (get-buffer-create "*butler-status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (mapcar (lambda (job)
		(let ((name (cdr (assoc 'name job)))
		      (color (cdr (assoc 'color job))))
		  (cond
		   ((string= color  "red")
		    (insert (propertize "● " 'face `(:foreground ,color))))
		   ((string= color "yellow")
		    (insert (propertize "● " 'face `(:foreground ,color))))
		   ((string= color  "blue")
		    (insert (propertize "● " 'face `(:foreground ,color))))
		   ((string= color  "grey")
		    (insert (propertize "● " 'face `(:foreground ,color))))
		   ((string= color "disabled")
		    (insert (propertize "● " 'face `(:foreground "black"))))
		   (t (insert (concat "Unknown: " color))))
		  (insert name)
		  (insert "\n")))
	      jobs))))


(defun get-jobs (server)
  (interactive)
  (let* ((url-request-method "GET")
	 (args (cdr (cdr server)))
	 (username (cdr (assoc 'jenkins-user args)))
	 (password (cdr (assoc 'jenkins-password args)))
	 (url (cdr (assoc 'jenkins-address args)))
	 (buffer (generate-new-buffer "jenkins"))
	 (url-request-extra-headers
	  `(("Authorization" . ,(concat "Basic "
					(base64-encode-string
					 (concat username ":" password)))))))
    (url-retrieve (concat url "/api/json?pretty=true") #'update-butler-status)
    (switch-to-buffer (get-buffer-create "*butler-status*"))
    ))
