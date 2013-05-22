;; -*- lexical-binding: t -*-
;;; butler.el --- Client for Jenkins

;; Copyright © 2012-2013 Ashton Kemerling
;;
;; Author: Ashton Kemerling <ashtonkemerling@gmail.com>
;; URL: http://www.github.com/AshtonKem/Butler.git
;; Version: 0.1.3
;; Keywords: Jenkins, Hudson, CI
;; Package-Requires: ((web "0.3.7"))

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


;;; Code:

(require 'json)
(require 'web)
(require 'butler-servers)


(defun butler-buffer ()
  (get-buffer-create "*butler-status*"))

(defvar butler-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "g") 'butler-refresh)
    (define-key map (kbd "t") 'trigger-butler-job)
    map))


(define-derived-mode butler-mode fundamental-mode "Butler"
  "A major mode for interacting with various CI servers"
  (use-local-map butler-mode-map))

(defun refresh-butler-status ()
  (prepare-servers)
  (maphash (lambda (server-name server)
                  (let* ((url-request-method "GET")
                         (url (gethash 'url server))
                         (auth (gethash 'auth server))
                         (headers
                          `(("Authorization" . ,auth))))
                    (puthash 'jobs (make-hash-table :test #'equal) server)
                    (web-http-get (lambda (httpc header data)
                                    (let ((parsed (json-read-from-string data)))
                                      (mapc (lambda (job)
                                              (let ((hash (make-hash-table :test #'equal)))
                                                (puthash 'color (cdr (assoc 'color job))
                                                         hash)
                                                (puthash 'name (cdr (assoc 'name job))
                                                         hash)
                                                (puthash (cdr (assoc 'name job))
                                                         hash
                                                         (gethash 'jobs server))))
                                            (cdr (assoc 'jobs parsed)))))
                                  :url (concat
                                        url
                                        "/api/json?tree=jobs[name,inQueue,color,url,lastBuild[building,duration,estimatedDuration,timestamp,executor[likelyStuck]]]")
                                  :extra-headers headers)))
           butler-hash))


(defun parse-jobs (data)
  (print data)
  (let* ((parsed (json-read-from-string data))
	 (jobs (cdr (assoc 'jobs parsed))))
    jobs))

(defun find-trigger-url (line)
  (let ((start (search "url: " line)))
    (if start
        (substring line (+ start 5)))))

(defun find-trigger-auth ()
  (with-current-buffer (butler-buffer)
    (save-excursion
      (condition-case nil
          (let* ((pos (search-backward "auth: "))
                 (line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line (buffer-substring line-start line-end))
                 (auth-start (search "auth: " line))
                 (auth-string (substring line (+ auth-start 6))))
            auth-string)
          (search-failed nil)))))

(defun trigger-butler-job ()
  (interactive)
  (with-current-buffer (butler-buffer)
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line (buffer-substring line-start line-end))
           (url (find-trigger-url line))
           (auth (find-trigger-auth)))
      (print url)
      (if (and url auth)
          (web-http-get (lambda (conn headers data))
                        :url url
                        :extra-headers `(("Authorization" . ,auth)))))))

(defun generate-progress-string (timestamp expected)
  (let* ((current-time (string-to-number (format-time-string "%s")))
         (milliseconds (* current-time 1000))
         (duration (- milliseconds timestamp))
         (percentage (min
                      (/ (float duration)
                         expected)
                      1.0))
         (rounded (floor (* 10 percentage))))
    (concat " |"
            (make-string rounded ?+)
            (make-string (- 10 rounded) ?\ )
            "| ")))



(defun update-butler-status (data target-buffer callback)
  (let ((jobs (parse-jobs data)))
    (with-current-buffer target-buffer
      (mapc (lambda (job)
              (let* ((name (cdr (assoc 'name job)))
                     (inhibit-read-only t)
                     (color (cdr (assoc 'color job)))
                     (last-build (cdr (assoc 'lastBuild job)))
                     (in-queue (equal t (cdr (assoc 'inQueue job))))
                     (executor (cdr (assoc 'executor last-build)))
                     (likely-stuck (equal t (cdr (assoc 'likelyStuck executor))))
                     (timestamp (cdr (assoc 'timestamp last-build)))
                     (expected-duration (cdr (assoc 'estimatedDuration last-build)))
                     (url (concat "url: "
                                  (cdr (assoc 'url job))
                                  "build/"))
                     (building (equal t
                                      (cdr (assoc 'building last-build)))))
                (insert "    ")
                (cond
                 ((string= color  "red")
                  (insert (propertize "●" 'face `(:foreground ,color))))
                 ((string= color "yellow")
                  (insert (propertize "●" 'face `(:foreground ,color))))
                 ((string= color  "blue")
                  (insert (propertize "●" 'face `(:foreground ,color))))
                 ((string= color  "grey")
                  (insert (propertize "●" 'face `(:foreground ,color))))
                 ((string= color  "aborted")
                  (insert (propertize "●" 'face `(:foreground "grey"))))
                 ((string= color "disabled")
                  (insert (propertize "●" 'face `(:foreground "black"))))
                 ((string= (subseq color -6) "_anime")
                  (insert (propertize "●" 'face `(:foreground ,(subseq color 0 -6)))))
                 (t (insert (concat "Unknown: " "'" color "' "))))
                (if building
                    (if likely-stuck
                        (insert (propertize (generate-progress-string timestamp expected-duration)
                                            'face '(:foreground "res")))
                      (insert (generate-progress-string timestamp expected-duration) ))
                  (if in-queue
                      (insert "    Waiting   ")
                    (insert "              ")))
                (insert name)
                (insert " ")
                (insert (propertize url 'invisible t))
                (insert "\n")))
            jobs)
      (funcall callback))))


(defun get-jobs (server buffer callback)
  (let* ((url-request-method "GET")
         (name (car (cdr server)))
         (parsed (get-server name))
         (url (gethash 'url parsed))
         (auth (gethash 'auth parsed))
         (headers
	  `(("Authorization" . ,auth))))
    (web-http-get (lambda (httpc header data)
                    (update-butler-status data buffer callback))
                  :url (concat
                        url
                        "/api/json?tree=jobs[name,inQueue,color,url,lastBuild[building,duration,estimatedDuration,timestamp,executor[likelyStuck]]]")
                  :extra-headers headers)))



(defun draw-butler (buffer callback)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)))
    (dolist (server butler-server-list)
      (let* ((name (car (cdr server)))
             (parsed-server (get-server (car (cdr server))))
             (inhibit-read-only t)
             (address (gethash 'url parsed-server))
             (auth (gethash 'auth parsed-server)))
        (goto-char (point-max))
	(insert (concat name " (" (org-link-unescape address) "): "))
        (insert (propertize (concat "auth: "
                                    auth)
                            'invisible t))
        (insert "\n")
	(get-jobs server buffer
                  (if (equal server (car (last butler-server-list)))
                      callback
                    (lambda ())))))))


(defun butler-status ()
  (interactive)
  (butler-refresh)
  (switch-to-buffer (butler-buffer))
  (butler-mode))

(defun butler-refresh ()
  (interactive)
  (let ((target-point nil)
        (target-buffer (generate-new-buffer "temp")))
    (refresh-butler-status)
    (with-current-buffer (butler-buffer)
      (setq target-point (or (point) 0)))
    (draw-butler target-buffer (lambda ()
                                 (let ((results (buffer-string))
                                       (inhibit-read-only t))
                                   (with-current-buffer (butler-buffer)
                                     (erase-buffer)
                                     (insert results)
                                     (goto-char target-point)
                                     (setq buffer-read-only t))
                                   (kill-buffer target-buffer))))))


(provide 'butler)


;;; butler.el ends here
