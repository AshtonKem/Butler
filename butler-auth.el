;;;; butler-auth.el -- Authentication setup for Butler.
;;

;; Copyright © 2012-2013 Ashton Kemerling
;;
;; Author: Ashton Kemerling <ashtonkemerling@gmail.com>
;; URL: http://www.github.comc/AshtonKem/Butler.git
;; Version: 0.1.1
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


(defun basic-auth (username password)
    (concat "Basic "
            (base64-encode-string
             (concat username ":" password))))

(defun parse-authinfo-file (filename servername)
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (search-forward (concat "machine " servername))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring line-start line-end))
               (splitted (split-string line " "))
               (filtered (delq "" splitted))
               (username (car (cdr (member "login" filtered))))
               (password (car (cdr (member "password" filtered)))))
          (if (and username password)
              (basic-auth username password))))))

(defun auth-string (server)
  (let* ((args (cdr (cdr server)))
         (name (car (cdr server)))
         (username (cdr (assoc 'server-user args)))
         (password (cdr (assoc 'server-password args)))
         (auth-file (cdr (assoc 'auth-file args))))
    (if auth-file
        (parse-authinfo-file auth-file name)
      (basic-auth username password))))


(provide 'butler-auth)
