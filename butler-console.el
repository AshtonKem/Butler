;; -*- lexical-binding: t -*-
;;; butler-servers.el --- Code to deal with Jenkins' Console

;; Copyright © 2012-2013 Ashton Kemerling
;;
;; Author: Ashton Kemerling <ashtonkemerling@gmail.com>
;; URL: http://www.github.com/AshtonKem/Butler.git
;; Version: 0.1.3
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

;; Deals with the Jenkins console


;;; Code:

(provide 'butler-console)

(defun get-console-data (buffer server job offset)
  (let* ((url-request-method "GET")
         (base-url (gethash 'url server))
         (auth (gethash 'auth server))
         (url-request-extra-headers
          `(("Authorization" . ,auth)
            ("Content-Type" . "application/json")))
         (url (concat
               (if (string= "/" (substring base-url (- (length base-url) 1)))
                   (substring base-url 0 (- (length base-url) 1))
                 base-url)
               "/job/"
               (gethash 'name job)
               "/lastBuild/logText/progressiveText?start="
               (int-to-string offset))))
    (print url)
    (deferred:$
      (deferred:url-retrieve url)
      (deferred:nextc it
        (lambda (buf)
          (with-current-buffer buf
            (beginning-of-buffer)
            (search-forward-regexp "^\s*$")
            (let ((new-data (buffer-substring (point) (point-max))))
              (with-current-buffer buffer
                (let ((end-of-buffer (= (point) (point-max)))
                      (old-point (point)))
                  (end-of-buffer)
                  (insert-string new-data)
                  (if end-of-buffer
                      (end-of-buffer)
                    (goto-char old-point)))))
            (kill-buffer buf)))))))

;; butler-console.el ends here
