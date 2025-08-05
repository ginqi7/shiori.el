;;; shiori.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `shiori-login'
;;    Login.
;;  `shiori-add'
;;    Add Bookmark.
;;  `shiori-bookmarks'
;;    List Bookmarks.
;;  `shiori-delete-article'
;;    Open an article.
;;  `shiori-open-article'
;;    Open an article.
;;  `shiori-bookmarks-mode'
;;    Major mode for handling a list of shiori unread.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `shiori-password'
;;    The Password of Shiori.
;;    default = nil
;;  `shiori-url'
;;    The URL of Shiori.
;;    default = nil
;;  `shiori-username'
;;    The User Name of Shiori.
;;    default = nil

;;; Code:

(defcustom shiori-password nil
  "The Password of Shiori."
  :group 'shiori
  :type 'string)

(defcustom shiori-url nil
  "The URL of Shiori."
  :group 'shiori
  :type 'string)

(defcustom shiori-username nil
  "The User Name of Shiori."
  :group 'shiori
  :type 'string)

(defvar shiori--apis
  (list
   :login (list
           :path "/api/v1/auth/login"
           :method "POST"
           :heads '("Content-Type: application/json")
           :body "{\"username\": \":username\" ,
                   \"password\": \":password\" ,
                   \"remember\": true}")
   :bookmarks (list
               :heads '("Content-Type: application/json"
                        "Authorization: Bearer :token")
               :path "/api/bookmarks"
               :method "GET")
   :article (list
             :heads '("Content-Type: application/json"
                      "Authorization: Bearer :token")
             :path "/bookmark/:id/content"
             :method "GET")
   :add (list
         :heads '("Content-Type: application/json"
                  "Authorization: Bearer :token")
         :path "/api/bookmarks"
         :method "POST"
         :body "{\"url\": \":new-url\"}")
   :delete (list
            :heads '("Content-Type: application/json"
                     "Authorization: Bearer :token")
            :path "/api/bookmarks"
            :method "DELETE"
            :body "[:ids]")))

(defvar shiori--article-buffer "*shiori-article*"
  "The article buffer name of Shiori.")

(defvar shiori--bookmarks-buffer "*shiori-bookmarks*"
  "The bookmarks buffer name of Shiori.")

(defvar shiori--expires nil
  "The Token Expires of Shiori.")

(defvar shiori--token nil
  "The Token of Shiori.")

(defun shiori--plist-keys (plist)
  "Return all keys in the PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun shiori--format (str &rest args)
  "Format a STR by ARGS."
  (let ((keys (shiori--plist-keys args)))
    (dolist (key keys)
      (setq str (string-replace
                 (symbol-name key)
                 (plist-get args key)
                 str)))
    str))

(defun shiori-api-cmd (name &rest args)
  "Get API command string by NAME and ARGS."
  (let* ((api (plist-get shiori--apis name))
         (url (concat shiori-url (plist-get api :path)))
         (heads (plist-get api :heads))
         (body (plist-get api :body))
         (method (plist-get api :method))
         (cmd "curl -s -X :method :heads :body :url"))
    (setq heads
          (string-join
           (mapcar (lambda (head)
                     (format "-H '%s'" head))
                   heads)
           " "))
    (when body
      (setq body (format "-d '%s'" body)))

    (setq cmd (shiori--format cmd
                              :heads heads
                              :body body
                              :token shiori--token
                              :method method
                              :url url))
    (apply #'shiori--format cmd args)))

(defun shiori-login ()
  "Login."
  (interactive)
  (let ((cmd
         (shiori-api-cmd :login
                         :username
                         shiori-username
                         :password
                         shiori-password))

        (response)
        (ok)
        (msg))
    (setq response (json-parse-string (shell-command-to-string cmd)))
    (setq ok (gethash "ok" response))
    (setq msg (gethash "message" response))
    (if (equal ok :false)
        (message "Shiori Login Error: %s" msg)
      (setq shiori--token (gethash "token" msg))
      (setq shiori--expires (gethash "expires" msg))
      (message "Shiori Login Success"))))

(defun shiori-auth-check ()
  "Authentication check."
  (unless shiori-url
    (message "You must specify the value for `shiori-url'."))
  (unless shiori-username
    (message "You must specify the value for `shiori-username'."))
  (unless shiori-password
    (message "You must specify the value for `shiori-password'."))
  (unless (and shiori--expires
               (< (float-time) shiori--expires))
    (shiori-login)))

(defun shiori-add ()
  "Add Bookmark."
  (interactive)
  (shiori-auth-check)
  (let ((cmd (shiori-api-cmd
              :add
              :new-url (read-string "Add new bookmark: ")
              :token shiori--token))
        (response)
        (bookmarks))
    (setq response (json-parse-string (shell-command-to-string cmd)))
    (message "Add Bookmark Success.")))

(defun shiori-article-delete (article-id)
  "Render article by ARTICLE-ID."
  (shiori-auth-check)
  (let ((cmd (shiori-api-cmd
              :delete
              :token
              shiori--token
              :ids
              article-id)))
    (shell-command-to-string cmd)
    (message "Delete Success.")))

(defun shiori-render-html (html buffer)
  "Render HTML in BUFFER."
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (switch-to-buffer buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (shr-insert-document
   (with-temp-buffer
     (insert html)
     (libxml-parse-html-region (point-min) (point-max))))
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun shiori-article-render (article-id)
  "Render article by ARTICLE-ID."
  (shiori-auth-check)
  (let ((cmd (shiori-api-cmd
              :article
              :token
              shiori--token
              :id
              article-id)))
    (shiori-render-html (shell-command-to-string cmd) (get-buffer-create shiori--article-buffer))))

(defun shiori-bookmarks-render (bookmarks)
  "Render BOOKMARKS."
  (switch-to-buffer (get-buffer-create shiori--bookmarks-buffer))
  (let ((buffer-read-only))
    (erase-buffer)
    (setq tabulated-list-entries
          (mapcar (lambda (item)
                    (list (number-to-string (gethash "id" item))
                          (vector
                           (gethash "title" item)
                           ;; (gethash "author" item)
                           (gethash "createdAt" item))))
                  bookmarks))
    (shiori-bookmarks-mode)
    (goto-char (point-min))))

(defun shiori-bookmarks ()
  "List Bookmarks."
  (interactive)
  (shiori-auth-check)
  (let ((cmd (shiori-api-cmd
              :bookmarks
              :token shiori--token))

        (response)
        (bookmarks))
    (setq response (json-parse-string (shell-command-to-string cmd)))
    (setq bookmarks (gethash "bookmarks" response))
    (shiori-bookmarks-render bookmarks)))

(defun shiori-delete-article ()
  "Open an article."
  (interactive)
  (when (yes-or-no-p
         (format "Do you want delete article %s"
                 (aref (tabulated-list-get-entry) 0)))
    (shiori-article-delete (tabulated-list-get-id))))

(defun shiori-open-article ()
  "Open an article."
  (interactive)
  (shiori-article-render (tabulated-list-get-id)))

(define-derived-mode shiori-bookmarks-mode tabulated-list-mode "Contexts Menu"
  "Major mode for handling a list of shiori unread."
  (setq tabulated-list-format [("Title" 50 t)
                               ;; ("Author" 20 t :right-align t)
                               ("Time"  10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'shiori-open-article)
    (define-key map (kbd "d") 'shiori-delete-article)
    (define-key map (kbd "a") 'shiori-add)
    (define-key map (kbd "r") 'shiori-bookmarks)
    (use-local-map map)))

(provide 'shiori)
;;; shiori.el ends here
