;;; mysql-to-org-mode.el --- Minor mode to output the results of mysql queries to org tables -*- lexical-binding: t -*-

;; Copyright © 2016 Tijs Mallaerts
;;
;; Author: Tijs Mallaerts <tijs.mallaerts@gmail.com>

;; Package-Requires: ((emacs "24.3") (s "1.11.0") (company "0.9.0") (expand-region "0.10.0"))

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

;; Minor mode to output the results of mysql queries to org tables.
;; C-c m e will output the query inside the active region or current line to an org table.
;; C-c m p will output the query defined by the string at point to an org table.
;; C-c m s will open a mysql to org scratch buffer.

;;; Code:

(require 'org)
(require 's)
(require 'company)
(require 'comint)
(require 'sql)
(require 'expand-region)

(defgroup mysql-to-org nil
  "Mysql to org customizations."
  :group 'processes)

(defcustom mysql-to-org-mysql-command "mysql"
  "Path to the mysql command."
  :type 'string
  :group 'mysql-to-org)

(defcustom mysql-to-org-mysql-user "root"
  "The mysql user."
  :type 'string
  :group 'mysql-to-org)

(defvar mysql-to-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m e") 'mysql-to-org-eval)
    (define-key map (kbd "C-c m p") 'mysql-to-org-eval-string-at-point)
    (define-key map (kbd "C-c m s") 'mysql-to-org-scratch)
    map))

(defun mysql-to-org--eval-process-filter (proc str)
  "The evaluation filter for the mysql to org PROC.
STR is the output string of the PROC."
  (let* ((buf (get-buffer-create "mysql-to-org-output")))
    (with-current-buffer buf
      (org-mode)
      (if (string-match-p "mysql>" str)
          (progn (insert str)
                 (save-excursion
                   (goto-char (point-min))
                   (when (string= "+" (thing-at-point 'char))
                     (kill-line)
                     (forward-line 2)
                     (kill-line)
                     (insert "|--")
                     (org-cycle)))
                 (goto-char (point-max))
                 (beginning-of-line)
                 (kill-line))
        (insert str)))))

(defun mysql-to-org--company-process-filter (proc str)
  "The company filter for the mysql to org PROC.
STR is the output string of the PROC."
  (with-current-buffer (get-buffer-create "mysql-to-org-company")
    (insert str)))

(defun mysql-to-org--load-company-candidates ()
  "Load company completion candidates into mysql-to-org-company buffer."
  (let* ((proc (get-process "mysql-to-org")))
    (while (not proc)
      (setq proc (get-process "mysql-to-org")))
    (set-process-filter proc
                        'mysql-to-org--company-process-filter)
    (process-send-string proc "SELECT DISTINCT TABLE_NAME FROM information_schema.tables;\n")
    (process-send-string proc "SELECT DISTINCT COLUMN_NAME FROM information_schema.columns;\n")))

(defun mysql-to-org--company-backend (command &optional arg &rest ignored)
  "Mysql to org company backend for completing of table and column names."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'mysql-to-org--company-backend))
    (prefix (company-grab-symbol))
    (candidates (mapcar 'car
                        (remove-if-not
                         (lambda (x) (string-prefix-p arg (car x)))
                         (with-current-buffer (get-buffer "mysql-to-org-company")
                           (s-match-strings-all "\\_<[a-z|_|0-9]*+\\_>"
                                                (buffer-substring-no-properties
                                                 (point-min) (point-max)))))))
    (meta (format "%s" arg))))

(defun mysql-to-org--replace-query-params (query)
  "Replace the parameters of the QUERY by values supplied by the user."
  (let* ((matches (s-match-strings-all ":\\w+" query))
         (replacements (mapcar (lambda (x)
                                 (cons (car x)
                                       (read-string (concat "value for "
                                                            (car x)
                                                            " => "))))
                               matches)))
    (if matches
        (s-replace-all replacements query)
      query)))

(defun mysql-to-org--start-process ()
  "Start the mysql to org mysql process."
  (when (not (get-process "mysql-to-org"))
    (make-comint "mysql-to-org" "mysql" nil
                 (concat "-u" mysql-to-org-mysql-user)
                 (concat "-p" (read-passwd "mysql passwd: ")))
    (mysql-to-org--load-company-candidates))
  (add-to-list 'company-backends 'mysql-to-org--company-backend))

;;;###autoload
(defun mysql-to-org-eval ()
  "Evaluate the query inside the active region or current line."
  (interactive)
  (save-excursion
    (let* ((proc (get-process "mysql-to-org"))
           (reg-beg (if (region-active-p)
                        (region-beginning)
                      (line-beginning-position)))
           (reg-end (if (region-active-p)
                        (region-end)
                      (line-end-position)))
           (region (buffer-substring-no-properties reg-beg reg-end))
           (replace (replace-regexp-in-string ";" "" region))
           (query (concat (s-collapse-whitespace (s-trim replace)) ";\n")))
      (set-process-filter proc
                          'mysql-to-org--eval-process-filter)
      (with-current-buffer (get-buffer-create "mysql-to-org-output")
        (erase-buffer))
      (process-send-string proc (mysql-to-org--replace-query-params query))
      (display-buffer "mysql-to-org-output"))))

;;;###autoload
(defun mysql-to-org-eval-string-at-point ()
  "Evaluate the string at point."
  (interactive)
  (save-excursion
    (er/mark-inside-quotes)
    (mysql-to-org-eval)))

;;;###autoload
(defun mysql-to-org-scratch ()
  "Open mysql to org scratch buffer."
  (interactive)
  (get-buffer-create "*mysql-to-org-scratch*")
  (switch-to-buffer "*mysql-to-org-scratch*")
  (sql-mode)
  (mysql-to-org-mode))

;;;###autoload
(define-minor-mode mysql-to-org-mode
  "Minor mode to output the results of mysql queries to org tables."
  :lighter " mysql-to-org"
  :keymap mysql-to-org-mode-map
  :after-hook (mysql-to-org--start-process))

(provide 'mysql-to-org-mode)

;;; mysql-to-org-mode.el ends here
