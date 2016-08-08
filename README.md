[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

## mysql-to-org-mode for emacs
Minor mode for emacs to output the results of mysql queries to org tables, with auto completion of table and column names and parameter replacement.

<p align="center">
<img src="https://raw.github.com/mallt/mysql-to-org-mode/master/mysql-to-org-mode.gif" alt="mysql-to-org-mode screencast"/>
</p>

## Installation
Change the mysql command or the mysql user through the variables `mysql-to-org-mysql-command` variable (defaults to `"mysql"`) and `mysql-to-org-mysql-user` (defaults to `"root"`) if necessary.

## Usage
1. Activate the minor mode by pressing <kbd>M-x</kbd> `mysql-to-org-mode` or adding the mode as a hook to another mode, f.ex. `(add-hook 'php-mode-hook 'mysql-to-org-mode`).
2. On first activiation of the mode, you will be asked to enter a password to connect to mysql.
3. When the mode is active, the following commands are available:
  * <kbd>C-c C-m e</kbd> (`mysql-to-org-eval`): evaluate the mysql query inside the active region or current line.
  * <kbd>C-c C-m p</kbd> (`mysql-to-org-eval-string-at-point`): evaluate the string at point.
  * <kbd>C-c C-m s</kbd> (`mysql-to-org-scratch`): open a scratch buffer to evaluate mysql queries.
4. If a query contains parameters starting with a colon, you will be asked to supply values for these parameters in the minibuffer to evaluate the query.
5. All table names and column names are available for auto completion using company mode.
6. Query results are available as org tables.


[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
