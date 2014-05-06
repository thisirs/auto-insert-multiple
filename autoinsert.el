;;; autoinsert.el --- automatic mode-dependent insertion of text into new files

;; Copyright (C) 1985-1987, 1994-1995, 1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Charlie Martin <crm@cs.duke.edu>
;; Adapted-By: Daniel Pfeiffer <occitan@esperanto.org>
;; Adapted-By: Sylvain Rousseau <thisirs@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  The following defines an association list for text to be
;;  automatically inserted when a new file is created, and a function
;;  which automatically inserts these files; the idea is to insert
;;  default text much as the mode is automatically set using
;;  auto-mode-alist.
;;
;;  To use:
;;     (add-hook 'find-file-hook 'auto-insert)
;;     setq auto-insert-directory to an appropriate slash-terminated value
;;
;;  You can also customize the variable `auto-insert-mode' to load the
;;  package.  Alternatively, add the following to your .emacs file:
;;  (auto-insert-mode 1)
;;
;;  Author:  Charlie Martin
;;           Department of Computer Science and
;;           National Biomedical Simulation Resource
;;           Box 3709
;;           Duke University Medical Center
;;           Durham, NC 27710
;;            (crm@cs.duke.edu,mcnc!duke!crm)

;;; Code:

(defgroup auto-insert nil
  "Automatic mode-dependent insertion of text into new files."
  :prefix "auto-insert-"
  :group 'files
  :group 'convenience
  :link '(custom-manual "(autotype) Autoinserting"))


(defcustom auto-insert 'not-modified
  "Controls automatic insertion into newly found empty files.
Possible values:
        nil     do nothing
        t       insert if possible
        other   insert if possible, but mark as unmodified.
Insertion is possible when something appropriate is found in
`auto-insert-alist'.  When the insertion is marked as unmodified, you can
save it with  \\[write-file] RET.
This variable is used when the function `auto-insert' is called, e.g.
when you do (add-hook 'find-file-hook 'auto-insert).
With \\[auto-insert], this is always treated as if it were t."
  :type '(choice (const :tag "Insert if possible" t)
                 (const :tag "Do nothing" nil)
                 (other :tag "insert if possible, mark as unmodified."
                        not-modified))
  :group 'auto-insert)

(defcustom auto-insert-query 'function
  "Non-nil means ask user before auto-inserting.
When this is `function', only ask when called non-interactively."
  :type '(choice (const :tag "Don't ask" nil)
                 (const :tag "Ask if called non-interactively" function)
                 (other :tag "Ask" t))
  :group 'auto-insert)

(defcustom auto-insert-prompt "Perform %s auto-insertion? "
  "Prompt to use when querying whether to auto-insert.
If this contains a %s, that will be replaced by the matching rule."
  :type 'string
  :group 'auto-insert)


(defcustom auto-insert-alist
  '(("\\.\\([Hh]\\|hh\\|hpp\\)\\'" "C / C++ header"
     (auto-insert-skeleton
      '((upcase
         (concat
          (file-name-nondirectory
           (file-name-sans-extension buffer-file-name))
          "_"
          (file-name-extension buffer-file-name)))
        "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif")))
    ("\\.\\([Cc]\\|cc\\|cpp\\)\\'" "C / C++ program"
     (auto-insert-skeleton
      '(nil "#include \""
            (let
                ((stem
                  (file-name-sans-extension buffer-file-name)))
              (cond
               ((file-exists-p
                 (concat stem ".h"))
                (file-name-nondirectory
                 (concat stem ".h")))
               ((file-exists-p
                 (concat stem ".hh"))
                (file-name-nondirectory
                 (concat stem ".hh")))))
            & 34 | -10)))
    ("[Mm]akefile\\'" "Makefile"
     (auto-insert-raw "makefile.inc"))
    (html-mode "html-mode"
               (sgml-tag "html"))
    (plain-tex-mode "plain-tex-mode"
                    (auto-insert-raw "tex-insert.tex"))
    (bibtex-mode "bibtex-mode"
                 (auto-insert-raw "tex-insert.tex"))
    (latex-mode "latex-mode"
                (auto-insert-skeleton
                 '("options, RET: " "\\documentclass[" str & 93 | -1 123
                   (read-string "class: ")
                   "}\n"
                   ("package, %s: " "\\usepackage["
                    (read-string "options, RET: ")
                    & 93 | -1 123 str "}\n")
                   _ "\n\\begin{document}\n" _ "\n\\end{document}")))
    ("/bin/.*[^/]\\'" "Shell-Script mode magic number"
     (if
         (eq major-mode
             (default-value 'major-mode))
         (sh-mode)))
    (ada-mode "ada-mode"
              (ada-header))
    ("\\.[1-9]\\'" "Man page skeleton"
     (auto-insert-skeleton
      '("Short description: " ".\\\" Copyright (C), "
        (substring
         (current-time-string)
         -4)
        "  "
        (getenv "ORGANIZATION")
        |
        (progn user-full-name)
        "\n.\\\" You may distribute this file under the terms of the GNU Free\n.\\\" Documentation License.\n.TH "
        (file-name-sans-extension
         (file-name-nondirectory
          (buffer-file-name)))
        " "
        (file-name-extension
         (buffer-file-name))
        " "
        (format-time-string "%Y-%m-%d ")
        "\n.SH NAME\n"
        (file-name-sans-extension
         (file-name-nondirectory
          (buffer-file-name)))
        " \\- " str "\n.SH SYNOPSIS\n.B "
        (file-name-sans-extension
         (file-name-nondirectory
          (buffer-file-name)))
        "\n" _ "\n.SH DESCRIPTION\n.SH OPTIONS\n.SH FILES\n.SH \"SEE ALSO\"\n.SH BUGS\n.SH AUTHOR\n"
        (user-full-name)
        '(if
             (search-backward "&"
                              (line-beginning-position)
                              t)
             (replace-match
              (capitalize
               (user-login-name))
              t t))
        '(end-of-line 1)
        " <"
        (progn user-mail-address)
        ">\n")))
    ("\\.el\\'" "Emacs Lisp header"
     (auto-insert-skeleton
      '("Short description: " ";;; "
        (file-name-nondirectory
         (buffer-file-name))
        " --- " str "\n\n;; Copyright (C) "
        (substring
         (current-time-string)
         -4)
        "  "
        (getenv "ORGANIZATION")
        |
        (progn user-full-name)
        "\n\n;; Author: "
        (user-full-name)
        '(if
             (search-backward "&"
                              (line-beginning-position)
                              t)
             (replace-match
              (capitalize
               (user-login-name))
              t t))
        '(end-of-line 1)
        " <"
        (progn user-mail-address)
        ">\n;; Keywords: "
        '(require 'finder)
        '(setq v1
               (mapcar
                (lambda
                  (x)
                  (list
                   (symbol-name
                    (car x))))
                finder-known-keywords)
               v2
               (mapconcat
                (lambda
                  (x)
                  (format "%12s:  %s"
                          (car x)
                          (cdr x)))
                finder-known-keywords "\n"))
        ((let
             ((minibuffer-help-form v2))
           (completing-read "Keyword, C-h: " v1 nil t))
         str ", ")
        & -2 "\n\n;; This program is free software; you can redistribute it and/or modify\n;; it under the terms of the GNU General Public License as published by\n;; the Free Software Foundation, either version 3 of the License, or\n;; (at your option) any later version.\n\n;; This program is distributed in the hope that it will be useful,\n;; but WITHOUT ANY WARRANTY; without even the implied warranty of\n;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n;; GNU General Public License for more details.\n\n;; You should have received a copy of the GNU General Public License\n;; along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\n;;; Commentary:\n\n;; " _ "\n\n;;; Code:\n\n\n\n(provide '"
        (file-name-sans-extension
         (file-name-nondirectory
          (buffer-file-name)))
        ")\n;;; "
        (file-name-nondirectory
         (buffer-file-name))
        " ends here\n")))
    ("\\.texi\\(nfo\\)?\\'" "Texinfo file skeleton"
     (auto-insert-skeleton
      '("Title: " "\\input texinfo   @c -*-texinfo-*-\n@c %**start of header\n@setfilename "
        (file-name-sans-extension
         (file-name-nondirectory
          (buffer-file-name)))
        ".info\n" "@settitle " str "\n@c %**end of header\n@copying\n"
        (setq short-description
              (read-string "Short description: "))
        ".\n\n" "Copyright @copyright{} "
        (substring
         (current-time-string)
         -4)
        "  "
        (getenv "ORGANIZATION")
        |
        (progn user-full-name)
        "\n\n@quotation\nPermission is granted to copy, distribute and/or modify this document\nunder the terms of the GNU Free Documentation License, Version 1.3\nor any later version published by the Free Software Foundation;\nwith no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.\nA copy of the license is included in the section entitled ``GNU\nFree Documentation License''.\n\nA copy of the license is also available from the Free Software\nFoundation Web site at @url{http://www.gnu.org/licenses/fdl.html}.\n\n@end quotation\n\nThe document was typeset with\n@uref{http://www.texinfo.org/, GNU Texinfo}.\n\n@end copying\n\n@titlepage\n@title " str "\n@subtitle " short-description "\n@author "
        (getenv "ORGANIZATION")
        |
        (progn user-full-name)
        " <"
        (progn user-mail-address)
        ">\n@page\n@vskip 0pt plus 1filll\n@insertcopying\n@end titlepage\n\n@c Output the table of the contents at the beginning.\n@contents\n\n@ifnottex\n@node Top\n@top " str "\n\n@insertcopying\n@end ifnottex\n\n@c Generate the nodes for this menu with `C-c C-u C-m'.\n@menu\n@end menu\n\n@c Update all node entries with `C-c C-u C-n'.\n@c Insert new nodes with `C-c C-c n'.\n@node Chapter One\n@chapter Chapter One\n\n" _ "\n\n@node Copying This Manual\n@appendix Copying This Manual\n\n@menu\n* GNU Free Documentation License::  License for copying this manual.\n@end menu\n\n@c Get fdl.texi from http://www.gnu.org/licenses/fdl.html\n@include fdl.texi\n\n@node Index\n@unnumbered Index\n\n@printindex cp\n\n@bye\n\n@c "
        (file-name-nondirectory
         (buffer-file-name))
        " ends here\n"))))
  "A list specifying text to insert by default into a new file.
Elements look like (CONDITION DESCRIPTION ACTION) or (CONDITION
TEMPLATES) where TEMPLATES is a list of elements of
type (DESCRIPTION ACTION). CONDITION may be a regexp that must
match the new file's name, or it may be a symbol that must match
the major mode for this element to apply. DESCRIPTION is a string
for filling `auto-insert-prompt' and describing the template when
the user has to choose one.

 ACTION may be an absolute
file-name or one relative to `auto-insert-directory' or a form to evaluate."
  :type 'sexp
  :group 'auto-insert)

(defvar auto-insert-local-alist nil
  "Like `auto-insert-alist' but buffer-local.")
(make-variable-buffer-local 'auto-insert-local-alist)

(defvar auto-insert-merge t
  "If non-nil, merge local templates with global ones.")

;; Establish a default value for auto-insert-directory
(defcustom auto-insert-directory (expand-file-name "insert" user-emacs-directory)
  "Directory from which auto-inserted files are taken.
The value must be an absolute directory name;
thus, on a GNU or Unix system, it must end in a slash."
  :type 'directory
  :group 'auto-insert)

(defun auto-insert-filter (candidates)
  "Filter the alist of CANDIDATES according to current major mode
and buffer filename."
  (let (condition filtered-candidates)
    (while candidates
      (setq condition (car (car candidates)))
      (if (if (symbolp condition)
              (eq condition major-mode)
            (and buffer-file-name
                 (string-match condition buffer-file-name)))
          (setq filtered-candidates
                (append filtered-candidates
                        (cond
                         ((stringp (car (cdr (car candidates))))
                          (list (cdr (car candidates))))
                         ((listp (car (cdr (car candidates))))
                          (cdr (car candidates)))))))
      (setq candidates (cdr candidates)))
    filtered-candidates))

;;;###autoload
(defun auto-insert ()
  "Insert default contents into new files if variable `auto-insert' is non-nil.
Matches the visited file name against the elements of
`auto-insert-alist' and `auto-insert-local-alist' if local."
  (interactive)
  (and (not buffer-read-only)
       (or (eq this-command 'auto-insert)
           (and auto-insert
                (and (buffer-file-name)
                     (not (file-exists-p (buffer-file-name))))
                (bobp) (eobp)))
       (let (case-fold-search candidates cand-local cand-global action)
         (goto-char (point-min))

         ;; find global and local templates
         (setq cand-global (auto-insert-filter auto-insert-alist))
         (if (local-variable-p 'auto-insert-local-alist)
             (setq cand-local (auto-insert-filter auto-insert-local-alist)))

         ;; merge them according to `auto-insert-merge'
         (setq candidates
               (if auto-insert-merge
                   (append cand-local cand-global)
                 (or cand-local cand-global)))

         (and candidates
              (if (cdr candidates)
                  (setq desc (completing-read
                              "Template to insert: "
                              (mapcar #'car candidates))
                        action (cdr (assoc desc candidates)))
                (if (or (not auto-insert-query)
                        (if (eq auto-insert-query 'function)
                            (eq this-command 'auto-insert))
                        (y-or-n-p (format auto-insert-prompt desc)))
                    (setq action (cdr (car candidates))
                          desc (car (car candidates)))))

              (if (stringp (car action))
                  (if (file-readable-p
                       (setq action (expand-file-name
                                     (car action)
                                     auto-insert-directory)))
                      (insert-file-contents action))
                (eval (car action))))

         (and (buffer-modified-p)
              (not (eq this-command 'auto-insert))
              (set-buffer-modified-p (eq auto-insert t)))))
  ;; Return nil so that it could be used in
  ;; `find-file-not-found-hooks', though that's probably inadvisable.
  nil)

;; Helper functions
(defun auto-insert-yasnippet-expand (snippet)
  "Expand yasnippet's SNIPPET in current buffer."
  (with-demoted-errors
      (save-window-excursion
        (require 'yasnippet)
        ;; make buffer visible before yasnippet
        ;; which might ask the user for something
        (switch-to-buffer (current-buffer))
        (yas-expand-snippet snippet))))

(defmacro auto-insert-yasnippet (mode key &rest body)
  "Expand to a list of templates constructed from yasnippet in
mode MODE for key KEY."
  (or (mapcar
       (lambda (template)
         (list (car template)
               `(progn
                  (auto-insert-yasnippet-expand ,(yas--template-content (cdr template)))
                  ,@body)))
       (mapcan #'(lambda (table)
                   (yas--fetch table key))
               (let ((major-mode mode))
                 (yas--get-snippet-tables))))
      (error "No snippet with key \"%s\" in mode %s" key mode)))

(defun auto-insert-skeleton (skeleton)
  (save-window-excursion
    ;; make buffer visible before skeleton
    ;; which might ask the user for something
    (switch-to-buffer (current-buffer))
    (skeleton-insert skeleton)))

(defun auto-insert-raw (filename)
  (if (file-readable-p
       (setq filename (expand-file-name
                       filename auto-insert-directory)))
      (insert-file-contents action)))


(defun auto-insert-convert (auto-insert-alist)
  "Convert your old `auto-insert-alist'"
  (mapcar
   (lambda (template)
     (let (desc cond action)
       (setq action (cdr template))
       (if (atom (setq cond (car template)))
           (setq desc (if (stringp cond) cond (symbol-name cond)))
         (setq desc (cdr cond)
               cond (car cond)))
       (setq action
             (flet ((subcond (action)
                             (cond
                              ((stringp action)
                               (list 'auto-insert-raw action))
                              ((consp action)
                               (if (eq (car action) 'lambda)
                                   (if (eq (length action) 3)
                                       (caddr action)
                                     (cons 'progn (cddr action)))
                               `(auto-insert-skeleton (quote ,action))))
                              (t (list action)))))
               (if (vectorp action)
                   (if (eq (length action) 1)
                       (subcond (elt action 0))
                     (cons 'progn
                           (mapcar #'subcond action)))
                 (subcond action))))
       (list cond desc action)))
   auto-insert-alist))


;;;###autoload
(define-minor-mode auto-insert-mode
  "Toggle Auto-insert mode, a global minor mode.
With a prefix argument ARG, enable Auto-insert mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Auto-insert mode is enabled, when new files are created you can
insert a template for the file depending on the mode of the buffer."
  :global t :group 'auto-insert
  (if auto-insert-mode
      (add-hook 'find-file-hook 'auto-insert)
    (remove-hook 'find-file-hook 'auto-insert)))

(provide 'autoinsert)

;;; autoinsert.el ends here
