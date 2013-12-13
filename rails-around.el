;;;; projectile-rails.el --- Add more rails specfific features to projectile

;; Copyright © 2013 Ian Goodrich

;; Author: Ian Goodrich
;; URL: https://github.com/igoodrich/projectile-rails
;; Version: 0.1.0
;; Keywords: project, rails
;; Package-Requires: ((s "1.4.0") (dash "1.2.0") (projectile "0.9.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package uses projectile to add more rails specific features, specifically for navigation
;; then comes with projectile out of the box.  Several ideas are stolen from rails.vim.
;;
;;; Code:

(require 'projectile)
(require 'fiplr)
(require 's)
(require 'dash)

(defun ra/project-root ()
  (projectile-project-root))

(defun ra/all-project-files ()
  (projectile-current-project-files))

(defun ra/find-file (prompt file-func)
  (let ((file (projectile-completing-read prompt
                                          (funcall file-func))))
    (find-file (expand-file-name file (ra/project-root)))))

;; (defun ra/project-root ()
;;   (fiplr-root))

;; (defun ra/all-project-files ()
;;   (projectile-current-project-files))

;; (defun ra/find-file (prompt file-func)
;;   (let ((file (projectile-completing-read prompt
;;                                           (funcall file-func))))
;;     (find-file (expand-file-name file (ra/project-root)))))

(defun ra/buffer-file-name-or-empty () (or buffer-file-name ""))

(defun ra/true-project-root () (file-truename (ra/project-root)))

(defun ra/buffer-file-name-relative-to-project ()
  (s-chop-prefix (ra/true-project-root) (ra/buffer-file-name-or-empty)))

(defun ra/buffer-file-name-relative-to-app ()
  (s-chop-prefix (concat (ra/true-project-root) "app/") (ra/buffer-file-name-or-empty)))

(defun ra/buffer-file-name-relative-to-unit-test ()
  (s-chop-prefix (concat (ra/true-project-root) "test/unit") (ra/buffer-file-name-or-empty)))

(defun ra/buffer-file-name-relative-to-functional-test ()
  (s-chop-prefix (concat (ra/true-project-root) "test/functional") (ra/buffer-file-name-or-empty)))

(defun ra/buffer-rails-model? ()
  (s-starts-with? "app/models" (ra/buffer-file-name-relative-to-project)))

(defun ra/buffer-rails-controller? ()
  (s-starts-with? "app/controller" (ra/buffer-file-name-relative-to-project)))

(defun ra/buffer-rails-view? ()
  (s-starts-with? "app/views" (ra/buffer-file-name-relative-to-project)))

(defun ra/buffer-rails-test? ()
  (s-starts-with? "test/" (ra/buffer-file-name-relative-to-project)))

(defun ra/buffer-rails-unit-test? ()
  (s-starts-with? "test/unit" (ra/buffer-file-name-relative-to-project)))

(defun ra/buffer-rails-functional-test? ()
  (s-starts-with? "test/functional" (ra/buffer-file-name-relative-to-project)))

(defun ra/buffer-rails-migration? ()
  (s-starts-with? "db/migrate" (ra/buffer-file-name-relative-to-project)))

(defun ra/buffer-rails-spec? ()
  (s-starts-with? "spec/" (ra/buffer-file-name-relative-to-project)))

(defun ra/test-file-from-file-name (fname)
  (concat (s-chop-suffix ".rb" fname) "_test.rb"))

(defun ra/spec-file-from-file-name (fname)
  (concat (s-chop-suffix ".rb" fname) "_spec.rb"))

(defun ra/base-file-from-test-file-name (fname)
  (s-replace "_test.rb" ".rb" fname))

(defun ra/test-file-from-model ()
  (ra/test-file-from-file-name
   (concat
    (ra/project-root)
    (s-replace "models" "test/unit" (ra/buffer-file-name-relative-to-app)))))

(defun ra/test-file-from-app-file ()
  "Given an arbirary app file, assume a corresponding relative path in test/unit"
  (concat
   (ra/project-root)
   "test/unit/"
   (ra/test-file-from-file-name (ra/buffer-file-name-relative-to-app))))

(defun ra/app-file-in-dir-from-unit-test-file (dir-from-root)
  (concat
   (ra/project-root)
   dir-from-root
   (ra/base-file-from-test-file-name (ra/buffer-file-name-relative-to-unit-test))))

(defun ra/app-file-from-unit-test-file ()
  (let ((model-file (ra/app-file-in-dir-from-unit-test-file "app/models"))
        (other-file (ra/app-file-in-dir-from-unit-test-file "app")))
    (cond ((file-exists-p model-file) model-file)
          ('t other-file))))

(defun ra/app-file-from-functional-test-file ()
  (concat
   (ra/project-root)
   "app/controllers"
   (ra/base-file-from-test-file-name (ra/buffer-file-name-relative-to-functional-test))))

(defun ra/test-file-from-controller ()
  (ra/test-file-from-file-name
   (concat
    (ra/project-root)
    (s-replace "controllers" "test/functional" (ra/buffer-file-name-relative-to-app)))))

(defun ra/test-file-from-view ()
  (ra/test-file-from-view-file (ra/project-root) (ra/buffer-file-name-relative-to-project)))

(defun ra/test-file-from-view-file (root view-file-name)
  (concat root
          "test/functional/"
          (s-join "/" (butlast (s-split "/" (s-chop-prefix  "app/views/" view-file-name))))
          "_controller_test.rb"))

(defun ra/alternate-file-name ()
  (cond ((ra/buffer-rails-model?) (ra/test-file-from-model))
        ((ra/buffer-rails-controller?) (ra/test-file-from-controller))
        ((ra/buffer-rails-unit-test?) (ra/app-file-from-unit-test-file))
        ((ra/buffer-rails-functional-test?) (ra/app-file-from-functional-test-file))
        ((ra/buffer-rails-view?) (ra/test-file-from-view))
        ((ra/buffer-rails-migration?) (ra/prev-file))
        ('t (ra/test-file-from-app-file))))

;; will want name and location in some cases
;; model -> right location in schema file
;; controller -> right view based on method in controller
(defun ra/related-file-name ()
  (cond ((ra/buffer-rails-model?) (ra/test-file-from-model))
        ((ra/buffer-rails-controller?) (ra/test-file-from-controller))
        ((ra/buffer-rails-unit-test?) (ra/app-file-from-unit-test-file))
        ((ra/buffer-rails-functional-test?) (ra/app-file-from-functional-test-file))
        ((ra/buffer-rails-view?) (ra/test-file-from-view))
        ((ra/buffer-rails-migration?) (ra/prev-file))
        ('t (ra/test-file-from-app-file))))

(defun ra/migrations-dir ()
  (file-name-as-directory (concat (ra/project-root) "db/migrate")))

(defun ra/migrations ()
  (directory-files (ra/migrations-dir)))

(defun ra/last-migration ()
  (car (reverse (ra/migrations))))

;; exported
(defun ra/find-last-migration ()
  (interactive)
  (find-file (concat (ra/migrations-dir) (ra/last-migration))))

(defun ra/prev-file ()
  (let* ((current-dir (file-name-directory (buffer-file-name)))
         (files (-reject (lambda (file) (file-directory-p file)) (directory-files current-dir)))
         (prev-file (ra/file-before files (buffer-name))))
    (cond (prev-file (concat current-dir prev-file))
          ('t nil))))

(defun ra/next-file ()
  (let* ((current-dir (file-name-directory (buffer-file-name)))
         (files (reverse
                 (-reject (lambda (file)
                            (file-directory-p file)) (directory-files current-dir))))
         (next-file (ra/file-before files (buffer-name))))
    (cond (next-file (concat current-dir next-file))
          ('t nil))))

(defun ra/file-before (files matching-file)
  (car (last (-take-while (lambda (file) (not (equal file matching-file))) files))))

(defun ra/current-project-model-files ()
  (-select (lambda (file) (s-starts-with? "app/models" file)) (ra/all-project-files)))

(defun ra/current-project-controller-files ()
  (-select (lambda (file) (s-starts-with? "app/controllers" file)) (ra/all-project-files)))

(defun ra/current-project-view-files ()
  (-select (lambda (file) (s-starts-with? "app/views" file)) (ra/all-project-files)))

(defun ra/current-project-js-files ()
  (-select (lambda (file) (or
                           (s-ends-with? ".js" file)
                           (s-ends-with? ".coffee" file)))
             (ra/all-project-files)))

(defun ra/current-project-style-files ()
  (-select (lambda (file) (or
                           (s-ends-with? ".css" file)
                           (s-ends-with? ".scss" file)
                           (s-ends-with? ".sass" file)
                           (s-ends-with? ".less" file))) (ra/all-project-files)))

(defun ra/current-project-test-files ()
  (-select (lambda (file) (s-starts-with? "test" file)) (ra/all-project-files)))

(defun ra/current-project-lib-files ()
  (-select (lambda (file) (s-starts-with? "lib" file)) (ra/all-project-files)))

(defun ra/current-project-factory-files ()
  (-select (lambda (file) (s-starts-with? "test/factories" file)) (ra/all-project-files)))

(defun ra/current-project-spec-files ()
  (-select (lambda (file) (s-starts-with? "spec" file)) (ra/all-project-files)))

(defun ra/current-project-feature-files ()
  (-select (lambda (file) (s-ends-with? ".feature" file)) (ra/all-project-files)))

;; exported
(defun ra/find-prev-file ()
  (interactive)
  (let ((prev-file (ra/prev-file)))
    (cond (prev-file (find-file prev-file))
          ('t (message "No previous file")))))

;; exported
(defun ra/find-next-file ()
  (interactive)
  (let ((next-file (ra/next-file)))
    (cond (next-file (find-file next-file))
          ('t (message "No next file")))))

;; exported
(defun ra/find-alternate-file ()
  (interactive)
  (let ((f (ra/alternate-file-name)))
    (cond ((file-exists-p f)
           (if arg (find-file-other-window f)
                   (find-file f)))
          ('t (message "No alternate file exists")))))

;; exported
(defun ra/find-alternate-file-other-window ()
  (interactive)
  (let ((f (ra/alternate-file-name)))
    (cond ((file-exists-p f)
           (find-file-other-window f)
          ('t (message "No alternate file exists"))))))

;; exported
(defun ra/find-model ()
  (interactive)
  (ra/find-file "Find model: " 'ra/current-project-model-files))

;; exported
(defun ra/find-controller ()
  (interactive)
  (ra/find-file "Find controller: " 'ra/current-project-controller-files))

;; exported
(defun ra/find-view ()
  (interactive)
  (ra/find-file "Find view: " 'ra/current-project-view-files))

;; exported
(defun ra/find-test ()
  (interactive)
  (ra/find-file "Find test: " 'ra/current-project-test-files))

;; exported
(defun ra/find-lib ()
  (interactive)
  (ra/find-file "Find in lib: " 'ra/current-project-lib-files))

;; exported
(defun ra/find-factory ()
  (interactive)
  (ra/find-file "Find factory: " 'ra/current-project-factory-files))

;; exported
(defun ra/find-feature ()
  (interactive)
  (ra/find-file "Find feature: " 'ra/current-project-feature-files))

;; exported
(defun ra/find-spec ()
  (interactive)
  (ra/find-file "Find spec: " 'ra/current-project-spec-files))

;; exported
(defun ra/find-js ()
  (interactive)
  (ra/find-file "Find js: " 'ra/current-project-js-files))

;; exported
(defun ra/find-style ()
  (interactive)
  (ra/find-file "Find style: " 'ra/current-project-style-files))

;; exported
(defun ra/find-gemfile ()
  "Find a rails stylesheet in the current project"
  (interactive)
  (find-file (concat (ra/project-root) "Gemfile")))

;; exported
(defun ra/find-routes-file ()
  "Find a rails stylesheet in the current project"
  (interactive)
  (find-file (concat (ra/project-root) "config/routes.rb")))

;; exported
(defun ra/find-schema-file ()
  "Find a rails stylesheet in the current project"
  (interactive)
  (find-file (concat (ra/project-root) "db/schema.rb")))

(provide 'rails-around)

;; projectile-rails.el ends here
