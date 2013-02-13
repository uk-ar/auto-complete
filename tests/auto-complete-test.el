(require 'ert)

(require 'auto-complete)
(require 'auto-complete-config)

;; Move this into test case or setup macro once we start testing with
;; non-default config.
(ac-config-default)

(defun ac-test-helper-action ()
  (incf counter))

(defmacro ac-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (let ((counter 0))
       (with-temp-buffer
         (switch-to-buffer (current-buffer))
         (switch-to-buffer (current-buffer))
         (delete-other-windows)
         (erase-buffer)
         (auto-complete-mode 1)
         (emacs-lisp-mode)
         (unwind-protect
             (progn ,@body)
           (when ac-menu
             (ac-menu-delete)))
         ))))

(defun ac-test-helper-popup-exist-p ()
      (and (popup-live-p ac-menu)
          ac-menu))

(defun ac-test-helper-popup-item ()
  (and (popup-live-p ac-menu)
       (popup-list ac-menu)))

(ert-deftest ac-test-simple-invocation ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "Ba")
      (auto-complete)
      (should (equal (ac-test-helper-popup-item) '("Bar" "Baz")))
      (should (equal (ac-selected-candidate) "Bar"))
      (execute-kbd-macro (kbd "RET"))
      (should-not (ac-test-helper-popup-exist-p))
      (should (string= (buffer-string) "Bar"))
      )))

(ert-deftest ac-test-execute-action ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Action1" "Action2")
             (action . ac-test-helper-action)))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "Act")
      (auto-complete)
      (should (equal (ac-test-helper-popup-item) '("Action1" "Action2")))
      (should (equal (ac-selected-candidate) "Action1"))
      (execute-kbd-macro (kbd "RET"))
      (should (eq counter 1))
      (should-not (ac-test-helper-popup-exist-p))
      (should (string= (buffer-string) "Action1"))
      )))

(ert-deftest ac-test-select-next ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Bar1" "Bar2" "Bar3")))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "Ba")
      (auto-complete)
      (should (equal (ac-test-helper-popup-item) '("Bar1" "Bar2" "Bar3")))
      (should (equal (ac-selected-candidate) "Bar1"))
      (execute-kbd-macro (kbd "TAB"))
      (should (equal (ac-selected-candidate) "Bar1"))
      (execute-kbd-macro (kbd "TAB"))
      (should (equal (ac-selected-candidate) "Bar2"))
      (execute-kbd-macro (kbd "TAB"))
      (should (equal (ac-selected-candidate) "Bar3"))
      (execute-kbd-macro (kbd "TAB"))
      (should (equal (ac-selected-candidate) "Bar1"))
      )))

(ert-deftest ac-test-single-candidate ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "FooB")
      (auto-complete)
      (should-not (ac-test-helper-popup-exist-p))
      (should (string= (buffer-string) "FooBar"))
      )))

(ert-deftest ac-test-complete-common-part ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar")))
          (ac-sources '(ac-source-test)))
      (execute-kbd-macro "Fo")
      (auto-complete)
      (ac-stop)
      (should (string= (buffer-string) "Foo"))
      )))

(ert-deftest ac-test-complete-common-part2 ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "FooQux" "FooBar")))
          (ac-sources '(ac-source-test)))
      (execute-kbd-macro "Fo")
      (auto-complete)
      (ac-stop)
      (should (string= (buffer-string) "Foo"))
      )))

(ert-deftest ac-test-simple-update ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "Foo")
      (auto-complete)
      (execute-kbd-macro "B")
      (ac-update-greedy)
      (should (equal (ac-test-helper-popup-item) '("FooBar")))
      )))

(ert-deftest ac-test-update-no-candidate ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "Foo")
      (auto-complete)
      (execute-kbd-macro "A")
      (ac-update-greedy)
      (should-not (ac-test-helper-popup-exist-p))
      (should (string= (buffer-string) "FooA"))
      )))

(ert-deftest ac-test-update-when-delete-char ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "FooBaz" "FooBar")))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "FooA")
      (auto-complete)
      (delete-backward-char 1)
      (ac-update-greedy)
      ;; Pending?
      ;; (should (equal (ac-test-helper-popup-item) '("FooBaz" "FooBar")))
      )))

(ert-deftest ac-test-update-when-delete-char2 ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
          (ac-sources '(ac-source-test)))
      (should-not (ac-test-helper-popup-exist-p))
      (insert "FooA")
      (auto-complete)
      (delete-backward-char 1)
      (ac-update-greedy)
      ;; Pending?
      ;; (should (equal (ac-test-helper-popup-item) '("Foo" "FooBar")))
      )))

(ert-deftest ac-test-simple-invocation-buffer-string ()
  (ac-test-with-common-setup
    (let ((ac-sources '(ac-source-words-in-buffer)))
      (insert "Bar Baz\n")
      (should-not (ac-test-helper-popup-exist-p))
      (insert "Ba")
      (auto-complete)
      (should (equal (ac-test-helper-popup-item) '("Bar" "Baz")))
      (should (equal (ac-selected-candidate) "Bar"))
      (execute-kbd-macro (kbd "RET"))
      (should-not (ac-test-helper-popup-exist-p))
      (should (string= (buffer-substring (line-beginning-position)
                                         (line-end-position)) "Bar"))
      )))

(ert-deftest ac-test-complete-common-part-when-buffer-undo-list-is-t ()
  (ac-test-with-common-setup
   (let ((ac-source-test
          '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
         (ac-sources '(ac-source-test)))
     (execute-kbd-macro "Fo")
     (let ((last-command this-command)
           (buffer-undo-list t))
       (auto-complete))
     (ac-stop)
     (should (string= (buffer-string) "Foo"))
     )))

