;;; test-basic.el --- Basic smoke tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'ascii-tree-export)

(ert-deftest test-build-prefix-empty ()
  "Empty spine produces empty prefix."
  (should (equal "" (ascii-tree-export--build-prefix '()))))

(ert-deftest test-build-prefix-connector ()
  "Non-last child gets connector."
  (should (equal "├── " (ascii-tree-export--build-prefix '(nil)))))

(ert-deftest test-multiline-roundtrip ()
  "Multi-line content is preserved."
  (let* ((input "root/\n├── file # test\n│   # comment1\n│   # comment2\n│\n└── end")
         (lines (split-string input "\n"))
         (parsed (ascii-tree-import--parse-lines lines))
         (org-output (ascii-tree-import--emit-org parsed)))
    (with-temp-buffer
      (insert org-output)
      (org-mode)
      (let* ((tree (org-element-parse-buffer))
             (out-buf (get-buffer-create " *test*"))
             (src-buf (current-buffer)))
        (with-current-buffer out-buf (erase-buffer))
        (ascii-tree-export--walk tree '() out-buf src-buf)
        (let* ((result (with-current-buffer out-buf (buffer-string)))
               (norm-in (mapcar #'string-trim-right (split-string input "\n" t)))
               (norm-out (mapcar #'string-trim-right (split-string (string-trim result) "\n" t))))
          (should (= (length norm-in) (length norm-out)))
          (should (string-match-p "comment1" result))
          (should (string-match-p "comment2" result)))
        (kill-buffer out-buf)))))

;;; test-basic.el ends here
