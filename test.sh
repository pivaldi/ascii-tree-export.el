#!/bin/bash
# test.sh - Test suite for ascii-tree-export

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Track test results
TESTS_PASSED=0
TESTS_FAILED=0

print_header() {
    echo -e "\n${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}\n"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

print_failure() {
    echo -e "${RED}✗${NC} $1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

print_info() {
    echo -e "${YELLOW}ℹ${NC} $1"
}

# Test 1: ERT Unit Tests
print_header "Running ERT Unit Tests"
if emacs --batch -L . -l ert -l ascii-tree-export.el -l tests/test-basic.el \
    -f ert-run-tests-batch-and-exit 2>&1 | tee /tmp/test-ert.log | grep -q "results as expected"; then
    PASSED=$(grep "Ran.*tests" /tmp/test-ert.log | grep -o '[0-9]\+' | head -1)
    print_success "All $PASSED ERT unit tests passed"
else
    print_failure "ERT unit tests failed"
    cat /tmp/test-ert.log
fi

# Test 2: Roundtrip Test (mock.txt)
print_header "Running Roundtrip Test (mock.txt)"
if [ ! -f tests/mock.txt ]; then
    print_info "Skipping: tests/mock.txt not found"
else
    if emacs --batch -L . -l ert -l ascii-tree-export.el -l tests/test-roundtrip-mock.el \
        -f ert-run-tests-batch-and-exit 2>&1 | tee /tmp/test-roundtrip.log; then
        # Extract statistics from output
        if grep -q "Structural differences: 0" /tmp/test-roundtrip.log; then
            ALIGNMENT=$(grep "Alignment differences:" /tmp/test-roundtrip.log | grep -o '[0-9]\+' | head -1)
            print_success "Roundtrip test passed (structure perfect, $ALIGNMENT alignment diffs)"
        else
            print_success "Roundtrip test passed"
        fi
    else
        print_failure "Roundtrip test failed"
        echo "See /tmp/test-roundtrip.log for details"
    fi
fi

# Test 3: Tree Command Compatibility
print_header "Testing Tree Command Compatibility"
if ! command -v tree &> /dev/null; then
    print_info "Skipping: 'tree' command not found"
else
    # Create a simple test directory structure
    TEST_DIR=$(mktemp -d)
    mkdir -p "$TEST_DIR/project/src"
    touch "$TEST_DIR/project/README.md"
    touch "$TEST_DIR/project/src/main.js"

    # Generate tree output (remove blank line and summary)
    TREE_OUTPUT=$(cd "$TEST_DIR" && tree project 2>/dev/null | sed '$d' | sed '$d')

    # Save to file for Emacs to read
    echo "$TREE_OUTPUT" > /tmp/tree-output.txt

    # Test roundtrip with inline Emacs code
    if emacs --batch -L . -l ascii-tree-export.el --eval "
(let* ((input (with-temp-buffer
               (insert-file-contents \"/tmp/tree-output.txt\")
               (buffer-string)))
       (lines (split-string input \"\\n\"))
       (parsed (ascii-tree-import--parse-lines lines))
       (org-output (ascii-tree-import--emit-org parsed)))
  (with-temp-buffer
    (insert org-output)
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
           (out-buf (get-buffer-create \" *test*\"))
           (src-buf (current-buffer)))
      (with-current-buffer out-buf (erase-buffer))
      (ascii-tree-export--walk tree '() out-buf src-buf)
      (let* ((result (with-current-buffer out-buf (buffer-string)))
             (input-lines (mapcar #'string-trim-right (split-string input \"\\n\" t)))
             (result-lines (mapcar #'string-trim-right (split-string (string-trim result) \"\\n\" t))))
        (if (equal input-lines result-lines)
            (kill-emacs 0)
          (kill-emacs 1)))
      (kill-buffer out-buf))))" 2>/dev/null; then
        print_success "Tree command output roundtrip successful"
    else
        print_failure "Tree command output roundtrip failed (check if 'tree' command available)"
    fi

    # Cleanup
    rm -rf "$TEST_DIR"
    rm -f /tmp/tree-output.txt
fi

# Test 4: Empty Line Preservation
print_header "Testing Empty Line Preservation"
cat > /tmp/test-empty-lines.el <<'EOF'
(add-to-list 'load-path ".")
(require 'ascii-tree-export)

(let* ((input "root/
├── file1
│   # comment
│
└── file2")
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
             (input-lines (split-string input "\n" t))
             (result-lines (split-string (string-trim result) "\n" t)))
        (if (and (= (length input-lines) (length result-lines))
                 (string-match-p "│" result))
            (progn
              (princ "PASS\n")
              (kill-emacs 0))
          (progn
            (princ "FAIL\n")
            (princ (format "Input: %d lines, Result: %d lines\n"
                          (length input-lines) (length result-lines)))
            (princ result)
            (kill-emacs 1))))
      (kill-buffer out-buf))))
EOF

if emacs --batch -l /tmp/test-empty-lines.el 2>&1 | grep -q "PASS"; then
    print_success "Empty line preservation works correctly"
else
    print_failure "Empty line preservation failed"
fi
rm -f /tmp/test-empty-lines.el

# Test 5: Multi-line Content Preservation
print_header "Testing Multi-line Content (Fixed-width & Comments)"
cat > /tmp/test-multiline.el <<'EOF'
(add-to-list 'load-path ".")
(require 'ascii-tree-export)

(with-temp-buffer
  (org-mode)
  (insert "* Test\n")
  (insert "    : line1\n")
  (insert "    : line2\n")
  (insert "    : line3\n")
  (insert "# comment1\n")
  (insert "# comment2\n")

  (let* ((tree (org-element-parse-buffer))
         (out-buf (get-buffer-create " *test*"))
         (src-buf (current-buffer)))
    (with-current-buffer out-buf (erase-buffer))
    (ascii-tree-export--walk tree '() out-buf src-buf)

    (let ((result (with-current-buffer out-buf (buffer-string))))
      (if (and (string-match-p "line1" result)
               (string-match-p "line2" result)
               (string-match-p "line3" result)
               (string-match-p "comment1" result)
               (string-match-p "comment2" result))
          (progn
            (princ "PASS\n")
            (kill-emacs 0))
        (progn
          (princ "FAIL\n")
          (princ result)
          (kill-emacs 1))))
    (kill-buffer out-buf)))
EOF

if emacs --batch -l /tmp/test-multiline.el 2>&1 | grep -q "PASS"; then
    print_success "Multi-line content preserved correctly"
else
    print_failure "Multi-line content preservation failed"
fi
rm -f /tmp/test-multiline.el

# Test Summary
print_header "Test Summary"
TOTAL_TESTS=$((TESTS_PASSED + TESTS_FAILED))
echo -e "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✓ All tests passed!${NC}\n"
    exit 0
else
    echo -e "\n${RED}✗ Some tests failed${NC}\n"
    exit 1
fi
