# Testing ascii-tree-export

## Quick Start

Run all tests:
```bash
./test.sh
```

## Test Suite

The test suite includes:

### 1. **ERT Unit Tests** (`tests/test-basic.el`)
- Basic functionality tests
- Prefix building
- Multi-line content preservation
- ~3 tests

### 2. **Roundtrip Test** (`tests/test-roundtrip-mock.el`)
- Full import → org → export cycle
- Tests with `tests/mock.txt` (309 lines)
- Separates structural vs cosmetic differences
- **Pass criteria**: 0 structural differences (alignment spaces OK)

### 3. **Tree Command Compatibility**
- Tests real `tree` command output
- Creates temp directory structure
- Verifies perfect roundtrip with actual `tree` output
- **Requires**: `tree` command installed

### 4. **Empty Line Preservation**
- Tests that `│` spacing lines are preserved
- Critical for maintaining visual structure

### 5. **Multi-line Content**
- Tests fixed-width blocks (`:` prefix)
- Tests comment blocks (`#` prefix)
- Ensures consecutive lines are not lost

## Test Output

```
========================================
Test Summary
========================================
Total tests: 5
Passed: 5
Failed: 0

✓ All tests passed!
```

## Running Individual Tests

### ERT tests only:
```bash
emacs --batch -L . -l ert -l ascii-tree-export.el \
  -l tests/test-basic.el -f ert-run-tests-batch-and-exit
```

### Roundtrip test only:
```bash
emacs --batch -L . -l ert -l ascii-tree-export.el \
  -l tests/test-roundtrip-mock.el -f ert-run-tests-batch-and-exit
```

## CI/CD Integration

The test script exits with:
- **0** if all tests pass
- **1** if any test fails

Perfect for continuous integration:
```yaml
# .github/workflows/test.yml
- name: Run tests
  run: ./test.sh
```

## Test Coverage

✓ Empty line preservation (fixed bug)
✓ Multi-line content (fixed bug)
✓ Tree command compatibility
✓ Content tree prefixes (`│`)
✓ Roundtrip conversions (309/309 lines)

## Known Limitations

- **Alignment spaces**: The package does not preserve cosmetic alignment spaces before `#` comments
  - Input: `├── file.txt      # comment`
  - Output: `├── file.txt # comment`
  - This is cosmetic only and does not affect tree structure
