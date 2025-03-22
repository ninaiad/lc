# lc - file statistics cli tool

#### HSE AMI Haskell course project inspired by https://github.com/XAMPPRocky/tokei

### Usage:
```sh

stack install

# Print file statistics for current directory in tabular format
lc .

# Print individual file statistics
lc ./testdata --files

# Include hidden files or files in hidden directories
lc ./testdata --include-hidden

# Exclude files that contain "py" in their name (Python files for instance)
lc ./testdata --files --exclude py

# Export the data from above in JSON format
lc ./testdata --files --exclude py --export json

# Or Yaml
lc ./testdata --files --exclude py --export yaml
```

```
> lc ./testdata/01

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
 Language   Files Lines Code Comments Blanks
─────────────────────────────────────────────
 C++           14   595  476       18    101
─────────────────────────────────────────────
 C++ Header     2    54   42        0     12
─────────────────────────────────────────────
 Markdown       1    39   34        0      5
─────────────────────────────────────────────
 Total         17   688  552       18    118
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

```
> lc ./testdata/01 --files 

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
 Language                           Files Lines Code Comments Blanks
─────────────────────────────────────────────────────────────────────
 C++                                   14   595  476       18    101
─────────────────────────────────────────────────────────────────────
 ./testdata/01/forward_and_overloads.cpp     59   49        0     10
 ./testdata/01/move_4.cpp                    30   23        0      7
 ./testdata/01/move_2.cpp                    32   28        0      4
 ./testdata/01/move_3.cpp                    52   44        0      8
 ./testdata/01/move_1.cpp                    48   38        2      8
 ./testdata/01/move_0.cpp                    50   40        0     10
 ./testdata/01/smart_pointers_3.cpp          26   23        0      3
 ./testdata/01/smart_pointers_2.cpp          39   31        2      6
 ./testdata/01/lifetime.cpp                  31   23        1      7
 ./testdata/01/smart_pointers_0.cpp          29   24        1      4
 ./testdata/01/smart_pointers_1.cpp          27   22        1      4
 ./testdata/01/smart_pointers_5.cpp          39   32        0      7
 ./testdata/01/smart_pointers_4.cpp          90   62       11     17
 ./testdata/01/smart_pointers_6.cpp          43   37        0      6
─────────────────────────────────────────────────────────────────────
 C++ Header                             2    54   42        0     12
─────────────────────────────────────────────────────────────────────
 ./testdata/01/test_type.h                   27   21        0      6
 ./testdata/01/test_type_rich.h              27   21        0      6
─────────────────────────────────────────────────────────────────────
 Markdown                               1    39   34        0      5
─────────────────────────────────────────────────────────────────────
 ./testdata/01/plan.md                       39   34        0      5
─────────────────────────────────────────────────────────────────────
 Total                                 17   688  552       18    118
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```