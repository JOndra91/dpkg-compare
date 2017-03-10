# dpkg-compare

Utility for comparing output of 'dpkg --list' command.
For alignment of output into columns, pipe output into 'column -t' command.

## Usage

```
dpkg-compare {[-r|--reference] REFERENCE_FILE} {[-o|--other] OTHER_FILE}
             [--only-extra|--only-missing|--only-mismatch] [--only-installed]
dpkg-compare {-h|--help}

  -h                 --help                      Print this help.
  -r REFERENCE_FILE  --reference=REFERENCE_FILE  Path to file that's result of 'dpkg --list' command.
  -o OTHER_FILE      --other=OTHER_FILE          Path to file that's result of 'dpkg --list' command.
                     --only-extra                Print only packages found in REFERENCE_FILE but not in OTHER_FILE.
                     --only-missing              Print only packages found in OTHER_FILE but not in REFERENCE_FILE.
                     --only-mismatch             Print only packages found in both files but not with same version/architecture/status.
                     --only-installed            Print only packages with 'installed' status.
```
