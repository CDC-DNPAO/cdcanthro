## Update

* The SAS validation fixture was reduced from 100,000 to 10,000 children,
  cutting the package size from 8.3 MB to 1.2 MB.
* Unrecognized sex codes now produce a warning that reports how many rows were
  affected and which values were seen; those rows get NA as before. Text codes
  are matched as whole words (m, male, b, boy, f, female, g, girl, and their
  plurals, in any case) rather than by first letter, so a value such as "Masc"
  is now NA with a warning instead of being read as male.
* who_z() warns when agedays is negative; those rows get NA as before.
* cdc_z(): the log form of the z-score used when |L| < 0.01 (CDC SAS
  convention) now applies to height only. L for weight is never within 0.19 of
  zero in the CDC tables, so no weight z-score changes.
* The reference-table lookup in cdc_z() and who_z() is now done by index
  rather than a join, which uses less memory on large data. No computed value
  changes; the regression and SAS-comparison tests are unchanged.
* Documentation: the help pages for cdc_z(), who_z() and gradual_z() have been
  rewritten with new examples; all_vars outputs, the WHO adjustment beyond
  +/-3 SD, and the modified z-scores are now described; NHanes and
  who_ref_data document every column; a README was added. The Hales et al.
  (2022) NCHS report on extended BMI metrics is cited alongside Wei et al.
* The package source is now on GitHub (https://github.com/CDC-DNPAO/cdcanthro);
  please report problems at https://github.com/CDC-DNPAO/cdcanthro/issues.


## Test environments
* local macOS, R 4.6.2
* win-builder (devtools::check_win_devel())

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
None
