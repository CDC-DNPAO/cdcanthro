## Update

This is a substantial update. Changes:
* Added two new functions: who_z() and gradual_z().  The former calculates standardized
  metrics based on the WHO standards, and the latter produces a weighted average of CDC and WHO z-score values for children between 2.0 and 5.0 years.
* Added a sex_code argument to cdc_z and gradual_z that may be helpul to people who code sex as 0/1.
* Added logarithmic form of the z-score calculation when |L| < 0.01, to
  match the CDC SAS implementation for weight and height. BMI_L never approaches 0.
* Fixed documentation formatting for DOIs in references.
* If input data has ages outside the reference range [24,240 months), cdc_z function
  warns about this and gives the N. These rows are kept in the output.
* Both cdc_z and who_z check for possible mistakes in the supplied function arguments

## Test environments
* local macOS, R 4.6.0
* win-builder (devtools::check_win_devel())

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
None
