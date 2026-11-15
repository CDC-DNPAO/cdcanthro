# cdcanthro: standardized growth metrics from the CDC and WHO growth charts

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/cdcanthro)](https://CRAN.R-project.org/package=cdcanthro)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

`cdcanthro` calculates sex- and age-standardized z-scores, percentiles and
related metrics for weight, height (or length), BMI and head circumference
in children, using the LMS (lambda-mu-sigma) method and the reference
tables of the CDC Growth Charts and the WHO Child Growth Standards.

Three functions:

| Function      | Reference                       | Ages                   | Main outputs |
|---------------|---------------------------------|------------------------|--------------|
| `cdc_z()`     | 2000 CDC Growth Charts          | 24 to < 240 months     | `waz`, `haz`, `bmiz`, `bmip`, extended BMI z-scores and percentiles for BMI >= 95th percentile, `bmip95`, modified z-scores |
| `who_z()`     | WHO Child Growth Standards 2006 | 0 to 1826 days (5 y)   | `who_waz`, `who_lhaz`, `who_bmiz`, `who_wflz`, `who_headcz`, plus biologically-implausible-value flags |
| `gradual_z()` | Daymont et al. 2025             | 2 to 5 years           | `grad_waz`, `grad_haz`, `grad_bmiz`: a weighted average of the WHO and CDC z-scores that removes the jump at age 2 |

CDC recommends the WHO standards under 24 months and the CDC charts from
24 months. Switching references on a child's second birthday creates a
discontinuity -- a girl weighing 9.9 kg is at the WHO 10th percentile the
day before her second birthday and at the CDC 2nd percentile the day
after. `gradual_z()` blends the two z-scores linearly between 2 and 5
years so the transition is continuous.

`cdc_z()` reproduces the output of the CDC SAS program, including the
extended BMI metrics of Wei et al. (2020) for children with obesity, and is
checked against SAS output in the package tests. The functions are written
with `data.table` and are vectorized: call them once on a whole dataset,
not once per child.

## Installation

From CRAN:

```r
install.packages("cdcanthro")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("CDC-DNPAO/cdcanthro")
```

## Usage

The input is a `data.frame`, `data.table` or tibble with a column named
`sex` (coded 1/2, M/F, boy/girl, male/female -- case-insensitive; other
numeric codings via `sex_code`) and whatever measurements are available.
Column names can be given quoted or unquoted. The same class of object is
returned, with the new columns added.

```r
library(cdcanthro)
library(data.table)

# CDC charts, ages 24 to < 240 months. NHanes is an example dataset in
# the package; its ages are completed months, so 0.5 is added (with a
# warning) to place each child at the midpoint of the month.
d <- copy(NHanes)
out <- cdc_z(d, age = agemos, wt = wt, ht = ht, bmi = bmi)
out[, .(seqn, sex, agemos, waz, haz, bmiz, bmip, bmip95)]

# WHO standards, ages 0 to 1826 days. lenhei is recumbent length through
# 730 days and standing height after that.
d[, lenhei := fifelse(agedays <= 730, length, ht)]
young <- who_z(d[agedays <= 1826], agedays = agedays, wt = wt,
               lenhei = lenhei, headc = headc)
young[, .(seqn, agedays, who_waz, who_lhaz, who_bmiz, who_wflz, fwaz)]

# Gradual WHO-to-CDC transition for 2- to 5-year-olds. After who_z() and
# cdc_z() have both been run on the same data, the default column names
# line up and no arguments other than the data are needed.
both <- cdc_z(young, age = agemos, wt = wt, ht = ht)
gradual_z(both)[, .(agedays, who_waz, waz, grad_waz)]
```

Notes:

* Weight in kg, height/length in cm, BMI in kg/m^2, CDC age in months,
  WHO age in days.
* Zero and negative measurements are treated as missing.
* Children outside a reference's age range are kept in the output with
  `NA` for the computed metrics.
* `cdc_z()`'s `bmiz` and `bmip` are the extended values above the 95th
  percentile; the LMS-only values are returned as `orig_bmiz` and
  `orig_bmip`.

See `?cdc_z`, `?who_z` and `?gradual_z` for the full list of output
variables and the formulas used, and [NEWS.md](NEWS.md) for changes by
version.

## References

* Kuczmarski RJ, Ogden CL, Guo SS, et al. 2000 CDC Growth Charts for the
  United States: methods and development. *Vital Health Stat* 2002;11(246):1-190.
* Wei R, Ogden CL, Parsons VL, Freedman DS, Hales CM. A method for
  calculating BMI z-scores and percentiles above the 95th percentile of the
  CDC growth charts. *Ann Hum Biol* 2020;47(6):514-521.
  doi:10.1080/03014460.2020.1808065
* Freedman DS, Woo JG, Ogden CL, Xu JH, Cole TJ. Distance and percent
  distance from median BMI as alternatives to BMI z-score. *Br J Nutr*
  2020;124(5):493-500. doi:10.1017/S0007114519002046
* World Health Organization. WHO Child Growth Standards: methods and
  development. Geneva: WHO; 2006.
* Daymont C, Hwang W, Paul IM, Shur N, Freedman DS. Creation and
  evaluation of new growth charts with a gradual transition from WHO to CDC
  values. *Pediatrics* 2025;156(3):e2025070697. doi:10.1542/peds.2025-070697
* CDC SAS program for the growth charts:
  https://www.cdc.gov/growth-chart-training/hcp/computer-programs/sas.html

## Citation

```r
citation("cdcanthro")
```

## Bug reports

Please use the [issue tracker](https://github.com/CDC-DNPAO/cdcanthro/issues).
