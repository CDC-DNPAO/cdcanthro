# who_z(): WHO growth-standard z-scores for young children (0 to 5 y, 1826 days)
#
# Computes WHO Child Growth Standard z-scores, plus biologically-implausible-
# value (BIV) flags, for weight-for-age, length/height-for-age, BMI-for-age,
# head-circumference-for-age, and weight-for-length.
#
# lenhei is recumbent length up to age 2.0 y (<= 730 d) and standing height
# after that (>= 731 d), so there is a deliberate jump in M between 730 and
# 731 days. Weight-for-length is computed only for ages <= 730 d and lengths
# 45 to 110 cm.
#
# Units: wt in kg, lenhei and headc in cm, age in days. Accepted sex codes
# are described at normalize_sex() in R/utils-anthro.R.
#
# 'BIV' flags (match the CRAN anthro package): f* = 1 if the z-score is outside the
# WHO plausibility range, 0 if inside, NA if the z-score is NA.
#   fwaz    : -6 <= waz    <= 5
#   flhaz   : -6 <= lhaz   <= 6
#   fwflz   : -5 <= wflz   <= 5
#   fbmiz   : -5 <= bmiz   <= 5
#   fheadcz : -5 <= headcz <= 5
#
# ROW ALIGNMENT AND THE `work` TABLE
# -----------------------------------
# The measurements are copied, under standard names (agedays, sex_n, wt,
# lenhei, headc, bmi, len_2), into a private `work` table. No rows are
# dropped and nothing is re-sorted, so `work` stays in the input's row
# order throughout, and both reference lookups return one value per row of
# `work`, in that order. Children outside a reference range get NA
# reference values and therefore NA z-scores. The z-scores are computed on
# `work`; only at the end are the new result columns (who_waz, fwaz, ...)
# written onto `data`, by position. No row-id column or final join is
# needed, and internal column names can never collide with the caller's.
#
# Helpers marked "(utils-anthro.R)" below are shared with cdc_z() and are
# defined in R/utils-anthro.R.


# ---- helpers ----------------------------------------------------------------

# LMS z-score, z = ((x/M)^L - 1) / (L*S), with the WHO adjustment used for
# the weight-based indicators (correct = TRUE): beyond +/-3 the z-score is
# recomputed on a linear scale between the +/-2 and +/-3 SD values,
#   z =  3 + (x - SD3pos) / (SD3pos - SD2pos)   for z >  3
#   z = -3 - (x - SD3neg) / (SD3neg - SD2neg)   for z < -3
# (WHO 2006, Methods and Development, chapter 7). Length/height and head
# circumference are not adjusted (correct = FALSE). NA inputs stay NA:
# which() drops them from idx_hi / idx_lo.
calc_who_z <- function(var, l, m, s, correct = FALSE) {
  z <- ((var / m)^l - 1) / (l * s)

  if (correct) {
    idx_hi <- which(z > 3)
    idx_lo <- which(z < -3)

    if (length(idx_hi) > 0) {
      inv_l <- 1 / l[idx_hi]
      sdp2 <- m[idx_hi] * (1 + 2 * l[idx_hi] * s[idx_hi])^inv_l
      sdp3 <- m[idx_hi] * (1 + 3 * l[idx_hi] * s[idx_hi])^inv_l
      z[idx_hi] <- 3 + (var[idx_hi] - sdp3) / (sdp3 - sdp2)
    }

    if (length(idx_lo) > 0) {
      inv_l <- 1 / l[idx_lo]
      sdm2 <- m[idx_lo] * (1 - 2 * l[idx_lo] * s[idx_lo])^inv_l
      sdm3 <- m[idx_lo] * (1 - 3 * l[idx_lo] * s[idx_lo])^inv_l
      z[idx_lo] <- -3 - (var[idx_lo] - sdm3) / (sdm3 - sdm2)
    }
  }
  return(z)
}


who_z <- function(data,
               agedays = agedays,
               wt      = wt,
               lenhei  = lenhei,
               bmi     = bmi,
               headc   = headc,
               sex_code = 1:2
               )
{
  # Catch a missing or malformed 'data' argument early
  if (missing(data))
    stop("Argument 'data' is required.")
  if (!is.data.frame(data))
    stop("'data' must be a data.frame, data.table, or tibble, not '",
         class(data)[1], "'.")

   # Check sex_code before any other work, so a bad value fails at once.
   # validate_sex_code() (utils-anthro.R)
   validate_sex_code(sex_code)

  # Work in a new data.table container, so the caller's object is never
  # modified, and remember the input's class for the return value.
  # as_data_table_safe() (utils-anthro.R)
   type <- as_data_table_safe(data)
   data <- type$data

   # Name of the sex column: sex, SEX, Sex, ... resolve_sex_column() (utils-anthro.R)
   sex_var <- resolve_sex_column(data)

   # ---- column-name arguments -> column names, once, up front ----
   # agedays is required. wt, lenhei, headc and bmi are optional: if the
   # named column is not in data, $has is FALSE and that z-score is left out
   # of the output; a supplied name that isn't a column (a likely typo)
   # gives a warning. resolve_col_name() and resolve_optional_var()
   # (utils-anthro.R)
   agedays_var <- resolve_col_name(substitute(agedays))
   wt_arg      <- resolve_optional_var(data, substitute(wt),     missing(wt),     "wt")
   lenhei_arg  <- resolve_optional_var(data, substitute(lenhei), missing(lenhei), "lenhei")
   headc_arg   <- resolve_optional_var(data, substitute(headc),  missing(headc),  "headc")
   bmi_arg     <- resolve_optional_var(data, substitute(bmi),    missing(bmi),    "bmi")

   # The same column named for two measurements is almost always a positional
   # slip, e.g. who_z(d, agedays, lenhei) puts the length column into 'wt'.
   used <- c(wt = wt_arg$var, lenhei = lenhei_arg$var,
             headc = headc_arg$var, bmi = bmi_arg$var)
   dup  <- used[duplicated(used) | duplicated(used, fromLast = TRUE)]
   if (length(dup))
     stop("Column '", dup[[1]], "' is used for more than one of: ",
          paste(names(dup), collapse = ", "),
          ". Name the arguments, e.g. who_z(data, agedays = ..., lenhei = ...).")

   has_wt     <- wt_arg$has
   has_lenhei <- lenhei_arg$has
   has_headc  <- headc_arg$has
   has_bmi    <- bmi_arg$has
   have_bmi   <- has_bmi || (has_wt && has_lenhei)

   if (!agedays_var %in% names(data))
      stop("Age variable '", agedays_var, "' not found in data.")
   av <- data[[agedays_var]]
   if (!is.numeric(av))
      stop("Age variable '", agedays_var, "' must be numeric (age in days).")

   if (any(abs(av - round(av)) > 1e-8, na.rm = TRUE)) {
     warning("Non-integer agedays found; rounded to whole days.")
     av <- round(av)
   }
   # A negative age is a data error (e.g. exam date before birth date). The
   # rows are kept and get NA, as for any age outside the reference range,
   # but say so once, as gradual_z() does.
   n_neg <- sum(av < 0, na.rm = TRUE)
   if (n_neg > 0L)
     warning(n_neg, " row(s) have negative 'agedays'; their results are NA. ",
             "Check your data.", call. = FALSE)

   if (!any(has_wt, has_lenhei, has_headc, has_bmi))
     stop("No measurement columns found. Supply at least one of wt, lenhei, headc, bmi.")

   # Report every non-numeric measurement column in one error rather than
   # stopping at the first. is_num_or_all_na() (utils-anthro.R)
   bad <- character(0)
   if (has_wt && !is_num_or_all_na(data[[wt_arg$var]]))
     bad <- c(bad, sprintf("weight variable '%s' (kg)", wt_arg$var))
   if (has_lenhei && !is_num_or_all_na(data[[lenhei_arg$var]]))
     bad <- c(bad, sprintf("length/height variable '%s' (cm)", lenhei_arg$var))
   if (has_headc && !is_num_or_all_na(data[[headc_arg$var]]))
     bad <- c(bad, sprintf("head circumference variable '%s' (cm)", headc_arg$var))
   if (has_bmi && !is_num_or_all_na(data[[bmi_arg$var]]))
     bad <- c(bad, sprintf("BMI variable '%s'", bmi_arg$var))

   if (length(bad) > 0)
     stop("The following must be numeric: ", paste(bad, collapse = "; "), ".")

   # ---- working table (see ROW ALIGNMENT above) --------------------------
   # Sex codes -> 1L / 2L / NA, warning about unrecognized codes:
   # normalize_sex() (utils-anthro.R)
   work <- data.table(agedays = av,
                      sex_n = normalize_sex(data[[sex_var]],
                      sex_code = sex_code))

   if (nrow(work) > 0L && all(is.na(work$sex_n)))
      stop("No valid sex values found. Expected M/F, ",
           sex_code[1], "/", sex_code[2], ", B/G, or lower-case.")

   # num_col() turns an all-NA logical column into NA_real_ (utils-anthro.R)
   if (has_wt)     work[, wt     := num_col(data[[wt_arg$var]])]
   if (has_lenhei) work[, lenhei := num_col(data[[lenhei_arg$var]])]
   if (has_headc)  work[, headc  := num_col(data[[headc_arg$var]])]

   # Zero and negative measurements become NA. set_nonpositive_na() (utils-anthro.R)
   set_nonpositive_na(work, c("wt", "lenhei", "headc"))

   if (has_bmi) {
     work[, bmi := num_col(data[[bmi_arg$var]])]
   } else if (has_wt && has_lenhei) {
      work[, bmi := (wt * 10000) / (lenhei * lenhei)]
   }

   # length is used for weight-for-length, which applies only to ages <= 730 d;
   # older children get NA here and so find no match in the WFL reference.
   if (has_lenhei) {
     work[, len_2 := fifelse(agedays <= 730L, as.double(lenhei), NA_real_)]
   }

   set_nonpositive_na(work, c("bmi"))

   # ---- age-based indicators (waz, lhaz, bmiz, headcz) -----------------------
   # Only 'forage' rows can match on agedays; keep just the twelve L/M/S
   # columns the z-scores need.
   lmscols <- c('wei_l', 'wei_m', 'wei_s', 'len_l', 'len_m', 'len_s',
                'bmi_l', 'bmi_m', 'bmi_s', 'headc_l', 'headc_m', 'headc_s')
   lmscols <- intersect(lmscols, names(cdcanthro::who_ref_data))

   dref1 <- cdcanthro::who_ref_data[denom=='forage' & agedays <= 1826,
                                    c('sexn', 'agedays', lmscols), with = FALSE]

   # Look up each child's row of dref1 by (sex, agedays), by index rather
   # than a join. rc("wei_m") then returns the reference median weight for
   # every row of `work` (NA where there is no match, e.g. age over 1826
   # days). ref_index_lookup() (utils-anthro.R), which also explains rc()
   ref1 <- ref_index_lookup(dref1, work, on = c(sexn = "sex_n", "agedays"))
   rc   <- ref1$rc

   # Weight-based measures (waz, bmiz, and wflz below) use the WHO adjustment
   # beyond +/-3 SD (correct = TRUE); length/height and head circumference do
   # not. See calc_who_z() above.
   if (has_wt)
     work[, waz := calc_who_z(wt, rc('wei_l'), rc('wei_m'), rc('wei_s'),
                              correct = TRUE)]
   if (has_lenhei)
     work[, lhaz := calc_who_z(lenhei, rc('len_l'), rc('len_m'), rc('len_s'),
                               correct = FALSE)]
   if (have_bmi)
     work[, bmiz := calc_who_z(bmi, rc('bmi_l'), rc('bmi_m'), rc('bmi_s'),
                               correct = TRUE)]
   if (has_headc)
     work[, headcz := calc_who_z(headc, rc('headc_l'), rc('headc_m'), rc('headc_s'),
                                 correct = FALSE)]

   # ---- weight-for-length (with reference interpolation) ---------------------
   # who_ref_data tabulates weight-for-length on a 0.1 cm grid. Unlike the
   # age lookup, lengths can fall between grid points, so this uses a join
   # on a (possibly interpolated) table rather than ref_index_lookup().

   if (has_wt && has_lenhei) {
      dref2 <- cdcanthro::who_ref_data[denom=='forlen' & len >= 45 & len <= 110,
                            .(sexn,len,wfl_l,wfl_m,wfl_s)]

      # Unique lengths needing a reference row (len_2 is NA after 730 days,
      # so this is the <= 2 y subset).
      ulens <- unique(work$len_2[!is.na(work$len_2)])
      needs_interp <- any(!ulens %in% dref2$len)

      # Lengths off the 0.1 cm grid get L, M, S by linear interpolation
      # within sex. rule = 1 returns NA outside 45-110 cm, so those children
      # get NA wflz.
      if (needs_interp) {
        setkey(dref2, sexn, len)

      dref2 <- dref2[, {
        list(
          len   = ulens,
          wfl_l = stats::approx(len, wfl_l, xout = ulens, rule = 1)$y,
          wfl_m = stats::approx(len, wfl_m, xout = ulens, rule = 1)$y,
          wfl_s = stats::approx(len, wfl_s, xout = ulens, rule = 1)$y
        )
      }, by = sexn]
      }

      # Join `work` to dref2 on (sex, length). The j expression is evaluated
      # once per row of `work`, in `work`'s order, so the result can be set
      # by position. i.wt is the weight column of `work` (the i-table);
      # dref2 has no wt column, so the prefix just makes the source explicit.
      # dref2's key columns are 'sexn'/'len'; work's are 'sex_n'/'len_2'.
      set(work, j = 'wflz',
          value = dref2[work, on = c(sexn = 'sex_n', len = 'len_2'),
                        calc_who_z(i.wt, wfl_l, wfl_m, wfl_s, correct = TRUE)])
   }

   # ---- write the results onto `data` ----------------------------------------
   # `work` is in `data`'s row order, so each result column is copied across
   # by position with set(). BIV flags are NA wherever the z-score is NA.
   zs <- intersect(c('waz', 'lhaz', 'wflz', 'bmiz', 'headcz'), names(work))

   # BIV plausibility ranges (listed in the header); a z-score outside
   # [lo, hi] is flagged. Note that the waz range is asymmetric.
   biv <- list(waz    = c(lo = -6, hi = 5),
               lhaz   = c(lo = -6, hi = 6),
               wflz   = c(lo = -5, hi = 5),
               bmiz   = c(lo = -5, hi = 5),
               headcz = c(lo = -5, hi = 5))
   fnm <- c(waz = 'fwaz', lhaz = 'flhaz', wflz = 'fwflz',
            bmiz = 'fbmiz', headcz = 'fheadcz')

   # Every column this call is about to add: the who_-prefixed z-scores, their
   # (unprefixed) BIV flags, and -- when bmi was derived rather than supplied
   # -- 'bmi' itself. Collected up front so overlap with the caller's own
   # columns can be checked once, the same way cdc_z() does it.
   out_cols <- c(paste0('who_', zs), unname(fnm[zs]))
   if (!has_bmi && have_bmi) out_cols <- c(out_cols, 'bmi')

   # Rename any of data's own columns that would collide with a computed
   # output column. New names are made unique against every other input
   # column and every output column, so an existing 'fwaz0' can't collide.
   overlap <- intersect(names(data), out_cols)
   if (length(overlap) > 0) {
     new_nm <- utils::tail(make.unique(c(setdiff(names(data), overlap), out_cols,
                                         paste0(overlap, "0"))),
                           length(overlap))
     setnames(data, overlap, new_nm)
     warning(paste(overlap, collapse = ", "),
             " in original data renamed to ",
             paste(new_nm, collapse = ", "),
             " to avoid conflict with computed values.")
   }

   stopifnot(nrow(work) == nrow(data))
   for (z in zs) {
     v <- work[[z]]
     # z-scores are prefixed 'who_' so they never collide with cdc_z()'s
     set(data, j = paste0('who_', z), value = v)
     set(data, j = fnm[[z]],
         value = as.integer(!v %between% c(biv[[z]]['lo'], biv[[z]]['hi'])))
   }

   if (!has_bmi && have_bmi) {
     set(data, j = "bmi", value = work$bmi)
   }

   # Return the same class as the input. restore_input_type() (utils-anthro.R)
   restore_input_type(data, type$input_was_df, type$input_was_tibble)
}
