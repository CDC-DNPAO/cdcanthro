# cdc_z(): CDC 2000 growth-chart z-scores and percentiles for weight, height,
# and BMI (24.0 to < 240 months), including the extended BMI metrics
# (Wei et al. 2020; Hales et al. 2022) for BMIs at or above the 95th
# percentile.
#
# Pipeline: validate arguments -> build a thin `work` table (age, sex,
# measurements) in the input's row order -> align ages (+0.5 for completed
# months) -> interpolate the reference L, M, S to the ages present -> look
# up each row's reference values by index -> compute z-scores, percentiles
# and the extended BMI metrics on `work` -> bind the requested columns to
# the input and return it in the input's class.
#
# Helpers marked "(utils-anthro.R)" below are shared with who_z() and are
# defined in R/utils-anthro.R. The globalVariables() declarations for the
# column names used in data.table calls are in R/globals.R.

# z at the 95th percentile; p95 = M * (1 + L*S*.qn95)^(1/L)
.qn95 <- qnorm(0.95)
# cdcanthro() is the old name of cdc_z(); it warns once per session.
.cdcanthro_state <- new.env(parent = emptyenv())
.cdcanthro_state$warned <- FALSE

cdcanthro <- function(data, age = agemos, wt = wt, ht = ht, bmi = bmi,
                      sex_code = 1:2, all_vars = FALSE) {
  if (!.cdcanthro_state$warned) {
    warning(
      "`cdcanthro()` is deprecated; please use `cdc_z()`.",
      call. = FALSE
    )
    .cdcanthro_state$warned <- TRUE
  }
  mc <- match.call()

  # Quote the fully qualified function name to protect the traceback
  # and guarantee namespace resolution.
  mc[[1]] <- quote(cdcanthro::cdc_z)
  eval(mc, envir = parent.frame())
}

# -------------------------------------------------------------------------
# cdc_z()
# -------------------------------------------------------------------------

cdc_z <- function(data,
                      age = agemos,
                      wt = wt,
                      ht = ht,
                      bmi = bmi,
                      sex_code = 1:2,
                      all_vars = FALSE
                      )
{

  # --- INPUT VALIDATION ---
  # Catch a missing or malformed 'data' argument early with a clear message,
  # rather than failing deep inside as.data.table() or a downstream
  # data.table call with a confusing error.
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

  # --- ARGUMENT PARSING ---
  # age is required. wt, ht and bmi are optional: if the named column is not
  # in data, $has is FALSE and the metrics that need it are left out of the
  # output; a supplied name that isn't a column (a likely typo) gives a
  # warning. resolve_col_name() and resolve_optional_var() (utils-anthro.R)
  age_var <- resolve_col_name(substitute(age))
  if (!age_var %in% names(data))
    stop("Age variable '", age_var, "' not found in data.")
  if (!is.numeric(data[[age_var]]))
    stop("Age variable '", age_var, "' must be numeric (age in months).")

  wt_arg  <- resolve_optional_var(data, substitute(wt),  missing(wt),  "wt")
  ht_arg  <- resolve_optional_var(data, substitute(ht),  missing(ht),  "ht")
  bmi_arg <- resolve_optional_var(data, substitute(bmi), missing(bmi), "bmi")

  # The same column named for two measurements is almost always a positional
  # slip, e.g. cdc_z(d, age, ht) puts the height column into 'wt'.
  used <- c(wt = wt_arg$var, ht = ht_arg$var, bmi = bmi_arg$var)
  dup  <- used[duplicated(used) | duplicated(used, fromLast = TRUE)]
  if (length(dup))
    stop("Column '", dup[[1]], "' is used for more than one of: ",
         paste(names(dup), collapse = ", "),
         ". Name the arguments, e.g. cdc_z(data, age = ..., ht = ...).")

  has_wt  <- wt_arg$has
  has_ht  <- ht_arg$has
  has_bmi <- bmi_arg$has
  have_bmi <- has_bmi || (has_wt && has_ht)

  if (!has_wt && !has_ht && !has_bmi)
    stop("At least one of weight, height, or BMI must be present in data.")

  # Report every non-numeric measurement column in one error rather than
  # stopping at the first. is_num_or_all_na() (utils-anthro.R)
  bad <- character(0)
  if (has_wt && !is_num_or_all_na(data[[wt_arg$var]]))
    bad <- c(bad, sprintf("weight variable '%s' (kg)", wt_arg$var))
  if (has_ht && !is_num_or_all_na(data[[ht_arg$var]]))
    bad <- c(bad, sprintf("height variable '%s' (cm)", ht_arg$var))
  if (has_bmi && !is_num_or_all_na(data[[bmi_arg$var]]))
    bad <- c(bad, sprintf("BMI variable '%s'", bmi_arg$var))
  if (length(bad) > 0)
    stop("The following must be numeric: ", paste(bad, collapse = "; "), ".")

  # --- WORK TABLE EXTRACTION ---
  # Build a thin working table under standard names (age, sex_n, wt, ht,
  # bmi), so the caller's columns are never written to. It is built by
  # direct column extraction, so it stays in `data`'s row order; the
  # positional lookup below relies on this.
  # Sex codes -> 1L / 2L / NA, warning about unrecognized codes:
  # normalize_sex() (utils-anthro.R)
  sex_n <- normalize_sex(data[[sex_var]], sex_code = sex_code)

  work <- data.table(
    age  = data[[age_var]],
    sex_n = sex_n
  )

  if (nrow(work) > 0L && all(is.na(work$sex_n)))
    stop("No valid sex values found. Expected M/F, ",
         sex_code[1], "/", sex_code[2], ", B/G, or lower-case.")

  # num_col() turns an all-NA logical column into NA_real_ (utils-anthro.R)
  if (has_wt) work[, wt := num_col(data[[wt_arg$var]])]
  if (has_ht) work[, ht := num_col(data[[ht_arg$var]])]

  # Zero and negative measurements become NA. set_nonpositive_na() (utils-anthro.R)
  set_nonpositive_na(work, c("wt", "ht"))

  if (has_bmi) {
    work[, bmi := num_col(data[[bmi_arg$var]])]
  } else if (has_wt && has_ht) {
    work[, bmi := (wt * 10000) / (ht * ht)]
  }

  set_nonpositive_na(work, c("bmi"))

  # --- AGE ALIGNMENT ---
  # If no age has a fractional part, the ages are taken to be completed
  # months (as NHANES reports them) and 0.5 is added so that 36 means the
  # midpoint of [36, 37). One non-integer age anywhere disables this and
  # the ages are used as given. The caller's data are not modified.
  uages <- unique(work$age)
  uages <- uages[!is.na(uages)]

  if (length(uages) > 0L && !any(abs(uages - round(uages)) > 1e-8)) {
    warning("All ages are integers so 0.5 months has been added before the ",
            "calculations to align with CDC intervals. See Details. ",
            "The input data was not modified.")
    set(work, j = "age", value = work$age + 0.5)
    uages <- uages + 0.5  # Update unique ages to match
  }

  # --- REFERENCE CHART INTERPOLATION ---
  # The reference grid is 23.5, 24.5, ..., 239.5, 240 months. Ages not on
  # the grid get L, M, S (and the other reference columns) by linear
  # interpolation with stats::approx(), within sex, at the unique ages
  # present in the data.
  #
  # The 23.5 row is kept because the table has no 24.0 row: ages 24.0 to
  # < 24.5 are interpolated between 23.5 and 24.5. Ages under 24 and ages
  # >= 240 are excluded from uages, find no reference row, and get NA.
  ref_data <- cdcanthro::cdc_ref_data  # ages 23.5, 24.5, ..., 239.5, 240
  uages <- uages[uages >= 24 & uages < 240]

  # Interpolate only if some age is off the grid; otherwise subset.
  need_interp <- !all(uages %in% ref_data$age)
  interp_vars <- setdiff(names(ref_data), c('sexn', 'age', 'mref', 'sref'))

  # age is passed in j as list(age = uages) rather than through .SD, so
  # lapply() does not interpolate age against itself. mref and sref (the
  # BMI M and S at 240 months) are constant within sex, so one value each
  # is carried through rather than interpolated.
  if (need_interp) {
    ref_data <- ref_data[,
         c(
           list(
             age  = uages,
             mref = mref[1L],
             sref = sref[1L]
           ),
           lapply(.SD, function(col) approx(age, col, xout = uages)$y)
         ),
         by = sexn, .SDcols = interp_vars
    ]
  } else {
    ref_data <- ref_data[age %in% uages]
  }

  # Derived reference quantities (1/(L*S), the +/-2 SD values for the
  # modified z-scores, p95 and sigma) are computed on the reference table --
  # one row per sex and unique age -- rather than on the n data rows.
  if (has_wt) {
    ref_data[, let(
      inv_wl_ws = 1 / (wl * ws),
      wt_sdm2 = wm - (wm * (1 - 2 * wl * ws)^(1 / wl)),
      wt_sdp2 = (wm * (1 + 2 * wl * ws)^(1 / wl)) - wm
    )]
  }
  if (has_ht) {
    ref_data[, let(
      inv_hl_hs = 1 / (hl * hs),
      ht_sdm2 = hm - (hm * (1 - 2 * hl * hs)^(1 / hl)),
      ht_sdp2 = (hm * (1 + 2 * hl * hs)^(1 / hl)) - hm
    )]
  }
  if (have_bmi) {
    ref_data[, let(
      inv_bl_bs = 1 / (bl * bs),
      bmi_sdm2 = bm - (bm * (1 - 2 * bl * bs)^(1 / bl)),
      bmi_sdp2 = (bm * (1 + 2 * bl * bs)^(1 / bl)) - bm,
      p95      = bm * (1 + bl * bs * .qn95) ^ (1 / bl),
      sigma    = fifelse(sexn == 1L,
              round(0.3728 + 0.5196 * (age / 12) - 0.0091 * (age / 12)^2, 8),
              round(0.8334 + 0.3712 * (age / 12) - 0.0011 * (age / 12)^2, 8))
              # rounded to 8 decimals to match the sigma values in the
              # CDC SAS program
    )]
  }

  # Look up each child's row of ref_data by (sex, age), by index rather than
  # a join. rc("wm") then returns the reference median weight for every row
  # of `work` (NA where there is no match, e.g. age outside 24 to < 240).
  # ref_index_lookup() (utils-anthro.R), which also explains rc()
  ref_idx <- ref_index_lookup(ref_data, work, on = c(sexn = "sex_n", "age"))
  rc      <- ref_idx$rc

  if (has_wt) {
    # L, M and S are stored on `work` because each is used more than once
    # below; dropped at the end of the block.
    work[, let(wl = rc("wl"), wm = rc("wm"), ws = rc("ws"))]
    work[, waz     := ((wt / wm)^wl - 1) * rc("inv_wl_ws")]
    work[, mod_waz := (wt - wm) / (0.5 * fifelse(wt < wm, rc("wt_sdm2"), rc("wt_sdp2")))]
    # The CDC SAS program switches to the log (L = 0) form when |L| < 0.01.
    # wl in cdc_ref_data is always <= -0.197, so that can never happen for
    # weight and the rule is applied to height only (below).
    work[, wap := 100 * pnorm(waz)]
    work[, c("wl", "wm", "ws") := NULL]
  }

  if (has_ht) {
    # L, M and S are stored on `work` because the |L| < 0.01 line below
    # subsets rows by them; dropped at the end of the block.
    work[, let(hl = rc("hl"), hm = rc("hm"), hs = rc("hs"))]
    work[, haz     := ((ht / hm)^hl - 1) * rc("inv_hl_hs")]
    work[, mod_haz := (ht - hm) / (0.5 * fifelse(ht < hm, rc("ht_sdm2"), rc("ht_sdp2")))]
    # Log form when |L| < 0.01, as in the CDC SAS program. hl crosses zero
    # (range -0.39 to 2.24), so this applies at some ages.
    work[abs(hl) < 0.01, haz := log(ht / hm) / hs]
    work[, hap := 100 * pnorm(haz)]
    work[, c("hl", "hm", "hs") := NULL]
  }

  if (have_bmi) {
    # Reference columns stored on `work` because each is used more than once:
    # bl and bm in several formulas, p95 and sigma by the extended-BMI lines
    # below, which subset rows (bmip >= 95).
    work[, let(bl = rc("bl"), bm = rc("bm"), p95 = rc("p95"), sigma = rc("sigma"))]

    # --- ALWAYS COMPUTED ---
    work[, bmi_ratio := bmi / bm]
    work[, let(
      perc_median = (bmi_ratio - 1) * 100,
      bmip95      = 100 * (bmi / p95),
      mod_bmiz    = (bmi - bm) / (0.5 * fifelse(bmi < bm, rc("bmi_sdm2"), rc("bmi_sdp2"))),
      bmiz        = (bmi_ratio^bl - 1) * rc("inv_bl_bs")
    )]
    # No log-form switch for BMI: bl in cdc_ref_data is always <= -0.95,
    # far from the 0.01 threshold used for weight and height.
    work[, let(bmip = 100 * pnorm(bmiz))]
    work[, let(ebp = bmip, ebz = bmiz)]

    # --- all_vars EXTRAS ONLY ---
    if (isTRUE(all_vars)) {
      work[, bs := rc("bs")]
      sref <- rc("sref")
      work[, let(
        dist_median = bmi - bm,
        z1          = (bmi_ratio - 1) / bs,
        z0          = log(bmi_ratio) / bs
      )]
      work[, let(
        adj_dist_median     = z1 * sref * rc("mref"),
        adj_perc_median     = z1 * 100 * sref,
        log_perc_median     = z0 * 100 * bs,
        adj_log_perc_median = z0 * 100 * sref
      )]
      rm(sref)
      # bl, bm and bs are already on `work`; renaming them to their output
      # names avoids gathering them from ref_data a second time.
      setnames(work, c("bl", "bm", "bs"), c("bmi_l", "bmi_m", "bmi_s"))
    }

    # Drop intermediate columns that are never returned.
    drop_cols <- intersect(c('bmi_ratio', 'z1', 'z0'), names(work))
    if (length(drop_cols)) work[, (drop_cols) := NULL]

    # Extended BMI percentile and z-score for BMI >= 95th percentile
    # (Wei et al. 2020; Hales et al. 2022): above the 95th percentile the LMS
    # z-scores compress into a narrow range, so BMI is instead modelled as
    # half-normal above p95 with scale sigma, and the percentile is
    # 90 + 10 * Phi((BMI - p95) / sigma).
    work[bmip >= 95, ebp := 90 + 10 * pnorm((bmi - p95) / sigma)]
    # ebp can round to exactly 100, where qnorm(1) is Inf. qnorm() of the
    # largest double below 1 is 8.2095, so 8.21 is the ceiling of what ebz
    # could ever be and is used as the cap.
    work[bmip >= 95 & ebp < 100, ebz := qnorm(ebp / 100)]
    work[bmip >= 95 & ebp >= 100, ebz := 8.21]

    # The extended values become bmiz / bmip; the LMS-only values are kept
    # as orig_bmiz / orig_bmip.
    setnames(work,
             c('bmiz',      'bmip',      'ebp',  'ebz'),
             c('orig_bmiz', 'orig_bmip', 'bmip', 'bmiz'))
    # Without all_vars these reference columns are not returned.
    if (!isTRUE(all_vars)) work[, c("sigma", "bl", "bm") := NULL]
  }

  # --- OUTPUT ---
  # Columns to return, in this order; bmi only when it was computed here.
  v <- c('bmiz', 'bmip', 'p95', 'bmip95', 'orig_bmip', 'orig_bmiz',
        'perc_median', 'mod_bmiz', 'waz', 'wap', 'mod_waz', 'haz', 'hap',
        'mod_haz')

  if (!has_bmi && have_bmi) v <- c('bmi', v)

  if (isTRUE(all_vars) && have_bmi) {
    v <- c(v, 'bmi_l', 'bmi_m', 'bmi_s',  'sigma', 'adj_dist_median', 'dist_median',
        'adj_perc_median', 'log_perc_median', 'adj_log_perc_median')
  }

  # Keep only cols that were actually computed
  v <- intersect(v, names(work))

  # Rename any of data's own columns that would collide with a computed
  # output column. New names are made unique against every other input
  # column and every output column, so an existing 'bmiz0' can't collide.
  overlap <- intersect(names(data), v)
  if (length(overlap) > 0) {
    new_nm <-  utils::tail(make.unique(c(setdiff(names(data), overlap), v,
                                 paste0(overlap, "0"))),
                   length(overlap))
    setnames(data, overlap, new_nm)
    warning(paste(overlap, collapse = ", "),
            " in original data renamed to ",
            paste(new_nm, collapse = ", "),
            " to avoid conflict with computed values.")
  }

  # `work` is in `data`'s row order, so the new columns are bound by position.
  stopifnot(nrow(work) == nrow(data))
  out <- setDT(c(as.list(data), as.list(work)[v]))

  # Return the same class as the input. restore_input_type() (utils-anthro.R)
  restore_input_type(out, type$input_was_df, type$input_was_tibble)
}
