# =============================================================================
# Internal helpers shared by cdc_z() and who_z()
# =============================================================================
#
# Both functions take a data.frame, data.table or tibble, resolve column-name
# arguments against it, standardize the sex column, look up reference values,
# and return a result of the same class they were given. That shared plumbing
# lives here, once, so the two functions cannot drift apart.
#
# Contents, in the order the main functions use them:
#   validate_sex_code()     check the sex_code argument
#   as_data_table_safe()    data.table container to work in; records input class
#   resolve_sex_column()    find the column named sex / SEX / Sex
#   resolve_col_name()      argument expression -> column-name string
#   resolve_optional_var()  resolve an optional measurement argument
#   is_num_or_all_na()      is a measurement column usable?
#   normalize_sex()         sex codes -> 1L (boys) / 2L (girls) / NA
#   num_col()               measurement column -> numeric
#   set_nonpositive_na()    zero and negative measurements -> NA
#   ref_index_lookup()      row index into a reference table, and rc()
#   restore_input_type()    convert the result back to the input's class

# ---- column-name argument resolution ---------------------------------------

# Turn a captured argument expression into a column-name string. Accepts a
# bare symbol (wt = weight) or a quoted string (wt = "weight"). The
# expression is never evaluated, so a column name that happens to match a
# variable in the caller's workspace is not mistaken for that variable.
#
# The caller must pass substitute(arg) taken in its own frame: substitute()
# only sees how an argument was written in the function whose argument it
# is, so it cannot be called here.
resolve_col_name <- function(expr) {
   if (is.character(expr)) expr else deparse(expr)
}

# Resolve one optional measurement argument (wt, ht, lenhei, bmi, headc).
# `expr` is substitute(arg) and `was_missing` is missing(arg), both taken in
# the calling function.
#
# The name is resolved whether or not the argument was supplied, so a
# default such as wt = wt finds a column named 'wt' without the caller
# having to write wt = wt.
#
# - Column exists: has = TRUE, var = its name.
# - Argument supplied but the column doesn't exist: almost certainly a typo,
#   so warn; has = FALSE.
# - Argument not supplied and the default column doesn't exist: the caller
#   simply doesn't have that measurement; no warning; has = FALSE.
#
# Returns list(var = <name or NULL>, has = <logical>, typo = <logical>).
resolve_optional_var <- function(data, expr, was_missing, arg_label) {
   var  <- resolve_col_name(expr)
   has  <- var %in% names(data)
   typo <- !was_missing && !has
   if (typo) {
      warning("Argument '", arg_label, " = ", var, "' was supplied, but '",
              var, "' is not a column in data (check for a typo).")
   }
   list(var = if (has) var else NULL, has = has, typo = typo)
}

# ---- sex column -------------------------------------------------------------

# Return the name of the caller's sex column, which must be some case
# variant of 'sex' (sex, SEX, Sex, ...). Stops with a specific message if
# there is none, or more than one (e.g. both 'sex' and 'SEX').
resolve_sex_column <- function(data) {
  nms <- grep('^sex$', names(data), ignore.case = TRUE, value = TRUE)
  if (length(nms) == 0) {
    stop("No sex column found. A child's sex column MUST be named some ",
         "case variant of 'sex' (e.g. 'sex', 'SEX', 'Sex').")
  }
  if (length(nms) > 1) {
    stop("Multiple candidate sex columns found: ", paste(nms, collapse = ", "),
         ". Only one column named some case variant of 'sex' may be ",
         "present in data.")
  }
  nms   # return the name; no renaming
}

# Check sex_code = c(male_code, female_code): the two values that mean boy
# and girl in a numeric sex column (default 1/2; c(0, 1) for 0/1 coding).
# cdc_z() and who_z() call this first so a bad value fails before any other
# work; normalize_sex() calls it again.
validate_sex_code <- function(sex_code) {
   if (!is.numeric(sex_code) || length(sex_code) != 2 || anyNA(sex_code) ||
       sex_code[1] == sex_code[2]) {
      stop("'sex_code' must be a length-2 numeric vector of two distinct, ",
           "non-NA values: c(male_code, female_code). Got: ",
           paste(sex_code, collapse = ", "))
   }
   invisible(sex_code)
}

# Recognized text sex codes, compared after trimws() and tolower(). Whole
# values are matched, not first letters, so codes such as 'missing',
# 'mujer' or 'not stated' become NA instead of being read as a sex.
.male_words   <- c("m", "male", "males", "b", "boy", "boys")
.female_words <- c("f", "female", "females", "g", "girl", "girls")

# Map a vector of sex codes to 1L (boys), 2L (girls) or NA_integer_.
#   numeric sx:          sex_code[1] -> 1L, sex_code[2] -> 2L
#   character/factor sx: the sex_code values as text (e.g. "1", "2"), or the
#                        words in .male_words / .female_words
# Non-missing values that match neither become NA, with ONE warning giving
# the number of rows and the values seen, so a coding problem cannot
# silently drop children. (If no value matches at all, the caller stops
# with an error instead, so no warning is given here.)
#
# Text input is matched on its unique values only and expanded back with
# chmatch(), so the string work runs on a handful of distinct codes.
normalize_sex <- function(sx, sex_code = 1:2) {
  validate_sex_code(sex_code)
  if (is.numeric(sx)) {
    out <- fcase(sx == sex_code[1], 1L, sx == sex_code[2], 2L,
                 default = NA_integer_)
  } else {
    sx    <- as.character(sx)
    usex  <- unique(sx)
    key   <- tolower(trimws(usex))
    male_codes   <- c(format(sex_code[1], scientific = FALSE), .male_words)
    female_codes <- c(format(sex_code[2], scientific = FALSE), .female_words)
    usexn <- fifelse(key %chin% male_codes, 1L,
                     fifelse(key %chin% female_codes, 2L, NA_integer_))
    out <- usexn[chmatch(sx, usex)]
  }
  bad <- !is.na(sx) & is.na(out)
  if (any(bad) && !all(is.na(out))) {
    vals <- unique(sx[bad])
    warning(sum(bad), " row(s) have an unrecognized sex code (",
            paste0("'", utils::head(vals, 5), "'", collapse = ", "),
            if (length(vals) > 5) ", ..." else "",
            "); their results are NA. See 'Details' or 'sex_code' in the help page.",
            call. = FALSE)
  }
  out
}

# Put the caller's columns in a new data.table container to work in, and
# record whether the input was a data.frame or a tibble so
# restore_input_type() can convert the result back.
#
# This is a shallow copy: the container (and so its column list and names)
# is new, but the column vectors are shared with the caller's object. That
# is safe because cdc_z() and who_z() only add new columns to it or rename
# it (setnames()); neither ever modifies an existing column in place, which
# would change the caller's data too.
as_data_table_safe <- function(data) {
  input_was_tibble <- inherits(data, "tbl_df")
  input_was_df     <- inherits(data, "data.frame") &&
    !inherits(data, "data.table") && !input_was_tibble

  data <- data.table::setDT(as.list(data))

  list(
    data = data,
    input_was_df = input_was_df,
    input_was_tibble = input_was_tibble
  )
}

# Set zero and negative values in the named measurement columns to NA, by
# reference. Used on the private `work` table only, never on the caller's
# columns. Columns not present are skipped.
set_nonpositive_na <- function(data, cols) {
  cols <- intersect(cols, names(data))

  for (j in cols) {
    x <- data[[j]]
    i <- which(x <= 0)

    if (length(i))
      data.table::set(data, i = i, j = j, value = rep(NA_real_, length(i)) )
  }
  invisible(data)
}

# Look up each row of `work` in a reference table by index, not by a join.
#
# idx[i] is the row of `ref` whose keys (sex, age) match work[i], or NA when
# there is no match (age outside the reference range, unrecognized sex,
# missing age). Nothing is re-sorted, so idx[i] always belongs to work[i].
#
# rc(col) returns ref[[col]][idx]: one reference value per row of `work`,
# NA where idx is NA, which is what makes those children's z-scores NA.
# Pulling one column at a time avoids building an n-row copy of the
# reference table, and columns for measurements not supplied are never
# touched. rc() keeps `ref` and `idx` from this call (a closure), so the
# caller passes only the column name: rc("wm"), rc("wei_l"), ...
#
# idx has exactly nrow(work) elements only if `ref` has one row per key;
# duplicated keys would misalign every result, so that is an error.
# Used by both cdc_z() and who_z().
ref_index_lookup <- function(ref, work, on) {
  idx <- ref[work, on = on, which = TRUE]
  if (length(idx) != nrow(work)) {
    stop(
      "Reference lookup returned ", length(idx),
      " indices for ", nrow(work),
      " rows; reference table may contain duplicate join keys.",
      call. = FALSE
    )
  }
  list(
    idx = idx,
    rc  = function(col) ref[[col]][idx]
  )
}

# A measurement column is usable if it is numeric or entirely NA. An all-NA
# column such as data.frame(ht = NA) is logical, not numeric, but just means
# "no values"; num_col() turns it into NA_real_.
is_num_or_all_na <- function(x) is.numeric(x) || (is.logical(x) && all(is.na(x)))
num_col <- function(x) if (is.numeric(x)) x else as.numeric(x)

# Inverse of as_data_table_safe(): restore the caller's original type.
restore_input_type <- function(data, input_was_df, input_was_tibble) {
  if (input_was_df) return(setDF(data))
  if (input_was_tibble) {
    if (requireNamespace("tibble", quietly = TRUE)) {
      return(tibble::as_tibble(data))
    }
    warning("tibble package not installed; returning a data.frame instead.")
    return(as.data.frame(data))
  }
  data[]
}

