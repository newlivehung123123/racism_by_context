#==============================================================================
# analysis_tools.R
# Structural inequalities in education in China, CFPS 2010 to 2018
#
# Settings and functions that 02_link.R, 03_describe.R, 04_models.R and
# 05_checks.R share. Each of those scripts reads 00_setup.R and then this
# file. The file defines settings and functions only and reads no data.
#
# Status. Written on 23 October 2026, after the waves 2020 and 2022 were
# dropped, and tested on made-up data only. Revised the same day, after the
# first run on the CFPS data and before any table of estimates was read: the
# education variables of 2010 (more names, and name patterns, because the
# 2010 files hold none of the names first listed for school level and years
# of schooling), the merging of small or separated birth cohorts and
# provinces, the checks of separation, and the starting values of check R10.
# The revision was tested on made-up data only. Revised again on 24 October
# 2026, after the second run and before any table of estimates of that run
# was read: a model whose non-Han respondents of one hukou group at age 3
# have separation is fitted without them and shows the effect for the other
# hukou group (run_model, sep_hukou), a fit of check R10 that has not
# converged or whose cumulative logits cross counts as not fitted (fit_cum),
# and the log lists the events logged by mice. Tested on made-up data only.
#
# The functions follow the data rules of 00_setup.R. Data sets exist only
# inside functions, every count in a log has thousands separators, so that no
# run of nine or more digits appears, and every table goes through write_out,
# which shows a count from 1 to 9 as <10.
#==============================================================================

suppressPackageStartupMessages(library(survey))
options(survey.lonely.psu = "adjust")

# Number of processor cores used at once and number of imputed data sets.
# Tests on made-up data change both through the settings CFPS_CORES and
# CFPS_M.
CORES <- as.integer(Sys.getenv("CFPS_CORES", "4"))
M_IMP <- as.integer(Sys.getenv("CFPS_M", "20"))
SEED <- 20261014L

# Smallest number of Han and of non-Han respondents in a category of an
# outcome (section 6.7 of the analysis plan), and smallest number of
# respondents for a province or a birth cohort to have an indicator of its
# own
MINCAT <- 20
MINPROV <- 30


#------------------------------------------------------------------------------
# Variables read from each file. Names absent from a file become empty
# columns, and the log 02_link.log lists them. A name with * reads every
# variable of the file whose name it matches.
#------------------------------------------------------------------------------
FS <- c("familysize", "familysize10", "familysize12", "familysize14", "familysize16",
        "familysize18", "fml_count")
AV <- list(
  A10 = c("pid", "fid", "provcd", "countyid", "subpopulation", "subsample", "urban", "rswt_nat",
          "rswt_res", "qa1age", "qa1y_best", "gender", "qa5code", "qa2", "qa302", "qa402", "qd2",
          "ks3", "qz205", "wordtest", "qd3", "cfps2010edu_best", "cfps2010sch_best",
          "cfps2010eduy_best", "cfps2010edu", "cfps2010sch", "cfps2010eduy", "edu2010_t1_best",
          "edu2010", "qc1", "sch2010", "eduy2010", "cfps2010edu_*", "cfps2010sch_*",
          "cfps2010eduy_*", "edu2010_*", "sch2010_*", "eduy2010_*"),
  C10 = c("pid", "fid", "provcd", "countyid", "subpopulation", "subsample", "rswt_nat", "rswt_res",
          "wa1age", "wa1y", "gender", "wa6code", "wa3", "wk2", "wz205", "ks3", "wordtest",
          "cfps2010edu_best", "cfps2010edu", "edu2010", "cfps2010edu_*", "edu2010_*"),
  A12 = c("pid", "fid12", "fid10", "provcd", "countyid", "subpopulation", "subsample",
          "cfps2012_age", "cfps2012_birthy", "cfps2012_gender", "qa701code", "qa301", "qa503",
          "qa603", "edu2012", "cfps2012edu", "sch2012", "eduy2012", "qd201", "ks3m", "qz205",
          "wc01", "urban12", "rswt_natcs12", "rswt_rescs12", "rswt_natpn1012"),
  C12 = c("pid", "wa6code", "cfps_minzu", "wa3"),
  A14 = c("pid", "fid14", "provcd14", "countyid14", "subpopulation10", "subsample10",
          "cfps2014_age", "cfps_birthy", "cfps_gender", "qa701code", "qa301", "qa503", "qa603",
          "cfps2014edu", "cfps2014sch", "cfps2014eduy_im", "ks3m", "qz205", "wordtest14", "wc01",
          "selfrpt", "rswt_natcs14", "rswt_rescs14"),
  C14 = c("pid", "wa6code", "wa3"),
  A16 = c("pid", "fid16", "provcd16", "countyid16", "subpopulation", "subsample", "cfps_age",
          "cfps_birthy", "cfps_gender", "pa701code", "pa301", "pa503", "pa603", "cfps2016edu",
          "cfps2016sch", "cfps2016eduy_im", "ks3m", "qz205", "school", "selfrpt", "rswt_natcs16",
          "rswt_rescs16"),
  C16 = c("pid", "pa701code", "pa503", "pa603"),
  A18 = c("pid", "fid18", "provcd18", "psu", "countyid18", "subpopulation", "subsample", "age",
          "ibirthy", "gender", "minzu", "qa701code", "qa301", "qa603", "cfps2018edu",
          "cfps2018sch", "cfps2018eduy_im", "qs3m", "qz103", "wordtest18", "school", "selfrpt",
          "urban18", "rswt_natcs18n", "rswt_natpn1018n"),
  C18 = c("pid", "minzu", "wa701code", "wa503", "wa603", "rswt_natpn1018n", "psu",
          "subpopulation"),
  R10 = c("pid", "fid", "feduc", "meduc"),
  R12 = c("pid", "fid12", "fid10", "feduc12", "meduc12"),
  R14 = c("pid", "fid14", "tb4_a14_f", "tb4_a14_m"),
  R16 = c("pid", "fid16", "tb4_a16_f", "tb4_a16_m"),
  R18 = c("pid", "fid18", "tb4_a18_f", "tb4_a18_m"),
  E10 = c("fid", "faminc_net", FS),
  E12 = c("fid12", "fincome1", FS),
  E14 = c("fid14", "fincome1", FS),
  E16 = c("fid16", "fincome1", FS),
  E18 = c("fid18", "fincome1", FS))

# Family ID of each wave and the parents' education items of each roster
FIDV <- c("10" = "fid", "12" = "fid12", "14" = "fid14", "16" = "fid16", "18" = "fid18")
PE <- list("10" = c("feduc", "meduc"), "12" = c("feduc12", "meduc12"),
           "14" = c("tb4_a14_f", "tb4_a14_m"), "16" = c("tb4_a16_f", "tb4_a16_m"),
           "18" = c("tb4_a18_f", "tb4_a18_m"))

# Variables whose value labels give the names of provinces and ethnic groups
LABEL_VARS <- c("provcd", "provcd14", "provcd16", "provcd18", "qa5code", "qa701code",
                "pa701code", "minzu", "wa6code", "wa701code", "cfps_minzu")
ETH_VARS <- c("qa5code", "qa701code", "pa701code", "minzu", "wa6code", "wa701code", "cfps_minzu")
PROV_VARS <- c("provcd", "provcd14", "provcd16", "provcd18")

# Variables of each wave. The items starting with c_ are the child file
# items. Where a field lists several names, the first name with a valid
# value is used, in each record for ethnicity and for the whole file for
# education.
VM <- list(
  "10" = list(fid = "fid", prov = "provcd", cnty = "countyid", psu = "countyid",
              subpop = "subpopulation", subsample = "subsample", w = "rswt_nat", wres = "rswt_res",
              wpn = NA, age = "qa1age", birthy = "qa1y_best", male = "gender", eth = "qa5code",
              curhk = "qa2", hk3 = "qa302", lang = "qd2", clm = "ks3", rating = "qz205",
              wordtest = "wordtest", enrol = "qd3",
              edu = c("cfps2010edu_best", "cfps2010edu", "edu2010_t1_best", "edu2010",
                      "cfps2010edu_*", "edu2010_*", "qc1"),
              sch = c("cfps2010sch_best", "cfps2010sch", "sch2010", "cfps2010sch_*", "sch2010_*"),
              eduy = c("cfps2010eduy_best", "cfps2010eduy", "eduy2010", "cfps2010eduy_*",
                       "eduy2010_*"), selfrpt = NA,
              c_eth = "wa6code", c_hk3 = "wa3",
              c_age = "wa1age", c_birthy = "wa1y", c_male = "gender", c_prov = "provcd",
              c_cnty = "countyid", c_psu = "countyid", c_subpop = "subpopulation",
              c_subsample = "subsample"),
  "12" = list(fid = "fid12", prov = "provcd", cnty = "countyid", psu = "countyid",
              subpop = "subpopulation", subsample = "subsample", w = "rswt_natcs12",
              wres = "rswt_rescs12", wpn = "rswt_natpn1012", age = "cfps2012_age",
              birthy = "cfps2012_birthy", male = "cfps2012_gender", eth = "qa701code",
              curhk = "qa301", hk3 = "qa503", hk12 = "qa603", lang = "qd201", clm = "ks3m",
              rating = "qz205", wordtest = NA, enrol = "wc01", edu = c("edu2012", "cfps2012edu"),
              sch = "sch2012", eduy = "eduy2012", selfrpt = NA,
              c_eth = c("wa6code", "cfps_minzu"), c_hk3 = "wa3"),
  "14" = list(fid = "fid14", prov = "provcd14", cnty = "countyid14", psu = "countyid14",
              subpop = "subpopulation10", subsample = "subsample10", w = "rswt_natcs14",
              wres = "rswt_rescs14", wpn = NA, age = "cfps2014_age", birthy = "cfps_birthy",
              male = "cfps_gender", eth = "qa701code", curhk = "qa301", hk3 = "qa503",
              lang = NA, clm = "ks3m", rating = "qz205", wordtest = "wordtest14", enrol = "wc01",
              edu = "cfps2014edu", sch = "cfps2014sch", eduy = "cfps2014eduy_im",
              selfrpt = "selfrpt", c_eth = "wa6code", c_hk3 = "wa3"),
  "16" = list(fid = "fid16", prov = "provcd16", cnty = "countyid16", psu = "countyid16",
              subpop = "subpopulation", subsample = "subsample", w = "rswt_natcs16",
              wres = "rswt_rescs16", wpn = NA, age = "cfps_age", birthy = "cfps_birthy",
              male = "cfps_gender", eth = "pa701code", curhk = "pa301", hk3 = "pa503",
              lang = NA, clm = "ks3m", rating = "qz205", wordtest = NA, enrol = "school",
              edu = "cfps2016edu", sch = "cfps2016sch", eduy = "cfps2016eduy_im",
              selfrpt = "selfrpt", c_eth = "pa701code", c_hk3 = "pa503"),
  "18" = list(fid = "fid18", prov = "provcd18", cnty = "countyid18", psu = "psu",
              subpop = "subpopulation", subsample = "subsample", w = "rswt_natcs18n",
              wres = NA, wpn = "rswt_natpn1018n", age = "age", birthy = "ibirthy", male = "gender",
              eth = c("minzu", "qa701code"), curhk = "qa301", hk3 = NA, hk12 = "qa603",
              lang = NA, clm = "qs3m", rating = NA, wordtest = "wordtest18", enrol = "school",
              edu = "cfps2018edu", sch = "cfps2018sch", eduy = "cfps2018eduy_im",
              selfrpt = "selfrpt", c_eth = c("wa701code", "minzu"), c_hk3 = "wa503",
              c_psu = "psu", c_subpop = "subpopulation"))


#------------------------------------------------------------------------------
# Small helpers
#------------------------------------------------------------------------------

# Count with thousands separators, so that no long run of digits appears
fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")

# Count of respondents for a log, <10 for a count from 1 to 9
fmt_c <- function(x) ifelse(!is.na(x) & x >= 1 & x < MINCELL, paste0("<", MINCELL), fmt_n(x))

# Percentage with one decimal
fmt_p <- function(x) sprintf("%.1f", x)

# TRUE where x is a number from lo to hi
inr <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi

# x where x is from lo to hi, and NA elsewhere
valid_rng <- function(x, lo, hi) {
  x[!inr(x, lo, hi)] <- NA
  x
}

# A count as shown in the output, <10 for a count from 1 to 9
show_n <- function(x) {
  ifelse(is.na(x), "", ifelse(x >= 1 & x < MINCELL, paste0("<", MINCELL), sprintf("%.0f", x)))
}

# A message from R with any run of nine or more digits replaced
mask_digits <- function(s) gsub("[0-9]{9,}", "<long number>", s)

# TRUE where x is in the set s, and NA where x is NA
in_set <- function(x, s) ifelse(is.na(x), NA, x %in% s)

# The notes that are not empty, joined by semicolons
join_notes <- function(...) {
  x <- c(...)
  paste(x[nzchar(x)], collapse = "; ")
}

# Value of the first listed variable that is valid, record by record
coalesce_valid <- function(d, v, ok) {
  out <- rep(NA_real_, nrow(d))
  for (x in v[!is.na(v)]) {
    if (!x %in% names(d)) next
    y <- d[[x]]
    take <- is.na(out) & ok(y)
    out[take] <- y[take]
  }
  out
}

# The first listed variable with any valid value, for the whole file. A
# name with * stands for the names of the file that it matches, in
# alphabetical order.
first_var <- function(d, v, ok) {
  for (x in v[!is.na(v)]) {
    cand <- if (grepl("*", x, fixed = TRUE)) sort(grep(utils::glob2rx(x), names(d), value = TRUE)) else x
    for (y in intersect(cand, names(d))) {
      if (any(ok(d[[y]]))) return(list(x = d[[y]], name = y))
    }
  }
  list(x = rep(NA_real_, nrow(d)), name = "none")
}

# Column of d named by the wave map, or an empty column
col_of <- function(d, name) {
  if (length(name) == 0 || is.na(name[1]) || !name[1] %in% names(d)) rep(NA_real_, nrow(d)) else d[[name[1]]]
}

# Names of codes from the value labels kept by read_file
label_lookup <- function(labs, vars) {
  out <- character(0)
  for (l in labs) {
    for (v in intersect(vars, names(l))) {
      x <- l[[v]]
      cd <- as.character(as.numeric(unclass(x)))
      new <- !cd %in% names(out) & !duplicated(cd)
      out[cd[new]] <- names(x)[new]
    }
  }
  out
}


#------------------------------------------------------------------------------
# Reading the files
#------------------------------------------------------------------------------

# Reads the variables AV[[code]] of one file, keeps the value labels of the
# variables in LABEL_VARS, changes every column into plain numbers and adds an
# empty column for each variable not in the file.
read_file <- function(code) {
  f <- F_FILE[[code]]
  if (!nzchar(f) || !file.exists(f)) {
    stop("The file ", code, " was not found. The log 00_setup.log lists the files found.")
  }
  u <- cfps_use(f, AV[[code]])
  if (is.null(u$data)) stop("The file ", code, " holds none of the variables needed.")
  d <- u$data
  labs <- list()
  for (v in intersect(names(d), LABEL_VARS)) {
    l <- attr(d[[v]], "labels", exact = TRUE)
    if (!is.null(l)) labs[[v]] <- l
  }
  d <- data.frame(lapply(d, num), check.names = FALSE)
  for (v in setdiff(grep("*", AV[[code]], fixed = TRUE, value = TRUE, invert = TRUE), names(d))) {
    d[[v]] <- NA_real_
  }
  cat(sprintf("%-4s records %s, variables read %d, not in the file: %s\n", code, fmt_n(u$n),
              length(u$found), if (length(u$absent)) paste(u$absent, collapse = ", ") else "none"))
  if (length(u$clash)) cat("     names in the file in two letter cases: ", paste(u$clash, collapse = ", "), "\n", sep = "")
  list(d = d, labels = labs)
}

# Drops the records with a missing or invalid ID, and keeps one record for
# each ID, the record with the most valid values.
clean_ids <- function(d, id, code) {
  bad <- is.na(d[[id]]) | d[[id]] <= 0
  d <- d[!bad, , drop = FALSE]
  nvalid <- rowSums(!is.na(d) & d >= 0)
  d <- d[order(d[[id]], -nvalid), , drop = FALSE]
  dup <- duplicated(d[[id]])
  d <- d[!dup, , drop = FALSE]
  rownames(d) <- NULL
  cat(sprintf("%-4s %s: records with a missing or invalid ID dropped %s, duplicate records dropped %s\n",
              code, id, fmt_c(sum(bad)), fmt_c(sum(dup))))
  d
}


#------------------------------------------------------------------------------
# Output tables
#------------------------------------------------------------------------------
AME_COLS <- c("sample", "model", "outcome", "target", "group", "estimate", "se", "ci_low",
              "ci_high", "p", "df", "n", "n_nonhan", "m", "merge_note")
COEF_COLS <- c("sample", "model", "outcome", "term", "estimate", "se", "ci_low", "ci_high", "p",
               "df", "n", "n_nonhan", "m")
SCHEMA <- list(
  T1_describe = c("sample", "group", "variable", "level", "estimate", "se", "n_shown"),
  F1_transitions = c("source", "cohort", "group", "transition", "estimate", "se", "n_shown"),
  S1_flow = c("sample", "step", "group", "n_shown"),
  S2_groups = c("sample", "ethcode", "name", "n_shown", "pct"),
  S2_provinces = c("sample", "prov_code", "prov_name", "group", "n_shown"),
  S9_attrition = c("group", "variable", "status", "estimate", "se", "n_shown"),
  S10_classmates = c("wave", "item", "group", "level", "estimate", "se", "n_shown"),
  S11_cohorts = c("cohort", "source", "group", "variable", "estimate", "se", "n_shown"),
  T2_ame_main = AME_COLS, T3_ame_language = AME_COLS, T4_bcohort = AME_COLS,
  S4_transitions = AME_COLS, S5_ame_other = AME_COLS, S6_ame_enrol = AME_COLS,
  S3_coef_A = COEF_COLS, S3_coef_Aother = COEF_COLS, S3_coef_C = COEF_COLS, S3_coef_B = COEF_COLS,
  T4_khb = c("sample", "step", "outcome", "group", "quantity", "estimate", "se", "ci_low",
             "ci_high", "p", "df", "n", "n_nonhan", "m"),
  S7_robust = c("check", "variant", AME_COLS),
  S7_oaxaca = c("sample", "component", "estimate", "se", "ci_low", "ci_high", "p", "df", "pct",
                "n", "n_nonhan", "m"),
  S7_r10_selection = c("sample", "step", "term", "wald", "df", "p", "decision"),
  S8_imputation = c("sample", "variable", "group", "n", "n_missing_shown", "pct_missing",
                    "mean_observed", "mean_imputed", "logged_events", "m"))

# Writes one table into the folder Output, with the columns of SCHEMA, numbers
# with six significant digits and empty cells for missing values.
write_out <- function(name, d) {
  cols <- SCHEMA[[name]]
  if (is.null(cols)) stop("The table ", name, " is not in SCHEMA.")
  if (is.null(d) || nrow(d) == 0) {
    d <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols))
  }
  miss <- setdiff(cols, names(d))
  if (length(miss)) stop("The table ", name, " lacks the columns ", paste(miss, collapse = ", "))
  d <- d[, cols, drop = FALSE]
  for (v in cols) {
    x <- d[[v]]
    if (is.numeric(x)) {
      s <- trimws(formatC(x, digits = 6, format = "g"))
      s[is.na(x)] <- ""
    } else {
      s <- as.character(x)
      s[is.na(s)] <- ""
    }
    d[[v]] <- s
  }
  if (nrow(d) > 5000) stop("The table ", name, " has more than 5,000 rows.")
  f <- file.path(OUT, paste0(name, ".csv"))
  readr::write_excel_csv(d, f, na = "")
  if (file.size(f) > 2e6) stop("The table ", name, " is larger than 2 MB.")
  cat(sprintf("Wrote %s.csv, %s rows\n", name, fmt_n(nrow(d))))
  invisible(NULL)
}


#------------------------------------------------------------------------------
# Survey designs
#------------------------------------------------------------------------------

# Design for the rows dom of the frame f. The design is set up on every row
# of the frame with a positive weight, so that each stratum keeps all its
# PSUs, and is then restricted to the rows dom (domain estimation). The
# weight .w has mean 1 over those rows, or is 1 throughout for check R1.
dom_design <- function(f, dom, wcol = "w", equal = FALSE) {
  base <- !is.na(f[[wcol]]) & f[[wcol]] > 0
  b <- f[base, , drop = FALSE]
  b$.w <- if (equal) 1 else b[[wcol]] / mean(b[[wcol]])
  des <- svydesign(ids = ~psu, strata = ~strata, weights = ~.w, nest = TRUE, data = b)
  keep <- dom[base]
  keep[is.na(keep)] <- FALSE
  des <- des[keep, ]
  v <- des$variables
  rownames(v) <- NULL
  des$variables <- v
  des
}

# Weighted mean of x in the rows keep of the design des, with NA values of x
# left out, and its standard error, as a list(est, se, n). x and keep run
# along the rows of des.
est_mean <- function(des, keep, x) {
  ok <- keep & !is.na(x)
  ok[is.na(ok)] <- FALSE
  if (sum(ok) == 0) return(list(est = NA_real_, se = NA_real_, n = 0))
  d2 <- des[ok, ]
  d2$variables$.x <- x[ok]
  r <- svymean(~.x, d2)
  list(est = unname(coef(r)), se = unname(SE(r)), n = sum(ok))
}

# Code as text without an exponent, for PSU labels
code_str <- function(x) sprintf("%.0f", x)


#------------------------------------------------------------------------------
# Model columns
#------------------------------------------------------------------------------
TERM_ORDER <- c("nonhan", "agri3", "nh_ag", "male", "cohort", "paredu", "prov", "edu", "lang",
                "rating", "wordtest", "linc", "age", "wave")
TERMS <- list(
  M1 = c("nonhan", "cohort", "male"),
  M2 = c("nonhan", "agri3", "nh_ag", "cohort", "male"),
  M3 = c("nonhan", "agri3", "nh_ag", "cohort", "male", "paredu"),
  M4 = c("nonhan", "agri3", "nh_ag", "cohort", "male", "paredu", "prov"))
TERMS_B <- c("nonhan", "agri3", "nh_ag", "male", "paredu", "linc", "prov", "age")
TERMS_BE <- c("nonhan", "agri3", "nh_ag", "male", "paredu", "prov", "linc", "age", "wave")
SRC_DEFAULT <- list(prov = "prov", age = "age", linc = "linc", lang = "lang", rating = "rating",
                    wordtest = "wordtest", age_ref = 16)
SRC_B <- list(prov = "prov10", age = "age10", linc = "linc10", lang = "lang10",
              rating = "rating10", wordtest = "wordtest10", age_ref = 12)
KEYS <- c("nonhan", "agri3", "nh_ag")

# Birth cohort, five-year groups from 1940 and one group before 1940
cohort_of <- function(by) ifelse(by < 1940, 1935, 1940 + 5 * floor((by - 1940) / 5))

# The lowest and the highest outcome category of each row, after the
# categories of an ordered outcome are merged, for the checks of separation.
# The merged categories are those of the job (job$ms) when it has them.
# NULL for the multinomial and the linear models.
extremes <- function(job, v) {
  y <- v[[job$y]]
  if (job$type == "olr") {
    ms <- if (is.null(job$ms)) merge_sets(y, v$nonhan, job$codes) else job$ms
    yi <- ms$set_of(y)
    return(list(lowest = yi %in% 1, highest = yi %in% ms$k))
  }
  if (job$type == "logit") return(list(lowest = y %in% 0, highest = y %in% 1))
  NULL
}

# TRUE when the rows hold respondents and all of them are in the lowest, or
# all in the highest, outcome category (separation). The coefficient of an
# indicator of such rows has no finite estimate.
separated <- function(rows, ext) {
  rows <- rows %in% TRUE
  !is.null(ext) && any(rows) && (all(ext$lowest[rows]) || all(ext$highest[rows]))
}

# Groups of birth cohorts. A group with fewer than MINPROV respondents, or
# with separation, joins a neighbouring group, the smallest such group first.
# A group at either end joins its only neighbour, and a group in the middle
# joins the smaller neighbour. The first group is the reference.
cohort_groups <- function(co, ext = NULL) {
  g <- as.list(sort(unique(co[!is.na(co)])))
  while (length(g) > 1) {
    n <- vapply(g, function(s) sum(co %in% s), 1)
    bad <- which(n < MINPROV | vapply(g, function(s) separated(co %in% s, ext), TRUE))
    if (!length(bad)) break
    i <- bad[which.min(n[bad])]
    j <- if (i == 1) 2 else if (i == length(g)) i - 1 else if (n[i - 1] <= n[i + 1]) i - 1 else i + 1
    g[[min(i, j)]] <- c(g[[min(i, j)]], g[[max(i, j)]])
    g[[max(i, j)]] <- NULL
  }
  g
}

# Province groups. A province with fewer than MINPROV respondents, or with
# separation, joins the group 0, and the largest group is the reference. With
# by_group, a province joins the group 0 when either ethnic group has fewer
# than MINPROV. When the group 0 itself has separation, it joins the
# reference.
prov_groups <- function(p, nonhan = NULL, ext = NULL) {
  pc <- as.character(p)
  if (is.null(nonhan)) {
    tab <- table(pc)
    small <- names(tab)[tab < MINPROV]
  } else {
    th <- table(factor(pc[nonhan == 0], levels = unique(pc)))
    tn <- table(factor(pc[nonhan == 1], levels = unique(pc)))
    small <- names(th)[pmin(th, tn[names(th)]) < MINPROV]
  }
  sep <- Filter(function(x) separated(pc == x, ext), setdiff(sort(unique(pc)), small))
  small <- c(small, sep)
  g <- ifelse(pc %in% small, "0", pc)
  gt <- sort(table(g), decreasing = TRUE)
  ref <- names(gt)[1]
  lv <- setdiff(sort(unique(g)), ref)
  zero_ref <- "0" %in% lv && separated(g == "0", ext)
  list(small = small, ref = ref, levels = setdiff(lv, if (zero_ref) "0"), sep = sep,
       zero_ref = zero_ref)
}

# Notes on the merged birth cohorts and the pooled provinces
lev_note <- function(lev) {
  mg <- Filter(function(s) length(s) > 1, lev$cohort)
  join_notes(
    if (length(mg)) paste0("birth cohorts merged: ", paste(vapply(mg, paste, "", collapse = "+"), collapse = ", ")),
    if (length(lev$prov$sep)) paste0("provinces pooled into the small-province group: ", paste(lev$prov$sep, collapse = ", ")),
    if (isTRUE(lev$prov$zero_ref)) "small-province group pooled with the reference province")
}

# The reason why a model is not estimable when a group of non-Han ethnicity
# and hukou at age 3 has separation, or ""
key_separation <- function(v, terms, ext) {
  if (is.null(ext)) return("")
  eth <- c("Han", "non-Han")
  hk <- c("non-agricultural", "agricultural")
  cells <- list()
  if (all(c("nonhan", "nh_ag") %in% terms)) {
    for (a in 0:1) for (h in 0:1) {
      cells[[paste("the", eth[a + 1], "respondents with", hk[h + 1], "hukou at age 3")]] <- v$nonhan == a & v$agri3 == h
    }
  } else {
    if ("nonhan" %in% terms) for (a in 0:1) cells[[paste("the", eth[a + 1], "respondents")]] <- v$nonhan == a
    if ("agri3" %in% terms) for (h in 0:1) cells[[paste("the respondents with", hk[h + 1], "hukou at age 3")]] <- v$agri3 == h
  }
  bad <- names(cells)[vapply(cells, separated, TRUE, ext)]
  if (length(bad)) paste("the outcome does not vary among", paste(bad, collapse = " or among ")) else ""
}

# The hukou group at age 3 (0 non-agricultural, 1 agricultural) whose
# non-Han respondents have separation, in a model with the interaction where
# no other group of ethnicity and hukou at age 3 has separation, or else NA.
# The non-Han coefficient of that group has no finite estimate. The fit
# tends to the fit without these respondents and without the interaction,
# whose non-Han coefficient is that of the other hukou group.
sep_hukou <- function(v, terms, ext) {
  if (is.null(ext) || !all(c("nonhan", "nh_ag") %in% terms)) return(NA)
  s <- vapply(0:1, function(a) {
    vapply(0:1, function(h) separated(v$nonhan == a & v$agri3 == h, ext), TRUE)
  }, logical(2))
  if (any(s[, 1]) || sum(s[, 2]) != 1) return(NA)
  which(s[, 2]) - 1
}

# Levels of the grouped terms, decided on the fitting rows
make_levels <- function(v, terms, src, prov_by_group = FALSE, ext = NULL) {
  lev <- list()
  if ("cohort" %in% terms) lev$cohort <- cohort_groups(v$cohort, ext)
  if ("prov" %in% terms) {
    lev$prov <- prov_groups(v[[src$prov]], if (prov_by_group) v$nonhan else NULL, ext)
  }
  if ("age" %in% terms) {
    a <- sort(unique(v[[src$age]]))
    ref <- if (src$age_ref %in% a) src$age_ref else min(a)
    lev$age <- c(ref, setdiff(a, ref))
  }
  if ("wave" %in% terms) {
    w <- sort(unique(v$wave))
    ref <- if (10 %in% w) 10 else min(w)
    lev$wave <- c(ref, setdiff(w, ref))
  }
  lev
}

# Adds the model columns to v and returns list(v, cols)
build_cols <- function(v, spec) {
  s <- spec$src
  lev <- spec$lev
  cols <- character(0)
  add <- function(nm, x) {
    v[[nm]] <<- as.numeric(x)
    cols <<- c(cols, nm)
  }
  for (term in TERM_ORDER[TERM_ORDER %in% spec$terms]) {
    switch(term,
      nonhan = add("nonhan", v$nonhan),
      agri3 = add("agri3", v$agri3),
      nh_ag = add("nh_ag", v$nonhan * v$agri3),
      male = add("male", v$male),
      cohort = for (g in lev$cohort[-1]) add(paste0("co_", g[1]), in_set(v$cohort, g)),
      paredu = for (k in 2:5) add(paste0("pe_", k), v$paredu == k),
      prov = {
        pc <- as.character(v[[s$prov]])
        g <- ifelse(pc %in% lev$prov$small, "0", pc)
        for (c in lev$prov$levels) add(paste0("pv_", c), g == c)
      },
      edu = for (k in 2:5) add(paste0("ed_", k), v$edu5 == k),
      lang = {
        add("lang_min", v[[s$lang]] == 2)
        add("lang_dia", v[[s$lang]] == 3)
      },
      rating = add("rating", v[[s$rating]]),
      wordtest = add("wordtest", v[[s$wordtest]]),
      linc = add("linc", v[[s$linc]]),
      age = for (a in lev$age[-1]) add(paste0("ag_", a), v[[s$age]] == a),
      wave = for (w in lev$wave[-1]) add(paste0("wv_", w), v$wave == w))
  }
  list(v = v, cols = cols)
}

# Columns without variation, and columns that are linear combinations of
# earlier columns and the intercept
alias_drop <- function(X) {
  if (ncol(X) == 0) return(character(0))
  nz <- apply(X, 2, function(x) max(x) > min(x))
  keep <- colnames(X)[nz]
  out <- colnames(X)[!nz]
  if (length(keep)) {
    q <- qr(cbind(1, X[, keep, drop = FALSE]), tol = 1e-7)
    ok <- q$pivot[seq_len(q$rank)]
    ok <- ok[ok > 1] - 1
    out <- c(out, setdiff(keep, keep[ok]))
  }
  out
}

# Fills the missing values of v with imputed data set i
apply_imp <- function(v, IMP, i) {
  if (is.null(IMP)) return(v)
  for (var in names(IMP$rows)) {
    k <- match(IMP$rows[[var]], v$.row)
    ok <- !is.na(k)
    v[[var]][k[ok]] <- IMP$vals[[var]][ok, min(i, IMP$m)]
  }
  v
}


#------------------------------------------------------------------------------
# Outcome categories (section 6.7 of the analysis plan)
#------------------------------------------------------------------------------

# Merges the categories of an ordered outcome until every category holds at
# least MINCAT Han and MINCAT non-Han respondents, or two categories remain.
# The category with the smallest count is merged first, with the neighbour
# closer to the middle of the scale, and at the middle with the smaller
# neighbour.
merge_sets <- function(y, nonhan, codes) {
  sets <- as.list(codes)
  counts <- function() {
    sapply(sets, function(s) c(han = sum(y %in% s & nonhan == 0), nh = sum(y %in% s & nonhan == 1)))
  }
  repeat {
    k <- length(sets)
    if (k <= 2) break
    cn <- counts()
    bad <- which(cn["han", ] < MINCAT | cn["nh", ] < MINCAT)
    if (!length(bad)) break
    i <- bad[which.min(pmin(cn["han", bad], cn["nh", bad]))]
    mid <- (k + 1) / 2
    j <- if (i < mid) {
      i + 1
    } else if (i > mid) {
      i - 1
    } else {
      tot <- colSums(cn)
      if (tot[i - 1] <= tot[i + 1]) i - 1 else i + 1
    }
    sets[[min(i, j)]] <- sort(c(sets[[i]], sets[[j]]))
    sets[[max(i, j)]] <- NULL
  }
  merged <- sets[lengths(sets) > 1]
  idx <- rep(seq_along(sets), lengths(sets))
  vals <- unlist(sets)
  list(sets = sets, k = length(sets),
       set_of = function(x) idx[match(x, vals)],
       scores = vapply(sets, mean, 1),
       gnames = paste0("ge", vapply(sets, min, 1))[-1],
       note = paste(vapply(merged, paste, "", collapse = "+"), collapse = "; "))
}


#------------------------------------------------------------------------------
# Predictions of each model type, as a function of the coefficients theta
#------------------------------------------------------------------------------

# Ordered logit (svyolr). P(Y <= k) = plogis(zeta_k - x'beta).
pred_olr <- function(th, X, info) {
  b <- th[seq_len(info$nb)]
  z <- th[-seq_len(info$nb)]
  eta <- drop(X %*% b)
  cum <- plogis(outer(-eta, z, "+"))
  p <- cbind(cum, 1) - cbind(0, cum)
  out <- cbind(1 - cum, p %*% info$scores)
  colnames(out) <- c(info$gnames, "mean")
  out
}

# Logit of the upper of two merged categories
pred_bin <- function(th, X, info) {
  p <- plogis(drop(cbind(1, X) %*% th))
  out <- cbind(p, info$scores[1] + (info$scores[2] - info$scores[1]) * p)
  colnames(out) <- c(info$gnames, "mean")
  out
}

pred_logit <- function(th, X, info) {
  out <- matrix(plogis(drop(cbind(1, X) %*% th)), ncol = 1)
  colnames(out) <- "pr"
  out
}

pred_lin <- function(th, X, info) {
  out <- matrix(drop(cbind(1, X) %*% th), ncol = 1)
  colnames(out) <- "mean"
  out
}

# Coefficient matrix of a VGAM model, one row for the intercept and for each
# column, one column for each linear predictor
theta_to_B <- function(th, H) {
  k <- 0
  B <- NULL
  for (nm in names(H)) {
    q <- ncol(H[[nm]])
    B <- rbind(B, drop(H[[nm]] %*% th[k + seq_len(q)]))
    k <- k + q
  }
  B
}

# Multinomial logit with Chinese dialect (level 3) as the comparison category
pred_mn <- function(th, X, info) {
  eta <- cbind(1, X) %*% theta_to_B(th, info$H)
  pr <- 1 / (1 + rowSums(exp(eta)))
  out <- cbind(exp(eta) * pr, pr)
  colnames(out) <- c("pr_mandarin", "pr_minority", "pr_dialect")
  out
}

# Cumulative logit (VGAM), P(Y <= j) = plogis(eta_j)
pred_cum <- function(th, X, info) {
  cum <- plogis(cbind(1, X) %*% theta_to_B(th, info$H))
  p <- cbind(cum, 1) - cbind(0, cum)
  out <- cbind(1 - cum, p %*% info$scores)
  colnames(out) <- c(info$gnames, "mean")
  out
}


#------------------------------------------------------------------------------
# Model fits
#------------------------------------------------------------------------------

# Fits one model of the given type with the columns cols on the design des,
# whose variables hold the outcome yf. The result holds the coefficients
# theta, their design-based variance V, the columns xcols of the model and
# the prediction function.
fit_one <- function(type, des, cols, ms = NULL) {
  rhs <- if (length(cols)) paste(cols, collapse = " + ") else "1"
  fml <- as.formula(paste("yf ~", rhs))
  if (type == "olr") {
    fit <- svyolr(fml, design = des)
    th <- coef(fit)
    th <- th[!is.na(th)]
    V <- vcov(fit)[names(th), names(th), drop = FALSE]
    isz <- grepl("|", names(th), fixed = TRUE)
    return(list(type = type, theta = th, V = V, xcols = names(th)[!isz], pred = pred_olr,
                info = list(nb = sum(!isz), scores = ms$scores, gnames = ms$gnames), fit = fit))
  }
  if (type %in% c("bin", "logit", "linear")) {
    fam <- if (type == "linear") gaussian() else quasibinomial()
    fit <- svyglm(fml, design = des, family = fam)
    th <- coef(fit)
    th <- th[!is.na(th)]
    V <- vcov(fit)[names(th), names(th), drop = FALSE]
    pred <- switch(type, bin = pred_bin, logit = pred_logit, linear = pred_lin)
    return(list(type = type, theta = th, V = V, xcols = setdiff(names(th), "(Intercept)"),
                pred = pred, info = list(scores = ms$scores, gnames = ms$gnames), fit = fit))
  }
  if (type == "mn") {
    fit <- svyVGAM::svy_vglm(fml, family = VGAM::multinomial(refLevel = 3), design = des)
    th <- coef(fit)
    V <- vcov(fit)
    H <- VGAM::constraints(fit$fit, type = "lm")
    if (anyNA(th)) stop("the multinomial model has coefficients that are not estimable")
    return(list(type = type, theta = th, V = V, xcols = names(H)[-1], pred = pred_mn,
                info = list(H = H), fit = fit))
  }
  stop("unknown model type ", type)
}

# Cumulative logit with the constraint matrices cl (check R10). Other
# arguments, such as the starting values coefstart, go to vglm. The fit
# stops, and counts as not fitted, when it has not converged after MAXIT_CUM
# iterations, when a fitted category probability of a fitting row is not
# above 1e-8 (the cumulative logits cross, or meet, inside the data), or when
# a variance is not positive. The warnings still go to the log.
MAXIT_CUM <- 100
fit_cum <- function(des, cols, cl, ms, ...) {
  fml <- as.formula(paste("yf ~", paste(cols, collapse = " + ")))
  noconv <- FALSE
  fit <- withCallingHandlers(
    svyVGAM::svy_vglm(fml, family = VGAM::cumulative(parallel = FALSE), design = des,
                      constraints = cl, maxit = MAXIT_CUM, ...),
    warning = function(w) {
      if (grepl("convergence not obtained", conditionMessage(w), fixed = TRUE)) noconv <<- TRUE
    })
  if (noconv) stop("the cumulative model did not converge in ", MAXIT_CUM, " iterations")
  th <- coef(fit)
  if (anyNA(th)) stop("the cumulative model has coefficients that are not estimable")
  H <- VGAM::constraints(fit$fit, type = "lm")
  X <- as.matrix(des$variables[, names(H)[-1], drop = FALSE])
  cum <- plogis(cbind(1, X) %*% theta_to_B(th, H))
  if (!(min(cbind(cum, 1) - cbind(0, cum)) > 1e-8)) {
    stop("the fitted cumulative logits of the model cross or meet")
  }
  V <- vcov(fit)
  if (!all(is.finite(V)) || !all(diag(V) > 0)) stop("the cumulative model has variances that are not positive")
  list(type = "cum", theta = th, V = V, xcols = names(H)[-1], pred = pred_cum,
       info = list(H = H, scores = ms$scores, gnames = ms$gnames), fit = fit)
}


#------------------------------------------------------------------------------
# Average marginal effects, the delta method and Rubin's rules
#------------------------------------------------------------------------------

# Numerical Jacobian of f at th by central differences
num_jac <- function(f, th) {
  f0 <- f(th)
  J <- matrix(0, length(f0), length(th))
  for (k in seq_along(th)) {
    h <- 1e-5 * max(1, abs(th[k]))
    tp <- th
    tm <- th
    tp[k] <- th[k] + h
    tm[k] <- th[k] - h
    J[, k] <- (f(tp) - f(tm)) / (2 * h)
  }
  J
}

AME_GROUPS <- c("all", "agricultural", "non-agricultural", "difference")

# Average marginal effect of non-Han on each prediction, for all
# respondents, for each hukou group and for the difference between the two
# hukou groups. Non-Han is set to 1 and to 0 for every respondent, with the
# interaction set to hukou at age 3 and to 0. The variance comes from the
# delta method and treats the covariates as fixed.
ame_core <- function(fo, X, w, agri) {
  X1 <- X
  X0 <- X
  X1[, "nonhan"] <- 1
  X0[, "nonhan"] <- 0
  if ("nh_ag" %in% colnames(X)) {
    X1[, "nh_ag"] <- agri
    X0[, "nh_ag"] <- 0
  }
  G <- list(agri >= 0, agri == 1, agri == 0)
  gfun <- function(th) {
    D <- fo$pred(th, X1, fo$info) - fo$pred(th, X0, fo$info)
    out <- t(vapply(G, function(r) {
      if (!any(r)) rep(NA_real_, ncol(D)) else colSums(D[r, , drop = FALSE] * w[r]) / sum(w[r])
    }, numeric(ncol(D))))
    if (ncol(D) == 1) out <- matrix(out, ncol = 1)
    out <- rbind(out, out[2, ] - out[3, ])
    v <- as.vector(t(out))
    names(v) <- paste(rep(colnames(D), 4), rep(AME_GROUPS, each = ncol(D)), sep = "\r")
    v
  }
  est <- gfun(fo$theta)
  J <- num_jac(gfun, fo$theta)
  J[is.na(J)] <- 0
  V <- J %*% fo$V %*% t(J)
  list(est = est, var = diag(V))
}

# Rubin's rules for the rows of Q (estimates) and U (variances), one row for
# each imputed data set, with the degrees of freedom of Rubin (1987) as in
# the R package mitools
rubin <- function(Q, U) {
  Q <- as.matrix(Q)
  U <- as.matrix(U)
  m <- nrow(Q)
  qbar <- colMeans(Q)
  ubar <- colMeans(U)
  if (m == 1) return(list(est = qbar, se = sqrt(ubar), df = rep(Inf, length(qbar))))
  B <- apply(Q, 2, var)
  tt <- ubar + (1 + 1 / m) * B
  r <- (1 + 1 / m) * B / ubar
  df <- (m - 1) * (1 + 1 / r)^2
  df[!is.finite(df)] <- Inf
  list(est = qbar, se = sqrt(tt), df = df)
}

# Confidence interval and p value from an estimate, its standard error and
# its degrees of freedom
ci_p <- function(est, se, df) {
  tq <- qt(0.975, df)
  list(lo = est - tq * se, hi = est + tq * se, p = 2 * pt(-abs(est / se), df))
}

# Respondents and non-Han respondents in each group of AME_GROUPS. For the
# difference, n is the total and n_nonhan the smaller non-Han count of the
# two hukou groups.
group_counts <- function(v) {
  nh <- v$nonhan == 1
  a <- v$agri3
  n <- c(nrow(v), sum(a == 1), sum(a == 0), nrow(v))
  h <- c(sum(nh), sum(nh & a == 1), sum(nh & a == 0), min(sum(nh & a == 1), sum(nh & a == 0)))
  list(n = setNames(n, AME_GROUPS), nh = setNames(h, AME_GROUPS))
}

# The counts n and n_nonhan of a table with the groups of AME_GROUPS as
# shown in the output, with a count from 1 to 9 shown as <10 (section 9 of
# the analysis plan). When the count of one hukou group is hidden, the count
# of the other hukou group with the same key is hidden too, and shown as
# suppressed, so that the hidden count cannot be found from the count of
# all respondents.
hide_counts <- function(d, key) {
  hk <- d$group %in% c("agricultural", "non-agricultural")
  d$.set <- paste(key, ifelse(hk, "hukou", d$group), sep = "\r")
  for (v in c("n", "n_nonhan")) {
    d <- suppress_small(d, v, groups = list(".set"))
    names(d)[names(d) == paste0(v, "_shown")] <- v
  }
  d$.set <- NULL
  d
}

# Rows of an AME table from the pooled estimates
ame_rows <- function(job, pooled, gc, m, note) {
  sp <- strsplit(names(pooled$est), "\r", fixed = TRUE)
  target <- vapply(sp, `[`, "", 1)
  group <- vapply(sp, `[`, "", 2)
  cp <- ci_p(pooled$est, pooled$se, pooled$df)
  d <- data.frame(sample = job$sample, model = job$model, outcome = job$outcome, target = target,
                  group = group, estimate = pooled$est, se = pooled$se, ci_low = cp$lo,
                  ci_high = cp$hi, p = cp$p, df = pooled$df, n = unname(gc$n[group]),
                  n_nonhan = unname(gc$nh[group]), m = m, merge_note = note,
                  stringsAsFactors = FALSE)
  small <- d$n_nonhan < MINCELL
  d[small, c("estimate", "se", "ci_low", "ci_high", "p", "df")] <- NA
  d <- hide_counts(d, d$target)
  rownames(d) <- NULL
  d
}

# Rows of a coefficient table from the pooled estimates
coef_rows <- function(job, pooled, gc, m) {
  term <- names(pooled$est)
  term <- sub(":1$", ":Mandarin", sub(":2$", ":Minority", term))
  if (job$type != "mn") term <- names(pooled$est)
  cp <- ci_p(pooled$est, pooled$se, pooled$df)
  d <- data.frame(sample = job$sample, model = job$model, outcome = job$outcome, term = term,
                  estimate = pooled$est, se = pooled$se, ci_low = cp$lo, ci_high = cp$hi,
                  p = cp$p, df = pooled$df, n = show_n(gc$n[["all"]]),
                  n_nonhan = show_n(gc$nh[["all"]]), m = m, stringsAsFactors = FALSE)
  if (gc$nh[["all"]] < MINCELL) d[, c("estimate", "se", "ci_low", "ci_high", "p", "df")] <- NA
  rownames(d) <- NULL
  d
}

# One row that marks a model as not estimable, with the reason
not_est <- function(job, why, n = NA, n_nonhan = NA) {
  list(ame = data.frame(sample = job$sample, model = job$model, outcome = job$outcome,
                        target = "not estimable", group = "", estimate = NA_real_,
                        se = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p = NA_real_,
                        df = NA_real_, n = show_n(n), n_nonhan = show_n(n_nonhan), m = NA_real_,
                        merge_note = why, stringsAsFactors = FALSE),
       coef = NULL, note = why)
}


#------------------------------------------------------------------------------
# Influence functions, for the decompositions
#------------------------------------------------------------------------------

# Influence functions of the coefficients of an ordered logit fitted by
# svyolr, one row for each respondent of the design, with the weights
# included. Checked against small changes of the weights of one PSU.
if_olr <- function(fit, X, y, w) {
  cf <- coef(fit)
  isz <- grepl("|", names(cf), fixed = TRUE)
  beta <- cf[!isz]
  zeta <- cf[isz]
  X <- X[, names(beta), drop = FALSE]
  eta <- drop(X %*% beta)
  zU <- c(zeta, Inf)[y]
  zL <- c(-Inf, zeta)[y]
  fU <- dlogis(zU - eta)
  fL <- dlogis(zL - eta)
  p <- plogis(zU - eta) - plogis(zL - eta)
  S <- cbind(-X * ((fU - fL) / p),
             vapply(seq_along(zeta), function(k) ((y == k) * fU - (y == k + 1) * fL) / p,
                    numeric(length(y)))) * w
  IF <- S %*% solve(fit$Hessian[names(cf), names(cf)])
  colnames(IF) <- names(cf)
  IF
}

# Influence functions of the coefficients of a model fitted by svyglm
if_glm <- function(g) {
  IF <- (model.matrix(g) * resid(g, "working") * g$weights) %*% summary.glm(g)$cov.unscaled
  colnames(IF) <- names(coef(g))
  IF
}

# Weighted least squares of y on cbind(1, X), with the influence functions
wls_if <- function(X, y, w, rows = rep(TRUE, length(y))) {
  Z <- cbind(1, X)
  Zr <- Z[rows, , drop = FALSE]
  wr <- w[rows]
  A <- solve(crossprod(Zr, Zr * wr))
  b <- drop(A %*% crossprod(Zr, y[rows] * wr))
  e <- rep(0, length(y))
  e[rows] <- y[rows] - drop(Zr %*% b)
  IF <- (Z * (w * e * rows)) %*% A
  names(b) <- c("(Intercept)", colnames(X))
  colnames(IF) <- names(b)
  list(b = b, IF = IF)
}

# Design-based variance of the totals of the columns of IF
if_var <- function(IF, des) {
  as.matrix(svyrecvar(IF, des$cluster, des$strata, des$fpc, postStrata = des$postStrata))
}


#------------------------------------------------------------------------------
# Running jobs in parallel
#------------------------------------------------------------------------------

# Runs fun on each job, CORES jobs at a time. A job that stops returns a
# job_error with the reason. The log gets the name, the time, the warnings
# and the reason of each job, with long runs of digits masked.
run_jobs <- function(jobs, fun) {
  res <- parallel::mclapply(jobs, function(j) {
    warn <- character(0)
    t1 <- proc.time()[["elapsed"]]
    out <- withCallingHandlers(
      tryCatch(fun(j), error = function(e) structure(list(msg = conditionMessage(e)), class = "job_error")),
      warning = function(w) {
        warn <<- c(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage"))
    list(out = out, warnings = unique(warn), secs = proc.time()[["elapsed"]] - t1)
  }, mc.cores = CORES, mc.preschedule = FALSE)
  out <- vector("list", length(jobs))
  for (i in seq_along(jobs)) {
    r <- res[[i]]
    nm <- jobs[[i]]$name
    if (is.null(r) || inherits(r, "try-error") || is.null(r$out)) {
      cat(nm, ", the job process ended without a result\n", sep = "")
      out[[i]] <- structure(list(msg = "the job process ended without a result"), class = "job_error")
      next
    }
    cat(sprintf("%s, %.0f seconds\n", nm, r$secs))
    for (w in r$warnings) cat("   warning: ", mask_digits(substr(w, 1, 200)), "\n", sep = "")
    if (inherits(r$out, "job_error")) cat("   stopped: ", mask_digits(substr(r$out$msg, 1, 200)), "\n", sep = "")
    out[[i]] <- r$out
  }
  out
}


#------------------------------------------------------------------------------
# Imputation (section 6.6 of the analysis plan)
#------------------------------------------------------------------------------

# Fills the missing values of the variables imp_vars of d by predictive mean
# matching (mice), with M_IMP data sets and the seed SEED. The predictors are
# pred_vars, and the variables in factors enter as categories. The result
# holds the rows and the imputed values of each variable, and the rows of
# Table S8.
impute_sample <- function(name, d, imp_vars, pred_vars, factors = character(0)) {
  rownames(d) <- NULL
  dd <- d[, unique(c(imp_vars, pred_vars)), drop = FALSE]
  for (v in intersect(factors, names(dd))) dd[[v]] <- factor(dd[[v]])
  nmis <- vapply(imp_vars, function(v) sum(is.na(dd[[v]])), 1)
  todo <- imp_vars[nmis > 0]
  bad <- setdiff(names(dd)[vapply(dd, anyNA, TRUE)], imp_vars)
  if (length(bad)) stop("the predictors ", paste(bad, collapse = ", "), " have missing values")
  IMP <- list(m = 1L, rows = list(), vals = list(), logged = 0L)
  if (length(todo)) {
    meth <- setNames(rep("", ncol(dd)), names(dd))
    meth[todo] <- "pmm"
    pm <- matrix(1, ncol(dd), ncol(dd), dimnames = list(names(dd), names(dd)))
    diag(pm) <- 0
    pm[!rownames(pm) %in% todo, ] <- 0
    imp <- mice::mice(dd, m = M_IMP, method = meth, predictorMatrix = pm, seed = SEED,
                      printFlag = FALSE)
    IMP$m <- as.integer(M_IMP)
    IMP$logged <- if (is.null(imp$loggedEvents)) 0L else nrow(imp$loggedEvents)
    ev <- imp$loggedEvents
    if (!is.null(ev) && all(c("dep", "meth", "out") %in% names(ev))) {
      IMP$events <- unique(ev[, c("dep", "meth", "out")])
    }
    for (v in todo) {
      iv <- imp$imp[[v]]
      IMP$rows[[v]] <- d$.row[as.integer(rownames(iv))]
      IMP$vals[[v]] <- as.matrix(iv)
    }
  }
  # Rows of Table S8
  grp <- list(All = rep(TRUE, nrow(d)), Han = d$nonhan == 0, "Non-Han" = d$nonhan == 1)
  s8 <- NULL
  for (v in imp_vars) {
    for (g in names(grp)) {
      r <- grp[[g]]
      x <- d[[v]][r]
      nm <- sum(is.na(x))
      mi <- NA_real_
      if (v %in% names(IMP$rows) && nm > 0) {
        k <- match(d$.row[r][is.na(x)], IMP$rows[[v]])
        mi <- mean(IMP$vals[[v]][k, , drop = FALSE])
      }
      s8 <- rbind(s8, data.frame(sample = name, variable = v, group = g, n = sum(r),
        n_missing = nm, pct_missing = if (sum(r) >= MINCELL) 100 * nm / sum(r) else NA,
        mean_observed = if (sum(!is.na(x)) >= MINCELL) mean(x, na.rm = TRUE) else NA,
        mean_imputed = if (nm >= MINCELL) mi else NA, logged_events = IMP$logged, m = IMP$m,
        stringsAsFactors = FALSE))
    }
  }
  # A count from 1 to 9 is hidden with the percentage and the means of its
  # cell, and so is a second count of the same variable, so that the hidden
  # count cannot be found from the count of all respondents
  s8 <- suppress_small(s8, "n", groups = list("variable"),
                       stats = c("pct_missing", "mean_observed", "mean_imputed"))
  names(s8)[names(s8) == "n_shown"] <- "n"
  IMP$s8 <- suppress_small(s8, "n_missing", groups = list("variable"),
                           stats = c("pct_missing", "mean_imputed"))
  IMP
}


#------------------------------------------------------------------------------
# Model jobs
#------------------------------------------------------------------------------

# A job names the sample, the model, the outcome, the frame and its rows, the
# model type, the outcome column, the terms, the source columns, the imputed
# data sets, the weight and the output tables.
make_job <- function(name, sample, model, outcome, frame, rows, type, y, terms, imp = NULL,
                     src = list(), codes = NULL, wcol = "w", equal = FALSE, table = NULL,
                     coef_table = NULL, check = NULL, variant = NULL, kind = "model", ...) {
  c(list(name = name, sample = sample, model = model, outcome = outcome, frame = frame,
         rows = rows, type = type, y = y, terms = terms, imp = imp, src = src, codes = codes,
         wcol = wcol, equal = equal, table = table, coef_table = coef_table, check = check,
         variant = variant, kind = kind), list(...))
}

# Sets up a job: the design of the job's rows, the imputed data sets, the
# model columns kept and the outcome categories
prepare_job <- function(job, L, IMP, terms = job$terms) {
  f <- L$frames[[job$frame]]
  dom <- seq_len(nrow(f)) %in% job$rows
  des <- dom_design(f, dom, job$wcol, job$equal)
  v0 <- des$variables
  imp <- if (is.null(job$imp)) NULL else IMP[[job$imp]]
  if (!is.null(job$imp) && is.null(imp)) stop("the imputed data sets of ", job$imp, " are missing")
  m <- if (is.null(imp)) 1L else imp$m
  src <- modifyList(SRC_DEFAULT, job$src)
  ext <- extremes(job, v0)
  spec <- list(terms = terms, src = src,
               lev = make_levels(v0, terms, src, isTRUE(job$prov_by_group), ext))
  drop <- character(0)
  for (i in seq_len(m)) {
    b <- build_cols(apply_imp(v0, imp, i), spec)
    X <- as.matrix(b$v[, b$cols, drop = FALSE])
    if (anyNA(X)) stop("model columns with missing values: ", paste(colnames(X)[colSums(is.na(X)) > 0], collapse = ", "))
    drop <- union(drop, alias_drop(X))
  }
  list(des = des, v0 = v0, imp = imp, m = m, spec = spec, cols = setdiff(b$cols, drop),
       drop = drop, keys_dropped = intersect(intersect(KEYS, b$cols), drop), gc = group_counts(v0),
       sep_key = key_separation(v0, terms, ext), sep_h = sep_hukou(v0, terms, ext),
       lev_note = lev_note(spec$lev))
}

# Groups of model columns whose reference rows (all columns of the group 0)
# are checked for separation when an ordered logit does not converge
REF_GROUPS <- c(co_ = "birth cohort", pe_ = "parents' education", pv_ = "province", ag_ = "age",
                wv_ = "wave", ed_ = "education")

# The reason why an ordered logit with the columns cols did not converge.
# The reason names the columns, and the reference levels, whose respondents
# are all in the lowest or all in the highest outcome category, or else the
# columns that are nearly collinear in the fit, or else gives the message of
# R.
olr_failure <- function(v, cols, oc, w, err) {
  msg <- mask_digits(substr(conditionMessage(err), 1, 150))
  tryCatch({
    yi <- as.integer(oc$yf)
    ext <- list(lowest = yi == 1, highest = yi == oc$ms$k)
    X <- as.matrix(v[, cols, drop = FALSE])
    bad <- character(0)
    for (c in cols[vapply(cols, function(c) all(X[, c] %in% 0:1), TRUE)]) {
      if (separated(X[, c] == 1, ext)) bad <- c(bad, c)
      if (separated(X[, c] == 0, ext)) bad <- c(bad, paste(c, "= 0"))
    }
    for (p in names(REF_GROUPS)) {
      gc <- grep(paste0("^", p), cols, value = TRUE)
      if (length(gc) && separated(rowSums(X[, gc, drop = FALSE]) == 0, ext)) {
        bad <- c(setdiff(bad, paste(gc, "= 0")), paste("the reference", REF_GROUPS[[p]]))
      }
    }
    if (length(bad)) {
      return(paste0("the ordered logit did not converge; the outcome does not vary within: ",
                    paste(bad, collapse = ", ")))
    }
    d <- data.frame(yf = oc$yf, X, .wt = w / mean(w), check.names = FALSE)
    fml <- as.formula(paste("yf ~", paste(cols, collapse = " + ")))
    pf <- suppressWarnings(MASS::polr(fml, data = d, weights = .wt, Hess = TRUE))
    e <- eigen(pf$Hessian, symmetric = TRUE)
    small <- which(e$values < 1e-6 * max(e$values))
    near <- unique(unlist(lapply(small, function(k) rownames(pf$Hessian)[abs(e$vectors[, k]) >= 0.3])))
    near <- intersect(near, cols)
    if (length(near)) {
      return(paste0("the ordered logit did not converge; near-collinear in the fit: ",
                    paste(near, collapse = ", ")))
    }
    paste0("the ordered logit did not converge: ", msg)
  }, error = function(e) paste0("the ordered logit did not converge: ", msg))
}

# Outcome column yf for a model type, with the categories merged for an
# ordered outcome (those of job$ms when the job has them). The result holds
# yf, the type to fit and the merge note.
make_outcome <- function(job, v0) {
  y <- v0[[job$y]]
  if (job$type == "olr") {
    ms <- if (is.null(job$ms)) merge_sets(y, v0$nonhan, job$codes) else job$ms
    yi <- ms$set_of(y)
    if (ms$k == 2) return(list(yf = as.numeric(yi == 2), type = "bin", ms = ms, note = ms$note))
    return(list(yf = factor(yi, levels = seq_len(ms$k)), type = "olr", ms = ms, note = ms$note))
  }
  if (job$type == "mn") {
    tab <- table(factor(y, levels = 1:3), factor(v0$nonhan, levels = 0:1))
    low <- which(tab < MINCAT, arr.ind = TRUE)
    if (nrow(low)) {
      lab <- c("Mandarin", "minority language", "Chinese dialect")
      why <- paste0("the category ", lab[low[, 1]], " has fewer than ", MINCAT, " ",
                    c("Han", "non-Han")[low[, 2]], " respondents", collapse = "; ")
      return(list(stop = why))
    }
    return(list(yf = factor(y, levels = 1:3, labels = c("Mandarin", "Minority", "Dialect")),
                type = "mn", ms = NULL, note = ""))
  }
  list(yf = as.numeric(y), type = job$type, ms = NULL, note = "")
}

# Runs one model job on every imputed data set and returns the rows of the
# AME table and of the coefficient table. When the non-Han respondents of
# one hukou group at age 3 have separation (sep_hukou), the model is fitted
# without them and without the interaction, with the outcome categories and
# the counts of all the rows, and only the effect for the other hukou group
# is shown.
run_model <- function(job, L, IMP) {
  pj <- prepare_job(job, L, IMP)
  gc <- pj$gc
  n <- gc$n[["all"]]
  nh <- gc$nh[["all"]]
  if (length(pj$keys_dropped)) {
    return(not_est(job, paste0("no variation or collinear: ", paste(pj$keys_dropped, collapse = ", ")),
                   n, nh))
  }
  sep_h <- NA
  sep_note <- ""
  fit_job <- job
  if (nzchar(pj$sep_key)) {
    if (is.na(pj$sep_h)) return(not_est(job, pj$sep_key, n, nh))
    sep_h <- pj$sep_h
    keep_g <- c("agricultural", "non-agricultural")[sep_h + 1]
    sep_note <- paste0(pj$sep_key, "; the model was fitted without them and without the interaction, ",
                       "and only the effect for the ", keep_g, " hukou group is shown")
    cell <- pj$v0$nonhan == 1 & pj$v0$agri3 == sep_h
    fit_job$rows <- setdiff(job$rows, pj$v0$.row[cell])
    fit_job$terms <- setdiff(job$terms, "nh_ag")
    fit_job$ms <- make_outcome(job, pj$v0)$ms
    pj <- prepare_job(fit_job, L, IMP)
    if (length(pj$keys_dropped)) {
      return(not_est(job, join_notes(sep_note, paste0("no variation or collinear: ",
                                                      paste(pj$keys_dropped, collapse = ", "))), n, nh))
    }
    if (nzchar(pj$sep_key)) return(not_est(job, join_notes(sep_note, pj$sep_key), n, nh))
  }
  oc <- make_outcome(fit_job, pj$v0)
  if (!is.null(oc$stop)) return(not_est(job, oc$stop, n, nh))
  w <- weights(pj$des)
  des <- pj$des
  res <- vector("list", pj$m)
  for (i in seq_len(pj$m)) {
    b <- build_cols(apply_imp(pj$v0, pj$imp, i), pj$spec)
    v <- b$v
    v$yf <- oc$yf
    des$variables <- v
    fo <- tryCatch(fit_one(oc$type, des, pj$cols, oc$ms), error = function(e) e)
    if (inherits(fo, "error")) {
      if (oc$type != "olr") stop(fo)
      return(not_est(job, join_notes(sep_note, olr_failure(v, pj$cols, oc, w, fo)), n, nh))
    }
    X <- as.matrix(v[, fo$xcols, drop = FALSE])
    res[[i]] <- list(theta = fo$theta, V = fo$V, ame = ame_core(fo, X, w, v$agri3))
  }
  nms <- lapply(res, function(r) names(r$theta))
  if (!all(vapply(nms, identical, TRUE, nms[[1]]))) {
    return(not_est(job, join_notes(sep_note, "the models of the imputed data sets have different coefficients"),
                   n, nh))
  }
  pa <- rubin(do.call(rbind, lapply(res, function(r) r$ame$est)),
              do.call(rbind, lapply(res, function(r) r$ame$var)))
  pc <- rubin(do.call(rbind, lapply(res, `[[`, "theta")),
              do.call(rbind, lapply(res, function(r) diag(r$V))))
  note <- join_notes(sep_note, oc$note, if (oc$type == "bin") "two categories, logit", pj$lev_note)
  job$type <- oc$type
  ame <- ame_rows(job, pa, gc, pj$m, note)
  coef <- coef_rows(job, pc, gc, pj$m)
  if (!is.na(sep_h)) {
    ame[ame$group != keep_g, c("estimate", "se", "ci_low", "ci_high", "p", "df")] <- NA
    # without the non-Han respondents of non-agricultural hukou, the non-Han
    # coefficient is that of non-Han plus the interaction
    if (sep_h == 0) coef$term[coef$term == "nonhan"] <- "nonhan + nh_ag"
  }
  list(ame = ame, coef = coef, note = note)
}


#------------------------------------------------------------------------------
# KHB decomposition (section 6.4 of the analysis plan)
#------------------------------------------------------------------------------

# The non-Han coefficient (and its interaction with hukou at age 3) of the
# model with the language measures (full model), plus the sum over the
# measures of the measure's coefficient times the non-Han coefficient of a
# weighted linear regression of the measure on the other terms, gives the
# coefficient of the model without the measures on the scale of the full
# model (Karlson, Holm and Breen 2012). The variance comes from the stacked
# influence functions of all the models.
KHB_GROUPS <- list("non-agricultural" = c(1, 0), agricultural = c(1, 1), difference = c(0, 1))

run_khb <- function(job, L, IMP) {
  pj <- prepare_job(job, L, IMP, terms = c(job$terms, job$meds))
  gc <- pj$gc
  if (length(pj$keys_dropped)) {
    return(list(khb = NULL, note = paste0("no variation or collinear: ", paste(pj$keys_dropped, collapse = ", "))))
  }
  if (nzchar(pj$sep_key)) return(list(khb = NULL, note = pj$sep_key))
  oc <- make_outcome(job, pj$v0)
  w <- weights(pj$des)
  des <- pj$des
  med_all <- c("lang_min", "lang_dia", "rating", "wordtest")
  med <- intersect(med_all, pj$cols)
  ctrl <- setdiff(pj$cols, med)
  Q <- NULL
  U <- NULL
  check <- NA_real_
  for (i in seq_len(pj$m)) {
    b <- build_cols(apply_imp(pj$v0, pj$imp, i), pj$spec)
    v <- b$v
    v$yf <- oc$yf
    des$variables <- v
    fo <- tryCatch(fit_one(oc$type, des, pj$cols, oc$ms), error = function(e) e)
    if (inherits(fo, "error")) {
      if (oc$type != "olr") stop(fo)
      return(list(khb = NULL, note = olr_failure(v, pj$cols, oc, w, fo)))
    }
    X <- as.matrix(v[, pj$cols, drop = FALSE])
    IFf <- if (oc$type == "olr") if_olr(fo$fit, X, as.integer(v$yf), w) else if_glm(fo$fit)
    if (i == 1) {
      se1 <- sqrt(diag(if_var(IFf, des)))
      se0 <- sqrt(diag(fo$V))[colnames(IFf)]
      check <- max(abs(se1 / se0 - 1))
    }
    x2 <- intersect(c("nonhan", "nh_ag"), pj$cols)
    rel <- IFf[, c(x2, med), drop = FALSE]
    th <- fo$theta[c(x2, med)]
    for (z in med) {
      r <- wls_if(X[, ctrl, drop = FALSE], v[[z]], w)
      rel <- cbind(rel, r$IF[, x2, drop = FALSE])
      th <- c(th, r$b[x2])
    }
    Vr <- if_var(rel, des)
    nx <- length(x2)
    qfun <- function(t) {
      bF <- t[seq_len(nx)]
      gam <- t[nx + seq_along(med)]
      del <- matrix(t[nx + length(med) + seq_len(nx * length(med))], nrow = nx)
      out <- NULL
      for (g in names(KHB_GROUPS)) {
        a <- KHB_GROUPS[[g]][seq_len(nx)]
        direct <- sum(a * bF)
        contrib <- gam * drop(crossprod(a, del))
        names(contrib) <- med
        ind <- sum(contrib)
        tot <- direct + ind
        q <- c(total = tot, direct = direct, indirect = ind, pct_indirect = 100 * ind / tot,
               setNames(contrib, paste0("contrib_", med)))
        if (all(c("lang_min", "lang_dia") %in% med)) {
          q <- c(q, contrib_language = unname(contrib["lang_min"] + contrib["lang_dia"]))
        }
        names(q) <- paste(names(q), g, sep = "\r")
        out <- c(out, q)
      }
      out
    }
    est <- qfun(th)
    J <- num_jac(qfun, th)
    Q <- rbind(Q, est)
    U <- rbind(U, diag(J %*% Vr %*% t(J)))
  }
  pq <- rubin(Q, U)
  sp <- strsplit(names(pq$est), "\r", fixed = TRUE)
  group <- vapply(sp, `[`, "", 2)
  cp <- ci_p(pq$est, pq$se, pq$df)
  d <- data.frame(sample = job$sample, step = job$model, outcome = job$outcome, group = group,
                  quantity = vapply(sp, `[`, "", 1), estimate = pq$est, se = pq$se,
                  ci_low = cp$lo, ci_high = cp$hi, p = cp$p, df = pq$df,
                  n = unname(gc$n[group]), n_nonhan = unname(gc$nh[group]), m = pj$m,
                  stringsAsFactors = FALSE)
  d[d$n_nonhan < MINCELL, c("estimate", "se", "ci_low", "ci_high", "p", "df")] <- NA
  d <- hide_counts(d, d$quantity)
  rownames(d) <- NULL
  list(khb = d, note = join_notes(oc$note, pj$lev_note), check = check)
}


#------------------------------------------------------------------------------
# Oaxaca and Blinder decomposition of years of schooling (check R8)
#------------------------------------------------------------------------------
OAX_PARTS <- list(agri3 = "^agri3$", cohort = "^co_", male = "^male$", paredu = "^pe_",
                  province = "^pv_")

run_oaxaca <- function(job, L, IMP) {
  job$prov_by_group <- TRUE
  pj <- prepare_job(job, L, IMP)
  gc <- pj$gc
  w <- weights(pj$des)
  v0 <- pj$v0
  H <- v0$nonhan == 0
  N <- v0$nonhan == 1
  y <- v0[[job$y]]
  # columns without variation, or collinear, in either group are dropped from both
  drop <- character(0)
  for (i in seq_len(pj$m)) {
    b <- build_cols(apply_imp(v0, pj$imp, i), pj$spec)
    X <- as.matrix(b$v[, pj$cols, drop = FALSE])
    drop <- union(drop, c(alias_drop(X[H, , drop = FALSE]), alias_drop(X[N, , drop = FALSE])))
  }
  cols <- setdiff(pj$cols, drop)
  p <- length(cols) + 1
  comp <- function(t) {
    bH <- t[1:p]
    bN <- t[p + 1:p]
    mH <- t[2 * p + 1:p]
    mN <- t[3 * p + 1:p]
    out <- c(total = sum(mH * bH) - sum(mN * bN), explained = sum((mH - mN) * bH),
             unexplained = sum(mN * (bH - bN)))
    nm <- c("(Intercept)", cols)
    for (k in names(OAX_PARTS)) {
      s <- grepl(OAX_PARTS[[k]], nm)
      out[paste0("explained_", k)] <- sum(((mH - mN) * bH)[s])
    }
    out
  }
  Q <- NULL
  U <- NULL
  for (i in seq_len(pj$m)) {
    b <- build_cols(apply_imp(v0, pj$imp, i), pj$spec)
    X <- as.matrix(b$v[, cols, drop = FALSE])
    rH <- wls_if(X, y, w, H)
    rN <- wls_if(X, y, w, N)
    Z <- cbind(1, X)
    mH <- colSums(Z[H, , drop = FALSE] * w[H]) / sum(w[H])
    mN <- colSums(Z[N, , drop = FALSE] * w[N]) / sum(w[N])
    IFmH <- sweep(Z, 2, mH) * (w * H) / sum(w[H])
    IFmN <- sweep(Z, 2, mN) * (w * N) / sum(w[N])
    th <- c(rH$b, rN$b, mH, mN)
    V <- if_var(cbind(rH$IF, rN$IF, IFmH, IFmN), pj$des)
    J <- num_jac(comp, th)
    Q <- rbind(Q, comp(th))
    U <- rbind(U, diag(J %*% V %*% t(J)))
  }
  pq <- rubin(Q, U)
  cp <- ci_p(pq$est, pq$se, pq$df)
  d <- data.frame(sample = job$sample, component = names(pq$est), estimate = pq$est, se = pq$se,
                  ci_low = cp$lo, ci_high = cp$hi, p = cp$p, df = pq$df,
                  pct = 100 * pq$est / pq$est[["total"]], n = show_n(gc$n[["all"]]),
                  n_nonhan = show_n(gc$nh[["all"]]), m = pj$m, stringsAsFactors = FALSE)
  if (gc$nh[["all"]] < MINCELL) d[, c("estimate", "se", "ci_low", "ci_high", "p", "df", "pct")] <- NA
  rownames(d) <- NULL
  list(oax = d)
}


#------------------------------------------------------------------------------
# Partial proportional odds model (check R10)
#------------------------------------------------------------------------------
R10_PARTS <- list(nonhan = "^nonhan$", agri3 = "^agri3$", nh_ag = "^nh_ag$", male = "^male$",
                  cohort = "^co_", paredu = "^pe_", province = "^pv_")

run_r10 <- function(job, L, IMP) {
  pj <- prepare_job(job, L, IMP)
  if (length(pj$keys_dropped)) {
    return(c(not_est(job, paste0("no variation or collinear: ", paste(pj$keys_dropped, collapse = ", "))),
             list(sel = NULL)))
  }
  if (nzchar(pj$sep_key)) return(c(not_est(job, pj$sep_key), list(sel = NULL)))
  oc <- make_outcome(job, pj$v0)
  if (oc$type != "olr") return(c(not_est(job, "fewer than three outcome categories"), list(sel = NULL)))
  M <- oc$ms$k - 1
  cols <- pj$cols
  parts <- lapply(R10_PARTS, function(p) grep(p, cols, value = TRUE))
  parts <- parts[lengths(parts) > 0]
  par <- setNames(rep(FALSE, length(parts)), names(parts))
  w <- weights(pj$des)
  des <- pj$des
  yi <- as.integer(oc$yf)
  data_i <- function(i) {
    b <- build_cols(apply_imp(pj$v0, pj$imp, i), pj$spec)
    v <- b$v
    v$yf <- ordered(yi, levels = seq_len(oc$ms$k))
    v
  }
  cons <- function(par) {
    cl <- list("(Intercept)" = diag(M))
    for (g in names(parts)) for (c in parts[[g]]) cl[[c]] <- if (par[[g]]) matrix(1, M, 1) else diag(M)
    cl
  }
  # Starting values for the constraints of par, from the earlier fit fo
  start_at <- function(fo, par) {
    if (is.null(fo)) return(NULL)
    B <- theta_to_B(fo$theta, fo$info$H)
    rownames(B) <- names(fo$info$H)
    cl <- cons(par)
    unlist(lapply(c("(Intercept)", cols), function(c) if (ncol(cl[[c]]) == 1) mean(B[c, ]) else B[c, ]),
           use.names = FALSE)
  }
  fit_at <- function(v, par, from = NULL) {
    des$variables <- v
    tryCatch(fit_cum(des, cols, cons(par), oc$ms, coefstart = start_at(from, par)),
             error = function(e) e)
  }
  failed <- function(f) inherits(f, "error")
  why <- function(f) mask_digits(substr(conditionMessage(f), 1, 150))
  sel <- NULL
  step0 <- function(terms, decision) {
    sel <<- rbind(sel, data.frame(sample = job$sample, step = 0, term = terms, wald = NA, df = NA,
                                  p = NA, decision = decision))
  }
  # A term with an empty outcome category among the respondents of one of
  # its levels, the reference level included, in any imputed data set, is
  # equal across levels from the start. A free term would have no finite
  # estimate.
  empty <- setNames(rep(FALSE, length(parts)), names(parts))
  for (i in seq_len(pj$m)) {
    vi <- data_i(i)
    for (g in names(parts)[!empty]) {
      Xg <- as.matrix(vi[, parts[[g]], drop = FALSE])
      rows <- c(lapply(parts[[g]], function(c) Xg[, c] == 1), list(rowSums(Xg) == 0))
      empty[[g]] <- any(vapply(rows, function(r) any(tabulate(yi[r], M + 1) == 0), TRUE))
    }
  }
  if (any(empty)) {
    par[empty] <- TRUE
    step0(names(par)[empty], "made equal across levels at the start, because an outcome category is empty among the respondents of one of its levels")
  }
  # The model with every term equal across levels gives the starting values
  allpar <- setNames(rep(TRUE, length(parts)), names(parts))
  v1 <- data_i(1)
  p0 <- fit_at(v1, allpar)
  if (failed(p0)) p0 <- NULL
  fo <- fit_at(v1, par, p0)
  for (fb in list(c("province", "cohort"), c("paredu", "male"))) {
    if (!failed(fo)) break
    fb <- intersect(fb, names(par)[!par])
    if (!length(fb)) next
    par[fb] <- TRUE
    step0(fb, "made equal across levels at the start, because the model with more terms free did not fit")
    fo <- fit_at(v1, par, p0)
  }
  if (failed(fo)) {
    return(c(not_est(job, paste0("the partial proportional odds model did not fit: ", why(fo))),
             list(sel = sel)))
  }
  for (step in 1:7) {
    free <- names(par)[!par]
    if (!length(free)) break
    th <- fo$theta
    V <- fo$V
    tests <- NULL
    for (g in free) {
      D <- NULL
      for (c in parts[[g]]) {
        idx <- match(paste0(c, ":", seq_len(M)), names(th))
        for (j in seq_len(M - 1)) {
          r <- rep(0, length(th))
          r[idx[j]] <- 1
          r[idx[j + 1]] <- -1
          D <- rbind(D, r)
        }
      }
      dv <- D %*% th
      S <- D %*% V %*% t(D)
      Si <- tryCatch(solve(S), error = function(e) MASS::ginv(S))
      wald <- drop(t(dv) %*% Si %*% dv)
      df <- nrow(D)
      tests <- rbind(tests, data.frame(term = g, wald = wald, df = df,
                                       p = pchisq(wald, df, lower.tail = FALSE)))
    }
    cand <- tests$term[tests$p >= 0.05]
    pick <- if (length(cand)) cand[which.max(tests$p[tests$p >= 0.05])] else NA
    tests$decision <- ifelse(tests$term %in% pick, "made equal across levels", "kept free")
    sel <- rbind(sel, data.frame(sample = job$sample, step = step, tests))
    if (is.na(pick)) break
    par[[pick]] <- TRUE
    fo2 <- fit_at(v1, par, fo)
    if (failed(fo2)) {
      par[[pick]] <- FALSE
      sel$decision[nrow(sel) - nrow(tests) + which(tests$term == pick)] <- "kept free, because the model with the term equal across levels did not fit"
      break
    }
    fo <- fo2
  }
  # The final model on each imputed data set, started from the values of the
  # first, and else from the model with every term equal across levels
  res <- vector("list", pj$m)
  for (i in seq_len(pj$m)) {
    v <- if (i == 1) v1 else data_i(i)
    f2 <- if (i == 1) fo else fit_at(v, par, fo)
    if (failed(f2)) {
      q0 <- fit_at(v, allpar)
      f2 <- fit_at(v, par, if (failed(q0)) NULL else q0)
    }
    if (failed(f2)) {
      return(c(not_est(job, paste0("the final model did not fit in an imputed data set: ", why(f2))),
               list(sel = sel)))
    }
    X <- as.matrix(v[, f2$xcols, drop = FALSE])
    res[[i]] <- ame_core(f2, X, w, v$agri3)
  }
  pa <- rubin(do.call(rbind, lapply(res, `[[`, "est")), do.call(rbind, lapply(res, `[[`, "var")))
  free <- names(par)[!par]
  note <- join_notes(oc$note, pj$lev_note, paste0("terms free across levels: ",
                     if (length(free)) paste(free, collapse = ", ") else "none"))
  list(ame = ame_rows(job, pa, pj$gc, pj$m, note), sel = sel, note = note)
}


#------------------------------------------------------------------------------
# Running the jobs and collecting the tables
#------------------------------------------------------------------------------

# Runs one job of any kind
run_job <- function(job, L, IMP) {
  switch(job$kind,
    model = run_model(job, L, IMP),
    khb = run_khb(job, L, IMP),
    oaxaca = run_oaxaca(job, L, IMP),
    r10 = run_r10(job, L, IMP),
    stop("unknown job kind ", job$kind))
}

# Row of the KHB table that marks a decomposition as not estimable
khb_stub <- function(job, why) {
  data.frame(sample = job$sample, step = job$model, outcome = job$outcome, group = "",
             quantity = paste("not estimable:", why), estimate = NA_real_, se = NA_real_,
             ci_low = NA_real_, ci_high = NA_real_, p = NA_real_, df = NA_real_, n = "",
             n_nonhan = "", m = NA_real_, stringsAsFactors = FALSE)
}

# Row of the Oaxaca table that marks a decomposition as not estimable
oax_stub <- function(job, why) {
  data.frame(sample = job$sample, component = paste("not estimable:", why), estimate = NA_real_,
             se = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p = NA_real_, df = NA_real_,
             pct = NA_real_, n = "", n_nonhan = "", m = NA_real_, stringsAsFactors = FALSE)
}

# KHB rows in the columns of the AME tables, for Table S7
khb_as_ame <- function(d) {
  data.frame(sample = d$sample, model = paste("KHB", d$step), outcome = d$outcome,
             target = d$quantity, group = d$group, estimate = d$estimate, se = d$se,
             ci_low = d$ci_low, ci_high = d$ci_high, p = d$p, df = d$df, n = d$n,
             n_nonhan = d$n_nonhan, m = d$m, merge_note = "", stringsAsFactors = FALSE)
}

# Adds the columns check and variant of Table S7
add_check <- function(d, job) {
  if (is.null(job$check) || is.null(d)) return(d)
  cbind(data.frame(check = rep(job$check, nrow(d)), variant = rep(job$variant, nrow(d)),
                   stringsAsFactors = FALSE), d)
}

# Puts the results of the jobs into tables, in the order of the jobs. A job
# that stopped gives a row that marks the model as not estimable, with the
# reason.
collect_results <- function(jobs, res) {
  out <- list()
  put <- function(tab, d) {
    if (!is.null(tab) && !is.null(d) && nrow(d) > 0) out[[tab]] <<- rbind(out[[tab]], d)
  }
  for (i in seq_along(jobs)) {
    j <- jobs[[i]]
    r <- res[[i]]
    err <- if (inherits(r, "job_error")) paste0("stopped, ", mask_digits(substr(r$msg, 1, 150))) else NULL
    if (j$kind == "khb") {
      d <- if (is.null(err) && !is.null(r$khb)) r$khb else khb_stub(j, if (is.null(err)) r$note else err)
      if (!is.null(j$check)) d <- add_check(khb_as_ame(d), j)
      put(j$table, d)
    } else if (j$kind == "oaxaca") {
      put(j$table, if (is.null(err)) r$oax else oax_stub(j, err))
    } else {
      if (!is.null(err)) r <- not_est(j, err)
      put(j$table, add_check(r$ame, j))
      if (!is.null(j$coef_table)) put(j$coef_table, r$coef)
      if (j$kind == "r10") put("S7_r10_selection", r$sel)
    }
  }
  out
}


#------------------------------------------------------------------------------
# Jobs of 04_models.R and 05_checks.R
#------------------------------------------------------------------------------

# Samples of the language outcomes (Table III), with the outcome and the
# model type
C_SAMPLES <- data.frame(
  sample = c("C-lang-2010", "C-lang-2012", "C-rating-2010", "C-rating-2012", "C-rating-2014",
             "C-rating-2016", "C-wordtest-2010", "C-wordtest-2014", "C-wordtest-2018"),
  y = c("lang", "lang", rep("rating", 4), rep("wordtest", 3)),
  type = c("mn", "mn", rep("olr", 4), rep("linear", 3)), stringsAsFactors = FALSE)

# Categories of the ordered outcomes
CODES <- list(edu5 = 1:5, edu6 = 1:6, rating = 1:7, rating0 = 0:7)

# Language measures of the models with language, and the measures added at
# each step of the KHB decomposition
LANG_TERMS <- c("lang", "rating", "wordtest")
KHB_STEPS <- list("step 1" = "lang", "step 2" = LANG_TERMS)

# TRUE where every column vars of the frame f is known
all_known <- function(f, vars) Reduce(`&`, lapply(vars, function(x) !is.na(f[[x]])))

# Job for the sample s of the link file L, on the rows of the sample where
# the outcome y is known and keep(frame) is TRUE. The imputed data sets are
# those of the sample unless imp names others or is NULL. The name of the
# job joins the check, the variant, the sample, the model and the outcome,
# and the other fields go to make_job.
sample_job <- function(L, s, model, y, type, terms, imp = s, keep = NULL, check = NULL,
                       variant = NULL, ...) {
  fr <- L$frame_of[[s]]
  f <- L$frames[[fr]]
  ok <- seq_len(nrow(f)) %in% L$rows[[s]] & !is.na(f[[y]])
  if (!is.null(keep)) {
    k <- keep(f)
    ok <- ok & !is.na(k) & k
  }
  make_job(name = paste(c(check, variant, s, model, y), collapse = ", "), sample = s,
           model = model, outcome = y, frame = fr, rows = which(ok), type = type, y = y,
           terms = terms, imp = imp, codes = CODES[[y]], check = check, variant = variant, ...)
}

# Runs the jobs, the slowest kinds first so that no core waits at the end,
# and returns the results in the order of the jobs
run_all <- function(jobs, L, IMP) {
  cost <- vapply(jobs, function(j) {
    4 * (j$kind %in% c("khb", "r10")) + 3 * (j$type == "mn") + 2 * !is.null(j$imp)
  }, 1)
  ord <- order(-cost, seq_along(jobs))
  cat(sprintf("%s jobs on %d cores, started at %s. The log lists the jobs when all have finished.\n",
              fmt_n(length(jobs)), CORES, format(Sys.time(), "%H.%M")))
  flush.console()
  res <- vector("list", length(jobs))
  res[ord] <- run_jobs(jobs[ord], function(j) run_job(j, L, IMP))
  res
}

# Writes into the log, for each KHB decomposition, how far the standard
# errors of the full model from the influence functions are from those of
# the model fit (first imputed data set)
log_khb_check <- function(jobs, res) {
  for (i in seq_along(jobs)) {
    r <- res[[i]]
    if (jobs[[i]]$kind != "khb" || inherits(r, "job_error") || is.null(r$check)) next
    cat(sprintf("KHB check, %s: standard errors from the influence functions against those of the model fit, largest relative difference %s%%%s\n",
                jobs[[i]]$name, sprintf("%.2f", 100 * r$check),
                if (nzchar(r$note)) paste0("; notes: ", r$note) else ""))
  }
}

# Writes into the log the models of each table that are not estimable, with
# the reason
log_not_estimable <- function(tabs) {
  for (nm in names(tabs)) {
    d <- tabs[[nm]]
    bad <- if ("target" %in% names(d)) d$target == "not estimable" else if ("quantity" %in% names(d)) {
      startsWith(d$quantity, "not estimable")
    } else if ("component" %in% names(d)) startsWith(d$component, "not estimable") else FALSE
    if (!any(bad)) next
    why <- if ("merge_note" %in% names(d)) d$merge_note else if ("quantity" %in% names(d)) d$quantity else d$component
    cat(sprintf("%s, %s models not estimable\n", nm, fmt_n(sum(bad))))
    cols <- intersect(c("check", "variant", "sample", "model", "step", "outcome"), names(d))
    for (i in which(bad)) {
      cat("   ", paste(unlist(d[i, cols]), collapse = ", "), ": ", why[i], "\n", sep = "")
    }
  }
}

# Writes into the log the models of each table that were fitted without the
# non-Han respondents of one hukou group at age 3 (run_model), with the note
log_refitted <- function(tabs) {
  for (nm in names(tabs)) {
    d <- tabs[[nm]]
    if (!all(c("target", "merge_note") %in% names(d))) next
    idx <- which(d$target != "not estimable" & grepl("was fitted without them", d$merge_note, fixed = TRUE))
    if (!length(idx)) next
    cols <- intersect(c("check", "variant", "sample", "model", "outcome"), names(d))
    key <- do.call(paste, c(d[idx, cols, drop = FALSE], sep = ", "))
    first <- idx[!duplicated(key)]
    cat(sprintf("%s, %s models fitted without the non-Han respondents of one hukou group\n", nm,
                fmt_n(length(first))))
    for (i in first) {
      cat("   ", paste(unlist(d[i, cols]), collapse = ", "), ": ", d$merge_note[i], "\n", sep = "")
    }
  }
}

# Writes into the log the events that mice logged for each imputed sample:
# the variable imputed, the kind of event and the predictors removed. Names
# only, no values.
log_imp_events <- function(IMP) {
  for (nm in names(IMP)) {
    ev <- IMP[[nm]]$events
    if (is.null(ev) || !nrow(ev)) next
    # an event without a variable imputed comes from the checks before the
    # imputation, which remove a predictor from every model
    lab <- ifelse(nzchar(ev$dep), paste0(ev$out, " removed from the model of ", ev$dep, " (", ev$meth, ")"),
                  paste0(ev$out, " removed (", ev$meth, ")"))
    cat(nm, ", events logged by mice: ", paste(unique(lab), collapse = "; "), "\n", sep = "")
  }
}


#------------------------------------------------------------------------------
# Sample flows
#------------------------------------------------------------------------------

# Applies the steps in turn and counts the respondents left after each step,
# for Han, non-Han, ethnicity missing and all. The result holds the rows left
# and the rows of Table S1.
flow <- function(sample, f, steps) {
  keep <- rep(TRUE, nrow(f))
  g <- ifelse(is.na(f$nonhan), "Ethnicity missing", ifelse(f$nonhan == 1, "Non-Han", "Han"))
  out <- NULL
  for (s in names(steps)) {
    ok <- steps[[s]]
    ok[is.na(ok)] <- FALSE
    keep <- keep & ok
    n <- c(Han = sum(keep & g == "Han"), "Non-Han" = sum(keep & g == "Non-Han"),
           "Ethnicity missing" = sum(keep & g == "Ethnicity missing"), All = sum(keep))
    out <- rbind(out, data.frame(sample = sample, step = s, group = names(n), n = unname(n),
                                 stringsAsFactors = FALSE))
  }
  list(rows = which(keep), flow = out)
}
