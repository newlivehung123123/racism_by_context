#==============================================================================
# 00_setup.R
# Structural inequalities in education in China, CFPS 2010 to 2018
#
# Sets the folder paths, finds each CFPS data file by the CFPS file name, sets
# the variable lists, defines the helper functions that the later R scripts
# use, and checks the R packages. Every later script reads 00_setup.R first,
# so 00_setup.R never needs to be run by hand.
#
# Status. First run on the CFPS data on 19 October 2026. On 23 October 2026 the
# waves 2020 and 2022 were dropped, because their archives would not open, so
# the analysis uses the waves 2010 to 2018.
#
# Data rules for every R script of the project
# 1. Open the disk CFPS data only to run R, with no AI tool open, as the
#    CFPS rules require. The scripts stop with a message when a known AI app
#    is open. Eject the disk afterwards.
# 2. No View, print, head, str or summary of a data set, and no output that
#    shows a person ID, a family ID, a PSU code or a county code. Variable
#    names may appear in the output, ID values never.
# 3. Aggregates only. A cell with fewer than 10 respondents is shown as <10.
# 4. Person records stay on the disk. Data sets exist only inside functions,
#    never in the R workspace, and the scripts write data files only to the
#    folder Work on the disk. Rscript --vanilla never writes the workspace to
#    a file.
# 5. The file Run checks and copy.command screens the folder Output and copies
#    it into the folder Output (checked) in the project folder by itself.
#
# Weight variables. A weight variable is any variable whose name contains swt
# or weight. The pattern swt, and not wt, keeps out the tutoring items wt1 to
# wt8 and wt201 to wt701 of the child files from 2018 to 2022.
#==============================================================================

set.seed(20261014)
options(warn = 1)

# R packages in the user library, also under Rscript --vanilla
local({
  lib <- path.expand(Sys.getenv("R_LIBS_USER"))
  if (nzchar(lib) && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
})

# Folders. Every folder is on the encrypted disk CFPS data. Tests on made-up
# data point the scripts at another folder through the setting CFPS_DATA.
DATA <- Sys.getenv("CFPS_DATA", "/Volumes/CFPS data")
RAW  <- file.path(DATA, "Newly downloaded Data")
OUT  <- file.path(DATA, "Output")
LOGS <- file.path(OUT, "logs")
WORK <- file.path(DATA, "Work")

# Process names of known AI apps. With the real disk, the scripts stop when
# one of the apps is open.
AI_APPS <- c("claude", "chatgpt")
if (DATA == "/Volumes/CFPS data") {
  local({
    open_apps <- tryCatch(
      suppressWarnings(system2("pgrep",
        c("-ai", shQuote(paste0("^(", paste(AI_APPS, collapse = "|"), ")$"))),
        stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0))
    if (length(open_apps) > 0) {
      stop("An AI app is open. Quit every AI app, and close any AI tool in the web browser, before using the disk CFPS data, as the CFPS rules require. Then run the R script again.", call. = FALSE)
    }
  })
}

# Stop with a plain message when the disk is not open.
if (!dir.exists(DATA)) {
  stop("The disk CFPS data is not open. In Finder, open the folder /Users/Shared, double-click CFPS data.sparsebundle and type the disk password. Then run the R script again.", call. = FALSE)
}
if (!dir.exists(RAW)) {
  stop("The disk CFPS data is open, but the folder Newly downloaded Data is not on the disk.", call. = FALSE)
}
for (folder in c(OUT, LOGS, WORK)) dir.create(folder, showWarnings = FALSE)
rm(folder)


#------------------------------------------------------------------------------
# Log of each run
#------------------------------------------------------------------------------

# cfps_run runs the function fun and writes everything that fun prints, with
# every warning and message, both to the console and to the log <name>.log in
# the folder logs inside the folder Output on the disk. An error ends the run
# with a plain message that points to the log.
cfps_run <- function(name, fun) {
  con <- file(file.path(LOGS, paste0(name, ".log")), open = "wt", encoding = "UTF-8")
  depth <- sink.number()
  sink(con, split = TRUE)
  on.exit({
    while (sink.number() > depth) sink()
    close(con)
  })
  cat(name, ", run on ", format(Sys.time(), "%d %B %Y at %H.%M"), " with ",
      R.version.string, "\n\n", sep = "")
  failed <- NULL
  result <- tryCatch(
    withCallingHandlers(fun(),
      warning = function(w) {
        cat("Warning. ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        cat(conditionMessage(m))
        invokeRestart("muffleMessage")
      }),
    error = function(e) {
      failed <<- conditionMessage(e)
      cat("\nError. ", failed, "\n", sep = "")
      NULL
    })
  if (!is.null(failed)) {
    stop(name, " stopped. The reason is at the end of the log ", name,
         ".log in the folder logs inside the folder Output on the disk.", call. = FALSE)
  }
  invisible(result)
}


#------------------------------------------------------------------------------
# Functions that find the data files by name
#------------------------------------------------------------------------------

# Paths of the files in root and in the folders below root, up to depth
# levels down, whose suffix is one of sfx. Hidden files, Mac copies whose
# names begin with ._ and the folder __MACOSX are skipped. Files inside a
# half-finished "(unpacking)" folder, which an archive leaves when it does not
# open, are skipped as well, so only files from a finished "(unpacked)" folder
# are ever used.
cfps_files <- function(root, depth, sfx) {
  rel <- list.files(root, recursive = TRUE, all.files = FALSE)
  rel <- rel[!grepl("(^|/)(__MACOSX|\\.)", rel)]
  rel <- rel[!grepl("(unpacking)", rel, fixed = TRUE)]
  rel <- rel[nchar(gsub("[^/]", "", rel)) <= depth]
  rel <- rel[tolower(paste0(".", tools::file_ext(rel))) %in% sfx]
  file.path(root, rel)
}

# File name in lower case, without the suffix and without the characters _
# and - and spaces.
cfps_stem <- function(path) {
  gsub("[-_ ]", "", tolower(tools::file_path_sans_ext(basename(path))))
}

# Path inside the folder Newly downloaded Data, for display.
cfps_rel <- function(path) {
  root <- paste0(RAW, "/")
  ifelse(startsWith(path, root), substring(path, nchar(root) + 1), path)
}

# Files of one type in one wave. A file matches when the file name contains
# cfps and the year, at least one word in kws and no word in nots.
cfps_pick <- function(f, yr, kws, nots = character(0)) {
  s <- cfps_stem(f)
  ok <- grepl(paste0("cfps", yr), s, fixed = TRUE)
  ok <- ok & Reduce(`|`, lapply(kws, function(w) grepl(w, s, fixed = TRUE)), FALSE)
  for (w in nots) ok <- ok & !grepl(w, s, fixed = TRUE)
  f[ok]
}

# Among several matching files, the single file with English value labels,
# whose name begins with ecfps. The result is f unchanged when there is not
# exactly one such file.
cfps_english <- function(f) {
  e <- f[startsWith(cfps_stem(f), "ecfps")]
  if (length(f) > 1 && length(e) == 1) e else f
}

# One cross-year file among several. The 2022 release comes first, then the
# 2020 release. Within a release the file with English labels is used. The
# result is empty when no single file qualifies.
cfps_prefer <- function(f, pref) {
  for (p in pref) {
    hits <- cfps_english(f[grepl(p, cfps_stem(f), fixed = TRUE)])
    if (length(hits) == 1) return(hits)
    if (length(hits) > 1) return(character(0))
  }
  character(0)
}

# Every data file the project uses. The result holds the matching files for
# each file code, the number of Stata data files and the number of archive
# files in the folder Newly downloaded Data.
cfps_findall <- function(root) {
  f <- cfps_files(root, 6, ".dta")
  hits <- list(XY = f[grepl("crossyear", cfps_stem(f), fixed = TRUE)])
  for (y in WAVES) {
    yr <- paste0("20", y)
    hits[[paste0("A", y)]] <- cfps_pick(f, yr, c("adult", "person"), "crossyear")
    hits[[paste0("C", y)]] <- cfps_pick(f, yr, "child", "crossyear")
    hits[[paste0("R", y)]] <- cfps_pick(f, yr, c("famconf", "famros"), "crossyear")
    e <- cfps_pick(f, yr, "famecon", "crossyear")
    if (length(e) == 0) e <- cfps_pick(f, yr, "family", c("crossyear", "famconf"))
    hits[[paste0("E", y)]] <- e
  }
  list(hits = hits, n_dta = length(f),
       n_archive = length(cfps_files(root, 6, c(".zip", ".rar", ".7z"))))
}


#------------------------------------------------------------------------------
# Manual section
# Use the manual section only when the table in the log 00_setup.log shows "not
# found", or shows that several files match, for a file that the analysis
# needs. Delete the hash sign at the beginning of the line for that file, then
# type the folder and the file name between the quotation marks, as in the two
# examples. The folder is the folder inside Newly downloaded Data. XY is the
# cross-year file. A is the adult or person file, C the child file, R the
# family roster file and E the family economy file, each followed by the last
# two digits of the wave year. Keep the comma at the end of each line.
#------------------------------------------------------------------------------
MANUAL <- c(
  # XY  = "<folder>/<file name>.dta",
  # A12 = "<folder>/<file name>.dta",
  NULL
)


#------------------------------------------------------------------------------
# Variable lists, checked against the English codebooks of download Set A on
# 15 October 2026. The scripts match names in any letter case and change each
# name to lower case after reading.
#------------------------------------------------------------------------------
WAVES <- c("10", "12", "14", "16", "18")
V <- list()

# Cross-year file, the person variables and the variables of each wave. The
# variables alive and selfrpt exist from 2012 only. The hukou variable is
# hk20r in 2020 and hk22r in 2022.
V$XY <- c("pid", "fidbaseline", "ethnicity", "gender", "birthy", "subpopulation",
          "subsample", "psu", "entrayear")
for (y in WAVES) {
  V$XY <- c(V$XY,
    paste0("cfps20", y, c("edu", "sch", "eduy", "eduy_im")),
    paste0(c("indsurvey", "inroster", "fid", "urban"), y),
    if (y != "10") paste0("alive", y),
    switch(y, "20" = "hk20r", "22" = "hk22r", paste0("hk", y)),
    if (y != "10") paste0("selfrpt", y),
    if (y == "12") "genetype12r",
    if (as.integer(y) >= 14) paste0("genetype", y))
}
rm(y)

# Adult files 2010 to 2016 and person files 2018 to 2022. Hukou at age 3 is
# qa302 in 2010, qa503 in 2012 and 2014 and pa503 in 2016, and the person
# files from 2018 have no such item. Hukou at age 12 is qa402 in 2010, pa603
# in 2016 and qa603 in the other waves. Ethnicity is qa5code in 2010,
# pa701code in 2016 and qa701code in the other waves, and the 2018 file also
# holds minzu. Current school enrolment is qd3 in 2010, wc01 in 2012 and
# 2014, school, pc1 and pc2 in 2016 and school, qc1 and qc2 from 2018. In
# 2010 the name qc1 is the highest level of schooling, not enrolment.
V$A10 <- c("pid", "fid", "provcd", "qa1age", "gender", "qa5code", "qa2", "qa302",
           "qa402", "qc1", "qd2", "ks3", "qz205", "wordtest", "qd3")
V$A12 <- c("pid", "fid12", "provcd", "cfps2012_age", "cfps2012_gender", "qa701code",
           "qa301", "qa503", "qa603", "edu2012", "qd201", "ks3m", "qz205", "wc01")
V$A14 <- c("pid", "fid14", "provcd14", "cfps2014_age", "cfps_gender", "qa701code",
           "qa301", "qa503", "qa603", "te4", "ks3m", "qz205", "wordtest14", "wc01")
V$A16 <- c("pid", "fid16", "provcd16", "cfps_age", "cfps_gender", "pa701code",
           "pa301", "pa503", "pa603", "cfps2016edu", "ks3m", "qz205", "school",
           "pc1", "pc2")
V$A18 <- c("pid", "fid18", "provcd18", "age", "gender", "qa701code", "minzu",
           "qa301", "qa603", "cfps2018edu", "qs3m", "qz103", "wordtest18", "school",
           "qc1", "qc2")
V$A20 <- c("pid", "fid20", "provcd20", "age", "gender", "qa701code", "qa301",
           "qa603", "qs3m", "qz103", "school", "qc1", "qc2")
V$A22 <- c("pid", "fid22", "provcd22", "age", "gender", "qa701code", "qa603",
           "selfrpt", "proxytype", "self_iwmode", "proxy_iwmode", "wordtest22",
           "cfps2022edu", "school", "qc1", "qc2")

# Child files 2010 to 2016 and child proxy files 2018 to 2022. Hukou at age 3
# is wa3 from 2010 to 2014, pa503 in 2016 and wa503 from 2018. Hukou at age
# 12 is wa302 in 2014, pa603 in 2016 and wa603 from 2018. The 2010 child file
# also holds the home language wk2, the interviewer rating of Mandarin wz205,
# the language with classmates ks3 and the word test.
V$C10 <- c("pid", "wa3", "wk2", "wz205", "ks3", "wordtest")
V$C12 <- c("pid", "wa3")
V$C14 <- c("pid", "wa3", "wa302")
V$C16 <- c("pid", "pa503", "pa603")
V$C18 <- c("pid", "wa503", "wa603")
V$C20 <- c("pid", "wa503", "wa603")
V$C22 <- c("pid", "wa503", "wa603")

# Family roster files. The parent IDs are pid_a_f and pid_a_m in 2020 and
# pid_f and pid_m in 2022.
V$R10 <- c("pid", "feduc", "meduc")
V$R12 <- c("pid", "feduc12", "meduc12")
V$R14 <- c("pid", "tb4_a14_f", "tb4_a14_m")
V$R16 <- c("pid", "tb4_a16_f", "tb4_a16_m")
V$R18 <- c("pid", "tb4_a18_f", "tb4_a18_m")
V$R20 <- c("pid", "tb4_a20_p", "kz103_20", "pid_a_f", "pid_a_m")
V$R22 <- c("pid", "tb4_a22_p", "hukou_a22_p", "pid_f", "pid_m", "kz103_22")

# Family economy files. The 2010 file has the family ID fid and no fid10.
V$E10 <- c("fid", "faminc_net")
for (y in WAVES[-1]) V[[paste0("E", y)]] <- c(paste0("fid", y), "fincome1")
rm(y)

# Person weights of each wave. In the names, nat marks the national full
# sample, res the national resample, cs a cross-sectional weight, pn10 a
# panel weight from 2010 and ps a post-stratified weight. From 2018 CFPS
# provides full-sample weights only. 01_checks.R also lists every weight
# variable in every file.
W <- list(
  "10" = c("rswt_nat", "rswt_res"),
  "12" = c("rswt_natcs12", "rswt_rescs12", "rswt_natpn1012", "rswt_respn1012"),
  "14" = c("rswt_natcs14", "rswt_rescs14", "rswt_natpn1014", "rswt_respn1014"),
  "16" = c("rswt_natcs16", "rswt_rescs16", "rswt_natpn1016", "rswt_respn1016"),
  "18" = c("rswt_natcs18n", "rswt_natpn1018n"),
  "20" = c("rswt_natcs20n", "rswt_natpn1020n", "rswtps_natcs20n", "rswtps_natpn1020n"),
  "22" = c("rswt_natcs22n", "rswt_natpn1022n"))
W_PATTERN <- c("*swt*", "*weight*")

# Person and family ID variables, changed into numbers when held as text
ID_VARS <- c("pid", "fid", "fidbaseline", paste0("fid", WAVES))

# Ethnicity code for Han in the cross-year file
HAN <- 1

# Smallest count shown in any output
MINCELL <- 10

# R packages of the project
PACKAGES <- c("haven", "readr", "tidyselect", "survey", "VGAM", "svyVGAM", "mice", "mitools")


#------------------------------------------------------------------------------
# Helper functions
#------------------------------------------------------------------------------

# Values as plain numbers, with the value labels dropped. Text that holds a
# number becomes the number, and other text becomes NA.
num <- function(x) {
  x <- unclass(x)
  if (is.character(x)) x <- suppressWarnings(as.numeric(trimws(x)))
  as.numeric(x)
}

# Type of a variable as shown in the tables, text, number or date
vtype <- function(x) {
  if (is.character(x)) "text" else if (inherits(x, c("Date", "POSIXt"))) "date" else "number"
}

# Variable label, or empty text when the variable has no label
label_of <- function(x) {
  l <- attr(x, "label", exact = TRUE)
  if (is.null(l)) "" else as.character(l)[1]
}

# Matches the wanted names, in any letter case and with * as a wildcard,
# against the names in a file. The result holds the names found, in the
# order of the file, and the wanted names with no match.
cfps_match <- function(all, wanted) {
  la <- tolower(all)
  hit <- rep(FALSE, length(all))
  absent <- character(0)
  for (w in unique(tolower(wanted))) {
    m <- grepl(utils::glob2rx(w), la)
    if (!any(m)) absent <- c(absent, w)
    hit <- hit | m
  }
  list(found = all[hit], absent = absent)
}

# Release number of a Stata data file, 117 for Stata 13 and 118 or 119 for
# Stata 14 and later. Formats older than Stata 13 give 115 or less.
dta_release <- function(file) {
  tryCatch({
    b <- readBin(file, "raw", 64)
    s <- rawToChar(b[b != as.raw(0)])
    m <- regmatches(s, regexpr("<release>[0-9]+</release>", s, useBytes = TRUE))
    if (length(m) == 1) as.integer(gsub("[^0-9]", "", m, useBytes = TRUE)) else as.integer(b[1])
  }, error = function(e) NA_integer_)
}

# TRUE when every name, variable label, value label and text value in d is
# valid UTF-8
text_ok <- function(d) {
  txt <- names(d)
  for (x in d) {
    txt <- c(txt, label_of(x), names(attr(x, "labels", exact = TRUE)))
    if (is.character(x)) txt <- c(txt, x[!is.na(x)])
  }
  all(validUTF8(txt))
}

# Reads the columns cols of a Stata data file, or no rows at all when cols is
# NULL.
read_cols <- function(file, enc, cols) {
  if (is.null(cols)) {
    haven::read_dta(file, encoding = enc, n_max = 0)
  } else {
    haven::read_dta(file, encoding = enc, col_select = tidyselect::all_of(cols))
  }
}

# cfps_use reads the listed variables from one file. Names match in any
# letter case and * works as a wildcard. Each name read changes to lower
# case, unless the lower case name is also in the file. Person and family
# IDs held as text become numbers. Text in files from Stata 13 or older, and
# text that is not valid UTF-8, is read as GB18030, the Chinese encoding.
# The result holds the data (NULL when no listed variable is in the file),
# the names found, the names absent, the names in the file twice, the IDs
# changed from text into numbers, the number of records, the number of
# variables in the file and the encoding used.
cfps_use <- function(file, vars) {
  rel <- dta_release(file)
  enc <- if (!is.na(rel) && rel <= 117) "GB18030" else NULL
  top <- read_cols(file, enc, NULL)
  if (is.null(enc) && !text_ok(top)) {
    enc <- "GB18030"
    top <- read_cols(file, enc, NULL)
  }
  m <- cfps_match(names(top), vars)
  if (length(m$found) == 0) {
    nrec <- nrow(read_cols(file, enc, names(top)[1]))
    return(list(data = NULL, found = character(0), absent = m$absent,
                clash = character(0), idtext = character(0), n = nrec, k = ncol(top),
                enc = if (is.null(enc)) "UTF-8" else enc))
  }
  d <- read_cols(file, enc, m$found)
  if (is.null(enc) && !text_ok(d)) {
    enc <- "GB18030"
    d <- read_cols(file, enc, m$found)
  }
  nm <- names(d)
  clash <- character(0)
  for (i in seq_along(nm)) {
    low <- tolower(nm[i])
    if (low != nm[i]) {
      if (low %in% nm) clash <- c(clash, low) else nm[i] <- low
    }
  }
  names(d) <- nm
  idtext <- character(0)
  for (v in intersect(nm, ID_VARS)) {
    if (is.character(d[[v]])) {
      lab <- attr(d[[v]], "label", exact = TRUE)
      d[[v]] <- suppressWarnings(as.numeric(trimws(d[[v]])))
      attr(d[[v]], "label") <- lab
      idtext <- c(idtext, v)
    }
  }
  list(data = d, found = nm, absent = m$absent, clash = unique(clash), idtext = idtext,
       n = nrow(d), k = ncol(top), enc = if (is.null(enc)) "UTF-8" else enc)
}

# suppress_small hides small cells in a table of counts, one row for each
# cell, and returns the table.
# count   name of the count column. A count from 1 to 9 is shown as <10.
# groups  list of sets of column names. Each set defines cells that add up
#         to a total, and an empty set means the whole table. When exactly
#         one cell in a set is hidden, the smallest other positive cell in
#         the set is hidden too and shown as suppressed, so that no hidden
#         count can be calculated from a total.
# stats   columns blanked in every hidden cell
# also    a second count. The cell is also hidden, and shown as suppressed,
#         when the second count is from 1 to 9.
# prior   TRUE for each cell hidden by an earlier call. Such a cell stays
#         blank and counts as hidden in each set.
# The count column is replaced by a text column named <count>_shown.
suppress_small <- function(d, count, groups = list(), stats = character(0),
                           also = NULL, prior = NULL) {
  x <- d[[count]]
  small <- function(v) !is.na(v) & v >= 1 & v < MINCELL
  hidden <- ifelse(small(x), 1L, 0L)
  if (!is.null(also)) hidden[hidden == 0L & small(d[[also]])] <- 2L
  if (!is.null(prior)) hidden[hidden == 0L & prior] <- 3L
  repeat {
    changed <- FALSE
    for (g in groups) {
      gid <- if (length(g)) {
        do.call(paste, c(lapply(d[g], as.character), sep = "\r"))
      } else rep("", nrow(d))
      for (rows in split(seq_len(nrow(d)), gid)) {
        if (sum(hidden[rows] > 0L) != 1L) next
        cand <- rows[hidden[rows] == 0L & !is.na(x[rows]) & x[rows] > 0]
        if (length(cand) == 0) next
        hidden[cand[order(x[cand], cand)][1]] <- 2L
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  shown <- ifelse(is.na(x), "", sprintf("%.0f", as.numeric(x)))
  shown[hidden == 1L] <- paste0("<", MINCELL)
  shown[hidden == 2L] <- "suppressed"
  shown[hidden == 3L] <- ""
  for (s in stats) d[[s]][hidden > 0L] <- NA
  pos <- match(count, names(d))
  d[[count]] <- shown
  names(d)[pos] <- paste0(count, "_shown")
  d
}


#------------------------------------------------------------------------------
# Find the data files, print the table of data files and check the packages
#------------------------------------------------------------------------------
cfps_setup <- function() {
  codes <- c("XY", as.vector(outer(c("A", "C", "R", "E"), WAVES, paste0)))
  labels <- c(XY = "2010 to 2022 cross-year")
  for (y in WAVES) {
    labels[paste0(c("A", "C", "R", "E"), y)] <- paste0("20", y, c(" adult or person",
      " child", " family roster", " family economy"))
  }

  found <- cfps_findall(RAW)
  file <- setNames(rep("", length(codes)), codes)
  n <- setNames(integer(length(codes)), codes)
  for (code in codes) {
    h <- found$hits[[code]]
    if (length(h) > 1) {
      e <- cfps_english(h)
      if (length(e) == 1) {
        cat(code, ", ", length(h), " files match, using the one with English labels\n", sep = "")
        cat(paste0("    ", cfps_rel(h), "\n"), sep = "")
        cat("    The file used is ", cfps_rel(e), "\n", sep = "")
        h <- e
      }
    }
    n[code] <- length(h)
    if (length(h) == 1) file[code] <- h
    if (length(h) > 1) {
      cat(code, ", ", length(h), " files match\n", sep = "")
      cat(paste0("    ", cfps_rel(h), "\n"), sep = "")
    }
  }
  if (n["XY"] > 1) {
    pick <- cfps_prefer(found$hits$XY, c("cfps2022", "cfps2020"))
    if (length(pick) == 1) {
      file["XY"] <- pick
      n["XY"] <- 1L
      cat("    The cross-year file used is ", cfps_rel(pick), "\n", sep = "")
    }
  }
  for (code in names(MANUAL)) {
    if (!code %in% codes) {
      cat("The manual section names ", code, ", which is not a file code, so that line is ignored.\n", sep = "")
      next
    }
    file[code] <- file.path(RAW, MANUAL[[code]])
    n[code] <- 1L
  }

  cat("\nFolder of the CFPS data files, ", RAW, "\n", sep = "")
  cat("\nData files in use, shown as paths inside the folder Newly downloaded Data\n")
  cat(strrep("-", 100), "\n", sep = "")
  for (code in codes) {
    status <- if (nzchar(file[code])) {
      if (file.exists(file[code])) cfps_rel(file[code]) else paste0(cfps_rel(file[code]), ", no such file")
    } else if (n[code] == 0) {
      "not found"
    } else {
      paste0(n[code], " files match, choose one in the manual section")
    }
    cat(sprintf("%-6s%-28s%s\n", code, labels[code], status))
  }
  cat(strrep("-", 100), "\n", sep = "")
  cat("Stata data files in the folder Newly downloaded Data, ", found$n_dta, "\n", sep = "")
  if (found$n_archive > 0) {
    cat("Archive files (zip, rar or 7z) in the same folder, ", found$n_archive,
        ". An archive whose data the analysis needs has to be unpacked first.\n", sep = "")
  }

  cat("\nR packages\n")
  for (p in PACKAGES) {
    if (requireNamespace(p, quietly = TRUE)) {
      cat("Package ", p, " ", format(utils::packageVersion(p)), " is installed.\n", sep = "")
    } else {
      cat("Package ", p, " is not installed. Type   install.packages(\"", p,
          "\")   in the R console.\n", sep = "")
    }
  }
  cat("\n00_setup.R finished.\n")
  list(file = file, n = n)
}

SETUP <- cfps_run("00_setup", cfps_setup)
F_FILE <- SETUP$file
N_MATCH <- SETUP$n
rm(SETUP)
