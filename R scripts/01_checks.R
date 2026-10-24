#==============================================================================
# 01_checks.R
# Structural inequalities in education in China, CFPS 2010 to 2022
#
# Checks every data file before any analysis and writes five tables to the
# folder Output on the disk.
# C0  every weight variable in every file, with the variable label, the
#     type and the number of positive values
# C1  every file, with the number of records and variables, the ID
#     variable, the number of records with a missing or negative ID and the
#     number of records whose ID appears more than once
# C2  every variable in the variable lists of 00_setup.R, whether the
#     variable is in the file, and the number and percentage of records
#     coded -8 (not applicable), coded as missing (-1, -2, -9, -10 or
#     empty), or holding another negative value
# C3  respondents in the cross-year file by the first wave with an
#     interview and by ethnic group
# C4  records in each adult and child file, with the number and the sum of
#     positive weights, by national resample, oversampled province and
#     ethnic group
# No table shows an education or language outcome by ethnic group. A count
# from 1 to 9 is shown as <10, and a second cell is hidden where needed, so
# that no hidden count can be calculated from a total.
#
# How to run. Quit every AI app, then double-click Run checks and copy.command
# in the folder R scripts. That file opens the disk, unpacks any packed CFPS
# data, runs these checks, screens the output and copies it, and ejects the
# disk. The file 00_setup.R runs first by itself.
#
# Status. First run on the CFPS data on 19 October 2026.
#==============================================================================

# Folder of the R scripts, also when the folder name holds spaces
script_dir <- local({
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg) == 1) {
    dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", arg), fixed = TRUE)))
  } else {
    f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    if (is.null(f)) getwd() else dirname(normalizePath(f))
  }
})
source(file.path(script_dir, "00_setup.R"))


cfps_checks <- function() {
  for (p in c("haven", "readr", "tidyselect")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("The R package ", p, " is not installed. Type   install.packages(\"", p,
           "\")   in the R console, then run the checks again.", call. = FALSE)
    }
  }

  # Rows of the tables C0, C1, C2 and C4. C3 is made from the cross-year file
  # further down.
  c0 <- list()
  c1 <- list()
  c2 <- list()
  c4 <- list()
  xy <- NULL

  #----------------------------------------------------------------------------
  # Functions
  #----------------------------------------------------------------------------

  # check_file reads one file and adds one row for the file to C1, one row
  # for each listed variable to C2 and one row for each weight variable to
  # C0. The result is the data for c4_weights, or NULL when no listed
  # variable is in the file. The arguments are the wave year (0 for the
  # cross-year file), the file type and the code of the file in 00_setup.R.
  check_file <- function(wave, ftype, code) {
    file <- F_FILE[[code]]
    wanted <- tolower(V[[code]])
    if (!nzchar(file)) {
      status <- if (N_MATCH[[code]] == 0) "not found" else
        paste0(N_MATCH[[code]], " files match, choose one in the manual section of 00_setup.R")
      c1[[length(c1) + 1]] <<- data.frame(wave = wave, ftype = ftype, file = "",
        records = NA, nvars = NA, idvar = "", n_badid = NA, n_dupid = NA,
        status = status)
      cat(code, ", ", status, "\n", sep = "")
      return(NULL)
    }

    rel <- cfps_rel(file)
    cat(code, ", checking ", rel, "\n", sep = "")
    u <- tryCatch(cfps_use(file, c(wanted, W_PATTERN)), error = function(e) {
      cat("    The file could not be read. ", conditionMessage(e), "\n", sep = "")
      NULL
    })
    if (is.null(u)) {
      c1[[length(c1) + 1]] <<- data.frame(wave = wave, ftype = ftype, file = rel,
        records = NA, nvars = NA, idvar = "", n_badid = NA, n_dupid = NA,
        status = "could not be read, see the log 01_checks.log")
      return(NULL)
    }
    d <- u$data

    # ID variable. Family economy files use the family ID, the other files pid.
    idvar <- ""
    nbad <- NA
    ndup <- NA
    cands <- if (ftype == "E") paste0("fid", c(substr(wave, 3, 4), "")) else "pid"
    for (v in cands) {
      if (idvar == "" && v %in% names(d) && vtype(d[[v]]) == "number") idvar <- v
    }
    if (idvar != "") {
      id <- num(d[[idvar]])
      ok <- !is.na(id) & id >= 0
      nbad <- sum(!ok)
      ndup <- sum(ok & (duplicated(id) | duplicated(id, fromLast = TRUE)))
    }
    status <- if (is.null(d)) "opened, none of the listed variables is in the file" else "opened"
    if (u$enc == "GB18030") status <- paste0(status, ", text read as GB18030")
    if (length(u$idtext) > 0) {
      status <- paste0(status, ", ", paste(u$idtext, collapse = " and "),
                       " held as text and changed into numbers")
    }
    c1[[length(c1) + 1]] <<- data.frame(wave = wave, ftype = ftype, file = rel,
      records = u$n, nvars = u$k, idvar = idvar, n_badid = nbad, n_dupid = ndup,
      status = status)

    # C2. Text values that are neither empty nor a missing code count as
    # valid answers. A date counts as missing only when empty.
    for (w in wanted) {
      fnd <- 0
      vt <- ""
      vl <- ""
      nm8 <- NA
      nmiss <- NA
      nneg <- NA
      if (w %in% names(d)) {
        x <- d[[w]]
        fnd <- if (w %in% u$clash) 2 else 1
        vt <- vtype(x)
        vl <- label_of(x)
        if (vt == "date") {
          nmiss <- sum(is.na(x))
          nm8 <- 0
          nneg <- 0
        } else {
          v <- num(x)
          miss <- is.na(v) | (v %in% c(-1, -2, -9, -10))
          if (vt == "text") miss <- is.na(x) | trimws(x) == "" | (v %in% c(-1, -2, -9, -10))
          nmiss <- sum(miss)
          nm8 <- sum(v %in% -8)
          nneg <- sum(!is.na(v) & v < 0 & !(v %in% c(-1, -2, -8, -9, -10)))
        }
      }
      c2[[length(c2) + 1]] <<- data.frame(wave = wave, ftype = ftype, varname = w,
        found = fnd, vtype = vt, varlabel = vl, records = u$n, n_m8 = nm8,
        n_miss = nmiss, n_neg = nneg)
    }

    # C0, every variable whose name contains swt or weight
    for (v in names(d)) {
      if (grepl("swt|weight", tolower(v))) {
        x <- d[[v]]
        npos <- if (vtype(x) == "number") sum(num(x) > 0, na.rm = TRUE) else NA
        c0[[length(c0) + 1]] <<- data.frame(wave = wave, ftype = ftype, varname = v,
          varlabel = label_of(x), vtype = vtype(x), n_pos = npos)
      }
    }
    d
  }

  # c4_weights adds rows to C4 for one adult or child file. Each person is
  # linked to the cross-year file by pid. People with no record in the
  # cross-year file form group 5. A file with no weight variable whose name
  # begins with rswt still gets the counts, with the weight name none.
  c4_weights <- function(d, wave, ftype) {
    if (!"pid" %in% names(d) || vtype(d$pid) != "number") {
      cat("The ", ftype, " file of ", wave, " has no numeric pid, so the weight check of the file is skipped.\n", sep = "")
      return(invisible(NULL))
    }
    nrec <- nrow(d)
    if (nrec == 0) return(invisible(NULL))
    i <- if (is.null(xy)) rep(NA_integer_, nrec) else match(num(d$pid), xy$pid)
    grp <- ifelse(is.na(i), 5, xy$grp[i])
    ss <- ifelse(is.na(i), 9, xy$ss[i])
    sp <- ifelse(is.na(i), 9, xy$sp[i])
    wts <- names(d)[startsWith(names(d), "rswt")]
    wts <- wts[vapply(d[wts], function(x) vtype(x) == "number", logical(1))]
    if (length(wts) == 0) wts <- "none"
    for (w in wts) {
      if (w == "none") {
        p <- rep(0, nrec)
        s <- rep(0, nrec)
      } else {
        x <- num(d[[w]])
        pos <- !is.na(x) & x > 0
        p <- as.numeric(pos)
        s <- ifelse(pos, x, 0)
      }
      a <- aggregate(data.frame(n = rep(1, nrec), npos = p, wsum = s),
                     by = list(ss = ss, sp = sp, grp = grp), FUN = sum)
      a <- a[order(a$ss, a$sp, a$grp), ]
      if (w == "none") {
        a$npos <- NA
        a$wsum <- NA
      }
      c4[[length(c4) + 1]] <<- data.frame(wave = wave, ftype = ftype, wname = w, a)
      if (sum(a$n) != nrec) {
        cat("In the weight check of the ", ftype, " file of ", wave, " with weight ", w,
            ", the cells add up to ", sum(a$n), " records, but the file has ", nrec,
            " records.\n", sep = "")
      }
    }
  }

  # write_table writes one table to the folder Output.
  write_table <- function(d, name) {
    if (is.null(d) || nrow(d) == 0) {
      cat("    ", name, " has no rows, so no file was written.\n", sep = "")
      return(invisible(NULL))
    }
    readr::write_excel_csv(d, file.path(OUT, paste0(name, ".csv")), na = "")
    cat("    ", name, ".csv\n", sep = "")
  }

  # Text for the codes in the tables
  stack <- function(rows) if (length(rows) > 0) do.call(rbind, rows) else NULL
  fmt <- function(x, digits) ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), as.numeric(x)))
  wave_text <- function(w) ifelse(w == 0, "cross-year file", as.character(w))
  grp_text <- function(g) {
    c("Han", "non-Han", "other ethnicity code", "not recorded", "not in the cross-year file")[g]
  }
  ss_text <- function(s) {
    ifelse(s == 9, "not recorded", c("not in the national resample", "national resample")[s + 1])
  }
  sp_text <- function(s) {
    ifelse(s == 9, "not recorded",
           c("Shanghai", "Liaoning", "Henan", "Gansu", "Guangdong", "other provinces")[s])
  }


  #----------------------------------------------------------------------------
  # Cross-year file. C3, and the ethnic group of each person for C4.
  #----------------------------------------------------------------------------
  d <- check_file(0, "XY", "XY")
  c3 <- NULL
  if (is.null(d) || !"pid" %in% names(d) || vtype(d$pid) != "number" || nrow(d) == 0) {
    cat("No cross-year file is in use, so the table C3 is not made, and the weight check C4 counts every person as not in the cross-year file. Unpack the 2020 and 2022 data, or name the cross-year file in the manual section of 00_setup.R, to include it.\n")
    xy <- NULL
  } else {
    n <- nrow(d)
    column <- function(v) if (v %in% names(d)) num(d[[v]]) else rep(NA_real_, n)

    # First wave with an interview, 0 for a person with no interview
    firstwave <- rep(0, n)
    for (y in rev(WAVES)) {
      x <- column(paste0("indsurvey", y))
      firstwave[!is.na(x) & x >= 1] <- as.numeric(paste0("20", y))
    }

    # Ethnic group. The code for Han is set in 00_setup.R.
    eth <- column("ethnicity")
    grp <- rep(4, n)
    grp[!is.na(eth) & eth >= 0] <- 3
    grp[!is.na(eth) & eth >= 1 & eth <= 56 & eth != HAN] <- 2
    grp[!is.na(eth) & eth == HAN] <- 1

    # National resample and oversampled province
    x <- column("subsample")
    ss <- ifelse(x %in% c(0, 1), x, 9)
    x <- column("subpopulation")
    sp <- ifelse(x %in% 1:6, x, 9)

    # C3
    c3 <- aggregate(data.frame(n = rep(1, n)), by = list(firstwave = firstwave, grp = grp),
                    FUN = sum)

    # One record for each valid pid, for the links from the wave files
    pid <- num(d$pid)
    ok <- !is.na(pid) & pid >= 0
    xy <- data.frame(pid = pid, grp = grp, ss = ss, sp = sp)[ok, ]
    xy <- xy[!duplicated(xy$pid), ]
    rm(pid, eth, grp, ss, sp, x, ok, firstwave)
  }
  rm(d)


  #----------------------------------------------------------------------------
  # Adult or person, child, family roster and family economy files of each wave
  #----------------------------------------------------------------------------
  for (y in WAVES) {
    for (t in c("A", "C", "R", "E")) {
      d <- check_file(as.numeric(paste0("20", y)), t, paste0(t, y))
      if (t %in% c("A", "C") && !is.null(d)) c4_weights(d, as.numeric(paste0("20", y)), t)
      rm(d)
    }
  }


  #----------------------------------------------------------------------------
  # Hide small counts and write the tables
  #----------------------------------------------------------------------------
  cat("\nTables written to the folder Output on the disk\n")

  t0 <- stack(c0)
  if (!is.null(t0)) {
    t0 <- suppress_small(t0, "n_pos")
    t0$wave <- wave_text(t0$wave)
    t0 <- t0[c("wave", "ftype", "varname", "varlabel", "vtype", "n_pos_shown")]
  }
  write_table(t0, "C0_weight_names")

  t1 <- stack(c1)
  t1 <- suppress_small(t1, "n_badid")
  t1 <- suppress_small(t1, "n_dupid")
  t1$file_type <- unname(c(XY = "cross-year", A = "adult or person", C = "child",
                           R = "family roster", E = "family economy")[t1$ftype])
  t1$wave <- wave_text(t1$wave)
  t1$records <- fmt(t1$records, 0)
  t1 <- t1[c("wave", "ftype", "file_type", "file", "records", "nvars", "idvar",
             "n_badid_shown", "n_dupid_shown", "status")]
  write_table(t1, "C1_files")

  t2 <- stack(c2)
  if (!is.null(t2)) {
    for (s in c("m8", "miss", "neg")) {
      t2[[paste0("pct_", s)]] <- 100 * t2[[paste0("n_", s)]] / t2$records
      t2 <- suppress_small(t2, paste0("n_", s), stats = paste0("pct_", s))
      t2[[paste0("pct_", s)]] <- fmt(t2[[paste0("pct_", s)]], 1)
    }
    t2$wave <- wave_text(t2$wave)
    t2$found <- c("not in the file", "in the file",
                  "in the file twice, in upper and lower case")[t2$found + 1]
    t2$records <- fmt(t2$records, 0)
    t2 <- t2[c("wave", "ftype", "varname", "found", "vtype", "varlabel", "records",
               "n_m8_shown", "pct_m8", "n_miss_shown", "pct_miss", "n_neg_shown", "pct_neg")]
  }
  write_table(t2, "C2_missing")

  if (is.null(c3)) {
    cat("    C3_first_wave was not made, because no cross-year file is in use.\n")
  } else {
    t3 <- c3[order(c3$firstwave, c3$grp), ]
    t3 <- suppress_small(t3, "n", groups = list("firstwave", "grp", character(0)))
    t3$firstwave <- ifelse(t3$firstwave == 0, "no interview in any wave", as.character(t3$firstwave))
    t3$grp <- grp_text(t3$grp)
    t3 <- t3[c("firstwave", "grp", "n_shown")]
    write_table(t3, "C3_first_wave")
  }

  t4 <- stack(c4)
  if (!is.null(t4)) {
    t4$nzero <- t4$n - t4$npos
    G <- list(c("wave", "ftype", "wname", "ss", "sp"), c("wave", "ftype", "wname", "ss", "grp"),
              c("wave", "ftype", "wname"))
    t4 <- suppress_small(t4, "n", groups = G, stats = c("npos", "wsum", "nzero"))
    t4 <- suppress_small(t4, "npos", groups = G, also = "nzero", stats = "wsum",
                         prior = t4$n_shown %in% c(paste0("<", MINCELL), "suppressed"))
    t4$nzero <- NULL
    t4$wave <- wave_text(t4$wave)
    t4$ss <- ss_text(t4$ss)
    t4$sp <- sp_text(t4$sp)
    t4$grp <- grp_text(t4$grp)
    t4$wsum <- fmt(t4$wsum, 1)
    t4 <- t4[c("wave", "ftype", "wname", "ss", "sp", "grp", "n_shown", "npos_shown", "wsum")]
  }
  write_table(t4, "C4_weights")

  cat("\n01_checks.R finished. Run checks and copy.command now screens every table and log and copies them into the folder Output (checked).\n")
  invisible(NULL)
}

cfps_run("01_checks", cfps_checks)
