#==============================================================================
# 02_link.R
# Structural inequalities in education in China, CFPS 2010 to 2018
#
# Reads the adult, child, family roster and family economy files of the waves
# 2010 to 2018, links them by person and family ID and builds the frames and
# samples of the analysis. Everything goes into the file link.rds in the
# folder Work on the disk, which is never copied. No output table is written
# here. The log 02_link.log lists the files read, the IDs dropped, the
# variables chosen and the respondents in each sample, with every count from
# 1 to 9 shown as <10.
#
# Frames
# W10 to W18  adult respondents of each wave
# C10         child respondents of 2010
# F18         respondents of 2018 with a positive panel weight from 2010
# BE          adult respondents aged 16 to 22, all waves together
#
# Ethnicity, hukou at age 3, birth year, gender and parents' education come
# from the earliest wave with a valid value, the adult file before the child
# file. The codes follow the analysis plan.
#
# How to run. Quit every AI app, then double-click Run analysis.command in the
# folder R scripts, which runs 02_link.R to 05_checks.R in turn.
#
# Status. Written on 23 October 2026 and tested on made-up data only. Revised
# the same day, after the first run on the CFPS data and before any table of
# estimates was read: the education variable of the 2010 child file is the
# first of a list of names, as for the 2010 adult file (see analysis_tools.R).
# The revision was tested on made-up data only.
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
source(file.path(script_dir, "analysis_tools.R"))


# Person items kept in every frame
PCOLS <- c("nonhan", "ethcode", "eth_other", "agri3", "nh_ag", "hk3_fb", "hk3_other", "birthy",
           "cohort", "male", "paredu", "prov10")

# Columns of the frame BE taken from the wave frames
BE_COLS <- c("pid", "wave", "w", "age", "prov", "enrol", "linc", "selfrpt", "subsample", PCOLS)

# TRUE where x is zero or more
nonneg <- function(x) !is.na(x) & x >= 0

# x with its missing values taken from y
fill <- function(x, y) {
  m <- is.na(x)
  x[m] <- y[m]
  x
}

# New codes of x from the map, whose names are the old codes, NA elsewhere
recode <- function(x, map) unname(map[match(x, as.numeric(names(map)))])

# TRUE where every listed column of f is known
known <- function(f, ...) Reduce(`&`, lapply(c(...), function(v) !is.na(f[[v]])))

# Value of a person item for each ID in pid, from the earliest wave with a
# valid value, the adult file before the child file. field names an item of
# the wave map VM, and c_<field> the item of the child file.
earliest <- function(D, pid, field, ok, waves = WAVES) {
  out <- rep(NA_real_, length(pid))
  for (yy in waves) {
    for (t in c("A", "C")) {
      vars <- VM[[yy]][[if (t == "A") field else paste0("c_", field)]]
      if (is.null(vars) || all(is.na(vars))) next
      d <- D[[paste0(t, yy)]]
      out <- fill(out, coalesce_valid(d, vars, ok)[match(pid, d$pid)])
    }
  }
  out
}

# Transitions of an education level x: level 2 or above among all, 3 or
# above among those at 2 or above, 4 or above among those at 3 or above, and
# 5 or above among those at 4 or above
transitions <- function(x, prefix) {
  out <- list()
  for (k in 1:4) out[[paste0(prefix, k)]] <- ifelse(!is.na(x) & x >= k, as.numeric(x >= k + 1), NA_real_)
  as.data.frame(out)
}

# Person items for the IDs pid
person_cols <- function(P, pid) {
  p <- P[match(pid, P$pid), PCOLS, drop = FALSE]
  rownames(p) <- NULL
  p
}

# Stratum: the wave value, else the person's earliest value, else 9
strata_of <- function(x, person) {
  x[!nonneg(x)] <- NA
  x <- fill(x, person)
  x[is.na(x)] <- 9
  x
}


#------------------------------------------------------------------------------
# Frames
#------------------------------------------------------------------------------

# Frame of the adult respondents of wave yy
wave_frame <- function(yy, D, P, PFX, pool, LINC) {
  a <- D[[paste0("A", yy)]]
  vm <- VM[[yy]]
  k <- match(a$pid, P$pid)
  f <- data.frame(pid = a$pid, wave = as.numeric(yy))
  f$w <- col_of(a, vm$w)
  f$wres <- col_of(a, vm$wres)
  f$wpn <- col_of(a, vm$wpn)

  # PSU, stratum and national resample
  x <- col_of(a, vm$psu)
  f$psu <- ifelse(nonneg(x), paste0(PFX[[yy]], code_str(x)), pool[k])
  f$strata <- strata_of(col_of(a, vm$subpop), P$subpop_p[k])
  ss <- col_of(a, vm$subsample)
  ss[!nonneg(ss)] <- NA
  f$subsample <- fill(ss, P$subsample_p[k])

  # Age, else the wave year minus the birth year, and province
  age <- valid_rng(col_of(a, vm$age), 0, 120)
  n_age <- sum(is.na(age) & !is.na(P$birthy[k]))
  f$age <- valid_rng(fill(age, 2000 + as.numeric(yy) - P$birthy[k]), 0, 120)
  f$prov <- valid_rng(col_of(a, vm$prov), 11, 82)

  # Education, with the variable chosen for the whole file
  e <- first_var(a, vm$edu, function(x) inr(x, 1, 8))
  sc <- first_var(a, vm$sch, function(x) inr(x, 1, 8))
  ey <- first_var(a, vm$eduy, function(x) inr(x, 0, 30))
  f$edu <- valid_rng(e$x, 1, 8)
  f$edu5 <- pmin(f$edu, 5)
  f$edu6 <- pmin(f$edu, 6)
  f$sch <- valid_rng(sc$x, 1, 8)
  f$eduy <- valid_rng(ey$x, 0, 30)
  en <- col_of(a, vm$enrol)
  f$enrol <- ifelse(nonneg(en), as.numeric(en %in% if (yy %in% c("16", "18")) c(1, 5) else 1), NA)

  # Home language: 1 Mandarin, 2 minority language, 3 Chinese dialect
  lg <- col_of(a, vm$lang)
  map <- switch(yy, "10" = c("1" = 1, "3" = 3, "5" = 2), "12" = c("1" = 1, "2" = 3, "5" = 3, "6" = 2), NULL)
  if (is.null(map)) {
    f$lang <- NA_real_
    f$lang_other <- 0
  } else {
    f$lang <- recode(lg, map)
    f$lang_other <- as.numeric(nonneg(lg) & is.na(f$lang))
  }

  # Mandarin rating, word test and language of the classmates
  rt <- col_of(a, vm$rating)
  f$rating <- valid_rng(rt, 1, 7)
  f$rating79 <- as.numeric(!is.na(rt) & rt == 79)
  f$rating0 <- ifelse(f$rating79 == 1, 0, f$rating)
  wt <- col_of(a, vm$wordtest)
  wt[!nonneg(wt)] <- NA
  f$wordtest <- wt
  cl <- col_of(a, vm$clm)
  cl[!(nonneg(cl) & cl >= 1 & cl != 79)] <- NA
  f$clm <- cl
  f$clm_min <- as.numeric(cl == if (yy == "10") 5 else 6)
  sr <- col_of(a, vm$selfrpt)
  sr[!nonneg(sr)] <- NA
  f$selfrpt <- sr
  hk <- col_of(a, vm$curhk)
  f$curhk <- ifelse(hk %in% 1, 1, ifelse(hk %in% 3, 0, NA))
  qz <- if (yy == "18") col_of(a, "qz103") else rep(NA_real_, nrow(a))
  qz[!(nonneg(qz) & qz >= 1)] <- NA
  f$qz103 <- qz

  # Family income per person, from the family economy file of the wave
  li <- LINC[[yy]]
  f$linc <- li$linc[match(col_of(a, vm$fid), li$fid)]

  f <- cbind(f, transitions(f$edu, "t"), transitions(f$sch, "s"), person_cols(P, a$pid))

  cat(sprintf("W%s  adult respondents %s; education %s, school level %s, years of schooling %s; age from the birth year %s; PSU from the person %s; family income per person known %s\n",
              yy, fmt_c(nrow(f)), e$name, sc$name, ey$name, fmt_c(n_age), fmt_c(sum(!nonneg(x))),
              fmt_c(sum(!is.na(f$linc)))))
  if (yy == "10" && e$name != "qc1" && "qc1" %in% names(a)) {
    q <- valid_rng(a$qc1, 1, 8)
    both <- !is.na(q) & !is.na(f$edu)
    if (any(both)) {
      cat(sprintf("     qc1 gives the same level as %s for %s%% of the %s respondents with both\n",
                  e$name, fmt_p(100 * mean(q[both] == f$edu[both])), fmt_c(sum(both))))
    }
  }
  f
}

# Frame of the child respondents of 2010
child_frame <- function(D, P, pool, LINC) {
  a <- D$C10
  vm <- VM[["10"]]
  k <- match(a$pid, P$pid)
  f <- data.frame(pid = a$pid, wave = 10)
  f$w <- col_of(a, vm$w)
  f$wres <- col_of(a, vm$wres)
  x <- col_of(a, vm$c_psu)
  f$psu <- ifelse(nonneg(x), paste0("c_", code_str(x)), pool[k])
  f$strata <- strata_of(col_of(a, vm$c_subpop), P$subpop_p[k])
  ss <- col_of(a, vm$c_subsample)
  ss[!nonneg(ss)] <- NA
  f$subsample <- fill(ss, P$subsample_p[k])
  age <- valid_rng(col_of(a, vm$c_age), 0, 120)
  f$age <- valid_rng(fill(age, 2010 - P$birthy[k]), 0, 120)
  f$prov <- valid_rng(col_of(a, vm$c_prov), 11, 82)
  wk <- col_of(a, "wk2")
  f$lang <- recode(wk, c("1" = 1, "2" = 3, "3" = 2))
  f$lang_other <- as.numeric(nonneg(wk) & is.na(f$lang))
  f$rating <- valid_rng(col_of(a, "wz205"), 1, 7)
  wt <- col_of(a, "wordtest")
  wt[!nonneg(wt)] <- NA
  f$wordtest <- wt
  cl <- col_of(a, "ks3")
  cl[!(nonneg(cl) & cl >= 1 & cl != 79)] <- NA
  f$clm <- cl
  f$clm_min <- as.numeric(cl == 5)
  e <- first_var(a, c("cfps2010edu_best", "cfps2010edu", "edu2010", "cfps2010edu_*", "edu2010_*"),
                 function(x) inr(x, 1, 8))
  f$edu <- valid_rng(e$x, 1, 8)
  f$edu5 <- pmin(f$edu, 5)
  li <- LINC[["10"]]
  f$linc <- li$linc[match(col_of(a, "fid"), li$fid)]
  f <- cbind(f, person_cols(P, a$pid))
  cat(sprintf("C10  child respondents %s; education %s; PSU from the person %s; family income per person known %s\n",
              fmt_c(nrow(f)), e$name, fmt_c(sum(!nonneg(x))), fmt_c(sum(!is.na(f$linc)))))
  f
}

# Frame of the respondents of 2018 with a positive panel weight from 2010,
# with their answers as children in 2010. A person in both the adult and the
# child file of 2018 keeps the adult record.
panel_frame <- function(D, P, pool, W18, C10) {
  wa <- col_of(D$A18, "rswt_natpn1018n")
  wc <- col_of(D$C18, "rswt_natpn1018n")
  ida <- D$A18$pid[!is.na(wa) & wa > 0]
  idc <- setdiff(D$C18$pid[!is.na(wc) & wc > 0], ida)
  pid <- c(ida, idc)
  k <- match(pid, P$pid)
  f <- data.frame(pid = pid, wave = 18)
  f$w <- c(wa[match(ida, D$A18$pid)], wc[match(idc, D$C18$pid)])
  f$psu <- pool[k]
  f$strata <- strata_of(rep(NA_real_, length(pid)), P$subpop_p[k])
  ka <- c(match(ida, W18$pid), rep(NA_integer_, length(idc)))
  f$age <- W18$age[ka]
  f$edu18 <- W18$edu[ka]
  f$sch18 <- W18$sch[ka]
  f$selfrpt18 <- W18$selfrpt[ka]
  kc <- match(pid, C10$pid)
  f$in_c10 <- as.numeric(!is.na(kc))
  f$age10 <- C10$age[kc]
  f$lang10 <- C10$lang[kc]
  f$rating10 <- C10$rating[kc]
  f$wordtest10 <- C10$wordtest[kc]
  f$linc10 <- C10$linc[kc]
  f$y_sh <- as.numeric(f$edu18 >= 4)
  f$y_ter <- ifelse(is.na(f$edu18), NA, as.numeric(f$edu18 >= 5 | (!is.na(f$sch18) & f$sch18 >= 5)))
  f <- cbind(f, person_cols(P, pid))
  cat(sprintf("F18  respondents with a positive panel weight %s, from the adult file %s, child respondents in 2010 %s\n",
              fmt_c(nrow(f)), fmt_c(length(ida)), fmt_c(sum(f$in_c10 == 1))))
  f
}

# Frame of the adult respondents aged 16 to 22 of all waves, with the weights
# scaled to mean 1 within each wave and the pooled PSU of each person
enrol_frame <- function(W, P, pool) {
  parts <- list()
  for (yy in WAVES) {
    f <- W[[yy]]
    g <- f[which(inr(f$age, 16, 22)), BE_COLS, drop = FALSE]
    pos <- !is.na(g$w) & g$w > 0
    g$w[pos] <- g$w[pos] / mean(g$w[pos])
    k <- match(g$pid, P$pid)
    g$psu <- pool[k]
    g$strata <- strata_of(rep(NA_real_, nrow(g)), P$subpop_p[k])
    parts[[yy]] <- g
  }
  f <- do.call(rbind, parts)
  rownames(f) <- NULL
  cat(sprintf("BE   adult respondents aged 16 to 22, all waves %s\n", fmt_c(nrow(f))))
  f
}


#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
cfps_link <- function() {
  t0 <- proc.time()[["elapsed"]]

  # 1. Files, with the records of a missing or repeated ID dropped
  cat("Files\n")
  D <- list()
  LABS <- list()
  for (yy in WAVES) {
    for (t in c("A", "C", "R", "E")) {
      code <- paste0(t, yy)
      r <- read_file(code)
      D[[code]] <- clean_ids(r$d, if (t == "E") FIDV[[yy]] else "pid", code)
      LABS[[code]] <- r$labels
    }
  }
  labels <- list(eth = label_lookup(LABS, ETH_VARS), prov = label_lookup(LABS, PROV_VARS))
  cat(sprintf("Names of ethnic groups found %d, names of provinces found %d\n",
              sum(as.numeric(names(labels$eth)) %in% 1:60), sum(as.numeric(names(labels$prov)) %in% 11:82)))

  # 2. Persons
  pid <- sort(unique(unlist(lapply(c(paste0("A", WAVES), paste0("C", WAVES)), function(k) D[[k]]$pid))))
  P <- data.frame(pid = pid)
  first <- rep(NA_real_, length(pid))
  for (yy in rev(WAVES)) {
    for (t in c("A", "C")) first[pid %in% D[[paste0(t, yy)]]$pid] <- as.numeric(yy)
  }
  P$ethcode <- earliest(D, pid, "eth", function(x) inr(x, 1, 60))
  P$nonhan <- ifelse(is.na(P$ethcode), NA, as.numeric(P$ethcode != 1))
  oth <- earliest(D, pid, "eth", function(x) nonneg(x) & !inr(x, 1, 60))
  P$eth_other <- as.numeric(is.na(P$ethcode) & !is.na(oth))

  # Hukou at age 3. A person first interviewed in 2018 without a value takes
  # the hukou at age 12 of the adult file of 2018.
  hk <- earliest(D, pid, "hk3", function(x) x %in% c(1, 3))
  q12 <- coalesce_valid(D$A18, VM[["18"]]$hk12, function(x) x %in% c(1, 3))[match(pid, D$A18$pid)]
  fb <- is.na(hk) & first == 18 & !is.na(q12)
  hk[fb] <- q12[fb]
  P$agri3 <- ifelse(is.na(hk), NA, as.numeric(hk == 1))
  P$hk3_fb <- as.numeric(fb)
  hoth <- earliest(D, pid, "hk3", function(x) x %in% c(5, 7, 79))
  P$hk3_other <- as.numeric(is.na(hk) & !is.na(hoth))
  P$nh_ag <- P$nonhan * P$agri3

  P$birthy <- earliest(D, pid, "birthy", function(x) inr(x, 1900, 2018))
  P$cohort <- cohort_of(P$birthy)
  P$male <- earliest(D, pid, "male", function(x) x %in% c(0, 1))

  # Parents' education: the earliest roster with a valid value, the higher of
  # the two parents, with levels 5 to 8 as 5
  pe <- rep(NA_real_, length(pid))
  for (yy in WAVES) {
    r <- D[[paste0("R", yy)]]
    x <- suppressWarnings(pmax(valid_rng(r[[PE[[yy]][1]]], 1, 8), valid_rng(r[[PE[[yy]][2]]], 1, 8),
                               na.rm = TRUE))
    pe <- fill(pe, x[match(pid, r$pid)])
  }
  P$paredu <- pmin(pe, 5)

  P$prov10 <- earliest(D, pid, "prov", function(x) inr(x, 11, 82), "10")
  P$cnty10 <- earliest(D, pid, "cnty", nonneg, "10")
  P$psu18 <- earliest(D, pid, "psu", nonneg, "18")
  P$subpop_p <- earliest(D, pid, "subpop", nonneg)
  P$subsample_p <- earliest(D, pid, "subsample", nonneg)

  cat("\nPersons\n")
  cat(sprintf("Persons in the adult and child files %s\n", fmt_c(nrow(P))))
  for (yy in WAVES) cat(sprintf("  first interviewed in 20%s %s\n", yy, fmt_c(sum(first == as.numeric(yy)))))
  cat(sprintf("Ethnicity known %s, non-Han %s; only codes outside 1 to 60 %s\n", fmt_c(sum(!is.na(P$nonhan))),
              fmt_c(sum(P$nonhan == 1, na.rm = TRUE)), fmt_c(sum(P$eth_other))))
  cat(sprintf("Hukou at age 3 known %s, from the 2018 item on hukou at age 12 %s; only codes 5, 7 or 79 %s\n",
              fmt_c(sum(!is.na(P$agri3))), fmt_c(sum(P$hk3_fb)), fmt_c(sum(P$hk3_other))))
  cat(sprintf("Birth year known %s, gender known %s, parents' education known %s, 2010 province known %s\n",
              fmt_c(sum(!is.na(P$birthy))), fmt_c(sum(!is.na(P$male))), fmt_c(sum(!is.na(P$paredu))),
              fmt_c(sum(!is.na(P$prov10)))))

  # 3. PSU labels. A wave whose county codes match the 2010 codes for at
  # least 95% of the respondents interviewed in both waves shares the 2010
  # labels; any other wave gets labels of its own.
  cat("\nPSUs\n")
  PFX <- c("10" = "c_")
  for (yy in c("12", "14", "16", "18")) {
    a <- D[[paste0("A", yy)]]
    x <- col_of(a, VM[[yy]]$psu)
    c10 <- P$cnty10[match(a$pid, P$pid)]
    ok <- nonneg(x) & !is.na(c10)
    sh <- if (any(ok)) mean(x[ok] == c10[ok]) else NA
    PFX[[yy]] <- if (!is.na(sh) && sh >= 0.95) "c_" else if (yy == "18") "p18_" else paste0("c", yy, "_")
    cat(sprintf("20%s: %s equal to the 2010 county for %s%% of the %s respondents with both, labels %s\n",
                yy, VM[[yy]]$psu, if (is.na(sh)) "no" else fmt_p(100 * sh), fmt_c(sum(ok)), PFX[[yy]]))
  }

  # PSU of each person for the frames that pool waves: the 2010 county, else
  # the 2018 PSU, else the earliest county of 2012 to 2016, else a PSU of
  # the person's own
  pool <- rep(NA_character_, nrow(P))
  ok <- !is.na(P$cnty10)
  pool[ok] <- paste0("c_", code_str(P$cnty10[ok]))
  n1 <- sum(ok)
  ok <- is.na(pool) & !is.na(P$psu18)
  pool[ok] <- paste0(PFX[["18"]], code_str(P$psu18[ok]))
  n2 <- sum(ok)
  n3 <- 0
  for (yy in c("12", "14", "16")) {
    a <- D[[paste0("A", yy)]]
    x <- coalesce_valid(a, VM[[yy]]$cnty, nonneg)[match(pid, a$pid)]
    ok <- is.na(pool) & !is.na(x)
    pool[ok] <- paste0(PFX[[yy]], code_str(x[ok]))
    n3 <- n3 + sum(ok)
  }
  own <- which(is.na(pool))
  pool[own] <- paste0("u_", own)
  cat(sprintf("Pooled PSU from the 2010 county %s, from the 2018 PSU %s, from a county of 2012 to 2016 %s, own PSU %s\n",
              fmt_c(n1), fmt_c(n2), fmt_c(n3), fmt_c(length(own))))

  # 4. Family income per person. The family size comes from the first size
  # variable of the file with any value of 1 or more, else from the number
  # of persons in the family roster of the wave.
  cat("\nFamily income\n")
  LINC <- list()
  for (yy in WAVES) {
    e <- D[[paste0("E", yy)]]
    r <- D[[paste0("R", yy)]]
    fv <- FIDV[[yy]]
    iv <- if (yy == "10") "faminc_net" else "fincome1"
    inc <- col_of(e, iv)
    inc[!nonneg(inc)] <- NA
    fs <- first_var(e, FS, function(x) !is.na(x) & x >= 1)
    size <- fs$x
    size[!is.na(size) & size < 1] <- NA
    rf <- r[[fv]]
    rc <- table(code_str(rf[!is.na(rf) & rf > 0]))
    fb <- is.na(size)
    size[fb] <- as.numeric(rc[code_str(e[[fv]][fb])])
    LINC[[yy]] <- list(fid = e[[fv]], linc = log1p(inc / size))
    cat(sprintf("20%s: income %s, family size %s, size from the roster for %s families, no size for %s, income missing for %s of %s families\n",
                yy, iv, fs$name, fmt_c(sum(fb & !is.na(size))), fmt_c(sum(is.na(size))),
                fmt_c(sum(is.na(inc))), fmt_c(nrow(e))))
  }

  # 5. Frames
  cat("\nFrames\n")
  W <- list()
  for (yy in WAVES) W[[yy]] <- wave_frame(yy, D, P, PFX, pool, LINC)
  C10 <- child_frame(D, P, pool, LINC)
  F18 <- panel_frame(D, P, pool, W[["18"]], C10)
  C10$followed <- as.numeric(C10$pid %in% F18$pid[!is.na(F18$edu18)])
  BE <- enrol_frame(W, P, pool)
  FR <- c(setNames(W, paste0("W", WAVES)), list(C10 = C10, F18 = F18, BE = BE))
  rm(D, W, C10, F18, BE)
  for (nm in names(FR)) {
    f <- FR[[nm]]
    f$pid <- NULL
    f <- data.frame(.row = seq_len(nrow(f)), f, check.names = FALSE)
    rownames(f) <- NULL
    FR[[nm]] <- f
  }

  # 6. Samples and their flows
  ROWS <- list()
  FRAME_OF <- character(0)
  FLOWS <- NULL
  add <- function(name, frame, steps) {
    fl <- flow(name, FR[[frame]], steps)
    ROWS[[name]] <<- fl$rows
    FRAME_OF[[name]] <<- frame
    FLOWS <<- rbind(FLOWS, fl$flow)
  }
  a_steps <- function(f) {
    list("All adult respondents" = rep(TRUE, nrow(f)),
         "Aged 25 or older" = f$age >= 25,
         "Positive weight" = f$w > 0,
         "Ethnicity known" = !is.na(f$nonhan),
         "Hukou at age 3 known" = !is.na(f$agri3),
         "Birth year, gender and province known" = known(f, "birthy", "male", "prov"),
         "Education known" = !is.na(f$edu))
  }
  c_steps <- function(f, label, ok) {
    st <- list("All adult respondents" = rep(TRUE, nrow(f)),
               "Aged 16 or older" = f$age >= 16,
               "Positive weight" = f$w > 0,
               "Ethnicity known" = !is.na(f$nonhan),
               "Hukou at age 3 known" = !is.na(f$agri3),
               "Birth year, gender and province known" = known(f, "birthy", "male", "prov"))
    st[[label]] <- ok
    st
  }
  b_steps <- function(f, lo, hi, lang) {
    st <- list("Respondents of 2018 with a positive panel weight" = rep(TRUE, nrow(f)),
               "Child respondents in 2010" = f$in_c10 == 1)
    st[[sprintf("Aged %d to %d in 2010", lo, hi)]] <- inr(f$age10, lo, hi)
    st[["Education in 2018 known"]] <- !is.na(f$edu18)
    st[["Ethnicity known"]] <- !is.na(f$nonhan)
    st[["Hukou at age 3 known"]] <- !is.na(f$agri3)
    st[["Gender and province in 2010 known"]] <- known(f, "male", "prov10")
    if (lang) {
      st[["Home language, Mandarin rating and word test in 2010 known"]] <- known(f, "lang10", "rating10", "wordtest10")
    }
    st
  }

  for (yy in WAVES) add(paste0("A-20", yy), paste0("W", yy), a_steps(FR[[paste0("W", yy)]]))
  for (yy in c("10", "12")) {
    f <- FR[[paste0("W", yy)]]
    add(paste0("C-lang-20", yy), paste0("W", yy), c_steps(f, "Home language known", !is.na(f$lang)))
  }
  for (yy in c("10", "12", "14", "16")) {
    f <- FR[[paste0("W", yy)]]
    add(paste0("C-rating-20", yy), paste0("W", yy), c_steps(f, "Mandarin rating known", !is.na(f$rating)))
  }
  for (yy in c("10", "14", "18")) {
    f <- FR[[paste0("W", yy)]]
    add(paste0("C-wordtest-20", yy), paste0("W", yy), c_steps(f, "Word test known", !is.na(f$wordtest)))
  }
  f <- FR$W10
  st <- a_steps(f)
  st[["Home language, Mandarin rating and word test known"]] <- known(f, "lang", "rating", "wordtest")
  add("D-A10", "W10", st)
  add("B-cohort", "F18", b_steps(FR$F18, 12, 15, FALSE))
  add("B-cohort-lang", "F18", b_steps(FR$F18, 12, 15, TRUE))
  add("B-cohort-10to15", "F18", b_steps(FR$F18, 10, 15, FALSE))
  add("B-cohort-lang-10to15", "F18", b_steps(FR$F18, 10, 15, TRUE))
  f <- FR$BE
  for (r in list(c(16, 22), c(16, 18), c(19, 22))) {
    st <- list("Adult respondents aged 16 to 22, waves 2010 to 2018" = rep(TRUE, nrow(f)))
    st[[sprintf("Aged %d to %d", r[1], r[2])]] <- inr(f$age, r[1], r[2])
    st[["Positive weight"]] <- f$w > 0
    st[["Ethnicity known"]] <- !is.na(f$nonhan)
    st[["Hukou at age 3 known"]] <- !is.na(f$agri3)
    st[["Gender and province known"]] <- known(f, "male", "prov")
    st[["Enrolment known"]] <- !is.na(f$enrol)
    add(sprintf("B-enrol-%d-%d", r[1], r[2]), "BE", st)
  }
  for (yy in c("12", "14", "16")) {
    f <- FR[[paste0("W", yy)]]
    add(paste0("R15-rating-20", yy), paste0("W", yy),
        c_steps(f, "Mandarin rating known, with code 79 as a category", !is.na(f$rating0)))
  }

  cat("\nSamples\n")
  for (nm in names(ROWS)) {
    f <- FR[[FRAME_OF[[nm]]]]
    r <- ROWS[[nm]]
    cat(sprintf("%-22s frame %-4s respondents %s, non-Han %s\n", nm, FRAME_OF[[nm]], fmt_c(length(r)),
                fmt_c(sum(f$nonhan[r] == 1))))
  }

  # 7. Result
  L <- list(frames = FR, rows = ROWS, frame_of = FRAME_OF, flows = FLOWS, labels = labels, pfx = PFX)
  saveRDS(L, file.path(WORK, "link.rds"))
  cat(sprintf("\nTime %.0f seconds\n", proc.time()[["elapsed"]] - t0))
  cat("\n02_link.R finished. The frames and samples are in the file link.rds in the folder Work on the disk.\n")
  invisible(NULL)
}

cfps_run("02_link", cfps_link)
