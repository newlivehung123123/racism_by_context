#==============================================================================
# 05_checks.R
# Structural inequalities in education in China, CFPS 2010 to 2018
#
# Runs the checks of section 7 of the analysis plan on the samples of
# 04_models.R, with the imputed data sets that 04_models.R saved in the file
# imp.rds in the folder Work. Writes into the folder Output on the disk:
# S7_robust          Table S7, the effects of non-Han ethnicity under each
#                    check, with the columns check and variant
# S7_oaxaca          Table S7, check R8, the Oaxaca and Blinder decomposition
#                    of years of schooling
# S7_r10_selection   Table S7, check R10, the Wald tests that decide which
#                    terms stay free across the education levels
#
# The checks. Check R4 (telephone interviews) needs the waves 2020 and 2022
# and is not run.
# R1   no weights
# R2   complete cases, no imputation
# R3   proxy interviews excluded (waves 2014 to 2018)
# R5   each non-Han group with at least 100 respondents in the sample,
#      against Han
# R6   models by decade of birth, and the transition to tertiary education
#      for people born in 1980 or earlier and in 1981 or later
# R7   hukou at age 3 only (adults whose value came from hukou at age 12
#      are excluded)
# R8   years of schooling, Oaxaca and Blinder decomposition
# R9   the 2010 province in place of the province of 2018
# R10  partial proportional odds models
# R11  the national resample with the resample weights (waves 2010 to 2016)
# R12  transitions by the highest stage attended
# R13  six education levels
# R14  children aged 10 to 15 in 2010 in place of 12 to 15
# R15  code 79 of the Mandarin rating as a category below the lowest rating
#
# How to run. Quit every AI app, then double-click Run analysis.command in the
# folder R scripts, which runs 02_link.R to 05_checks.R in turn. Run by hand,
# the script needs 04_models.R to have run first.
#
# Status. Written on 23 October 2026 and tested on made-up data only. The
# functions of analysis_tools.R that fit the models were revised the same
# day, after the first run on the CFPS data and before any table of
# estimates was read, and tested on made-up data only. They were revised
# again on 24 October 2026, after the second run and before any table of
# estimates of that run was read (models with a separated group of non-Han
# respondents, and the convergence of check R10), and tested on made-up data
# only.
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


# Samples of the main tables used by most checks
A2 <- c("A-2010", "A-2018")
C10 <- c("C-lang-2010", "C-rating-2010", "C-wordtest-2010")

# Decades of birth of check R6
DECADES <- list("born before 1940" = c(1900, 1939), "born 1940 to 1949" = c(1940, 1949),
                "born 1950 to 1959" = c(1950, 1959), "born 1960 to 1969" = c(1960, 1969),
                "born 1970 to 1979" = c(1970, 1979), "born 1980 to 1989" = c(1980, 1989),
                "born 1990 or later" = c(1990, 2018))

# Smallest number of respondents of a non-Han group in check R5
MIN_GROUP <- 100

# Outcome and model type of a sample of Table III
c_spec <- function(s) as.list(C_SAMPLES[C_SAMPLES$sample == s, c("y", "type")])

# Jobs of the checks
check_jobs <- function(L) {
  J <- list()
  add <- function(j) J[[length(J) + 1]] <<- j
  S7 <- "S7_robust"

  # R1 and R2: the main models without weights, and on complete cases
  for (ck in c("R1", "R2")) {
    r1 <- ck == "R1"
    va <- if (r1) "no weights" else "complete cases"
    imp_of <- function(s) if (r1) s else NULL
    known <- function(vars) if (r1) NULL else function(f) all_known(f, vars)
    for (s in A2) {
      add(sample_job(L, s, "M4", "edu5", "olr", TERMS$M4, imp = imp_of(s), keep = known("paredu"),
                     equal = r1, check = ck, variant = va, table = S7))
    }
    for (y in c("y_sh", "y_ter")) {
      add(sample_job(L, "B-cohort", "base", y, "logit", TERMS_B, imp = imp_of("B-cohort"),
                     keep = known(c("paredu", "linc10")), src = SRC_B, equal = r1, check = ck,
                     variant = va, table = S7))
    }
    for (s in C10) {
      cs <- c_spec(s)
      add(sample_job(L, s, "M4", cs$y, cs$type, TERMS$M4, imp = imp_of(s), keep = known("paredu"),
                     equal = r1, check = ck, variant = va, table = S7))
    }
    add(sample_job(L, "D-A10", "step 2", "edu5", "olr", TERMS$M4, imp = imp_of("D-A10"),
                   keep = known("paredu"), meds = KHB_STEPS[["step 2"]], kind = "khb", equal = r1,
                   check = ck, variant = va, table = S7))
  }

  # R3: proxy interviews excluded
  for (s in c("A-2014", "A-2016", "A-2018")) {
    add(sample_job(L, s, "M4", "edu5", "olr", TERMS$M4, keep = function(f) f$selfrpt == 1,
                   check = "R3", variant = "proxy interviews excluded", table = S7))
  }
  for (y in c("y_sh", "y_ter")) {
    add(sample_job(L, "B-cohort", "base", y, "logit", TERMS_B, keep = function(f) f$selfrpt18 == 1,
                   src = SRC_B, check = "R3", variant = "proxy interviews excluded", table = S7))
  }

  # R5: each non-Han group with at least MIN_GROUP respondents, against Han
  for (s in A2) {
    f <- L$frames[[L$frame_of[[s]]]]
    r <- L$rows[[s]]
    r <- r[f$nonhan[r] %in% 1 & !is.na(f$edu5[r])]
    tab <- table(f$ethcode[r])
    for (g in as.numeric(names(tab)[tab >= MIN_GROUP])) {
      nm <- unname(L$labels$eth[as.character(g)])
      if (length(nm) != 1 || is.na(nm) || !nzchar(nm)) nm <- paste("ethnic group code", g)
      add(sample_job(L, s, "M4", "edu5", "olr", TERMS$M4, keep = function(f) f$ethcode %in% c(1, g),
                     check = "R5", variant = nm, table = S7))
    }
  }

  # R6: models by decade of birth, and the transition to tertiary education
  # before and after the expansion of higher education
  for (s in A2) {
    f <- L$frames[[L$frame_of[[s]]]]
    by <- f$birthy[L$rows[[s]]]
    for (dn in names(DECADES)) {
      lo <- DECADES[[dn]][1]
      hi <- DECADES[[dn]][2]
      if (!any(inr(by, lo, hi))) next
      add(sample_job(L, s, "M4", "edu5", "olr", TERMS$M4, keep = function(f) inr(f$birthy, lo, hi),
                     check = "R6", variant = dn, table = S7))
    }
  }
  add(sample_job(L, "A-2018", "M4", "t4", "logit", TERMS$M4,
                 keep = function(f) inr(f$birthy, 1900, 1980), check = "R6",
                 variant = "born 1980 or earlier", table = S7))
  add(sample_job(L, "A-2018", "M4", "t4", "logit", TERMS$M4,
                 keep = function(f) inr(f$birthy, 1981, 2018), check = "R6",
                 variant = "born 1981 or later", table = S7))

  # R7: hukou at age 3 only
  add(sample_job(L, "A-2018", "M4", "edu5", "olr", TERMS$M4, keep = function(f) f$hk3_fb == 0,
                 check = "R7", variant = "hukou at age 3 only", table = S7))

  # R8: years of schooling, Oaxaca and Blinder decomposition
  for (s in A2) {
    add(sample_job(L, s, "Oaxaca", "eduy", "linear", c("agri3", "cohort", "male", "paredu", "prov"),
                   kind = "oaxaca", check = "R8", table = "S7_oaxaca"))
  }

  # R9: the 2010 province
  add(sample_job(L, "A-2018", "M4", "edu5", "olr", TERMS$M4, keep = function(f) !is.na(f$prov10),
                 src = list(prov = "prov10"), check = "R9", variant = "province of 2010", table = S7))

  # R10: partial proportional odds models
  for (s in A2) {
    add(sample_job(L, s, "M4", "edu5", "olr", TERMS$M4, kind = "r10", check = "R10",
                   variant = "partial proportional odds", table = S7))
  }

  # R11: the national resample with the resample weights
  for (s in c("A-2010", "A-2012", "A-2014", "A-2016", C10)) {
    cs <- if (s %in% C10) c_spec(s) else list(y = "edu5", type = "olr")
    add(sample_job(L, s, "M4", cs$y, cs$type, TERMS$M4,
                   keep = function(f) f$subsample %in% 1 & f$wres > 0, wcol = "wres", check = "R11",
                   variant = "national resample", table = S7))
  }

  # R12: transitions by the highest stage attended
  for (s in A2) {
    for (k in 1:4) {
      add(sample_job(L, s, "M4", paste0("s", k), "logit", TERMS$M4, check = "R12",
                     variant = "highest stage attended", table = S7))
    }
  }

  # R13: six education levels
  for (s in A2) {
    add(sample_job(L, s, "M4", "edu6", "olr", TERMS$M4, check = "R13",
                   variant = "six education levels", table = S7))
  }

  # R14: children aged 10 to 15 in 2010
  for (y in c("y_sh", "y_ter")) {
    va <- "aged 10 to 15 in 2010"
    add(sample_job(L, "B-cohort-10to15", "base", y, "logit", TERMS_B, src = SRC_B, check = "R14",
                   variant = va, table = S7))
    add(sample_job(L, "B-cohort-lang-10to15", "base, language sample", y, "logit", TERMS_B,
                   imp = "B-cohort-10to15", src = SRC_B, check = "R14", variant = va, table = S7))
    add(sample_job(L, "B-cohort-lang-10to15", "base + language", y, "logit", c(TERMS_B, LANG_TERMS),
                   imp = "B-cohort-10to15", src = SRC_B, check = "R14", variant = va, table = S7))
  }

  # R15: code 79 of the Mandarin rating as a category below the lowest rating
  for (yy in c("12", "14", "16")) {
    for (mo in c("M4", "M4 + education")) {
      add(sample_job(L, paste0("R15-rating-20", yy), mo, "rating0", "olr",
                     if (mo == "M4") TERMS$M4 else c(TERMS$M4, "edu"), check = "R15",
                     variant = "code 79 as the lowest category", table = S7))
    }
  }
  J
}

TABLES_05 <- c("S7_robust", "S7_oaxaca", "S7_r10_selection")


#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
cfps_checks <- function() {
  t0 <- proc.time()[["elapsed"]]
  L <- readRDS(file.path(WORK, "link.rds"))
  IMP <- readRDS(file.path(WORK, "imp.rds"))
  cat(sprintf("Cores %d, imputed data sets %d, imputed samples %d\n", CORES,
              max(vapply(IMP, `[[`, 1L, "m")), length(IMP)))

  jobs <- check_jobs(L)
  ck <- vapply(jobs, `[[`, "", "check")
  n_by <- table(factor(ck, levels = unique(ck)))
  cat("Jobs by check:", paste(names(n_by), n_by, collapse = ", "), "\n\n")
  res <- run_all(jobs, L, IMP)
  tabs <- collect_results(jobs, res)
  cat("\n")
  for (nm in TABLES_05) write_out(nm, tabs[[nm]])
  cat("\n")
  log_khb_check(jobs, res)
  log_not_estimable(tabs)
  log_refitted(tabs)

  cat(sprintf("\nTime %.0f minutes\n", (proc.time()[["elapsed"]] - t0) / 60))
  cat("\n05_checks.R finished.\n")
  invisible(NULL)
}

cfps_run("05_checks", cfps_checks)
