#==============================================================================
# 04_models.R
# Structural inequalities in education in China, CFPS 2010 to 2018
#
# Imputes the missing values of parents' education, of education and of
# family income, and fits the models of the main tables and of the
# supplement. Writes into the folder Output on the disk:
# T2_ame_main      Table II, effects of non-Han ethnicity on education,
#                  A-2010 and A-2018, models M1 to M4
# S4_transitions   the four school transitions, A-2010 and A-2018
# S5_ame_other     Tables II and S4 for A-2012, A-2014 and A-2016
# T3_ame_language  Table III, effects of non-Han ethnicity on home language,
#                  Mandarin rating and word test
# T4_khb           Table IV, panel A, the part of the effect on education
#                  that runs through language (KHB), sample D-A10
# T4_bcohort       Table IV, panel B, children of 2010 aged 12 to 15 and their
#                  education in 2018
# S6_ame_enrol     school enrolment at ages 16 to 22, waves 2010 to 2018
# S3_coef_A        coefficients of the models of Tables II and S4
# S3_coef_Aother   coefficients of the models of Table S5
# S3_coef_C        coefficients of the models of Table III
# S3_coef_B        coefficients of the models of Table IV, panel B, and S6
# S8_imputation    missing and imputed values of each sample
#
# Every effect is an average marginal effect of non-Han ethnicity, for all
# respondents, for each group of hukou at age 3 and for the difference
# between the two groups, pooled over the imputed data sets by Rubin's rules.
# Standard errors come from the survey design (PSUs and strata) by the delta
# method. An effect with fewer than 10 non-Han respondents is left blank.
#
# The imputed data sets go into the file imp.rds in the folder Work on the
# disk, for 05_checks.R.
#
# How to run. Quit every AI app, then double-click Run analysis.command in the
# folder R scripts, which runs 02_link.R to 05_checks.R in turn. Run by hand,
# the script needs 02_link.R to have run first.
#
# Status. Written on 23 October 2026 and tested on made-up data only. The
# functions of analysis_tools.R that fit the models were revised the same
# day, after the first run on the CFPS data and before any table of
# estimates was read, and tested on made-up data only. They were revised
# again on 24 October 2026, after the second run and before any table of
# estimates of that run was read (models with a separated group of non-Han
# respondents, and the convergence of check R10), and tested on made-up data
# only. The log now also lists the events logged by mice.
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


# Predictors of the imputation models (section 6.6 of the analysis plan)
P_A <- c("nonhan", "agri3", "nh_ag", "male", "cohort", "prov", "w")
P_B <- c("y_sh", "y_ter", "nonhan", "agri3", "nh_ag", "male", "age10", "prov10", "w")
P_BE <- c("enrol", "nonhan", "agri3", "nh_ag", "male", "age", "wave", "prov", "w")

# Imputation runs: the sample, the variables imputed, the predictors and the
# predictors that enter as categories. A subset of a sample, and each check
# of 05_checks.R on a subset, uses the imputed data sets of the sample.
imp_runs <- function() {
  r <- list()
  add <- function(s, imp, pred, fac) {
    r[[length(r) + 1]] <<- list(name = paste0("Imputation, ", s), sample = s, imp = imp,
                                pred = pred, fac = fac)
  }
  for (yy in WAVES) add(paste0("A-20", yy), "paredu", c("edu5", P_A), "prov")
  for (i in seq_len(nrow(C_SAMPLES))) {
    y <- C_SAMPLES$y[i]
    add(C_SAMPLES$sample[i], c("paredu", "edu5"), c(y, P_A), c("prov", if (y == "lang") "lang"))
  }
  add("D-A10", "paredu", c("edu5", P_A, LANG_TERMS), c("prov", "lang"))
  add("B-cohort", c("paredu", "linc10"), P_B, "prov10")
  add("B-cohort-lang", c("paredu", "linc10"), c(P_B, "lang10", "rating10", "wordtest10"),
      c("prov10", "lang10"))
  add("B-cohort-10to15", c("paredu", "linc10"), P_B, "prov10")
  add("B-enrol-16-22", c("paredu", "linc"), P_BE, c("prov", "wave"))
  for (yy in c("12", "14", "16")) {
    add(paste0("R15-rating-20", yy), c("paredu", "edu5"), c("rating0", P_A), "prov")
  }
  r
}

# Columns cols of the rows of the sample s, with the row number .row
sample_data <- function(L, s, cols) {
  f <- L$frames[[L$frame_of[[s]]]]
  d <- f[L$rows[[s]], c(".row", cols), drop = FALSE]
  rownames(d) <- NULL
  d
}

# Model jobs of the main tables and of the tables S3 to S6
model_jobs <- function(L) {
  J <- list()
  add <- function(j) J[[length(J) + 1]] <<- j

  # Samples A: education (five levels) with models M1 to M4, and the four
  # school transitions with model M4
  for (yy in WAVES) {
    s <- paste0("A-20", yy)
    main <- s %in% c("A-2010", "A-2018")
    ct <- if (main) "S3_coef_A" else "S3_coef_Aother"
    for (mo in names(TERMS)) {
      add(sample_job(L, s, mo, "edu5", "olr", TERMS[[mo]], imp = if (mo %in% c("M1", "M2")) NULL else s,
                     table = if (main) "T2_ame_main" else "S5_ame_other", coef_table = ct))
    }
    for (k in 1:4) {
      add(sample_job(L, s, "M4", paste0("t", k), "logit", TERMS$M4,
                     table = if (main) "S4_transitions" else "S5_ame_other", coef_table = ct))
    }
  }

  # Samples C: home language, Mandarin rating and word test, without and
  # with the respondent's education
  for (i in seq_len(nrow(C_SAMPLES))) {
    for (mo in c("M4", "M4 + education")) {
      add(sample_job(L, C_SAMPLES$sample[i], mo, C_SAMPLES$y[i], C_SAMPLES$type[i],
                     if (mo == "M4") TERMS$M4 else c(TERMS$M4, "edu"),
                     table = "T3_ame_language", coef_table = "S3_coef_C"))
    }
  }

  # Sample D-A10: KHB decomposition of the effect on education, with home
  # language (step 1) and then all three language measures (step 2)
  for (st in names(KHB_STEPS)) {
    add(sample_job(L, "D-A10", st, "edu5", "olr", TERMS$M4, meds = KHB_STEPS[[st]], kind = "khb",
                   table = "T4_khb"))
  }

  # Samples B-cohort: senior high school and college or above by 2018
  for (y in c("y_sh", "y_ter")) {
    add(sample_job(L, "B-cohort", "base", y, "logit", TERMS_B, src = SRC_B,
                   table = "T4_bcohort", coef_table = "S3_coef_B"))
    add(sample_job(L, "B-cohort-lang", "base, language sample", y, "logit", TERMS_B, src = SRC_B,
                   table = "T4_bcohort", coef_table = "S3_coef_B"))
    add(sample_job(L, "B-cohort-lang", "base + language", y, "logit", c(TERMS_B, LANG_TERMS),
                   src = SRC_B, table = "T4_bcohort", coef_table = "S3_coef_B"))
  }

  # Samples B-enrol: school enrolment, waves 2010 to 2018 pooled
  for (s in c("B-enrol-16-22", "B-enrol-16-18", "B-enrol-19-22")) {
    add(sample_job(L, s, "base", "enrol", "logit", TERMS_BE, imp = "B-enrol-16-22",
                   table = "S6_ame_enrol", coef_table = "S3_coef_B"))
  }
  J
}

TABLES_04 <- c("T2_ame_main", "S4_transitions", "S5_ame_other", "T3_ame_language", "T4_khb",
               "T4_bcohort", "S6_ame_enrol", "S3_coef_A", "S3_coef_Aother", "S3_coef_C", "S3_coef_B")


#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
cfps_models <- function() {
  t0 <- proc.time()[["elapsed"]]
  L <- readRDS(file.path(WORK, "link.rds"))
  cat(sprintf("Cores %d, imputed data sets %d\n", CORES, M_IMP))

  # 1. Imputation
  cat("\nImputation\n")
  runs <- imp_runs()
  ires <- run_jobs(runs, function(r) {
    d <- sample_data(L, r$sample, unique(c(r$imp, r$pred, "nonhan")))
    impute_sample(r$sample, d, r$imp, r$pred, r$fac)
  })
  IMP <- list()
  for (i in seq_along(runs)) {
    if (!inherits(ires[[i]], "job_error")) IMP[[runs[[i]]$sample]] <- ires[[i]]
  }
  cat(sprintf("Imputation runs finished %d of %d\n", length(IMP), length(runs)))
  tryCatch(log_imp_events(IMP), error = function(e) cat("The events logged by mice could not be listed\n"))
  saveRDS(IMP, file.path(WORK, "imp.rds"))
  write_out("S8_imputation", do.call(rbind, lapply(IMP, `[[`, "s8")))

  # 2. Models
  cat("\nModels\n")
  jobs <- model_jobs(L)
  res <- run_all(jobs, L, IMP)
  tabs <- collect_results(jobs, res)
  cat("\n")
  for (nm in TABLES_04) write_out(nm, tabs[[nm]])
  cat("\n")
  log_khb_check(jobs, res)
  log_not_estimable(tabs)
  log_refitted(tabs)

  cat(sprintf("\nTime %.0f minutes\n", (proc.time()[["elapsed"]] - t0) / 60))
  cat("\n04_models.R finished.\n")
  invisible(NULL)
}

cfps_run("04_models", cfps_models)
