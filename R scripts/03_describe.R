#==============================================================================
# 03_describe.R
# Structural inequalities in education in China, CFPS 2010 to 2018
#
# Writes the descriptive tables into the folder Output on the disk:
# T1_describe     Table I, the samples A-2010 and A-2018 by ethnicity and
#                 hukou at age 3
# F1_transitions  Figure 1, the four school transitions by birth cohort
# S1_flow         respondents kept at each step of every sample, and the
#                 answers in codes that the analysis sets to missing
# S2_groups       non-Han respondents by ethnic group
# S2_provinces    respondents by province
# S9_attrition    children of 2010 aged 12 to 15, followed up to 2018 or lost
# S10_classmates  language of the classmates, and the interview language of
#                 2018
# S11_cohorts     education of the same birth cohorts in 2010 and 2018
#
# Percentages and means are weighted and come with their standard errors from
# the survey design. Every count from 1 to 9 is shown as <10, a second cell is
# hidden where a hidden count could be worked out from the others, and the
# estimates of a hidden cell are left blank.
#
# How to run. Quit every AI app, then double-click Run analysis.command in the
# folder R scripts, which runs 02_link.R to 05_checks.R in turn. Run by hand,
# the script needs 02_link.R to have run first.
#
# Status. Written on 23 October 2026 and tested on made-up data only.
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


EDU_NAMES <- c("Illiterate or semi-literate", "Primary school", "Junior high school",
               "Senior high school", "College or above")
TRANS_NAMES <- c("T1 Primary school", "T2 Junior high school", "T3 Senior high school", "T4 College")

# Birth cohorts of Figure 1: up to 1985 from A-2010, later from A-2018
F1_COHORTS <- rbind(
  data.frame(source = "A-2010",
             cohort = c("<1940", sprintf("%d-%d", seq(1940, 1975, 5), seq(1944, 1979, 5)), "1980-1985"),
             lo = c(1900, seq(1940, 1975, 5), 1980), hi = c(1939, seq(1944, 1979, 5), 1985)),
  data.frame(source = "A-2018", cohort = c("1986-1989", "1990-1993"), lo = c(1986, 1990),
             hi = c(1989, 1993)))

# Birth cohorts interviewed in both 2010 and 2018
S11_COHORTS <- data.frame(cohort = c("1960-1964", "1965-1969", "1970-1974", "1975-1979", "1980-1985"),
                          lo = c(1960, 1965, 1970, 1975, 1980), hi = c(1964, 1969, 1974, 1979, 1985))

# Samples of the tables S2
S2_SAMPLES <- c("A-2010", "A-2012", "A-2014", "A-2016", "A-2018", "B-cohort", "B-enrol-16-22")

# Groups of ethnicity and of ethnicity by hukou at age 3
eth_groups <- function(v, all = FALSE) {
  g <- list("Han" = v$nonhan == 0, "Non-Han" = v$nonhan == 1)
  if (all) g$All <- rep(TRUE, nrow(v))
  g
}
eth_hukou_groups <- function(v) {
  list("Han, agricultural" = v$nonhan == 0 & v$agri3 == 1,
       "Han, non-agricultural" = v$nonhan == 0 & v$agri3 == 0,
       "Non-Han, agricultural" = v$nonhan == 1 & v$agri3 == 1,
       "Non-Han, non-agricultural" = v$nonhan == 1 & v$agri3 == 0)
}

# Items of a descriptive table. type "pct" is the percentage with x equal to
# 1, with the respondents with x equal to 1 as the count and those with x
# equal to 0 as the second count; "level" is one level of a variable with
# several levels; "mean" is the mean, with the respondents with a value as
# the count; "n" is the number of respondents.
item <- function(variable, level, type, x = NULL) list(variable = variable, level = level, type = type, x = x)
levels_of <- function(variable, x, codes, names) {
  lapply(seq_along(codes), function(i) item(variable, names[i], "level", ifelse(is.na(x), NA, as.numeric(x == codes[i]))))
}

# Estimates of the items for each group, in the design des. The groups and
# the items run along the rows of des.
describe <- function(des, groups, items) {
  out <- list()
  for (g in names(groups)) {
    keep <- groups[[g]]
    keep[is.na(keep)] <- FALSE
    for (it in items) {
      x <- it$x
      if (it$type == "n") {
        r <- list(est = NA_real_, se = NA_real_)
        n <- sum(keep)
        also <- NA
      } else {
        r <- est_mean(des, keep, x)
        k <- if (it$type == "mean") 1 else 100
        r$est <- r$est * k
        r$se <- r$se * k
        n <- if (it$type == "mean") sum(keep & !is.na(x)) else sum(keep & !is.na(x) & x == 1)
        also <- if (it$type == "pct") sum(keep & !is.na(x) & x == 0) else NA
      }
      out[[length(out) + 1]] <- data.frame(group = g, variable = it$variable, level = it$level,
                                           estimate = r$est, se = r$se, n = n, n_also = also,
                                           stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

# Design for the rows of a sample
sample_design <- function(L, s, wcol = "w") {
  f <- L$frames[[L$frame_of[[s]]]]
  dom_design(f, seq_len(nrow(f)) %in% L$rows[[s]], wcol)
}

# Respondents with a code that the analysis sets to missing, for S1
info_row <- function(step, f, flag) {
  g <- ifelse(is.na(f$nonhan), "Ethnicity missing", ifelse(f$nonhan == 1, "Non-Han", "Han"))
  ok <- !is.na(flag) & flag == 1
  n <- c(Han = sum(ok & g == "Han"), "Non-Han" = sum(ok & g == "Non-Han"),
         "Ethnicity missing" = sum(ok & g == "Ethnicity missing"), All = sum(ok))
  data.frame(sample = "Codes set to missing", step = step, group = names(n), n = unname(n),
             stringsAsFactors = FALSE)
}


#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
cfps_describe <- function() {
  t0 <- proc.time()[["elapsed"]]
  L <- readRDS(file.path(WORK, "link.rds"))
  FR <- L$frames

  # Table I
  t1 <- NULL
  for (s in c("A-2010", "A-2018")) {
    des <- sample_design(L, s)
    v <- des$variables
    items <- c(list(item("Hukou at age 3", "Agricultural", "pct", v$agri3),
                    item("Current hukou", "Agricultural", "pct", v$curhk),
                    item("Gender", "Male", "pct", v$male),
                    item("Age", "Mean", "mean", v$age),
                    item("Parents' education", "Mean level, 1 to 5", "mean", v$paredu)),
               levels_of("Education", v$edu5, 1:5, EDU_NAMES),
               list(item("Years of schooling", "Mean", "mean", v$eduy)))
    if (s == "A-2010") {
      items <- c(items, levels_of("Home language", v$lang, c(1, 2, 3), c("Mandarin", "Minority language", "Chinese dialect")),
                 list(item("Mandarin rating", "Mean, 1 to 7", "mean", v$rating)))
    }
    items <- c(items, list(item("Word test", "Mean", "mean", v$wordtest), item("Respondents", "n", "n")))
    d <- describe(des, c(eth_groups(v), eth_hukou_groups(v)), items)
    d$sample <- s
    t1 <- rbind(t1, d)
  }
  t1$ethset <- sub(",.*$", "", t1$group)
  t1 <- suppress_small(t1, "n", groups = list(c("sample", "group", "variable"), c("sample", "variable", "level", "ethset")),
                       stats = c("estimate", "se"), also = "n_also")
  write_out("T1_describe", t1)

  # Figure 1
  f1 <- NULL
  for (src in unique(F1_COHORTS$source)) {
    des <- sample_design(L, src)
    v <- des$variables
    gr <- eth_hukou_groups(v)
    for (i in which(F1_COHORTS$source == src)) {
      co <- inr(v$birthy, F1_COHORTS$lo[i], F1_COHORTS$hi[i])
      for (g in names(gr)) {
        for (k in 1:4) {
          r <- est_mean(des, co & gr[[g]], v[[paste0("t", k)]])
          f1 <- rbind(f1, data.frame(source = src, cohort = F1_COHORTS$cohort[i], group = g,
                                     transition = TRANS_NAMES[k], estimate = 100 * r$est,
                                     se = 100 * r$se, n = r$n, stringsAsFactors = FALSE))
        }
      }
    }
  }
  f1 <- suppress_small(f1, "n", stats = c("estimate", "se"))
  write_out("F1_transitions", f1)

  # S1, with the codes set to missing
  s1 <- L$flows
  for (yy in WAVES) {
    f <- FR[[paste0("W", yy)]]
    if (yy %in% c("10", "12")) {
      s1 <- rbind(s1, info_row(sprintf("Adult respondents 20%s, home language in %s", yy,
                                       if (yy == "10") "code 77" else "codes 3, 4 or another code"), f, f$lang_other))
    }
    if (yy %in% c("12", "14", "16")) {
      s1 <- rbind(s1, info_row(sprintf("Adult respondents 20%s, Mandarin rating in code 79", yy), f, f$rating79))
    }
    s1 <- rbind(s1, info_row(sprintf("Adult respondents 20%s, hukou at age 3 only in codes 5, 7 or 79", yy), f, f$hk3_other),
                info_row(sprintf("Adult respondents 20%s, ethnicity only in codes outside 1 to 60", yy), f, f$eth_other))
  }
  s1 <- rbind(s1, info_row("Child respondents 2010, home language in code 77", FR$C10, FR$C10$lang_other),
              info_row("Adult respondents 2018, hukou at age 3 from the item on hukou at age 12", FR$W18, FR$W18$hk3_fb))
  s1 <- suppress_small(s1, "n", groups = list(c("sample", "step")))
  write_out("S1_flow", s1)

  # S2, ethnic groups and provinces
  s2g <- NULL
  s2p <- NULL
  for (s in S2_SAMPLES) {
    f <- FR[[L$frame_of[[s]]]]
    r <- L$rows[[s]]
    e <- f$ethcode[r][f$nonhan[r] == 1]
    tab <- table(e)
    d <- data.frame(sample = s, ethcode = as.numeric(names(tab)), n = as.numeric(tab), stringsAsFactors = FALSE)
    d$name <- unname(L$labels$eth[as.character(d$ethcode)])
    d$pct <- 100 * d$n / sum(d$n)
    s2g <- rbind(s2g, d)
    pv <- if (grepl("^B-cohort", s)) f$prov10[r] else f$prov[r]
    g <- factor(ifelse(f$nonhan[r] == 1, "Non-Han", "Han"), levels = c("Han", "Non-Han"))
    tab <- as.data.frame(table(prov_code = pv, group = g), stringsAsFactors = FALSE)
    tab$prov_code <- as.numeric(tab$prov_code)
    tab$prov_name <- unname(L$labels$prov[as.character(tab$prov_code)])
    s2p <- rbind(s2p, data.frame(sample = s, tab[, c("prov_code", "prov_name", "group")], n = tab$Freq,
                                 stringsAsFactors = FALSE))
  }
  s2g <- suppress_small(s2g, "n", groups = list("sample"), stats = "pct")
  write_out("S2_groups", s2g)
  s2p <- suppress_small(s2p, "n", groups = list(c("sample", "group")))
  write_out("S2_provinces", s2p)

  # S9, children of 2010 aged 12 to 15, followed up to 2018 or lost
  f <- FR$C10
  des <- dom_design(f, inr(f$age, 12, 15))
  v <- des$variables
  gr <- eth_groups(v, all = TRUE)
  st <- list("Followed up" = v$followed == 1, "Lost" = v$followed == 0)
  items <- list(item("Agricultural hukou at age 3 (%)", "", "pct", v$agri3),
                item("Male (%)", "", "pct", v$male),
                item("Mandarin at home (%)", "", "pct", ifelse(is.na(v$lang), NA, as.numeric(v$lang == 1))),
                item("Parents' education, mean level 1 to 5", "", "mean", v$paredu),
                item("Log family income per person, mean", "", "mean", v$linc),
                item("Word test, mean", "", "mean", v$wordtest),
                item("Mandarin rating, mean 1 to 7", "", "mean", v$rating),
                item("Age in 2010, mean", "", "mean", v$age))
  s9 <- NULL
  for (g in names(gr)) {
    for (s in names(st)) {
      d <- describe(des, setNames(list(gr[[g]] & st[[s]]), g), items)
      d$status <- s
      s9 <- rbind(s9, d)
    }
    d <- describe(des, setNames(list(gr[[g]]), g), list(item("Followed up to 2018 (%)", "", "pct", v$followed)))
    d$status <- "All"
    s9 <- rbind(s9, d)
  }
  s9 <- suppress_small(s9, "n", groups = list(c("variable", "status")), stats = c("estimate", "se"), also = "n_also")
  write_out("S9_attrition", s9)

  # S10, language of the classmates of the respondents in school, and the
  # interview language of the sample A-2018
  s10 <- NULL
  src <- list(c("W10", "2010 adults", "ks3"), c("C10", "2010 children", "ks3"), c("W12", "2012", "ks3m"),
              c("W14", "2014", "ks3m"), c("W16", "2016", "ks3m"), c("W18", "2018", "qs3m"))
  for (x in src) {
    f <- FR[[x[1]]]
    des <- dom_design(f, !is.na(f$clm))
    v <- des$variables
    d <- describe(des, eth_groups(v, all = TRUE), list(item("Classmates", "Mostly a minority language", "pct", v$clm_min)))
    d$wave <- x[2]
    d$item <- x[3]
    s10 <- rbind(s10, d)
  }
  des <- sample_design(L, "A-2018")
  v <- des$variables
  q <- ifelse(is.na(v$qz103), NA, ifelse(v$qz103 == 1, 1, ifelse(v$qz103 == 5, 5, 0)))
  d <- describe(des, eth_groups(v, all = TRUE), levels_of("Interview language", q, c(1, 5, 0),
                                                         c("Mandarin", "Chinese dialect", "Other codes")))
  d$wave <- "2018, sample A-2018"
  d$item <- "qz103"
  s10 <- rbind(s10, d)
  s10 <- suppress_small(s10, "n", groups = list(c("wave", "item", "group"), c("wave", "item", "level")),
                        stats = c("estimate", "se"), also = "n_also")
  write_out("S10_classmates", s10)

  # S11, the same birth cohorts in 2010 and 2018
  s11 <- NULL
  for (src in c("A-2010", "A-2018")) {
    des <- sample_design(L, src)
    v <- des$variables
    for (i in seq_len(nrow(S11_COHORTS))) {
      co <- inr(v$birthy, S11_COHORTS$lo[i], S11_COHORTS$hi[i])
      gr <- lapply(eth_groups(v), function(g) g & co)
      items <- c(levels_of("edu5", v$edu5, 1:5, EDU_NAMES), list(item("eduy", "", "mean", v$eduy)))
      d <- describe(des, gr, items)
      d$variable <- ifelse(d$variable == "edu5", paste0("Education: ", d$level, " (%)"), "Years of schooling, mean")
      d$vset <- ifelse(grepl("^Education", d$variable), "edu5", "eduy")
      d$cohort <- S11_COHORTS$cohort[i]
      d$source <- src
      s11 <- rbind(s11, d)
    }
  }
  s11 <- suppress_small(s11, "n", groups = list(c("cohort", "source", "group", "vset")), stats = c("estimate", "se"))
  write_out("S11_cohorts", s11)

  cat(sprintf("\nTime %.0f seconds\n", proc.time()[["elapsed"]] - t0))
  cat("\n03_describe.R finished.\n")
  invisible(NULL)
}

cfps_run("03_describe", cfps_describe)
