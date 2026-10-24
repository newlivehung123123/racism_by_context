#==============================================================================
# make_test_data.R
# Structural inequalities in education in China, CFPS 2010 to 2018
#
# Writes made-up data files in the folder layout of the CFPS data, with the
# CFPS file names and the variables that the R scripts read, so that the R
# scripts can be run without the CFPS data. Every value is drawn at random.
# Nothing in the files comes from the CFPS data, and no result from them says
# anything about China.
#
# How to run. In Terminal, from the folder of the repository:
#   Rscript --vanilla tests/make_test_data.R "$HOME/cfps_test" 0.25
# The first argument is the folder to write into. The second sets the size,
# where 1 gives about 14,800 families in 2010, close to the CFPS samples, and
# 0.25 gives a quarter of that. The README says how to run the R scripts on
# the made-up files.
#==============================================================================
.libPaths(c(path.expand(Sys.getenv("R_LIBS_USER")), .libPaths()))
suppressMessages(library(haven))
a <- commandArgs(TRUE); DATA <- a[1]; SCALE <- if (length(a) > 1) as.numeric(a[2]) else 1
RAW <- file.path(DATA, "Newly downloaded Data")
set.seed(20261023)
DIR <- c("10" = "CFPS2010/[CFPS Public Data] CFPS2010 in STATA (English)", "12" = "CFPS2012",
         "14" = "CFPS2014", "16" = "CFPS2016", "18" = "CFPS2018")
for (d in DIR) dir.create(file.path(RAW, d), recursive = TRUE, showWarnings = FALSE)
FN <- list(A10 = "ecfps2010adult_202008", C10 = "ecfps2010child_201906", R10 = "ecfps2010famconf_202008", E10 = "ecfps2010famecon_202008",
  A12 = "ecfps2012adult_202505", C12 = "ecfps2012child_201906", R12 = "ecfps2012famconf_092015", E12 = "ecfps2012famecon_201906",
  A14 = "ecfps2014adult_201906", C14 = "ecfps2014child_201906", R14 = "ecfps2014famconf_170630", E14 = "ecfps2014famecon_201906",
  A16 = "ecfps2016adult_201906", C16 = "ecfps2016child_201906", R16 = "ecfps2016famconf_201804", E16 = "ecfps2016famecon_201807",
  A18 = "ecfps2018person_202012", C18 = "ecfps2018childproxy_202012", R18 = "ecfps2018famconf_202008", E18 = "ecfps2018famecon_202101")
wr <- function(d, code, version = 14) {
  y <- substr(code, 2, 3)
  write_dta(d, file.path(RAW, DIR[[y]], paste0(FN[[code]], ".dta")), version = version)
}
lab <- function(x, labels) labelled(as.numeric(x), labels)
cut8 <- function(z, cuts) findInterval(z, cuts) + 1

prov <- c(11,12,13,14,21,22,23,31,32,33,34,35,36,37,41,42,43,44,45,50,51,52,53,61,62)
plab <- setNames(prov, c("Beijing","Tianjin","Hebei","Shanxi","Liaoning","Jilin","Heilongjiang","Shanghai","Jiangsu","Zhejiang","Anhui","Fujian","Jiangxi","Shandong","Henan","Hubei","Hunan","Guangdong","Guangxi","Chongqing","Sichuan","Guizhou","Yunnan","Shaanxi","Gansu"))
big5 <- c(31, 21, 41, 62, 44)
ncty <- 162
cty_prov <- c(rep(prov, 2), sample(prov, ncty - 50, TRUE, prob = ifelse(prov %in% big5, 5, 1)))
subpop <- function(p) ifelse(p == 31, 1, ifelse(p == 21, 2, ifelse(p == 41, 3, ifelse(p == 62, 4, ifelse(p == 44, 5, 6)))))
nhr <- setNames(rep(0.012, 25), prov)
nhr[c("52","53","45","43","50","42","22","21","13")] <- c(.38,.33,.32,.12,.08,.06,.09,.07,.05)
peff <- setNames(rnorm(25, 0, 0.3), prov)
south <- setNames(prov %in% c(32,33,34,35,36,43,44,45,50,51,52,53,42,31), prov)
ethlab <- c(Han = 1, Mongol = 2, Hui = 3, Tibetan = 4, Uyghur = 5, Miao = 6, Yi = 7, Zhuang = 8, Buyi = 9,
  Korean = 10, Manchu = 11, Dong = 12, Yao = 13, Bai = 14, Tujia = 15, "Not applicable" = -8, Unknown = -1)
grp_of <- function(p) {
  n <- length(p); g <- sample(c(3, 11, 2, 15, 6), n, TRUE)
  g[p == 52] <- sample(c(6, 9, 12, 15), sum(p == 52), TRUE, prob = c(.45, .25, .2, .1))
  g[p == 53] <- sample(c(7, 14, 6, 13), sum(p == 53), TRUE, prob = c(.5, .2, .2, .1))
  g[p == 45] <- sample(c(8, 13, 6), sum(p == 45), TRUE, prob = c(.8, .1, .1))
  g[p %in% c(43, 42, 50)] <- sample(c(15, 6, 12), sum(p %in% c(43, 42, 50)), TRUE)
  g[p %in% c(21, 22, 13)] <- sample(c(11, 10, 3, 2), sum(p %in% c(21, 22, 13)), TRUE, prob = c(.6, .2, .1, .1))
  g
}

# Families and persons in 2010
NF <- round(14800 * SCALE)
fam <- data.frame(fid = 100000 + seq_len(NF) * 3, cty = sample(ncty, NF, TRUE))
fam$prov <- cty_prov[fam$cty]
fam$nh <- rbinom(NF, 1, nhr[as.character(fam$prov)])
fam$grp <- ifelse(fam$nh == 1, grp_of(fam$prov), 1)
fam$agri <- rbinom(NF, 1, ifelse(fam$nh == 1, .85, .7))
fam$inc <- round(exp(rnorm(NF, 10 - 0.5 * fam$agri - 0.3 * fam$nh, 0.9)))
P <- list(); k <- 0
hb <- round(pmin(1985, pmax(1925, rnorm(NF, 1962, 13))))
add <- function(fid, idx, by, male, role) data.frame(fid = fid, pid = fid * 1000 + idx, birthy = by, male = male, role = role)
P[[1]] <- add(fam$fid, 1, hb, rbinom(NF, 1, .85), "head")
sp <- rbinom(NF, 1, .85) == 1
P[[2]] <- add(fam$fid[sp], 2, pmin(1990, hb[sp] + sample(-3:4, sum(sp), TRUE)), 1 - P[[1]]$male[sp], "spouse")
nk <- sample(0:3, NF, TRUE, prob = c(.2, .35, .3, .15))
for (j in 1:3) {
  s <- nk >= j
  by <- hb[s] + 19 + sample(1:16, sum(s), TRUE) + 2 * j
  ok <- by <= 2010
  P[[2 + j]] <- add(fam$fid[s][ok], 2 + j, by[ok], rbinom(sum(ok), 1, .52), "child")
}
gp <- rbinom(NF, 1, .25) == 1 & hb > 1950
P[[6]] <- add(fam$fid[gp], 6, hb[gp] - sample(22:30, sum(gp), TRUE), rbinom(sum(gp), 1, .45), "grand")
P <- do.call(rbind, P)
P <- P[P$birthy >= 1912, ]
m <- match(P$fid, fam$fid)
P$prov <- fam$prov[m]; P$cty <- fam$cty[m]
P$nh <- fam$nh[m]; P$grp <- fam$grp[m]
flip <- P$role == "spouse" & runif(nrow(P)) < .08
P$nh[flip] <- 1 - P$nh[flip]; P$grp[flip & P$nh == 1] <- grp_of(P$prov[flip & P$nh == 1]); P$grp[P$nh == 0] <- 1
P$agri <- fam$agri[m]; sw <- runif(nrow(P)) < .08; P$agri[sw] <- 1 - P$agri[sw]
# parents' education for children from the head and spouse, other people random
gen_edu <- function(by, nh, agri, pe, male, pr) {
  z <- 0.035 * (by - 1950) - 0.6 * nh - 1.1 * agri - 0.4 * nh * agri + 0.45 * pe + 0.15 * male + peff[as.character(pr)] + rlogis(length(by))
  cut8(z, c(-1.2, 0.2, 1.5, 2.6, 3.3, 4.3, 5.6))
}
adult <- P$role != "child"
P$pe <- NA_real_
P$pe[adult] <- pmin(5, cut8(0.04 * (P$birthy[adult] - 1930) - 0.5 * P$agri[adult] + rlogis(sum(adult)), c(0, 1.2, 2.4, 3.6)))
P$edu_f <- NA_real_
P$edu_f[adult] <- gen_edu(P$birthy[adult], P$nh[adult], P$agri[adult], P$pe[adult], P$male[adult], P$prov[adult])
he <- tapply(P$edu_f[adult], P$fid[adult], max)
ch <- !adult
P$pe[ch] <- pmin(8, he[as.character(P$fid[ch])])
P$edu_f[ch] <- gen_edu(P$birthy[ch], P$nh[ch], P$agri[ch], pmin(5, P$pe[ch]), P$male[ch], P$prov[ch])
P$lang <- ifelse(P$nh == 1, sample(c(1, 3, 5), nrow(P), TRUE, prob = c(.2, .35, .45)),
                 sample(c(1, 3, 5), nrow(P), TRUE, prob = c(.35, .645, .005)))
P$lang[P$nh == 0 & P$agri == 0 & runif(nrow(P)) < .3] <- 1
P$lang[runif(nrow(P)) < .005] <- 77
P$first <- 2010
cmpl_age <- c(0, 12, 15, 18, 21, 22, 25, 28)
edu_at <- function(final, age) { e <- rep(1, length(final)); for (L in 2:8) e[final >= L & age >= cmpl_age[L]] <- L; e }

# New entrants after 2010 (married in), in existing families
newp <- function(yr, n) {
  f <- sample(fam$fid, n, TRUE); mm <- match(f, fam$fid)
  d <- data.frame(fid = f, pid = f * 1000 + 40 + (yr - 2010) + sample(0:9, n, TRUE) * 0, birthy = round(runif(n, 1955, yr - 18)),
                  male = rbinom(n, 1, .3), role = "new")
  d$pid <- f * 1000 + 30 + (yr - 2010) / 2 * 10 + seq_len(n) %% 10
  d$prov <- fam$prov[mm]; d$cty <- fam$cty[mm]; d$nh <- rbinom(n, 1, nhr[as.character(d$prov)])
  d$grp <- ifelse(d$nh == 1, grp_of(d$prov), 1); d$agri <- rbinom(n, 1, .7)
  d$pe <- pmin(5, cut8(0.04 * (d$birthy - 1930) - 0.5 * d$agri + rlogis(n), c(0, 1.2, 2.4, 3.6)))
  d$edu_f <- gen_edu(d$birthy, d$nh, d$agri, d$pe, d$male, d$prov)
  d$lang <- ifelse(d$nh == 1, 5, 3); d$first <- yr
  d[!duplicated(d$pid), ]
}
NEW <- do.call(rbind, lapply(c(2012, 2014, 2016, 2018), function(y) newp(y, round(900 * SCALE))))
NEW <- NEW[!NEW$pid %in% P$pid, ]
ALL <- rbind(P, NEW)
ALL$alive <- TRUE

# county codes, 2018 psu equal to the 2010 county except a few
psu18 <- seq_len(ncty); psu18[sample(ncty, 3)] <- sample(200:250, 3)
ALL$subpop <- subpop(ALL$prov)
ALL$subsample <- ifelse(ALL$prov %in% big5, rbinom(nrow(ALL), 1, .45), 1)
ALL$wbase <- exp(rnorm(nrow(ALL), 10, .5)) * ifelse(ALL$prov %in% big5, .3, 1)

waveset <- function(yr) {
  d <- ALL[ALL$first <= yr & ALL$birthy <= yr, ]
  p_in <- ifelse(d$first == yr, .97, .87 - .02 * (yr - 2010) / 2)
  d <- d[runif(nrow(d)) < p_in, ]
  d$age <- yr - d$birthy
  d$edu <- edu_at(d$edu_f, d$age)
  d$enrol <- as.numeric(d$edu_f > d$edu & d$age >= 6 & d$age <= 28)
  d$sch <- pmin(8, d$edu + d$enrol)
  d
}
na_codes <- function(x, p8 = 0, p9 = 0) { u <- runif(length(x)); x[u < p8] <- -8; x[u >= p8 & u < p8 + p9] <- -9; x }
rating <- function(d) cut8(0.5 * d$edu - 1.2 * (d$lang == 5) - 0.5 * (d$lang == 3) - 0.6 * d$nh + rlogis(nrow(d)), c(-0.5, 0.5, 1.3, 2.0, 2.7, 3.5))
wtest <- function(d) pmax(0, pmin(34, round(3.5 * d$edu + rnorm(nrow(d), 0, 4) - 2 * d$nh)))
hknow <- function(d) ifelse(d$edu >= 5 & runif(nrow(d)) < .7, 3, ifelse(d$agri == 1, 1, 3))
eyears <- function(e) pmax(0, c(0, 6, 9, 12, 15, 16, 19, 22)[e] + sample(-1:1, length(e), TRUE))

seen <- c()
for (yr in c(2010, 2012, 2014, 2016, 2018)) {
  y <- substr(yr, 3, 4)
  d <- waveset(yr)
  firstAC <- !(d$pid %in% seen)
  seen <- c(seen, d$pid)
  A <- d[d$age >= 16, ]; C <- d[d$age < 16, ]
  newA <- !(A$pid %in% seen[duplicated(seen)])
  newC <- !(C$pid %in% seen[duplicated(seen)])
  wcs <- A$wbase * exp(rnorm(nrow(A), 0, .2)) * (runif(nrow(A)) > .05)
  wres <- ifelse(A$subsample == 1, wcs * 1.2, 0)
  if (yr == 2018) { wcs <- wcs / mean(wcs); wres <- NULL }
  eth_new <- function(dd, isnew, p_unk = .003) { e <- ifelse(dd$nh == 1, dd$grp, 1); e[!isnew] <- -8; e[isnew & runif(nrow(dd)) < p_unk] <- -1; e }
  hk3 <- function(dd, isnew) { h <- ifelse(dd$agri == 1, 1, 3); h[!isnew] <- -8; u <- runif(nrow(dd)); h[isnew & u < .01] <- 5; h[isnew & u > .995] <- 79; h }
  ages_lab <- function(x) x
  if (yr == 2010) {
    Ad <- data.frame(pid = A$pid, fid = A$fid, provcd = lab(A$prov, plab), countyid = A$cty, subpopulation = A$subpop,
      subsample = A$subsample, urban = 1 - A$agri, rswt_nat = wcs, rswt_res = wres, qa1age = A$age,
      qa1y_best = A$birthy, gender = A$male, qa5code = lab(ifelse(A$nh == 1, A$grp, 1), ethlab),
      qa2 = hknow(A), qa302 = ifelse(runif(nrow(A)) < .003, -1, ifelse(A$agri == 1, 1, 3)), qa402 = ifelse(A$agri == 1, 1, 3),
      qd2 = A$lang, ks3 = ifelse(A$enrol == 1, ifelse(A$lang == 5, 5, 1), -8), qz205 = rating(A), wordtest = wtest(A),
      qd3 = ifelse(A$enrol == 1, 1, 5), edu2010_t1_best = A$edu, edu2010 = A$edu, qc1 = A$edu, sch2010 = A$sch, eduy2010 = eyears(A$edu))
    Ad$qa1age[sample(nrow(Ad), 5)] <- -9
    names(Ad)[names(Ad) == "qz205"] <- "QZ205"
    wr(Ad, "A10")
    old <- C$age >= 10
    Cd <- data.frame(pid = C$pid, fid = C$fid, provcd = C$prov, countyid = C$cty, subpopulation = C$subpop,
      subsample = C$subsample, rswt_nat = C$wbase, rswt_res = ifelse(C$subsample == 1, C$wbase, 0), wa1age = C$age,
      wa1y = C$birthy, gender = C$male, wa6code = ifelse(C$nh == 1, C$grp, 1), wa3 = ifelse(C$age < 1, -8, ifelse(C$agri == 1, 1, 3)),
      wk2 = ifelse(old, c("1" = 1, "3" = 2, "5" = 3, "77" = 77)[as.character(C$lang)], -8),
      wz205 = ifelse(old, rating(C), -8), ks3 = ifelse(old & C$enrol == 1, ifelse(C$lang == 5, 5, 1), -8),
      wordtest = ifelse(old, wtest(C), -8), edu2010 = C$edu, sch2010 = C$sch, eduy2010 = eyears(C$edu))
    wr(Cd, "C10")
  }
  if (yr == 2012) {
    Ad <- data.frame(pid = A$pid, fid12 = A$fid + 0, fid10 = A$fid, provcd = A$prov, countyid = ifelse(runif(nrow(A)) < .01, -9, A$cty),
      subpopulation = A$subpop, subsample = A$subsample, cfps2012_age = A$age, cfps2012_birthy = A$birthy,
      cfps2012_gender = A$male, qa701code = eth_new(A, newA), qa301 = hknow(A), qa503 = hk3(A, newA), qa603 = hk3(A, newA),
      edu2012 = A$edu, sch2012 = A$sch, eduy2012 = eyears(A$edu),
      qd201 = c("1" = 1, "3" = 2, "5" = 6, "77" = 3)[as.character(A$lang)], ks3m = ifelse(A$enrol == 1, ifelse(A$lang == 5, 6, 1), -8),
      qz205 = ifelse(south[as.character(A$prov)] & runif(nrow(A)) < .35, 79, rating(A)), wc01 = ifelse(A$enrol == 1, 1, 0),
      urban12 = 1 - A$agri, rswt_natcs12 = wcs, rswt_rescs12 = wres, rswt_natpn1012 = wcs)
    Ad$qd201[runif(nrow(Ad)) < .01] <- 5
    wr(Ad, "A12")
    Cd <- data.frame(pid = ifelse(runif(nrow(C)) < .13, NA, C$pid), wa6code = eth_new(C, newC), cfps_minzu = ifelse(C$nh == 1, C$grp, 1),
      wa3 = hk3(C, newC))
    wr(Cd, "C12", version = 13)
  }
  if (yr == 2014) {
    Ad <- data.frame(pid = A$pid, fid14 = A$fid, provcd14 = A$prov, countyid14 = A$cty, subpopulation10 = A$subpop,
      subsample10 = A$subsample, cfps2014_age = A$age, cfps_birthy = A$birthy, cfps_gender = A$male, QA701CODE = eth_new(A, newA),
      qa301 = hknow(A), qa503 = hk3(A, newA), qa603 = hk3(A, newA), cfps2014edu = A$edu, cfps2014sch = A$sch,
      cfps2014eduy_im = eyears(A$edu), ks3m = ifelse(A$enrol == 1, ifelse(A$lang == 5, 6, 1), -8),
      qz205 = ifelse(south[as.character(A$prov)] & runif(nrow(A)) < .3, 79, rating(A)), wordtest14 = wtest(A),
      wc01 = ifelse(A$enrol == 1, 1, ifelse(A$age < 30, 0, -8)), selfrpt = rbinom(nrow(A), 1, .87),
      rswt_natcs14 = wcs, rswt_rescs14 = wres)
    wr(Ad, "A14")
    wr(data.frame(pid = C$pid, wa6code = eth_new(C, newC), wa3 = hk3(C, newC)), "C14")
  }
  if (yr == 2016) {
    Ad <- data.frame(pid = A$pid, fid16 = A$fid, provcd16 = A$prov, countyid16 = A$cty, subpopulation = A$subpop,
      cfps_age = A$age, cfps_birthy = A$birthy, cfps_gender = A$male, pa701code = eth_new(A, newA),
      Pa301 = hknow(A), pa503 = hk3(A, newA), pa603 = hk3(A, newA), cfps2016edu = na_codes(A$edu, 0, .02), cfps2016sch = A$sch,
      cfps2016eduy_im = eyears(A$edu), ks3m = ifelse(A$enrol == 1, ifelse(A$lang == 5, 6, 1), -8),
      qz205 = ifelse(south[as.character(A$prov)] & runif(nrow(A)) < .3, 79, rating(A)),
      school = ifelse(A$enrol == 1, sample(c(1, 5), nrow(A), TRUE, prob = c(.85, .15)), 0), selfrpt = rbinom(nrow(A), 1, .88),
      rswt_natcs16 = wcs, rswt_rescs16 = wres)
    wr(Ad, "A16")
    wr(data.frame(pid = C$pid, pa701code = eth_new(C, newC), pa503 = hk3(C, newC), pa603 = -8), "C16")
  }
  if (yr == 2018) {
    in10 <- A$first == 2010 & A$birthy <= 2010
    wpn <- ifelse(in10, wcs * exp(rnorm(nrow(A), 0, .1)), 0)
    wpn[runif(nrow(A)) < .1] <- 0
    Ad <- data.frame(pid = A$pid, fid18 = A$fid, provcd18 = A$prov, psu = psu18[A$cty], countyid18 = A$cty, subpopulation = A$subpop,
      subsample = A$subsample, age = A$age, ibirthy = A$birthy, gender = A$male,
      minzu = ifelse(runif(nrow(A)) < .09, -8, ifelse(A$nh == 1, A$grp, 1)), qa701code = eth_new(A, newA),
      qa301 = hknow(A), qa603 = ifelse(newA, ifelse(A$agri == 1, 1, 3), -8), cfps2018edu = A$edu, cfps2018sch = A$sch,
      cfps2018eduy_im = eyears(A$edu), qs3m = ifelse(A$enrol == 1, ifelse(A$lang == 5, 6, 1), -8),
      qz103 = ifelse(A$lang == 1, 1, ifelse(runif(nrow(A)) < .6, 1, 5)), wordtest18 = na_codes(wtest(A), .1, .05),
      school = ifelse(A$enrol == 1, 1, 0), selfrpt = rbinom(nrow(A), 1, .85), urban18 = 1 - A$agri,
      rswt_natcs18n = wcs, rswt_natpn1018n = wpn / mean(wpn[wpn > 0]))
    wr(Ad, "A18")
    Cin10 <- C$first == 2010 & C$birthy <= 2010
    Cd <- data.frame(pid = C$pid, minzu = ifelse(C$nh == 1, C$grp, 1), wa701code = eth_new(C, newC),
      wa503 = hk3(C, newC), wa603 = ifelse(C$age >= 12, ifelse(C$agri == 1, 1, 3), -8),
      rswt_natpn1018n = ifelse(Cin10, runif(nrow(C), .5, 1.5), 0), psu = psu18[C$cty], subpopulation = C$subpop)
    wr(Cd, "C18")
  }
  # roster: every family member alive in the wave, with parents' education
  R <- ALL[ALL$birthy <= yr & ALL$first <= yr, ]
  fe <- function(x) { v <- pmin(8, x); v[runif(length(v)) < .12] <- -8; v[runif(length(v)) < .03] <- -1; v }
  par1 <- fe(ifelse(is.na(R$pe), 2, R$pe)); par2 <- fe(pmax(1, ifelse(is.na(R$pe), 2, R$pe) - rbinom(nrow(R), 1, .5)))
  if (yr == 2010) Rd <- data.frame(pid = R$pid, fid = R$fid, feduc = par1, meduc = par2)
  if (yr == 2012) {
    Rd <- data.frame(pid = R$pid, fid12 = R$fid, feduc12 = par1, meduc12 = par2)
    dup <- Rd[sample(nrow(Rd), round(.045 * nrow(Rd))), ]; dup$feduc12 <- -8; dup$meduc12 <- -8
    Rd <- rbind(Rd, dup)[sample(nrow(Rd) + nrow(dup)), ]
  }
  if (yr == 2014) {
    par1[runif(length(par1)) < .02] <- 9
    Rd <- data.frame(pid = R$pid, fid14 = R$fid, tb4_a14_f = par1, tb4_a14_m = par2)
    dup <- Rd[sample(nrow(Rd), round(.07 * nrow(Rd))), ]
    Rd <- rbind(Rd, dup)
  }
  if (yr == 2016) Rd <- data.frame(pid = R$pid, fid16 = R$fid, tb4_a16_f = par1, tb4_a16_m = par2)
  if (yr == 2018) Rd <- data.frame(pid = R$pid, fid18 = R$fid, tb4_a18_f = par1, tb4_a18_m = par2)
  wr(Rd, paste0("R", y))
  fs <- as.numeric(table(R$fid)[as.character(fam$fid)]); fs[is.na(fs)] <- 1
  inc <- round(fam$inc * exp(rnorm(NF, 0.1 * (yr - 2010) / 2, .3))); inc[runif(NF) < .05] <- -9; inc[runif(NF) < .01] <- -100
  if (yr == 2010) Ed <- data.frame(fid = fam$fid, faminc_net = inc, familysize = fs)
  if (yr == 2012) { Ed <- data.frame(fid12 = ifelse(runif(NF) < .09, NA, fam$fid), fincome1 = inc, familysize = fs) }
  if (yr == 2014) Ed <- data.frame(fid14 = fam$fid, fincome1 = inc, familysize = fs)
  if (yr == 2016) Ed <- data.frame(fid16 = fam$fid, fincome1 = inc, familysize16 = fs)
  if (yr == 2018) Ed <- data.frame(fid18 = fam$fid, fincome1 = inc)
  wr(Ed, paste0("E", y))
}
# the two cross-year ID files and two archives that the folder also holds
write_dta(data.frame(pid = P$pid[1:50]), file.path(RAW, DIR[["12"]], "ecfps2012crossyearid_032015.dta"))
write_dta(data.frame(pid = P$pid[1:50]), file.path(RAW, DIR[["14"]], "ecfps2014crossyearid_170630.dta"))
writeBin(as.raw(1:20), file.path(RAW, "CFPS2020.zip")); writeBin(as.raw(1:20), file.path(RAW, "CFPS2022.rar"))
cat("made-up data written, persons", nrow(ALL), "\n")
