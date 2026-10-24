# Structural inequalities in education in China, CFPS 2010 to 2018

This repository holds the code, the aggregate results and the tables of a study of how non-Han Chinese are disadvantaged in education and in language. The study uses the China Family Panel Studies (CFPS), waves 2010, 2012, 2014, 2016 and 2018.

To see the results in the browser, open [Exhibits](Exhibits/README.md) for Tables I to IV and Figure 1, and [Exhibits/Supplementary tables](Exhibits/Supplementary%20tables/README.md) for Tables S1 to S11. The same tables are in the two Word files in `Exhibits`.

The repository holds no CFPS data. The CFPS user agreement does not allow the data to be passed on, so the steps below start with an application for the data. Everything in `Output (checked)` and `Exhibits` is aggregate (counts, shares and model estimates by group), and no published file shows a count of respondents from 1 to 9.

With the data in place, the checks and the analysis take about 40 minutes on a recent Mac.

## Contents

- [What the repository holds](#what-the-repository-holds)
- [What you need](#what-you-need)
- [Step 1. Get the CFPS data](#step-1-get-the-cfps-data)
- [Step 2. Put the data on an encrypted disk](#step-2-put-the-data-on-an-encrypted-disk)
- [Step 3. Install the software and get the code](#step-3-install-the-software-and-get-the-code)
- [Step 4. Quit every AI app](#step-4-quit-every-ai-app)
- [Step 5. Run the data checks](#step-5-run-the-data-checks)
- [Step 6. Run the analysis](#step-6-run-the-analysis)
- [Step 7. Build the tables and the figure](#step-7-build-the-tables-and-the-figure)
- [Step 8. Compare your results with the published ones](#step-8-compare-your-results-with-the-published-ones)
- [Running the R scripts by hand](#running-the-r-scripts-by-hand)
- [Testing the code on made-up data](#testing-the-code-on-made-up-data)
- [What the analysis does](#what-the-analysis-does)
- [Output files and the tables of the paper](#output-files-and-the-tables-of-the-paper)
- [Hidden counts](#hidden-counts)
- [Models fitted differently](#models-fitted-differently)
- [Notes](#notes)
- [Licence](#licence)

## What the repository holds

```
R scripts/
  00_setup.R                    folders, search for the data files, variable lists,
                                shared functions and the package check
  01_checks.R                   data checks, tables C0 to C4
  02_link.R                     reads and links the waves and builds the samples
  03_describe.R                 descriptive tables and the data of Figure 1
  04_models.R                   multiple imputation and the models of Tables II to IV
                                and S3 to S6, and Table S8
  05_checks.R                   robustness checks R1 to R15 (Table S7)
  analysis_tools.R              functions of 02 to 05 (recodes, survey models,
                                marginal effects, KHB, hiding of small counts)
  06_exhibits.py                builds the Word tables, Figure 1 and the pages
                                that show the tables on GitHub
  Run checks and copy.command   macOS runner of 01_checks.R
  Run analysis.command          macOS runner of 02_link.R to 05_checks.R
  Last run.txt                  report of the run that gave the published results
Output (checked)/               the published results, 27 CSV tables
  logs/                         the logs of 00_setup.R to 05_checks.R
Exhibits/
  README.md                     Tables I to IV and Figure 1, shown on GitHub
  Supplementary tables/         Tables S1 to S11, one page per table, shown on GitHub
  Main tables and figure.docx   Tables I to IV and Figure 1
  Supplementary tables.docx     Tables S1 to S11
  Figure 1.png, Figure 1.pdf    Figure 1 in black and white
tests/
  make_test_data.R              writes made-up data in the CFPS layout, for testing
LICENSE                         MIT licence of the code
```

## What you need

| What | Version used | What it is for |
|---|---|---|
| A Mac | macOS 15.6, Apple M2 | the two runner files are macOS shell scripts |
| R from CRAN | 4.6.1 (arm64) | the analysis |
| Eight R packages | haven 2.5.5, readr 2.2.0, tidyselect 1.2.1, survey 4.5, VGAM 1.1.14, svyVGAM 1.3, mice 3.19.0, mitools 2.7 | reading the Stata files, survey models, imputation |
| Apple Command Line Tools | the version for macOS 15 | `/usr/bin/python3`, which the runners use to screen the output |
| Python and four packages | Python 3.13.5, pandas 2.2.3, matplotlib 3.10.0, lxml 5.3.0, python-docx 1.2.0 | the Word tables and Figure 1 only |
| 7-Zip (optional) | 26.03 | only for CFPS archives that macOS cannot open |
| Free disk space | about 10 GB | the data, any unpacked archives and the work files |

The R scripts can also be run by hand on other systems (see [Running the R scripts by hand](#running-the-r-scripts-by-hand)), but only macOS was tested.

## Step 1. Get the CFPS data

1. Register at the CFPS data platform, https://cfpsdata.pku.edu.cn, and apply for the public data. Approval takes about three working days.
2. Download the Stata files with English labels (file names beginning with `ecfps`) of the five waves. The study needs four files per wave, 20 in all. These releases were used.

   | Wave | Adult or person file | Child file | Family roster file | Family economy file |
   |---|---|---|---|---|
   | 2010 | `ecfps2010adult_202008` | `ecfps2010child_201906` | `ecfps2010famconf_202008` | `ecfps2010famecon_202008` |
   | 2012 | `ecfps2012adult_202505` | `ecfps2012child_201906` | `ecfps2012famconf_092015` | `ecfps2012famecon_201906` |
   | 2014 | `ecfps2014adult_201906` | `ecfps2014child_201906` | `ecfps2014famconf_170630` | `ecfps2014famecon_201906` |
   | 2016 | `ecfps2016adult_201906` | `ecfps2016child_201906` | `ecfps2016famconf_201804` | `ecfps2016famecon_201807` |
   | 2018 | `ecfps2018person_202012` | `ecfps2018childproxy_202012` | `ecfps2018famconf_202008` | `ecfps2018famecon_202101` |

3. The scripts find each file by the wave and the file type in its name, so other release dates work too. A later release can hold corrected values and give slightly different results.
4. The cross-year file is not needed. 01_checks.R uses it for one check table (C3) when it is present, and the analysis never reads it.
5. Some files come as password-protected archives. The archive password is in the CFPS instructions document that comes with the data. It is not the password of your CFPS account.
6. The CFPS codebooks and user guides are at https://www.isss.pku.edu.cn/cfps/docs/.

## Step 2. Put the data on an encrypted disk

The runners expect the data on an encrypted disk image, `/Users/Shared/CFPS data.sparsebundle`, whose disk is named `CFPS data`. The CFPS rule on AI tools (see [Step 4](#step-4-quit-every-ai-app)) asks that the data sit in a separate folder outside any synced drive or cloud backup, and `/Users/Shared` is outside iCloud Drive.

1. In Terminal, create the disk image. You are asked to choose its password. The image grows as files are added, up to 50 GB.

   ```bash
   hdiutil create -size 50g -type SPARSEBUNDLE -fs APFS -encryption AES-128 -volname "CFPS data" "/Users/Shared/CFPS data.sparsebundle"
   ```

2. Double-click `CFPS data.sparsebundle` in the folder `/Users/Shared`, type the password, and make a folder named `Newly downloaded Data` on the disk `CFPS data`.
3. Copy the CFPS files, or the archives as downloaded, into `Newly downloaded Data`. Subfolders are fine, down to six levels. The study used one folder per wave, for example `CFPS2012`, with the 2010 files in `CFPS2010/[CFPS Public Data] CFPS2010 in STATA (English)`.
4. Eject the disk with the eject button next to `CFPS data` in the Finder sidebar.

The scripts write two more folders on the disk. `Output` holds the aggregate results, and `Work` holds the linked and imputed data, which never leave the disk.

To use other names or places, change `IMAGE` and `DATA` near the top of both runner files and `DATA` in 00_setup.R. When the runners are started with `bash`, setting the environment variables `CFPS_IMAGE` and `CFPS_DATA` does the same.

## Step 3. Install the software and get the code

1. Install R 4.6.1 for macOS from CRAN, https://cran.r-project.org/bin/macosx/ (the arm64 installer for Apple chips). The runners call R at `/Library/Frameworks/R.framework/Resources/bin/Rscript`, where the CRAN installer puts it.
2. Open R and install the packages.

   ```r
   install.packages(c("haven", "readr", "tidyselect", "survey", "VGAM", "svyVGAM", "mice", "mitools"))
   ```

   This installs the current versions. For the versions used in the study, which is safer for an exact replication, run the lines below instead. Packages built from source need the compilers listed at https://mac.r-project.org/tools/.

   ```r
   install.packages("remotes")
   v <- c(haven = "2.5.5", readr = "2.2.0", tidyselect = "1.2.1", survey = "4.5",
          VGAM = "1.1.14", svyVGAM = "1.3", mice = "3.19.0", mitools = "2.7")
   for (p in names(v)) remotes::install_version(p, version = v[[p]], upgrade = "never")
   ```

   MASS and parallel, which the scripts also use, come with R. The log 00_setup.log (Step 5) lists the version of each package found.

3. Install Apple's Command Line Tools, which give `/usr/bin/python3` for the runners. Skip this when `xcode-select -p` already prints a folder.

   ```bash
   xcode-select --install
   ```

4. For the tables and the figure, install Python 3.13 from https://www.python.org/downloads/macos/ (Python 3.10 to 3.13 work with the package versions below) and make an environment with the four packages.

   ```bash
   python3.13 -m venv "$HOME/venv-cfps"
   ```

   ```bash
   "$HOME/venv-cfps/bin/pip" install pandas==2.2.3 matplotlib==3.10.0 lxml==5.3.0 python-docx==1.2.0
   ```

   The `/usr/bin/python3` of the Command Line Tools is too old for these versions.

5. Optional. When some CFPS archives do not open with the Mac's own tool, put the program `7zz` of 7-Zip for macOS, from https://www.7-zip.org/download.html, into the folder `~/.local/7zip-26.03`. The checks runner looks there first, then in `/opt/homebrew/bin` and `/usr/local/bin`. The Homebrew build of 7-Zip leaves out rar files. Archives can also be unpacked by hand before Step 5.
6. Get the code.

   ```bash
   git clone https://github.com/newlivehung123123/racism_by_context.git
   ```

   A clone made with git can be double-clicked in Finder. A ZIP downloaded from GitHub is marked by macOS as coming from the internet, so run its runner files with `bash`, as shown in Steps 5 and 6.

## Step 4. Quit every AI app

The CFPS rule on AI tools, https://www.isss.pku.edu.cn/cfps/sjzx/AIsyglgd/index.htm, bars AI tools from processing CFPS microdata. This covers chatbots, AI services called from code, coding assistants and agents that can read the data folder.

Before each run, quit every AI app with Command-Q, and close any AI tool in the web browser and in the code editor. Both runners stop and eject the disk when one of a few known AI apps is open (the list `AI_APPS` in 00_setup.R), and 00_setup.R stops too, but the rule covers every AI tool.

## Step 5. Run the data checks

1. In Terminal, go to the repository folder and keep a copy of the published results. Each runner copies its results into `Output (checked)` and replaces the files of the same name.

   ```bash
   cd racism_by_context
   ```

   ```bash
   cp -R "Output (checked)" "Output (published)"
   ```

2. Double-click `Run checks and copy.command` in the folder `R scripts`, or run it from the repository folder.

   ```bash
   bash "R scripts/Run checks and copy.command"
   ```

   The runner works through six steps and writes what happened, with no value from the data, to `R scripts/Last run.txt`, which replaces the published report (the published one stays on GitHub).

   1. It stops, and ejects the disk, when a known AI app is open.
   2. It opens the disk, asking for the disk password, unless the disk is open already. The password shows while it is typed so that it can be checked, there are three tries, and the window is wiped afterwards.
   3. It unpacks any zip, 7z or rar archive in `Newly downloaded Data` into a folder beside it whose name ends in `(unpacked)`, asking for the archive password when a file is locked.
   4. It runs 01_checks.R, which first runs 00_setup.R. 00_setup.R finds each data file and writes the log 00_setup.log, and 01_checks.R writes the check tables C0 to C4.
   5. It screens the folder `Output` on the disk and copies it into `Output (checked)` only when the screen finds nothing wrong.
   6. It ejects the disk.

3. Open `Output (checked)/logs/00_setup.log`. Its table of files must show one file for each code from A10 to E18 (A for the adult or person file, C child, R family roster, E family economy, followed by the last two digits of the wave year). When a code shows `not found`, or several files, name the file by hand in the manual section of 00_setup.R (the lines `MANUAL <- c(...)`), as its comments explain, and run the checks again.

   The line `XY`, the cross-year file, can show `not found` or `2 files match` without harm, because the analysis does not use it. Without it, 01_checks.R skips table C3 and counts every person as not in the cross-year file in table C4, as in the published run.

## Step 6. Run the analysis

1. Quit every AI app again, as in Step 4.
2. Double-click `Run analysis.command` in the folder `R scripts`, or run it from the repository folder.

   ```bash
   bash "R scripts/Run analysis.command"
   ```

3. The runner opens the disk as in Step 5 and runs four R scripts in turn, each of which first runs 00_setup.R.

   | Script | What it does | Time in the published run |
   |---|---|---|
   | 02_link.R | reads the 20 files, links the waves by person and family ID and builds the samples, saved as `Work/link.rds` on the disk | under a minute |
   | 03_describe.R | descriptive tables and the data of Figure 1 | under a minute |
   | 04_models.R | multiple imputation and the models, with the imputed data saved as `Work/imp.rds` | 13 minutes |
   | 05_checks.R | the robustness checks | 20 minutes |

   It then screens `Output`, copies it into `Output (checked)` when the screen finds nothing wrong, and ejects the disk. The published run took 33 minutes on an Apple M2 with four cores. The runner keeps the Mac awake while R runs, so keep the Mac plugged in with its lid open.

When a script stops with an error, the runner goes on as far as it can. When 02_link.R stops, nothing else runs. When 03_describe.R stops, the models still run. 05_checks.R runs only when 04_models.R saved the imputed data. The screen runs in every case, so when it passes, the log of a script that stopped also reaches `Output (checked)` and gives the reason.

The screen accepts only the tables and logs that the scripts write, each with the columns its script gives it. It stops the copy at any other file, at more than 5,000 rows in a table, at any number of nine or more digits (the length of a CFPS person ID) and at any count of respondents from 1 to 9. When it stops the copy, `Last run.txt` names the file and the test it failed, and the results stay in `Output` on the disk.

A run that worked ends `Last run.txt` with these lines.

```
C3_first_wave.csv: missing
CLEAN
The screen found nothing wrong, and the files are copied into Output (checked) in the project folder.
The disk is ejected.
Everything worked.
```

The line about `C3_first_wave.csv` is expected when no cross-year file is used.

## Step 7. Build the tables and the figure

From the repository folder, run 06_exhibits.py.

```bash
"$HOME/venv-cfps/bin/python" "R scripts/06_exhibits.py"
```

06_exhibits.py reads only `Output (checked)` and writes the two Word files, Figure 1 and the pages that show the tables on GitHub (`Exhibits/README.md` and the folder `Exhibits/Supplementary tables`) into `Exhibits`. It needs neither the data nor the disk, so it also rebuilds the published exhibits straight after cloning. Table numbers are Roman numerals, tables have three horizontal lines only, and Figure 1 is in black and white.

## Step 8. Compare your results with the published ones

```bash
diff -r "Output (published)" "Output (checked)"
```

The logs always differ, because they hold dates and run times. Other differences have known causes.

- 00_setup.R sets the seed 20261014, and each imputation sets the same seed in mice, so the results do not depend on the order of the jobs or on the number of cores. On made-up data, runs with two and with four cores gave byte-identical tables.
- The published tables C0, C1, C2 and C4 and the log 01_checks.log come from the checks run of 21 October 2026, when the scripts still looked for the waves 2020 and 2022. `C1_files.csv` therefore has 29 rows, 8 of them `not found` for 2020 and 2022, where the code as published gives 21 rows.
- Three files had cells hidden after the run (see [Hidden counts](#hidden-counts)). analysis_tools.R now hides these cells itself, so a new run gives the files as published, but their sizes in the published `Last run.txt` are those from before the change.
- Another CFPS release, other package versions or another system can change the last digits of some estimates, or more where CFPS has corrected values.

## Running the R scripts by hand

The runners are macOS files, but the R scripts can also be run one at a time with Rscript, with no screen and no copy. Set `CFPS_DATA` to the folder that holds `Newly downloaded Data`, then run the scripts in order from the repository folder.

```bash
export CFPS_DATA="/Volumes/CFPS data"
```

```bash
Rscript --vanilla "R scripts/01_checks.R"
```

```bash
Rscript --vanilla "R scripts/02_link.R"
```

```bash
Rscript --vanilla "R scripts/03_describe.R"
```

```bash
Rscript --vanilla "R scripts/04_models.R"
```

```bash
Rscript --vanilla "R scripts/05_checks.R"
```

Each script runs 00_setup.R first. 03_describe.R and 04_models.R need `Work/link.rds` from 02_link.R, and 05_checks.R needs `Work/imp.rds` from 04_models.R. The results go into the folder `Output` inside `CFPS_DATA`. The scripts show any count from 1 to 9 as `<10`, but nothing is screened, so look over `Output` before you share it.

Two settings change the run.

- `CFPS_CORES`, the number of cores for the models, 4 by default. On Windows set it to 1, because R's `mclapply` cannot use more than one core there.
- `CFPS_M`, the number of imputed data sets, 20 by default, as in the published run.

The AI app check of 00_setup.R works only with the default folder `/Volumes/CFPS data`. With another folder, quitting every AI app is up to you.

## Testing the code on made-up data

`tests/make_test_data.R` writes random data in the CFPS folder layout, with the CFPS file names and the variables that the scripts read, so the code can be tried before the CFPS data arrive. Nothing in these files comes from CFPS, and no result from them says anything about China. From the repository folder, run the two lines below, then 01_checks.R to 05_checks.R as in the previous section.

```bash
Rscript --vanilla tests/make_test_data.R "$HOME/cfps_test" 0.25
```

```bash
export CFPS_DATA="$HOME/cfps_test"
```

The last number of the first line sets the size. 1 gives about 14,800 families in 2010, close to the CFPS samples, and 0.25 a quarter of that. At 0.25 the five scripts take about three minutes on an Apple M2, and setting `CFPS_M` to 5 shortens the run further. The results go into `$HOME/cfps_test/Output`.

## What the analysis does

### Data

02_link.R links the five waves by person ID and family ID. Ethnicity, hukou at age 3, birth year and gender come from the earliest wave with a valid value, the adult file before the child file. Parents' education is the higher level of the two parents in the earliest family roster with a valid value. People first interviewed in 2018 with no value for hukou at age 3 take their hukou at age 12 from the 2018 file (check R7 leaves them out). Ethnicity is coded Han or non-Han, and hukou at age 3 agricultural or non-agricultural.

### Samples

Each sample keeps the respondents with a positive weight and known values of the outcome and of ethnicity, hukou at age 3, birth year, gender and province. Table S1 shows the respondents left after each step.

| Sample | Respondents | Used in |
|---|---|---|
| A-2010, A-2018 | adult respondents aged 25 or older | Tables I and II, Figure 1, Tables S3 and S4 |
| A-2012, A-2014, A-2016 | the same in the other waves | Table S5 |
| C-lang-2010, C-lang-2012 | adult respondents aged 16 or older with a known home language | Table III |
| C-rating-2010 to C-rating-2016 | the same with a known interviewer's rating of Mandarin | Table III |
| C-wordtest-2010, 2014, 2018 | the same with a known word test score | Table III |
| D-A10 | A-2010 respondents with all three language measures | Table IV, panel A |
| B-cohort | child respondents of 2010 aged 12 to 15, interviewed again in 2018 (positive panel weight from 2010) | Table IV, panel B |
| B-cohort-lang | B-cohort with all three language measures of 2010 | Table IV, panel B |
| B-enrol-16-22, B-enrol-16-18, B-enrol-19-22 | adult respondents aged 16 to 22, waves 2010 to 2018 pooled | Table S6 |
| B-cohort-10to15, B-cohort-lang-10to15 | as B-cohort, children aged 10 to 15 in 2010 | check R14 |
| R15-rating-2012, 2014, 2016 | as C-rating, keeping the Mandarin rating code 79 | check R15 |

### Outcomes

| Outcome | Model |
|---|---|
| Education in five levels (illiterate or semi-literate, primary school, junior high school, senior high school, college or above) | ordered logit |
| Four school transitions (primary school or above, then each next level among those who reached the level before) | logit |
| Home language (Mandarin, minority language, Chinese dialect) | multinomial logit, Chinese dialect as the base |
| Interviewer's rating of Mandarin, seven levels | ordered logit |
| Word test score | linear |
| Senior high school or above by 2018, and college by 2018 (for college, the highest stage attended also counts) | logit |
| School enrolment | logit |

### Models

| Model | Terms |
|---|---|
| M1 | non-Han, five-year birth cohort (before 1940 as one group), gender |
| M2 | M1 plus agricultural hukou at age 3 and its interaction with non-Han |
| M3 | M2 plus parents' education |
| M4 | M3 plus province |
| M4 + education | M4 plus the respondent's education (Table III) |
| base, B-cohort | non-Han, agricultural hukou at age 3 and their interaction, gender, parents' education, log of family income per person, province and age, all from 2010 |
| base, B-enrol | the same terms in the wave of the interview, plus the wave |

### Estimation

- Every model uses the CFPS survey design (PSUs, strata and weights, the weights rescaled to a mean of 1). The design is set up on every respondent with a positive weight, so that each stratum keeps all its PSUs, and is then restricted to the sample (domain estimation). Ordered logits use `svyolr`, logits and linear models `svyglm`, and the multinomial logit `svy_vglm` of svyVGAM.
- Each effect is the average marginal effect (AME) of non-Han ethnicity, for all respondents, within each hukou group and for the difference between the two groups. For an ordered outcome, the effects are on the probability of reaching each level or above (targets `ge2` and up) and on the mean level (`mean`). Standard errors come from the design-based variance by the delta method.
- Missing values of parents' education (all samples), of education (samples C) and of family income (samples B) are imputed by predictive mean matching with mice, in 20 data sets, and the estimates are pooled by Rubin's rules. M1 and M2 need no imputed values.
- The categories of an ordered outcome are merged with a neighbouring category, the smallest first, until every category holds at least 20 Han and 20 non-Han respondents or two categories remain. A birth cohort with fewer than 30 respondents, or in which the outcome does not vary, is merged with its neighbour, and small provinces are pooled with the reference province. The column `merge_note` of each table records these merges.
- Table IV, panel A, splits the effect of non-Han ethnicity on education in D-A10 into a part that runs through language and a direct part with the KHB method (Karlson, Holm and Breen), with home language in step 1 and the rating of Mandarin and the word test added in step 2.

### Robustness checks (Table S7)

| Check | What changes |
|---|---|
| R1 | no weights |
| R2 | complete cases, no imputation |
| R3 | proxy interviews excluded (waves 2014 to 2018) |
| R5 | each non-Han group with at least 100 respondents in the sample, against Han |
| R6 | models by decade of birth, and the transition to college for people born in 1980 or earlier and in 1981 or later |
| R7 | hukou at age 3 only, leaving out the values taken from hukou at age 12 |
| R8 | years of schooling, Oaxaca and Blinder decomposition |
| R9 | the 2010 province in place of the province of 2018 |
| R10 | partial proportional odds models |
| R11 | the national resample with its own weights (waves 2010 to 2016) |
| R12 | transitions by the highest stage attended |
| R13 | six education levels |
| R14 | children aged 10 to 15 in 2010 in place of 12 to 15 |
| R15 | code 79 of the Mandarin rating as a category below the lowest rating |

Check R4 (telephone interviews) needs the waves 2020 and 2022 and is not run.

## Output files and the tables of the paper

| Exhibit | What it shows | Files in `Output (checked)` | Written by |
|---|---|---|---|
| Table I | characteristics of the samples by ethnicity and hukou at age 3, 2010 and 2018 | `T1_describe.csv` | 03_describe.R |
| Table II | AMEs of non-Han ethnicity on education, models M1 to M4 | `T2_ame_main.csv` | 04_models.R |
| Table III | AMEs on home language, the rating of Mandarin and the word test | `T3_ame_language.csv` | 04_models.R |
| Table IV | panel A, KHB decomposition; panel B, education by 2018 of the children aged 12 to 15 in 2010 | `T4_khb.csv`, `T4_bcohort.csv` | 04_models.R |
| Figure 1 | share completing each school transition by birth cohort, ethnicity and hukou at age 3 | `F1_transitions.csv` | 03_describe.R |
| Table S1 | sample flow | `S1_flow.csv` | 03_describe.R |
| Table S2 | non-Han groups and provinces | `S2_groups.csv`, `S2_provinces.csv` | 03_describe.R |
| Table S3 | full model results | `S3_coef_A.csv`, `S3_coef_C.csv`, `S3_coef_B.csv` | 04_models.R |
| Table S4 | school transitions | `S4_transitions.csv` | 04_models.R |
| Table S5 | waves 2012 to 2016 | `S5_ame_other.csv`, `S3_coef_Aother.csv` | 04_models.R |
| Table S6 | school enrolment at ages 16 to 22 | `S6_ame_enrol.csv` | 04_models.R |
| Table S7 | robustness checks | `S7_robust.csv`, `S7_oaxaca.csv`, `S7_r10_selection.csv` | 05_checks.R |
| Table S8 | missing and imputed values | `S8_imputation.csv` | 04_models.R |
| Table S9 | children followed to 2018 or lost | `S9_attrition.csv` | 03_describe.R |
| Table S10 | language of the classmates and of the interview | `S10_classmates.csv` | 03_describe.R |
| Table S11 | the same birth cohorts in 2010 and 2018 | `S11_cohorts.csv` | 03_describe.R |
| none | data checks (weight variables, files found, missing values, weights) | `C0_weight_names.csv`, `C1_files.csv`, `C2_missing.csv`, `C4_weights.csv` | 01_checks.R |

The effect tables share their columns. `target` names the quantity (`ge2` and up, `mean`, `pr` for a probability, or `pr_mandarin`, `pr_minority` and `pr_dialect`), `group` is `all`, `agricultural`, `non-agricultural` or `difference`, `df` holds the degrees of freedom of Rubin's rules, `m` the number of imputed data sets, and `n` and `n_nonhan` the respondents.

## Hidden counts

- The R scripts show a count of respondents from 1 to 9 as `<10`, and leave out an effect for a group with fewer than 10 non-Han respondents. In the Word tables an en dash (–) marks a value that is not reported.
- On 24 October 2026, after the run, 76 more cells in three files were hidden, because each could be combined with another published number to give a count from 1 to 9. These were 6 counts of non-Han respondents in `T4_bcohort.csv`, 35 in `S7_robust.csv`, and 35 cells of `S8_imputation.csv` (4 counts, 27 percentages and 4 imputed means). The counts read `suppressed` and the other cells are blank. No estimate changed. analysis_tools.R now hides these cells itself, which is why the sizes of the three files differ from those in `Last run.txt`.
- Table S1 shows no count from 1 to 9, but in 66 places, across 20 samples, two adjacent steps differ by 1 to 9 respondents. These differences are part of the sample flow and are shown.

## Models fitted differently

- Two models could not be estimated. In C-lang-2012, models M4 and M4 + education, the category minority language has fewer than 20 Han respondents. Their rows in `T3_ame_language.csv` read `not estimable`.
- Nine models were fitted without the non-Han respondents with non-agricultural hukou at age 3, and without the interaction, because the outcome does not vary among those respondents. Only the effect for the agricultural hukou group is shown for these models. Three are in `T4_bcohort.csv` (senior high school or above by 2018 in B-cohort, in B-cohort-lang, and in B-cohort-lang with the language measures). Six are in `S7_robust.csv` (checks R1, R2 and R3 on the same B-cohort model, and check R5 for Tibetan and Yi respondents in A-2010 and for Tibetan respondents in A-2018). The column `merge_note` says so in each row.

## Notes

- The waves 2020 and 2022 are not used, because their archives could not be opened. Check R4 needs them and is not run.
- The comments of the R scripts cite sections of the analysis plan, which was written before the data were analysed. The plan is not part of this repository.
- Two earlier runs on the CFPS data found models that could not be estimated. Each time the code was corrected before any table of estimates from that run was read, as the status notes at the top of the scripts record. The third run, of 24 October 2026, gave the published results.
- The repository leaves out everything the CFPS user agreement keeps private, namely the data, any file built from person records (`link.rds`, `imp.rds`) and the CFPS documentation, which CFPS provides to approved users.

## Licence

The code is under the MIT licence in `LICENSE`. The licence does not cover the CFPS data, which remain under the CFPS user agreement. Cite the CFPS as its data platform asks.
