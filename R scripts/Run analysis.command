#!/bin/bash
# Runs the analysis with no checking by hand: 02_link.R, 03_describe.R,
# 04_models.R and 05_checks.R in turn, each of which reads 00_setup.R first.
# Quit every AI app, then double-click the file in Finder. The file opens the
# disk CFPS data unless the disk is open already (it asks for the disk
# password), runs the four scripts, screens the folder Output without showing
# any value from it, copies Output into the folder Output (checked) in the
# project folder only when the screen finds nothing wrong, and ejects the
# disk. What happened is written, with no value from the data, to the file
# Last run.txt next to this file.
#
# The run takes an hour or more. The Mac stays awake while the scripts run as
# long as its lid stays open. Keep it plugged in.
#
# The disk password shows while it is typed, so it can be checked. The window
# and its scrollback are wiped as soon as the password has been used, so
# nothing copied from the window holds the password, and the password is not
# written to Last run.txt.
#
# When 02_link.R stops with an error, nothing else runs, because the other
# scripts need its result. When 03_describe.R stops, the models still run.
# 05_checks.R runs only when 04_models.R saved the imputed data sets. The
# screen and the copy run in every case, so the log of a script that stopped
# reaches Output (checked) and gives the reason.
#
# The screen accepts only the tables and logs that 00_setup.R to 05_checks.R
# write, each with the columns its script gives it, and stops the copy at any
# other file, at more than 5,000 rows in a table, at any number of nine or
# more digits, the length of a CFPS person ID, and at any count of
# respondents from 1 to 9.
#
# Tests on made-up data point the file at another disk image through the
# settings CFPS_IMAGE and CFPS_DATA.

IMAGE="${CFPS_IMAGE:-/Users/Shared/CFPS data.sparsebundle}"
DATA="${CFPS_DATA:-/Volumes/CFPS data}"
export CFPS_DATA="$DATA"
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
cd "$(dirname "$0")" || exit 1
CHECKED="$(dirname "$(pwd)")/Output (checked)"
REPORT="$(pwd)/Last run.txt"
RSCRIPT=/Library/Frameworks/R.framework/Resources/bin/Rscript
date "+Run analysis started %d %B %Y at %H.%M" > "$REPORT"

# Shows a line in the window and writes the line to Last run.txt.
note() {
  echo "$1"
  echo "$1" >> "$REPORT"
}

# Clears the window and its scrollback, so that a password typed in view is
# gone, then shows again what Last run.txt holds so far.
wipe() {
  printf '\033[H\033[2J\033[3J'
  cat "$REPORT"
}

# Ejects the disk, with two more tries after a pause, and never by force.
eject_disk() {
  [ -d "$DATA" ] || return 0
  for pause in 0 3 10; do
    sleep "$pause"
    if hdiutil detach "$DATA" > /dev/null 2>&1; then
      note "The disk is ejected."
      return 0
    fi
  done
  note "The disk did not eject. Eject the disk CFPS data in Finder with the eject button next to its name in the sidebar."
  return 1
}

finish() {
  trap - INT TERM HUP
  date "+Run ended %d %B %Y at %H.%M" >> "$REPORT"
  echo
  read -r -p "Press Return to close the window."
  echo
  exit "$1"
}

# A run stopped with Control-C, or by closing the window, still wipes the
# window and ejects the disk.
trap 'wipe; echo; note "The run was stopped before the end."; eject_disk; finish 1' INT TERM HUP

# 1. With an AI app open, stop and eject the disk, as the CFPS rules require.
if pgrep -ai '^(claude|chatgpt)$' > /dev/null; then
  note "An AI app (Claude or ChatGPT) is open, so nothing was run. Quit every AI app with Command-Q, then double-click Run analysis.command again."
  pgrep -ail '^(claude|chatgpt)$' >> "$REPORT"
  eject_disk
  finish 1
fi

# 2. Open the disk unless the disk is open already. The disk password shows
# while it is typed, there are three tries, and the window is wiped afterwards.
if [ -d "$DATA" ]; then
  note "The disk was open already."
else
  if [ ! -e "$IMAGE" ]; then
    note "The disk image $IMAGE was not found, so nothing was run."
    finish 1
  fi
  opened=""
  why=""
  tries=0
  while [ -z "$opened" ] && [ "$tries" -lt 3 ]; do
    tries=$((tries + 1))
    echo
    echo "Type the DISK password, the one that opens the disk CFPS data. It shows while you type, so check it, then press Return."
    read -r -p "Disk password: " dpw
    why=$(printf '%s' "$dpw" | hdiutil attach -stdinpass "$IMAGE" 2>&1 > /dev/null) && opened=1
    if [ -z "$opened" ]; then
      if printf '%s' "$why" | grep -q "Authentication error"; then
        echo "That disk password did not open the disk."
      else
        break
      fi
    fi
  done
  unset dpw
  wipe
  if [ -z "$opened" ]; then
    if printf '%s' "$why" | grep -q "Authentication error"; then
      note "None of the three disk passwords opened the disk, so nothing was run."
    else
      note "The disk did not open, so nothing was run. macOS said: $(printf '%s' "$why" | tr '\n' ' ')"
    fi
    finish 1
  fi
  if [ ! -d "$DATA" ]; then
    note "The disk opened under another name, so nothing was run. Eject every CFPS disk in Finder, then double-click Run analysis.command again."
    finish 1
  fi
  note "The disk was opened."
fi

# 3. Run the four scripts in turn. R reads nothing from the keyboard, and
# caffeinate keeps the Mac from sleeping while R runs.
status=0

# Runs one script and notes when it started and how it ended.
run_r() {
  note "$(date '+%H.%M') Running $1, $2."
  /usr/bin/caffeinate -i "$RSCRIPT" --vanilla "$1" < /dev/null
  local rc=$?
  echo
  if [ "$rc" -eq 0 ]; then
    note "$(date '+%H.%M') $1 finished."
  else
    note "$(date '+%H.%M') $1 stopped with an error (R exit status $rc). Its log in Output/logs gives the reason, or the window does when there is no log."
    status=1
  fi
  return "$rc"
}

if run_r 02_link.R "which links the waves"; then
  run_r 03_describe.R "which makes the descriptive tables"
  run_r 04_models.R "which imputes and fits the models, the longest step"
  if [ "$DATA/Work/imp.rds" -nt "$DATA/Work/link.rds" ]; then
    run_r 05_checks.R "which runs the robustness checks, also long"
  else
    note "05_checks.R was not run, because 04_models.R saved no imputed data sets."
    status=1
  fi
else
  note "03_describe.R, 04_models.R and 05_checks.R were not run, because they need the result of 02_link.R."
fi

# 4. Screen the folder Output, and copy the folder only when the screen
# finds nothing wrong.
if [ ! -d "$DATA/Output" ]; then
  note "The scripts wrote no folder Output, so nothing was copied."
  status=1
else
  echo "Screening the folder Output."
  echo "Screen of the folder Output (names, sizes, row counts, long numbers and small counts only)" >> "$REPORT"
  /usr/bin/python3 -I - "$DATA/Output" >> "$REPORT" 2>&1 <<'PYEOF'
import csv
import os
import re
import sys

root = sys.argv[1]
AME = ["sample", "model", "outcome", "target", "group", "estimate", "se", "ci_low", "ci_high",
       "p", "df", "n", "n_nonhan", "m", "merge_note"]
COEF = ["sample", "model", "outcome", "term", "estimate", "se", "ci_low", "ci_high", "p", "df",
        "n", "n_nonhan", "m"]
EXPECTED = {
    # 01_checks.R
    "C0_weight_names.csv": ["wave", "ftype", "varname", "varlabel", "vtype", "n_pos_shown"],
    "C1_files.csv": ["wave", "ftype", "file_type", "file", "records", "nvars", "idvar",
                     "n_badid_shown", "n_dupid_shown", "status"],
    "C2_missing.csv": ["wave", "ftype", "varname", "found", "vtype", "varlabel", "records",
                       "n_m8_shown", "pct_m8", "n_miss_shown", "pct_miss", "n_neg_shown", "pct_neg"],
    "C3_first_wave.csv": ["firstwave", "grp", "n_shown"],
    "C4_weights.csv": ["wave", "ftype", "wname", "ss", "sp", "grp", "n_shown", "npos_shown", "wsum"],
    # 03_describe.R
    "T1_describe.csv": ["sample", "group", "variable", "level", "estimate", "se", "n_shown"],
    "F1_transitions.csv": ["source", "cohort", "group", "transition", "estimate", "se", "n_shown"],
    "S1_flow.csv": ["sample", "step", "group", "n_shown"],
    "S2_groups.csv": ["sample", "ethcode", "name", "n_shown", "pct"],
    "S2_provinces.csv": ["sample", "prov_code", "prov_name", "group", "n_shown"],
    "S9_attrition.csv": ["group", "variable", "status", "estimate", "se", "n_shown"],
    "S10_classmates.csv": ["wave", "item", "group", "level", "estimate", "se", "n_shown"],
    "S11_cohorts.csv": ["cohort", "source", "group", "variable", "estimate", "se", "n_shown"],
    # 04_models.R
    "T2_ame_main.csv": AME,
    "T3_ame_language.csv": AME,
    "T4_bcohort.csv": AME,
    "S4_transitions.csv": AME,
    "S5_ame_other.csv": AME,
    "S6_ame_enrol.csv": AME,
    "S3_coef_A.csv": COEF,
    "S3_coef_Aother.csv": COEF,
    "S3_coef_C.csv": COEF,
    "S3_coef_B.csv": COEF,
    "T4_khb.csv": ["sample", "step", "outcome", "group", "quantity", "estimate", "se", "ci_low",
                   "ci_high", "p", "df", "n", "n_nonhan", "m"],
    "S8_imputation.csv": ["sample", "variable", "group", "n", "n_missing_shown", "pct_missing",
                          "mean_observed", "mean_imputed", "logged_events", "m"],
    # 05_checks.R
    "S7_robust.csv": ["check", "variant"] + AME,
    "S7_oaxaca.csv": ["sample", "component", "estimate", "se", "ci_low", "ci_high", "p", "df",
                      "pct", "n", "n_nonhan", "m"],
    "S7_r10_selection.csv": ["sample", "step", "term", "wald", "df", "p", "decision"],
}
LOGS = {
    os.path.join("logs", s + ".log"): s + ".R finished."
    for s in ["00_setup", "01_checks", "02_link", "03_describe", "04_models", "05_checks"]
}
CELL_ID = re.compile(r"^\s*\d{9,}\s*$")
TEXT_ID = re.compile(r"(?<![\d.,])\d{9,}(?![\d.,])")
# A count of respondents from 1 to 9, which the scripts show as <10
SMALL = re.compile(r"^\s*[1-9](\.0*)?\s*$")
COUNT_COLS = {"n", "n_nonhan"}
MAXBYTES = 2_000_000
MAXROWS = 5000
problems = []

found = []
for dirpath, dirnames, filenames in os.walk(root):
    for f in filenames:
        found.append(os.path.relpath(os.path.join(dirpath, f), root))

for rel in sorted(found):
    path = os.path.join(root, rel)
    size = os.path.getsize(path)
    if os.path.basename(rel) == ".DS_Store":
        print(f"{rel}: Finder settings file, ignored")
        continue
    if size > MAXBYTES:
        problems.append(f"{rel} is larger than 2 MB")
    if rel in EXPECTED:
        with open(path, encoding="utf-8-sig", errors="replace", newline="") as fh:
            rows = list(csv.reader(fh))
        header, body = (rows[0], rows[1:]) if rows else ([], [])
        ok = header == EXPECTED[rel]
        if not ok:
            problems.append(f"{rel} has other columns than designed ({len(header)} columns)")
        if len(body) > MAXROWS:
            problems.append(f"{rel} has {len(body)} rows")
        widths = {len(r) for r in body}
        if widths - {len(header)}:
            problems.append(f"{rel} has rows of other widths {sorted(widths)}")
        ids = {}
        small = {}
        for r in body:
            for i, cell in enumerate(r):
                name = header[i] if i < len(header) else f"column {i + 1}"
                if CELL_ID.match(cell):
                    ids[name] = ids.get(name, 0) + 1
                if (name in COUNT_COLS or name.endswith("_shown")) and SMALL.match(cell):
                    small[name] = small.get(name, 0) + 1
        if ids:
            problems.append(f"{rel} ID-like numbers by column {ids}")
        if small:
            problems.append(f"{rel} counts from 1 to 9 by column {small}")
        print(f"{rel}: {size} bytes, {len(body)} rows, columns as designed {ok}, "
              f"ID-like numbers {sum(ids.values())}, counts from 1 to 9 {sum(small.values())}")
    elif rel in LOGS:
        with open(path, encoding="utf-8", errors="replace") as fh:
            text = fh.read()
        lines = text.splitlines()
        nid = len(TEXT_ID.findall(text))
        done = LOGS[rel] in text
        nerr = sum(1 for l in lines if l.startswith("Error."))
        if nid:
            problems.append(f"{rel} holds {nid} ID-like numbers")
        print(f"{rel}: {size} bytes, {len(lines)} lines, finished {done}, "
              f"error lines {nerr}, ID-like numbers {nid}")
    else:
        problems.append(f"unexpected file {rel} ({size} bytes)")

for rel in list(EXPECTED) + list(LOGS):
    if rel not in found:
        print(f"{rel}: missing")

print("CLEAN" if not problems else "NOT CLEAN")
for p in problems:
    print("  " + p)
sys.exit(0 if not problems else 3)
PYEOF
  screen=$?
  if [ "$screen" -eq 0 ]; then
    if ditto "$DATA/Output" "$CHECKED"; then
      note "The screen found nothing wrong, and the files are copied into Output (checked) in the project folder."
    else
      note "The copy failed, so Output (checked) may be incomplete."
      status=1
    fi
  else
    note "The screen found a problem, so nothing was copied. Last run.txt says which file."
    status=1
  fi
fi

# 5. Eject the disk.
eject_disk || status=1
if [ "$status" -eq 0 ]; then
  note "Everything worked."
else
  note "Something did not work. Last run.txt in the folder R scripts says what."
fi
finish "$status"
