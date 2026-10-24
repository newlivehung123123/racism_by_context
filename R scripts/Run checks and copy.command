#!/bin/bash
# Runs 01_checks.R, which reads 00_setup.R first, with no checking by hand.
# Quit every AI app, then double-click the file in Finder. The file opens the
# disk CFPS data unless the disk is open already (it asks for the disk
# password), unpacks any packed CFPS data (it asks for the archive password
# when a file is locked), runs the checks, screens the folder Output without
# showing any value from it, copies Output into the folder Output (checked) in
# the project folder only when the screen finds nothing wrong, and ejects the
# disk. What happened is written, with no value from the data, to the file
# Last run.txt next to this file.
#
# Both passwords show while they are typed, so they can be checked. The window
# and its scrollback are wiped as soon as the passwords have been used, so
# nothing copied from the window holds a password, and no password is written
# to Last run.txt.
#
# The screen accepts only the five tables and two logs that 01_checks.R
# writes, each with the columns 01_checks.R gives it, and stops the copy at
# any other file, at more than 5,000 rows in a table, and at any number of
# nine or more digits, the length of a CFPS person ID.
#
# Tests on made-up data point the file at another disk image and at another
# 7-Zip, or at none, through the settings CFPS_IMAGE, CFPS_DATA and CFPS_7ZZ.

IMAGE="${CFPS_IMAGE:-/Users/Shared/CFPS data.sparsebundle}"
DATA="${CFPS_DATA:-/Volumes/CFPS data}"
export CFPS_DATA="$DATA"
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
cd "$(dirname "$0")" || exit 1
CHECKED="$(dirname "$(pwd)")/Output (checked)"
REPORT="$(pwd)/Last run.txt"
date "+Run started %d %B %Y at %H.%M" > "$REPORT"

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
  note "An AI app (Claude or ChatGPT) is open, so nothing was run. Quit every AI app with Command-Q, then double-click Run checks and copy.command again."
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
    note "The disk opened under another name, so nothing was run. Eject every CFPS disk in Finder, then double-click Run checks and copy.command again."
    finish 1
  fi
  note "The disk was opened."
fi

# 3. Unpack any packed CFPS data. The 2020 and 2022 releases arrive as
# password-protected zip and rar files. The full 7-Zip from 7-zip.org, kept in
# the hidden folder .local in the home folder, opens zip, 7z and rar files,
# including zip files that the Mac's own tool cannot open. Without it, another
# 7-Zip or the Mac's own tool is used. Type the archive password, which is in
# the CFPS instructions document, when asked. It shows while it is typed, and
# the window is wiped once the file is done. A password that works is reused
# for the next file. Each archive is unpacked into a new folder inside a folder
# "... (unpacking)" beside it, which the checks ignore, and the new folder
# becomes the folder "... (unpacked)" once every file is out. Archives that
# come out of an archive are unpacked the same way. When a file does not open,
# the message of the unpacker is written to Last run.txt. The password never is.
note "Looking for packed CFPS data."
RAW="$DATA/Newly downloaded Data"
SEVENZ=""
SEVENZ_RAR=""
if [ -n "$CFPS_7ZZ" ]; then
  candidates=("$CFPS_7ZZ")
else
  candidates=("$HOME/.local/7zip-26.03/7zz" /opt/homebrew/bin/7zz /usr/local/bin/7zz)
fi
for z in "${candidates[@]}"; do
  [ -x "$z" ] || continue
  codecs=$("$z" i < /dev/null 2> /dev/null) || continue
  if printf '%s\n' "$codecs" | grep -q '[[:space:]]Rar[35]$'; then
    SEVENZ="$z"
    SEVENZ_RAR=1
    break
  fi
  [ -n "$SEVENZ" ] || SEVENZ="$z"
done
if [ -n "$SEVENZ_RAR" ]; then
  note "7-Zip is used for zip, 7z and rar files ($SEVENZ)."
elif [ -n "$SEVENZ" ]; then
  note "7-Zip is used for zip and 7z files ($SEVENZ). It cannot open rar files."
else
  note "7-Zip was not found, so the Mac's own tool, which cannot open every zip file, is used."
fi

# Tries one password on one archive, with 7-Zip when USE7 is set and with the
# Mac's own tool otherwise. Sets RESULT to ok, to locked when the password did
# not open the file, or to failed for any other problem, and WHY to the first
# lines of the message of the unpacker, without the folder names. The message
# is read byte by byte, because file names in Chinese encodings are not valid
# text, and each character other than plain letters, digits and punctuation
# becomes a question mark.
attempt() {
  local out rc cut1="$2/" cut2="$RAW/"
  if [ -n "$USE7" ]; then
    out=$("$SEVENZ" x -y -aoa -bso0 -bsp0 -p"$3" -o"$2" "$1" < /dev/null 2>&1)
    rc=$?
    if [ "$rc" -le 1 ]; then
      RESULT=ok
    elif printf '%s' "$out" | LC_ALL=C grep -q 'Wrong password'; then
      RESULT=locked
    else
      RESULT=failed
    fi
  else
    out=$(/usr/bin/tar -xf "$1" -C "$2" --passphrase "$3" < /dev/null 2>&1 > /dev/null)
    rc=$?
    if [ "$rc" -eq 0 ]; then
      RESULT=ok
    elif printf '%s' "$out" | LC_ALL=C grep -q 'Incorrect passphrase\|Passphrase required'; then
      RESULT=locked
    else
      RESULT=failed
    fi
  fi
  WHY=$(printf '%s\n' "$out" | LC_ALL=C tr -c '[:print:]\n' '?' | LC_ALL=C grep -v '^[[:space:]?]*$' | head -4 | LC_ALL=C tr '\n' ' ')
  WHY=${WHY//"$cut1"/}
  WHY=${WHY//"$cut2"/}
  WHY=${WHY:0:300}
}

# Unpacks one archive, asking for the password only when the file is locked
# and no password typed before opens it.
unpack_one() {
  local a="$1" base dest tmp pw tries=0 asked=""
  base=$(basename "$a")
  dest="${a%.*} (unpacked)"
  if [ -d "$dest" ] && [ -n "$(ls -A "$dest" 2> /dev/null)" ]; then
    note "Already unpacked: $base"
    return
  fi
  USE7=""
  case "$base" in
    *.[Rr][Aa][Rr]) [ -n "$SEVENZ_RAR" ] && USE7=1 ;;
    *) [ -n "$SEVENZ" ] && USE7=1 ;;
  esac
  tmp="${a%.*} (unpacking)/$(date '+%Y-%m-%d %H.%M.%S')"
  mkdir -p "$tmp"
  echo "Unpacking $base. This can take several minutes."
  attempt "$a" "$tmp" none
  if [ "$RESULT" = locked ] && [ -n "$PW_OK" ]; then
    attempt "$a" "$tmp" "$PW_OK"
  fi
  while [ "$RESULT" = locked ] && [ "$tries" -lt 3 ]; do
    tries=$((tries + 1))
    echo
    echo "The file $base is locked. Type the archive password from the CFPS instructions. It shows while you type, so check it, then press Return. Press Return alone to skip this file."
    read -r -p "Archive password: " pw
    if [ -z "$pw" ]; then
      RESULT=skipped
      break
    fi
    asked=1
    echo "Unpacking $base. This can take several minutes."
    attempt "$a" "$tmp" "$pw"
    if [ "$RESULT" = ok ]; then
      PW_OK="$pw"
    elif [ "$RESULT" = locked ]; then
      echo "That password did not open the file."
    fi
  done
  pw=""
  [ -n "$asked" ] && wipe
  if [ "$RESULT" = ok ]; then
    rmdir "$dest" 2> /dev/null
    if mv "$tmp" "$dest" 2> /dev/null; then
      if [ -n "$USE7" ]; then
        note "Unpacked with 7-Zip: $base"
      else
        note "Unpacked: $base"
      fi
      return
    fi
    note "Not usable: $base was unpacked, but the folder ${base%.*} (unpacked) could not be made."
  elif [ "$RESULT" = skipped ]; then
    note "Not unpacked (skipped): $base"
  elif [ "$RESULT" = locked ]; then
    note "Not unpacked, none of the three passwords typed opened it: $base"
  else
    note "Not unpacked: $base. The unpacker said: $WHY"
    case "$base" in
      *.[Rr][Aa][Rr]) [ -n "$SEVENZ_RAR" ] || note "The full 7-Zip, which opens rar files, was not found." ;;
    esac
  fi
  n_left=$((n_left + 1))
  left_names="$left_names, $base"
}

PW_OK=""
n_arc=0
n_left=0
left_names=""
handled=""
for pass in 1 2 3; do
  more=""
  while IFS= read -r a <&3; do
    [ -n "$a" ] || continue
    printf '%s\n' "$handled" | LC_ALL=C grep -Fxq -- "$a" && continue
    handled="$handled
$a"
    more=1
    n_arc=$((n_arc + 1))
    unpack_one "$a"
  done 3<<EOF
$(/usr/bin/find "$RAW" -type f \( -iname '*.zip' -o -iname '*.7z' -o -iname '*.rar' \) ! -name '._*' ! -path '*(unpacking)*' ! -path '*/__MACOSX/*' 2> /dev/null)
EOF
  [ -n "$more" ] || break
done
PW_OK=""
if [ "$n_arc" -eq 0 ]; then
  note "No packed CFPS data to open."
fi
if [ "$n_left" -gt 0 ]; then
  note "Still packed: ${left_names#, }"
fi

# 4. Run the checks. R reads nothing from the keyboard.
note "Running the checks, which can take several minutes."
/Library/Frameworks/R.framework/Resources/bin/Rscript --vanilla 01_checks.R < /dev/null
status=$?
echo
if [ "$status" -eq 0 ]; then
  note "The checks finished."
else
  note "The checks stopped with an error (R exit status $status). The log in Output/logs gives the details."
fi

# 5. Screen the folder Output, and copy the folder only when the screen
# finds nothing wrong.
if [ ! -d "$DATA/Output" ]; then
  note "The checks wrote no folder Output, so nothing was copied."
  status=1
else
  echo "Screening the folder Output."
  echo "Screen of the folder Output (names, sizes, row counts and long numbers only)" >> "$REPORT"
  /usr/bin/python3 -I - "$DATA/Output" >> "$REPORT" 2>&1 <<'PYEOF'
import csv
import os
import re
import sys

root = sys.argv[1]
EXPECTED = {
    "C0_weight_names.csv": ["wave", "ftype", "varname", "varlabel", "vtype", "n_pos_shown"],
    "C1_files.csv": ["wave", "ftype", "file_type", "file", "records", "nvars", "idvar",
                     "n_badid_shown", "n_dupid_shown", "status"],
    "C2_missing.csv": ["wave", "ftype", "varname", "found", "vtype", "varlabel", "records",
                       "n_m8_shown", "pct_m8", "n_miss_shown", "pct_miss", "n_neg_shown", "pct_neg"],
    "C3_first_wave.csv": ["firstwave", "grp", "n_shown"],
    "C4_weights.csv": ["wave", "ftype", "wname", "ss", "sp", "grp", "n_shown", "npos_shown", "wsum"],
}
LOGS = {
    os.path.join("logs", "00_setup.log"): "00_setup.R finished.",
    os.path.join("logs", "01_checks.log"): "01_checks.R finished.",
}
CELL_ID = re.compile(r"^\s*\d{9,}\s*$")
TEXT_ID = re.compile(r"(?<![\d.,])\d{9,}(?![\d.,])")
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
        for r in body:
            for i, cell in enumerate(r):
                if CELL_ID.match(cell):
                    name = header[i] if ok and i < len(header) else f"column {i + 1}"
                    ids[name] = ids.get(name, 0) + 1
        if ids:
            problems.append(f"{rel} ID-like numbers by column {ids}")
        print(f"{rel}: {size} bytes, {len(body)} rows, columns as designed {ok}, "
              f"ID-like numbers {sum(ids.values())}")
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

# 6. Eject the disk.
eject_disk || status=1
if [ "$status" -eq 0 ]; then
  if [ "${n_left:-0}" -gt 0 ]; then
    note "The checks and the copy worked, but some packed data stayed packed. Last run.txt in the folder R scripts says which file and why."
  else
    note "Everything worked."
  fi
else
  note "Something did not work. Last run.txt in the folder R scripts says what."
fi
finish "$status"
