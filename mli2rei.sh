  echo pwd
  chmod +x
  find . -name "*.mli" | while read -r filename; do bsrefmt --parse ml --print re "$filename" >"${filename%.mli}.rei"; done