
  echo pwd
  fd .ml | while read filename; do bsrefmt --parse ml --print re $filename >${filename%.ml}.re; done
