function __convert_ml_to_re() {
  echo pwd
  find . -name "*.ml" | while read filename; do bsrefmt --parse ml --print re $filename >${filename%.ml}.re; done

}