required_packages <- c(
  "readxl",
  "ggplot2",
  "dplyr",
  "tidyr",
  "car",
  "lmtest",
  "nortest"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(required_packages, library, character.only = TRUE))

options(stringsAsFactors = FALSE)
theme_set(theme_bw())