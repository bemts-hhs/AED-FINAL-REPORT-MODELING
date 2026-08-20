###___________________________________________________________________________
# Setup for the modeling arm of the AED final report ----
###___________________________________________________________________________

## install pak ----
install.packages("pak")

## install tidyverse, tidymodels, renv is loaded via Positron project template ----
pak::pak("tidyverse")
pak::pak("tidymodels")
pak::pak("cpp11") # required for readxl?
pak::pak("progress") # required for readxl?
pak::pak("finetune")
pak::pak("xgboost")
pak::pak("ranger")
pak::pak("lme4")
pak::pak("glmnet")
pak::pak("RcppArmadillo")
pak::pak("RcppEigen")
pak::pak("themis")
pak::pak("lorax")
pak::pak("marginaleffects")
pak::pak("sessioninfo")

## access the environment variable ----
aed_file <- Sys.getenv("aed_path")

## load data ----
aed_final <- readxl::read_excel(aed_file)

## get session info ----

### initialize the session object ----
session_init <- sessioninfo::session_info()

### convert to long format with one-to-one fields ----
session <- session_init |>
  as.data.frame() |>
  dplyr::select(
    platform.version,
    platform.os,
    platform.system,
    platform.ui,
    platform.language,
    platform.collate,
    platform.ctype,
    platform.tz,
    platform.date,
    platform.pandoc,
    platform.quarto
  ) |>
  dplyr::distinct() |>
  dplyr::rename_all(~ stringr::str_remove(., "platform\\.")) |>
  tidyr::pivot_longer(cols = tidyselect::everything())

### write the session object to disk ----
readr::write_csv(x = session, file = "./output/R_session_info.csv")
