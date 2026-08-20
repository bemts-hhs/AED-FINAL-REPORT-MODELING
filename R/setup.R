###___________________________________________________________________________
# Setup for the modeling arm of the AED final report ----
###___________________________________________________________________________

## install pak ----
# install.packages("pak")

## install tidyverse, tidymodels, renv is loaded via Positron project template ----
# pak::pak("tidyverse")
# pak::pak("tidymodels")
# pak::pak("cpp11") # required for readxl?
# pak::pak("progress") # required for readxl?
# pak::pak("finetune")
# pak::pak("xgboost")
# pak::pak("ranger")
# pak::pak("lme4")
# pak::pak("glmnet")
# pak::pak("RcppArmadillo")
# pak::pak("RcppEigen")
# pak::pak("themis")
# pak::pak("lorax")
# pak::pak("sessioninfo")

## access the environment variable ----
aed_file <- Sys.getenv("aed_path")

## load data ----
aed_final <- readxl::read_excel(aed_file)
