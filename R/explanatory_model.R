###___________________________________________________________________________
# Some data manipulation prior to defining the regression equation ----
###___________________________________________________________________________

## set the random number seed ----
set.seed(2026)

## Identify all nominal predictors BEFORE recipe ----
nominal_vars <- aed_final |>
  dplyr::select(
    survival,
    sex,
    witnessed,
    bystander_cpr,
    cardiac_episode,
    od_case,
    other_case_binary,
    agency_type,
    urbanicity
  ) |>
  names()

## ensure applicable columns are factors ----
# this helps make sure training set and test set have all the same factor levels
aed_final_aligned <- aed_final |>
  dplyr::mutate(
    dplyr::across(
      all_of(nominal_vars),
      ~ forcats::as_factor(.)
    ),
    survival = forcats::fct_relevel(survival, c("Deceased", "Survived"))
  )


###___________________________________________________________________________
# Feature engineering recipe ----
###___________________________________________________________________________

## define the recipe specification for the model ----
aed_recipe <- recipes::recipe(
  survival ~ unique_incident_id +
    age_years +
    sex +
    witnessed +
    bystander_cpr +
    call_type +
    urbanicity +
    shock_no_shock +
    time_from_call_to_patient +
    time_from_call_to_aed_on +
    time_at_patient_to_end_aed,
  data = aed_final_aligned
) |>
  recipes::update_role(unique_incident_id, new_role = "ID") |>
  recipes::step_string2factor(recipes::all_nominal_predictors()) |>
  recipes::step_unknown(recipes::all_nominal_predictors()) |>
  recipes::step_other(recipes::all_nominal_predictors(), threshold = 0.05)

###___________________________________________________________________________
# Set up the model specification ----
###___________________________________________________________________________
aed_spec <- parsnip::logistic_reg() |>
  parsnip::set_engine("glm") |>
  parsnip::set_mode("classification")

###___________________________________________________________________________
# Workflow and fit ----
###___________________________________________________________________________

## workflow ----
aed_wf <- workflows::workflow() |>
  workflows::add_recipe(aed_recipe) |>
  workflows::add_model(aed_spec)

## fit ----
aed_fit <- aed_wf |>
  parsnip::fit(aed_final_aligned)

###___________________________________________________________________________
# Interpret coeffcients ----
###___________________________________________________________________________
aed_effects <- broom::tidy(
  aed_fit,
  exponentiate = TRUE, # odds ratios
  conf.int = TRUE
) |>
  dplyr::mutate(sig = p.value < 0.05)
