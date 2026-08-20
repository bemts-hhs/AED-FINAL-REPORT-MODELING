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
    call_type,
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
  survival ~ age_years +
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

## get main effects ----
aed_effects <- broom::tidy(
  aed_fit,
  exponentiate = TRUE, # odds ratios
  conf.int = TRUE
) |>
  dplyr::mutate(sig = p.value < 0.05)

## baseline scenario ----
baseline_scenario <- baseline_scenario <- tibble::tibble(
  age_years = mean(aed_final_aligned$age_years, na.rm = TRUE),
  sex = names(sort(table(aed_final_aligned$sex), decreasing = TRUE))[1],
  witnessed = names(sort(
    table(aed_final_aligned$witnessed),
    decreasing = TRUE
  ))[1],
  bystander_cpr = names(sort(
    table(aed_final_aligned$bystander_cpr),
    decreasing = TRUE
  ))[1],
  call_type = names(sort(
    table(aed_final_aligned$call_type),
    decreasing = TRUE
  ))[1],
  urbanicity = names(sort(
    table(aed_final_aligned$urbanicity),
    decreasing = TRUE
  ))[1],
  shock_no_shock = median(aed_final_aligned$shock_no_shock, na.rm = TRUE),
  time_from_call_to_patient = median(
    aed_final_aligned$time_from_call_to_patient,
    na.rm = TRUE
  ),
  time_from_call_to_aed_on = median(
    aed_final_aligned$time_from_call_to_aed_on,
    na.rm = TRUE
  ),
  time_at_patient_to_end_aed = median(
    aed_final_aligned$time_at_patient_to_end_aed,
    na.rm = TRUE
  )
) |>
  dplyr::mutate(
    dplyr::across(witnessed:bystander_cpr, ~ as.logical(.)),
    dplyr::across(call_type:urbanicity, ~ factor(.))
  )

## get predicted probability for the baseline ----
# baseline is derived as the predicted probability of survival for an individual
# with all variables set to their baseline
baseline_prob <- predict(
  aed_fit,
  new_data = baseline_scenario,
  type = "prob"
)$.pred_Survived

## define a function to compute scenario probabilities using the odds ratio
#' Compute scenario probability using OR relative to baseline
#' baseline_prob - numeric baseline probability
#' OR - odds ratio for variable
scenario_probability <- function(baseline_prob, OR) {
  baseline_odds <- baseline_prob / (1 - baseline_prob)
  new_odds <- baseline_odds * OR
  new_prob <- new_odds / (1 + new_odds)
  return(new_prob)
}

## Build explanatory OR table with % change in odds + scenario probabilities ----
aed_effects_pct <- aed_effects |>
  dplyr::mutate(
    pct_change = (estimate - 1) * 100,
    pct_low = (conf.low - 1) * 100,
    pct_high = (conf.high - 1) * 100,
    prob_baseline = baseline_prob,
    prob_scenario = scenario_probability(baseline_prob, estimate),
    prob_lower = scenario_probability(baseline_prob, conf.low),
    prob_high = scenario_probability(baseline_prob, conf.high)
  )
