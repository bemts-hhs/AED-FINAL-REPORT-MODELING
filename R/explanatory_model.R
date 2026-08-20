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
  dplyr::filter(!is.na(sex)) |>
  dplyr::mutate(
    sex = forcats::fct(sex, levels = c("M", "F")),
    call_type = dplyr::coalesce(call_type, "Unknown"),
    call_type = forcats::fct(
      call_type,
      levels = c("Cardiac Arrest", "Overdose", "Other Cause", "Unknown")
    ),
    urbanicity = dplyr::coalesce(urbanicity, "Unknown"),
    urbanicity = forcats::fct(
      urbanicity,
      levels = c("Metropolitan", "Micropolitan", "Rural", "Unknown")
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
  recipes::step_impute_mode(recipes::all_nominal_predictors()) |>
  recipes::step_impute_mean(recipes::all_numeric_predictors())

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

### baseline probability ----
baseline_prob <- predict(
  aed_fit,
  new_data = baseline_scenario,
  type = "prob"
)$.pred_Survived

### baseline probabilities with 95% CI
## get predicted probability for the baseline ----
# baseline is derived as the predicted probability of survival for an individual
# with all variables set to their baseline
baseline_tbl <- marginaleffects::predictions(
  aed_fit,
  newdata = baseline_scenario,
  type = "prob"
) |>
  broom::tidy()

## define a function to compute scenario probabilities using the odds ratio ----
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
    pct_change = (estimate - 1),
    pct_low = (conf.low - 1),
    pct_high = (conf.high - 1),
    prob_baseline = baseline_prob,
    prob_scenario = scenario_probability(baseline_prob, estimate),
    prob_lower = scenario_probability(baseline_prob, conf.low),
    prob_high = scenario_probability(baseline_prob, conf.high)
  )

## vector to assist in renaming terms in the aed_effects_pct table ----
term_labels <- c(
  "(Intercept)" = "Baseline (Intercept)",
  "age_years" = "Age (years)",
  "sexF" = "Female",
  "witnessedTRUE" = "Witnessed arrest",
  "bystander_cprTRUE" = "Bystander CPR",
  "call_typeOverdose" = "Overdose call",
  "call_typeOther Cause" = "Other Cause call",
  "call_typeUnknown" = "Unknown call type",
  "urbanicityMicropolitan" = "Micropolitan",
  "urbanicityRural" = "Rural",
  "urbanicityUnknown" = "Unknown Urbanicity",
  "shock_no_shock" = "# shocks delivered",
  "time_from_call_to_patient" = "Time: Call to patient",
  "time_from_call_to_aed_on" = "Time: Call to AED on",
  "time_at_patient_to_end_aed" = "Time: AED duration"
)

## apply new labels to the effects table ----
aed_effects_pct_clean <- aed_effects_pct |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::select(term, estimate, p.value:prob_high) |>
  dplyr::mutate(
    term_clean = term_labels[term],
    term_clean = ifelse(sig, paste(term_clean, "[*]"), term_clean),
    term_clean = forcats::fct_reorder(term_clean, estimate),
    .before = term
  ) |>
  dplyr::select(-term) |>
  dplyr::rename(term = term_clean)

## create the forest plot ----
main_effects_plot <- ggplot2::ggplot(
  aed_effects_pct_clean,
  ggplot2::aes(x = estimate, y = term_clean)
) +
  ggplot2::geom_vline(
    xintercept = c(0.1, 1.0, 10.0),
    linetype = "dashed",
    color = "#B9E1DA"
  ) +
  ggplot2::geom_point(size = 3, color = "steelblue") +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = conf.low, xmax = conf.high),
    height = 0.25,
    color = "steelblue"
  ) +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    x = "Adjusted Odds Ratio (log10 scale)",
    y = NULL,
    title = "Adjusted Odds Ratios for Survival After AED Deployment",
    subtitle = "95% confidence intervals shown as horizontal bars",
    caption = "\n`*` indicates statistical significance at the 0.05 level."
  ) +
  ggplot2::theme_minimal(base_size = 16, base_family = "Work Sans") +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    text = ggplot2::element_text(
      family = "Work Sans",
      color = "#4D4D4F"
    ),
    plot.title = ggplot2::element_text(
      size = 18,
      color = "#19405B",
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(size = 16, color = "#F27026"),
    plot.caption = ggplot2::element_text(
      size = 12,
      hjust = 0,
      color = "#F27026",
      face = "bold"
    )
  )


## save the forest plot ----
ggplot2::ggsave(
  filename = "./output/aed_or_forest_plot.png",
  plot = main_effects_plot,
  width = 9.3,
  height = 5.5
)
