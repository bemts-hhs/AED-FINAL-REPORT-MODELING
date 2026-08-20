###___________________________________________________________________________
# Some data manipulation prior to pre-processing ----
###___________________________________________________________________________

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
# Train/test split ----
###___________________________________________________________________________

## set the random number seed ----
set.seed(2026)

## split ----
aed_split <- rsample::initial_split(
  aed_final_aligned,
  prop = 0.7,
  strata = survival
)

aed_train <- rsample::training(aed_split)
aed_test <- rsample::testing(aed_split)

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
  data = aed_train
) |>
  recipes::update_role(unique_incident_id, new_role = "ID") |>
  recipes::step_string2factor(recipes::all_nominal_predictors()) |>
  recipes::step_unknown(recipes::all_nominal_predictors()) |>
  recipes::step_other(recipes::all_nominal_predictors(), threshold = 0.05) |>

  # DUMMY ENCODE BEFORE INTERACTIONS
  recipes::step_dummy(recipes::all_nominal_predictors()) |>

  # INTERACTIONS ON NUMERIC DUMMIES
  recipes::step_interact(
    ~ matches("witnessed_"):matches("bystander_cpr_") +
      matches("witnessed_"):shock_no_shock +
      matches("urbanicity_"):time_from_call_to_patient +
      matches("call_type_"):matches("witnessed_")
  ) |>

  recipes::step_zv(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors()) |>
  recipes::step_impute_knn(recipes::all_predictors(), neighbors = 5) |>
  recipes::step_normalize(recipes::all_numeric_predictors())

## vfold cross validation on the training set ----
aed_cv <- rsample::vfold_cv(aed_train, v = 10, strata = survival)

## set up the metrics set for later ----
full_metric_set <- yardstick::metric_set(
  yardstick::roc_auc,
  yardstick::accuracy,
  yardstick::recall,
  yardstick::sens,
  yardstick::spec,
  yardstick::precision,
  yardstick::f_meas,
  yardstick::bal_accuracy,
  yardstick::j_index
)

# no ROC AUC
metric_set_no_auc <- yardstick::metric_set(
  yardstick::accuracy,
  yardstick::sens,
  yardstick::spec,
  yardstick::precision,
  yardstick::f_meas,
  yardstick::bal_accuracy,
  yardstick::j_index
)

###___________________________________________________________________________
# 1. Random Forest (ranger)
###___________________________________________________________________________

## Discrete weighting ----
ranger_weights <- c(
  Deceased = 1,
  Survived = 5
)

## ranger model specification ----
rf_spec <- parsnip::rand_forest(
  mtry = tune::tune(),
  trees = tune::tune(),
  min_n = tune::tune()
) |>
  parsnip::set_engine(
    "ranger",
    importance = "impurity",
    class.weights = ranger_weights
  ) |>
  parsnip::set_mode("classification")

## ranger workflow ----
rf_wf <- workflows::workflow() |>
  workflows::add_recipe(aed_recipe) |>
  workflows::add_model(rf_spec)

## use a space filling grid search to tune ranger hyperparameters ----
rf_grid <- dials::grid_space_filling(
  dials::mtry(range = c(2, 20)),
  dials::trees(range = c(500, 2000)),
  dials::min_n(range = c(2, 20)),
  size = 20
)

## tuning workflow for ranger ----
rf_tune <- finetune::tune_race_anova(
  rf_wf,
  resamples = aed_cv,
  grid = rf_grid,
  metrics = full_metric_set,
  control = finetune::control_race(save_pred = TRUE)
)

## select the best hyperparameters for the final ranger model fit ----
rf_best <- tune::select_best(rf_tune, metric = "roc_auc")

## collect metrics for the ranger model ----
rf_tune |>
  tune::collect_predictions(parameters = rf_best) |>
  yardstick::roc_curve(survival, .pred_Survived) |>
  ggplot2::autoplot()

## finalize the ranger workflow ----
rf_fit_resample <- tune::finalize_workflow(rf_wf, rf_best) |>
  tune::fit_resamples(aed_cv)

## get the fit to generate predictions ----
rf_final <- tune::finalize_workflow(rf_wf, rf_best) |>
  tune::last_fit(aed_train)

rf_last_fit <- tune::finalize_workflow(rf_wf, rf_best) |>
  tune::last_fit(split = aed_split)

## Fit the model, then sweep thresholds ----

### thresholds ----
threshold_grid <- seq(0.05, 0.95, by = 0.01)

### iterate ----
perf_tbl <- purrr::map_df(
  threshold_grid,
  function(th) {
    preds <- rf_tune |>
      tune::collect_predictions(parameters = rf_best) |>
      dplyr::mutate(
        .pred_class_th = ifelse(.pred_Survived >= th, "Survived", "Deceased"),
        .pred_class_th = forcats::fct(
          .pred_class_th,
          levels = c("Deceased", "Survived")
        ),
        threshold = th
      )

    yardstick::metrics(
      preds,
      truth = survival,
      estimate = .pred_class_th
    ) |>
      dplyr::mutate(threshold = th)
  }
)

### find the optimal threshold ----
optimal_ranger_threshold <- perf_tbl |>
  dplyr::filter(.estimate == max(.estimate), .by = .metric)

### create classified predictions at your chosen threshold ----
rf_preds_th <- rf_tune |>
  tune::collect_predictions(parameters = rf_best) |>
  dplyr::mutate(
    .pred_class_th = ifelse(.pred_Survived >= 0.35, "Survived", "Deceased"),
    .pred_class_th = forcats::fct(
      .pred_class_th,
      levels = c("Deceased", "Survived")
    )
  )

### calculate metrics using the new threshold ----
rf_metrics_threshold <- metric_set_no_auc(
  rf_preds_th,
  truth = survival,
  estimate = .pred_class_th
)

###___________________________________________________________________________
# Logistic regression (glmnet) ----
###___________________________________________________________________________

## logistc regression glmnet model spec ----
log_spec <- parsnip::logistic_reg(
  penalty = tune(),
  mixture = tune()
) |>
  parsnip::set_engine("glmnet") |>
  parsnip::set_mode("classification")

## logistic regression workflow ----
log_wf <- workflows::workflow() |>
  workflows::add_recipe(aed_recipe) |>
  workflows::add_model(log_spec)

## set up space filling grid search for glmnet ----
log_grid <- dials::grid_space_filling(
  dials::penalty(),
  dials::mixture(),
  size = 20
)

## tune the hyperparameters using anova race ----
log_tune <- finetune::tune_race_anova(
  log_wf,
  resamples = aed_cv,
  grid = log_grid,
  metrics = full_metric_set
)

## get the best hyperparameter combinations for glmnet ----
log_best <- tune::select_best(log_tune, metric = "roc_auc")

## fit the final glmnet model ----
log_final <- tune::finalize_workflow(log_wf, log_best) |>
  parsnip::fit(aed_train)

###___________________________________________________________________________
# Gradient Boosting (xgboost) ----
###___________________________________________________________________________

## xgboost model spec ----
xgb_spec <- parsnip::boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune()
) |>
  parsnip::set_engine("xgboost") |>
  parsnip::set_mode("classification")

## xgboost workflow ----
xgb_wf <- workflows::workflow() |>
  workflows::add_recipe(aed_recipe) |>
  workflows::add_model(xgb_spec)

## grid search via max entropy ----
xgb_grid <- dials::grid_space_filling(
  dials::trees(),
  dials::learn_rate(),
  dials::tree_depth(),
  dials::min_n(),
  dials::loss_reduction(),
  size = 30
)

## tune the xgboost hyperparameters via race anova method ----
xgb_tune <- finetune::tune_race_anova(
  xgb_wf,
  resamples = aed_cv,
  grid = xgb_grid,
  metrics = full_metric_set
)

## pull the best hyperparameters for xgboost ----
xgb_best <- tune::select_best(xgb_tune, metric = "roc_auc")

## fit the xgboost model with tuned hyperparameters to the training data ----
xgb_final <- tune::finalize_workflow(xgb_wf, xgb_best) |>
  parsnip::fit(aed_train)

###___________________________________________________________________________
# Diagnostics for each model ----
###___________________________________________________________________________

## Predictions and performance ----

### ranger predictions ----
rf_preds <- rf_final |>
  predict(aed_test, type = "prob") |>
  dplyr::mutate(
    prediction = forcats::fct(dplyr::if_else(
      .pred_Survived >= 0.35,
      "Survived",
      "Deceased"
    )),
    .after = .pred_Survived
  ) |>
  dplyr::bind_cols(aed_test)

### glmnet predictions ----
log_preds <- log_final |>
  predict(aed_test, type = "prob") |>
  dplyr::mutate(
    prediction = forcats::fct(
      dplyr::if_else(
        .pred_Survived > 0.35,
        "Survived",
        "Deceased"
      ),
      levels = c("Deceased", "Survived")
    ),
    .after = .pred_Survived
  ) |>
  dplyr::bind_cols(aed_test)

### xgboost predictions ----
xgb_preds <- xgb_final |>
  predict(aed_test, type = "prob") |>
  dplyr::mutate(
    prediction = forcats::fct(dplyr::if_else(
      .pred_Survived > 0.35,
      "Survived",
      "Deceased"
    )),
    .after = .pred_Survived
  ) |>
  dplyr::bind_cols(aed_test)

## metrics for each model ----

### ranger ROC AUC ----
rf_results <- rf_preds |>
  metric_set_no_auc(truth = survival, estimate = prediction)

### glmnet ROC AUC ----
log_auc <- log_preds |>
  metric_set_no_auc(truth = survival, estimate = prediction)

### xgboost ROC AUC ----
xgb_auc <- xgb_preds |>
  metric_set_no_auc(truth = survival, estimate = prediction)

###___________________________________________________________________________
# Variable importance for best model ----
###___________________________________________________________________________
