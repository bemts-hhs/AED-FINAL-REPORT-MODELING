# Modeling Arm: Iowa First Responder AED Initiative (2021–2026)

## Overview
This modeling arm supports the 2021–2026 Iowa First Responder AED (FRAED)
Initiative led by the Bureau of Emergency Medical and Trauma Services (BEMTS).
The broader project evaluates automated external defibrillator (AED) deployments
documented in Iowa law enforcement records and integrates corresponding
ImageTrend Elite EMS data.

The modeling workflow presented here operationalizes core analytic tasks, including:

* Preparing and aligning AED deployment data.
* Constructing a feature‐engineering recipe for regression modeling.
* Fitting a logistic regression model to estimate adjusted odds of survival.
* Computing baseline survival probabilities.
* Generating scenario probabilities based on odds ratios.
* Producing a forest plot to visually summarize adjusted effects.

This work contributes to the program’s goal of assessing AED utilization by law enforcement officers (LEOs) and supporting evidence‑based improvements in out‑of‑hospital cardiac arrest response.

## Environment Setup
The project uses tidyverse, tidymodels, and supporting packages installed via pak.
An environment variable (aed_path) provides the encrypted path to the AED analytic file.
Session details are exported to ./output/R_session_info.csv for reproducibility.

## Data Preparation
Key steps include:

* Enforcing consistent factor levels for nominal predictors (sex, call_type, urbanicity, survival).
* Filtering records with missing sex data.
* Aligning categorical fields to ensure identical levels across training and prediction stages.

## Modeling Workflow
A logistic regression model (parsnip::logistic_reg(engine = "glm")) predicts survival (Survived vs. Deceased) using predictors such as:

* Age
* Sex
* Witness status
* Bystander CPR
* Urbanicity
* Call type
* AED shocks
* Time intervals from call to patient contact and AED use
* Imputation steps manage both nominal and numeric missingness.

## Baseline Scenario
A baseline individual is created using modal or median values of key predictors.
This baseline is used to:

* Compute baseline survival probability.
* Derive scenario survival probabilities using adjusted odds ratios.

## Outputs
The modeling arm generates:

* A cleaned table of adjusted odds ratios with 95% CIs and scenario probabilities.
* A forest plot saved to: `./output/aed_or_forest_plot.png`

## Purpose of This Modeling Arm of the Report
This section of the AED project provides quantitative insight into survival after LEO AED deployment. The modeling results support interpretation of operational performance, identification of factors associated with survival, and development of evidence‑based recommendations for Iowa’s emergency response system following AED placement across law enforcement agencies.