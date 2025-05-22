#' Create Confusion Matrix
#'
#' This function creates a confusion matrix for your model
#'
#' @param pred_tbl A tibble with predicted values of your model
#' @return A list
#' @export
models_logistic_create_confusion_matrix <- function (pred_tbl)
{
  pred_tbl <- pred_tbl %>% mutate(across(everything(), as.factor))
  confusion_matrix <- pred_tbl %>% yardstick::conf_mat(truth = 1,
                                                       estimate = 2)
  my_metrics <- metric_set(yardstick::sens, yardstick::spec,
                           yardstick::ppv, yardstick::npv, yardstick::accuracy,
                           yardstick::f_meas)
  results <- my_metrics(data = pred_tbl, truth = 1, estimate = 2)
  return(list(conf_mat = confusion_matrix, metrics = results))
}

#' Create Summary Datatable
#'
#' This function creates a summary datatable for your model
#'
#' @param model_fit Your model fit object
#' @param round how many decimal places to found values in the datatable
#' @param type default is "Summary"
#' @param title_chr default is NULL
#' @param ... other args taken in by the `DT::datatable` function
#' @return A datatable object
#' @export
models_logistic_create_summary_datatable <- function (model_fit, round = 3, type = "Summary", title_chr = NULL,
                                                      ...)
{
  if (!type %in% c("Summary", "Anova")) {
    stop("Argument `type` must equal 'Summary' or 'Anova'.")
  }
  options(scipen = round)
  model_fit %>% broom::tidy() %>% mutate(across(where(is.numeric),
                                                ~round(.x, round)), significance = case_when(p.value <
                                                                                               0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~
                                                                                               "*", p.value < 0.1 ~ ".", TRUE ~ ""), across(p.value,
                                                                                                                                            ~ifelse(.x < 1/(10^round), str_glue("<{1/(10^round)}"),
                                                                                                                                                    as.character(.x)))) %>% DT::datatable(caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; color:black; font-size:200% ;",
                                                                                                                                                                                                                            str_glue("{ifelse(is.null(title_chr), '', paste0(title_chr, ' '))}{ifelse(type == 'Summary', 'Model', 'ANOVA')} Summary")),
                                                                                                                                                                                          options = list(pageLength = nrow(.), ...))
}

#' Modify Column Names of a Tibble
#'
#' This function is reused to rename columns in your output datatables to create more presentatble tables
#' This function is used internally and does not need to be called explicitly by the user
#'
#' @param model_obj_tbl Your model object tibble
#' @param ... other args
#' @return
models_general_modify_tibble_colnames <- function (model_obj_tbl, ...)
{
  quos_args <- enquos(...)
  valid_renaming_args <- tibble(quo_names = names(quos_args),
                                quo_vals = sapply(quos_args, rlang::as_label)) %>% filter(str_detect(quo_vals,
                                                                                                     paste0(names(model_obj_tbl), collapse = "|")))
  if (nrow(valid_renaming_args) > 0) {
    model_obj_tbl <- model_obj_tbl %>% rename(!!!quos_args[names(quos_args) %in%
                                                             valid_renaming_args$quo_names])
  }
  remaining_args <- quos_args[!names(quos_args) %in% valid_renaming_args$quo_names]
  return(list(quos_args = quos_args, valid_renaming_args = valid_renaming_args,
              model_obj_tbl = model_obj_tbl, remaining_args = remaining_args))
}

#' Create General Datatable
#'
#' This function creates a datatable for other aspects of your modeling process, such as capturing best metrics
#'
#' @param model_obj_tbl Your model object tibble
#' @param datatable_title_chr What to title the datatable
#' @param round how many decimal places to found values in the datatable
#' @param ... other args taken in by the `models_general_modify_tibble_colnames` function
#' @return A datatable object
#' @export
models_general_create_datatable <- function (model_obj_tbl, datatable_title_chr, round = 3, ...)
{
  models_modifications_list <- models_general_modify_tibble_colnames(model_obj_tbl,
                                                                     ...)
  models_modifications_list$model_obj_tbl %>% mutate(across(where(is.numeric),
                                                            ~round(.x, round))) %>% DT::datatable(caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; color:black; font-size:200%;",
                                                                                                                                    str_glue("{datatable_title_chr}")), options = list(pageLength = nrow(.),
                                                                                                                                                                                       models_modifications_list$remaining_args))
}

#' Evaluate Tuning Results
#'
#' This function processes a tuning object to extract the best model parameters and generate evaluation plots and tables.
#'
#' @param tuning_obj A tibble with a `TUNE_RESULTS` column containing `tune::tune_results` objects
#'
#' @return A tibble with added columns:
#' \describe{
#'   \item{TUNING_METRICS}{All evaluation metrics}
#'   \item{BEST_OF_METRICS}{Best results per metric}
#'   \item{BEST_OF_METRICS_TABLE}{Datatable summarizing best metrics}
#'   \item{BEST_OF_METRICS_PLOT}{Faceted ggplot for best metrics}
#'   \item{BEST_OF_METRICS_PLOTLY}{Interactive Plotly version of the plot}
#' }
#' @export
tuning_eval <- function(tuning_obj) {

  tuning_obj %>%

    mutate(
      # *** Collect metrics from tuning ----
      TUNING_METRICS  = map(
        TUNE_RESULTS, workflowsets::collect_metrics
      ),
      # *** Filter to the best value of under-sample ratio for each metric ----
      BEST_OF_METRICS = pmap(
        list(TUNING_METRICS, TUNE_RESULTS),
        function(.tuning_metrics, .tune_results) {

          # Extract the unique metrics
          metric_list <- unique(.tuning_metrics$.metric)

          # Create a tibble that stores the best result for each metric
          best_results_tibble <- tibble(
            .metric = metric_list,
            best_result = map(metric_list, function(.y) {
              select_best(.tune_results, metric = .y)
            })
          ) %>%
            unnest(everything())

          # Return the tibble
          best_results_tibble
        }
      ),
      BEST_OF_METRICS_TABLE = map(
        BEST_OF_METRICS,
        ~models_general_create_datatable(
          .x, "Best Tuning Parameters", round = 5, `Under Ratio` = "under_ratio", Metric = ".metric", Config = ".config"
        )
      ) ,
      BEST_OF_METRICS_PLOT  = map(
        TUNING_METRICS, ~.x %>%
          mutate(
            .metric = case_when(
              .metric == "accuracy"  ~ "Accuracy",
              .metric == "f_meas"    ~ "F-Measure",
              .metric == "precision" ~ "Precision",
              .metric == "recall"    ~ "Recall",
              .metric == "roc_auc"   ~ "ROC Area Under Curve"
            ),
            across(mean, as.numeric),
            .tooltip = str_glue(
              "Under-sample Ratio: {under_ratio}
            {.metric}: {round(mean, 2)}"
            )
          ) %>%
          ggplot(aes(x = under_ratio, y = mean, group = .metric, color = .metric, text = .tooltip)) +
          geom_point() +
          geom_line() +
          facet_wrap(~ .metric, scales = "free_y") +
          labs(
            x = "Under-sampling Ratio",
            y = "Metric Value",
            title = ""
          ) +
          tidyquant::theme_tq() +
          scale_color_manual(values = c("#009CDE", "#8DC8E8", "#40A829", "#97C800", "#666666")) +
          theme(legend.position = "none") +
          theme(strip.text = element_text(size = 10))
      ),
      BEST_OF_METRICS_PLOTLY = map(BEST_OF_METRICS_PLOT, ~ggplotly(.x, tooltip = ".tooltip")),
      TUNING_METRICS         = map(
        TUNING_METRICS, ~models_general_create_datatable(
          .x, "All Tuning Parameters", round = 2,
          `Under Ratio`    = "under_ratio",
          Metric           = ".metric",
          Estimator        = ".estimator",
          Mean             = "mean",
          `Standard Error` = "std_err",
          Config           = ".config"
        )
      )

    )


}

#' Generate Significance Stars
#'
#' Converts a numeric p-value or test statistic into significance stars for display.
#'
#' @param statistic A numeric value or vector of test statistics (typically p-values)
#'
#' @return A character vector of significance indicators (e.g., "***", "**", "*", ".", "")
#' @export
get_significance <- function(statistic) {

  tibble(
    statistic = statistic
  ) %>%
    mutate(
      str_append = case_when(
        statistic < 0.001 ~ "***",
        statistic < 0.01  ~ "**",
        statistic < 0.05  ~ "*",
        statistic < 0.1   ~ ".",
        TRUE              ~ ""
      )
    ) %>%
    pull(str_append)

}

#' Create Feature Importance Plot
#'
#' Generates interactive feature importance plots from a model object tibble containing model fit summaries.
#'
#' @param model_obj_tbl A tibble with a column `EXTRACT_FIT_MODEL` containing model objects (e.g., `glm`) and a `DEP_VAR` column
#'
#' @return A tibble with a new column `FEATURE_IMPORTANCE` containing interactive Plotly objects showing coefficient-based feature importance
#' @export
feature_importance <- function(model_obj_tbl) {

  model_obj_tbl %>%

    mutate(

      FEATURE_IMPORTANCE = map(
        EXTRACT_FIT_MODEL, ~{
          summary_tbl <- summary(.x)$coefficients
          summary_tbl <- summary_tbl %>% .[rownames(.) != "(Intercept)",]
          tibble(
            Feature            = rownames(summary_tbl),
            Importance         = as.vector(summary_tbl[,"Estimate"]),
            Sign               = if_else(as.vector(summary_tbl[,"Estimate"]) > 0, "Positive", "Negative"),
            Significance       = get_significance(as.vector(summary_tbl[,"Pr(>|z|)"])),
            Importance_Abs_Val = abs(Importance)
          ) %>%
            mutate(
              Feature    = str_glue("{Feature} {Significance}"),
              .tooltip   = str_glue(
                "Factor: {Feature}
              Estimate: {round(Importance, 2)}
              Association: {Sign}
              "
              )
            )
        }
        #EXTRACT_FIT_MODEL, ~vip::vip(.x, geom = "point")
      ),
      FEATURE_IMPORTANCE = pmap(
        list(FEATURE_IMPORTANCE, DEP_VAR),
        function(importances, dep_var) {
          ggplot(importances, aes(x = Importance, y = reorder(Feature, Importance_Abs_Val), fill = Sign, text = .tooltip)) +
            geom_point() +
            labs(
              title    = "Feature Importance",
              subtitle = str_glue("The higher the score, the more important the feature is in predicting {dep_var}"),
              y        = ""
            ) +
            tidyquant::theme_tq() +
            tidyquant::scale_fill_tq() +  # Adjust as necessary to apply to 'fill' aesthetic based on 'Sign'
            tidyquant::scale_color_tq() +
            scale_fill_manual(values = c("Positive" = "#004C97", "Negative" = "#007A33")) + # Customize colors as needed
            geom_vline(xintercept = 0) #+
          #geom_vline(xintercept = 2, color = "#215732", linetype = "dotted", linewidth = 0.5) +
          #geom_vline(xintercept = -2, color = "#215732", linetype = "dotted", linewidth = 0.5)
        }
        # function(.x, .y) .x +
        #   labs(
        #     title    = "Feature Importance",
        #     subtitle = str_glue(
        #       "The higher the score, the more important the feature is in predicting {.y}"
        #     )
        #   ) +
        #   tidyquant::theme_tq() +
        #   tidyquant::scale_fill_tq() +
        #   tidyquant::scale_color_tq()
      ),
      FEATURE_IMPORTANCE = map(FEATURE_IMPORTANCE, ~ggplotly(.x, tooltip = ".tooltip"))

    )

}

#' Reconstruct Binning Labels
#'
#' This function adjusts bin labels in a model object tibble to make them more readable by converting "-Inf" and "Inf" boundaries into human-friendly phrases.
#'
#' @param model_obj_tbl A tibble with a `bin` column containing binning labels as character strings
#'
#' @return A tibble with the `bin` column modified to show more descriptive labels, such as "Less than X" or "Greater than Y"
#' @export
reconstruct_bin <- function(model_obj_tbl) {

  model_obj_tbl %>%
    mutate(
      bin = case_when(
        str_sub(bin, end = 4)    == "-Inf" ~ str_replace(bin, "^-Inf_", "Less than "),
        str_sub(bin, start = -3) == "Inf"  ~ str_replace(bin, "(.*)_Inf$", "Greater than \\1"),
        TRUE                               ~ bin
      )
    )

}
