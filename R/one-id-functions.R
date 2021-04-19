#' @title Two Sigma Change
#' @description Was there a two sigma change in the most recent event?
#' @param metric_df A dataframe requiring the following columns: asset_id,
#' name, time, and value.
#'
#' @return A sentence if a two sigma event has occurred; an empty
#' character string if not.
#'
#'@examples
#' \dontrun{
#'two_sigma_sentence(metric_df)
#'}
#'
#' @export two_sigma_sentence
#' @import tibble
#' @import httr
#' @import dplyr

two_sigma_sentence <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/two-sigma-change",
             body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Describe Trend
#'
#' @description Describes the trend in the data set over time.
#' if not, returns an empty character string.
#'
#' @param metric_df A dataframe requiring the following columns:
#' asset_id, name, time, and value.
#'
#' @return A sentence describing the trend over time of the metric
#' in the table.
#'
#'@examples
#' \dontrun{
#'two_sigma_sentence(metric_df)
#'}
#'
#' @export describe_trend
#' @import tibble
#' @import httr
#' @import dplyr

describe_trend <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/describe-trend",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Metric
#'
#' @description Reports the latest value of a given metric.
#'
#' @param metric_df A dataframe requiring the following columns:
#' asset_id, name, time, and value.
#'
#' @return A sentence reporting the latest value of a given metric.
#'
#'@examples
#' \dontrun{
#'report_metric(metric_df)
#'}
#'
#' @export report_metric
#' @import tibble
#' @import httr
#' @import dplyr

report_metric <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-metric",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Metric
#'
#' @description Reports the average value of a given metric.
#'
#' @param metric_df A dataframe requiring the following columns:
#' asset_id, name, time, and value.
#'
#' @return A sentence reporting the average value of a given metric
#' in the submitted dataframe.
#'
#'@examples
#' \dontrun{
#'report_average(metric_df)
#'}
#'
#' @export report_average
#' @import tibble
#' @import httr
#' @import dplyr

report_average <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-average",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Change Over Time
#'
#' @description Reports the change in metric from the most recent record
#' relative to the earliest record in the table submitted.
#'
#' @param metric_df A dataframe requiring the following columns:
#' asset_id, name, time, and value.
#'
#' @return A sentence reporting the difference between the value of the
#' most recent record and that of the earliest record.
#'
#'@examples
#' \dontrun{
#'change_over_time_random(metric_df)
#'}
#'
#' @export change_over_time
#' @import tibble
#' @import httr
#' @import dplyr

change_over_time <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/change-over-time",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Change Over Time Random
#'
#' @description Reports the change in metric from the most recent record
#' relative to a randomly selected earlier record in the table submitted.
#'
#' @param metric_df A dataframe requiring the following columns:
#' asset_id, name, time, and value.
#'
#' @return A sentence reporting the difference between the value of the
#' most recent record and that of the randomly selected earlier record.
#'
#'@examples
#' \dontrun{
#'change_over_time_random(metric_df)
#'}
#'
#' @export change_over_time_random
#' @import tibble
#' @import httr
#' @import dplyr

change_over_time_random <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/change-over-time-random",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Change Over Time Random Low Good
#'
#' @description Reports the change in metric from the most recent record
#' relative to a randomly selected earlier record in the table submitted,
#' when it is preferred that the value be lower.
#'
#' @param metric_df A dataframe requiring the following columns:
#' asset_id, name, time, and value.
#'
#' @return A sentence reporting the difference between the value of the
#' most recent record and that of the randomly selected earlier record,
#' when a low value is seen as more desirable.
#'
#'@examples
#' \dontrun{
#'change_over_time_random_low_good(metric_df)
#'}
#'
#' @export change_over_time_random_low_good
#' @import tibble
#' @import httr
#' @import dplyr

change_over_time_random_low_good <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/change-over-time-random-low-good",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
