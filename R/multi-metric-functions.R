#' @title Latest Best Rank
#'
#' @description Out of a series of metrics, which one does the given asset_id
#' rank best in?
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm), and name (chr). NOTE: value should be a
#' percentile rank between 0 and 100.
#'
#' @return A sentence denoting which metric the given asset_id has the highest
#' rank for within its asset class.
#'
#'@examples
#' \dontrun{
#'latest_best_rank(metric_df)
#'}
#'
#' @export latest_best_rank
#' @import tibble
#' @import httr
#' @import dplyr

latest_best_rank <- function(metric_df) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/latest-best-rank",
             body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Strongest Trend
#'
#' @description Out of a series of metrics,
#' which one does the given asset_id
#' show the clearest trend in over time?
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' asset_class(chr), value (dbl), time (date or dttm), and name (chr).
#'
#' @return A sentence denoting which metric in the table the asset id
#' in question exhibited the strongest trend in over time.
#'
#'@examples
#' \dontrun{
#'strongest_trend(metric_df)
#'}
#'
#' @export strongest_trend
#' @import tibble
#' @import httr
#' @import dplyr

strongest_trend <- function(metric_df) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/strongest-trend",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}


#' @title Latest Lowest Rank
#'
#' @description Out of a series of ranked metrics,
#' which one does the given asset_id
#' report the lowest rank in?
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' asset_class (chr), value (dbl), time (date or dttm), and name (chr). NOTE: the value column
#' should reflect a percentage rank whose value is between 0 (worst rank)
#' and 100 (best rank).
#'
#' @return A sentence denoting which metric in the submitted dataframe
#' the given asset id had the lowest rank for.
#'
#'@examples
#' \dontrun{
#'latest_lowest_rank(metric_df)
#'}
#'
#' @export latest_lowest_rank
#' @import tibble
#' @import httr
#' @import dplyr

latest_lowest_rank <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/latest-lowest-rank",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Compare Two Metrics From Two Assets
#'
#' @description How do two metricsm, one from one asset and one from another, compare?
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' asset_id_compare (chr), value (dbl), time (date or dttm),
#' value_compare (dbl), and name (chr).
#'
#' @return A sentence denoting which metric in the submitted dataframe
#' the given asset id had the lowest rank for.
#'
#'@examples
#' \dontrun{
#'compare_two_metrics_latest(metric_df)
#'}
#'
#' @export compare_two_metrics_latest
#' @import tibble
#' @import httr
#' @import dplyr

compare_two_metrics_latest <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key,
             template_id = ifelse(is.null(template_id),'default',template_id))
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/compare-two-metrics-latest",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Compare Two Trends
#'
#' @description How do the timeseries trends amongst two metrics compare?
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' asset_id2 (chr),time (date or dttm),value (dbl),value2 (dbl)
#'
#' @return A sentence denoting the difference or similarity between the
#' trend of two metrics.
#'
#'@examples
#' \dontrun{
#'compare_two_metrics_trend(metric_df)
#'}
#'
#' @export compare_two_metrics_trend
#' @import tibble
#' @import httr
#' @import dplyr

compare_two_metrics_trend <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,key = key,
             template_id = ifelse(is.null(template_id),'default',template_id))
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/compare-two-metrics-trend",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Return Three Random Facts
#'
#' @description What are three random facts about the data presented?
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' time (date or dttm)
#'
#' @return A sentence denoting which metric in the submitted dataframe
#' the given asset id had the lowest rank for.
#'
#'@examples
#' \dontrun{
#'random_fact_3bullets(metric_df)
#'}
#'
#' @export random_fact_3bullets
#' @import tibble
#' @import httr
#' @import dplyr

random_fact_3bullets <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key,
             template_id = ifelse(is.null(template_id),'default',template_id))
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/random-fact-3bullets",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}


#' @title Report Three Random Metrics
#'
#' @description What are three random metrics from the data presented?
#'
#' @param metric_df A dataframe an asset_id column (chr), and an unspecified
#' number of numeric columns. The column names of the numeric columns will be
#' used in the generated text.
#'
#' @return Three sentences, each wrapped in HTML bullet tags (<li>), reporting
#' the value of three randomly selected numeric columns.
#'
#'@examples
#' \dontrun{
#'report_3metrics_random(metric_df)
#'}
#'
#' @export report_3metrics_random
#' @import tibble
#' @import httr
#' @import dplyr

report_3metrics_random <- function(metric_df) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-3metrics-random",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
