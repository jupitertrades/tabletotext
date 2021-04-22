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

  pb <- list(metric_df,
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

latest_lowest_rank <- function(metric_df) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/latest-lowest-rank",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
