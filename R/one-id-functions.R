#' @title Two Sigma Event
#'
#' @description Checks if a two sigma change has occurred. If so, returns a sentence,
#' if not, returns an empty character string.
#'
#' @title Two Sigma Change
#' @description Was there a two sigma change in the most recent event?
#' @param metric_df A dataframe requiring the following columns: asset_id,
#' time, and value.
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
#' @title Two Sigma Change
#' @description Was there a two sigma change in the most recent event?
#' @param metric_df
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

describe_trend <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-dev-mrwwgrktvq-ue.a.run.app/describe-trend",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
