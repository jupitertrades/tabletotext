#' @title Visualize a single metric
#'
#' @description Show a metric plotted over time as a line chart.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' time (date or dttm), value (dbl), name (chr)
#'
#' @return A link to an image showing a visualization of the metric included.
#'
#'@examples
#' \dontrun{
#'return_3news_snippets(metric_df)
#'}
#'
#' @export one_metric_over_time
#' @import tibble
#' @import httr
#' @import dplyr

one_metric_over_time <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/one-metric-over-time",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Visualize a single metric (story format)
#'
#' @description Show a metric plotted over time as a line chart, in a format
#' friendly for Google Web Stories.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' time (date or dttm), value (dbl), name (chr)
#'
#' @return A link to an image showing a visualization of the metric included.
#'
#'@examples
#' \dontrun{
#'return_3news_snippets(metric_df)
#'}
#'
#' @export one_metric_over_time_story
#' @import tibble
#' @import httr
#' @import dplyr

one_metric_over_time_story <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/one-metric-over-time-story",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Get Price Chart
#'
#' @description Return a candlestick chart for a given asset, with the
#' specified theme.
#'
#' @param symbol (chr) A ticker for a financial asset.
#' @param theme (chr) The theme that should be applied to the candlestick chart.
#'
#' @return A link to an image showing a visualization of the metric included.
#'
#'@examples
#' \dontrun{
#'get_price_chart('amzn','stocknews')
#'}
#'
#'
#' @export get_price_chart
#' @import tibble
#' @import httr
#' @import dplyr

get_price_chart <- function(symbol,theme) {
  response <- httr::GET(glue::glue('https://generate-text-mrwwgrktvq-ue.a.run.app/get-price-chart?symbol={symbol}&theme={theme}')) %>%
    httr::content() %>% as.character()
  return(response)
}
