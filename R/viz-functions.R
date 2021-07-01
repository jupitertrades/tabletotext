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
