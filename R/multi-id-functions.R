#' @title Two Sigma Event
#'
#' @description Checks if a two sigma change has occurred. If so, returns a sentence,
#' if not, returns an empty character string.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm), and name (chr).
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
  response <- httr::POST("https://generate-text-dev-mrwwgrktvq-ue.a.run.app/two-sigma-change",
             body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Asset Rank (Raw)
#'
#' @description In a table with multiple asset IDs
#' if not, returns an empty character string.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm), and name (chr).
#' @param subject The specific asset ID whose rank will be calculated,
#' and for whom the sentence will be about.
#'
#' @return A sentence denoting what rank in the asset class the specified asset ID
#' (aka subject) has.
#'
#'@examples
#' \dontrun{
#'asset_rank_raw(metric_df)
#'}
#'
#' @export asset_rank_raw
#' @import tibble
#' @import httr
#' @import dplyr

asset_rank_raw <- function(metric_df, subject, metric_name = NULL) {
  key <- Sys.getenv('table_to_text')
  name <- ifelse(is.null(metric_name),metric_df$name,metric_name)
  if(is.null(name)) {
    stop('The name of the metric is needed. Provide this by adding a metric_name
         argument to the function, or by adding a column called "name" to the
         dataframe passed to the function.')
  }

  pb <- list(metric_df = metric_df[1:10,],
             key = key,
             subject = subject,
             name = name)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-dev-mrwwgrktvq-ue.a.run.app/asset-rank-raw",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
