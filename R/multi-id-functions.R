#' @title Asset Rank (Raw)
#'
#' @description In a table with multiple asset IDs,
#' returns the rank of the specified ID (subject) for the metric in the table.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm), asset_class (chr), and name (chr).
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

  pb <- list(metric_df = metric_df,
             key = key,
             subject = subject,
             name = name)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/asset-rank-raw",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Asset Rank (Pct)
#'
#' @description In a table with multiple asset IDs,
#' returns the percent rank of the specified ID (subject) for the metric in the table.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm), asset_class (chr), and name (chr).
#' @param subject The specific asset ID whose rank will be calculated,
#' and for whom the sentence will be about.
#'
#' @return A sentence denoting what rank in the asset class the specified asset ID
#' (aka subject) has in percent form; in other words, what percent of the asset class
#' had a lower value.
#'
#'@examples
#' \dontrun{
#'asset_rank_raw(metric_df)
#'}
#'
#' @export asset_rank_pct
#' @import tibble
#' @import httr
#' @import dplyr

asset_rank_pct <- function(metric_df, subject, metric_name = NULL) {
  key <- Sys.getenv('table_to_text')
  name <- ifelse(is.null(metric_name),metric_df$name,metric_name)
  if(is.null(name)) {
    stop('The name of the metric is needed. Provide this by adding a metric_name
         argument to the function, or by adding a column called "name" to the
         dataframe passed to the function.')
  }

  pb <- list(metric_df = metric_df,
             key = key,
             subject = subject,
             name = metric_name)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/asset-rank-pct",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
