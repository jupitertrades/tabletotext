#' @title Report Big Moves in Class
#' @description Which asset IDs in a given asset class had a big move
#' since their prior recorded value?
#' @param metric_df A dataframe requiring the following columns: asset_id,
#' asset_class, name, time, value.
#'
#' @return A sentence denoting which, if any, asset IDs had large moves.
#'
#'@examples
#' \dontrun{
#'report_big_moves_in_class(metric_df)
#'}
#'
#' @export report_big_moves_in_class
#' @import tibble
#' @import httr
#' @import dplyr

report_big_moves_in_class <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-big-moves-in-class",
             body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Top 3 Movers in Class
#' @description Which 3 asset IDs in a given asset class had the largest
#' change, on a percentage basis, since their prior recorded value?
#' @param metric_df A dataframe requiring the following columns: asset_id,
#' asset_class, name, time, value.
#'
#' @return A sentence denoting up to 3 largest movers on a percentage basis
#' in a given asset class.
#'
#'@examples
#' \dontrun{
#'report_top3_movers_in_class(metric_df)
#'}
#'
#' @export report_top3_movers_in_class
#' @import tibble
#' @import httr
#' @import dplyr

report_top3_movers_in_class <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-top3-movers-in-class",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Rank
#' @description What is the rank of a given asset for a given metric for its
#' stated asset class?
#' @param metric_df A dataframe requiring the following columns: asset_id,
#' asset_class, name, value.
#' #' @param template_id The template to use. Optional.
#' @return A sentence denoting the rank of a given asset for the metric in question.
#'
#'@examples
#' \dontrun{
#'report_rank(metric_df)
#'}
#'
#' @export report_rank
#' @import tibble
#' @import httr
#' @import dplyr

report_rank <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key,template_id = ifelse(is.null(template_id),'default',template_id))
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-rank",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
