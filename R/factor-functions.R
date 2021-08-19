#' @title Factor and Metric
#'
#' @description Report a factor and a metric.
#'
#' @param metric_df A dataframe
#' @param template_id Optional. A character string.
#'
#' @return A sentence ready for insertion into
#' an HTML document.
#'
#'@examples
#' \dontrun{
#'report_factor_and_metric(metric_df)
#'}
#'
#' @export report_factor_and_metric
#' @import tibble
#' @import httr
#' @import dplyr

report_factor_and_metric <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key,
             template_id = ifelse(is.null(template_id),'default',template_id))

  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-factor-and-metric",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Most Common Factor
#'
#' @description Report which factors (categorical variables) are the
#' most common.
#'
#' @param metric_df A dataframe
#' @param template_id Optional. A character string.
#'
#' @return A sentence ready for insertion into
#' an HTML document.
#'
#'@examples
#' \dontrun{
#'report_most_common_factor(metric_df)
#'}
#'
#' @export report_most_common_factor
#' @import tibble
#' @import httr
#' @import dplyr

report_most_common_factor <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key,
             template_id = ifelse(is.null(template_id),'default',template_id))

  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-most-common-factors",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Least Common Factor
#'
#' @description Report which factors (categorical variables) are the
#' least common.
#'
#' @param metric_df A dataframe
#' @param template_id Optional. A character string.
#'
#' @return A sentence ready for insertion into
#' an HTML document.
#'
#'@examples
#' \dontrun{
#'report_least_common_factor(metric_df)
#'}
#'
#' @export report_least_common_factor
#' @import tibble
#' @import httr
#' @import dplyr

report_least_common_factor <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  if(is.null(template_id)) {
    pb <- list(metric_df = metric_df,
               key = key)
  }
  if(!is.null(template_id)) {
    pb <- list(metric_df = metric_df,
               key = key,
               template_id = template_id)
  }

  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-least-common-factors",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Average by Common Factors
#'
#' @description Report average value of factors
#'  (categorical variables) that are common.
#'
#' @param metric_df A dataframe
#' @param template_id Optional. A character string.
#'
#' @return A sentence ready for insertion into
#' an HTML document.
#'
#'@examples
#' \dontrun{
#'report_avg_by_common_factors(metric_df)
#'}
#'
#' @export report_avg_by_common_factors
#' @import tibble
#' @import httr
#' @import dplyr

report_avg_by_common_factors <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  if(is.null(template_id)) {
    pb <- list(metric_df = metric_df,
               key = key)
  }
  if(!is.null(template_id)) {
    pb <- list(metric_df = metric_df,
               key = key,
               template_id = template_id)
  }

  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-avg-by-common-factors",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Average by Rare Factors
#'
#' @description Report average value of factors
#'  (categorical variables) that are rare.
#'
#' @param metric_df A dataframe
#' @param template_id Optional. A character string.
#'
#' @return A sentence ready for insertion into
#' an HTML document.
#'
#'@examples
#' \dontrun{
#'report_avg_by_rare_factors(metric_df)
#'}
#'
#' @export report_avg_by_rare_factors
#' @import tibble
#' @import httr
#' @import dplyr

report_avg_by_rare_factors <- function(metric_df,template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  if(is.null(template_id)) {
    pb <- list(metric_df = metric_df,
               key = key)
  }
  if(!is.null(template_id)) {
    pb <- list(metric_df = metric_df,
               key = key,
               template_id = template_id)
  }

  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-avg-by-rare-factors",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

