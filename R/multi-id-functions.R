#' @title Report Highest Performer
#'
#' @description In a table with multiple asset IDs,
#' reports the asset ID (subject) with the highest value.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm), asset_class (chr), and name (chr).
#' @param template_id Optional. The template to be used for the article.
#'
#' @return A sentence denoting the highest performer out of a set of
#' given asset IDs.
#'
#'@examples
#' \dontrun{
#'report_highest_performer(metric_df)
#'}
#'
#' @export report_highest_performer
#' @import tibble
#' @import httr
#' @import dplyr

report_highest_performer <- function(metric_df, template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key,
             template_id = template_id)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-highest-performer",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Report Lowest Performer
#'
#' @description In a table with multiple asset IDs,
#' reports the asset ID (subject) with the highest value.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm), asset_class (chr), and name (chr).
#' @param template_id Optional. The template to be used for the article.
#'
#' @return A sentence denoting the lowest performer out of a set of
#' given asset IDs.
#'
#'@examples
#' \dontrun{
#'report_lowest_performer(metric_df)
#'}
#'
#' @export report_lowest_performer
#' @import tibble
#' @import httr
#' @import dplyr

report_lowest_performer <- function(metric_df, template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key,
             template_id = template_id)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/report-lowest-performer",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @title Most Correlated
#'
#' @description In a table with multiple asset IDs,
#' reports the asset ID most correlated to the subject asset_Id ID.
#'
#' @param metric_df A dataframe with the following columns: asset_id (chr),
#' value (dbl), time (date or dttm)
#' @param template_id Optional. The template to be used for the article.
#' @param subject The asset ID to measure correlations for.
#'
#' @return A sentence denoting the asset ID most correlated to the
#' declared subject.
#'
#'@examples
#' \dontrun{
#'most_correlated(metric_df)
#'}
#'
#' @export most_correlated
#' @import tibble
#' @import httr
#' @import dplyr

most_correlated <- function(metric_df, subject,template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key,
             subject = subject,
             template_id = template_id)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/most-correlated",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}


#' @export least_correlated
#' @import tibble
#' @import httr
#' @import dplyr

least_correlated <- function(metric_df, subject,template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key,
             subject = subject,
             template_id = ifelse(is.null(template_id),'default',template_id))
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/least-correlated",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}

#' @export most_correlated3
#' @import tibble
#' @import httr
#' @import dplyr

most_correlated3 <- function(metric_df, subject,template_id = NULL) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key,
             subject = subject,
             template_id = ifelse(is.null(template_id),'default',template_id))
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/most-correlated3",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}


############### LEGACY


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
