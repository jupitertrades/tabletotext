#' @title Return 3 News Snippets
#'
#' @description In a table of news stories, return 3 based on recency
#' and length.
#'
#' @param metric_df A dataframe with the following columns: link (chr),
#' title (chr), time (date or dttm), author (chr), and description (chr).
#'
#' @return HTML formatted text with links and descriptions of articles.
#'
#'@examples
#' \dontrun{
#'return_3news_snippets(metric_df)
#'}
#'
#' @export return_3news_snippets
#' @import tibble
#' @import httr
#' @import dplyr

return_3news_snippets <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/return-3news-snippets",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
