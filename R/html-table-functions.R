#' @title Transform a dataframe into an HTML table
#'
#' @description Turn a dataframe into an HTML formatted table.
#'
#' @param metric_df A dataframe
#'
#' @return An HTML formatted character string ready for insertion into
#' an HTML document.
#'
#'@examples
#' \dontrun{
#'make_html_table(metric_df)
#'}
#'
#' @export make_html_table
#' @import tibble
#' @import httr
#' @import dplyr

make_html_table <- function(metric_df) {
  key <- Sys.getenv('table_to_text')
  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  response <- httr::POST("https://generate-text-mrwwgrktvq-ue.a.run.app/make-html-table",
                         body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  return(response)
}
