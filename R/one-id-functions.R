#' @title Two Sigma Event
#'
#' @description Checks if a two sigma change has occurred. If so, returns a sentence,
#' if not, returns an empty character string.
#'
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

two_sigma_sentence <- function(metric_df) {
  key <- Sys.getenv('table_to_text')

  pb <- list(metric_df = metric_df,
             key = key)
  body_json <- paste0('{"post_body":',jsonlite::toJSON(pb), '}', sep = '')
  httr::POST("https://generate-text-dev-mrwwgrktvq-ue.a.run.app/two-sigma-change",
             body = body_json,httr::accept_json()) %>% httr::content() %>%
    as.character()
  response <- ch %>% httr::content()
  return(response)
}
