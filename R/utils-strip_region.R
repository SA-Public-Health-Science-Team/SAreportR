#' strip_region
#'
#' @param tbl regional data table
#'
#' @return
#' @export
#'
#' @examples
strip_region <- function(tbl){
    tbl |>
        mutate(fema_region = str_remove(fema_region, "Region ") |> as.numeric())
}
