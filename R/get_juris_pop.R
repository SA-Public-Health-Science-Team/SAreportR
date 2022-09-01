#' get_jur_pop
#'
#' @return jurisdiction population table
#' @export
#'
#' @examples
get_jur_pop <- function(username){

    pop <- readr::read_csv(paste0(
        "C:/Users/",
        username,
        "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/Power BI/Data/2020 Census/2020_Census_by_Juris.csv"),
        show_col_types = FALSE) |>
        select(Juris, Population) |>
        rename(juris = Juris,
               pop   = Population)


    readr::write_rds(pop,here::here("data/pop.rds"))
}
