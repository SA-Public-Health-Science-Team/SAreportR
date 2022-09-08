#' plot_region_map
#'
#' @param region_number Region number
#'
#' @return map of US with region highlighted
#' @export
#'
#' @examples
plot_region_us_map <- function(region_number = 1){


    pop <- read_rds(here::here("data/pop.rds")) |> mutate(juris_name = str_to_lower(juris_name))


    state <- map_data('state') |>
        left_join(pop, by = c("region" = "juris_name")) |>
        mutate(fema_region_true = fema_region_number == region_number)


    region_colors <- read_rds(here::here("data/region_colors.rds"))

    region_color  <- region_colors[region_number]



    ggplot(data=state, aes(x=long, y=lat, fill=fema_region_true, group=group)) +
        geom_polygon(color = "white") +
        guides(fill='none') +
        scale_fill_manual(values = c( "#cccccc", region_color)) +
        theme(axis.line = element_blank(),
              axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
        coord_fixed(1.3)


}



plot_region_map <- function(region_number = 1){

    region_colors <- read_rds(here::here("data/region_colors.rds"))

    region_color  <- region_colors[region_number]

    pop <- read_rds(here::here("data/pop.rds"))

    state_in_region <- pop |>
        filter(fema_region_number == region_number) |>
        pull(juris)

    plot_usmap(include = state_in_region, fill = region_color)

}
