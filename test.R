library(usmap)
library(ggplot2)

region <- 1




state_in_region <- pop |>
    filter(fema_region == region) |>
    pull(juris)

plot_usmap(include = state_in_region) +
    geom_text(data = abbr)
    labs(title = glue::glue("Region {region}"))





library(ggplot2)
library(maps)
library(mapdata)

plot_region <- function(region_number = 1){


    pop <- read_rds(here::here("data/pop.rds")) |> mutate(juris_name = str_to_lower(juris_name))


    state <- map_data('state') |>
        left_join(pop, by = c("region" = "juris_name")) |>
        filter(fema_region == region_number)


    region_colors <- read_rds(here::here("data/region_colors.rds"))

    region_color  <- region_colors[region]



    ggplot(data=state, aes(x=long, y=lat, fill=fema_region, group=group)) +
        geom_polygon(color = "white") +
        guides(fill=FALSE) +
        scale_fill_manual(values = region_colors) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
        ggtitle('U.S. Map with States') +
        coord_fixed(1.3)


}

plot_region(2)
as.data.frame(state.name,state.abb)


pop_temp <- cbind(data.frame(state.name), data.frame(state.abb))




read_rds(here::here("data/pop.rds"))  |>
    mutate(fema_region = str_remove(fema_region, "Region ") |> as.numeric())|>
    write_rds(here::here("data/pop.rds"))
