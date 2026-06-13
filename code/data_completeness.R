source("setup.R")
load(here::here("data", "dcounts_month.rda"))

dcounts_month |> 
  mutate(year = str_remove(month, "-.*") |> as.numeric() ) |> 
  filter(agency == "DOE_FERC") |> 
  count(year)

dir.create(here::here("data", "completeness"))

d <- dcounts_month

agencies <- dcounts_month$agency |> unique()

breaks <- d |> ungroup() |> distinct(month) |> filter(str_detect(month, "-01") ) |> pull(month)





d |> 
  mutate(year = str_remove(month, "-.*") |> as.numeric() ) |> 
  filter(year>2004) |> 
  group_by(month, agency) |> 
  summarise(n = sum(per_icpsr_chamber_month_agency_type)) |> 
  group_by(agency) |> 
  mutate(percent = percent_rank(n)) |> 
  ungroup() |> 
  ggplot() + 
  aes(x = month,
      y = agency, 
      fill = n) + 
  geom_tile(color = "white") +
  scale_x_discrete(breaks = breaks) +
  theme_minimal()  +
  #facet_grid(rows = "agency", scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90),
        panel.grid = element_blank())

ggsave(here::here("data", "completeness", "completeness.png"),
       height = 16, width = 12 )

d |> 
  mutate(year = str_remove(month, "-.*") |> as.numeric() ) |> 
  #filter(year>2004) |> 
  group_by(month, agency) |> 
  summarise(n = sum(per_icpsr_chamber_month_agency_type)) |> 
  group_by(agency) |> 
  mutate(percent = percent_rank(n),
         year = ifelse(str_detect(month, "-06"),
                       month %>% str_sub(3,4), 
                       NA)
         ) |> 
  ungroup() |> 
  ggplot() + 
  aes(x = month,
      y = agency, 
      fill = percent,
      label = year ) + 
  geom_tile(color = "white") +
  labs(title = "Relative density of data per agency",
       fill = "Density") + 
  geom_text(check_overlap = T, color = "grey") + 
  scale_x_discrete(breaks = breaks) +
  #geom_vline(xintercept = "2007-01", color = "red") + 
  #geom_vline(xintercept = "2020-01", color = "red") + 
  theme_minimal()  +
  #facet_grid(rows = "agency", scales = "free_y") + 
  theme(panel.grid =  
          #element_line(color = "black") ,
        element_blank(),
        axis.text.x = element_text(angle = 90),
        )

ggsave(here::here("data", "completeness", "completeness-percent.png"),
       height = 16, width = 12 )




# plots by agency
plot_completeness <- function(a){
  
  p <- d |> 
    filter(agency == a) |> 
    group_by(month, agency, TYPE) |> 
    summarise(n = sum(per_icpsr_chamber_month_agency_type)) |> 
    ggplot() + 
    aes(x = month,
        y = TYPE, 
        fill = n) + 
    labs(title = a) + 
    geom_tile(color = "white") +
    scale_x_discrete(breaks = breaks) +
    theme_minimal()  +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid = element_blank())
  
  ggsave(plot = p,
         here::here("data", "completeness", paste0(a, ".png")),
         height = 2.2, width = 10 )
}

walk(agencies, plot_completeness)
