library(readxl)
library(tidyverse)
library(mapchina)
library(sf)
library(ggplot2)
library(gifski)
library(gganimate)
library(viridis)
library(ggthemes)

##------------------------------------------------------------------------------
## PREPARING THE DATA SET
##------------------------------------------------------------------------------

#reading the excel sheets containing demographic data
file_list <- list.files(pattern = "^xj_pop.*\\.xlsx$", full.names = TRUE)

for (file in file_list) {
  df_name <- gsub("\\.xlsx$", "", basename(file))
  
  assign(
    df_name,
    read_excel(file) %>%
      filter(!is.na(Code_County)) %>%  
      mutate(Other = Total - rowSums(select(., 5:10)))
  )
}

#creating a single data frame
df_list <- list(xj_pop_00, xj_pop_01, xj_pop_02, xj_pop_03, xj_pop_04, 
                xj_pop_05, xj_pop_06, xj_pop_07, xj_pop_08, xj_pop_09, 
                xj_pop_10, xj_pop_11, xj_pop_12, xj_pop_13, xj_pop_14, 
                xj_pop_15, xj_pop_16, xj_pop_17, xj_pop_18)

df_xj_pop_combined <- bind_rows(df_list)

summary(df_xj_pop_combined)

#demographic information seems to be erroneously reported in the sources for some county years. Values of the "Other" category smaller than 0 are logically impossible; values greater than 100,000 are practically impossible. Hence, these values are chosen as threshold for exclusion.
df_xj_pop_combined <- df_xj_pop_combined %>%
  mutate(across(4:11, ~ifelse(Other < 0 | Other > 100000, NA, .)))

summary(df_xj_pop_combined)

#calculating a measure of ethnic fractionalization: 1-HHI, i.e., higher values = MORE diverse
df_xj_fractionalization <- df_xj_pop_combined %>%
  rowwise() %>%
  mutate(
    Fractionalization = 1 - sum((c_across(5:11) / Total)^2, na.rm = FALSE)
  ) %>%
  ungroup()
  
#calculating a measure of ethnic polarization (Reynal-Querol 2002, Montalvo and Reynal-Querol 2005): higher values = LOWER polarization - 0.5 as reference for perfect polarization as two eqully sized groups
df_xj_polarization <- df_xj_pop_combined %>%
  rowwise() %>%
  mutate(
    Polarization = 1 - sum((((0.5 - (c_across(5:11) / Total)) / 0.5)^2) * (c_across(5:11) / Total), na.rm = FALSE)
  ) %>%
  ungroup()

#exporting the diversity data
df_xj_diversity_f <- df_xj_fractionalisation %>%
  select(c(1:3, 12))

df_xj_diversity_p <- df_xj_polarization %>%
  select(c(1, 3, 12))

df_xj_diversity <- df_xj_diversity_f %>%
  left_join(df_xj_diversity_p)

write.csv(df_xj_diversity, "EDXJ_1.0_complete.csv")  
  
##------------------------------------------------------------------------------
## VISUALIZATIONS
##------------------------------------------------------------------------------
  
#Population development
df_total <- df_xj_pop_combined %>%
  group_by(Year) %>%
  summarize(
    Uyghur = sum(Uyghur, na.rm = TRUE),  
    Han = sum(Han, na.rm = TRUE)
  )

df_long <- df_total %>%
  pivot_longer(cols = c(Uyghur, Han),
               names_to = "Ethnic Group",
               values_to = "Number")

summary(df_long)

df_long <- df_long %>%
  mutate(Number = na_if(Number, 0))

summary(df_long)

pop_dev_plot <- df_long %>%
  filter(!is.na(Number)) %>%
  ggplot(aes(x = Year, y = Number/100000, color = `Ethnic Group`, group = `Ethnic Group`)) +
  geom_line( ) + 
  geom_point(aes(shape = `Ethnic Group`),size = 2) +
  scale_color_manual(values = c("Uyghur" = "red", "Han" = "blue")) + 
  scale_shape_manual(values = c("Uyghur" = 16, "Han" = 17)) +
  scale_x_continuous(limits = c(min(df_long$Year), 2015)) +
  labs(title = "Population development in Xinjiang",
       x = "Year",
       y = "Number in 100,000s") +
  theme_clean() +
  theme(legend.position = "bottom")

ggsave("xj_pop_dev.png", plot = pop_dev_plot, width = 8, height = 5)

#shapefiles for counties in Xinjiang from the mapchina package
df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区") %>%
  select(c(Code_County, geometry)) %>%
  mutate(Code_County = as.factor(Code_County))

df_xj_diversity <- df_xj_diversity %>%
  mutate(Code_County = as.factor(Code_County))

map_data <- df_xj_diversity %>%
  left_join(df_china_sf) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year != 2010)

#animated map FRACTIONALIZATION
map_fractionalization_gif <- ggplot() +
  geom_sf(data = map_data, aes(fill = Fractionalisation, geometry = geometry)) +
  scico::scale_fill_scico(palette = "lajolla", na.value = "white") +
  labs(
    title = "Ethnic Fractionalization in Xinjiang, {closest_state}",
    fill = "Degree of Fractionalization",
    caption = "© Jan Schlebusch 2024"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 15, hjust = 0.1)
  ) +
  transition_states(
    states = Year,
    transition_length = 0,
    state_length = 0.5,
    wrap = FALSE
  ) +
  ease_aes('cubic-in-out')

map_fractionalization_animated <- animate(map_fractionalization_gif, 
                       renderer = gifski_renderer(), 
                       duration = 30, 
                       fps = 20,
                       width = 800, 
                       height = 800)

anim_save("xj_fractionalization_map.gif", animation = map_fractionalization_animated)

#animated map POLARIZATION
map_polarization_gif <- ggplot() +
  geom_sf(data = map_data, aes(fill = Polarization, geometry = geometry)) +
  scico::scale_fill_scico(palette = "devon", na.value = "white") +
  labs(
    title = "Ethnic Polarization in Xinjiang, {closest_state}",
    fill = "Degree of Polarization",
    caption = "© Jan Schlebusch 2024"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 15, hjust = 0.1)
  ) +
  transition_states(
    states = Year,
    transition_length = 0,
    state_length = 0.5,
    wrap = FALSE
  ) +
  ease_aes('cubic-in-out')

map_polarization_animated <- animate(map_polarization_gif, 
                                          renderer = gifski_renderer(), 
                                          duration = 30, 
                                          fps = 20,
                                          width = 800, 
                                          height = 800)

anim_save("xj_polarization_map.gif", animation = map_polarization_animated)

#CHANGE 
df_xj_diversity_ch <- df_xj_diversity %>%
  group_by(Code_County) %>% 
  arrange(Code_County, Year) %>%  
  mutate(Change_Fractionalization = Fractionalisation - lag(Fractionalisation),
         Change_Fractionalization_Start = Fractionalisation - first(Fractionalisation)) %>%
  ungroup()

summary(df_xj_diversity_ch$Change_Fractionalization)
hist(df_xj_diversity_ch$Change_Fractionalization)

map_data_2 <- df_xj_diversity_ch %>%
  left_join(df_china_sf) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year != 2010)

map_fractionalization_change_gif <- ggplot() +
  geom_sf(data = map_data_2, aes(fill = Change_Fractionalization, geometry = geometry)) +
  scico::scale_fill_scico(palette = "vik", na.value = "white") +
  labs(
    title = "Change in Ethnic Fractionalization in Xinjiang, {closest_state}",
    fill = "Annual Chnage in Degree of Fractionalization",
    caption = "© Jan Schlebusch 2024"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 15, hjust = 0.1)
  ) +
  transition_states(
    states = Year,
    transition_length = 0,
    state_length = 0.5,
    wrap = FALSE
  ) +
  ease_aes('cubic-in-out')

map_fractionalization_change_animated <- animate(map_fractionalization_change_gif, 
                                          renderer = gifski_renderer(), 
                                          duration = 30, 
                                          fps = 20,
                                          width = 800, 
                                          height = 800)

anim_save("xj_fractionalization_change_map.gif", animation = map_fractionalization_change_animated)

#OVERALL change
df_overall_change <- df_xj_diversity %>%
  filter(Year %in% c(2000, 2018)) %>%  # Filter only for the first and last years
  group_by(Code_County) %>%
  summarize(
    First_Fractionalization = Fractionalisation[Year == 2000],
    Last_Fractionalization = Fractionalisation[Year == 2018],
    Overall_Change = Last_Fractionalization - First_Fractionalization
  )  
  
map_data_3 <- df_overall_change %>%
  left_join(df_china_sf)

xj_fractionalization_abschange_map <- ggplot(data = map_data_3) +
  geom_sf(aes(fill = Overall_Change, geometry = geometry)) + 
  scico::scale_fill_scico(palette = "cork", na.value = "white") +
  labs(
    title = "Change in Ethnic Fractionalization in Xinjiang, 2000-2018",
    fill = "Absolute Change in Fractionalization"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10, hjust = 0.01,),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) 

ggsave("xj_fractioalization_abschange_map.png", plot = xj_fractionalization_abschange_map, width = 8, height = 5)

