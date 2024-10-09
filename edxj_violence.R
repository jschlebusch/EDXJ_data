library(readxl)
library(tidyverse)
library(mapchina)
library(sf)
library(ggplot2)

#shapefiles for counties in Xinjiang from the mapchina package
df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区") %>%
  select(c(Code_County, geometry)) %>%
  mutate(Code_County = as.factor(Code_County))

#all attacks from the Global Terrorism Database (START 2022)
df_attacks<-read_xlsx("globalterrorismdb_0522dist.xlsx")%>%
  filter(country_txt == "China")%>%
  filter(provstate %in% c("Xinjiang", "Xinjiang Uyghur"))%>%
  filter(iyear > 2000)

##cross-sectional correlation: heterogeneity and occurrence of violence; change in heterogeneity and occurrence of violence

#as sf to match with counties via coordinates
attacks_sf <- st_as_sf(df_attacks, coords = c("longitude", "latitude"), crs = 4326)
attacks_per_county <- st_join(attacks_sf, df_china_sf, join = st_within)
attacks_count <- attacks_per_county %>%
  group_by(Code_County) %>%
  summarize(attacks = n())

summary(attacks_count)

#as regular data frame
df_attacks_count <- st_drop_geometry(attacks_count)
df_attacks_count <- as.data.frame(df_attacks_count)

summary(df_attacks_count)
barplot(df_attacks_count$attacks)

duplicate_attacks <- table(df_attacks_count$Code_County)[table(df_attacks_count$Code_County) > 1]
print(duplicate_attacks)

#read demographic data of the EDXJDS
df_fractionalization_average <- read_xlsx("xj_fractionalization_average.xlsx") %>%
  mutate(Code_County = as.factor(Code_County))
summary(df_fractionalization_average)

df_polarization_average <- read_xlsx("xj_polarization_average.xlsx")%>%
  mutate(Code_County = as.factor(Code_County))
summary(df_polarization_average)

df_fractionalization_change <- read_xlsx("xj_fractionalization_change.xlsx")%>%
  mutate(Code_County = as.factor(Code_County))
summary(df_fractionalization_change)

df_polarization_change <- read_xlsx("xj_polarization_change.xlsx")%>%
  mutate(Code_County = as.factor(Code_County))
summary(df_polarization_change)

#fractionalization and attacks -- significant but very weak negative association; inconclusive
df_fa_a <- df_fractionalization_average %>%
  left_join(df_attacks_count) %>%
  mutate(attacks = ifelse(is.na(attacks), 0, attacks)) %>%
  filter(!is.na(Avg_Fractionalization))

summary(df_fa_a)

cor.test(df_fa_a$attacks, df_fa_a$Avg_Fractionalization, 
         method = "spearman")

ggplot(df_fa_a, aes(x = Avg_Fractionalization, y = attacks)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Attacks vs. Avg_Fractionalization",
       x = "Average Fractionalization",
       y = "Number of Attacks")

#polarization and attacks -- significant but very weak negative association; inconclusive
df_pa_a <- df_polarization_average %>%
  left_join(df_attacks_count) %>%
  mutate(attacks = ifelse(is.na(attacks), 0, attacks)) %>%
  filter(!is.na(Avg_Polarization))

summary(df_pa_a)

cor.test(df_pa_a$attacks, df_pa_a$Avg_Polarization, 
         method = "spearman")

ggplot(df_pa_a, aes(x = Avg_Polarization, y = attacks)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Attacks vs. Avg_Polarization",
       x = "Average Polarization",
       y = "Number of Attacks")

#change in fractionalization and attacks -- to be further explored
df_fc_a <- df_fractionalization_change %>%
  left_join(df_attacks_count) %>%
  mutate(attacks = ifelse(is.na(attacks), 0, attacks)) %>%
  filter(!is.na(Overall_Change))

summary(df_fc_a)

cor.test(df_fc_a$attacks, df_fc_a$Overall_Change, 
         method = "spearman")

ggplot(df_fc_a, aes(x = Overall_Change, y = attacks)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Attacks vs. Overall_Change",
       x = "Change in Fractionalization",
       y = "Number of Attacks")

#change in polarization -- to be further explored
df_pc_a <- df_polarization_change %>%
  left_join(df_attacks_count) %>%
  mutate(attacks = ifelse(is.na(attacks), 0, attacks)) %>%
  filter(!is.na(Overall_Change))

summary(df_pc_a)

cor.test(df_pc_a$attacks, df_pc_a$Overall_Change, 
         method = "spearman")

ggplot(df_pc_a, aes(x = Overall_Change, y = attacks)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Attacks vs. Overall_Change",
       x = "Change in Polarization",
       y = "Number of Attacks")
