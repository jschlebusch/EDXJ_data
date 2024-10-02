library(readxl)
library(tidyverse)

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
  
  
  
  


