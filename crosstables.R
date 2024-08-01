##Creates crosstable of FOIA data from "Eternal Nightmares" report

library(tidyverse)
library(janitor)
library(readxl)

df <- read_excel('./SRMS spreadsheet 9.1.2018 - 9.1.2023 Redacted.xlsx')

#clean variables
df <- df |>
  filter(`Tracking Number` != "") |>#Filter out cases that have no tracking number 
  filter(!is.na(`Release Date`)) |> #Filter out cases with no release date
  mutate(`Mental Illness` = ifelse(`Mental Illness` == "Yes", #Standardize Mental Illness field
                                   "Mental Illness", `Mental Illness`))|>
  mutate(Facility = ifelse(Facility == "WEBB COUNTY DETENTION CENTER (TX)", #Standardize facility field
                           "WEBB COUNTY DETENTION CENTER (CCA) (TX)", Facility)) |>
  filter(`Detainee Request Segregation` == "Facility-Initiated")

#Top 20 Country of Citizenship
ctzn <- df |>
  group_by(`Country of Citizenship`)|>
  filter(!is.na(`Country of Citizenship`))|>
  count()|>
  arrange(desc(n)) |>
  head(20)

#Facility by Top 20 Citizenship
ctzn_facility <- df |>
  filter(`Country of Citizenship` %in% ctzn$`Country of Citizenship`)|>
  tabyl(Facility, `Country of Citizenship`) |>
  adorn_totals(c('row', 'col'))

#Facility by Gender
facililty_gender <- df |>
  tabyl(Facility, Gender)|>
  adorn_totals(c('row', 'col'))

#Facility by Suicide Risk
gender_suicide <- df |>
  tabyl(Gender, `Suicide Risk?`)|>
  adorn_totals(c('row', 'col'))

