#validating page 16-19 from "Eternal Nightmares" and graphics 4 and 5
library(tidyverse)
library(janitor)
library(readxl)


## Page 16
df <- read_excel('./SRMS spreadsheet 9.1.2018 - 9.1.2023 Redacted.xlsx')

#clean variables
df <- df |>
  filter(`Tracking Number` != "") |>#Filter out cases that have no tracking number 
  mutate(`Mental Illness` = ifelse(`Mental Illness` == "Yes", #Standardize Mental Illness field
                                   "Mental Illness", `Mental Illness`))|>
  mutate(Facility = ifelse(Facility == "WEBB COUNTY DETENTION CENTER (TX)", #Standardize facility field
                           "WEBB COUNTY DETENTION CENTER (CCA) (TX)", Facility))

### Graphic 6

time <- df |>
  filter(!is.na(`Release Date`))|>
  mutate(placement_year = year(as.Date(`Placement Date`)),
         placement_month = month(as.Date(`Placement Date`)),
         placement_yearmonth = paste0(placement_year, '-', 
                                      placement_month),
         placement_fyq = quarter(as.Date(`Placement Date`), type="year.quarter",fiscal_start = 10),
         release_year = year(as.Date(`Release Date`)))

avg_length <- time |>
  group_by(release_year) |>
  summarize(mean_length = mean(`Length of Stay`)) |>
  filter(release_year >= 2019)

### Graphic 6
fig6 <- avg_length |>
  ggplot(aes(x=release_year, y=mean_length))+
  geom_line(color="red")+
  geom_hline(yintercept=15, color="purple")+
  labs(title = "Replication of Figure 6 in the 'Endless Nightmares' Report",
       subtitle = "Average Number of Days in Solitary Confinement")+
  scale_y_continuous(limits = c(0, 35), breaks = seq(0,35,5))+
  theme_classic()
fig6  



#*transgender statistic
trans <- df |>
  filter(`Placement Reason` == "Protective Custody: Lesbian, Gay, Bisexual, Transgender (LGBT)")

summary(trans)

trans_fyq <- trans |>
  mutate(placement_fyq = quarter(as.Date(`Placement Date`), type="year.quarter",fiscal_start = 10))|>
  group_by(placement_fyq)|>
  count()


##Pg 17

mental_illness <- df |>
  group_by(`Mental Illness`)|>
  filter(!is.na(`Mental Illness`))|>
  count() |>
  adorn_totals("row")

mental_illnes_nacount <- df |>
  group_by(`Mental Illness`)|>
  count()

mental_year <- df |>
  mutate(placement_year = year(as.Date(`Placement Date`)))|>
  group_by(`Mental Illness`, placement_year) |>
  count() #|>
  #adorn_percentages()
  