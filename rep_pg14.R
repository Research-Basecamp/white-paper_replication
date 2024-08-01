#validating page 14-15 from "Eternal Nightmares" and graphics 4 and 5
library(tidyverse)
library(janitor)
library(readxl)

df <- read_excel('./SRMS spreadsheet 9.1.2018 - 9.1.2023 Redacted.xlsx')

#clean variables
df <- df |>
  filter(`Tracking Number` != "") |>#Filter out cases that have no tracking number 
  mutate(`Mental Illness` = ifelse(`Mental Illness` == "Yes", #Standardize Mental Illness field
                                   "Mental Illness", `Mental Illness`))|>
  mutate(Facility = ifelse(Facility == "WEBB COUNTY DETENTION CENTER (TX)", #Standardize facility field
                           "WEBB COUNTY DETENTION CENTER (CCA) (TX)", Facility))

#find average length of stay
summarize_length <- df |>
  filter(!is.na(`Release Date`))|>
  summarize(count = n(),
            mean_length = mean(`Length of Stay`),
            median_length = median(`Length of Stay`))

length_90 <-df |>
  filter(!is.na(`Release Date`))|>
  filter(`Mental Illness` == "Mental Illness" | `Mental Illness` == "Serious Mental Illness") |>
  filter(`Length of Stay` >= 90)

length_365 <-df |>
  filter(!is.na(`Release Date`))|>
  filter(`Mental Illness` == "Mental Illness" | `Mental Illness` == "Serious Mental Illness") |>
  filter(`Length of Stay` >= 365)

##Sort facilities by length
longest_stays <- df |>
  filter(!is.na(`Release Date`))|>
  group_by(Facility, `Length of Stay`)|>
  count() |>
  arrange(desc(`Length of Stay`))

average_staysxfacility <- df |>
  filter(!is.na(`Release Date`))|>
  group_by(Facility) |>
  summarise(mean_stay = mean(`Length of Stay`)) |>
  arrange(desc(mean_stay))


## Year by year placements

#month-year
time <- df |>
  #filter(!is.na(`Release Date`))|>
  mutate(placement_year = year(as.Date(`Placement Date`)),
         placement_month = month(as.Date(`Placement Date`)),
         placement_yearmonth = paste0(placement_year, '-', 
                                      placement_month),
         placement_fyq = quarter(as.Date(`Placement Date`), type="year.quarter",fiscal_start = 10),
         release_year = year(as.Date(`Release Date`)))

yearly <- time |>
  group_by(placement_year)|>
  count()

monthly <- time |>
  group_by(placement_year, placement_month)|>
  count()

release_date <- time |>
  filter(!is.na(`Release Date`))|>
  group_by(release_year)|>
  summarise(mean_length = mean(`Length of Stay`))

###Proportion of solitary per 10k detained
det <- read_excel("2019_2023_daily_detention_confinement.xlsx", sheet = 'combined')

##Plot graphic 4
detplot <- det |>
  ggplot(aes(x=YearMonth, y=confinement_prop))+
  geom_line(color = "orange") +
  labs(title = "Replication of Figure 4 in the 'Eternal Nightmares' Report",
       subtitle = "Number of Immigrants Held in Solitary Confinement out of Total Population in Detention",
       x="",
       y="")+
  scale_y_continuous(limits = c(0,200))+
  theme_classic()
detplot
