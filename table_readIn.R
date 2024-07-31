#replicate table and graphics
library(ggthemes)

tab <- read_excel('2022_2023_ICE_quarterly_confinement_vulnerable.xlsx', sheet = 5)
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

#month-year
df2 <- df |>
  mutate(placement_year = year(as.Date(`Placement Date`)),
         placement_month = month(as.Date(`Placement Date`)),
         placement_yearmonth = paste0(placement_year, '-', 
                                      placement_month),
         placement_fyq = quarter(as.Date(`Placement Date`), type="year.quarter",fiscal_start = 10))

vuln <- df2 |>
  mutate(vulnerable = case_when(`Mental Illness` == "Mental Illness" ~ 1,
                                `Mental Illness` == "Serious Mental Illness" ~ 1,
                                `Serious Medical Illness` == "Yes" ~ 1,
                                `Serious Disability` == "Yes" ~ 1,
                                `Placement Reason` == "Protective Custody: Lesbian, Gay, Bisexual, Transgender (LGBT)" ~ 1,
                                `Placement Reason` == "Protective Custody: Victim of Sexual Assault" ~ 1,
                                `Placement Reason` == "Hunger Strike" ~ 1,
                                `Placement Reason` == "Suicide Risk Placement" ~ 1,
                                `Suicide Risk?` == "Suicide Risk"~1,
                                .default = 0)) |>
  filter(vulnerable==1)


fy22_q1 <- vuln |>
  filter(`Placement Date` >= "2021-10-01") |>
  filter(`Placement Date` <= "2021-12-31") |>
  group_by(`Placement Reason`)|>
  count() |>
  adorn_totals('row')

fy22_q2 <- vuln |>
  filter(`Placement Date` >= "2022-01-01") |>
  filter(`Placement Date` <= "2022-03-31") |>
  group_by(`Placement Reason`)|>
  count()|>
  adorn_totals('row')

fy22_q3 <- vuln |>
  filter(`Placement Date` >= "2022-04-01") |>
  filter(`Placement Date` <= "2022-06-30") |>
  group_by(`Placement Reason`)|>
  count()|>
  adorn_totals('row')

fy22_q4 <- vuln |>
  filter(`Placement Date` >= "2022-07-01") |>
  filter(`Placement Date` <= "2022-09-30") |>
  group_by(`Placement Reason`)|>
  count()|>
  adorn_totals('row')

fy23_q1 <- vuln |>
  filter(`Placement Date` >= "2022-10-01") |>
  filter(`Placement Date` <= "2022-12-31") |>
  group_by(`Placement Reason`)|>
  count()|>
  adorn_totals('row')

all_vuln_fyq <- vuln |>
  group_by(placement_fyq)|>
  summarize(Placement_FOIA = n(),
            Length_FOIA = mean(`Length of Stay`)) |>
  filter(placement_fyq >= 2022.1)|>
  mutate(placement_fyq = case_when(placement_fyq == 2022.1 ~ "2022q1",
                                placement_fyq == 2022.2 ~ "2022q2",
                                placement_fyq == 2022.3 ~ "2022q3",
                                placement_fyq == 2022.4 ~ "2022q4",
                                placement_fyq == 2023.1 ~ "2023q1",
                                placement_fyq == 2023.2 ~ "2023q2",
                                placement_fyq == 2023.3 ~ "2023q3",
                                placement_fyq == 2023.4 ~ "2023q4"),
         source = "FOIA")

ice_nums <- tab |>
  mutate(source = "ICE_public")

combined <- all_vuln_fyq |> 
  left_join(ice_nums, by=c("placement_fyq"="PlacementQ")) |>
  filter(!is.na(Placement_ICE)) 


write.csv(combined, "rep_fig2-3.csv", row.names = FALSE)

fig2_data <- combined |>
  select(c(placement_fyq, Placement_FOIA, Placement_ICE)) |>
  pivot_longer(!placement_fyq, names_to = "Source", values_to = "Placements")

fig2_data$Source <- factor(fig2_data$Source, levels = c("Placement_ICE", "Placement_FOIA"))
fig3_data <- combined |>
  select(c(placement_fyq, Length_FOIA, Length_ICE))|>
  pivot_longer(!placement_fyq, names_to = "Source", values_to = "Mean Length")
fig3_data$Source <- factor(fig3_data$Source, levels = c("Length_ICE", "Length_FOIA"))


fig2_plot <- fig2_data |>
  ggplot(aes(x=placement_fyq, y=Placements, group = Source, color = Source))+
  geom_line() +
  labs(title = "Replication of Figure 2 in the 'Eternal Nightmares' Report",
       subtitle = "Number of Solitary Confinement Placements for Immigrants with Vulnerabilities")+
  scale_color_manual(values = c("red", "yellow"))+
  scale_y_continuous(limits = c(100, 425))+
  theme_classic() 
fig2_plot

fig3_plot <- fig3_data |>
  ggplot(aes(x=placement_fyq, y=`Mean Length`, group = Source, color = Source))+
  geom_line() +
  labs(title = "Replication of Figure 3 in the 'Eternal Nightmares' Report",
       subtitle = "Length of Solitary Confinement for Immigrants with Vulnerabilities")+
  scale_color_manual(values = c("red", "yellow"))+
  scale_y_continuous(limits = c(5, 22))+
  theme_classic()
fig3_plot
