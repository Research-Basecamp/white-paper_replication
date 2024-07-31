ctzn <- df |>
  group_by(`Country of Citizenship`)|>
  filter(!is.na(`Country of Citizenship`))|>
  count()|>
  arrange(desc(n)) |>
  head(20)

ctzn_facility <- df |>
  filter(`Country of Citizenship` %in% ctzn$`Country of Citizenship`)|>
  tabyl(Facility, `Country of Citizenship`) |>
  adorn_totals(c('row', 'col'))

facililty_gender <- df |>
  tabyl(Facility, Gender)|>
  adorn_totals(c('row', 'col'))

gender_suicide <- df |>
  tabyl(Gender, `Suicide Risk?`)|>
  adorn_totals(c('row', 'col'))

