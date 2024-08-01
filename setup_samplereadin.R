#This file reads in foia dataset and then gets column frequency counts for each
# field in the dataset

library(tidyverse)
library(janitor)
library(readxl)
library(stringi)
library(openxlsx)

#read in
df <- read_excel('./SRMS spreadsheet 9.1.2018 - 9.1.2023 Redacted.xlsx')

noms <- names(df)

#Function to take in all columns, gets count, and then sends them to nested lists
fyColumnCount <- function(input){
  
  # Mapper
  #https://stackoverflow.com/questions/61018366/getting-a-summary-of-every-column-of-a-df-dplyrcount
  out <- map(names(input), ~ input |>
               select(all_of(.x)) |>
               count(!! rlang::sym(.x)))
  
  noms <- names(input)
  noms <- paste0(seq_along(noms), noms, sep = "-")
  
  
  
  noms <- lapply(noms, function(x) stri_sub(x, 1, 31)) #Reduce to 31 characters
  named_list <- setNames(out, noms)
  
}

df_columns <- fyColumnCount(df)

#Summarizing and adding Pct to every column in list
list <- function(list){
  df <- bind_rows(list)
  print(names(df[1]))
  df |>
    group_by(df[1])|>
    summarise(total = sum(n))|>
    arrange(desc(total))|>
    mutate(pct = 100*(total/sum(total)))|>
    head(100)|>
    adorn_totals("row")
}

result <- lapply(df_columns, list)


#Write the columns to one sheet
write_file <- function(df_list, sheet_name, output){
  # Create a new Excel workbook
  wb <- createWorkbook()
  
  #COUNTS AND PCT
  addWorksheet(wb, sheetName = sheet_name)
  counter <- 1
  # Add each data frame as a new sheet in the workbook
  for (n in df_list) {
    print(head(n, 10))
    writeData(wb, sheet = sheet_name, x = n, startCol = counter, startRow = 1)
    counter <- counter + 4
  }
  
  
  # Save the workbook to the specified file path
  saveWorkbook(wb, file = output, overwrite = TRUE)
}
#Write =
write_file(result, 'all_column_count', './Documents/bookins_Varcounts_ALL.xlsx')

# Filter out the number of missing values in every column
filter_na_col <- function(df, col_name) {
  df[is.na(df[[1]]), ]
}

missing <- lapply(result, filter_na_col)

write_file_missing <- function(df_list, sheet_name, output){
  # Create a new Excel workbook
  wb <- createWorkbook()
  # 
  # #COUNTS AND PCT
  addWorksheet(wb, sheetName = sheet_name)
  counter <- 2
  # Add each data frame as a new sheet in the workbook
  for (n in df_list) {
    df <- data.frame(n)
    var <- names(df[1])
    pct <- 100-as.numeric(df[3]$pct)
    pct <- ifelse(length(pct) == 0, 100, pct)
    print(var)
    print(pct)
    writeData(wb, sheet = sheet_name, x="Variable", startCol = 1, startRow = 1)
    writeData(wb, sheet = sheet_name, x="Pct.Entered", startCol = 2, startRow = 1)
    writeData(wb, sheet = sheet_name, x = var, startCol = 1, startRow = counter)
    writeData(wb, sheet = sheet_name, x = pct, startCol = 2, startRow = counter)
    counter <- counter + 1
  }
  # 
  # 
  # # Save the workbook to the specified file path
  saveWorkbook(wb, file = output, overwrite = TRUE)
}
#Write
write_file_missing(missing, "Missing Values", "./Documents/MissingValues.xlsx")


### Unknown
filter_unknown_col <- function(df){
  df <- df |>
    filter(str_detect(tolower(df[[1]]), "unknown"))
}
unknown <- lapply(result, filter_unknown_col)
unk <- data.frame()

write_file_unknown <- function(df_list){
  for (n in df_list){
    df <- data.frame(n)
    var <- names(df[1])
    pct <- as.numeric(df[3]$pct)
    df <- df |>
      mutate(Category = "Unknown")|>
      group_by(Category)|>
      summarise(pct = sum(pct))|>
      mutate(variable = var)
    unk<- rbind(unk, df)
  }
  print(unk)
  #final <- unknown
}
#At this point, I am manually entering these values into the third column of the "Missing Values" Worksheet
x <- write_file_unknown(unknown)
