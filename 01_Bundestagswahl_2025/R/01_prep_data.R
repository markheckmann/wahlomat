library(readxl)
library(tidyverse)
library(OpenRepGrid)
library(janitor)
library(openxlsx)


# settings -----

data_folder <- "01_Bundestagswahl_2025/data"

file_in <- file.path(data_folder, "Wahl-O-Mat Bundestagswahl 2025_Datensatz_v1.01.xlsx")
file_out <- file.path(data_folder, "wide_format.xlsx")
file_out_grid <- file.path(data_folder, "grid.xlsx")

# prepare -----

x <- read_excel(file_in, "Datensatz BTW 2025") |> clean_names()
kf <- read_excel(file_in, "Kurzform") |>
  clean_names() |>
  select(these_nr, these_kurzform)

x2 <- x |> left_join(kf, by = join_by(these_nr))

x2 <- x2 |> mutate(
  position_num = case_match(
    position_position,
    "stimme nicht zu" ~ -1,
    "neutral" ~ 0,
    "stimme zu" ~ 1
  ),
  partei_kuerzel = ifelse(nchar(partei_kurzbezeichnung) <= 12,
    partei_kurzbezeichnung,
    abbreviate(partei_kurzbezeichnung, minlength = 10)
  )
)

df <- x2 |> pivot_wider(
  id_cols = c(these_nr, these_titel, these_kurzform, these_these),
  names_from = partei_kurzbezeichnung, values_from = position_num
)

df_grid <- df |>
  mutate(`-1` = NA) |>  # linke leere Spalte für Grid Daten
  select(`-1`, SPD:WerteUnion, `1` = these_kurzform)


# save ----

write.xlsx(df, file_out)
write.xlsx(df_grid, file_out_grid)
