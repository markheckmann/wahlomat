library(readxl)
library(tidyverse)
library(OpenRepGrid)
library(janitor)
library(openxlsx)
library(jsonlite)
library(glue)


# SETTINGS----------------------------------------------------------------------------------------------------------

data_folder <- "01_Bundestagswahl_2025/data"

file_in <- file.path(data_folder, "Wahl-O-Mat Bundestagswahl 2025_Datensatz_v1.01.xlsx")

file_out_grid_wide <- file.path(data_folder, "alle_parteien_wide_v1.xlsx")
file_out_grid_offiziell <- file.path(data_folder, "grid/alle_parteien_offiziell_v1.xlsx")
file_out_grid_ki <- file.path(data_folder, "grid/alle_parteien_offiziell_und_ki_v1.xlsx")
file_template_ki_partei <- file.path(data_folder, "grid/ki_{partei}_v1.xlsx")


# PREPARE ----------------------------------------------------------------------------------------------------------

## Offizielle antworten ------------------------------------------------------------------

x <- read_excel(file_in, "Datensatz BTW 2025") |> clean_names()

x <- x |> mutate(
  partei_kurzbezeichnung = recode(partei_kurzbezeichnung,
    "CDU / CSU" = "CDU/CSU",
    "FREIE WÄHLER" = "Freie Wähler",
    "BÜNDNIS DEUTSCHLAND" = "Bündnis Deutschland"
  )
)

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
  )
)

df <- x2 |> pivot_wider(
  id_cols = c(these_nr, these_titel, these_kurzform, these_these),
  names_from = partei_kurzbezeichnung, values_from = position_num
)

df_grid <- df |>
  mutate(`-1` = NA) |> # linke leere Spalte für Grid Daten
  select(`-1`, SPD:WerteUnion, `1` = these_kurzform)

write.xlsx(df, file_out_grid_wide)
write.xlsx(df_grid, file_out_grid_offiziell, overwrite = TRUE)


## ChatGPT antwortet ------------------------------------------------------------------------

files <- list.files("01_Bundestagswahl_2025/data/parteien_alle_v1/", pattern = ".json", full.names = TRUE)
df_gpt <- purrr::map_df(files, \(file) {
  read_json(file, simplifyVector = TRUE) |>
    mutate(nr = as.integer(nr))
}) |> mutate(
  partei = recode(partei, "Bündnis 90/Die Grünen" = "GRÜNE", "Bündnis Sarah Wagenknecht" = "BSW")
)

df_gpt <- df_gpt |>
  transmute(
    these_nr = nr,
    partei_kurzbezeichnung = glue("{partei}(c)"),
    position_num = case_match(
      antwort,
      "ablehnen" ~ -1,
      "neutral" ~ 0,
      "zustimmen" ~ 1
    )
  ) |>
  left_join(kf, by = join_by(these_nr))

df2 <- df_gpt |> pivot_wider(
  id_cols = these_nr,
  names_from = partei_kurzbezeichnung, values_from = position_num
)

df_all <- df |> inner_join(df2, by = join_by(these_nr))

df_grid_all <- df_all |>
  mutate(`-1` = NA) |> # linke leere Spalte für Grid Daten
  select(`-1`, SPD:`WerteUnion(c)`, `1` = these_kurzform)

write.xlsx(df_grid_all, file_out_grid_ki)


## ChatGPT mehrmals pro Partei ------------------------------------------------------------

parteien <- c("SPD", "Bündnis Deutschland")
for (partei in parteien) {
  partei_ <- partei |> make_clean_names()
  files <- list.files("01_Bundestagswahl_2025/data/parteien_v1/", pattern = ".json", full.names = TRUE)
  files_partei <- files |> str_subset(partei_)
  df_gpt_partei <- purrr::map_df(files_partei, \(file) {
    read_json(file, simplifyVector = TRUE) |>
      mutate(
        nr = as.integer(nr),
        file = basename(file),
        i = str_extract(file, "[0-9]+")
      )
  }) |> as_tibble()

  df_gpt_partei <- df_gpt_partei |>
    transmute(
      these_nr = nr,
      partei_kurzbezeichnung = glue("{partei_}(c_{i})"),
      position_num = case_match(
        antwort,
        "ablehnen" ~ -1,
        "neutral" ~ 0,
        "zustimmen" ~ 1
      )
    ) |>
    left_join(kf, by = join_by(these_nr))

  df2 <- df_gpt_partei |> pivot_wider(
    id_cols = these_nr,
    names_from = partei_kurzbezeichnung, values_from = position_num # , values_fill = NA
  )

  df_all <- df |>
    select(these_nr, these_kurzform) |>
    inner_join(df2, by = join_by(these_nr))

  df_grid_all <- df_all |>
    mutate(`-1` = NA) |> # linke leere Spalte für Grid Daten
    select(`-1`, 3:22, `1` = these_kurzform)

  write.xlsx(df_grid_all, glue(file_template_ki_partei, partei = partei_))
}
