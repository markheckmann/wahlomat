library(readxl)
library(tidyverse)
library(OpenRepGrid)
library(janitor)
library(openxlsx)

file_in <- "01_Bundestagswahl_2025/data/Wahl-O-Mat Bundestagswahl 2025_Datensatz_v1.01.xlsx"
file_out <- "01_Bundestagswahl_2025/data/wide_format.xlsx"

x <- read_excel(file_in, "Datensatz BTW 2025") |> clean_names()

x <- x |> mutate(
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

df <- x |> pivot_wider(
  id_cols = c(these_nr, these_titel, these_these),
  names_from = partei_kuerzel, values_from = position_num
)


write.xlsx(df, file_out)

#
# g <- importExcel("data/wide_format.xlsx")
# themen <- rightpoles(g)
# rightpoles(g) <- paste("↑", themen)
# leftpoles(g) <- paste(themen, "↓")
#
# parteien_auswahl <- c(1,2,3,4,5,6,8,14,25,28,26)
# ga <- g[, parteien_auswahl]
# bertinCluster(ga, align = F,
#               colors = c("red", "green"),
#               showvalues = F)
# biplot2d(ga, mai =c(1,1,1,1))
# gaa <- OpenRepGrid::reorder2d(ga)
# e_ii <- c(8:11, 1:7)
# bertin(gaa[, e_ii], align = F,
#               colors = c("red", "green"),
#               showvalues = F)
#
# biplot3d(g)
