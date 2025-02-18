# Simulation von 20 ChatGPT Wählern pro Partei
#
# env var OPENAI_API_KEY muss gesetzt sein
#

library(readr)
library(stringr)
library(glue)
library(ellmer)
library(dplyr)
library(cli)


chatgpt_abfrage <- \(partei, model = "gpt-4o") {
  p <- read_lines("01_Bundestagswahl_2025/data/prompt.txt")
  prompt <- glue_collapse(p, sep = "\n")
  prompt_filled <- glue(prompt, PARTEI = partei, .open = "<", .close = ">") # <,> wegen JSON im Prompt
  chat <- chat_openai(
    api_key = Sys.getenv("OPENAI_API_KEY"),
    model = model, system_prompt = "", echo = TRUE
  )
  res <- chat$chat(prompt_filled)
  res <- res |>
    str_remove_all("```json") |>
    str_remove_all("```")
  list(
    prompt_filled = prompt_filled,
    res = res
  )
}

# Einfache Abfrage

parteien <- c("SPD", "CDU/CSU", "Bündnis 90/Die Grünen", "FDP", "AfD", "Die Linke", "Freie Wähler", "Volt",
              "Bündnis Deutschland", "WerteUnion", "Bündnis Sarah Wagenknecht (BSW)")

for (partei in parteien) {
  cli_alert_info("Partei: {partei}")
  l <- chatgpt_abfrage(partei)
  partei_cleaned <- recode(partei, "Bündnis 90/Die Grünen" = "Grüne", "Bündnis Sarah Wagenknecht (BSW)" = "BSW") |>
    janitor::make_clean_names()
  writeLines(l$res, con = glue("01_Bundestagswahl_2025/data/parteien_alle/{partei_cleaned}.json"))
}

# Wiederholte Abfrage pro Partei
N <- 20
parteien <-  c("SPD", "Bündnis Deutschland")
for (partei in parteien) {
  for (i in seq(N)) {
    cli_alert_info("Partei: {partei}, {i}/{N}")
    l <- chatgpt_abfrage(partei)
    partei_cleaned <- janitor::make_clean_names(partei)
    nr <- formatC(i, width = 2, flag = "0")
    writeLines(l$res, con = glue("01_Bundestagswahl_2025/data/parteien_v2/{partei_cleaned}_{nr}.json"))
  }
}

# clipr::write_clip(l[[1]]$prompt_filled)
