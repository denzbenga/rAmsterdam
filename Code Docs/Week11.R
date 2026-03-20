library(tidyverse)
library(compmus)
library(ggplot2)
library(dplyr)

ten_bands_nvlty_tempo_2 |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(40, 176) +                         # Adjust the limits to the desired time range
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")


on_sight_act |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()