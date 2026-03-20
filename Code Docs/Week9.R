library(ggplot2)
library(compmus)
library(dplyr)
library(tidyverse)

#Self Similarity Matrices
on_sight_chroma |>
  compmus_wrangle_chroma() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(timbre = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_self_similarity(timbre) |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")
