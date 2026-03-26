library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(dendextend)
library(compmus)
library(ggraph)
library(igraph)
library(tidyverse)
library(ranger)
library(kknn)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

#This is all Kanye

kanye_corpus <- clean_corpus |>
  filter(artist_name == "Kanye West")

kanye_corpus <- kanye_cleaned

#full corpus dates
corpus_dates2 <- clean_corpus %>%
  group_by(album_name, artist_name) %>%
  summarize(album_date = median(release_date))


kanye_dates <- kanye_corpus %>%
  group_by(album_name) %>%
  summarize(album_date = median(release_date))

#Kanye Valence
ggplot(kanye_corpus, aes(x = release_date, y = valence)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = kanye_dates$album_date,   # only show ticks for albums
    labels = kanye_dates$album_name         # label with album name
  ) +
  labs(
    title = "Valence of Kanye West Songs Over Time",
    x = "Year",
    y = "Valence"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Kanye Energy
ggplot(kanye_corpus, aes(x = release_date, y = energy)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = kanye_dates$album_date,   # only show ticks for albums
    labels = kanye_dates$album_name         # label with album name
  ) +
  labs(
    title = "Energy of Kanye West Songs Over Time",
    x = "Year",
    y = "Energy"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Kanye Speechiness
ggplot(kanye_corpus, aes(x = release_date, y = speechiness)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = kanye_dates$album_date,   # only show ticks for albums
    labels = kanye_dates$album_name         # label with album name
  ) +
  labs(
    title = "Speechiness of Kanye West Songs Over Time",
    x = "Year",
    y = "Speechiness"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Kanye Instrumentalness
ggplot(kanye_corpus, aes(x = release_date, y = instrumentalness)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = kanye_dates$album_date,   # only show ticks for albums
    labels = kanye_dates$album_name         # label with album name
  ) +
  labs(
    title = "Instrumentalness of Kanye West Songs Over Time",
    x = "Year",
    y = "Instrumentalness"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Kanye Danceability
ggplot(kanye_corpus, aes(x = release_date, y = danceability)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = kanye_dates$album_date,   # only show ticks for albums
    labels = kanye_dates$album_name         # label with album name
  ) +
  labs(
    title = "Danceability of Kanye West Songs Over Time",
    x = "Year",
    y = "Danceability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Kanye Popularity
ggplot(kanye_corpus, aes(x = release_date, y = pop)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = kanye_dates$album_date,   # only show ticks for albums
    labels = kanye_dates$album_name         # label with album name
  ) +
  labs(
    title = "Popularity of Kanye West Songs Over Time",
    x = "Year",
    y = "Popularity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#trying to add album names

key_albums <- c("Graduation", "My Beautiful Dark Twisted Fantasy", "Yeezus")


labels_kanye <- kanye_corpus %>%
  filter(album_name %in% key_albums) %>%
  group_by(album_name) %>%
  summarise(    
    release_date = mean(release_date),# midpoint for placement
    valence = mean(valence),   # or pick max/min if you prefer
    .groups = "drop"
  )


kanye_dates <- kanye_corpus %>%
  group_by(album_name) %>%
  summarize(album_date = median(release_date))


clean_corpus <- bind_rows(kanye_corpus, drake_corpus)

# Drake
# Drake
# Drake
# Drake
# Drake
# Drake
# Drake
# Drake


clean_corpus <- clean_corpus %>%
  mutate(release_date = case_when(
    album_name == "The Life of Pablo" ~ as.Date("2016-09-15"),
    album_name == "Watch The Throne - 2011" ~ as.Date("2011-05-06"),
    TRUE ~ release_date
  )
)


clean_corpus <- clean_corpus %>%
  mutate(album_name = recode(album_name, 
                        "The Life Of Pablo" = "The Life of Pablo - 2016"))
   


#Making my Drake data
drake_corpus <- clean_corpus |>
  filter(artist_name == "Drake")

drake_dates <- drake_corpus %>%
  group_by(album_name) %>%
  summarize(album_date = median(release_date))

#Drake Valence
ggplot(drake_corpus, aes(x = release_date, y = valence)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = drake_dates$album_date,   # only show ticks for albums
    labels = drake_dates$album_name         # label with album name
  ) +
  labs(
    title = "Valence of Drake Songs Over Time",
    x = "Year",
    y = "Valence"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Drake Energy
ggplot(drake_corpus, aes(x = release_date, y = energy)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = drake_dates$album_date,   # only show ticks for albums
    labels = drake_dates$album_name         # label with album name
  ) +
  labs(
    title = "Energy of Drake Songs Over Time",
    x = "Year",
    y = "Energy"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Drake Speechiness
ggplot(drake_corpus, aes(x = release_date, y = speechiness)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = drake_dates$album_date,   # only show ticks for albums
    labels = drake_dates$album_name         # label with album name
  ) +
  labs(
    title = "Speechiness of Drake Songs Over Time",
    x = "Year",
    y = "Speechiness"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Drake Instrumentalness
ggplot(drake_corpus, aes(x = release_date, y = instrumentalness)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = drake_dates$album_date,   # only show ticks for albums
    labels = drake_dates$album_name         # label with album name
  ) +
  labs(
    title = "Instrumentalness of Drake Songs Over Time",
    x = "Year",
    y = "Instrumentalness"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Drake Danceability
ggplot(drake_corpus, aes(x = release_date, y = danceability)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = drake_dates$album_date,   # only show ticks for albums
    labels = drake_dates$album_name         # label with album name
  ) +
  labs(
    title = "Danceability of Drake Songs Over Time",
    x = "Year",
    y = "Danceability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

#Drake Popularity
ggplot(drake_corpus, aes(x = release_date, y = pop)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.05) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(
    breaks = drake_dates$album_date,   # only show ticks for albums
    labels = drake_dates$album_name         # label with album name
  ) +
  labs(
    title = "Popularity of Drake Songs Over Time",
    x = "Year",
    y = "Popularity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())



# TRYING TO COMBINE
# TRYING TO COMBINE
# TRYING TO COMBINE
# TRYING TO COMBINE
# TRYING TO COMBINE


# First Violin Attempt
ggplot(clean_corpus, aes(x = album_name, y = valence, fill = artist_name)) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.8)) +  # semi-transparent violins side by side
  stat_summary(
    fun = mean, 
    geom = "point", 
    position = position_dodge(width = 0.8), 
    color = "black", 
    size = 2
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # rotate album labels
    panel.grid = element_blank()
  ) +
  labs(
    x = "Album",
    y = "Valence",
    fill = "Artist"
  )

# Second Attempt at Both Artists
ggplot(clean_corpus, aes(x = release_date, y = valence, color = artist_name)) +
  geom_point(position = position_jitter(width = 5), alpha = 0.3, size = 2) +
  geom_smooth(aes(group = artist_name, color = artist_name), method = "loess", se = FALSE, size = 1.2, alpha = 0.5) +
  scale_x_date(
    breaks = kanye_dates$album_date,
    labels = kanye_dates$album_name
  ) +
   scale_color_manual(
    values = c(
      "Kanye West" = "#f26c18",  # orange
      "Drake" = "#5d42f5"   # blue
    )
  ) +
  theme_minimal() +
  theme(
    axis.ticks.length = unit(5, "cm"),
    #panel.grid = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  labs(
    title = "Drake and Kanye West Albums' Valence Over Time",
    x = "Release Date",
    y = "Valence",
    color = "Artist"
  )

#attempting to color labels
library(dplyr)

corpus_dates3 <- corpus_dates3 %>%
  mutate(
    label = ifelse(
      artist_name == "Kanye",
      paste0("<span style='color:#1DB954;'>", album_name, "</span>"),
      paste0("<span style='color:#F0A500;'>", album_name, "</span>")
    )
  )

