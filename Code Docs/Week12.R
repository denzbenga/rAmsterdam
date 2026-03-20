library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)

library(compmus)

#adding compmus functions
get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}  

#remove some of the stuff such as "Danceabiltiy" to make your
#clusters more reasonable and understandable and functional
album_means_juice <-
  recipe(
    `track_name` ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      `song_dur`,
    data = album_means
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> This is if you want to cap values
  #from 0 to 1, rather than the center and scale model
  prep(album_means |> mutate(`track_name` = str_trunc(`track_name`, 36))) |>
  #nrc used in the line above must be the same as what the data = above
  juice() |>
  column_to_rownames("track_name")
#main things you may want to change are the name (nrc_juice), center
#and scale vs value capping, data = "", i think thats all

album_means$track_name[380] <- "Intro(Drake)"
#setting distances
album_means_dist <- dist(album_means_juice, method = "euclidean")

#setting the distances and plotting my corpus
album_means_dist |> 
  hclust(method = "complete") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()
#heatmap of my corpus
heatmaply(
  album_means_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)

pop <- read_csv("~/downloads/indie-pop.csv", col_types = "ccccDnnlcTccnnnnnnnnnnnn")
party <- read_csv("~/downloads/indie-party.csv", col_types = "ccccDnnlcTccnnnnnnnnnnnn")
workout <- read_csv("~/downloads/indie-running.csv", col_types = "ccccDnnlcTccnnnnnnnnnnnn")

indie <-
  bind_rows(
    pop |> mutate(Playlist = "Indie Pop") |> slice_head(n = 20),
    party |> mutate(Playlist = "Indie Party") |> slice_head(n = 20),
    workout |> mutate(Playlist = "Indie Running") |> slice_head(n = 20)
  )


indie_recipe <-
  recipe(
    Playlist ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = indie                    # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

indie_cv <- indie |> vfold_cv(5)


install.packages("kknn")
library(kknn)

knn_model <-
  nearest_neighbor(neighbors = 1) |>
  set_mode("classification") |> 
  set_engine("kknn")
indie_knn <- 
  workflow() |> 
  add_recipe(indie_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(indie_cv, control = control_resamples(save_pred = TRUE))

indie_knn |> get_conf_mat()

indie_knn |> get_conf_mat() |> autoplot(type = "mosaic")

indie_knn |> get_conf_mat() |> autoplot(type = "heatmap")

indie_knn |> get_pr()

install.packages("ranger")
library(ranger)

forest_model <-
  rand_forest() |>
  set_mode("classification") |> 
  set_engine("ranger", importance = "impurity")
indie_forest <- 
  workflow() |> 
  add_recipe(indie_recipe) |> 
  add_model(forest_model) |> 
  fit_resamples(
    indie_cv, 
    control = control_resamples(save_pred = TRUE)
  )

indie_forest |> get_pr()

workflow() |> 
  add_recipe(indie_recipe) |> 
  add_model(forest_model) |> 
  fit(indie) |> 
  pluck("fit", "fit", "fit") |>
  ranger::importance() |> 
  enframe() |> 
  mutate(name = fct_reorder(name, value)) |> 
  ggplot(aes(name, value)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Importance")

indie |>
  ggplot(aes(x = Acousticness, y = Liveness, colour = Playlist, size = Energy)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    x = "Acousticness",
    y = "Liveness",
    size = "Energy",
    colour = "Playlist"
  )

clean_corpus <- album_means[1:228, ]




#Vrijdag Maart Twintig Code

my_corp_recipe <-
  recipe(
    artist_name ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      song_dur,
    data = clean_corpus                    # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

#attempt #2 with IYRTITL's updated column names
iyrtitl_recipe <-
  recipe(
    `Track Name` ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = iyrtitl_corpus                    # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].



#Making the Random Forest

workflow() |> 
  add_recipe(iyrtitl_recipe) |> 
  add_model(forest_model) |> 
  fit(iyrtitl_corpus) |> 
  pluck("fit", "fit", "fit") |>
  ranger::importance() |> 
  enframe() |> 
  mutate(name = fct_reorder(name, value)) |> 
  ggplot(aes(name, value)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Importance")

clean_corpus |>
  ggplot(aes(x = valence, y = liveness, colour = artist_name, size = speechiness)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    x = "Valence",
    y = "Livenss",
    size = "Speechiness",
    colour = "Artist"
  )


#Making a Dendrogram

iyrtitl_juice <-
  recipe(
    `Track Name` ~
      #Danceability +
      Energy +
      Loudness +
      Speechiness +
      #Liveness +
      Valence,
    data = iyrtitl_corpus
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(iyrtitl_corpus |> mutate(`Track Name` = str_trunc(`Track Name`, 36))) |>
  juice() |>
  column_to_rownames("Track Name")


iyrtitl_dist <- dist(iyrtitl_juice, method = "euclidean")

iyrtitl_dist |> 
  hclust(method = "average") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()

iyrtitl_corpus <- filter(clean_corpus, album_name == "If You're Reading This It's Too Late")
