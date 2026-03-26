library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(dendextend)
library(compmus)

library(ggraph)
library(igraph)
library(tidyverse)
theme_set(theme_void())


#Starting with clustering of tracks

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


#making a juice of the whole corpus
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
  #step_center(all_predictors()) |>
 # step_scale(all_predictors()) |> 
  step_range(all_predictors()) |> 
  #This is if you want to cap values
  #from 0 to 1, rather than the center and scale model
  prep(album_means |> mutate(`track_name` = str_trunc(`track_name`, 36))) |>
  #nrc used in the line above must be the same as what the data = above
  juice() |>
  column_to_rownames("track_name")
#main things you may want to change are the name (nrc_juice), center
#and scale vs value capping, data = "", i think thats all


#setting distances
album_means_dist <- dist(album_means_juice, method = "euclidean")

#setting the distances and plotting my corpus
album_means_dist |> 
  hclust(method = "complete") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram(rotate = TRUE)