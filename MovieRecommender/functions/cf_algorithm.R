
# ============================================================
# Functions used in implementation of collaborative filtering.
# ============================================================


library(Matrix)
library(recommenderlab)
library(slam)
library(data.table)
library(testit)

SYSTEM_ONE_TOP_N = 6
SYSTEM_TWO_TOP_N = 20
RANDOM_RATING_EN = TRUE

# Recommend the Top 6 movies in the checkbox genres. Moves are ranked in
# descending order by average rating. All moves must have been rated >= the mean
# number of ratings across all movies.
system_1_recommend <- function(ratings, movies,
                               genre_list, genre_matrix,
                               checkbox_genres) {
  assert(is.unsorted(movies$MovieID) == FALSE)
  num_genres = length(checkbox_genres)
  TOP_N = 6
  joined = ratings %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating))
  joined = movies %>%
    left_join(joined, by = 'MovieID', all.x = TRUE)

  # Transpose the relevant genres.
  row_idx = which(genre_list %in% checkbox_genres)
  genre_names = genre_list[row_idx]
  tgm = t(genre_matrix)
  tgm = tgm[row_idx, ]

  result_cols =  c("MovieID", "ave_ratings", "Genres", "Genre")
  results_list = vector("list", num_genres)
  names(results_list) = checkbox_genres
  results = data.frame(matrix(nrow = num_genres, ncol = 4))
  colnames(results) <- result_cols
  for (i in 1:length(row_idx)) {
    movies_row_idx = which(tgm[i, ] == 1)
    tmp = joined %>%
      slice(movies_row_idx) %>%
      mutate(mean_ratings_per_movie = mean(ratings_per_movie, na.rm = TRUE)) %>%
      filter(ratings_per_movie > mean_ratings_per_movie) %>%
      arrange(desc(ave_ratings))
    s = (i-1) * TOP_N + 1
    e = s + TOP_N - 1
    c = result_cols[!(result_cols %in% c("Genre"))]
    results[s:e, c] = tmp[1:TOP_N, c]
    results[s:e, ]$Genre = genre_names[i]
    results_list[[i]] = tmp$MovieID[1:TOP_N]
  }
  return(results)
}



# Recommend the Top 10 movies according the new user's ratings. Movies are
# ranked using UBCF.
system_2_recommend <- function(Rmat, ratings, rec_UBCF) {
  mids = paste0('m', ratings$MovieID)
  print("mids")
  print(mids)
  r = ratings$Rating

  movieIDs = colnames(Rmat)
  n.item = ncol(Rmat)
  new.ratings = rep(NA, n.item)
  new.ratings[which(movieIDs %in% mids)] = ratings$Rating
  # If all ratings are equal we can end up with NA predictions, so inject
  # at least 1 of each rating to avoid this degenerate case.
  if (length(unique(ratings$Rating))) {
    new.ratings[which(movieIDs == "m1193")] = 5
    new.ratings[which(movieIDs == "m661")] = 3
    new.ratings[which(movieIDs == "m76")] = 1
    new.ratings[which(movieIDs == "m2106")] = 2
    new.ratings[which(movieIDs == "m2804")] = 2
    new.ratings[which(movieIDs == "m919")] = 4
  }
  new.user = matrix(new.ratings,
                    nrow=1, ncol=n.item,
                    dimnames = list(
                      user=paste('Jimminy Cricket'),
                      item=movieIDs
                    ))
  new.Rmat = as(new.user, 'realRatingMatrix')

  recom1 = predict(rec_UBCF, new.Rmat, n=SYSTEM_TWO_TOP_N, type='topN')
  recom1@items
  recom1@ratings

  # recom2 = predict(rec_UBCF, new.Rmat, type = 'ratings')
  # order(as(recom2, "matrix"), decreasing = TRUE)[1:10]
  # as(recom1, "matrix")[order(as(recom1, "matrix"), decreasing = TRUE)[1:10]]
  return(recom1)
}












