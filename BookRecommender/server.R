## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
# get_user_ratings <- function(value_list) {
#   dat <- data.table(book_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
#                     rating = unlist(as.character(value_list)))
#   dat <- dat[!is.null(rating) & !is.na(book_id)]
#   dat[rating == " ", rating := 0]
#   dat[, ':=' (book_id = as.numeric(book_id), rating = as.numeric(rating))]
#   dat <- dat[rating > 0]
#
#   # get the indices of the ratings
#   # add the user ratings to the existing rating matrix
#   user_ratings <- sparseMatrix(i = dat$book_id,
#                                j = rep(1,nrow(dat)),
#                                x = dat$rating,
#                                dims = c(nrow(ratingmat), 1))
# }

movie_url_to_tag = function(url) {
  # paste0(
  #   "<img src=\"",
  return(base64enc::dataURI(file=url, mime="image/jpeg"))
    # "\"></img>")
}

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"),
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}


# read in data
# ratings <- fread('data/ratings.csv')
ratings = read.csv('data/ratings.dat',
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# read in data
# myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines('data/movies.dat')
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
# convert accented characters
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
# small_image_url = "https://liangfgithub.github.io/MovieImages/"
# movies$image_url = sapply(movies$MovieID,
#                           function(x) paste0(small_image_url, x, '.jpg?raw=true'))
movies$image_url = sapply(movies$MovieID,
                          function(x) paste0("MovieImages/", x, ".jpg"))

# # reshape to movies x user matrix
# ratingmat <- sparseMatrix(ratings$book_id, ratings$user_id, x=ratings$rating) # book x user matrix
# ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
# dimnames(ratingmat) <- list(book_id = as.character(1:10000), user_id = as.character(sort(unique(ratings$user_id))))

# shinyServer(function(input, output, session) {
# server = function(input, output, session) {
shinyServer(function(input, output, session) {

  # show the movies to be rated
  output$ratings <- renderUI({
    # num_rows <- 20
    num_rows <- 2
    num_movies <- 6 # movies per row

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], style = "max-height:150")),
                 # div(style = "text-align:center; color: #999999; font-size: 80%", movies$authors[(i - 1) * num_movies + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))
                 )) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)

        user_results = (1:10)/10
        user_predicted_ids = 1:10
        recom_results <- data.table(Rank = 1:10,
                                    MovieID = movies$MovieID[user_predicted_ids],
                                    Title = movies$Title[user_predicted_ids],
                                    Predicted_rating =  user_results)

        # # add user's ratings as first column to rating matrix
        # rmat <- cbind(user_ratings, ratingmat)
        #
        # # get the indices of which cells in the matrix should be predicted
        # # predict all movies the current user has not yet rated
        # items_to_predict <- which(rmat[, 1] == 0)
        # prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
        #
        # # run the ubcf-alogrithm
        # res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
        #
        # # sort, organize, and return the results
        # user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
        # user_predicted_ids <- as.numeric(names(user_results))
        # recom_results <- data.table(Rank = 1:20,
        #                             Book_id = user_predicted_ids,
        #                             Title = movies$Title[user_predicted_ids],
        #                             Predicted_rating =  user_results)

    }) # still busy

  }) # clicked on button


  # display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),

          div(style = "text-align:center",
              a(img(src =  movie_url_to_tag(movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]]), height = 150))
          ),
          div(style="text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
          )

          # div(style = "text-align:center",
          #     a(href = paste0('https://www.goodreads.com/book/show/', movies$best_book_id[recom_result$Book_id[(i - 1) * num_movies + j]]),
          #       target='blank',
          #       img(src = movies$image_url[recom_result$Book_id[(i - 1) * num_movies + j]], height = 150))
          #    ),
          # div(style = "text-align:center; color: #999999; font-size: 80%",
          #     movies$authors[recom_result$Book_id[(i - 1) * num_movies + j]]
          #    ),
          # div(style="text-align:center; font-size: 100%",
          #     strong(movies$title[recom_result$Book_id[(i - 1) * num_movies + j]])
          #    )

        )
      }))) # columns
    }) # rows

  }) # renderUI function

# }  # server function
}) # server function
