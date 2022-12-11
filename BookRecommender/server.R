## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# Define utility functions.

movie_url_to_tag = function(url) {
  USE_REMOTE_IMAGES = TRUE
  if (USE_REMOTE_IMAGES) {
    small_image_url = "https://liangfgithub.github.io/"
    return (paste0(small_image_url, url, '?raw=true'))
  } else {
    return (url)
  }
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


# Read in ratings data.
ratings = read.csv('data/ratings.dat',
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
Rmat = Rmat[1:500, ] # let's use a subset

rec_UBCF = NULL
if (all(file.exists("rec.rds"))) {
  rec_UBCF <- readRDS("rec.rds")
  rec_UBCF
} else {
  rec_UBCF = Recommender(Rmat, method = 'UBCF',
                         parameter = list(normalize = 'Z-score',
                                          method = 'Cosine',
                                          nn = 25))
  saveRDS(rec_UBCF, file = "rec.rds")
  # unlink("rec.rds")
}


# Read in movies data.
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


# Build the genre matrix
genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)
genre_list = c("Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical",
               "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")
m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list


shinyServer(function(input, output, session) {

  # TODO(aaronbotelho) - remove this in the final version
  observeEvent(input$browser, {
    browser()
    1 + 1
  })

  # Show the system 1 movie genres to be selected.
  # Output: thumbnails + genre checkbox.
  output$genres <- renderUI({
    # num_rows <- 20
    num_movies <- 6 # movies per row
    num_genres <- length(genre_list)

    # checkboxInput
    movies_in_genre = data.frame(matrix(nrow = num_genres*num_movies, ncol = ncol(movies)))
    colnames(movies_in_genre) = colnames(movies)
    for(i in 1:num_genres) {
      s = (i - 1) * num_movies + 1
      e = s + num_movies - 1
      movies_in_genre[s:e, ] = movies[which(t(genre_matrix)[i,]==1)[1:num_movies], ]
    }
    # Create the list of divs with each genre heading and 6 images for each genre.
    tmp = lapply(1:num_genres, function(i) {
      list(
        fluidRow(h3(style = "text-align:center", genre_list[i])),
        fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movie_url_to_tag(movies_in_genre$image_url[(i - 1) * num_movies + j]),
                                                      style = "max-height:90%; height:90%; width:100%")),
                 div(style = "text-align:center", strong(movies_in_genre$Title[(i - 1) * num_movies + j]))
                 ))
        }))
      )
    })

    # Add the checkbox list
    choices = rep(0, num_genres)
    choices = genre_list
    names(choices) = genre_list
    list(
      checkboxGroupInput("genres_checkbox",
                         label = "Genre picker",
                         choices = choices)
      ,tmp)
  })


  # Calculate top genre recommendations when the submit button is clicked
  df1 <- eventReactive(input$genre_btn, {
    withBusyIndicatorServer("genre_btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        num_movies <- SYSTEM_ONE_TOP_N # movies per row
        num_genres <- length(input$genres_checkbox)
        recommendations = system_1_recommend(ratings, movies,
                                             genre_list, genre_matrix,
                                             input$genres_checkbox)
        recommendations = recommendations %>%
                            inner_join(movies, by = 'MovieID')
        # print(recommendations)
        return(recommendations)

    }) # still busy

  }) # clicked on button

  # Show the system 1 genre-based recommendations.
  # Output: thumbnails + top 5 of selected genres.
  output$genre_results <- renderUI({
    recom_result <- df1()
    num_rows <- length(unique(recom_result$Genre))
    num_movies <- SYSTEM_ONE_TOP_N

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        m_id = recom_result$MovieID[(i - 1) * num_movies + j]
        row = movies$MovieID == m_id
        # box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", j),

          div(style = "text-align:center",
              a(img(src = movie_url_to_tag(movies$image_url[row]), height = 150))
          ),
          div(style="text-align:center; font-size: 100%",
              strong(movies$Title[row])
          ),
          div(style="text-align:center; font-size: 100%",
              # strong(movies$Genres[recom_result$MovieID[(i - 1) * num_movies + j]])
              strong(recom_result$Genre[(i - 1) * num_movies + j])
          )
        )
      }))) # columns
    }) # rows
  })









  #=========================================================================================================






  # show the movies to be rated
  output$ratings <- renderUI({
    # num_rows <- 20
    num_rows <- 10
    num_movies <- 6 # movies per row
    num_total <- num_rows * num_movies

    sampled_movies = movies
    if (RANDOM_RATING_EN) {
      sampled_movies = movies[sample(nrow(movies),size=num_total,replace=FALSE), ]
    }

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movie_url_to_tag(sampled_movies$image_url[(i - 1) * num_movies + j]),
                                                      style = "max-height:90%; height:90%; width:100%")),
                 div(style = "text-align:center", strong(sampled_movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;",
                     ratingInput(paste0("select_", sampled_movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))
                 )) #00c0ef
      })))
    })
  })

  # https://campuswire.com/c/G3D46BBBA/feed/1272 - Predicted ratings exceed 5 pts?!
  # https://campuswire.com/c/G3D46BBBA/feed/1275 - How to make movie recommendation for a new user
  # https://campuswire.com/c/G3D46BBBA/feed/1276 - No ratings for a new user using UBCF
  # Calculate recommendations when the sbumbutton is clicked
  df2 <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        # print("User Ratings")
        # print(user_ratings)

        results = system_2_recommend(Rmat, user_ratings, rec_UBCF)

        user_results = unlist(lapply(as.vector(results@ratings$`0`),
                                     function(x){ max(min(5,x),0) }))
        user_predicted_ids = as.vector(results@items$`0`)
        midxs = which(movies$MovieID %in% user_predicted_ids)
        # print("Midxs")
        # print(midxs)
        recom_results <- data.table(Rank = 1:SYSTEM_TWO_TOP_N,
                                    MovieID = movies$MovieID[midxs],
                                    Title = movies$Title[midxs],
                                    Predicted_rating = user_results,
                                    Row = midxs)
    }) # still busy

  }) # clicked on button


  # display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_movies <- 5
    assert(num_rows * num_movies == SYSTEM_TWO_TOP_N)
    recom_result <- df2()
    # print("recom_result")
    # print(recom_result)

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        m_id = recom_result$MovieID[(i - 1) * num_movies + j]
        row = recom_result$Row[(i - 1) * num_movies + j]
        box(width = 2, status = "success", solidHeader = TRUE,
            title = paste0("Rank ", recom_result$Rank[(i - 1) * num_movies + j]),
          div(style = "text-align:center",
              a(img(src = movie_url_to_tag(movies$image_url[row]), height = 150))
          ),
          div(style="text-align:center; font-size: 100%",
              strong(movies$Title[row])
          ),
          div(style="text-align:center; font-size: 100%",
              # strong(movies$Genres[recom_result$MovieID[(i - 1) * num_movies + j]])
              strong(recom_result$Predicted_rating[(i - 1) * num_movies + j])
          )
        )
      #   box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
      #
      #     div(style = "text-align:center",
      #         a(img(src = movie_url_to_tag(movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]]), height = 150))
      #     ),
      #     div(style="text-align:center; font-size: 100%",
      #         strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
      #     )
      #   )
      # }))) # columns
      }))) # columns
    }) # rows
  }) # renderUI function

# }  # server function
}) # server function
