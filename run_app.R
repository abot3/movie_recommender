library(shiny)

# Getting the path of your current open file
# current_path = rstudioapi::getActiveDocumentContext()$path
# setwd(dirname(current_path ))
# print(getwd())

runApp("MovieRecommender")
#runApp("BookRecommender", display.mode = "showcase")