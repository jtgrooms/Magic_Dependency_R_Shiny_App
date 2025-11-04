# Magic_Dependency
Builds a model to predict 5th Edition Dungeons and Dragons Magical Dependency classes and deploying the results via an R Shiny App on shinyapps.io

# model_build.R
Builds the continuation ratio model that is used in the R Shiny app to predict probabilities of character magical dependency classes.
- Uses final_data.csv, a curated version of dnd character data found on Kaggle

# app.R
Builds the actual R Shiny app using just rshiny

# app_deploy.R
Deploys the shiny app to shinyapps.io. This code assumes you have already gone through the steps to set up your shinyapps.io account using the following instructions:
https://shiny.posit.co/r/articles/share/shinyapps/
