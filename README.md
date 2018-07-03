# tourism-dashboard
Interactive R shiny dashboard based on tourism statistics from DCMS, ONS and VisitBritain

The version of the app (currently prototype/alpha) running on shinyapps.io can be found [here](https://dcmsstats.shinyapps.io/tourism-dashboard/).

In summary, there are 5 R scripts which form the app:
Fetch_Data.R - this downloads all the files required for producing the dashboard from the web and stores them in the Data folder in the directory
Clean_Data.R - contains functions which clean the data files downloaded by Fetch_Data.R
Output_Data.R - creates the outputs that feed into the shiny app using the cleaned data
ui.R - contains the R code that generates the HMTL user interface for the shiny app
server.R - contains the R code which inputs reactive content into the ui.

More information on how Shiny Apps work can be found [here](https://shiny.rstudio.com/tutorial/) (I particularly recommend the video tutorial).

More detail on how specific bits of the code works can be found in the R files located in this repository.

