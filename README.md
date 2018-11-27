# Behavior Analysis App

This is a Shiny app created to help collaborators in Dr. Craig Heller's lab analyze analyze mouse behavioral data. This app can analyze raw data files from the ViewPoint mouse tracker software for Open Field, Novel Object Recognition, Novel Object Location, and Barnes Maze experiments.

## Getting Started

You can use this app directly via https://catsai.shinyapps.io/BehaviorAnalysis.

**OR**

You can download [R](https://www.r-project.org/), [RStudio](https://www.rstudio.com/products/rstudio/download/#download), and these files. You'll also need these packages: shiny, readr, readxl, dplyr, and tidyr.

## Using the App
1. Head over to the app via the link or run the appBehavior.R file in RStudio.
2. Choose your experiment type from the left hand side drop down menu.
3. Upload the appropriate files. Example data files are also found in this repository. 
    * Open Field:
      * Takes 1 file.
      * Assumes there are 2 arenas with arena 2 being the center region.
    * Novel Object Recognition:
      * Takes 1 training day file and 1 testing day file.
      * Assumes there are 3 arenas with arena 3 being the new object.
    * Novel Object Location:
      * Takes 1 training day file and 1 testing day file.
      * Assumes there are 4 arenas with arena 4 being the object that has been moved.
    * Barnes Maze: 
      * Takes 1 file. Must choose whether it's a training day or testing day file.
      * Assumes there are 20 escape arenas and lets the user pick which arena was the target hole via .
      * User must input manually recorded primary latencies to target hole since tracking software sometimes fails to detect the initial entry.
4. Download the cleaned and analyzed data via the buttons on the left or the copy/csv buttons at the top of each table.

Good luck with your experiments!


## Authors
**Connie Tsai** 

