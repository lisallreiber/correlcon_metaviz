# script loads functions needed in this project

require(pacman) || install.packages("pacman")
library("pacman")

pacman::p_load(
               # codebook, package to generate automated codebooks, works well for labelled data and SOEP data
               here, # package to specify paths within R-projects
               forcats, # package for working with factors
               # janitor, # package for data cleaning
               knitr, # package for RMarkdown
               labelled,
               # prettydoc, package for other website layouts
               rmarkdown, # package for RMarkdown
               # sjlabelled, # pakcage for working with labelled data
               # skimr, #package for compact data description
               stringr, # for working with strings
               tidyr, # for drop_na() function and data cleaning
               rio, # for importing and exporting data
               scales, # for wrap_format() function and adapting plot scales
               ggplot2, # for plotting
               dplyr # for data wrangling
               # tidylog # very helpful to know and document what is going on in your data wrangling
               )
