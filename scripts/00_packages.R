# AUTHOR: Gustavo Castillo

cat("Run under:", version$version.string)

# requiere pacman
if(!require(pacman)){install.packages("pacman") ; require(pacman)}

p_load(tidyverse , # tidy-data
       rio, # read/write data from almost any file format
       dtplyr, # lazy_dt function for handling large amount of data
       stringr, # string functions
       data.table, # function: rbindlist()
       janitor, # function: clean_names()
       leaflet, # interactive html maps
       exactextractr,
       stars,
       ggmap, # import statemap options
       ggpattern,  # pattern fill for ggplot objects
       kableExtra, # making nice latex tables
       nngeo, # st_nn to do the distance to the nearest neigbour
       modelsummary, # summary tables
       readxl, # Import Excel spreadsheet files
       fixest, #  Fast Fixed-Effects Estimations
       viridis, # color pallete
       #ggsn, # north star and scale bar
       ggpubr, # ggexport function
       lubridate, # calculate diff between years function
       gridExtra, # grid.arrange function 
       spdep,
       stargazer, # View regression tables
       here, # Fix root directories
       haven, # Import stata databases
       labelled, # tools for labelled data
       rlang, # Toolbos for working with base types and core R features
       RcppRoll, # For building rolling windows
       RSelenium, # For scraping,
       rvest, # for web scraping
       arsenal, # To compare dataframes
       tools, 
       arrow,
       visdat,
       boot,
       papaja)

# solve package conflicts
filter = dplyr::filter
select = dplyr::select
`%no%` = Negate(`%in%`)
#scalebar = ggsn::scalebar

