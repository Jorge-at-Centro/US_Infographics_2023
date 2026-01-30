Infographics_2023
================
2026-01-22

\#This project is a step by step process to getting U.S. Census data,
\#accompanying boundary files, \#displaying them geographically, and
\#creating \#visualizations for CENTRO published infographics.

\#We will start by calling the necesarry libraries to perform all
\#operations.

``` r
#We first install the package {pacman} if necessary. This package allows the 
#user to load and install packages in a more streamlined manner.

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,       #Tidyverse for data manipulation.
               tidycensus,      #To handle Census data.
               sf,              #To handle spatial data.
               tmap,            #To visualize spatial data.
               leaflet,         #in case we wanted to do interactive maps
               stringr,         #To do some string manipulation.
               tigris,          #To do some spatial data manipulation.
               grid)            #To do some visualization

#We are also going to give the program the instruction to avoid using scientific
#notation when generating calculations.
options(scipen=999)
```

\#The first step in our analysis is gathering the data we need to
produce the \#visualizations.

\#In 2023, there were 15 states for wich Puerto Rican population data
was \#available based on the 65k \#threshhold the U.S. Census
establishes for reporting \#information. These were: California,
\#Connecticut, Florida, Georgia, Illinois, \#Massachusetts, Maryland,
New Jersey, New York, North \#Carolina, Ohio, \#Pennsylvania, Texas,
Virginia, adn Wisconsin.

\#We will use the “get_acs” function from the {tydcensus} package to get
Puerto \#Rican population \#data for each of the states listed above.
Additionally we will \#gather information on the Hispanic \#population
since we will also be visualazing \#this group in a select number of
states.

``` r
#First we create a list containing the abbreviations of all the states of
#interest. This will serve for us to loop through and create new data frames 
#for each state. 
st_abv <- list("CA","CT","FL","GA","IL","MA","MD","NJ","NY",
               "NC","OH","PA","TX","VA","WI")

#Getting data for all 15 states. 
for (abv in st_abv) {
  
  df_current <- get_acs(geography = "county",
                    variables = c(
                      hisp_pop = "B03001_003E",
                      pr_pop = "B03001_005E"
                    ),
                    state = abv,
                    year = 2024,
                    survey = "acs5",output = "wide",
                    geometry = TRUE,
                    progress_bar = FALSE) %>% 
    select(-ends_with("M")) %>%     #This command deletes the margins of error.
    mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
           pct_hispop = hisp_pop/sum(hisp_pop)*100,
           sub_name = str_replace(NAME, " County.*$",""))

#Here we name the data frame being created with the associated state.  
df_name <- paste0("df_", abv)
assign(df_name, df_current)
}
```

\#In the previous chunk, we obtained data for the PR adn Hispanic
population for \#selected states. In the code, lines were included to
clean the names of the \#counties to ease readability when inserting
labels into future visualizations. \#This works relatively well for all
states except Connecticut, who switched from \#its 8 historic counties
to 9 “planning regions”. We need to do some extra steps \#to clean the
names in this state.

``` r
#We will use a similar structure to replace the "Planning Region, Connecticut"
#string in the name within the newly created sub_name variable. 
df_CT$sub_name <- str_replace(df_CT$NAME, " Planning.*$","")

#Now we will be able to place county equivalent labels to our visualizations
#for the state of Connecticut. 
```

\#We are also interested in generating visualizations for the Puerto
Rican \#population in selected \#cities across the U.S.. Because we are
using data at the \#tract level and bounding them to counties \#that
constitute the desired cities, \#we will generate the data frames for
these cities separately.

``` r
#NYC
df_nyc <- get_acs(geography = "tract",
                  variables = c(
                      hisp_pop = "B03001_003E",
                      pr_pop = "B03001_005E"
                    ),
                  state = "NY",
                  county = c("New York","Bronx","Kings","Richmond","Queens"),
                  year = 2024,
                  survey = "acs5",
                  output = "wide",
                  geometry = TRUE,
                  progress_bar = FALSE) %>%
  select(-ends_with("M")) %>%
  mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
         pct_hispop = hisp_pop/sum(hisp_pop)*100,
         sub_name = str_replace(NAME, " County.*$",""))

#Philadelphia
df_phl <-  get_acs(geography = "tract",
                  variables = c(
                      hisp_pop = "B03001_003E",
                      pr_pop = "B03001_005E"
                    ),
                  state = "PA",
                  county = "Philadelphia",
                  year = 2024,
                  survey = "acs5",
                  output = "wide",
                  geometry = TRUE,
                  progress_bar = FALSE) %>%
  select(-ends_with("M")) %>%
  mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
         pct_hispop = hisp_pop/sum(hisp_pop)*100,
         sub_name = str_replace(NAME, " County.*$",""))

#Chicago
```

\#Finaly, we will generate similar datasets for the whole U.S. at the
state level.

``` r
df_us <- get_acs(geography = "state",
                  variables = c(
                      hisp_pop = "B03001_003E",
                      pr_pop = "B03001_005E"
                    ),
                  year = 2024,
                  survey = "acs5",
                  output = "wide",
                  geometry = TRUE,
                  progress_bar = FALSE) %>%
  select(-ends_with("M")) %>%
  mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
         pct_hispop = hisp_pop/sum(hisp_pop)*100,
         sub_name = str_replace(NAME, " County.*$",""))
```

\#After we have genereted our data, we can proceed with visualizing them
\#geographically.

\#We can quickly glance at our data by using the plot() function.

``` r
#For example, we can quickly see the distribution of Puerto Rican population in 
#NY state. 
plot(df_NY["pct_prpop"])
```

![](Infographics_2023_files/figure-gfm/Map%20at%20a%20glance-1.png)<!-- -->

``` r
#This map shows, rather poorly, that Puerto Ricans are concentrated in the NYC
#and surrounding areas, with some presence in counties with large cities, such 
#as Buffalo and Rochester.
```

\#We can now procede to develop a more nuanced structure that produces
maps that \#are both \#informative and visually appeasing.

\#But first we should look at the basic structures of our spatial data.

``` r
#Because all our data sets were downloaded in the same manner, we can expect them
#to have similar structures. Of particular interest to us right now is their 
#projections. 

#Sticking with NY, we can look at the basic structure of the spatial data frame
#using the str() function.
str(df_NY)
```

    ## Classes 'sf' and 'data.frame':   62 obs. of  8 variables:
    ##  $ GEOID     : chr  "36005" "36119" "36053" "36029" ...
    ##  $ NAME      : chr  "Bronx County, New York" "Westchester County, New York" "Madison County, New York" "Erie County, New York" ...
    ##  $ hisp_pop  : num  773828 273730 1776 60918 9412 ...
    ##  $ pr_pop    : num  229525 52193 451 35635 3621 ...
    ##  $ geometry  :sfc_MULTIPOLYGON of length 62; first list element: List of 4
    ##   ..$ :List of 1
    ##   .. ..$ : num [1:19, 1:2] -73.8 -73.8 -73.8 -73.8 -73.8 ...
    ##   ..$ :List of 1
    ##   .. ..$ : num [1:7, 1:2] -73.8 -73.8 -73.8 -73.8 -73.8 ...
    ##   ..$ :List of 1
    ##   .. ..$ : num [1:25, 1:2] -73.8 -73.8 -73.8 -73.8 -73.8 ...
    ##   ..$ :List of 1
    ##   .. ..$ : num [1:202, 1:2] -73.9 -73.9 -73.9 -73.9 -73.9 ...
    ##   ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
    ##  $ pct_prpop : num  22.8398 5.1937 0.0449 3.546 0.3603 ...
    ##  $ pct_hispop: num  19.6598 6.9544 0.0451 1.5477 0.2391 ...
    ##  $ sub_name  : chr  "Bronx" "Westchester" "Madison" "Erie" ...
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA
    ##   ..- attr(*, "names")= chr [1:7] "GEOID" "NAME" "hisp_pop" "pr_pop" ...

``` r
#This provides us with a basic description of the contents of the data frame. We
#can see from the results that all variables have correct types. But we do not 
#see the projection information. To get that information we will use the 
#st_cr() function.
print(st_crs(df_NY)$epsg)
```

    ## [1] 4269

``` r
print(st_crs(df_NY)$wkt)
```

    ## [1] "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"latitude\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"longitude\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]"

``` r
#Both instructions provide the EPSG code for the current coordinate system and 
#the name of the current coordinate system. For now we will not make any changes. 
```

\#With a basic understanding of our datasets, we will proceed with
visualizing \#them. \#We are creating maps of each of the states and
cities for which we have \#generated information for both the Puerto
Rican and Hispanic populations.

\#Here we set up the color palett that we will bes using to fill the
maps.

``` r
ctr_plt <- c("#dde8eb", "#bbd1d7", "#8eb2bc", "#637d84", "#39474b")
```

\###CALIFORNIA###

``` r
curr_map <- tm_shape(df_CA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"CA_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_CA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"CA_pct_hispop.png")
```

\###Connecticut###

``` r
curr_map <- tm_shape(df_CT) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 0.7,
                                                 item.width = 1.4,
                                                 position = tm_pos_in("right","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"CT_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_CT) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 1.4,
                                                 position = tm_pos_in("right","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"CT_pct_hispop.png")
```

\###FLORIDA###

``` r
curr_map <- tm_shape(df_FL) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"FL_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_FL) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"FL_pct_hispop.png")
```

\###GEORGIA###

``` r
curr_map <- tm_shape(df_GA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","top"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"GA_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_GA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","top"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"GA_pct_hispop.png")
```

\###ILLINOIS###

``` r
curr_map <- tm_shape(df_IL) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"IL_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_IL) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of \nHispanics by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"IL_pct_hispop.png")
```

\###MASSACHUSETTS###

``` r
curr_map <- tm_shape(df_MA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("center","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"MA_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_MA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("center","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"MA_pct_hispop.png")
```

\###MARYLAND###

``` r
curr_map <- tm_shape(df_MD) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("center","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"MD_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_MD) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("center","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"MD_pct_hispop.png")
```

\###NORTH CAROLINA###

``` r
curr_map <- tm_shape(df_NC) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"NC_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_NC) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"NC_pct_hispop.png")
```

\###NEW JERSEY###

``` r
curr_map <- tm_shape(df_NJ) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"NJ_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_NJ) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"NJ_pct_hispop.png")
```

\###NEW YORK###

``` r
ny <- st_read("NY_state.shp")
```

    ## Reading layer `NY_state' from data source 
    ##   `C:\Users\Jorge Soldevila\Documents\Jorge Centro\Projects\PR_in_US_Infographics\US_Infographics_2023\NY_state.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1 feature and 7 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -79.76198 ymin: 40.49605 xmax: -71.85615 ymax: 45.01291
    ## Geodetic CRS:  WGS 84

``` r
ny <- st_transform(ny, st_crs(df_NY))
df_NY_crop <- st_intersection(df_NY,ny)
df_NY_crop <- st_make_valid(df_NY_crop)

nyc_inset_bb <- st_bbox(c(xmin = -74.30, ymin = 40.46, xmax = -73.20, ymax = 41.50), 
                        crs = st_crs(df_NY_crop))

df_county_nyc <- st_crop(df_NY_crop, nyc_inset_bb) 
nyc_inset_poly <- tmaptools::bb_poly(nyc_inset_bb)
```

``` r
curr_map <- tm_shape(df_NY_crop) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17)) +
        tm_shape(nyc_inset_poly) +
          tm_borders(col = "black")

nyc_cnty_map <- tm_shape(df_county_nyc) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt),
                         fill.legend = tm_legend_hide()) +
            tm_text("sub_name",
                      size = 0.35,
                      auto.placement = TRUE,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.35,
                      col = "white",
                      auto.placement = TRUE,
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.35,
                      auto.placement = TRUE,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.35,
                      auto.placement = TRUE,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = T,
                      inner.margins = c(0.02,0,0,0))
# curr_map <- curr_map +
#   tm_inset(tmaptools::bb(c(-74.30,40.46,-73.20,41.50), relative = T), 
#            group_id = 1,
#            width = 9) +
#   tm_components(1, position = (c(0.78,0.7)))
# 
# print(curr_map)
#   # tm_inset(nyc_cnty_map, 
#   #          group_id = 1,
#   #          width = 13,
#   #          height = 11) +
#   # tm_components(1, position = c(0.7,0.7))
# 
# tmap_save(curr_map, "test.png")

tmap_save(curr_map,insets_tm = nyc_cnty_map,
          insets_vp = viewport(x=.88, y=0.5, width = 0.68, height = 0.38),
          "NY_pct_prpop.png", dpi = 500)
```

``` r
curr_map <- tm_shape(df_NY_crop) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of \nHispanics by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17)) +
        tm_shape(nyc_inset_poly) +
          tm_borders(col = "black")

nyc_cnty_map <- tm_shape(df_county_nyc) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt),
                         fill.legend = tm_legend_hide()) +
            tm_text("sub_name",
                      size = 0.35,
                      auto.placement = TRUE,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.35,
                      col = "white",
                      auto.placement = TRUE,
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.35,
                      auto.placement = TRUE,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.35,
                      auto.placement = TRUE,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = T,
                      inner.margins = c(0.02,0,0,0))
# curr_map <- curr_map +
#   tm_inset(tmaptools::bb(c(-74.30,40.46,-73.20,41.50), relative = T), 
#            group_id = 1,
#            width = 9) +
#   tm_components(1, position = (c(0.78,0.7)))
# 
# print(curr_map)
#   # tm_inset(nyc_cnty_map, 
#   #          group_id = 1,
#   #          width = 13,
#   #          height = 11) +
#   # tm_components(1, position = c(0.7,0.7))
# 
# tmap_save(curr_map, "test.png")

tmap_save(curr_map,insets_tm = nyc_cnty_map,
          insets_vp = viewport(x=.88, y=0.5, width = 0.68, height = 0.38),
          "NY_pct_hispop.png", dpi = 500)
```

\###NEW YORK CITY###

``` r
df_nyc_cnty <- df_NY_crop %>% 
  filter(sub_name == "New York" |
           sub_name == "Queens" |
           sub_name == "Kings" |
           sub_name == "Bronx" |
           sub_name == "Richmond")

df_nyc_clp <- st_intersection(df_nyc,df_nyc_cnty)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout
    ## all geometries

``` r
#df_nyc_clp <- st_make_valid(df_nyc_clp)
```

``` r
curr_map <- tm_shape(df_nyc_clp) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 2, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans in NYC\nby Census Tract*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","top"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)) +

            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17)) +
    tm_shape(df_nyc_cnty) +
     tm_borders(col = "black")+
     tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))
  

tmap_save(curr_map,"NYC_pct_prpop.png", dpi = 500)
```

``` r
curr_map <- tm_shape(df_nyc_clp) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 2, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of \nHispanics in NYC\nby Census Tract**",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","top"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)) +

            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17)) +
    tm_shape(df_nyc_cnty) +
     tm_borders(col = "black")+
     tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.75,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))
  


tmap_save(curr_map,"NYC_pct_hispop.png",dpi = 500)
```

\###OHIO###

``` r
curr_map <- tm_shape(df_OH) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"OH_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_OH) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"OH_pct_hispop.png")
```

\###PENSYLVANIA###

``` r
curr_map <- tm_shape(df_PA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"PA_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_PA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"PA_pct_hispop.png")
```

\###PHILADELPHIA###

``` r
curr_map <- tm_shape(df_phl) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)) #+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = -0.03,
            #                                 shadow.offset.y = 0.03))+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = 0.035,
            #                                 shadow.offset.y = -0.035))+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = -0.035,
            #                                 shadow.offset.y = -0.035))+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = 0.035,
            #                                 shadow.offset.y = 0.035))+
            # tm_layout(frame = FALSE,
            #           inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"PHL_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_phl) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)) #+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = -0.03,
            #                                 shadow.offset.y = 0.03))+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = 0.035,
            #                                 shadow.offset.y = -0.035))+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = -0.035,
            #                                 shadow.offset.y = -0.035))+
            # tm_text("sub_name",
            #           size = 0.55,
            #           col = "white",
            #           fontface = 2,
            #           options = opt_tm_text(remove_overlap = TRUE,
            #                                 shadow = TRUE,
            #                                 shadow.offset.x = 0.035,
            #                                 shadow.offset.y = 0.035))+
            # tm_layout(frame = FALSE,
            #           inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"PHL_pct_hispop.png")
```

\###TEXAS###

``` r
curr_map <- tm_shape(df_TX) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"TX_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_TX) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"TX_pct_hispop.png")
```

\###VIRGINIA###

``` r
curr_map <- tm_shape(df_VA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","top"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"VA_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_VA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("right","top"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"VA_pct_hispop.png")
```

\###WISCONSIN###

``` r
curr_map <- tm_shape(df_WI) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"WI_pct_prpop.png")
```

``` r
curr_map <- tm_shape(df_WI) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Hispanics\nby County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)
                           ) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = TRUE,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"WI_pct_hispop.png")
```

\###USA###

``` r
#For this exercise, we do not need Puerto Rico in the map. 
#We will remove it. 
df_us <- df_us %>% 
  filter(NAME != "Puerto Rico")
```

``` r
#We will use the shift_geometry() function from the {tigris} package, which 
#repositions objects in the map to the "standard" form for inset maps. 
us_shifted <- shift_geometry(df_us)
us_shifted$sub_name <- ifelse(us_shifted$sub_name == "Rhode Island","",us_shifted$sub_name)
curr_map <- tm_shape(us_shifted) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by State*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","center"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
  tm_shape(us_shifted %>% filter(NAME == "New York")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
    tm_shape(us_shifted %>% filter(NAME == "Pennsylvania")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
  tm_shape(us_shifted %>% filter(NAME == "Virginia")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
  tm_shape(us_shifted %>% filter(NAME == "Connecticut")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035)) +
  tm_shape(us_shifted %>% filter(NAME == "Massachusetts")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035)) +
  tm_shape(us_shifted %>% filter(NAME == "Indiana")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035)) +
   tm_shape(us_shifted %>% filter(NAME == "Wisconsin")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035)) +
   tm_layout(frame = FALSE,
             inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"usa_pct_prpop.png", dpi = 500)
```

``` r
curr_map <- tm_shape(us_shifted) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_hispop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt,
                         label.format = tm_label_format(digits = 1, 
                                                        suffix = " %",
                                                        text.separator = "to")),
                         fill.legend = tm_legend(title = "Distribution of \nHispanics by State*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 1,
                                                 item.width = 2,
                                                 position = tm_pos_in("left","center"),
                                                 frame = FALSE,
                                                 text.size = 0.8,
                                                 title.size = 1)) +
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = T,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
  tm_shape(us_shifted %>% filter(NAME == "New York")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
    tm_shape(us_shifted %>% filter(NAME == "Pennsylvania")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      ymod = 1.2,
                      options = opt_tm_text(remove_overlap = F,
                                            #just = "bottom",
                                            #ymod = 0.5,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
  tm_shape(us_shifted %>% filter(NAME == "Virginia")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
  tm_shape(us_shifted %>% filter(NAME == "Connecticut")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035)) +
  tm_shape(us_shifted %>% filter(NAME == "Massachusetts")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035)) +
  tm_shape(us_shifted %>% filter(NAME == "Indiana")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      xmod = 1.25,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035)) +
  tm_shape(us_shifted %>% filter(NAME == "Wisconsin")) +
    tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.03,
                                            shadow.offset.y = 0.03))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = -0.035,
                                            shadow.offset.y = -0.035))+
            tm_text("sub_name",
                      size = 0.55,
                      col = "white",
                      fontface = 2,
                      options = opt_tm_text(remove_overlap = F,
                                            shadow = TRUE,
                                            shadow.offset.x = 0.035,
                                            shadow.offset.y = 0.035))+
   tm_layout(frame = FALSE,
             inner.margins = c(0.02,0.17,0.02,0.17))

tmap_save(curr_map,"usa_pct_hispop.png", dpi = 500)
```
