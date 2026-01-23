Infographics_2023
================
2026-01-22

\#This project is a step by step process to getting U.S. Census data,
\#accompanying boundary files, displaying them geographically, and
\#creating \#visualizations for CENTRO published infographics.

\#We will start by calling the necesarry libraries to perform all
\#operations.

``` r
#We first install the package {pacman} if necessary. This package allows the 
#user to load and install packages in a more streamlined manner.

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, tidycensus, sf, tmap, leaflet)

#We are also going to give the program the instruction to avoid using scientific
#notation when generating calculations.
options(scipen=999)
```

\#The first step in our analysis is gathering the data we need to
produce the \#visualizations.

\#In 2023, there were 15 states for wich Puerto Rican population data
was \#available based on the 65k threshhold the U.S. Census establishes
for reporting \#information. These were: California, Connecticut,
Florida, Georgia, Illinois, \#Massachusetts, Maryland, New Jersey, New
York, North Carolina, Ohio, \#Pennsylvania, Texas, Virginia, adn
Wisconsin.

\#We will use the “get_acs” function from the {tydcensus} package to get
Puerto \#Rican population data for each of the states listed above.
Additionally we will \#gather information on the Hispanic population
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
                    year = 2023,
                    survey = "acs5",output = "wide",
                    geometry = TRUE,
                    progress_bar = FALSE) %>% 
    select(-ends_with("M")) %>%     #This command deletes the margins of error.
    mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
           pct_hispop = hisp_pop/sum(hisp_pop)*100)

#Here we name the data frame being created with the associated state.  
df_name <- paste0("df_", abv)
assign(df_name, df_current)
}
```

\#We are also interested in generating visualizations for the Puerto
Rican \#population in selected cities across the U.S.. Because we are
using data at the \#tract level and bounding them to counties that
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
                  year = 2023,
                  survey = "acs5",
                  output = "wide",
                  geometry = TRUE,
                  progress_bar = FALSE) %>%
  select(-ends_with("M")) %>%
  mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
         pct_hispop = hisp_pop/sum(hisp_pop)*100)

#Philadelphia
df_phl <-  get_acs(geography = "tract",
                  variables = c(
                      hisp_pop = "B03001_003E",
                      pr_pop = "B03001_005E"
                    ),
                  state = "PA",
                  county = "Philadelphia",
                  year = 2023,
                  survey = "acs5",
                  output = "wide",
                  geometry = TRUE,
                  progress_bar = FALSE) %>%
  select(-ends_with("M")) %>%
  mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
         pct_hispop = hisp_pop/sum(hisp_pop)*100)

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
                  year = 2023,
                  survey = "acs5",
                  output = "wide",
                  geometry = TRUE,
                  progress_bar = FALSE) %>%
  select(-ends_with("M")) %>%
  mutate(pct_prpop = pr_pop/sum(pr_pop)*100,
         pct_hispop = hisp_pop/sum(hisp_pop)*100)
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
maps that \#are both informative and visually appeasing.

\#But first we should look at the basic structures of our spatial data.

``` r
#Because all our data sets were downloaded in the same manner, we can expect them
#to have similar structures. Of particular interest to us right now is their 
#projections. 

#Sticking with NY, we can look at the basic structure of the spatial data frame
#using the str() function.
str(df_NY)
```

    ## Classes 'sf' and 'data.frame':   62 obs. of  7 variables:
    ##  $ GEOID     : chr  "36013" "36045" "36059" "36111" ...
    ##  $ NAME      : chr  "Chautauqua County, New York" "Jefferson County, New York" "Nassau County, New York" "Ulster County, New York" ...
    ##  $ hisp_pop  : num  11792 8481 256452 21562 2759 ...
    ##  $ pr_pop    : num  8679 2828 39882 7996 1438 ...
    ##  $ geometry  :sfc_MULTIPOLYGON of length 62; first list element: List of 1
    ##   ..$ :List of 1
    ##   .. ..$ : num [1:257, 1:2] -79.8 -79.8 -79.8 -79.8 -79.7 ...
    ##   ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
    ##  $ pct_prpop : num  0.835 0.272 3.836 0.769 0.138 ...
    ##  $ pct_hispop: num  0.3025 0.2175 6.578 0.5531 0.0708 ...
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA
    ##   ..- attr(*, "names")= chr [1:6] "GEOID" "NAME" "hisp_pop" "pr_pop" ...

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
visualizing \#them.

``` r
ctr_plt <- c("#dde8eb", "#bbd1d7", "#8eb2bc", "#637d84", "#39474b")

curr_map = tm_shape(df_CA) +
             tm_polygons(lwd = 0.4,
                         col="black",
                         fill = "pct_prpop",
                         fill.scale = tm_scale_intervals(style = "jenks",
                                                         values = ctr_plt),
                         fill.legend = tm_legend(title = "Distribution of Puerto\nRicans by County*",
                                                 title.fontface = 2,
                                                 title.fontfamily = "sans",
                                                 item.shape = "square",
                                                 item.height = 0.75,
                                                 item.width = 1.5,
                                                 position = tm_pos_in("left","bottom"),
                                                 frame = FALSE)
                           ) +
            tm_layout(frame = FALSE,
                      inner.margins = c(0.02,0.3,0.02,0.3))
print(curr_map)
```

![](Infographics_2023_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
tmap_save(curr_map,"test.png")
```

    ## Map saved to C:\Users\Jorge Soldevila\Documents\Jorge Centro\Projects\PR_in_US_Infographics\Infographics_2023\US_Infographics_2023\test.png

    ## Resolution: 2420.07 by 1822.261 pixels

    ## Size: 8.066901 by 6.074204 inches (300 dpi)

``` r
tmap_save(curr_map,"test.png")
```

    ## Map saved to C:\Users\Jorge Soldevila\Documents\Jorge Centro\Projects\PR_in_US_Infographics\Infographics_2023\US_Infographics_2023\test.png

    ## Resolution: 2420.07 by 1822.261 pixels

    ## Size: 8.066901 by 6.074204 inches (300 dpi)
