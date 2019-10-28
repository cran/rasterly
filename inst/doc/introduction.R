## ----library, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 4, out.width = "75%", warning=FALSE, message=FALSE----
library(rasterly)
library(data.table)
library(lubridate)
library(grid)
library(plotly)

## ----data----------------------------------------------------------------
# Load data
ridesRaw_1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv" %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv" %>% 
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"  %>% 
  data.table::fread(stringsAsFactors = FALSE)
ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>% 
  data.table::rbindlist()

# Extract hour of trip taken
time <- lubridate::ymd_hms(ridesDf$`Date/Time`)
ridesDf <-  ridesDf[, 'Date/Time':=NULL][, list(Lat, 
                                                Lon,
                                                hour = lubridate::hour(time), 
                                                month = lubridate::month(time),
                                                day = lubridate::day(time))]
head(ridesDf)

## ----basic, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 3----
start_time <- Sys.time()
p <- ridesDf %>% 
  rasterly(mapping = aes(x = Lat, y = Lon)) %>% 
  rasterize_points()
p
end_time <- Sys.time()
end_time - start_time

## ----list return---------------------------------------------------------
# A list of environments
str(p)

## ----subsetting, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 3----
p["background"]
# Replace the background in child layer `rasterly_points()`
p["background", level = 2] <- "black"
p["background"]
# color_maps in both `rasterly()` and `rasterly_points()` are replaced
## fire_map is a vector of colors (as character strings) with length 256
## see `rasterly::fire_map`
p["color_map", level = 1:2] <- fire_map
p

## ----rasterly_build------------------------------------------------------
build <- rasterly_build(p)
str(build)

## ----add_rasterly_heatmap, fig.width = 4, fig.height = 3-----------------
plotly::plot_ly(ridesDf, x = ~Lat, y = ~Lon) %>%
  add_rasterly_heatmap() %>% 
  layout(
    title = "Uber drives",
    x = list(
      title = "Lat"
    ),
    y = list(
      title = "Lon"
    )
  )

## ----plotly_rasterly, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 3, eval = FALSE----
#  # plotly
#  ply <- p %>%
#    plotly.rasterly(sizing = "contain")
#  ply

## ------------------------------------------------------------------------
r <- rasterly(data = ridesDf, 
                mapping = aes(x = Lat, y = Lon))

## ----set color, fig.width = 4, fig.height = 3----------------------------
r %>% 
  rasterize_points(
     mapping = aes(color = hour),
     color_key = hourColors_map,
     background = "black"
  ) -> g
g

## ----legend, fig.width = 4, fig.height = 3-------------------------------
# rasterly doesn't currently support legends, though this feature is forthcoming
plot(1:24, y = rep(1,24), col = hourColors_map, pch = 19, cex = 3)

## ----number of aggregation matrices--------------------------------------
build_g <- rasterly_build(g)
# the object has only one layer, so we index into the first element
length(build_g$agg[[1]])
# 24

## ----set color cover, fig.width = 4, fig.height = 3, eval = FALSE--------
#  r %>%
#    rasterize_points(
#       mapping = aes(color = hour),
#       color_key = hourColors_map,
#       background = "black",
#       layout = "cover"
#    )

## ----set on, fig.width = 4, fig.height = 3, eval = FALSE-----------------
#  r %>%
#    rasterize_points(
#      reduction_func = "mean", # take the "mean" reduction function
#      mapping = aes(on = -Lat)
#    )

## ----set size, fig.width = 4, fig.height = 3, eval = FALSE---------------
#  r %>%
#    rasterize_points(
#      mapping = aes(size = month),
#      max_size = 4
#    )

## ----reduction on mean, fig.width = 4, fig.height = 3, eval = FALSE------
#  r %>%
#    rasterize_points(
#      reduction_func = "mean", # process the data points using the mean reduction function
#      background = "black",    # change background to "black" from right to left (from dark to light)
#      color_map = fire_map # provide a custom color_map
#    )

## ----reduction on any, fig.width = 4, fig.height = 3, eval = FALSE-------
#  r %>%
#    rasterize_points(
#      reduction_func = "any",
#      color_map = c("white", "black")
#    )

