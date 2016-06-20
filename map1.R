library('tmap')
library('leaflet')
#install.packages('leaflet')
library('magrittr')
library('rio')
library('plyr')
library("scales")

datafile <- "data/election.csv"
election <- rio::import(datafile)
election$Pickup <- as.factor(election$Pickup)
str(election)
#mnshape <- "data/L2012-1.shp"
mnshape2 <- "data/cb_2015_27_sldl_500k.shp"
#mngeo <- read_shape(file=mnshape)
mngeo <- read_shape(file=mnshape2)

qtm(mngeo2)

str(mngeo@data$NAME)


election$District <- 
  revalue(election$District,
          c("1A"="01A", "1B"="01B","2A"="02A", "2B"= "02B", "3A"="03A", "3B"= "03B",
            "4A"="04A", "4B"="04B", "5A"="05A", "5B"="05B", "6A"="06A", "6B"="06B",
            "7A"="07A", "7B"="07B", "8A"="08A", "8B"="08B", "9A"="09A", "9B"="09B"))

election$District
mngeo@data$DISTRICT <- as.character(mngeo@data$NAME)

mnmap <- append_data(mngeo, election, key.shp = "NAME", key.data="District")

qtm(mnmap, "DemBase")

mnPalette1 <- colorNumeric(palette = "Blues", domain = mnmap@data$MNLEGPERC2014)
qtm(shp = mnmap, fill = c("MNLEGPERC2014", "MNGOVPERC2014"), fill.palette = mnPalette1, ncol = 2)

mnPalette <- colorNumeric(palette = "RdBu", domain = mnmap@data$MNLEGPERC2014)
mnPalette <- colorBin(palette = "RdBu", domain = mnmap@data$MNLEGPERC2014,bins = c(0, .4, .45, .5, .55, .6, 1) , pretty=FALSE)

mnpopup <- paste0("<b>","District: ","</b>", mnmap@data$DISTRICT, "<br>",
                  "<b>", "Representative: ", "</b>", mnmap@data$Name, "<br>",
                  "<b>", "Party: ", "</b>", mnmap@data$Party, "<br>",
                  "<b>","DFL House 2014: ","</b>", percent(mnmap@data$MNLEGPERC2014), "<br>",
                  "<b>", "Pickup: ", "</b>", mnmap@data$Pickup, "<br>",
                  "<b>","DPI: ","</b>", percent(mnmap@data$DPI), "<br>",
                  "<b>","Dem Base: ","</b>", percent(mnmap@data$DemBase))

leaflet(mngeo2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE, 
              smoothFactor = 0.2,
              weight = 1,
              fillOpacity = .6, 
              popup=mnpopup,
              color= ~mnPalette(mnmap@data$MNLEGPERC2014)
              )%>% addLegend("bottomright", pal = mnPalette, values = ~mnmap@data$MNLEGPERC2014, title = "Results",
                             labFormat = labelFormat(transform = function(x) {
                               print(x*100)
                               }))

#section for multi-layered
minPct <- min(c(mnmap@data$MNLEGPERC2014, mnmap@data$MNGOVPERC2014, mnmap@data$DPI, mnmap@data$DemBase))
maxPct <- max(c(mnmap@data$MNLEGPERC2014, mnmap@data$MNGOVPERC2014, mnmap@data$DPI, mnmap@data$DemBase))
paletteLayers <- colorBin(palette = "RdBu", domain = c(minPct, maxPct), bins = c(0, .4, .45, .5, .55, .6, 1) , pretty=FALSE)
incumbPalette <- colorFactor(palette = c("blue", "grey", "red"), domain = mnmap@data$Pickup)

leaflet(mngeo2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=mnpopup, 
              color= ~incumbPalette(mnmap@data$Pickup),
              group="Incumbency"
  ) %>%
  addLegend(position="bottomleft", colors=c('blue', 'grey', 'red'), labels=c("Dem", "Incumbent", "Repub"))

mnResultmap <- leaflet(mngeo2) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=mnpopup, 
              color= ~incumbPalette(mnmap@data$Pickup),
              group="Pickup"
  ) %>% 
  addLegend(position="bottomleft", colors=c('blue', 'grey', 'red'), labels=c("Dem", "Incumbent", "Repub"))  %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mnmap@data$MNLEGPERC2014),
              group="DFL House 2014"
  ) %>% addLegend("bottomright", pal=paletteLayers, values = ~mnmap@data$MNLEGPERC2014, title = "Results") %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mnmap@data$DPI),
              group="DPI"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mnmap@data$DemBase),
              group="Democratic Base"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mnpopup, 
              color= ~paletteLayers(mnmap@data$MNGOVPERC2014),
              group="Governor"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Pickup", "DFL House 2014", "DPI", "Democratic Base", "Governor"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 
mnResultmap
