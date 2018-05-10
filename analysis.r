#### Exploring income, density, and transit mode share in Vancouver ####

#load libraries
library(tidyverse)
library(plotly)
library(crosstalk)
library(cancensus)

#set api key for cancensus
options(cancensus.api_key = "YOUR_KEY")

#set working directory for data cache
options(cancensus.cache_path = getwd())

#list all available census year datasets
list_census_datasets()

#define the dataset and spatial level of analysis
dataset='CA16'
level="CSD" #census subdivision

#get a list of all regions
all_regions <- list_census_regions(dataset)

#define study area/region
regions <- all_regions %>%
  filter(level==level) %>% 
  filter(CMA_UID %in% c(59933)) %>% #Vancouver CMA
  as_census_region_list

#get a list of all vectors (variables)
all_vectors <- list_census_vectors(dataset)

#choose income and journey to work vectors
vectors <- all_vectors %>% 
  filter(vector %in% c("v_CA16_2397", 
                       "v_CA16_5792", 
                       "v_CA16_5801")) %>%
  pull("vector") 

#get data
data <- get_census(dataset = dataset,
                   level=level,
                   vectors=vectors, 
                   regions=regions, 
                   geo_format = "sf",
                   labels='detailed',
                   use_cache=TRUE)

#make better variables names 
names(data) <- tolower(make.names(names(data), unique=TRUE, allow_ = TRUE))

#rename a few variables
names(data)[13] <- "area_sqkm"
names(data)[14] <- "median_income"
names(data)[15] <- "total_allmodes"
names(data)[16] <- "transit"

#calculate transit mode share and filter missing data
data_transit <- data %>%
  mutate(transit_share = transit/total_allmodes,
         share = round(transit_share*100,2)) %>%
  filter(!is.na(median_income) & !is.na(transit_share))

#create a shared data object to enable linked brushing 
data_transit <- SharedData$new(data_transit, key = ~geouid)

#create ggplot map showing transit mode share
map_transit <- ggplot(data= data_transit) +
  geom_sf(aes(fill = share)) + 
  coord_sf(datum = "+proj=longlat +datum=WGS84 +no_defs")+
  scale_fill_distiller("Mode Share - Trip to Work", palette = 'RdYlGn', direction = 1, guide = FALSE)+
  theme(panel.grid.major=element_line(colour="transparent"), 
        rect = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggtitle('Journey To Work - Transit Mode Share\nSource: 2016 Census')

#make it interactive with plotly
maply_transit <- ggplotly(map_transit) %>%
  layout(xaxis = list(scaleanchor = "y", scaleratio = 1)) %>%
  highlight(on="plotly_click", color = "blue", off= "plotly_relayout")

#create a scatter plot of income and density
scatterly <- plot_ly(data_transit,
                     x = ~median_income/1000,
                     y = ~(population/area_sqkm), 
                     type = 'scatter',
                     mode = 'markers',
                     marker=list(size=15),
                     color = ~share,
                     colors='RdYlGn',
                     text= ~region.name,
                     hoverinfo = 'text') %>% 
  hide_legend() %>%
  layout(xaxis = list(title = "Median Household Income ($1000)") , 
         yaxis = list(title ="Density (population/sq km)")) %>%
  highlight("plotly_selected", color='blue')

#create a combined interactive visualization
bscols(maply_transit, scatterly)
