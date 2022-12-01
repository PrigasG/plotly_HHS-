#Here is a suggestion using ggplot. It can be improved, but it gives you the basic idea.

library(maps)
library(ggplot2)

us.map <-  map_data('state')

# add PADD zones
us.map$PADD[us.map$region %in% 
              c('Alabama','Connecticut', 'Maine', '
                         Massachusetts', 'New Hampshire', 'Rhode Island', 
                'Vermont')] <- "Region 1"
us.map$PADD[us.map$region %in% 
              c('New Jersey', 'New York', 'New York City',
                'Puerto Rico', 'Puerto Rico Commonwealth',
                'U.S. Virgin Islands')] <- "Region 2"
us.map$PADD[us.map$region %in% 
              c('Delaware', 'District of Columbia', 'Maryland', 
                'Pennsylvania', 'Virginia', 'West Virginia')] <- "Region 3"
us.map$PADD[us.map$region %in% 
              c("montana", "idaho", "wyoming", "utah", "colorado")] <- "PADD 4: Rocky Mountain"
# us.map$PADD[us.map$region %in% 
#               c("washington", "oregon", "nevada", "arizona", "california")] <- "PADD 5: West Coast"
us.map$PADD[us.map$region %in% 
              c('Florida', 'Georgia', 'Kentucky', 
                'Mississippi', 'North Carolina', 'South Carolina', 
                'Tennessee')] <- "Region 4"
us.map$PADD[us.map$region %in% 
              c('Illinois', 'Indiana', 'Michigan', 
                'Minnesota', 'Ohio', 'Wisconsin')] <- "Region 5"
us.map$PADD[us.map$region %in% 
              c('Arkansas', 'Louisiana', 'New Mexico', 
                'Oklahoma', 'Texas')] <- "Region 6"
us.map$PADD[us.map$region %in% 
              c('Iowa', 'Kansas', 'Missouri', 'Nebraska')] <- "Region 7"
us.map$PADD[us.map$region %in% 
              c('Colorado', 'Montana', 'North Dakota', 
                'South Dakota', 'Utah', 'Wyoming')] <- "Region 8"
us.map$PADD[us.map$region %in% 
              c('American Samoa','Arizona', 'California', 
                'Federated States of Micronesia', 'Guam',
                'Hawaii', 'Nevada', 'Northern Marianas Islands', 
                'Palau', 'Republic of Marshall Islands')] <- "Region 9"
us.map$PADD[us.map$region %in% 
              c('Alaska', 'Idaho', 'Oregon', 'Washington')] <- "Region 10"

# subset the dataframe by padd zones and move lat/lon accordingly
us.map$lat.transp[us.map$PADD == "Region 1"] <- us.map$lat[us.map$PADD == "Region 1"]
us.map$long.transp[us.map$PADD == "Region 1"] <- us.map$long[us.map$PADD == "Region 1"] + 1


us.map$lat.transp[us.map$PADD == "Region 2"] <- us.map$lat[us.map$PADD == "Region 2"]
us.map$long.transp[us.map$PADD == "Region 2"] <- us.map$long[us.map$PADD == "Region 2"] + 2


us.map$lat.transp[us.map$PADD == "Region 3"] <- us.map$lat[us.map$PADD == "Region 3"]
us.map$long.transp[us.map$PADD == "Region 3"] <- us.map$long[us.map$PADD == "Region 3"] + 3

us.map$lat.transp[us.map$PADD == "PADD 1: East Coast"] <- us.map$lat[us.map$PADD == "Region 4"]
us.map$long.transp[us.map$PADD == "PADD 1: East Coast"] <- us.map$long[us.map$PADD == "Region 4"] + 4

us.map$lat.transp[us.map$PADD == "Region 5"] <- us.map$lat[us.map$PADD == "Region 5"]
us.map$long.transp[us.map$PADD == "Region 5"] <- us.map$long[us.map$PADD == "Region 5"]

us.map$lat.transp[us.map$PADD == "Region 6"] <- us.map$lat[us.map$PADD == "Region 6"] - 1
us.map$long.transp[us.map$PADD == "Region 6"] <- us.map$long[us.map$PADD == "Region 6"]

us.map$lat.transp[us.map$PADD == "Region 7"] <- us.map$lat[us.map$PADD == "Region 7"] - 2
us.map$long.transp[us.map$PADD == "Region 7"] <- us.map$long[us.map$PADD == "Region 7"]

us.map$lat.transp[us.map$PADD == "Region 8"] <- us.map$lat[us.map$PADD == "Region 8"] - 5
us.map$long.transp[us.map$PADD == "Region 8"] <- us.map$long[us.map$PADD == "Region 8"]

us.map$lat.transp[us.map$PADD == "Region 9"] <- us.map$lat[us.map$PADD == "Region 9"] 
us.map$long.transp[us.map$PADD == "Region 9"] <- us.map$long[us.map$PADD == "Region 9"]

us.map$lat.transp[us.map$PADD == "Region 10"] <- us.map$lat[us.map$PADD == "Region 10"] 
us.map$long.transp[us.map$PADD == "Region 10"] <- us.map$long[us.map$PADD == "Region 10"] + 5

# us.map$lat.transp[us.map$PADD == "PADD 4: Rocky Mountain"] <- us.map$lat[us.map$PADD == "PADD 4: Rocky Mountain"]
# us.map$long.transp[us.map$PADD == "PADD 4: Rocky Mountain"] <- us.map$long[us.map$PADD == "PADD 4: Rocky Mountain"] - 5
# 
# us.map$lat.transp[us.map$PADD == "PADD 5: West Coast"] <- us.map$lat[us.map$PADD == "PADD 5: West Coast"] - 2
# us.map$long.transp[us.map$PADD == "PADD 5: West Coast"] <- us.map$long[us.map$PADD == "PADD 5: West Coast"] - 10

# add labels
states <- aggregate(cbind(long.transp, lat.transp) ~ region, data=us.map, 
                    FUN=function(x)mean(range(x)))
states$labels <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "IA", 
                   "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", 
                   "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", 
                   "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", 
                   "VT", "WA", "WI", "WV", "WY")

# plot and use padd zone as fill
ggplot(us.map,  aes(x=long.transp, y=lat.transp), colour="white") + 
  geom_polygon(aes(group = group, fill=PADD)) +
  geom_text(data=states, aes(long.transp, lat.transp, label=labels), size=3) +
  theme(panel.background = element_blank(),  # remove background
        panel.grid = element_blank(), 
        axis.line = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  coord_equal()



library(tigris)
library(stringr)
library(plotly)

df_MVPS_jur <-  as.data.frame(states) %>% 
  mutate(region = str_to_title(region)) 

df_MVPS_jur['region'][df_MVPS_jur['region'] == 'District Of Columbia'] <- 'District of Columbia'


df_MVPS_jur$HHS_region[df_MVPS_jur$region %in% 
                         c('Alabama','Connecticut', 'Maine', 'Massachusetts'
                           , 'New Hampshire', 'Rhode Island', 
                           'Vermont')] <- "Region 1"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('New Jersey', 'New York', 'New York City',
                           'Puerto Rico', 'Puerto Rico Commonwealth',
                           'U.S. Virgin Islands')] <- "Region 2"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('Delaware', 'District of Columbia', 'Maryland', 
                           'Pennsylvania', 'Virginia', 'West Virginia')] <- "Region 3"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('Florida', 'Georgia', 'Kentucky', 
                           'Mississippi', 'North Carolina', 'South Carolina', 
                           'Tennessee')] <- "Region 4"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('Illinois', 'Indiana', 'Michigan', 
                           'Minnesota', 'Ohio', 'Wisconsin')] <- "Region 5"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('Arkansas', 'Louisiana', 'New Mexico', 
                           'Oklahoma', 'Texas')] <- "Region 6"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('Iowa', 'Kansas', 'Missouri', 'Nebraska')] <- "Region 7"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('Colorado', 'Montana', 'North Dakota', 
                           'South Dakota', 'Utah', 'Wyoming')] <- "Region 8"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('American Samoa','Arizona', 'California', 
                           'Federated States of Micronesia', 'Guam',
                           'Hawaii', 'Nevada', 'Northern Marianas Islands', 
                           'Palau', 'Republic of Marshall Islands')] <- "Region 9"
df_MVPS_jur$HHS_region[df_MVPS_jur$region  %in% 
                         c('Alaska', 'Idaho', 'Oregon', 'Washington')] <- "Region 10"
------------------------------------------

df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 1"] <- 1
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 2"] <- 2
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 3"] <- 3
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 4"] <- 4
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 5"] <- 5
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 6"] <- 6
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 7"] <- 7
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 8"] <- 8
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 9"] <- 9
df_MVPS_jur$Region_value[df_MVPS_jur$HHS_region  == "Region 10"] <- 10



df_MVPS_jur <- df_MVPS_jur %>% 
  filter()



  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )

  #First try-------------------
  fig <- plot_geo(df_MVPS_jur, locationmode = 'USA-states')
  fig <- fig %>% add_trace(text = ~df_MVPS_jur$HHS_region,
    z = ~Region_value, text = ~region, locations = ~labels,
    color = ~Region_value, colors = 'YlGnBu', showscale = F
  ) %>% 
    add_trace(type = "scattergeo", locationmode = 'USA-states',locations = ~labels, 
      text = paste0(df_MVPS_jur$Region_value),
              mode = "text",
              textfont = list(color = rgb(0,0,0), size = 10))
  
  fig <- fig %>% colorbar(title = "Millions USD")
  fig <- fig %>% layout(
    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
    geo = g
  )
  
  fig
#Other try-----------------------
  plot_ly(df_MVPS_jur) %>%
    layout(geo = g) %>%
    add_trace(type = "choropleth", locationmode = 'USA-states',
              locations = ~labels,name = "HHS",
              z = ~Region_value, text = ~region, 
              color = ~Region_value, colors = 'YlGnBu', showscale = F) %>%
    add_trace(type = "scattergeo", locationmode = 'USA-states',
              locations = ~labels, text = paste0(df_MVPS_jur$Region_value, "\n", df_MVPS_jur$labels),
              mode = "text",hoverinfo = "skip",
              textfont = list(color = rgb(0,0,0), size = 10)) %>% 
    layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)'
              ) 
  
  
