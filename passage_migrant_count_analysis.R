# get a list of sampling event identifiers

library(tidyverse)
library(lubridate)
library(stringr)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)
library(extrafont)
library(cowplot)
library(ggpattern)

#pmclists = read.csv("pmc_list_data.csv")
#temp = str_split_fixed(pmclists$url, 'checklist/', 2)
#pmclists$samp.id = temp[,2]
#write.csv(pmclists,"pmc_list_data.csv",row.names = F)


# read checklist data

pmclists_2022 = read.csv("pmc_list_data_2022.csv")
pmclists_2023 = read.csv("pmc_list_data_2023.csv")



# read eBird data and start analysis 2022

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE.CODE","COUNTY.CODE","EXOTIC.CODE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN-GJ-KA_202209_202209_relOct-2022.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim("ebd_IN-GJ-KA_202209_202209_relOct-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

## choosing important columns required for further analyses

data = data %>% filter(OBSERVATION.DATE %in% c("2022-09-10","2022-09-11"))

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data = data %>%
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month],
         daym = day(OBSERVATION.DATE),
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear))

data_2022 = data



# read eBird data and start analysis 2023

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE.CODE","COUNTY.CODE","EXOTIC.CODE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN-GJ-KA_202309_202309_relDec-2023.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim("ebd_IN-GJ-KA_202309_202309_relDec-2023.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ","null",NA))

## choosing important columns required for further analyses

data = data %>% filter(OBSERVATION.DATE %in% c("2023-09-23","2023-09-24"))

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data = data %>%
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month],
         daym = day(OBSERVATION.DATE),
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear))

data_2023 = data


# combine data

data = data_2022 %>% bind_rows(data_2023)



##### add map data

ksdi = readOGR("Kutch_SD","Kutch_SD")


# single object at group ID level (same group ID, same grid/district/state)
temp0 = data %>% group_by(group.id) %>% slice(1) 

### add columns with GRID ATTRIBUTES to main data

temp = temp0 # separate object to prevent repeated slicing (intensive step)

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

temp = sp::over(temp, ksdi) %>% # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
  dplyr::select(area_name) %>% 
  rename(SUB.DISTRICT = area_name) %>% 
  rownames_to_column("group.id") 

data = left_join(temp, data)
data = data %>% filter(!is.na(SUB.DISTRICT))



##### plot and view districts

sdmap = gSimplify(ksdi, tol=0.01, topologyPreserve=TRUE)
d1 = ksdi@data
sdmap = sp::SpatialPolygonsDataFrame(sdmap, d1)
finalmap = gBuffer(sdmap, byid=TRUE, width=0)

proj4string(finalmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(finalmap, zcol = NULL, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = NULL, 
            popup = leafpop::popupTable(finalmap,c("area_name"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE), 
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "Kachchh_subdists.html")




##### select only systematic data

data_sys = data %>% filter(SAMPLING.EVENT.IDENTIFIER %in% pmclists_2022$SAMPLING.EVENT.IDENTIFIER |
                             SAMPLING.EVENT.IDENTIFIER %in% pmclists_2023$SAMPLING.EVENT.IDENTIFIER)

data_sys = data_sys %>% filter(OBSERVATION.COUNT != 'X')
data_sys$OBSERVATION.COUNT = as.numeric(data_sys$OBSERVATION.COUNT)

##### combine appropriate sub-districts

samp_size = data_sys %>%
  group_by(year,SUB.DISTRICT) %>% reframe(size = n_distinct(group.id))



# combine accordingly

ksdi@data$grp = ksdi@data$area_name
ksdi@data$grp[ksdi@data$area_name %in% c("Kalo Dungar","Banni")] = "Banni and Kalo Dungar"
ksdi@data$grp[ksdi@data$area_name %in% c("Mandvi","Mundra")] = "Mandvi and Mundra"
ksdi@data$grp[ksdi@data$area_name %in% c("Anjar","Gandhidham","Bhachau")] = "Anjar, Gandhidham and Bhachau"


pol = st_as_sf(ksdi)
pol = pol %>% 
  group_by(grp) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  st_transform(3857) %>% 
  sf::st_buffer(5) %>%
  st_transform(4326)
#pol = pol[-5,]
pol = as(pol, 'Spatial')

temp0 = data %>% group_by(group.id) %>% slice(1) 

### add columns with GRID ATTRIBUTES to main data

temp = temp0 # separate object to prevent repeated slicing (intensive step)

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

temp = sp::over(temp, pol) %>% # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
  dplyr::select(1) %>% 
  rename(grp = grp) %>% 
  rownames_to_column("group.id") 

data = left_join(temp, data)
data$SUB.DISTRICT[data$SUB.DISTRICT == "Little Rann of Kachchh"] = "Rapar"
data$grp[data$grp == "Little Rann of Kachchh"] = "Rapar"
data$grp[data$grp == "Great Rann of Kachchh"] = "Banni and Kalo Dungar"
ksdi_extra = ksdi[ksdi@data$grp %in% c("Little Rann of Kachchh","Great Rann of Kachchh"),]
ksdi_full = ksdi
ksdi = ksdi[!ksdi@data$grp %in% c("Little Rann of Kachchh","Great Rann of Kachchh"),]
ksdi_extra = st_as_sf(ksdi_extra)
ksdi_extra = ksdi_extra %>%
  st_transform(4326)


pol = st_as_sf(ksdi)
pol = pol %>% 
  group_by(grp) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  st_transform(3857) %>% 
  sf::st_buffer(5) %>%
  st_transform(4326)
#pol = pol[-5,]
#pol = as(pol, 'Spatial')
pol$area = st_area(pol)


##### select only systematic data

data_sys = data %>% filter(SAMPLING.EVENT.IDENTIFIER %in% pmclists_2022$SAMPLING.EVENT.IDENTIFIER |
                             SAMPLING.EVENT.IDENTIFIER %in% pmclists_2023$SAMPLING.EVENT.IDENTIFIER)

data_sys = data_sys %>% filter(OBSERVATION.COUNT != 'X')
data_sys$OBSERVATION.COUNT = as.numeric(data_sys$OBSERVATION.COUNT)

conv.factor = data_sys %>%
  filter(!is.na(EFFORT.DISTANCE.KM)) %>%
  group_by(group.id) %>% slice(1) %>%
  group_by(year) %>% reframe(dist = mean(EFFORT.DISTANCE.KM),
                             dur = mean(DURATION.MINUTES)) %>%
  mutate(conv.factor = 60/dur) %>%
  select(year,conv.factor)

dist.factor = data_sys %>%
  filter(!is.na(EFFORT.DISTANCE.KM)) %>%
  group_by(group.id) %>% slice(1) %>%
  group_by(year) %>% reframe(dist = mean(EFFORT.DISTANCE.KM),
                             dur = mean(DURATION.MINUTES)) %>%
  mutate(dist.factor = 1/(dist*0.1)) %>%
  select(year,dist.factor)


# check sample size

samp_size = data_sys %>%
  group_by(year,grp) %>% reframe(size = n_distinct(group.id))





###### Plot point distributions with the full data

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
                  "#CC6666", "#9999CC", "#000000", "#66CC99")
                  
ns = 6

cols1 = cols[c(1:ns)]

list_passage = c("European Roller","Spotted Flycatcher","Greater Whitethroat","Red-backed Shrike",
                 "Blue-cheeked Bee-eater","Rufous-tailed Scrub-Robin")

data$COMMON.NAME = factor(data$COMMON.NAME, levels=list_passage)

dataf = data %>% filter(COMMON.NAME %in% list_passage)

ggp = ggplot() +
  facet_wrap(.~COMMON.NAME+year) +
  geom_polygon(data = ksdi_full, aes(x=long, y=lat, group=group), colour = "black", fill = "white") +  
  geom_point(data = dataf, aes(x=LONGITUDE,y=LATITUDE,col=COMMON.NAME),size = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_colour_manual(values = cols1)+
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1)) +
  theme(strip.text.x = element_text(size = 10))+
  coord_map()


n1 = "species point locations.jpg"

print(ggp)
ggsave(file=n1, units="in", width=14, height=10, bg = "white")


ggp = ggplot() +
  geom_polygon(data = ksdi_full, aes(x=long, y=lat, group=group), colour = "black", fill = "white")+  
  geom_point(data = data_sys, aes(x=LONGITUDE,y=LATITUDE,col=as.factor(year)), size = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  coord_map()

n1 = "Sampling locations.jpg"

print(ggp)
ggsave(file=n1, units="in", width=10, height=6, bg = "white")











### Calculate important summaries
# sample size, species richness, passage migrant count per list, Roller count per list and frequency
# Flycatcher, Blue-cheeked Bee-eater, Whitethroat, Red-backed Shrike, Scrub-Robin

list_passage = c("European Roller","Spotted Flycatcher","Greater Whitethroat","Red-backed Shrike",
                 "Blue-cheeked Bee-eater","Rufous-tailed Scrub-Robin")

summaries = data_sys %>%
  group_by(grp,year) %>% mutate(size = n_distinct(group.id)) %>% ungroup()
                              

euro_sum = summaries %>%
  filter(COMMON.NAME == "European Roller") %>%
  group_by(grp,year) %>% reframe(count = round(sum(OBSERVATION.COUNT)/max(size),2),
                              freq = round(n()/max(size),2)) %>%
  mutate(species = "European Roller")

spfl_sum = summaries %>%
  filter(COMMON.NAME == "Spotted Flycatcher") %>%
  group_by(grp,year) %>% reframe(count = round(sum(OBSERVATION.COUNT)/max(size),3),
                              freq = round(n()/max(size),3)) %>%
  mutate(species = "Spotted Flycatcher")

grwh_sum = summaries %>%
  filter(COMMON.NAME == "Greater Whitethroat") %>%
  group_by(grp,year) %>% reframe(count = round(sum(OBSERVATION.COUNT)/max(size),3),
                              freq = round(n()/max(size),3)) %>%
  mutate(species = "Greater Whitethroat")

rbsh_sum = summaries %>%
  filter(COMMON.NAME == "Red-backed Shrike") %>%
  group_by(grp,year) %>% reframe(count = round(sum(OBSERVATION.COUNT)/max(size),3),
                              freq = round(n()/max(size),3)) %>%
  mutate(species = "Red-backed Shrike")

bcbe_sum = summaries %>%
  filter(COMMON.NAME == "Blue-cheeked Bee-eater") %>%
  group_by(grp,year) %>% reframe(count = round(sum(OBSERVATION.COUNT)/max(size),3),
                              freq = round(n()/max(size),3)) %>%
  mutate(species = "Blue-cheeked Bee-eater")

rtsr_sum = summaries %>%
  filter(COMMON.NAME == "Rufous-tailed Scrub-Robin") %>%
  group_by(grp,year) %>% reframe(count = round(sum(OBSERVATION.COUNT)/max(size),3),
                              freq = round(n()/max(size),3)) %>%
  mutate(species = "Rufous-tailed Scrub-Robin")

spec_sum = rbind(euro_sum,spfl_sum,grwh_sum,rbsh_sum,bcbe_sum,rtsr_sum)

samp_size = summaries %>%
  group_by(grp,year) %>% reframe(samp_size = max(size))


base = left_join(pol,samp_size)
base$samp_size[is.na(base$samp_size)] = 0

basef = data.frame(grp = rep(base$grp,6), year = rep(base$year,6), 
                   samp_size = rep(base$samp_size,6), 
                   species = rep(list_passage,  each = length(base$grp)))

summaries_final = left_join(basef,spec_sum)
summaries_final[is.na(summaries_final)] = 0

summaries_final$species = factor(summaries_final$species, levels=list_passage)


kmap = pol
kmap_birds = left_join(kmap,summaries_final)
kmap_birds = kmap_birds %>% left_join(dist.factor) %>%
  mutate(count = count*dist.factor)

require(mltools)
n1 = list()
n2 = list()
pmc_count_map = list()
pmc_freq_map = list()

### plot species counts
c=0
for (i in list_passage)
{
  c=c+1
  name = i
  temp = kmap_birds %>% filter(species == name)
  temp$count1 = mltools::bin_data(temp$count, bins=5, binType = "quantile")
  
  sm = temp %>%
    filter(!is.na(count)) %>%
    group_by(count1) %>% reframe(min = round(min(count),2),max = round(max(count),2))
  
  l = length(sm$count1)
  vals = c("#daeafa","#99CCFF","#6699CC","#336699","#003399")
  
  temp1 = temp %>% filter(count!=0 | is.na(count))
  temp2 = temp %>% filter(count==0)
  
  
  pmc_count_map[[c]] = ggplot(data = temp) +
    facet_wrap(.~year) +
    geom_sf(data = temp1, aes(fill=count1), colour = "black")+  
    geom_sf(data = temp2, fill = "white", colour = "black")+
    geom_sf_pattern(data = ksdi_extra, pattern = "stripe", pattern_density = 0.5,
                    pattern_size = 0.05, alpha = 0.3)+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(text=element_text(family="Gill Sans MT")) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank())+
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$count1, labels = sm$count1,
                                 name = "count per sq. km.")} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$count1, 
                                 labels = c(paste("<=",sm$max[1]),paste(sm$min[2],
                                                                        " - ",sm$max[2]), paste(">",sm$min[3])),
                                 name = "count per sq. km.")} +
    {if(l == 4)scale_fill_manual(values = vals,breaks = sm$count1, 
                                 labels = c(paste("<=",sm$max[1]),
                                            paste(sm$min[2]," - ",sm$max[2]), paste(sm$min[3]," - ",sm$max[3]),
                                            paste(">",sm$min[4])),
                                 name = "count per sq. km.")} +
    {if(l > 4)scale_fill_manual(values = vals,breaks = sm$count1, 
                                labels = c(paste("<=",sm$max[1]),
                                           paste(sm$min[2]," - ",sm$max[2]), paste(sm$min[3]," - ",sm$max[3]),
                                           paste(sm$min[4]," - ",sm$max[4]),
                                           paste(">",sm$min[5])),
                                name = "count per sq. km.")} +
    theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12),
          legend.position = "bottom")  +
    guides(fill = guide_legend(nrow = 1)) +
    theme(strip.text.x = element_text(size = 20))
  
  
  
  n1[[c]] = paste(name,".jpeg",sep="")
  
}


### plot species counts
c=0
for (i in list_passage)
{
  c = c+1
  name = i
  temp = kmap_birds %>% filter(species == name)
  temp$count1 = mltools::bin_data(temp$freq, bins=5, binType = "quantile")
  
  sm = temp %>%
    filter(!is.na(freq)) %>%
    group_by(count1) %>% reframe(min = round(min(freq),2),max = round(max(freq),2))
  
  l = length(sm$count1)
  vals = c("#daeafa","#99CCFF","#6699CC","#336699","#003399")
  
  temp1 = temp %>% filter(freq!=0 | is.na(freq))
  temp2 = temp %>% filter(freq==0)
  
  
  pmc_freq_map[[c]] = ggplot(data = temp) +
    facet_wrap(.~year) +
    geom_sf(data = temp1, aes(fill=count1), colour = "black")+  
    geom_sf(data = temp2, fill = "white", colour = "black")+  
    geom_sf_pattern(data = ksdi_extra, pattern = "stripe", pattern_density = 0.5,
                    pattern_size = 0.05, alpha = 0.3)+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(text=element_text(family="Gill Sans MT")) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank())+
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$count1, labels = sm$count1,
                                 name = "frequency of reporting")} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$count1, 
                                 labels = c(paste("<=",sm$max[1]),paste(sm$min[2],
                                                                        " - ",sm$max[2]), paste(">",sm$min[3])),
                                 name = "frequency of reporting")} +
    {if(l == 4)scale_fill_manual(values = vals,breaks = sm$count1, 
                                 labels = c(paste("<=",sm$max[1]),
                                            paste(sm$min[2]," - ",sm$max[2]), paste(sm$min[3]," - ",sm$max[3]),
                                            paste(">",sm$min[4])),
                                 name = "frequency of reporting")} +
    {if(l > 4)scale_fill_manual(values = vals,breaks = sm$count1, 
                                labels = c(paste("<=",sm$max[1]),
                                           paste(sm$min[2]," - ",sm$max[2]), paste(sm$min[3]," - ",sm$max[3]),
                                           paste(sm$min[4]," - ",sm$max[4]),
                                           paste(">",sm$min[5])),
                                name = "frequency of reporting")} +
    theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12),
          legend.position = "bottom")  +
    guides(fill = guide_legend(nrow = 1)) +
    theme(strip.text.x = element_blank())
  
  
  n2[[c]] = paste(name,".jpeg",sep="")

}

for (i in 1:6)
{
  plot_grid(pmc_count_map[[i]],pmc_freq_map[[i]],nrow=2,ncol=1,align='h')
  ggsave(file=n1[[i]], units="in", width=14, height=10.5)
}






##### plot and view districts

finalmap = st_as_sf(ksdi_full)
cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")
cols1 = cols[1:length(finalmap$grp)]

mapviewOptions(fgb = FALSE)
a = mapView(finalmap, zcol = NULL, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = NULL, 
            popup = leafpop::popupTable(finalmap,c("grp"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE), 
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "Kachchh_regions.html")

regions_map = ggplot() +
  geom_sf(data = finalmap, aes(fill=grp), colour = "black")+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank()) +
  scale_fill_manual(values = cols1) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 16),
        legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 3)) +
  theme(strip.text.x = element_blank())


n1 = paste("Kachchh_regions_map.jpg",sep="")

print(regions_map)
ggsave(file=n1, units="in", width=10, height=6)






######### Most common species during the count

pol_area = pol %>% select(grp,area) %>% mutate(total.area = sum(area)) %>%
  mutate(prop.area = as.numeric(area/total.area))

list_passage1 = c(list_passage,"Common Cuckoo","Eurasian Nightjar")

temp = expand.grid(year = unique(data_sys$year),
                   grp = unique(data_sys$grp),
                   COMMON.NAME = unique(data_sys[data_sys$CATEGORY %in% c("species","issf") & is.na(data_sys$EXOTIC.CODE),]$COMMON.NAME))

gids = data_sys %>% distinct(year,grp,group.id)
temp = temp %>% left_join(gids)

data_count_species = temp %>% left_join(data_sys) %>%
  mutate(OBSERVATION.COUNT = case_when(is.na(OBSERVATION.COUNT)~0,TRUE~OBSERVATION.COUNT)) %>%
  group_by(year,grp,COMMON.NAME) %>% reframe(count0 = mean(OBSERVATION.COUNT),
                                      countsd = sd(OBSERVATION.COUNT))

data_count_species = data_count_species %>%
  left_join(samp_size) %>%
  group_by(year,grp,COMMON.NAME) %>% mutate(count.se0 = countsd/sqrt(samp_size)) %>% ungroup %>%
  mutate(count.se0 = case_when(is.na(count0)~0,TRUE~count.se0)) %>%
  mutate(count0 = case_when(is.na(count0)~0,TRUE~count0)) %>%
  left_join(dist.factor) %>%
  mutate(count = count0*dist.factor,
         count.se = count.se0*dist.factor) %>%
  arrange(desc(year),desc(grp),desc(COMMON.NAME))

data_frequency_species = data_sys %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(year,grp) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(year,grp,COMMON.NAME) %>% reframe(freq0 = n_distinct(group.id)/max(lists))

data_frequency_species = temp %>% left_join(data_frequency_species) %>%
  left_join(pol_area) %>%
  mutate(freq0 = case_when(is.na(freq0)~0,TRUE~freq0)) %>%
  left_join(samp_size) %>%
  group_by(year,grp,COMMON.NAME) %>% mutate(freq.se0 = sqrt(freq0*(1-freq0)/samp_size)/sqrt(samp_size)) %>% ungroup() %>%
  mutate(freq.se0 = case_when(is.na(freq0)~0,TRUE~freq.se0)) %>%
  mutate(freq0 = case_when(is.na(freq0)~0,TRUE~freq0)) %>%
  group_by(year,COMMON.NAME) %>% reframe(freq = sum(freq0*prop.area),
                                         freq.se = sqrt(sum(prop.area*(freq.se0^2)))) %>%
  arrange(desc(year),desc(freq)) %>%
  group_by(year) %>% slice(1:10)


data_count_species_extrapolated = data_count_species %>%
  filter(COMMON.NAME %in% list_passage1) %>%
  left_join(pol_area) %>%
  mutate(count.temp = as.numeric(count*area)/10^6,
         se.temp = as.numeric(count.se*area)/10^6) %>%
  group_by(year,COMMON.NAME) %>% mutate(n = n()) %>% ungroup %>%
  group_by(year,COMMON.NAME) %>%  reframe(count = sum(count.temp),
                              se = sqrt(sum(se.temp^2)))


data_count_species_extrapolated$cil = data_count_species_extrapolated$count-1.96*data_count_species_extrapolated$se
data_count_species_extrapolated$cil[data_count_species_extrapolated$cil<0] = 0
data_count_species_extrapolated$cir = data_count_species_extrapolated$count+1.96*data_count_species_extrapolated$se

## plotting

pd = position_dodge(0.2)

ggp = ggplot(data_count_species_extrapolated %>% filter(!COMMON.NAME %in% c("Blue-cheeked Bee-eater","Eurasian Nightjar")), 
             aes(x=COMMON.NAME, y=count, col = factor(year))) + 
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), linewidth = 0.3, width = 0.1, position = pd) +
  xlab("Species") +
  ylab("Estimated numbers passing through Kachchh during the count")

ggp1 = ggp +
  scale_x_discrete(breaks = c("European Roller","Spotted Flycatcher","Greater Whitethroat","Red-backed Shrike",
                              "Common Cuckoo","Rufous-tailed Scrub-Robin"),
                   labels = c("European\nRoller","Spotted\nFlycatcher","Greater\nWhitethroat","Red-backed\nShrike",
                              "Common\nCuckoo","Rufous-tailed\nScrub-Robin")) +
  scale_y_continuous(breaks = seq(0,400000,50000), labels = c("0","50,000","100,000",
                                                              "150,000","200,000","250,000",
                                                              "300,000","350,000","400,000")) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14, margin = margin(r = 0)),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 16, vjust = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0.5), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  theme(strip.text.x = element_text(size = 15))+
  guides(fill = guide_legend(nrow = 1))



n1 = paste("individual_species_passage_extrapolated.jpg",sep="")

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)




### combined analyses

temp = expand.grid(year = unique(data_sys$year),
                   grp = unique(data_sys$grp),
                   COMMON.NAME = c(unique(data_sys[data_sys$CATEGORY %in% c("species","issf") & is.na(data_sys$EXOTIC.CODE),]$COMMON.NAME),"Passage Migrant"))

gids = data_sys %>% distinct(year,grp,group.id)
temp = temp %>% left_join(gids)

list_passage2 = list_passage1[list_passage1 != "Blue-cheeked Bee-eater"]
data_count_combined = data_sys
data_count_combined$comb = data_count_combined$COMMON.NAME
data_count_combined$comb[data_count_combined$COMMON.NAME %in% list_passage2] = "Passage Migrant"
data_count_combined = data_count_combined %>%
  select(-COMMON.NAME) %>%
  rename(COMMON.NAME = comb) %>%
  group_by(year,grp,group.id,COMMON.NAME) %>% reframe(OBSERVATION.COUNT = sum(OBSERVATION.COUNT))

data_count_combined = temp %>% left_join(data_count_combined) %>%
  mutate(OBSERVATION.COUNT = case_when(is.na(OBSERVATION.COUNT)~0,TRUE~OBSERVATION.COUNT)) %>%
  group_by(year,grp,COMMON.NAME) %>% reframe(count0 = mean(OBSERVATION.COUNT),
                                             countsd = sd(OBSERVATION.COUNT))

data_count_combined = data_count_combined %>%
  left_join(samp_size) %>%
  group_by(year,grp,COMMON.NAME) %>% mutate(count.se0 = countsd/sqrt(samp_size)) %>% ungroup %>%
  mutate(count.se0 = case_when(is.na(count0)~0,TRUE~count.se0)) %>%
  mutate(count0 = case_when(is.na(count0)~0,TRUE~count0)) %>%
  left_join(dist.factor) %>%
  mutate(count = count0*dist.factor,
         count.se = count.se0*dist.factor) %>%
  arrange(desc(year),desc(grp),desc(COMMON.NAME)) %>%
  filter(COMMON.NAME %in% "Passage Migrant")

# richness

data_rich_passage = data_sys %>%
  filter(COMMON.NAME %in% list_passage1) %>%
  group_by(grp,year) %>% reframe(rich = n_distinct(COMMON.NAME)) %>%
  arrange(desc(year),desc(rich))


data_count_combined$cil = data_count_combined$count-1.96*data_count_combined$count.se
data_count_combined$cil[data_count_combined$cil<0] = 0
data_count_combined$cir = data_count_combined$count+1.96*data_count_combined$count.se

## plotting

pd = position_dodge(0.2)

ggp = ggplot(data_count_combined, 
             aes(x=grp, y=count, col = factor(year))) + 
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), linewidth = 0.3, width = 0.1, position = pd) +
  xlab("Region") +
  ylab("Count of passage migrants per sq. km.")

ggp1 = ggp +
  scale_x_discrete(breaks = c("Lakhpat","Abdasa","Nakhatrana","Mandvi and Mundra","Bhuj",
                              "Banni and Kalo Dungar","Khadir","Anjar, Gandhidham and Bhachau","Rapar"),
                   labels = c("Lakhpat","Abdasa","Nakhatrana","Mandvi and\nMundra","Bhuj",
                              "Banni and\nKalo Dungar","Khadir","Anjar, Gandhidham\nand Bhachau","Rapar")) +
  scale_y_continuous(breaks = seq(5,90,5)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14, margin = margin(r = -15)),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 16, vjust = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0.5), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1))



n1 = paste("districts_passage.jpg",sep="")

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)

count_extrapolate = data_count_species_extrapolated %>%
  filter(COMMON.NAME  != "Blue-cheeked Bee-eater") %>%
  group_by(year) %>% mutate(n = n()) %>% ungroup %>%
  group_by(year) %>% reframe(count.extrapolate = sum(count),
                              se.extrapolate = sqrt(sum(se^2)))

  
# for table

table_1 = data_count_species_extrapolated %>%
  arrange(year,desc(count)) %>%
  filter(!COMMON.NAME %in% c("Blue-cheeked Bee-eater","Eurasian Nightjar"))

write.csv(table_1,"table_1.csv",row.names = F)