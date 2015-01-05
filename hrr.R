library("ggplot2")
library("Hmisc")
library("demogromatics")
library("rgeos")

process.hrr.shapefiles = function(folder, shapefile){
  hrr = readOGR(dsn = folder, layer = shapefile)
  hrr@data$id = rownames(hrr@data)
  hrr = gBuffer(hrr, width=0, byid=TRUE)
  hrr.points = fortify(hrr, region = "id")
  hrr.df = join(hrr.points, hrr@data, by = "id")
  hrr.df
}

hrr.boundaries = process.hrr.shapefiles(folder = "/Users/nancyorgan/Desktop/hrr_bdry",
                                        shapefile = "HRR_Bdry")
hrr.boundaries = subset(hrr.boundaries, select = c(long, lat, group, HRRNUM))
names(hrr.boundaries)[4] = "HRRCODE"

hrr.data = read.csv("/Users/nancyorgan/Desktop/heat_map_file.csv", header = TRUE)
hrr.data = hrr.data[!is.na(hrr.data$HRRCODE),]

means = NULL
for(i in 1:length(unique(hrr.data$HRRCODE))){
  means[i] = mean(hrr.data$rsrr[hrr.data$HRRCODE == unique(hrr.data$HRRCODE)[i]])
}
length(means)

pt.means = data.frame(cbind(means, unique(hrr.data$HRRCODE)))
names(pt.means) = c("mean", "HRRCODE")
hrr.data = hrr.data[!is.na(hrr.data$HRRCODE),]

######### KEEP THIS #####
#choropleth = join(hrr.data, hrr.boundaries)
###
choropleth = join(pt.means, hrr.boundaries)
choropleth$rsrr = as.numeric(choropleth$rsrr)
choropleth$mean = as.numeric(choropleth$mean)

choropleth = choropleth[!is.na(choropleth$lat),]
choropleth = choropleth[!is.na(choropleth$long),]


test = choropleth[choropleth$group %in% unique(choropleth$group)[1:10],]
dim(test)

myplot = ggplot(choropleth, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mean)) + 
  coord_map() +
  scale_fill_continuous(low = "darkseagreen1", high = "black", limits = c(min(choropleth$mean),max = 0.39))
myplot

