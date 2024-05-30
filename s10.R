#Load Spatial Data and Contiguity in RStudio
#Melakukan pengaturan terhadap Working Directory
getwd()

#Menambahkan Library yang akan digunakan
library(tidyverse)
library(sp)
library(spData)
library(spdep)
library(spatialreg)
library(rgdal)
library(rgeos)

#Menambahkan data spasial
java = readOGR(dsn = ".", layer = "java")

#Plot data spasial
plot(java)

#Melihat attribute table
java@data

#Cek data
glimpse(java)

#Queen Contiguity
java_qnb = poly2nb(java, row.names = java$kab)
print(java_qnb)

#Rook Contiguity
java_rnb = poly2nb(java, row.names = java$kab, queen = FALSE)
print(java_rnb)

#K Nearest Neighbour
java.coord = cbind(java@data$POINT_X, java@data$POINT_Y)
knn.5 = knearneigh(java.coord, k=5, longlat = TRUE)
knn.5.nb = knn2nb(knn.5, row.names = java$kab)
knn.5.nb



#Create Weight Matrix and Neighbourhood List
#Membuat weight matrix and nb list Queen Contiguity
mat_qnb = nb2mat(java_qnb, style = "W")
print(mat_qnb)
list_qnb = nb2listw(java_qnb, style = "W")
print(list_qnb)

#Membuat weight matrix and nb list Rook Contiguity
mat_rnb = nb2mat(java_rnb, style = "W")
print(mat_rnb)
list_rnb = nb2listw(java_rnb, style = "W")
print(list_rnb)



#Neighbourhood Visualisation
#Memperoleh koordinat XY
java.coord = cbind(java@data$POINT_X, java@data$POINT_Y)

#Melakukan Visualisasi pada hasil
#Queen
plot(java_qnb, java.coord)
#Rook
plot(java_rnb, java.coord)
#KNN
plot(knn.5.nb, java.coord)

#Melakukan komparasi antara queen contiguity dengan KNN
plot(diffnb(knn.5.nb,java_qnb), java.coord, add=TRUE, col="red", lty=2)



#Global Moran’s I Test
#Global moran test qnb
#Queen
moranqnb = moran.test(java$crime_2019, list_qnb)
print(moranqnb)
#Rook
moranrnb = moran.test(java$crime_2019, list_rnb)
print(moranrnb)
#KNN
moranknn5 = moran.test(java$crime_2019, nb2listw(knn.5.nb))
print(moranknn5)

#Plot hasil
#Queen
moran.plot(java$crime_2019,list_qnb,labels = TRUE)
#Rook
moran.plot(java$crime_2019,list_rnb,labels = TRUE)
#KNN
moran.plot(java$crime_2019, nb2listw(knn.5.nb),labels = TRUE)



#Local Moran’s I Test
#local moran test qnb
lmoranqnb = localmoran(java$crime_2019,list_qnb)
View(lmoranqnb)

#local moran test rnb
lmoranrnb = localmoran(java$crime_2019,list_rnb)
View(lmoranrnb)

#local moran test knn5.nb
lmoranknn.nb = localmoran(java$crime_2019,nb2listw(knn.5.nb))
View(lmoranknn.nb)

#Menambahkan library 
library(tmap)

#binds local moran’s I results to polygon shapefil
lmoranqnb.map = cbind(java, lmoranqnb)

#Melakukan visualisasi
tm_shape(lmoranqnb.map) + 
  tm_fill(col = "Ii", 
          style = "quantile", 
          title = "Moran’s I of Crime in Jawa Provinces 2019")

#create quadrant
quadrant = vector(mode="numeric",length=nrow(lmoranqnb))

#compute the centers the variable of interest around its mean
m.crime19 = java$crime_2019 - mean(java$crime_2019)

#centers the local Moran's around the mean
m.local = lmoranqnb[,1] - mean(lmoranqnb[,1])

#significance threshold
signif = 0.1

#builds a data quadrant
quadrant[m.crime19 >0 & m.local>0] <- 4
quadrant[m.crime19 <0 & m.local<0] <- 1
quadrant[m.crime19 <0 & m.local>0] <- 2
quadrant[m.crime19 >0 & m.local<0] <- 3
quadrant[lmoranqnb[,5]>signif] <- 0

#plot in r
brks = c(0,1,2,3,4)

colors = c("white","blue",rgb(0,0,1,alpha=0.4), rgb(1,0,0,alpha=0.4), "red")

plot(java,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()

legend("bottomleft", legend = c("not significant","low-low","low-high","high-low","high-high"), fill=colors,bty="n")



#Getis-Ord Gi
#Creates centroid and joins Neighbor based on distance (0-60km)
getis.nb = dnearneigh(coordinates(java), 0, 60000)

#create listw
getis.nb_lw = nb2listw(getis.nb, style = 'B')

#plot data and neighbour
plot(java, border = 'black')
plot(getis.nb, coordinates(java), add=TRUE, col = 'red')

#Calculate Getis-Ord Gi* Statistics
local_g = localG(java$crime_2019, getis.nb_lw)

#combine the gstat result with java polygons
local_g = cbind(java, as.matrix(local_g))
View(local_g@data)

#change the column name of gstat result into “gstat”
names(local_g)[10] <- "gstat"
View(local_g@data)

#define minimum z-score
min(local_g$gstat)
#define maximum z-score
max(local_g$gstat)
cuts = c( -3, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, 5)

#Plot with tmap
tm_shape(local_g) +
  tm_fill("gstat",
          breaks = cuts,
          palette= "-RdBu") +
  tm_borders(alpha = 1)

# Add Title and Legend Label
tm_shape(local_g) +
  tm_fill("gstat",
          breaks = cuts,
          palette= "-RdBu",
          title = "Getis Ord Gi* (z-score)",
          label = c("Cold Spot (99%)","Cold Spot (95%)","Cold Spot (90%)","Not Significant","Hot Spot (90%)", "Hot Spot (95%)", "Hot Spot (99%)")) +
  tm_borders(alpha = 1) +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08),
            title = "Crime Cases in Java, 2019",
            title.size = 1.1,
            title.position = c("center", "top"))

#add North Arrow and Scale Bar
tm_shape(local_g) +
  tm_fill("gstat",
          breaks = cuts,
          palette= "-RdBu",
          title = "Getis Ord Gi* (z-score)",
          label = c("Cold Spot (99%)","Cold Spot (95%)","Cold Spot (90%)","Not Significant","Hot Spot (90%)", "Hot Spot (95%)", "Hot Spot (99%)")) +
  tm_borders(alpha = 1) +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Crime Cases in Java, 2019",
            title.size = 1.1,
            title.position = c("center", "top"),
            inner.margins = c(0.06, 0.10, 0.10, 0.08))+
  tm_scale_bar(position = c("center", "bottom")) +
  tm_compass(position = c("right", "top"))