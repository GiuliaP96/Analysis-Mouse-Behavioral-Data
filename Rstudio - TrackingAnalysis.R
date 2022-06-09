#Inizialed libraries
library(ggplot2) #plot data
library(sp)  # vector data
library(rgeos)  # geometry ops
library(raster)
library(wesanderson) #colors from movies 
library(viridis) 
library(Paletteer)

#Read .csv file 
file_path <- 'C:/Users/giuli/Desktop/InternshipRichard/DeepLabCut/Training2/2022-05-02-14-06-08-mouse-23ExampleDLC_resnet50_EyeTrackingMay04shuffle1_200000.csv'
data = read.csv(file_path, skip = 3, header = F, sep = ",", as.is = T)
headers= c("index", "tongueX", "tongueY", "tongue", "lightOnX", "lightOnY", "lightOn", "e1X","e1Y", "e1", "e2X","e2Y","e2","e3X","e3Y","e3","e4X","e4Y","e4","e5X", "e5Y", "e5","e6X","e6Y","e6", "e7X","e7Y" ,"e7", "e8X","e8Y","e8", "pCX","pCY","pC", "p1X", "p1Y", "p1", "p2X","p2Y" ,"p2","p3X","p3Y","p3", "p4X","p4Y","p4", "p5X", "p5Y","p5", "p6X","p6Y","p6", "p7X","p7Y","p7", "p8X","p8Y","p8", "p_refX", "p_refY", "p_refP")
colnames(data) <-  headers

#Fix index to have time points in seconds
fps <- 24 
frames <- nrow(data)
video_lenght <- frames/fps
time <- seq(0, video_lenght, length.out=frames) 
time_limit <- video_lenght 

#Define theme settings for all plots
mytheme <- theme(
  plot.title = element_text(family = "Helvetica", face = "bold", size = (30), hjust = 0.5),
  legend.title = element_text(colour = "cornflowerblue", face = "bold.italic", family = "Helvetica",size = (25)),
  legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica", size = 20),
  axis.title = element_text(family = "Helvetica", size = (25)),
  axis.text = element_text(family = "Courier", size = (15)),
  panel.grid.major = element_line(color = "black",
                                  size = 0.5,
                                  linetype = 2)
)

#Extract eyes contour eyes <- data[3:603, 8:31] and pupil pupil <- (data[3:603, 32:58])
p1 <- cbind(data$p1X,data$p1Y) 
p2 <- cbind(data$p2X,data$p2Y) 
p3 <- cbind(data$p3X,data$p3Y) 
p4 <- cbind(data$p4X,data$p4Y) 
p5 <- cbind(data$p5X,data$p5Y) 
p6 <- cbind(data$p6X,data$p6Y) 
p7 <- cbind(data$p7X,data$p7Y) 
p8 <- cbind(data$p8X,data$p8Y) 
pRef <- cbind(data$p_refX, data$p_refY)
pC <- cbind(data$pCX, data$pCY)

#Distance between PRef and pn
distR1 <- pointDistance(p1, pRef, lonlat= FALSE)
distR2 <- pointDistance(p2, pRef, lonlat= FALSE)
distR3 <- pointDistance(p3, pRef, lonlat= FALSE)
distR4 <- pointDistance(p4, pRef, lonlat= FALSE)
distR5 <- pointDistance(p5, pRef, lonlat= FALSE)
distR6 <- pointDistance(p6, pRef, lonlat= FALSE)
distR7 <- pointDistance(p7, pRef, lonlat= FALSE)
distR8 <- pointDistance(p8, pRef, lonlat= FALSE)

#Distance between Pupil Center and all other pupil points
distC1 <- pointDistance(pC, p1, lonlat= FALSE)
distC2 <- pointDistance(pC, p2, lonlat= FALSE)
distC3 <- pointDistance(pC, p3, lonlat= FALSE)
distC4 <- pointDistance(pC, p4, lonlat= FALSE)
distC5 <- pointDistance(pC, p5, lonlat= FALSE)
distC6 <- pointDistance(pC, p6, lonlat= FALSE)
distC7 <- pointDistance(pC, p7, lonlat= FALSE)
distC8 <- pointDistance(pC, p8, lonlat= FALSE)

#Distance between p1 and each other pupil point
dist1 <- pointDistance(p1, p2, lonlat= FALSE)
dist2 <- pointDistance(p1, p3, lonlat= FALSE)
dist3 <- pointDistance(p1, p4, lonlat= FALSE)
dist4 <- pointDistance(p1, p5, lonlat= FALSE)
dist5 <- pointDistance(p1, p6, lonlat= FALSE)
dist6 <- pointDistance(p1, p7, lonlat= FALSE)
dist7 <- pointDistance(p1, p8, lonlat= FALSE)
dist8 <- pointDistance(p1, pC, lonlat= FALSE)

#Distance between p1 and each other pupil point
distT1 <- pointDistance(p1, p2, lonlat= FALSE)
distT2 <- pointDistance(p2, p3, lonlat= FALSE)
distT3 <- pointDistance(p3, p4, lonlat= FALSE)
distT4 <- pointDistance(p4, p5, lonlat= FALSE)
distT5 <- pointDistance(p5, p6, lonlat= FALSE)
distT6 <- pointDistance(p6, p7, lonlat= FALSE)
distT7 <- pointDistance(p7, p8, lonlat= FALSE)

#Create dataframe with distances to calculate mean and plot
pointsR <- data.frame(distR1, distR2, distR3, distR4, distR5, distR6, distR7, distR8)
pointsC <- data.frame(distC1, distC2, distC3, distC4, distC5, distC6, distC7, distC8)
points <- data.frame(dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8)
pointsT <- data.frame(distT1, distT2, distT3, distT4, distT5, distT6, distT7)

Distance_R <- ggplot(data = pointsR, mapping = aes(x = time , y = distance)) +
  geom_line(aes(x = time , y = distR1, color= "P1-Ref")) +
  geom_line(aes(x = time , y = distR2, color= "P2-Ref"))+
  geom_line(aes(x = time , y = distR3, color= "P3-Ref"))+
  geom_line(aes(x = time , y = distR4, color="P4-Ref"))+
  geom_line(aes(x = time , y = distR5, color="P5-Ref"))+
  geom_line(aes(x = time , y = distR6,color="P6-Ref"))+
  geom_line(aes(x = time , y = distR7,color="P7-Ref"))+
  geom_line(aes(x = time , y = distR8, color="P8-Ref"))+
  scale_color_brewer(palette = "PuOr")+
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))

Distance_C <- ggplot(data = pointsC, mapping = aes(x = time , y = distance)) +
  geom_line(aes(x = time , y = distC1, color= "pC-p1")) +
  geom_line(aes(x = time , y = distC2, color= "pC-p2"))+
  geom_line(aes(x = time , y = distC3, color= "pC-p3"))+
  geom_line(aes(x = time , y = distC4, color="pC-p4"))+
  geom_line(aes(x = time , y = distC5, color="pC-p5"))+
  geom_line(aes(x = time , y = distC6,color="pC-p6"))+
  geom_line(aes(x = time , y = distC7,color="pC-p7"))+
  geom_line(aes(x = time , y = distC8, color="pC-p8"))+
  scale_color_viridis(option = "D", discrete = T)+
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))
# scale_color_gradientn(colours = rainbow(5))
 # scale_fill_manual(values = wes_palette("GrandBudapest1", n = 8))

DistancePlot <- ggplot(data = points, mapping = aes(x = time , y = distance)) +
  geom_line(aes(x = time , y = dist1, color= "p1-p2")) +
  geom_line(aes(x = time , y = dist2, color= "p1-p3"))+
  geom_line(aes(x = time , y = dist3, color= "p1-p4"))+
  geom_line(aes(x = time , y = dist4, color="p1-p5"))+
  geom_line(aes(x = time , y = dist5, color="p1-p6"))+
  geom_line(aes(x = time , y = dist6,color="p1-p7"))+
  geom_line(aes(x = time , y = dist7,color="p1-p8"))+
  geom_line(aes(x = time , y = dist8, color="p1-pC"))+
  scale_color_viridis(option = "magma", discrete = T)+
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))

DistancePlotTotal <- ggplot(data = pointsT, mapping = aes(x = time , y = distance)) +
  geom_line(aes(x = time , y = distT1, color= "p1-p2")) +
  geom_line(aes(x = time , y = distT2, color= "p2-p3"))+
  geom_line(aes(x = time , y = distT3, color= "p3-p4"))+
  geom_line(aes(x = time , y = distT4, color="p4-p5"))+
  geom_line(aes(x = time , y = distT5, color="p5-p6"))+
  geom_line(aes(x = time , y = distT6,color="p6-p7"))+
  geom_line(aes(x = time , y = distT7,color="p7-p8"))+
  scale_color_viridis(option = "magma", discrete = T) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))

# Calculate means over rows (so mean of all calculated distances in the same frames)
mean_dist <- (rowMeans(points))
mean_distC <- (rowMeans(pointsC))
mean_distR <- (rowMeans(pointsR))
sum_pupil <- (rowSums(pointsT))

Mean_All <- data.frame(mean_dist, mean_distC,mean_distR)

Mean_Plot <- ggplot(data = Mean_All, mapping = aes(x = time , y = distance)) +
  geom_line(aes(x = time , y = mean_dist, color= "Mean_P1-Pn")) +
  geom_line(aes(x = time , y = mean_distC, color= "Mean_PC-Pn"))+
  geom_line(aes(x = time , y = mean_distR, color= "Mean_Ref-Pn"))+
  geom_line(aes(x = time, y = sum_pupil, color= "Total_dilatation")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", n = 3)) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))

MeanDistance_PRef <- ggplot(data = Mean_All, mapping = aes(x = time , y = distance)) +
  geom_line(aes(x = time, y = sum_pupil)) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))

 # scale_fill_manual(values = wes_palette("Zissou1", n = 3))
Total_Pupil <- ggplot(data = (data.frame(time, sum_pupil)), mapping = aes(x= time, y = sum_pupil, color = sum_pupil)) +
  geom_line(aes(x=time, y=sum_pupil)) +
  geom_point(aes(x=time, y=sum_pupil)) +
  scale_color_viridis(option = "magma", discrete = T) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 100), labels = seq(0, time_limit, by = 100))

##Licking Events
licking <- data.frame(time,data["tongue"])

#Plot likelihood tongue over time
LickingDetectionAccuracy <- ggplot(data=licking, mapping = aes(x = time , y = tongue, color = tongue >0.5)) +
  scale_colour_manual(values= setNames(c("red","green"),c(F,T))) +
  geom_point(alpha=0.6) + 
  geom_hline(yintercept = 0.5) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))

#Replace pvalue over 0.8 with 1 to plot licking events over time
licking$tongue[licking$tongue >= 0.8] <- 1
licking$tongue[licking$tongue < 0.8] <- 0
LickingActivity <- ggplot(data=licking, mapping = aes(x = time , y = tongue)) +
  geom_point(alpha=0.6) +
  geom_line(size=0.1, alpha=0.4) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))

led <- data.frame(time, data["lightOn"])
led$lightOn[led$lightOn >= 0.9] <- 1
led$lightOn[led$lightOn < 0.9] <- 0
LightPlot <- ggplot(data=led , mapping = aes(x = time, y = lightOn)) +
  geom_point(alpha=0.6) + 
  geom_line(mapping = aes(x = time, y = lightOn), size= 0.1) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))
# scale_color_viridis(option = "magma", discrete = T) +

#Print all plots
print(LickingDetectionAccuracy + mytheme + labs(title= "Licking Detection Accuracy", y= "Tongue detected (pvalue)", x= "Time (s)", colour = "PValue > 0.5"))
print(LickingActivity + mytheme + labs(title= "Licking Events", y= "Tongue detected", x= "Time (s)"))
print(LightPlot + mytheme + labs(title= "Light Event", y= "LightOn", x= "Time (s)"))
print(LightPlot + mytheme + labs(title= "LightOn Event", y= "LightOn", x= "Time (s)"))

print(DistancePlot + mytheme + labs(title= "Mean distances P1 - Pn", y= "Distance", x= "Time (s)", colour = "Point"))
print(Distance_C + mytheme + labs(title= "Mean distances pC - Pn", y= "Distance", x= "Time (s)", colour = "Point"))
print(Distance_R + mytheme + labs(title= "Mean distances P_All to P_Ref", y= "Distance", x= "Time (s)", colour = "Point"))
print(Mean_Plot + mytheme + labs(title= "PupilPoints Mean Distances", y= "Distance", x= "Time (s)", colour = "Mean all distances"))
print(Total_Pupil + mytheme + labs(title= "Sum Distances between all points over time", y= "Total lenght pupil", x= "Time (s)"))
print(MeanDistance_PRef  + mytheme + labs(title= "MeanPn-PRef Distances  ", y= "Distance", x= "Time (s)", colour = "Mean distances"))


##Analysis


#apply Min-Max normalization to sum_distance
pupil_norm <- ((sum_pupil - min(sum_pupil)) / (max(sum_pupil) - min(sum_pupil)))
minmaxN_pupil <- data.frame(time,pupil_norm)
MinMaxNorm_PupilDilation <- ggplot(data = minmaxN_pupil, mapping = aes(x= time, y = pupil_norm, color = pupil_norm)) +
  geom_line(aes(x=time, y=pupil_norm)) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))+
  geom_point(aes(x=time, y=pupil_norm)) +
  geom_hline(yintercept = 0.5) +
  scale_color_viridis(option = "magma", discrete = F)

#Z score normalization
pupil_stand <- ((sum_pupil - mean(sum_pupil)) / sd(sum_pupil))
zscoreN_pupil <- data.frame(time, pupil_stand)
ZNorm_PupilDilation <- ggplot(data = zscoreN_pupil, mapping = aes(x = time , y = pupil_stand, color = pupil_stand)) +
  geom_line(aes(x = time , y = pupil_stand)) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 60))+
  geom_hline(yintercept = 0) +
  scale_color_viridis(option = "magma", discrete = F) 
 
print(ZNorm_PupilDilation + mytheme + labs(title= "Normalized pupil dilation (Z-score) ", y= "Normalized pupil dilation", x= "Time (s)", colour="Z score"))
print(MinMaxNorm_PupilDilation + mytheme + labs(title= "Normalized pupil dilation (min-max norm) ", y= "Normalized pupil dilation", x= "Time (s)"))

#rlang::last_error()

#Plot all data, pupil dilatation, licking and led
zscore_norm_Pref <- ((mean_distR - mean(mean_distR)) / sd(mean_distR))
minmax_norm_Pref <- ((mean_distR - min(mean_distR)) / (max(mean_distR) - min(mean_distR)))
save_data <- data.frame(time,zscore_norm_Pref,minmax_norm_Pref, led["lightOn"], licking["tongue"], minmaxN_pupil["pupil_norm"],zscoreN_pupil["pupil_stand"])
Summarizing_Plot <- ggplot(data = save_data) +
  geom_line(aes(x=time, y=pupil_norm , color = "Pupil Dil(min-max norm)"), size = 1) +
  geom_line(aes(x=time, y=minmax_norm_Pref, color = "minmax Pn-Pref"), size = 1) +
 # geom_line(aes(x=time, y=zscore_norm_Pref, color = "zscore Pn-Pref"), size = 1) +
 # geom_line(aes(x=time, y=pupil_stand, color = "Pupil dil(z-score norm)")) +
  geom_line(aes(x=time, y=tongue, color = "Licking activity"), alpha = 1) +
  geom_line(aes(x=time, y=lightOn, color = "LightOn"), alpha =1 ) +
  scale_x_discrete(limits = 0:time_limit, breaks = seq(0, time_limit, by = 2)) +
  scale_color_manual(values=c("mediumspringgreen","slateblue4", "tomato3","springgreen4"))

print(Summarizing_Plot + mytheme + labs(title= "Pupil dilation and licking events", y= "A.U.", x= "Time (s)", colour = ""))


# Heatmap Pupil Ref-pn
library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
pupil_matrix <- as.matrix(data.frame(led["lightOn"], licking["tongue"],minmax_norm_Pref, zscore_norm_Pref, minmaxN_pupil["pupil_norm"], zscoreN_pupil["pupil_stand"])) 
heatmap(pupil_matrix, Colv = NA, Rowv = NA, scale="row")

