setwd("C:/Users/alfie/OneDrive/Documents/University/Masters/Semester2/Research Project/Datasets")


#Libraries
library(readxl)
library(tidyr)
library(ggplot2)
library(vegan)
library(pheatmap)
library(reshape2)

####################################################################################
##Line Graphs

#Read the Excel File File into R
WOstracods <- read_excel("Fossil Quantity Dataset.xlsx")

# Converts Data into long format 
ld <- pivot_longer(WOstracods, 
                          cols = -Group, 
                          names_to = "Bed", 
                          values_to = "Quantity")

#Plotting the three-way line graph 
ggplot(ld, aes(x = Bed, y = Quantity, color = Group, group = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Fossil Taxa Quantities by Sample Bed",
       x = "Bed",
       y = "Quantity",
       color = "Fossil Group") +
  theme_minimal() +
  #logs data to account for outliers with higher quantities
  scale_y_sqrt() +
  theme(plot.title = element_text(hjust = 0.5)) 

###############################################################################

##Cleaning the Data for the Bray-Curtis Analysis
rownames(WOstracods) <- WOstracods$Group
WOstracodsData <- WOstracods[ , -1]
WOstracodsT <- t(WOstracodsData)

#Performing the Bray-Curtis using the Vegan package (vegdist)
dist <- vegdist(WOstracodsT, method = "bray")
bray <- as.matrix(dist)

#Bray Curtis Dissimilarity Results
as.matrix(dist)


# Plotting heatmap
pheatmap(bray, 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean",
         color = colorRampPalette(c("white", "yellow", "red"))(100),
         main = "Heatmap for Bray-Curtis Sample Dissimilarity")


###########################################################

##Stacked Bar Chart

#Converting the data into long format
ld2 <- melt(WOstracods, id.vars = "Group", variable.name = "Sample", value.name = "Count")

#Plotting the stacked bar chart
ggplot(ld2, aes(x = Sample, y = Count, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Faunal Abundance by Sample", y = "Abundance", x = "Sample") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

###############################################################

citation()
devtools::session_info()
citation("readxl")
citation("tidyr")
citation("ggplot2")
citation("vegan")
citation("pheatmap")
citation("reshape2")



