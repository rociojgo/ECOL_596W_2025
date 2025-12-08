# Toy example of distance matrix making and NMDS
#
install.packages("vegan")
library(vegan)
library(dplyr)

# data: each row is a sample, columns represent counts of different microbes
# in that sample

data <- read.csv("datasets/toy_microbiome.csv")


# calculate the bray-curtis distance among samples
bray_dist <- vegdist(data, method = "bray")
bray_dist

# run an NMDS
nmds <- metaMDS(bray_dist, k = 2, trymax = 100)
plot(nmds)
text(nmds, labels= data$Sample, pos = 3)


# get stuff out of the NMDS

str(nmds)
nmds$points
