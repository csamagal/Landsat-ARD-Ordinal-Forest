# Splitting Marine Geo Data

setwd("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data files")
mg_cover <- read.csv("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/mg_cover_clean.csv")

View(mg_cover)
mg_cover_2015 <- mg_cover[(mg_cover$year==2015),]
mg_cover_2016 <- mg_cover[(mg_cover$year==2016),]
mg_cover_2017 <- mg_cover[(mg_cover$year==2017),]
mg_cover_2018 <- mg_cover[(mg_cover$year==2018),]


write.csv(mg_cover_2015, "mg_cover_2015.csv")
write.csv(mg_cover_2016, "mg_cover_2016.csv")
write.csv(mg_cover_2017, "mg_cover_2017.csv")
write.csv(mg_cover_2018, "mg_cover_2018.csv")


