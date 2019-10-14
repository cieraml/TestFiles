#Date-Time----
install.packages("nutshell")
install.packages("stringr")
load("~/Downloads/Test2_data_sets/test2_data.Rdata")
load("~/Downloads/fish_data.Rdata")
load("~/Downloads/Test2_data_sets/t2-2.Rdata")
#will use t2 data

#Importing & Saving Files----
load("~/Downloads/import_datasets/aurelia_15minCell_statareas.Rdata")
library(readxl)
Aurelia_SEAMAP_2012_2018_30minCell <- read_excel("import_datasets/Aurelia_SEAMAP_2012-2018_30minCell.xlsx")
View(Aurelia_SEAMAP_2012_2018_30minCell)
library(data.table)
d1 <- fread(input = "~/Downloads/import_datasets/aurelia_15minCell_statareas.txt", sep = ",", 
            header = T,stringsAsFactors = F)
d2 <- read.csv(file = "~/Downloads/import_datasets/aurelia_15minCell_statareas.txt", sep = ",",
               header = T, stringsAsFactors = F)
d3 <- read.table(file = "~/Downloads/import_datasets/aurelia_15minCell_statareas.txt", sep = ",",
                 header = T, stringsAsFactors = F)
library(tidyverse)
d4 <- read_csv(file = "~/Downloads/import_datasets/aurelia_15minCell_statareas.txt")
au <- Aurelia_SEAMAP_2012_2018_30minCell
dau <- au[au$year == "2012",]
dau2 <- subset(x = au, year == "2012")
save(dau2, file = "aurealia_2012.Rdata")

#Combining Data Frames----
load("~/Downloads/Test2_data_sets/test2_deep.Rdata")
load("~/Downloads/Test2_data_sets/test2_mid.Rdata")
load("~/Downloads/Test2_data_sets/test2_shallow.Rdata")
f.smd <- rbind(shallow, mid, deep)
load("~/Downloads/Test2_data_sets/t2-2.Rdata")
load("~/Downloads/Test2_data_sets/t2-1.Rdata")
join.t1t2 <- left_join(x = t2, y = t2.1, by = 'parcel.id')
t1t2 = merge(x = t2, y = t2.1, by = 'transect.id', 'group', 'parcel.id', all.y = T)

#Summarise----
data("batting.2008")
b <- batting.2008
bHR <- tapply(X = b$HR, b$teamID, FUN = sum)
bTotal <- aggregate(x = b$AB, b$H, b$BB, b$`2B`, b$HR, by = list(b$teamID), FUN = sum)
b %>% group_by(HR) %>% summarise(HRmean = mean(HR), HRtotal = sum(HR), HRsd = sd(HR))

#Bonus----
