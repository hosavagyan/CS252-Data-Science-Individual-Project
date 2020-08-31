modelData_18_19 <- read.csv("premLeague_18_19_clean.csv")
modelData_17_18 <- read.csv("premLeague_17_18_clean.csv")
modelData_16_17 <- read.csv("premLeague_16_17_clean.csv")

final_Data <-rbind(modelData_16_17,modelData_17_18,modelData_18_19)
read.csv(final_Data,"final_Data.csv")





