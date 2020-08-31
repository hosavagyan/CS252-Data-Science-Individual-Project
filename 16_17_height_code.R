library(xlsx)
posSelect <- function(theData){
  values <- c("M", "D", "F", "G")
  cleanDatasets <- vector("list", 4)
  for(i in 1:4) {
    clean_data <- theData %>%
      filter(X4 == values[i])
    cleanDatasets[[i]] <- clean_data
  }
  return(cleanDatasets)
}
scrapeFunc <- function(teamname,URL_link){
  webpage_url <- URL_link
  webpage <- xml2::read_html(webpage_url)
  ExOffndrsRaw <- rvest::html_table(webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  ExOffndrsRaw %>% dplyr::glimpse(45)
  ExOffndrsRaw <- ExOffndrsRaw[!apply(ExOffndrsRaw, 1, function(x) any(x=="")),]
  teamname <- ExOffndrsRaw[,c(4,5)]
  return(teamname)
}

#1 Man City
ManCity <- scrapeFunc(ManCity,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/mancity.htm")
write.csv(ManCity, "ManCity16-17.csv")
ManCity16_17 <- posSelect(ManCity)
ManCity_M <- as.numeric(ManCity16_17[[1]]$X5)
ManCity_median_M <-median(ManCity_M)
ManCity_D <- as.numeric(ManCity16_17[[2]]$X5)
ManCity_median_D <-median(ManCity_D)
ManCity_G <- as.numeric(ManCity16_17[[4]]$X5)
ManCity_median_G <-median(ManCity_G)
ManCity_F <- as.numeric(ManCity16_17[[3]]$X5)
ManCity_median_F <-median(ManCity_F)

#2 Liverpool
Liverpool <- scrapeFunc(Liverpool,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/liverpool.htm")
write.csv(Liverpool, "Liver16-17.csv")
Liver16_17 <- posSelect(Liverpool)
Liver_M <- as.numeric(Liver16_17[[1]]$X5)
Liver_median_M <-median(Liver_M)
liver_D <- as.numeric(Liver16_17[[2]]$X5)
Liver_median_D <-median(liver_D)
liver_G <- as.numeric(Liver16_17[[3]]$X5)
Liver_median_G <-median(liver_G)
liver_F <- as.numeric(Liver16_17[[4]]$X5)
Liver_median_F <-median(liver_F)

#3 Chelsea
Chelsea <- scrapeFunc(Chelsea,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/chelsea.htm")
write.csv(Chelsea, "Chels16-17.csv")
Chels16_17 <- posSelect(Chelsea)
Chelsea_M <- as.numeric(Chels16_17[[1]]$X5)
Chels_median_M <-median(Chelsea_M)
Chels_D <- as.numeric(Chels16_17[[2]]$X5)
Chels_median_D <-median(Chels_D)
Chels_G <- as.numeric(Chels16_17[[3]]$X5)
Chels_median_G <-median(Chels_G)
Chels_F <- as.numeric(Chels16_17[[4]]$X5)
Chels_median_F <-median(Chels_F)

#4 Tottenham
Tottenham <- scrapeFunc(Tottenham,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/tottenha.htm")
write.csv(Tottenham, "Tott16-17.csv")
Tott16_17 <- posSelect(Tottenham)
Tott_M <- as.numeric(Tott16_17[[1]]$X5)
Tott_median_M <-median(Tott_M)
Tott_D <- as.numeric(Tott16_17[[2]]$X5)
Tott_median_D <-median(Tott_D)
Tott_G <- as.numeric(Tott16_17[[3]]$X5)
Tott_median_G <-median(Tott_G)
Tott_F <- as.numeric(Tott16_17[[4]]$X5)
Tott_median_F <-median(Tott_F)

#5 Arsenal
Arsenal <- scrapeFunc(Arsenal,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/arsenal.htm")
write.csv(Arsenal, "Arsenal16-17.csv")
Ars16_17 <- posSelect(Arsenal)
Ars_M <- as.numeric(Ars16_17[[1]]$X5)
Ars_median_M <-median(Ars_M)
Ars_D <- as.numeric(Ars16_17[[2]]$X5)
Ars_median_D <-median(Ars_D)
Ars_F <-as.numeric(Ars16_17[[3]]$X5)
Ars_median_F <- median(Ars_F)
Ars_G <- as.numeric(Ars16_17[[4]]$X5)
Ars_median_G <-median(Ars_G)

#6 Man United
ManUnited <- scrapeFunc(ManUnited,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/manutd.htm")
write.csv(ManUnited, "ManUnited16-17.csv")
Utd16_17 <- posSelect(ManUnited)
Utd_M <- as.numeric(Utd16_17[[1]]$X5)
Utd_median_M <-median(Utd_M)
Utd_D <- as.numeric(Utd16_17[[2]]$X5)
Utd_median_D <-median(Utd_D)
Utd_F <-as.numeric(Utd16_17[[3]]$X5)
Utd_median_F <- median(Utd_F)
Utd_G <- as.numeric(Utd16_17[[4]]$X5)
Utd_median_G <-median(Utd_G)

#7Everton
Everton <- scrapeFunc(Everton,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/everton.htm")
write.csv(Everton, "Everton16-17.csv")
Evrt16_17 <- posSelect(Everton)
Evrt_M <- as.numeric(Evrt16_17[[1]]$X5)
Evrt_median_M <-median(Evrt_M)
Evrt_D <- as.numeric(Evrt16_17[[2]]$X5)
Evrt_median_D <-median(Evrt_D)
Evrt_F <-as.numeric(Evrt16_17[[3]]$X5)
Evrt_median_F <- median(Evrt_F)
Evrt_G <- as.numeric(Evrt16_17[[4]]$X5)
Evrt_median_G <-median(Evrt_G)

#8 Leicester
Leicester <- scrapeFunc(Leicester,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/leicester.htm")
write.csv(Leicester, "Leicester16-17.csv")
Lcst16_17 <- posSelect(Leicester)
Lcst_M <- as.numeric(Lcst16_17[[1]]$X5)
Lcst_median_M <-median(Lcst_M)
Lcst_D <- as.numeric(Lcst16_17[[2]]$X5)
Lcst_median_D <-median(Lcst_D)
Lcst_F <-as.numeric(Lcst16_17[[3]]$X5)
Lcst_median_F <- median(Lcst_F)
Lcst_G <- as.numeric(Lcst16_17[[4]]$X5)
Lcst_median_G <-median(Lcst_G)

#9 West Ham
WestHam <- scrapeFunc(WestHam,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/westham.htm")
write.csv(WestHam, "WestHam16-17.csv")
WstHm16_17 <- posSelect(WestHam)
WstHm_M <- as.numeric(WstHm16_17[[1]]$X5)
WstHm_median_M <-median(WstHm_M)
WstHm_D <- as.numeric(WstHm16_17[[2]]$X5)
WstHm_median_D <-median(WstHm_D)
WstHm_G <- as.numeric(WstHm16_17[[3]]$X5)
WstHm_median_G <-median(WstHm_G)
WstHm_F <- as.numeric(WstHm16_17[[4]]$X5)
WstHm_median_F <-median(WstHm_F)

#10 Watford
Watford <- scrapeFunc(Watford,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/watford.htm")
write.csv(Watford, "Watford16-17.csv")
Wtf16_17 <- posSelect(Watford)
Wtf_M <- as.numeric(Wtf16_17[[1]]$X5)
Wtf_median_M <-median(Wtf_M)
Wtf_D <- as.numeric(Wtf16_17[[2]]$X5)
Wtf_median_D <-median(Wtf_D)
Wtf_G <- as.numeric(Wtf16_17[[3]]$X5)
Wtf_median_G <-median(Wtf_G)
Wtf_F <- as.numeric(Wtf16_17[[4]]$X5)
Wtf_median_F <-median(Wtf_F)

#11 Crystal Palace

CrPalace <- scrapeFunc(CrPalace,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/cpalace.htm")
write.csv(CrPalace, "CrPalace16-17.csv")
Crplc16_17 <- posSelect(CrPalace)
Crplc_M <- as.numeric(Crplc16_17[[1]]$X5)
Crplc_median_M <-median(Crplc_M)
Crplc_D <- as.numeric(Crplc16_17[[2]]$X5)
Crplc_median_D <-median(Crplc_D)
Crplc_G <- as.numeric(Crplc16_17[[3]]$X5)
Crplc_median_G <-median(Crplc_G)
Crplc_F <- as.numeric(Crplc16_17[[4]]$X5)
Crplc_median_F <-median(Crplc_F)

#12 Bournemouth
Bormuth <- scrapeFunc(Bormuth,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/bourne.htm")
write.csv(Bormuth, "Bourmuth16-17.csv")
Brm16_17 <- posSelect(Bormuth)
Brm_M <- as.numeric(Brm16_17[[1]]$X5)
Brm_median_M <-median(Brm_M)
Brm_D <- as.numeric(Brm16_17[[2]]$X5)
Brm_median_D <-median(Brm_D)
Brm_G <- as.numeric(Brm16_17[[3]]$X5)
Brm_median_G <-median(Brm_G)
Brm_F <- as.numeric(Brm16_17[[4]]$X5)
Brm_median_F <-median(Brm_F)

#13 Burnley
Burnley <- scrapeFunc(Burnley,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/burnley.htm")
write.csv(Burnley, "Burnley16-17.csv")
Burn16_17 <- posSelect(Burnley)
Burn_M <- as.numeric(Burn16_17[[1]]$X5)
Burn_median_M <-median(Burn_M)
Burn_D <- as.numeric(Burn16_17[[2]]$X5)
Burn_median_D <-median(Burn_D)
Burn_G <- as.numeric(Burn16_17[[3]]$X5)
Burn_median_G <-median(Burn_G)
Burn_F <- as.numeric(Burn16_17[[4]]$X5)
Burn_median_F <-median(Burn_F)

#14 Southampton

Southam <- scrapeFunc(Southam,"http://www.footballsquads.co.uk/eng/2016-2017/faprem/southam.htm")
write.csv(Southam, "Southam16-17.csv")
South16_17 <- posSelect(Southam)
South_M <- as.numeric(South16_17[[1]]$X5)
South_median_M <-median(South_M)
South_D <- as.numeric(South16_17[[2]]$X5)
South_median_D <-median(South_D)
South_G <- as.numeric(South16_17[[3]]$X5)
South_median_G <-median(South_G)
South_F <- as.numeric(South16_17[[4]]$X5)
South_median_F <-median(South_F)
Midfielder_median <-c(Chels_median_M,Tott_median_M,ManCity_median_M,Liver_median_M,Ars_median_M,
                      Utd_median_M,Evrt_median_M,South_median_M,Brm_median_M,WstHm_median_M,
                      Lcst_median_M,Crplc_median_M,Burn_median_M,Wtf_median_M)
Defender_Median <- c(Chels_median_D,Tott_median_D,ManCity_median_D,Liver_median_D,Ars_median_D,
                     Utd_median_D,Evrt_median_D,South_median_D,Brm_median_D,WstHm_median_D,
                     Lcst_median_D,Crplc_median_D,Burn_median_D,Wtf_median_D)
GoalKeeper_Median <- c(Chels_median_G,Tott_median_G,ManCity_median_G,Liver_median_G,Ars_median_G,
                       Utd_median_G,Evrt_median_G,South_median_G,Brm_median_G,WstHm_median_G,
                       Lcst_median_G,Crplc_median_G,Burn_median_G,Wtf_median_G)
Forward_Median <- c(Chels_median_F,Tott_median_F,ManCity_median_F,Liver_median_F,Ars_median_F,
                    Utd_median_F,Evrt_median_F,South_median_F,Brm_median_F,WstHm_median_F,
                    Lcst_median_F,Crplc_median_F,Burn_median_F,Wtf_median_F)

dataOf16_17 <- cbind(as.numeric(Midfielder_Mdeian),as.numeric(Defender_Median),
                     as.numeric(GoalKeeper_Median),
                     as.numeric(Forward_Median))
dataOf16_17 <- as.data.frame(dataOf16_17)

medians_Data <- rbind(dataOf18_19,dataOf17_18,dataOf16_17)
last_Data <- cbind(final_Data,medians_Data)
last_Data <- last_Data %>%
  mutate(V1 = V1*100, V2 = V2*100,V3 = V3*100, V4 = V4*100)
colnames(last_Data)[10:14] = c("cum_AC","M_median","D_median","G_median","F_median")

write.csv(last_Data, "EPL_Data.csv")
new_Last <- last_Data %>%
  mutate(Shots = cum_HS + cum_AS, ShotsOnTarget = cum_HST + cum_AST, 
         Falls = cum_AF + cum_HF, Corners= cum_AC + cum_HC)
dropCol <- c("cum_HC", "cum_AC","cum_HF","cum_AF","cum_HS","cum_AS","cum_HST","cum_AST")
new_Last <-new_Last[ , !(names(new_Last) %in% dropCol)]
write.csv(new_Last, "Df1.csv")
