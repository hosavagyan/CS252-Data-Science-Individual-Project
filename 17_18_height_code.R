
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
ManCity <- scrapeFunc(ManCity,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/mancity.htm")
write.csv(ManCity, "ManCity17-18.csv")
ManCity17_18 <- posSelect(ManCity)
ManCity_M <- as.numeric(ManCity17_18[[1]]$X5)
ManCity_median_M <-median(ManCity_M)
ManCity_D <- as.numeric(ManCity17_18[[2]]$X5)
ManCity_median_D <-median(ManCity_D)
ManCity_G <- as.numeric(ManCity17_18[[4]]$X5)
ManCity_median_G <-median(ManCity_G)
ManCity_F <- as.numeric(ManCity17_18[[3]]$X5)
ManCity_median_F <-median(ManCity_F)

#2 Liverpool
Liverpool <- scrapeFunc(Liverpool,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/liverpool.htm")
write.csv(Liverpool, "Liver17-18.csv")
Liver17_18 <- posSelect(Liverpool)
Liver_M <- as.numeric(Liver17_18[[1]]$X5)
Liver_median_M <-median(Liver_M)
liver_D <- as.numeric(Liver17_18[[2]]$X5)
Liver_median_D <-median(liver_D)
liver_G <- as.numeric(Liver17_18[[3]]$X5)
Liver_median_G <-median(liver_G)
liver_F <- as.numeric(Liver17_18[[4]]$X5)
Liver_median_F <-median(liver_F)

#3 Chelsea
Chelsea <- scrapeFunc(Chelsea,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/chelsea.htm")
write.csv(Chelsea, "Chels17-18.csv")
Chels17_18 <- posSelect(Chelsea)
Chelsea_M <- as.numeric(Chels17_18[[1]]$X5)
Chels_median_M <-median(Chelsea_M)
Chels_D <- as.numeric(Chels17_18[[2]]$X5)
Chels_median_D <-median(Chels_D)
Chels_G <- as.numeric(Chels17_18[[3]]$X5)
Chels_median_G <-median(Chels_G)
Chels_F <- as.numeric(Chels17_18[[4]]$X5)
Chels_median_F <-median(Chels_F)

#4 Tottenham
Tottenham <- scrapeFunc(Tottenham,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/tottenha.htm")
write.csv(Tottenham, "Tott17-18.csv")
Tott17_18 <- posSelect(Tottenham)
Tott_M <- as.numeric(Tott17_18[[1]]$X5)
Tott_median_M <-median(Tott_M)
Tott_D <- as.numeric(Tott17_18[[2]]$X5)
Tott_median_D <-median(Tott_D)
Tott_G <- as.numeric(Tott17_18[[3]]$X5)
Tott_median_G <-median(Tott_G)
Tott_F <- as.numeric(Tott17_18[[4]]$X5)
Tott_median_F <-median(Tott_F)

#5 Arsenal
Arsenal <- scrapeFunc(Arsenal,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/arsenal.htm")
write.csv(Arsenal, "Arsenal17-18.csv")
Ars17_18 <- posSelect(Arsenal)
Ars_M <- as.numeric(Ars17_18[[1]]$X5)
Ars_median_M <-median(Ars_M)
Ars_D <- as.numeric(Ars17_18[[2]]$X5)
Ars_median_D <-median(Ars_D)
Ars_F <-as.numeric(Ars17_18[[3]]$X5)
Ars_median_F <- median(Ars_F)
Ars_G <- as.numeric(Ars17_18[[4]]$X5)
Ars_median_G <-median(Ars_G)

#6 Man United
ManUnited <- scrapeFunc(ManUnited,"http://www.footballsquads.co.uk/eng/2018-2019/engprem/manutd.htm")
write.csv(ManUnited, "ManUnited17-18.csv")
Utd17_18 <- posSelect(ManUnited)
Utd_M <- as.numeric(Utd17_18[[1]]$X5)
Utd_median_M <-median(Utd_M)
Utd_D <- as.numeric(Utd17_18[[2]]$X5)
Utd_median_D <-median(Utd_D)
Utd_F <-as.numeric(Utd17_18[[3]]$X5)
Utd_median_F <- median(Utd_F)
Utd_G <- as.numeric(Utd17_18[[4]]$X5)
Utd_median_G <-median(Utd_G)

#7Everton
Everton <- scrapeFunc(Everton,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/everton.htm")
write.csv(Everton, "Everton17-18.csv")
Evrt17_18 <- posSelect(Everton)
Evrt_M <- as.numeric(Evrt17_18[[1]]$X5)
Evrt_median_M <-median(Evrt_M)
Evrt_D <- as.numeric(Evrt17_18[[2]]$X5)
Evrt_median_D <-median(Evrt_D)
Evrt_F <-as.numeric(Evrt17_18[[3]]$X5)
Evrt_median_F <- median(Evrt_F)
Evrt_G <- as.numeric(Evrt17_18[[4]]$X5)
Evrt_median_G <-median(Evrt_G)

#8 Leicester
Leicester <- scrapeFunc(Leicester,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/leicester.htm")
write.csv(Leicester, "Leicester17-18.csv")
Lcst17_18 <- posSelect(Leicester)
Lcst_M <- as.numeric(Lcst17_18[[1]]$X5)
Lcst_median_M <-median(Lcst_M)
Lcst_D <- as.numeric(Lcst17_18[[2]]$X5)
Lcst_median_D <-median(Lcst_D)
Lcst_F <-as.numeric(Lcst17_18[[3]]$X5)
Lcst_median_F <- median(Lcst_F)
Lcst_G <- as.numeric(Lcst17_18[[4]]$X5)
Lcst_median_G <-median(Lcst_G)

#9 West Ham
WestHam <- scrapeFunc(WestHam,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/westham.htm")
write.csv(WestHam, "WestHam17-18.csv")
WstHm17_18 <- posSelect(WestHam)
WstHm_M <- as.numeric(WstHm17_18[[1]]$X5)
WstHm_median_M <-median(WstHm_M)
WstHm_D <- as.numeric(WstHm17_18[[2]]$X5)
WstHm_median_D <-median(WstHm_D)
WstHm_G <- as.numeric(WstHm17_18[[3]]$X5)
WstHm_median_G <-median(WstHm_G)
WstHm_F <- as.numeric(WstHm17_18[[4]]$X5)
WstHm_median_F <-median(WstHm_F)

#10 Watford
Watford <- scrapeFunc(Watford,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/watford.htm")
write.csv(Watford, "Watford17-18.csv")
Wtf17_18 <- posSelect(Watford)
Wtf_M <- as.numeric(Wtf17_18[[1]]$X5)
Wtf_median_M <-median(Wtf_M)
Wtf_D <- as.numeric(Wtf17_18[[2]]$X5)
Wtf_median_D <-median(Wtf_D)
Wtf_G <- as.numeric(Wtf17_18[[3]]$X5)
Wtf_median_G <-median(Wtf_G)
Wtf_F <- as.numeric(Wtf17_18[[4]]$X5)
Wtf_median_F <-median(Wtf_F)

#11 Crystal Palace

CrPalace <- scrapeFunc(CrPalace,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/cpalace.htm")
write.csv(CrPalace, "CrPalace17-18.csv")
Crplc17_18 <- posSelect(CrPalace)
Crplc_M <- as.numeric(Crplc17_18[[1]]$X5)
Crplc_median_M <-median(Crplc_M)
Crplc_D <- as.numeric(Crplc17_18[[2]]$X5)
Crplc_median_D <-median(Crplc_D)
Crplc_G <- as.numeric(Crplc17_18[[3]]$X5)
Crplc_median_G <-median(Crplc_G)
Crplc_F <- as.numeric(Crplc17_18[[4]]$X5)
Crplc_median_F <-median(Crplc_F)

#12 Bournemouth
Bormuth <- scrapeFunc(Bormuth,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/bourne.htm")
write.csv(Bormuth, "Bourmuth17-18.csv")
Brm17_18 <- posSelect(Bormuth)
Brm_M <- as.numeric(Brm17_18[[1]]$X5)
Brm_median_M <-median(Brm_M)
Brm_D <- as.numeric(Brm17_18[[2]]$X5)
Brm_median_D <-median(Brm_D)
Brm_G <- as.numeric(Brm17_18[[3]]$X5)
Brm_median_G <-median(Brm_G)
Brm_F <- as.numeric(Brm17_18[[4]]$X5)
Brm_median_F <-median(Brm_F)

#13 Burnley
Burnley <- scrapeFunc(Burnley,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/burnley.htm")
write.csv(Burnley, "Burnley17-18.csv")
Burn17_18 <- posSelect(Burnley)
Burn_M <- as.numeric(Burn17_18[[1]]$X5)
Burn_median_M <-median(Burn_M)
Burn_D <- as.numeric(Burn17_18[[2]]$X5)
Burn_median_D <-median(Burn_D)
Burn_G <- as.numeric(Burn17_18[[3]]$X5)
Burn_median_G <-median(Burn_G)
Burn_F <- as.numeric(Burn17_18[[4]]$X5)
Burn_median_F <-median(Burn_F)

#14 Southampton

Southam <- scrapeFunc(Southam,"http://www.footballsquads.co.uk/eng/2017-2018/faprem/southam.htm")
write.csv(Southam, "Southam17-18.csv")
South17_18 <- posSelect(Southam)
South_M <- as.numeric(South17_18[[1]]$X5)
South_median_M <-median(South_M)
South_D <- as.numeric(South17_18[[2]]$X5)
South_median_D <-median(South_D)
South_G <- as.numeric(South17_18[[3]]$X5)
South_median_G <-median(South_G)
South_F <- as.numeric(South17_18[[4]]$X5)
South_median_F <-median(South_F)
Midfielder_median <-c(ManCity_median_M,Utd_median_M,Tott_median_M,Liver_median_M,Chels_median_M,
                      Ars_median_M,Burn_median_M,Evrt_median_M,Lcst_median_M,Crplc_median_M,Brm_median_M,
                      WstHm_median_M,Wtf_median_M,South_median_M)
Defender_Median <- c(ManCity_median_D,Utd_median_D,Tott_median_D,Liver_median_D,Chels_median_D,
                     Ars_median_D,Burn_median_D,Evrt_median_D,Lcst_median_D,Crplc_median_D,Brm_median_D,
                     WstHm_median_D,Wtf_median_D,South_median_D)
GoalKeeper_Median <- c(ManCity_median_G,Utd_median_G,Tott_median_G,Liver_median_G,Chels_median_G,
                       Ars_median_G,Burn_median_G,Evrt_median_G,Lcst_median_G,Crplc_median_G,Brm_median_G,
                       WstHm_median_G,Wtf_median_G,South_median_G)
Forward_Median <- c(ManCity_median_F,Utd_median_F,Tott_median_F,Liver_median_F,Chels_median_F,
                    Ars_median_F,Burn_median_F,Evrt_median_F,Lcst_median_F,Crplc_median_F,Brm_median_F,
                    WstHm_median_F,Wtf_median_F,South_median_F)

dataOf17_18 <- cbind(as.numeric(Midfielder_Mdeian),as.numeric(Defender_Median),
                     as.numeric(GoalKeeper_Median),
                     as.numeric(Forward_Median))
dataOf17_18 <- as.data.frame(dataOf17_18)

