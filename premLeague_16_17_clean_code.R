premLeague_16_17 <- read.csv("Premier_League_16-17.csv")
columnSums <- function(teamName){
  
  dataset<- premLeague_16_17 %>%
    filter(HomeTeam == teamName | AwayTeam == teamName)
  
  #Home Team HS
  cumulative_HS<- dataset %>%
    filter(HomeTeam == teamName)%>%
    select(HS)
  cumulative_HS<-sum(cumulative_HS) 
  
  #Away Team AS
  cumulative_AS<- dataset %>%
    filter(AwayTeam == teamName)%>%
    select(AS)
  cumulative_AS<- sum(cumulative_AS)
  
  #Home Team HST
  cumulative_HST<- dataset %>%
    filter(HomeTeam == teamName)%>%
    select(HST)
  cumulative_HST<-sum(cumulative_HST) 
  
  #Away Team AST
  cumulative_AST<- dataset %>%
    filter(AwayTeam == teamName)%>%
    select(AST)
  cumulative_AST<- sum(cumulative_AST)
  
  #Home Team HF
  cumulative_HF<- dataset %>%
    filter(HomeTeam == teamName)%>%
    select(HF)
  cumulative_HF<-sum(cumulative_HF) 
  
  #Away Team AF
  cumulative_AF<- dataset %>%
    filter(AwayTeam == teamName)%>%
    select(AF)
  cumulative_AF<- sum(cumulative_AF)
  
  #Home Team HC
  cumulative_HC<- dataset %>%
    filter(HomeTeam == teamName)%>%
    select(HC)
  cumulative_HC<-sum(cumulative_HC) 
  
  #Away Team AC
  cumulative_AC<- dataset %>%
    filter(AwayTeam == teamName)%>%
    select(AC)
  cumulative_AC<- sum(cumulative_AC)
  output <- c(cumulative_HS, cumulative_AS,
              cumulative_HST,cumulative_AST,
              cumulative_HF,cumulative_AF,
              cumulative_HC, cumulative_AC)
  return(output)
}

vect <- c("Man City","Liverpool","Chelsea","Tottenham","Arsenal","Man United","Everton",
          "Leicester","West Ham","Watford","Crystal Palace","Bournemouth","Burnley",
          "Southampton")
vect2 <- c("cum_HS","cum_AS","cum_HST","cum_AST","cum_HF","cum_AF","cum_HC","cum_AC")
my_matrix <- matrix(nrow = 14, ncol = 8, byrow = TRUE)
my_matrix[1,] <-matrix(columnSums("Man City"))
my_matrix[2,] <-matrix(columnSums("Liverpool"))
my_matrix[3,] <-matrix(columnSums("Chelsea"))
my_matrix[4,] <-matrix(columnSums("Tottenham"))
my_matrix[5,] <-matrix(columnSums("Arsenal"))
my_matrix[6,] <-matrix(columnSums("Man United"))
my_matrix[7,] <-matrix(columnSums("Everton"))
my_matrix[8,] <-matrix(columnSums("Leicester"))
my_matrix[9,] <-matrix(columnSums("West Ham"))
my_matrix[10,] <-matrix(columnSums("Watford"))
my_matrix[11,] <-matrix(columnSums("Crystal Palace"))
my_matrix[12,] <-matrix(columnSums("Bournemouth"))
my_matrix[13,] <-matrix(columnSums("Burnley"))
my_matrix[14,] <-matrix(columnSums("Southampton"))



rownames(my_matrix) <- vect
colnames(my_matrix) <- vect2
my_matrix
premLeague_16_17_clean <- as.data.frame(my_matrix)
LeaguePosition <- c(3,4,1,2,5,6,7,12,11,17,14,9,16,8)
premLeague_16_17_clean$Position <- LeaguePosition
premLeague_16_17_clean <-premLeague_16_17_clean[,c(9,1,2,3,4,5,6,7,8)]
write.csv(premLeague_16_17_clean, "premLeague_16_17_clean.csv")