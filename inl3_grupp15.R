#### Programmering i R ####
#### Datorlaboration  [nummer] ####

# spara denna fil enligt:
# inl3_grupp01.R om ni tillhör grupp 01 och det är inl 3.


# Skiv era namn här
Namn <- c("Mustafa Helali", "Samuel Olivares")


# Skiv era liu ID här
LiuId  <- c("mushe150", "samol695")

# På del 2 i kursen, när ni arbetar i par, ange även grupptillhörighet enligt: 
# Grupp<-"grupp01", om ni tillhör grupp01
Grupp<-"grupp15"



# Uppgift 1
#' Title
#'
#' @description: Summerar värdet från ett fixt antal tärningar. Skriva en funktion som kan summera ett slumpmässigt antal tärningar. Tärningarna är vanliga 6-sidiga tärningar med värden 1 till 6.
#' Funktionerna ska heta sum_of_random_dice() med argumenten:
#'
#' @param N 
#' @param K 
#'
#' @return
#' 



sum_of_dice <- function(N, K){
  res <- integer(K)
  for (K in 1:K){
    res[K] <- sum(sample(1:6, size = N, replace = TRUE))
  }
  return(res)
}



sum_of_random_dice <- function(K, lambda, my_seed=NULL){
  
  #kolla om my_seed skiljer sig från null. Om inte väljs random seed.
  
  if(!is.null(my_seed)) {
    set.seed(my_seed)
  }
  
  #skapa matrisen med K rader och 2 kolumner. Kolumners namn är Value och Dice
  
  x<-matrix(nrow = K, ncol = 2)
  colnames(x) <-c("Value","Dice")
  rownames(x) <-c(1:K)
  
  #skapa poissonfördelad matris efter parametrar K och n
  
 for(i in 1:K) {
   n<-rpois(1, lambda = lambda)
   x[i, "Dice"] <- n
   if (n == 0) {
     x[i, "Value"] <- 0
   } else {
     x[i, "Value"] <- sum_of_dice(N = n, K = 1)
   }

 }

return(x)
}



# Uppgift 2 ----

#' Title
#' 
#' @description När är en person rekommenderad att ge blod efter parametrarna?
#'
#' @param lasttime 
#' @param holiday 
#' @param sex 
#' @param type_of_travel 
#'
#' @return

install.packages("lubridate")

library(lubridate)

#skapa funktionen och definiera argumenten baserat på parametrarna

give_blood <- function(lasttime= today(), holiday = "hemma", sex, type_of_travel= NULL){
  lasttime<-as.Date(lasttime)
  if(holiday=="hemma"){
    extraTime<-lasttime
  }else{
    slutdatum<-int_end(holiday)
    if(type_of_travel=="other"){
      extraTime<-slutdatum + weeks(4)
    }else if(type_of_travel== "malaria"){
      extraTime<-slutdatum + months(6)
    }
  }
  if(sex=="f"){
    forslag <- lasttime+months(4) 
  }else if(sex=="m"){
    forslag<-lasttime+ months(3)
    
  }
  if(forslag>extraTime){
    resultat<-forslag
  }else{
    resultat<-extraTime+days(1)
  }
  while(wday(resultat,week_start = 1)%in% c(6,7)){
    resultat<- resultat+days(1)
  }

  #returnera funtionen med följande layout
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
  week_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")  
  m_text <- month_names[month(resultat)] 
  w_text <- week_names[wday(resultat)]   
  return(paste("year=", year(resultat),
               " month=", m_text,
               " day=", day(resultat),
               " weekday=", w_text, sep = ""))
  
}


# Använda markmyassignment
library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl3.yml")
show_tasks()
mark_my_assignment()
