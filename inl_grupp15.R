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


# test 3
x <- sum_of_random_dice(K = 300, lambda = 5, my_seed = 387)
# Ritar ett histogram över resultaten av summorna.
hist(x[,2])


# test 4

y <- sum_of_random_dice(K = 100, lambda = 10, my_seed = 723)

mean(y[,2])
sd(y[,2])


# test 5

z <- sum_of_random_dice(K = 30, lambda = 0.4, my_seed = 395)
# korstabell över antalet tärningar och summan.
table(z[,1],z[,2])


#' Title
#'
#' @description
#'
#' @param K 
#' @param lambda 
#' @param my_seed 
#'
#' @return
#' 
#' 
#' 


# Uppgift 2 ----

#' Title
#' 
#' @description
#'
#' @param lasttime 
#' @param holiday 
#' @param sex 
#' @param type_of_travel 
#'
#' @return

install.packages("lubridate")

library(lubridate)

give_blood <- function(lasttime = today(), holiday = "hemma", sex, type_of_travel = NULL) {
  
  lasttime <- as.Date(lasttime)
  
  # räkna ut extraTime beroende på resa
  if (identical(holiday, "hemma")) {
    extraTime <- lasttime
  } else {
    end_date <- as.Date(int_end(holiday))
    if (type_of_travel == "malaria") {
      extraTime <- end_date + months(6)
    } else {
      extraTime <- end_date + weeks(4)
    }
  }
  
  # tidigaste möjliga datum baserat på kön
  if (sex == "f") {
    suggestion <- lasttime + months(4)
  } else {
    suggestion <- lasttime + months(3)
  }
  
  # jämför suggestion med extraTime
  if (suggestion > extraTime) {
    proposal <- suggestion
  } else {
    proposal <- extraTime + days(1)
  }
  
  # se till att förslaget är en vardag (måndag-fredag)
  # wday: 1 = söndag, 7 = lördag
  while (wday(proposal) %in% c(1, 7)) {
    proposal <- proposal + days(1)
  }
  
  # returnera som textsträng
  yr <- year(proposal)
  month_names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  mo <- month_names[month(proposal)]
  dy <- day(proposal)
  weekday_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  wd <- weekday_names[wday(proposal)]
  
  return(paste0("year=", yr, " month=", mo, " day=", dy, " weekday=", wd))
}



# OBS: kommentera bort all kod som inte är de obligatoriska variablerna ovan 
# eller funktionerna som defineras i inlämningsuppgifterna!


# Använda markmyassignment
library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl3.yml")
show_tasks()
mark_my_assignment()
