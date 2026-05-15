#### Programmering i R ####
#### Datorlaboration  [3] ####


# Skiv era namn här
Namn <- c("Mustafa Helali", "Samuel Olivares")


# Skiv era liu ID här
LiuId  <- c("mushe150", "samol695")

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
  if(!is.null(my_seed)) {
    set.seed(my_seed)
  }
  x<-matrix(nrow = K, ncol = 2)
  colnames(x) <-c("Value","Dice")
  rownames(x) <-c(1:K)
  
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
give_blood <- function(lasttime, holiday, sex, type_of_travel){
  # Skriv din funktion här
}





# OBS: kommentera bort all kod som inte är de obligatoriska variablerna ovan 
# eller funktionerna som defineras i inlämningsuppgifterna!


# Använda markmyassignment
library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl3.yml")
show_tasks()
mark_my_assignment()
