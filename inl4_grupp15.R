#### Programmering i R ####
#### Datorlaboration  [4] ####

# spara denna fil enligt:
# inl3_grupp01.R om ni tillhör grupp 01 och det är inl 3.


# Skiv era namn här
Namn <- c("Mustafa Helali", "Samuel Olivares")


# Skiv era liu ID här
LiuId  <- c("mushe150", "samol695")

# På del 2 i kursen, när ni arbetar i par, ange även grupptillhörighet enligt:
# Grupp<-"grupp01", om ni tillhör grupp01
Grupp<-"grupp15"



# Uppgift 1 ----
#' Title
#'
#' @description
#'
#' @param text
#' @param alpha
#'
#' @return


install.packages("tidyverse")

library(tidyverse)


# Ladda in data, obs 5 filer.
slant1 <- readLines(
  "https://raw.githubusercontent.com/STIMALiU/KursRprgm2/master/Labs/DataFiles/slant1.txt")
slant2 <- readLines(
  "https://raw.githubusercontent.com/STIMALiU/KursRprgm2/master/Labs/DataFiles/slant2.txt")
slant3 <- readLines(
  "https://raw.githubusercontent.com/STIMALiU/KursRprgm2/master/Labs/DataFiles/slant3.txt")
slant4 <- readLines(
  "https://raw.githubusercontent.com/STIMALiU/KursRprgm2/master/Labs/DataFiles/slant4.txt")
slant5 <- readLines(
  "https://raw.githubusercontent.com/STIMALiU/KursRprgm2/master/Labs/DataFiles/slant5.txt")




inl4 <- readlines(con = "raw.githubusercontent.com/STIMALiU/KursRprgm2/master/Labs/DataFiles/")
inl4

coin_test <- function(text, alpha){
  
  
  
  namn <- sub(" .*$", "", text)
  
  
  resultat <- regmatches(text, regexpr("(klave|krona)", text))
  
  
  data_df <- data.frame(Namn = namn, Resultat = resultat, stringsAsFactors = FALSE)
  
  data_df$Resultat <- factor(data_df$Resultat, levels = c("klave", "krona"))
  korstabell <- table(data_df$Namn, data_df$Resultat)
  
  
  test_res <- chisq.test(korstabell, correct = FALSE)
  p_val <- test_res$p.value
  
  
  if (p_val < alpha) {
    message(paste0("Testet fick p-värdet ", p_val, " vi kan därför förkasta H0 på nivån alpha = ", alpha))
  } else {
    message(paste0("Testet fick p-värdet ", p_val, " vi kan därför inte förkasta H0 på nivån alpha = ", alpha))
  }
  
  return(korstabell)
}




coin_test(text = slant4, alpha = 0.05)



res <- coin_test(text = slant5, alpha = 0.05)
str(res)


# OBS: kommentera bort all kod som inte är de obligatoriska variablerna ovan
# eller funktionerna som defineras i inlämningsuppgifterna!


# Använda markmyassignment
library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl4.yml")
show_tasks()
mark_my_assignment()
