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


inl4 <- readlines(con = "Teaching/R programmering/KursRprgm2/Labs/DataFiles/...")
inl4

coin_test <- function(text, alpha){
  p2 <- "(Josef|Johan) slingade slant, (klave|krona) blev resultatet!"
  str_extract(string = inl4[1:3], pattern = p2)
  str_match(string = inl4[1:3], pattern = p2)
}





# OBS: kommentera bort all kod som inte är de obligatoriska variablerna ovan
# eller funktionerna som defineras i inlämningsuppgifterna!


# Använda markmyassignment
library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl4.yml")
show_tasks()
mark_my_assignment()
