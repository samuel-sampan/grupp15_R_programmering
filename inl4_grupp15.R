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
#' @Title coin_test
#'
#' @description
#' funktionen kollar om det finns samband mellan vem kastar myntet och resultat
#' funktionen läser in text , skapar en tabell och gör ett chi-två test
#'
#' @param text lista med textrader där varje rad visar vem som har kastat och vad är resultat
#' @param alpha gränsen för när vi förkastar nollhypotesen 
#'
#' @return en tabell som visar hur många klave och krona varje person fick


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



coin_test <- function(text, alpha){
  
#'1- Hämta data 
#'sub puckar ut namnet (första ordet på raden)
  
  namn <- sub(" .*$", "", text)
  
#'regmatches och regexpr letar upp ordent "klove" eller "krona"
  resultat <- regmatches(text, regexpr("(klave|krona)", text))
  
#'2_ skapa en data tabell 
#'spara namn och resultat tillsamnas
  data_df <- data.frame(Namn = namn, Resultat = resultat, stringsAsFactors = FALSE)
  
#'lås ordning till "klove" sedan "krona" så tabellen alltid blir rätt
  data_df$Resultat <- factor(data_df$Resultat, levels = c("klave", "krona"))
  
#'tabell skapar själva korsytabell med antal 
  korstabell <- table(data_df$Namn, data_df$Resultat)
#'3-Gör statistisk-test
#' chisq.test räknar ut p-värdet 
  test_res <- chisq.test(korstabell, correct = FALSE)

#'spara p-värdet
   p_val <- test_res$p.value
  
#'4-skrivut resultat
#'kolla om pvärdet är mindre än alpha
  if (p_val < alpha) {
    message(paste0("Testet fick p-värdet ", p_val, " vi kan därför förkasta H0 på nivån alpha = ", alpha))
  } else {
    message(paste0("Testet fick p-värdet ", p_val, " vi kan därför inte förkasta H0 på nivån alpha = ", alpha))
  }
  
#'5- retunera tabellen
#'skicka tillbaka tabell som svar
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
