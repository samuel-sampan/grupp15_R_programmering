---
title: 'Projekt 732G33'
output:
  html_document: default
  pdf_document: default
---

*Forfattare:* Mustafa Helali och Samuel Olivares.

*LiU-id:* mushe150 och samol695

*Grupp:* 15

```{r installera-paket, echo=FALSE, message=FALSE, warning=FALSE}
pkgs <- c("ggplot2", "dplyr", "knitr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
```

```{r ladda-paket, echo=FALSE, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)
library(knitr)
```

# Introduktion

Analysen bygger pa tva dataset fran SCB. Kommundatasetet avser fyra kommuner
i Kalmar lan och innehaller foljande variabler:

- **antal_arbetsloshet**: Antal personer som mottagit arbetsloshetserssattning
  under perioden (populationsrelaterad, normaliseras).
- **invanare**: Totalt antal invanare i kommunen.
- **medianinkomst**: Medianinkomst per ar i kronor (ej populationsrelaterad).
- **kommunalskatt**: Kommunalskattesats i procent (ej populationsrelaterad).

Tidsserie-datasetet innehaller manadsvis antal skilsmassor i Sverige 2000-2024.

# 1.1.1 Kommundata

```{r kommundata-rensning, echo=TRUE}
#' Rensa och normalisera kommundata
#'
#' @description Byter kolumnnamn till rena variabelnamn och beraknar
#'   normaliserade variabler baserat pa invanarantal.
#'
#' @details andel_arbetsloshet ar proportionen (0-1).
#'   arbetsloshet_per_10k ar antal per 10 000 invanare.
#'   Medianinkomst och kommunalskatt normaliseras ej.

kommun_data <- persons_received_unemployment_benefits_municipality_year
names(kommun_data) <- c("kommun", "antal_arbetsloshet", "invanare",
                        "medianinkomst", "kommunalskatt")

kommun_data <- kommun_data |>
  mutate(
    andel_arbetsloshet   = antal_arbetsloshet / invanare,
    arbetsloshet_per_10k = (antal_arbetsloshet / invanare) * 10000
  )
```

## Tabell over kommuner

```{r kommundata-tabell, echo=FALSE}
#' Tabell: kommuner med normaliserade och ursprungliga variabler

kable(
  kommun_data |>
    select(Kommun          = kommun,
           `Andel arblosa` = andel_arbetsloshet,
           `Arblosa/10k`   = arbetsloshet_per_10k,
           `Medianinkomst` = medianinkomst,
           `Kommunalskatt` = kommunalskatt),
  digits  = 4,
  caption = "Kommundata for Kalmar lan"
)
```

## Histogram

```{r histogram-arbetsloshet, echo=FALSE}
#' Histogram: arbetsloshet per 10 000 invanare med Q1, median, Q3
#'
#' @details Vertikala linjer: bla streckad = Q1/Q3, rod heldragen = median.

q1_val  <- quantile(kommun_data$arbetsloshet_per_10k, 0.25)
med_val <- median(kommun_data$arbetsloshet_per_10k)
q3_val  <- quantile(kommun_data$arbetsloshet_per_10k, 0.75)

ggplot(kommun_data, aes(x = arbetsloshet_per_10k)) +
  geom_histogram(bins = 8) +
  geom_vline(xintercept = q1_val,  linetype = "dashed", color = "blue", linewidth = 0.8) +
  geom_vline(xintercept = med_val, linetype = "solid",  color = "red",  linewidth = 0.8) +
  geom_vline(xintercept = q3_val,  linetype = "dashed", color = "blue", linewidth = 0.8) +
  labs(
    title   = "Fordelning av arbetsloshet per 10 000 invanare",
    x       = "Arblosa per 10 000 invanare",
    y       = "Antal kommuner",
    caption = "Bla streckad = Q1 och Q3 | Rod heldragen = median"
  )
```

*Tolkning: [Beskriv vad histogrammet visar.]*

## Scatterplot och regression

```{r scatterplot, echo=FALSE}
#' Scatterplot: medianinkomst vs. arbetsloshet med OLS-regressionslinje

ggplot(kommun_data, aes(x = medianinkomst, y = arbetsloshet_per_10k)) +
  geom_point(size = 3) +
  geom_text(aes(label = kommun), vjust = -0.8, size = 3) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Medianinkomst vs. arbetsloshet per 10 000 invanare",
    x     = "Medianinkomst (kr/ar)",
    y     = "Arblosa per 10 000 invanare"
  )
```

*Tolkning: [Beskriv vad scatterplotten visar.]*

## Korrelationstest

```{r korrelationstest, echo=TRUE}
#' Korrelationstest
#'
#' @details H0: cor(medianinkomst, arbetsloshet_per_10k) = 0
#'   Ha: cor(medianinkomst, arbetsloshet_per_10k) != 0
#'   Tvasidigt Pearson-test med 95%-konfidensintervall.

kor_test <- cor.test(kommun_data$medianinkomst,
                     kommun_data$arbetsloshet_per_10k)

kable(
  data.frame(
    Korrelation  = round(kor_test$estimate, 4),
    t_statistika = round(kor_test$statistic, 4),
    p_varde      = round(kor_test$p.value, 4),
    KI_nedre     = round(kor_test$conf.int[1], 4),
    KI_ovre      = round(kor_test$conf.int[2], 4)
  ),
  caption = "Pearson-korrelationstest: medianinkomst och arbetsloshet"
)
```

*Tolkning: [Beskriv hur ni tolkar korrelationen och testet.]*

## Kategorisk variabel

```{r kategorisk-variabel, echo=TRUE}
#' Skapar kategorisk variabel: skatteklass
#'
#' @description Delar kommunalskatt i tre nivaer baserat pa fordelningen:
#'   "Lag" (< 33.9%), "Medel" (33.9-34.1%), "Hog" (> 34.1%).

kommun_data <- kommun_data |>
  mutate(
    skatteklass = case_when(
      kommunalskatt < 33.9  ~ "Lag",
      kommunalskatt <= 34.1 ~ "Medel",
      TRUE                  ~ "Hog"
    ),
    skatteklass = factor(skatteklass, levels = c("Lag", "Medel", "Hog"))
  )

table(kommun_data$skatteklass)
```

## Scatterplot med farg per kategori

```{r scatterplot-farg, echo=FALSE}
#' Scatterplot fargad efter skatteklass

ggplot(kommun_data, aes(x = medianinkomst, y = arbetsloshet_per_10k,
                        color = skatteklass)) +
  geom_point(size = 4) +
  geom_text(aes(label = kommun), vjust = -0.8, size = 3, show.legend = FALSE) +
  labs(
    title = "Medianinkomst vs. arbetsloshet, fargad efter skatteklass",
    x     = "Medianinkomst (kr/ar)",
    y     = "Arblosa per 10 000 invanare",
    color = "Skatteklass"
  )
```

## Boxplot grupperad pa kategorisk variabel

```{r boxplot-kategori, echo=FALSE}
#' Boxplot: arbetsloshet uppdelat per skatteklass

ggplot(kommun_data, aes(x = skatteklass, y = arbetsloshet_per_10k)) +
  geom_boxplot() +
  geom_point(size = 3) +
  labs(
    title = "Arbetsloshet per 10 000 uppdelat per skatteklass",
    x     = "Skatteklass",
    y     = "Arblosa per 10 000 invanare"
  )
```

*Tolkning: [Beskriv vad ni ser.]*

---

# 1.1.2 Tidseriedata

```{r tidserie-rensning, echo=TRUE}
#' Rensar och formar om manadsvis tidsseriedata till langt format
#'
#' @description Radata fran SCB ar brett format: rad 2 = ar, rad 3-14 =
#'   12 manader for Riket, kolumn 5-29 = aren 2000-2024.
#'   Omformas till ett data.frame med en rad per (ar, manad).
#'
#' @note Kolumner 1-2 ar regionkoder (endast Riket anvands).
#'   Rad 15 ar "okant" och rader 16+ ar SCB-metadata - ignoreras.

ts_raw_m <- X0000052M_20260611_213647

ar_vec     <- as.integer(unlist(ts_raw_m[2, 5:29]))    # 25 ar: 2000-2024
manad_nr   <- as.integer(unlist(ts_raw_m[3:14, 3]))    # 1-12
manad_namn <- as.character(unlist(ts_raw_m[3:14, 4]))  # "januari"-"december"

# Extrahera varden i matrisform: 12 rader (manader) x 25 kolumner (ar)
# as.vector() lasar kolumn for kolumn = ar for ar, manad for manad
varden_mat <- matrix(
  as.numeric(unlist(ts_raw_m[3:14, 5:29])),
  nrow = 12, ncol = 25
)

skilsmassdata <- data.frame(
  ar                = rep(ar_vec, each = 12),
  manad             = rep(manad_nr, times = 25),
  manad_namn        = rep(manad_namn, times = 25),
  antal_skilsmassor = as.vector(varden_mat)
)

skilsmassdata$datum <- as.Date(
  paste(skilsmassdata$ar, skilsmassdata$manad, "01", sep = "-")
)
```

## Linjeplot (uppgift 1)

```{r tidserie-linje, echo=FALSE}
#' Linjeplot: antal skilsmassor per manad 2000-2024

ggplot(skilsmassdata, aes(x = datum, y = antal_skilsmassor)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Antal skilsmassor i Sverige per manad (2000-2024)",
    x     = "Datum",
    y     = "Antal skilsmassor"
  )
```

*Tolkning: [Beskriv vad ni ser - trend, variation, avvikelser.]*

## Manadsmedelvarden (uppgift 2)

```{r manad-medel, echo=TRUE}
#' Beraknar genomsnittligt antal skilsmassor per manad over alla ar
#'
#' @details Varje manads medelvarde beraknas over 25 ar (2000-2024).

month_means <- aggregate(antal_skilsmassor ~ manad, data = skilsmassdata,
                         FUN = mean)
month_means$manad_namn <- factor(
  c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec"),
  levels = c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec")
)

kable(month_means[, c("manad_namn","antal_skilsmassor")],
      digits = 1,
      col.names = c("Manad", "Medelvarde"),
      caption = "Genomsnittligt antal skilsmassor per manad (2000-2024)")
```

```{r manad-medel-plot, echo=FALSE}
#' Stapeldiagram: manadsmedelvarden for antal skilsmassor

ggplot(month_means, aes(x = manad_namn, y = antal_skilsmassor)) +
  geom_col() +
  labs(
    title = "Medelantal skilsmassor per manad (2000-2024)",
    x     = "Manad",
    y     = "Medelvarde"
  )
```

*Tolkning: [Beskriv eventuell sasongsvarians.]*

## Boxplot per ar (uppgift 3)

```{r boxplot-ar, echo=FALSE}
#' Grupperade boxplots: en boxplot per ar, baserat pa de 12 manadesvardeana

ggplot(skilsmassdata, aes(x = factor(ar), y = antal_skilsmassor)) +
  geom_boxplot() +
  scale_x_discrete(breaks = as.character(seq(2000, 2024, by = 5))) +
  labs(
    title = "Skilsmassor per ar - en boxplot per ar",
    x     = "Ar",
    y     = "Antal skilsmassor per manad"
  )
```

*Tolkning: [Beskriv vad ni ser - niva, spridning, trend over ar.]*

## Sasongsranskning (uppgift 4-5)

```{r sasongsranskning, echo=TRUE}
#' Subtraherar manadsmedelvarden fran Y for att ta bort sasongsvariationen
#'
#' @details Z = Y - manadsmedel + mean(Y), sa att Z far ratt skala.
#'   Z representerar den sasongsjusterade tidsserien.

Y <- skilsmassdata$antal_skilsmassor

manad_match          <- month_means$antal_skilsmassor[
  match(skilsmassdata$manad, month_means$manad)
]
Z                    <- Y - manad_match + mean(Y)
skilsmassdata$Z      <- Z
```

```{r yz-plot, echo=FALSE}
#' Linjeplot: Y (original) och Z (sasongsransad) i samma graf

ggplot(skilsmassdata, aes(x = datum)) +
  geom_line(aes(y = antal_skilsmassor, color = "Y (original)")) +
  geom_line(aes(y = Z, color = "Z (sasongsransad)")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_color_manual(values = c("Y (original)"     = "grey60",
                                "Z (sasongsransad)" = "steelblue")) +
  labs(
    title = "Skilsmassor: original (Y) och sasongsransad (Z)",
    x     = "Datum",
    y     = "Antal skilsmassor",
    color = NULL
  )
```

*Tolkning: [Beskriv skillnaden mellan Y och Z.]*

## Regressionslinje (uppgift 6)

```{r tidserie-regression, echo=FALSE}
#' Linjeplot med linjear regressionslinje for att visa trend

ggplot(skilsmassdata, aes(x = datum, y = antal_skilsmassor)) +
  geom_line(color = "grey60") +
  geom_smooth(method = "lm") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Antal skilsmassor med regressionslinje (2000-2024)",
    x     = "Datum",
    y     = "Antal skilsmassor"
  )
```

*Tolkning (trend och sasongsvarians): [Beskriv om det finns en trend och/eller
sasongsvarians i data.]*

