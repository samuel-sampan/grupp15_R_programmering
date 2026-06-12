# ============================================================
# Projekt 732G33 - Grupp 15
# Mustafa Helali (mushe150) och Samuel Olivares (samol695)
# ============================================================

library(ggplot2)
library(dplyr)
library(knitr)

# Gemensamt tema for alla plottar
theme_set(theme_bw(base_size = 13))


# ============================================================
# 1.1.1 KOMMUNDATA
# ============================================================

# Rensa kommundata - rad 1-2 ar rubriker, datarader borjar pa rad 3
kommundata <- data.frame(
  kommun      = sub("^[0-9]+ ", "", data[[1]][3:nrow(data)]),
  folkmangd   = as.numeric(data[[2]][3:nrow(data)]),
  snittlon    = as.numeric(data[[3]][3:nrow(data)]),
  giftarmal   = as.numeric(data[[4]][3:nrow(data)]),
  skilsmassor = as.numeric(data[[5]][3:nrow(data)])
)


# --- Tabell: 5 utvalda kommuner ---

urval <- kommundata |>
  filter(grepl("Stockholm$|Goteborg$|Malmo$|Linkoping$|Uppsala$",
               iconv(kommun, "UTF-8", "ASCII//TRANSLIT")))

kable(
  urval |>
    select(
      Kommun              = kommun,
      Folkmangd           = folkmangd,
      `Snittlon (kr/man)` = snittlon,
      Giftarmal           = giftarmal,
      Skilsmassor         = skilsmassor
    ),
  caption = "Kommundata for fem utvalda kommuner, 2020"
)


# --- Histogram: Genomsnittlig manadslon ---

q1_val  <- quantile(kommundata$snittlon, 0.25)
med_val <- median(kommundata$snittlon)
q3_val  <- quantile(kommundata$snittlon, 0.75)

ggplot(kommundata, aes(x = snittlon)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = q1_val,  linetype = "dashed", color = "steelblue",
             linewidth = 0.9) +
  geom_vline(xintercept = med_val, linetype = "solid",  color = "red",
             linewidth = 0.9) +
  geom_vline(xintercept = q3_val,  linetype = "dashed", color = "steelblue",
             linewidth = 0.9) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title   = "Fordelning av genomsnittlig manadslon per kommun (2020)",
    x       = "Snittlon (kr/man)",
    y       = "Antal kommuner",
    caption = "Bla streckad = Q1 och Q3  |  Rod heldragen = Median"
  )


# --- Scatterplot: Snittlon vs. Giftarmal ---

ggplot(kommundata, aes(x = snittlon, y = giftarmal)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Snittlon vs. antal giftarmal per kommun (2020)",
    x     = "Snittlon (kr/man)",
    y     = "Antal giftarmal"
  )


# --- Korrelationstest: Snittlon ~ Giftarmal ---
# H0: cor(snittlon, giftarmal) = 0
# Ha: cor(snittlon, giftarmal) != 0

kor_test <- cor.test(kommundata$snittlon, kommundata$giftarmal)

kable(
  data.frame(
    Korrelation  = round(kor_test$estimate, 3),
    t_statistika = round(kor_test$statistic, 3),
    p_varde      = format.pval(kor_test$p.value, digits = 3, eps = 0.001),
    KI_nedre     = round(kor_test$conf.int[1], 3),
    KI_ovre      = round(kor_test$conf.int[2], 3)
  ),
  caption = "Pearson-korrelationstest: snittlon och antal giftarmal"
)


# --- Kategorisk variabel: Loneklass (terciler) ---

lon_grans1 <- quantile(kommundata$snittlon, 1/3)
lon_grans2 <- quantile(kommundata$snittlon, 2/3)

kommundata <- kommundata |>
  mutate(
    loneklass = case_when(
      snittlon <= lon_grans1 ~ "Lag",
      snittlon <= lon_grans2 ~ "Medel",
      TRUE                   ~ "Hog"
    ),
    loneklass = factor(loneklass, levels = c("Lag", "Medel", "Hog"))
  )

# Kontrollera fordelning
table(kommundata$loneklass)


# --- Scatterplot fargad per loneklass ---

ggplot(kommundata, aes(x = snittlon, y = skilsmassor, color = loneklass)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Snittlon vs. antal skilsmassor, fargad efter loneklass (2020)",
    x     = "Snittlon (kr/man)",
    y     = "Antal skilsmassor",
    color = "Loneklass"
  )


# --- Boxplot: Giftarmal per loneklass ---

ggplot(kommundata, aes(x = loneklass, y = giftarmal)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Antal giftarmal per loneklass (2020)",
    x     = "Loneklass",
    y     = "Antal giftarmal"
  )


# ============================================================
# 1.1.2 TIDSSERIEDATA
# ============================================================

# Rensa tidsseridata: manadsvis antal skilsmassor for Riket, 2014-2023
# Ar 2014-2023 finns i kolumnerna 3-12 (fast tidsperiod, 120 observationer)
ts_raw <- X0000052M_20260612_131545

ar_vec     <- as.integer(unlist(ts_raw[2, 3:12]))    # 2014-2023
manad_namn <- as.character(unlist(ts_raw[3:14, 2]))  # svenska manadsnamn

# Matris: 12 rader (manader) x 10 kolumner (ar)
varden_mat <- matrix(
  as.numeric(unlist(ts_raw[3:14, 3:12])),
  nrow = 12, ncol = 10
)

# Langt format: en rad per (ar, manad)
skilsmassdata <- data.frame(
  ar                = rep(ar_vec, each = 12),
  manad             = rep(1:12, times = 10),
  manad_namn        = rep(manad_namn, times = 10),
  antal_skilsmassor = as.vector(varden_mat)
)

skilsmassdata$datum <- as.Date(
  paste(skilsmassdata$ar, skilsmassdata$manad, "01", sep = "-")
)


# --- 1. Linjeplot ---

ggplot(skilsmassdata, aes(x = datum, y = antal_skilsmassor)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Antal skilsmassor i Sverige per manad, jan 2014 - dec 2023",
    x     = "Datum",
    y     = "Antal skilsmassor"
  )


# --- 2. Manadsmedelvarden ---

month_means <- aggregate(antal_skilsmassor ~ manad, data = skilsmassdata,
                         FUN = mean)

manad_kort <- c("jan","feb","mar","apr","maj","jun",
                "jul","aug","sep","okt","nov","dec")
month_means$manad_namn_kort <- factor(manad_kort, levels = manad_kort)

# Tabell
kable(
  month_means[, c("manad_namn_kort", "antal_skilsmassor")],
  digits    = 1,
  col.names = c("Manad", "Medelvarde"),
  caption   = "Genomsnittligt antal skilsmassor per manad (2014-2023)"
)

# Graf
ggplot(month_means, aes(x = manad_namn_kort, y = antal_skilsmassor)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Medelantal skilsmassor per manad (2014-2023)",
    x     = "Manad",
    y     = "Medelvarde antal skilsmassor"
  )


# --- 3. Boxplot per ar ---

ggplot(skilsmassdata, aes(x = factor(ar), y = antal_skilsmassor)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Skilsmassor per manad uppdelat per ar, 2014-2023",
    x     = "Ar",
    y     = "Antal skilsmassor per manad"
  )


# --- 4. Sasongsranskning ---

Y <- skilsmassdata$antal_skilsmassor

# Subtrahera manadsmedelvarden och aterst till ratt skala
manad_match <- month_means$antal_skilsmassor[
  match(skilsmassdata$manad, month_means$manad)
]

Z               <- Y - manad_match + mean(Y)
skilsmassdata$Z <- Z


# --- 5. Linjeplot: Y och Z ---

ggplot(skilsmassdata, aes(x = datum)) +
  geom_line(aes(y = antal_skilsmassor, color = "Y (original)")) +
  geom_line(aes(y = Z,                 color = "Z (sasongsransad)")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "Y (original)"      = "grey60",
    "Z (sasongsransad)" = "steelblue"
  )) +
  labs(
    title = "Skilsmassor: original (Y) och sasongsransad (Z), 2014-2023",
    x     = "Datum",
    y     = "Antal skilsmassor",
    color = NULL
  ) +
  theme(legend.position = "top")


# --- 6. Linjeplot med regressionslinje ---

ggplot(skilsmassdata, aes(x = datum, y = antal_skilsmassor)) +
  geom_line(color = "grey60") +
  geom_smooth(method = "lm") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Antal skilsmassor med linjear regressionslinje, 2014-2023",
    x     = "Datum",
    y     = "Antal skilsmassor"
  )

