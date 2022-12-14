rm(list = ls())

# Gestion de l’environnement -----
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")

library(tidyverse)
library(dplyr)
library(forcats)
library(MASS)

# Jeton API: FAILLE DE SECURITE
api_pwd <- "trotskitueleski$1917"


# Définition de fonctions -----

# fonction décennie
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

# fonction de stat agregee
fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  checkvalue <- FALSE
  for (x in c("moyenne", "variance", "ecart-type", "sd")) {
    checkvalue <- (checkvalue | b == x)
  }
  if (checkvalue == FALSE) stop("statistique non supportée")
  if (b == "moyenne") {
    x <- mean(a, na.rm = TRUE, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = TRUE, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = TRUE, ...)
  }
  return(x)
}


# Import des données -----
df <- readr::read_csv2("individu_reg.csv") %>%
  dplyr::select(c(
    "region", "dept", "aemm", "aged", "anai", "catl", "cs1", "cs2",
    "cs3", "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
    "trans", "ur"
  ))


# Retraitement des données -----

df <- df %>% 
  mutate(na38 = na_if(na38,"ZZ"),
         trans = na_if(trans,"Z"),
         tp = na_if(tp,"Z"),
         naf08 = na_if(naf08,"ZZZZZ"),
         aged = as.numeric(aged))

df$sexe <- df$sexe %>%
  fct_recode(Homme = "1", Femme = "2")

# Statistiques descriptives -----

## Catégorie socioprofessionnelle ------
NbrProf <- df %>% summarise(
  NbrProf_CS1 = n_distinct(cs1,na.rm = T),
  NbrProf_CS2 = n_distinct(cs2,na.rm = T),
  NbrProf_CS3 = n_distinct(cs3,na.rm = T)
)

## Fréquence âge détaillé -------
FreqAged <- df %>% group_by(aged) %>% summarise(Fréquence = n())

# Graphiques -----
df %>%
  dplyr::dplyr::select(aged) %>%
  ggplot(.) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

ggplot(df[as.numeric(df$aged) > 50, c(3, 4)], aes(
  x = as.numeric(aged),
  y = ..density.., fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
), alpha = 0.2) +
  geom_histogram() # position = "dodge") + scale_fill_viridis_d()

# part d'homme dans chaque cohort
ggplot(df %>%
         group_by(aged, sexe) %>%
         summarise(SH_sexe = n()) %>%
         group_by(aged) %>%
         mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
         filter(sexe == 1)) +
  geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity") +
  geom_point(aes(x = as.numeric(aged), y = SH_sexe),
             stat = "identity",
             color = "red"
  ) +
  coord_cartesian(c(0, 100))

# stats surf par statut
df3 <- tibble(df %>%
                group_by(couple, surf) %>%
                summarise(x = n()) %>%
                group_by(couple) %>%
                mutate(y = 100 * x / sum(x)))
ggplot(df3) +
  geom_bar(aes(x = surf, y = y, color = couple),
           stat = "identity",
           position = "dodge"
  )

# stats trans par statut
df3 <- tibble(df %>%
                group_by(couple, trans) %>%
                summarise(x = n()) %>%
                group_by(couple) %>%
                mutate(y = 100 * x / sum(x)))
p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple),
           stat = "identity",
           position = "dodge"
  )

ggsave("p.png", p)

# Modélisation -----




  














fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "ecart-type")
fonction_de_stat_agregee(rnorm(10), "variance")


fonction_de_stat_agregee(df %>%
                           filter(sexe == "Homme") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged))
fonction_de_stat_agregee(df %>%
                           filter(sexe == "Femme") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged))
fonction_de_stat_agregee(df %>%
                           filter(sexe == "Homme" & couple == "2") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged))
fonction_de_stat_agregee(df %>%
                           filter(sexe == "Femme" & couple == "2") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged))

# modelisation
df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)
polr(surf ~ cs1 + factor(ur), df3)
