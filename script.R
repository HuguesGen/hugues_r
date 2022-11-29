rm(list = ls())

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")

df <- readr::read_csv2("./individu_reg.csv") %>%
  select(
    c(
      "region", "dept", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
      "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur"
    )
  )

# combien de professions
print("Nombre de professions :")
df %>% summarise(n_distinct(cs3, na.rm = TRUE))

print("Nombre de professions :''")
print(summarise(df, length(unique(unlist(cs2[!is.na(cs2)])))))
print("Nombre de professions :")

print(summarise(df, length(unique(unlist(cs1[!is.na(cs1)])))))

summarise(group_by(df, aged), n())

decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %%
           10)
}
df %>%
  select(aged) %>%
  ggplot(.) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

# ggplot(df[as.numeric(df$aged) > 50,
#           c(3,4)],
#        aes(
#   x = as.numeric(df$aged),#x = as.numeric(aged) - as.numeric(aged) %% 5,
#   y = ..density..,
#   fill = factor(decennie_a_partir_annee(as.numeric(aemm)))),
#   alpha = 0.2) + geom_histogram()#position = "dodge") + scale_fill_viridis_d()

# part d'homme dans chaque cohort
ggplot(df %>% group_by(aged, sexe) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>% filter(sexe == 1)) +
  geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity") +
  geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))
# correction (qu'il faudra retirer)
# ggplot(
#   df %>% group_by(aged, sexe) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% filter(sexe==1)
# ) + geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat="identity") + geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat="identity", color = "red") + coord_cartesian(c(0,100))


# stats surf par statut
df3 <- df %>%
  group_by(couple, surf) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))

ggplot(df3) +
  geom_bar(
    aes(
      x = surf,
      y = y,
      color = couple
    ),
    stat = "identity",
    position = "dodge"
  )

# stats trans par statut
df3 <- tibble(df %>% group_by(couple, trans) %>% summarise(x = n()) %>% group_by(couple) %>% mutate(y = 100 * x / sum(x)))

p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity", position = "dodge")

dir.create("./output")
ggsave("./output/p.png", p)

# recode valeurs manquantes
# valeursManquantes <- data.frame(colonne = c(""), NBRE = c(NA))
# for (i in 1:length(colnames(df))){
#  x = df[,i]
#  j=0
#  t <-0
#  for (j in 1:nrow(x)){
#    if (is.na(pull(x[j,])) == T) t <- t+1
#  }
#  data.frame(
#
#  )
# }
df[df$na38 == "ZZ", "na38"] <- NA
df[df$trans == "Z", "trans"] <- NA
df[df$tp == "Z", "tp"] <- NA
df[df$naf08 == "ZZZZZ", "naf08"] <- NA
# df[df$aemm == "ZZZZ","aemm"] <- NA

str(df)
df[, ncol(df) - 1] <- factor(df[, ncol(df) - 1])
df$ur <- factor(df$ur)

library(forcats)
df$sexe <-
  fct_recode(as.character(df$sexe), "Homme" = "0", "Femme" = "1")

# fonction de stat agregee
fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  checkvalue <- F
  for (x in c("moyenne", "variance", "ecart-type", "sd")) {
    checkvalue <- (checkvalue | b == x)
  }
  if (checkvalue == FALSE) stop("statistique non supportée")
  
  if (b == "moyenne") {
    x <- mean(a, na.rm = T, ...)
  } else if (b == "ecart-type" | b == "sd") {
    x <- sd(a, na.rm = T, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = T, ...)
  }
  return(x)
}
fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "ecart-type")
fonction_de_stat_agregee(rnorm(10), "variance")


fonction_de_stat_agregee(df %>%
                           filter(sexe == "Homme") %>% 
                           mutate(aged = as.numeric(aged)) %>% 
                           pull(aged))
fonction_de_stat_agregee(df %>%
                           filter(sexe == "Femme") %>% mutate(aged = as.numeric(aged)) %>% pull(aged))
fonction_de_stat_agregee(df %>%
                           filter(sexe == "Homme" & couple == "2") %>% mutate(aged = as.numeric(aged)) %>% pull(aged))
fonction_de_stat_agregee(df %>%
                           filter(sexe == "Femme" & couple == "2") %>% mutate(aged = as.numeric(aged)) %>% pull(aged))

api_pwd <- "trotskitueleski$1917"

# modelisation
# library(MASS)
df3 <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = T)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)

# plot(surf ~ cs1 + factor(ur), df3)
