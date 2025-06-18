options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("labelled")
install.packages("GGally")
install.packages("lubridate")
install.packages("forcats")
install.packages("gtsummary")
library(tidyverse)
library(car)
library(cardx)
library(ggplot2)
library(labelled)
library(gtsummary)
library(ggstatsplot)
library(Hmisc)
data("trial")
theme_gtsummary_language("fr",decimal.mark = ",", big.mark = " ")
View(trial)
trial%>%
  look_for()

trial|>nrow()

str(trial)

#Recodage des variables et labellisation

trial <- trial %>%
  mutate(
    trt = as.character(trt),
    stage = as.character(stage),
    grade = as.character(grade),
    response=as.character(response),
    death=as.character(death)
  ) %>%
  set_variable_labels(
    trt = "Traitement attribué",
    age = "Age du patient",
    stage = "Stade de la maladie",
    grade = "Gravité de la maladie",
    response="Réponse de la tumeur",
    death="En vie ou mort",
    ttdeath="Total décès",
    marker="Marqueur sanguin"
  ) %>%
  set_value_labels(
    trt = c("A" = "Drug A", "B" = "Drug B"),
    stage = c("0" = "T1", "1" = "T2", "2" = "T3", "3" = "T4"),
    grade = c("Gravité1" = "I", "Gravité2" = "II", "Gravité3" = "III")
  ) %>%
  mutate(
    trt = labelled::to_factor(trt),
    stage = labelled::to_factor(stage),
    grade = labelled::to_factor(grade)
  )

# Forcer la mise à jour des étiquettes (facultatif)
force(trial)

trial %>%
  tbl_summary()%>%
  modify_header(label = "**Variables**")


trial %>%
  tbl_summary(by="trt")%>%
  modify_header(label = "**Variables**")



library(GGally)

trial|>ggbivariate(outcome = "trt", explanatory = c("age", "marker","grade", "stage","ttdeath","death"))

trial|>ggtable(columnsY = c("age", "marker","grade", "stage","ttdeath","death"), columnsX = "trt",
               fill="std.resid",
               cells="row.prop",
               )





#library(tidyverse)
#library(labelled)

#data("trial")

#trial_labeled <- trial %>%
#  set_variable_labels(
#    trt = "Traitement attribué",
#    age = "Age du patient",
#    marker = "Marqueur sanguin",
#    grade = "Gravité de la maladie",
#    stage = "Stade de la maladie",
#    ttdeath = "Temps jusqu'au décès",
#    death = "Décès"
#  )

apply_labels_ggbivariate <- function(p, data, outcome, explanatory) {
  outcome_label <- var_label(data[[outcome]])
  for (var in explanatory) {
    var_label_text <- var_label(data[[var]])
    if (!is.null(var_label_text)) {
      p <- p + labs(x = var_label_text, y = outcome_label)
    }
  }
  return(p)
}

plot <- trial %>%
  ggbivariate(
    outcome = "trt",
    explanatory = c("age", "marker", "grade", "stage", "ttdeath", "death")
  )

plot <- apply_labels_ggbivariate(plot, trial, "trt", c("age", "marker", "grade", "stage", "ttdeath", "death"))

print(plot)

library(ggplot2)

ggplot(trial) +
  aes(x = age, y = trt, fill = grade) +
  geom_boxplot() +
  scale_fill_manual(values = c(I = "#F8766D", 
                               II = "#00C19F", III = "#FF61C3")) +
  labs(x = "Age", y = "Traitement", title = "Répartition des traitements (trt) en fonction de l'âge, selon la gravité de la maladie (grade)", 
       caption = "FormationR", fill = "Gravité de la tumeur") +
  theme_linedraw() +
  theme(axis.text.y = element_text(face = "bold"), 
        axis.text.x = element_text(face = "bold", size = 15L), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))


#Modèle de regression linéaire simple et multiple.


# Préparation des données : sélection des variables et suppression des valeurs manquantes
trial_regression <- trial %>%
  select(age, marker,ttdeath) %>%
  drop_na()

# Création du modèle
modele_regression <- lm(ttdeath ~ marker+age, data = trial_regression)

# Affichage du résumé du modèle
summary(modele_regression)


library(tidyverse)
library(gtsummary)

data("trial")

# Conversion des variables catégorielles en facteurs
trial$stage <- as.factor(trial$stage)
trial$grade <- as.factor(trial$grade)
trial$trt <- as.factor(trial$trt)
trial$response <- as.factor(trial$response)

# Gestion des valeurs manquantes
trial <- trial %>% drop_na()

# Vérification des types de variables
str(trial)

# Modèle de régression logistique
modele_response <- glm(response ~ age + marker + stage + grade + trt, data = trial, family = "binomial")

# Affichage du résumé du modèle
summary(modele_response)
installed.packages("broom.helpers")
library(broom.helpers)
modele_response%>%
  tbl_regression(
    exponentiate=TRUE, add_estimate_to_reference_rows = TRUE
  )%>%
  add_global_p()




library(tidyverse)
library(gtsummary)

data("trial")

# Préparation des données : sélection des variables et conversion en facteurs
trial_anova <- trial %>%
  select(age, trt, grade) %>%
  mutate(
    trt = as.factor(trt),
    grade = as.factor(grade)
  ) %>%
  drop_na()


# ANOVA à un facteur : age ~ trt
modele_anova_1 <- aov(age ~ trt, data = trial_anova)

# Affichage du résumé du modèle
summary(modele_anova_1)


# ANOVA à deux facteurs : age ~ trt + grade
modele_anova_2 <- aov(age ~ trt + grade, data = trial_anova)

# Affichage du résumé du modèle
summary(modele_anova_2)


# ANOVA à deux facteurs avec interaction : age ~ trt * grade
modele_anova_inter <- aov(age ~ trt * grade, data = trial_anova)

# Affichage du résumé du modèle
summary(modele_anova_inter)


# Graphiques de diagnostic
plot(modele_anova_1)
plot(modele_anova_2)
plot(modele_anova_inter)

# Test de Levene pour l'homogénéité des variances
library(car)
leveneTest(age ~ trt, data = trial_anova)
leveneTest(age ~ trt * grade, data = trial_anova)




library(tidyverse)
library(survival)
library(survminer)
library(gtsummary)

data("trial")

# Préparation des données : sélection des variables et conversion en facteurs
trial_survie <- trial %>%
  select(ttdeath, death, trt) %>%
  mutate(
    trt = as.factor(trt)
  ) %>%
  drop_na()


# Création de l'objet de survie
objet_survie <- Surv(time = trial_survie$ttdeath, event = trial_survie$death)


# Estimation des courbes de survie
modele_survie <- survfit(objet_survie ~ trt, data = trial_survie)


# Visualisation des courbes de survie
ggsurvplot(modele_survie, data = trial_survie,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE,
           ggtheme = theme_bw())


# Test du log-rank
test_logrank <- survdiff(objet_survie ~ trt, data = trial_survie)
test_logrank


library(tidyverse)
library(caret)
library(gtsummary)
library(ggplot2)
library(pROC)

data("trial")

# Préparation des données (comme précédemment)
trial_ml <- trial %>%
  select(age, marker, stage, grade, trt, response) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

set.seed(123)
trainIndex <- createDataPartition(trial_ml$response, p = 0.7, list = FALSE)
train_data <- trial_ml[trainIndex, ]
test_data <- trial_ml[-trainIndex, ]

# Modèle de régression logistique
modele_logistique <- glm(response ~ ., data = train_data, family = "binomial")

# Prédictions sur l'ensemble de test
predictions_logistique_prob <- predict(modele_logistique, newdata = test_data, type = "response")
predictions_logistique <- ifelse(predictions_logistique_prob > 0.5, "1", "0")

# Convertir les prédictions et les valeurs réelles en facteurs
predictions_logistique <- as.factor(predictions_logistique)
test_data$response <- as.factor(test_data$response)

# Trouver et appliquer les mêmes niveaux
unique_levels <- unique(c(levels(predictions_logistique), levels(test_data$response)))

predictions_logistique <- factor(predictions_logistique, levels = unique_levels)
test_data$response <- factor(test_data$response, levels = unique_levels)

# Matrice de confusion
cm <- confusionMatrix(predictions_logistique, test_data$response)

# Visualisation de la matrice de confusion (exemple avec ggplot2)
cm_data <- data.frame(
  Prediction = predictions_logistique,
  Reference = test_data$response
)

ggplot(cm_data, aes(x = Reference, fill = Prediction)) +
  geom_bar(position = "dodge") +
  labs(title = "Matrice de confusion", x = "Valeurs réelles", y = "Nombre de prédictions")

# Courbe ROC
roc_obj <- roc(test_data$response, predictions_logistique_prob)
plot(roc_obj, main = "Courbe ROC")

# Importance des variables (exemple avec les forêts aléatoires)
modele_foret <- train(response ~ ., data = train_data, method = "rf")
importance <- varImp(modele_foret)
plot(importance, main = "Importance des variables")

