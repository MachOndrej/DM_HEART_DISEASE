set.seed(7)
# install.packages('caret', repos = 'http://cran.us.r-project.org', dependencies = TRUE)
# IMPORT KNIHOVEN
library(dplyr)
library(mlbench)
library(caret) # korelace a další fce
library(ggplot2)

# Práce s daty a jejich příprava
df <- read.csv("HEART.txt", sep = "\t", header = TRUE)

# Zobrazení dat v tabulce
# View(df)

# "Uzel Type" - změna typu některých sloupců
df <- df %>% # knihovna dplyr
    mutate(
        Hladina.cukru.nad.120.mg.dl = ifelse(Hladina.cukru.nad.120.mg.dl == "cukr>120mg/dl", 1, 0),
        Indikace.srdecni.choroby = as.factor(Indikace.srdecni.choroby),
        Bolest.hrudniku = as.factor(Bolest.hrudniku),
        Vysledky.EKG = as.factor(Vysledky.EKG),
        Sklon.krivky.pri.stres.testu = as.factor(Sklon.krivky.pri.stres.testu),
        Pocet.velkych.cev.oznacenych.barvivem = as.factor(Pocet.velkych.cev.oznacenych.barvivem),
        Vysledek.predchoziho.vysetreni = as.factor(Vysledek.predchoziho.vysetreni)
    )
# Kontrola datových typů sloupců
# str(df)

# Určete průměrnou Hladinu cholesterolu v krvi u pacientů nad 50 let.
df_subset <- df[df$Vek >= 50, ]
avg_chol <- mean(df_subset$Hladina.cholesterolu.v.krvi..mg.dl.)
print(avg_chol)

# Získání Mean, Min, Max
means <- colMeans(df[c(
    "Vek", "Klidovy.krevni.tlak..mm.",
    "Hladina.cholesterolu.v.krvi..mg.dl.",
    "Maximaln.srdecni.tep", "Hodnota.stres.testu"
)], na.rm = TRUE)
max_values <- apply(df[c(
    "Vek", "Klidovy.krevni.tlak..mm.",
    "Hladina.cholesterolu.v.krvi..mg.dl.", "Maximaln.srdecni.tep",
    "Hodnota.stres.testu"
)], MARGIN = 2, max, na.rm = TRUE)
min_values <- apply(df[c(
    "Vek", "Klidovy.krevni.tlak..mm.",
    "Hladina.cholesterolu.v.krvi..mg.dl.", "Maximaln.srdecni.tep",
    "Hodnota.stres.testu"
)], MARGIN = 2, min, na.rm = TRUE)
# Zobrazeni hodnot
info_df <- data.frame(Mean = means, Minimum = min_values, Maximum = max_values)
print(info_df)

# Histogram Indikace srdeční choroby
hist_indikace <- ggplot(df, aes(x = Indikace.srdecni.choroby, fill = Indikace.srdecni.choroby)) +
    geom_bar(stat = "count") +
    labs(
        x = "Pozitivní a negativní výsledky", y = "Počet", title = "Histogram indikace srdeční choroby",
        fill = "Výsledek:"
    ) +
    scale_fill_manual(labels = c("Negativní", "Pozitivní"), values = c("blue", "red")) +
    coord_cartesian() +
    theme(
        axis.text.x = element_blank(), axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 23), axis.title.y = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)
    )
ggsave(filename = "hist_indikace.png", plot = hist_indikace)
# print(hist_indikace)

# Plot Klidový krevní tlak x Maximální srdeční tep
plot_tlak_tep <- ggplot(data = df, aes(
    x = Klidovy.krevni.tlak..mm.,
    y = Maximaln.srdecni.tep,
    colour = Indikace.srdecni.choroby
)) +
    geom_point(size = 5) +
    labs(
        x = "Klidovy krevni tlak", y = "Maximalni srdecni tep",
        title = "Klidový krevní tlak\nx\nMax. srdeční tep",
        color = "Výsledek:"
    ) +
    coord_cartesian() +
    scale_color_manual(labels = c("Negativní", "Pozitivní"), values = c("blue", "red")) +
    theme(
        axis.text.x = element_text(size = 23), axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 23), axis.title.y = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)
    )
ggsave(filename = "plot_tlak_tep.png", plot = plot_tlak_tep)
# print(plot_tlak_tep)

# FEATURE SELECTION - knihovny: caret a mlbench
# Control tvoří random forest selection function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Stanovení predicors a target
predictors <- df[, 1:13]
target <- df[, 14]

# Provedení feature selection pomocí rfe()
model <- rfe(predictors, target, sizes = c(1:13), rfeControl = control)

# Výsledky modelu
print(model)
# Relevantní prediktory
predictors(model)

# Graf výsledků
fs_results_plot <- ggplot(data = model) +
    geom_point(size = 5) +
    labs(
        x = "Prediktory", y = "Přesnost (Cross-Validation)",
        title = "Feature Selection"
    ) +
    scale_x_discrete(limits = factor(c(1:13))) +
    theme(
        axis.text.x = element_text(size = 23), axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 23), axis.title.y = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold")
    )
print(fs_results_plot)
ggsave(filename = "feature_select.png", plot = fs_results_plot)

# Odečtení "nepodstatných" sloupců dat
new_df <- df[, -6]
# View(new_df)
write.table(new_df, file = "HEART2.csv", sep = "\t", row.names = FALSE)
