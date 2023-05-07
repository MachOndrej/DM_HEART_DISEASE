set.seed(7)
# Instalace knihoven - install.packages("library_name")
# install.packages('caret', repos = 'http://cran.us.r-project.org', dependencies = TRUE)
# IMPORT KNIHOVEN
# install.packages(caret)
library(dplyr)
library(mlbench)
library(caret) # korelace a další fce
library(ggplot2)
# library(CHAID)
# DATA PREPARATION
df <- read.csv("HEART.txt", sep = "\t", header = TRUE)
# data <- read.csv("HEART", header = TRUE, fileEncoding = "UTF-8")
# Zobrazení dat
# View(df)      # zobrazení dat v tabulce
# Type node
# Convert columns to different data types
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
# Check the data types of the columns
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
means_df <- data.frame(Mean = means, Minimum = min_values, Maximum = max_values)
print(means_df)

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
# ggsave(filename = "hist_indikace.png", plot = hist_indikace)
# print(hist_indikace)

# Plot Klidovy krevni tlax x Max. srdecni tep
plot_tlak_tep <- ggplot(data = df, aes(
    x = Klidovy.krevni.tlak..mm.,
    y = Maximaln.srdecni.tep,
    colour = Indikace.srdecni.choroby
)) +
    geom_point(size = 5) +
    labs(
        x = "Klidovy krevni tlak", y = "Maximalni srdecni tep", title = "Klidový krevní tlak\nx\nMax. srdeční tep",
        color = "Výsledek:"
    ) +
    # theme(aspect.ratio = 0.5) +
    coord_cartesian() +
    scale_color_manual(labels = c("Negativní", "Pozitivní"), values = c("blue", "red")) +
    theme(
        axis.text.x = element_text(size = 23), axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 23), axis.title.y = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)
    )
# ggsave(filename = "plot_tlak_tep.png", plot = plot_tlak_tep)
# print(plot_tlak_tep)

# FEATURE SELECTION - knihovny: caret a mlbench

#df <- na.omit(df)
#View(df)
# na numeric
df[] <- lapply(df, as.numeric)
# korelacni matice
cor_matrix <- cor(df[, 1 :(ncol(df) - 1)])   # Target je posledni, pro zbytek udelam korelaci
#cor_matrix <- cor(df)
# Targert
#cor_target <- cor_matrix["x", "y"] - cor_matrix["x", "z"] * cor_matrix["z", "y"]

# find attributes that are highly corrected (ideally >0.75)
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)
print(highly_correlated)