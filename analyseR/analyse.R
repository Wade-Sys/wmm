## --------------------------------------------------------------------------------------------
# Bibliotheken einbinden (Wenn notwendig bitte installieren)
library(psych)
library(MESS)
library(ggplot2)
library(DescTools)
library(corrplot)
library(dplyr)
library(reshape2)
library(car)
## --------------------------------------------------------------------------------------------
## Datenaufbereitung
## --------------------------------------------------------------------------------------------
# Daten einlesen & formatieren
df_wmm_csv <- read.csv2(file = '../data/wmm_data/daten_wmm_all_prepared.csv',header = TRUE,dec = ".",sep = ";")
df_wetter_csv <- read.csv2(file = '../data/wetter/daten_wetter_tmp.csv',header = TRUE,dec = ".",sep = ";")

df_wmm_1 <- df_wmm_csv
df_wetter_1 <- df_wetter_csv

df_wmm_1$Datum <- strptime(x = as.character(df_wmm_1$Datum),"%Y-%m-%d")
df_wetter_1$Datum <- strptime(x = as.character(df_wetter_1$Datum),"%Y-%m-%d")

# Nicht benötigte spalten entfernen (Marathondaten)
df_wmm_2 <- subset(df_wmm_1, select = -c(T_KM_5,T_KM_10,T_KM_15,T_KM_20,T_KM_HM,T_KM_25,T_KM_30,T_KM_35,T_KM_40,T_KM_FN,Startzeit,Datum_Startzeit_UTC))

# Nicht benötigte spalten entfernen (Wetterdaten)
df_wetter_2 <- subset(df_wetter_1, select = -c(TMP_MIN, TMP_MAX, TMP_MEDIAN))

# Wetterdaten: Runden
df_wetter_3 <- df_wetter_2
df_wetter_3$TMP_MEAN_RND1 <- round(df_wetter_3$TMP_MEAN, digits = 1)

# Wetter um Jahr bereinigt
df_wetter_3y <- subset(df_wetter_3, (Jahr >= 2010))
df_wetter_3y <- subset(df_wetter_3y, (Jahr != 2012))

# Wetterdaten und Marathondaten mergen (2)
df_ww3 <- merge(df_wmm_2, df_wetter_3, by = c("Jahr","Ort","Datum"))

# Neues Dataframe (normal und um die Jahre bereinigt)
df_ww3y <- subset(df_ww3, (Jahr==2010 | Jahr==2011 | Jahr > 2012))

# Nicht benötigte Dataframes entfernen
rm(df_wetter_csv, df_wetter_1, df_wetter_2, df_wetter3)
rm(df_wmm_csv, df_wmm_1, df_wmm_2)

# Neue Dataframes nach Geschlecht und Platzierung (2010,2011,2013-2019)

# Alle: 
df_ww3y_top3 <- subset(df_ww3y, (Platz <= 3))
df_ww3y_m_all <- subset(df_ww3y, (Geschlecht=='M'))
df_ww3y_m_top1 <- subset(df_ww3y, (Geschlecht=='M' & Platz == 1))
df_ww3y_m_top3 <- subset(df_ww3y, (Geschlecht=='M' & Platz <= 3))
df_ww3y_w_all <- subset(df_ww3y, (Geschlecht=='W'))
df_ww3y_w_top1 <- subset(df_ww3y, (Geschlecht=='W' & Platz == 1))
df_ww3y_w_top3 <- subset(df_ww3y, (Geschlecht=='W' & Platz <= 3))

# London: 
df_ww3y_london_m_all <- subset(df_ww3y, (Ort=='London' & Geschlecht=='M'))
df_ww3y_london_m_top3 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='M' & Platz <= 3))
df_ww3y_london_w_all <- subset(df_ww3y, (Ort=='London' & Geschlecht=='W'))
df_ww3y_london_w_top3 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='W' & Platz <= 3))

# Berlin: 
df_ww3y_berlin_m_all <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='M'))
df_ww3y_berlin_m_top3 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='M' & Platz <= 3))
df_ww3y_berlin_w_all <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='W'))
df_ww3y_berlin_w_top3 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='W' & Platz <= 3))

# Chicago: 
df_ww3y_chicago_m_all <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='M'))
df_ww3y_chicago_m_top3 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='M' & Platz <= 3))
df_ww3y_chicago_w_all <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='W'))
df_ww3y_chicago_w_top3 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='W' & Platz <= 3))

# NewYork: 
df_ww3y_newyork_m_all <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='M'))
df_ww3y_newyork_m_top3 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='M' & Platz <= 3))
df_ww3y_newyork_w_all <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='W'))
df_ww3y_newyork_w_top3 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='W' & Platz <= 3))

# Tokyo: 
df_ww3y_tokyo_m_all <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='M'))
df_ww3y_tokyo_m_top3 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='M' & Platz <= 3))
df_ww3y_tokyo_w_all <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='W'))
df_ww3y_tokyo_w_top3 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='W' & Platz <= 3))

## --------------------------------------------------------------------------------------------
## Datenanalysen
## --------------------------------------------------------------------------------------------
# Lage- und Streuungsparameter
# Männer:
describe(df_ww3y_m_top3$S_KM_FN,quant = c(.25,.75), skew=TRUE)
describeBy(df_ww3y_m_all$S_KM_FN, df_ww3y_m_all$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)
describeBy(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)

# Frauen:
describe(df_ww3y_w_top3$S_KM_FN,quant = c(.25,.75), skew=TRUE)
describeBy(df_ww3y_w_all$S_KM_FN, df_ww3y_w_all$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)
describeBy(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)

# Wetter
describeBy(df_wetter_3y$TMP_MEAN_RND1, df_wetter_3y$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)
## --------------------------------------------------------------------------------------------
# Histogramm: Verteilung der Ergebnisse
# Männer
ggplot(data = df_ww3y_m_all, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 50, color="white", fill="orange") + 
  labs(x="Zeit (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (M): TOP-10 (N=450)") + 
  scale_x_continuous(breaks = seq(7000,11000,100)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "plots/hplt_ergb_vert_m_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)
# Frauen
ggplot(data = df_ww3y_w_all, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 100, color="white", fill="skyblue") + 
  labs(x="Zeit (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (W): TOP-10 (N=450)") + 
  scale_x_continuous(breaks = seq(7000,11000,200)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "plots/hplt_ergb_vert_w_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## --------------------------------------------------------------------------------------------
# Boxplot: Ergebnisse / Wettbewerbsort (M)
ggplot(df_ww3y_m_all, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (M): TOP-10 (N=450)") +
  scale_y_continuous(breaks = seq(7000,8350,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "plots/bplt_ergb_m_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (M): TOP-3 (N=135)") +
  scale_y_continuous(breaks = seq(7000,8350,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "plots/bplt_ergb_m_n135_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Boxplot: Ergebnisse / Wettbewerbsort (W)
ggplot(df_ww3y_w_all, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (W): TOP-10 (N=450)") +
  scale_y_continuous(breaks = seq(8000,11000,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "plots/bplt_ergb_w_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (W): TOP-3 (N=135)") +
  scale_y_continuous(breaks = seq(8000,10000,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "plots/bplt_ergb_w_n135_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## --------------------------------------------------------------------------------------------
# Scatterplots:

# Männer (je Ort):
ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort)) + geom_point(alpha=1, size=2) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,8500,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Geschlecht:") +
  theme(legend.position = "bottom") +
  facet_wrap(~Ort, ncol=5) +
  theme(legend.position = "none") 
ggsave(filename = "plots/sctr_ergb_tmp_m_top3_grp.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Frauen (je Ort):
ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort)) + geom_point(alpha=1, size=2) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(8000,10000,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Geschlecht:") +
  theme(legend.position = "bottom") +
  facet_wrap(~Ort, ncol=5) +
  theme(legend.position = "none") 
ggsave(filename = "plots/sctr_ergb_tmp_w_top3_grp.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Männer (gesamt):
ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1)) + geom_point(alpha=0.8, size=3, color="orange") + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7100,10000,100)) + 
  scale_x_continuous(breaks = seq(0,25,1)) +
  scale_fill_brewer(palette="Set3") +
  #scale_color_discrete("Geschlecht:") +
  theme(legend.position = "none") 
ggsave(filename = "plots/sctr_ergb_tmp_m_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Frauen (gesamt):
ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1)) + geom_point(alpha=0.8, size=3, color="skyblue") + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7100,10000,100)) + 
  scale_x_continuous(breaks = seq(0,25,1)) +
  scale_fill_brewer(palette="Set3") +
  #scale_color_discrete("Geschlecht:") +
  theme(legend.position = "none") 
ggsave(filename = "plots/sctr_ergb_tmp_w_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## --------------------------------------------------------------------------------------------

## Temperaturverlauf - Balkendiagramm
ggplot(df_wetter_3y, aes(y=TMP_MEAN_RND1, x=Jahr, color=Ort)) + 
  geom_bar(stat = "identity") +
  geom_hline(data=aggregate(x=df_wetter_3y$TMP_MEAN_RND1, by=list(Ort=df_wetter_3y$Ort), FUN="mean"), aes(yintercept = x), color="red") + 
  labs(y="Temperatur (°C)", x="Jahr", title = "Temperaturverlauf (ausgewählte Jahre)") + 
  scale_y_continuous(breaks = seq(0,25,1.0)) + 
  scale_x_continuous(breaks = c(2010,2011,2013,2014,2015,2016,2017,2018,2019)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort") + 
  theme(axis.text.x = element_text(angle = 90),legend.position = "none") + 
  facet_wrap(~Ort, ncol=5)
ggsave(filename = "plots/bar_tmp_y_ort_wrap.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## --------------------------------------------------------------------------------------------
# Ermitteln der besten Zeiten überhaupt: Betrachtung nur der Top-1, jeweils Minumum und Maximum
subset(df_ww3y, (Platz==1), select = c(Ort, Geschlecht, S_KM_FN, TMP_MEAN_RND1, Jahr)) %>% 
  group_by(Ort, Geschlecht) %>% 
  slice(which.max(S_KM_FN)) %>%
  arrange(Geschlecht)

subset(df_ww3y, (Platz==1), select = c(Ort, Geschlecht, S_KM_FN, TMP_MEAN_RND1, Jahr)) %>% 
  group_by(Ort, Geschlecht) %>% 
  slice(which.min(S_KM_FN)) %>%
  arrange(Geschlecht)
## --------------------------------------------------------------------------------------------
## Korrelation
# Allen
round(cor(df_ww3y_top3$TMP_MEAN_RND1, df_ww3y_top3$S_KM_FN), 2)

# Männer:
round(cor(df_ww3y_m_top3$TMP_MEAN_RND1, df_ww3y_m_top3$S_KM_FN), 2)

round(cor(df_ww3y_berlin_m_top3$TMP_MEAN_RND1, df_ww3y_berlin_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_london_m_top3$TMP_MEAN_RND1, df_ww3y_london_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_chicago_m_top3$TMP_MEAN_RND1, df_ww3y_chicago_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_newyork_m_top3$TMP_MEAN_RND1, df_ww3y_newyork_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_tokyo_m_top3$TMP_MEAN_RND1, df_ww3y_tokyo_m_top3$S_KM_FN), 2)

# Frauen:
round(cor(df_ww3y_w_top3$TMP_MEAN_RND1, df_ww3y_w_top3$S_KM_FN), 2)

round(cor(df_ww3y_berlin_w_top3$TMP_MEAN_RND1, df_ww3y_berlin_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_london_w_top3$TMP_MEAN_RND1, df_ww3y_london_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_chicago_w_top3$TMP_MEAN_RND1, df_ww3y_chicago_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_newyork_w_top3$TMP_MEAN_RND1, df_ww3y_newyork_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_tokyo_w_top3$TMP_MEAN_RND1, df_ww3y_tokyo_w_top3$S_KM_FN), 2)
## --------------------------------------------------------------------------------------------
## Regressionen

## Alle Orte je Geschlecht
# Männer:
ggplot(subset(df_ww3y, (Geschlecht=="M" & Platz <= 3)), aes(y=S_KM_FN, x=TMP_MEAN_RND1, fill=Ort)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~poly(x,2)) +
  labs(title = "Ergebnisse (M): TOP-3", x="Temperatur (°C)", y="Zeit (in Sek.)", subtitle = "Zeit ~ Temperatur(x^2)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(7100,8300,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  facet_wrap(~Ort, ncol=5)
ggsave(filename = "plots/reg_p2_tmp_m_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Frauen:
ggplot(subset(df_ww3y, (Geschlecht=="W" & Platz <= 3)), aes(y=S_KM_FN, x=TMP_MEAN_RND1, fill=Ort)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~poly(x,2)) +
  labs(title = "Ergebnisse (W): TOP-3", x="Temperatur (°C)", y="Zeit (in Sek.)", subtitle = "Zeit ~ Temperatur(x^2)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(8000,9500,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  facet_wrap(~Ort, ncol=5)
ggsave(filename = "plots/reg_p2_tmp_w_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Regressionen Summary:
# Männer:
lm_berlin_m_top3_poly2 <- lm(data = df_ww3y_berlin_m_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_london_m_top3_poly2 <- lm(data = df_ww3y_london_m_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_chicago_m_top3_poly2 <- lm(data = df_ww3y_chicago_m_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_newyork_m_top3_poly2 <- lm(data = df_ww3y_newyork_m_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_tokyo_m_top3_poly2 <-lm(data = df_ww3y_tokyo_m_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))

summary(lm_berlin_m_top3_poly2)
summary(lm_london_m_top3_poly2)
summary(lm_chicago_m_top3_poly2)
summary(lm_newyork_m_top3_poly2)
summary(lm_tokyo_m_top3_poly2)

# Frauen
lm_berlin_w_top3_poly2 <- lm(data = df_ww3y_berlin_w_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_london_w_top3_poly2 <- lm(data = df_ww3y_london_w_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_chicago_w_top3_poly2 <- lm(data = df_ww3y_chicago_w_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_newyork_w_top3_poly2 <- lm(data = df_ww3y_newyork_w_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_tokyo_w_top3_poly2 <-lm(data = df_ww3y_tokyo_w_top3, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))

summary(lm_berlin_w_top3_poly2)
summary(lm_london_w_top3_poly2)
summary(lm_chicago_w_top3_poly2)
summary(lm_newyork_w_top3_poly2)
summary(lm_tokyo_w_top3_poly2)

## --------------------------------------------------------------------------------------------
## Voranalyse für den t-Test

## Leven-Test (Test auf Varianzhomogenität)
leveneTest(S_KM_FN~Ort, data = df_ww3y_m_top3)
leveneTest(S_KM_FN~Ort, data = df_ww3y_w_top3)

## QQ-Plot (Prüfen auf Normalverteilung)
ggplot(df_ww3y_m_top3, aes(sample=scale(S_KM_FN), color=factor(Ort))) + stat_qq() + stat_qq_line() +
  labs(y="Stichprobe", x="Theoretisch", title = "Prüfung auf Normalverteilung (Zeit)", 
       subtitle = "Ergebnisse (M): TOP-3") +
  scale_fill_brewer(palette="Set3") +
  theme(legend.position = "none") +
  facet_wrap(~Ort, ncol = 3)
ggsave(filename = "plots/qq_norm_m_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)


ggplot(df_ww3y_w_top3, aes(sample=scale(S_KM_FN), color=factor(Ort))) + stat_qq() + stat_qq_line() +
  labs(y="Stichprobe", x="Theoretisch", title = "Prüfung auf Normalverteilung (Zeit)", 
       subtitle = "Ergebnisse (W): TOP-3") +
  scale_fill_brewer(palette="Set3") +
  theme(legend.position = "none") +
  facet_wrap(~Ort, ncol = 3)
ggsave(filename = "plots/qq_norm_w_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)
## --------------------------------------------------------------------------------------------
## t-Welch-Test

# pairwise-test: two.sided
pairwise.t.test(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort, p.adjust.method = "bonferroni", alternative = "two.sided", paired = FALSE, pool.sd = TRUE, conf.level = 0.95)
pairwise.t.test(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort, p.adjust.method = "bonferroni", alternative = "two.sided", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)

# pairwise-test: greater
pairwise.t.test(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort, p.adjust.method = "bonferroni", alternative = "greater", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)
pairwise.t.test(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort, p.adjust.method = "bonferroni", alternative = "greater", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)

## --------------------------------------------------------------------------------------------