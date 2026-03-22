#### instalar e carregar pacotes ####
install.packages(c("car", "ggplot2", "lavaan", "semPlot", "readr"))

library(car)
library(ggplot2)
library(lavaan)
library(semPlot)
library(readr)

#### carregar o banco ####
bancoANOVA <- read.csv(
  url(
    "https://raw.githubusercontent.com/maynarapriscila/metodoquantitativocritico/refs/heads/main/bancoANOVA.csv"
  ), sep = ";"
)



bancoMIMIC <- read.csv(
  url(
   "https://raw.githubusercontent.com/maynarapriscila/metodoquantitativocritico/refs/heads/main/bancoMIMIC.csv"
  ), sep = ";"
)


#### análise ANOVA ####
ANOVAtest <- aov(CUIDADO ~ GRUPO, data = bancoANOVA)
ANOVAtest
summary(ANOVAtest)
leveneTest(CUIDADO ~ GRUPO, data = bancoANOVA)
shapiro.test(resid(ANOVAtest))
TukeyHSD(ANOVAtest)

#### gráficos ANOVA ####
ggplot(bancoANOVA, aes(x = CUIDADO, fill = GRUPO)) +
  geom_histogram(color = "black", binwidth = 5) +
  facet_grid(GRUPO ~ ., scales = "free_y") +
  labs(y = 'Frequência') +
  scale_fill_manual(values = c("#0f8bf7", "#ff12d0", "#2a8008")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = 'top',
        axis.line = element_line(colour = "black"))


#### Análise MIMIC nível latente ####
MIMIC <- 'CD =~ CD1 + CD2 + CD3 + CD4 + CD5 + CD6 + CD7 + CD8 + CD9 + CD10 + CD11 + CD12
CD ~ GRUPO'

MIMICFit <- sem(MIMIC,
                data = bancoMIMIC,
                estimator = 'WLSMV',
                ordered = TRUE)

summary(MIMICFit, fit.measures = T, standardized = T)

#### Análise MIMIC nível itens ####
MIMIC2 <- 'CD =~ CD1 + CD2 + CD3 + CD4 + CD5 + CD6 + CD7 + CD8 + CD9 + CD10 + CD11 + CD12
CD ~ GRUPO
CD9 ~ GRUPO'

MIMIC2Fit <- sem(MIMIC2,
                 data = bancoMIMIC,
                 estimator = 'WLSMV',
                 ordered = TRUE)

summary(MIMIC2Fit, fit.measures = T, standardized = T)

