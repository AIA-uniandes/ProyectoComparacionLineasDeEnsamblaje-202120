simu <- read.csv('Simulaciones.csv', header=T, sep = ";")

# Flowtime
ggplot(data = simu, aes(x = Seru, y = FT, color = Seru)) +
  geom_boxplot() +
  theme_bw() + ylab("Tiempo de ciclo (min)") + xlab("Sistema de Producción")

# Throughput
ggplot(data = simu, aes(x = Seru, y = TH, color = Seru)) +
  geom_boxplot() +
  theme_bw() + ylab("Throughput (und/hora)") + xlab("Sistema de Producción")

#WIP
ggplot(data = simu, aes(x = Seru, y = WIP, color = Seru)) +
  geom_boxplot() +
  theme_bw() + ylab("WIP (und)") + xlab("Sistema de Producción")

FT_anova <- aov(simu$FT ~ simu$Seru)
summary(FT_anova)
plot(FT_anova)

aov_residuals <- residuals(object = FT_anova )
shapiro.test(x = aov_residuals )

library(car)
leveneTest(simu$FT ~ simu$Seru)

oneway.test(simu$FT ~ simu$Seru)
pairwise.t.test(simu$FT, simu$Seru,
                p.adjust.method = "BH", pool.sd = FALSE)

kruskal.test(simu$FT ~ simu$Seru)
leveneTest(simu$FT ~ simu$Seru, center = "median")

## subset TH - linea/divisional

subset_TT_linea <- subset(simu, Seru == "DivisionalPush" | Seru == "Linea")
library("ggpubr")
ggboxplot(subset_TT_linea, x = "Seru", y = "TH", 
          color = "Seru", palette = c("#00AFBB", "#E7B800"),
          ylab = "Thorughput", xlab = "Configuración")


TH.ftest_linea <- var.test(TH ~ Seru, data = subset_TT_linea)
TH.ftest_linea
t.test(TH ~ Seru, data = subset_TT_linea, var.equal = TRUE)
t.test(TH ~ Seru, data = subset_TT_linea, alternative = "greater")

## subset todos menos pull

subset_TT <- subset(simu, Seru != "DivisionalPull")
library("ggpubr")
ggboxplot(subset_TT, x = "Seru", y = "TH", 
          color = "Seru",
          ylab = "Throughput", xlab = "Configuración")

ggplot(data = subset_TT, aes(x = Seru, y = TH, color = Seru)) +
  geom_boxplot() +
  theme_bw() + ylab("Throughput (und/hora)") + xlab("Sistema de Producción")

## un-paired wilcoxon tests

# divisional y rotativo

subset_div_rot <- subset(simu, Seru == "DivisionalPush" | Seru == "Rotativo")
wilcox.test(TH ~ Seru, data = subset_div_rot, alternative = "less")

# rotativo y yatai
subset_div_yat <- subset(simu, Seru == "Yatai" | Seru == "Rotativo")
wilcox.test(TH ~ Seru, data = subset_div_yat, alternative = "less")
