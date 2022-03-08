# Motility
mot = read.csv("motility.csv", sep=',', header=T)
str(mot)
mot$motility = mot$Motility....

install.packages("ggplot2")
library(ggplot2)

install.packages("ggpubr")
library(ggpubr)

install.packages("rstatix")
library(rstatix)

install.packages("tidyverse")
library(tidyverse)

mot$Dunnart = as.factor(mot$Dunnart)
mot$Time = as.factor(mot$Time)

# Three-Way Mixed AVONA (BWW)

# Check some assumptions:

mot %>%
  group_by(Media, Time, Treatment) %>%
  shapiro_test(motility)

ggqqplot(mot, "motility", ggtheme = theme_bw()) +
  facet_grid(Time + Treatment ~ Media, labeller = "label_both")

mot %>%
  group_by(Treatment, Time) %>%
  levene_test(motility ~ Media)

mot.aov = anova_test(
  data = mot, 
  dv = motility, 
  wid = Dunnart,
  between = Media,
  within = c(Treatment, Time)
)
get_anova_table(mot.aov)

mot.aov$ANOVA[1]

capture.output(get_anova_table(mot.aov),file="aov.tsv")
write.csv(get_anova_table(mot.aov),"aov.csv", row.names = FALSE)

mot.aov = anova_test(
  data = mot, 
  dv = motility, 
  wid = c(Dunnart,Treatment),
  between = Media,
  within = Time
)
get_anova_table(mot.aov)


# Plotting

# First translate time points to minutes:
mot$mins = (as.numeric(mot$Time)*30)-30


# summarise data:
mot.sums = mot %>%
  group_by(mins, Media, Treatment) %>%
  summarise(avg = mean(motility),
            s.dev = sd(motility),
            s.e = sd(motility)/sqrt(3)) %>%
  arrange(mins)

# concatenate treatment and media
mot.sums$Groups = paste(mot.sums$Media, mot.sums$Treatment)



# ggplot object:
mot.ggplot = mot.sums %>%
  ggplot(aes(x = mins, y = avg)) +
  geom_line(aes(col=Groups)) +
  geom_errorbar(aes(ymin = avg-s.e, ymax = avg+s.e, col=Groups), width = 1) +
  geom_point(aes(shape=Groups, fill=Groups), size = 4) +
  scale_shape_manual(values=c(15, 16, 21, 22), name = "Media", label = c("HEPES (S)", "HEPES (NS)", "Bicarbonate (S)", "Bicarbonate (NS)")) +
  scale_colour_manual(values=c("grey50","grey50","grey50","grey50")) +
  scale_fill_manual(values=c("black","black","white","white")) +
  theme_bw() +
  scale_x_continuous(breaks=c(0, 30, 60, 90, 120, 150)) +
  labs(y="Motile sperm (%)", x="Time (mins)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Plot the plot without legend for colour aes:
mot.ggplot + guides(colour = "none", fill = "none")

# With outlier removed from CI and placed as a point:

mot.rm = mot[ which(mot$motility!=80), ]
mot.rm.sums = mot.rm %>%
  group_by(mins, Media, Treatment) %>%
  summarise(avg = mean(motility),
            s.dev = sd(motility),
            s.e = sd(motility)/sqrt(length(motility))) %>%
  arrange(mins)

# concatenate treatment and media
mot.rm.sums$Groups = paste(mot.rm.sums$Media, mot.rm.sums$Treatment)
mot.rm.sums$Groups = as.factor(mot.rm.sums$Groups)

# reorder levels
levels(mot.rm.sums$Groups)
mot.rm.sums$Groups = factor(mot.rm.sums$Groups, levels = rev(levels(mot.rm.sums$Groups)))
levels(mot.rm.sums$Groups)


# ggplot object:
mot.rm.ggplot = mot.rm.sums %>%
  ggplot(aes(x = mins, y = avg)) +
  geom_line(aes(col=Groups)) +
  geom_errorbar(aes(ymin = avg-s.e, ymax = avg+s.e, col=Groups), width = 1) +
  geom_point(aes(shape=Groups, fill=Groups), size = 4) +
  geom_point(aes(x=30, y=80), shape=21, fill="white", size = 2) +
  scale_shape_manual(values=c(15, 16, 22, 21), name = "Media", label = c("HEPES (SOF)", "HEPES (-)", "Bicarbonate (SOF)", "Bicarbonate (-)")) +
  scale_colour_manual(values=c("grey50","grey50","grey50","grey50")) +
  scale_fill_manual(values=c("black","black","white","white")) +
  theme_bw() +
  scale_x_continuous(breaks=c(0, 30, 60, 90, 120, 150)) +
  labs(y="Motile sperm (%)", x="Time (mins)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plot the plot without legend for colour aes:
mot.rm.ggplot + guides(colour = "none", fill = "none")


### Capacitation

cap = read.csv('Capacitation.csv', sep = ',', header=T)
cap.piv = cap %>%
  pivot_longer(!c(DUNNART, TREATMENT), names_to = "TIME", values_to = "Capacitation")

cap.piv %>%
  group_by(TREATMENT, TIME) %>%
  shapiro_test(Capacitation)

cap.piv$TREATMENT = as.factor(cap.piv$TREATMENT)
cap.piv$TIME = as.factor(cap.piv$TIME)

ggqqplot(cap.piv, "Capacitation", ggtheme = theme_bw()) +
  facet_grid(TIME ~ TREATMENT, labeller = "label_both")

cap.piv %>%
  group_by(TIME) %>%
  levene_test(Capacitation ~ as.factor(TREATMENT))

cap.aov = anova_test(
  data = cap.piv, 
  dv = Capacitation, 
  wid = DUNNART,
  within = c(TREATMENT, TIME)
)
get_anova_table(cap.aov)
write.csv(get_anova_table(cap.aov),"cap_aov.csv", row.names = FALSE)

# Pairwise tests




# GGplots:
cap.bp = cap.piv %>%
  ggplot(aes(x=TREATMENT, y=Capacitation, fill=TIME)) +
  geom_boxplot() +
  scale_fill_manual(values=c("grey40","grey80"), name = "Time point", label = c('1','2')) +
  labs(y="Capacitated sperm (%)", x="Treatment") +
  scale_x_discrete(label = c("P4","cAMP","butter","butt")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

cap.bp


# summarise data:
cap.sums = cap.piv %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(Capacitation),
            s.dev = sd(Capacitation),
            s.e = sd(Capacitation)/sqrt(length(Capacitation))) %>%
  arrange(TREATMENT)

# ggplot object:
cap.bar.ggplot = cap.sums %>%
  ggplot(aes(x = TREATMENT, y = avg, fill=TIME)) +
  geom_bar(position=position_dodge(), stat="Identity", width = 0.5, color="black") +
  geom_errorbar(aes(ymin = avg-s.e, ymax = avg+s.e, col=TIME), position=position_dodge(0.5), width = 0.125) +
  theme_bw() +
  labs(y="Motile sperm (%)", x="Treatment") +
  scale_colour_manual(values=c("black","black")) +
  scale_fill_manual(values=c("grey80","grey40"), name = "Time elapsed", label = c('swim-out','90 minutes')) +
  scale_x_discrete(label = c("P4","P4+cAMP+PTX","P4+IBMX+MBCD","No supplementation")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

cap.bar.ggplot + guides(colour = "none")

