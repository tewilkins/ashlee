casa = read.table("CASA.csv", sep = ",", header = T)

install.packages("rstatix")

library(ggplot2)
library(rstatix)
library(tidyverse)

casa$DUNNART = as.factor(casa$DUNNART)
casa$TREATMENT = as.factor(casa$TREATMENT)
casa = casa[-c(28:39),]
casa.trim = subset(casa, TIME != 0)
casa.trim$TIME = as.factor(casa.trim$TIME)

casa.0alt = 

# Two-way Repeated Measures ANOVA 
# Treatment and Time are both repeated elements of the ID Dunnart

# Motility
casa.aov <- anova_test(
  data = casa.trim, dv = MOTILE, wid = DUNNART,
  within = c(TREATMENT,TIME)
)
get_anova_table(casa.aov)

# Pairwise comparisons for time variable
casa.trim %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    MOTILE ~ TIME, paired = TRUE, 
    p.adjust.method = "fdr"
  )
# Pairwise comparisons for time variable
casa.trim %>%
  group_by(TIME) %>%
  pairwise_t_test(
    MOTILE ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "fdr"
  )


# Progressive motility
casa.aov <- anova_test(
  data = casa.trim, dv = PROGRESSIVE, wid = DUNNART,
  within = c(TREATMENT,TIME)
)
get_anova_table(casa.aov)

# Pairwise comparisons for time variable
casa.trim %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    PROGRESSIVE ~ TIME, paired = TRUE, 
    p.adjust.method = "fdr"
  )
# Pairwise comparisons for time variable
casa.trim %>%
  group_by(TIME) %>%
  pairwise_t_test(
    PROGRESSIVE ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "fdr"
  )


# Forward progressive
# pairwise t-tests from point 0 to 1:

casa$TREATMENT = as.numeric(casa$TREATMENT)
casa.prog1 = casa[ which(casa$TIME!=2), ]
casa.prog1$TREATMENT[casa.prog1$TREATMENT == 1] = "t1"
casa.prog1$TREATMENT[casa.prog1$TREATMENT == 2] = "t2"
casa.prog1$TREATMENT[casa.prog1$TREATMENT == 3] = "t3"
casa.prog1$TREATMENT[casa.prog1$TREATMENT == 4] = "t4"
mycols = c("DUNNART", "TREATMENT", "PROGRESSIVE")
casa.prog1 = casa.prog1[mycols]
casa.prog1 = casa.prog1 %>%
  pivot_wider(names_from = TREATMENT, values_from = PROGRESSIVE)

t.test(casa.prog1$C, casa.prog1$t1, paired = T)
t.test(casa.prog1$C, casa.prog1$t2, paired = T)
t.test(casa.prog1$C, casa.prog1$t3, paired = T)
t.test(casa.prog1$C, casa.prog1$t4, paired = T)

# for P4 and Control:

t.test(casa.prog1$t1, casa.prog1$t4, paired = T)
t.test(casa.prog1$t2, casa.prog1$t4, paired = T)


# pairwise t-tests from point 0 to 2:

casa.prog2 = casa[ which(casa$TIME!=1), ]
casa.prog2$TREATMENT[casa.prog2$TREATMENT == 1] = "t1"
casa.prog2$TREATMENT[casa.prog2$TREATMENT == 2] = "t2"
casa.prog2$TREATMENT[casa.prog2$TREATMENT == 3] = "t3"
casa.prog2$TREATMENT[casa.prog2$TREATMENT == 4] = "t4"
mycols = c("DUNNART", "TREATMENT", "PROGRESSIVE")
casa.prog2 = casa.prog2[mycols]
casa.prog2 = casa.prog2 %>%
  pivot_wider(names_from = TREATMENT, values_from = PROGRESSIVE)

t.test(casa.prog2$C, casa.prog2$t1, paired = T)
t.test(casa.prog2$C, casa.prog2$t2, paired = T)
t.test(casa.prog2$C, casa.prog2$t3, paired = T)
t.test(casa.prog2$C, casa.prog2$t4, paired = T)



# Ashlee's special requests:




# Motility
# pairwise t-tests from point 0 to 1:

casa.mot1 = casa[ which(casa$TIME!=2), ]
casa.mot1$TREATMENT[casa.mot1$TREATMENT == 1] = "t1"
casa.mot1$TREATMENT[casa.mot1$TREATMENT == 2] = "t2"
casa.mot1$TREATMENT[casa.mot1$TREATMENT == 3] = "t3"
casa.mot1$TREATMENT[casa.mot1$TREATMENT == 4] = "t4"
mycols = c("DUNNART", "TREATMENT", "MOTILE")
casa.mot2 = casa.mot1[mycols]
casa.mot2 = casa.mot1 %>%
  pivot_wider(names_from = TREATMENT, values_from = MOTILE)

t.test(casa.mot1$C, casa.mot1$t1, paired = T)
t.test(casa.mot1$C, casa.mot1$t2, paired = T)
t.test(casa.mot1$C, casa.mot1$t3, paired = T)
t.test(casa.mot1$C, casa.mot1$t4, paired = T)

# for P4 and Control:

t.test(casa.mot1$t1, casa.mot1$t4, paired = T)
t.test(casa.mot1$t2, casa.mot1$t4, paired = T)
t.test(casa.mot1$t3, casa.mot1$t4, paired = T)

# pairwise t-tests from point 0 to 2:

casa.mot = casa[ which(casa$TIME!=1), ]
casa.mot$TREATMENT[casa.mot$TREATMENT == 1] = "t1"
casa.mot$TREATMENT[casa.mot$TREATMENT == 2] = "t2"
casa.mot$TREATMENT[casa.mot$TREATMENT == 3] = "t3"
casa.mot$TREATMENT[casa.mot$TREATMENT == 4] = "t4"
mycols = c("DUNNART", "TREATMENT", "MOTILE")
casa.mot = casa.mot[mycols]
casa.mot2 = casa.mot %>%
  pivot_wider(names_from = TREATMENT, values_from = MOTILE)

t.test(casa.mot2$C, casa.mot2$t1, paired = T)
t.test(casa.mot2$C, casa.mot2$t2, paired = T)
t.test(casa.mot2$C, casa.mot2$t3, paired = T)
t.test(casa.mot2$C, casa.mot2$t4, paired = T)

# for P4 and Control:

t.test(casa.mot2$t1, casa.mot2$t4, paired = T)
t.test(casa.mot2$t2, casa.mot2$t4, paired = T)


# Generating summary stats for PROGRESSIVE plot:
prog.sum = casa %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(PROGRESSIVE), 
            s.dev = sd(PROGRESSIVE), 
            s.error = sd(PROGRESSIVE)/sqrt(3)) %>%
  arrange(TIME)

prog.sum$Time = round((prog.sum$TIME^1.32)*30, 0)

# Need to duplicate T0 for each rep:
prog.sum = rbind(prog.sum, prog.sum[rep(1, 3), ])
prog.sum$TREATMENT[1] = 1
prog.sum$TREATMENT[10] = 2
prog.sum$TREATMENT[11] = 3
prog.sum$TREATMENT[12] = 4
prog.sum$TREATMENT = as.factor(prog.sum$TREATMENT)

prog.p = ggplot(prog.sum, aes(x = Time, y = avg)) + 
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#000000"), name = 'Treatment', label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) + 
  geom_line(aes(col=TREATMENT), size = 0.5) + 
  geom_errorbar(aes(ymin = avg-s.error, ymax = avg+s.error, col=TREATMENT), width = 1) + 
  geom_point(aes(col=TREATMENT), size = 1.5) + 
  theme_classic() + 
  scale_x_continuous(breaks=c(0,30,75)) +
  labs(y="Forward progressive sperm (%)", x="Time (mins)")
prog.p

prog.p = prog.sum %>%
  ggplot(aes(x = Time, y = avg)) +
  geom_line(aes(col=TREATMENT)) +   
  geom_point(aes(shape=TREATMENT), size = 4) +
  geom_errorbar(aes(ymin = avg-s.error, ymax = avg+s.error, col=TREATMENT), width = 1) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = 'Treatment', label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_color_manual(values=c("#000000", "#000000", "#000000", "#000000")) + 
  theme_bw() +
  scale_x_continuous(breaks=c(0,30,75)) +
  labs(y="Forward progressive sperm (%)", x="Time (mins)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
prog.p + guides(colour = FALSE)

# Generating summary stats for PROGRESSIVE plot:
mot.sum = casa %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(MOTILE), 
            s.dev = sd(MOTILE), 
            s.error = sd(MOTILE)/sqrt(3)) %>%
  arrange(TIME)

mot.sum$Time = round((mot.sum$TIME^1.32)*30, 0)

# Need to duplicate T0 for each rep:
mot.sum = rbind(mot.sum, mot.sum[rep(1, 3), ])
mot.sum$TREATMENT[1] = 1
mot.sum$TREATMENT[10] = 2
mot.sum$TREATMENT[11] = 3
mot.sum$TREATMENT[12] = 4
mot.sum$TREATMENT = as.factor(mot.sum$TREATMENT)


mot.p = mot.sum %>%
  ggplot(aes(x = Time, y = avg)) +
  geom_line(aes(col=TREATMENT)) +   
  geom_point(aes(shape=TREATMENT), size = 4) +
  geom_errorbar(aes(ymin = avg-s.error, ymax = avg+s.error, col=TREATMENT), width = 1) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = 'Treatment', label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_color_manual(values=c("#000000", "#000000", "#000000", "#000000")) + 
  theme_bw() +
  scale_x_continuous(breaks=c(0,30,75)) +
  labs(y="Motile sperm (%)", x="Time (mins)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
mot.p + guides(colour = FALSE)

# Boxplots

ggplot(casa.trim, aes(x=TREATMENT, y=MOTILE, fill=TIME)) + 
  geom_boxplot() +
  labs(x='Treatment', y='Motile Sperm (%)') +
  scale_x_discrete(label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_fill_manual(values = c('grey50', 'grey90'), name = 'Time Elapsed', label = c('30 mins', '75 mins')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(casa.trim, aes(x=TREATMENT, y=PROGRESSIVE, fill=TIME)) + 
  geom_boxplot() +
  labs(x='Treatment', y='Forward Progressive Sperm (%)') +
  scale_x_discrete(label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No supplementation')) +
  scale_fill_manual(values = c('grey50', 'grey90'), name = 'Time Elapsed', label = c('30 mins', '75 mins')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


tshape = read.csv("Researcher Tshape comparison.csv", sep=",", header=T)
