casa = read.table("CASA.csv", sep = ",", header = T)

install.packages("rstatix")

library(ggplot2)
library(rstatix)
library(tidyverse)

casa$DUNNART = as.factor(casa$DUNNART)
casa = casa[-c(28:39),]
casa.trim = subset(casa, TIME != 0)
casa.trim$TIME = as.factor(casa.trim$TIME)

casa.0alt = rbind(casa, casa[rep(1, 3), ])

casa.0alt$TREATMENT[1] = 1
casa.0alt$TREATMENT[28] = 2
casa.0alt$TREATMENT[29] = 3
casa.0alt$TREATMENT[30] = 4

casa.0alt = rbind(casa.0alt, casa.0alt[rep(10, 3), ])

casa.0alt$TREATMENT[10] = 1
casa.0alt$TREATMENT[31] = 2
casa.0alt$TREATMENT[32] = 3
casa.0alt$TREATMENT[33] = 4

casa.0alt = rbind(casa.0alt, casa.0alt[rep(19, 3), ])

casa.0alt$TREATMENT[19] = 1
casa.0alt$TREATMENT[34] = 2
casa.0alt$TREATMENT[35] = 3
casa.0alt$TREATMENT[36] = 4

# Two-way Repeated Measures ANOVA 
# Treatment and Time are both repeated elements of the ID Dunnart

######################### Motility #########################
mot.aov <- anova_test(
  data = casa.trim, dv = MOTILE, wid = DUNNART,
  within = c(TREATMENT,TIME)
)
get_anova_table(mot.aov)
mot.aov$CASA = "MOTILE"

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

# pairwise with t0 comp:

###
### Using 0alt data
###


mot0.aov <- anova_test(
  data = casa.0alt, dv = MOTILE, wid = DUNNART,
  within = c(TREATMENT, TIME)
)
get_anova_table(mot0.aov)

# Pairwise comparisons for time variable
mot.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    MOTILE ~ TIME, paired = TRUE, 
    p.adjust.method = "fdr"
  )

write.csv(na.omit(mot.time.pv), file = "Motile-Time")

# Pairwise comparisons for time variable
mot.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    MOTILE ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "fdr"
  )

######################### Progressive motility #########################
prog.aov = anova_test(
  data = casa.trim, dv = PROGRESSIVE, wid = DUNNART,
  within = c(TREATMENT,TIME)
)
get_anova_table(prog.aov)
prog.aov$CASA = "PROGRESSIVE"


###
### Using 0alt data
###

# Progressive motility
casa0.aov <- anova_test(
  data = casa.0alt, dv = PROGRESSIVE, wid = DUNNART,
  within = c(TREATMENT, TIME)
)
get_anova_table(casa0.aov)

# Pairwise comparisons for time variable
prog.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    PROGRESSIVE ~ TIME, paired = TRUE, 
    p.adjust.method = "fdr"
  )
prog.time.pv

# Pairwise comparisons for time variable
prog.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    PROGRESSIVE ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "fdr"
  )

######################### Rapid motility #########################
rapid.aov = anova_test(data = casa.trim, dv = RAPID, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(rapid.aov)
rapid.aov$CASA = "RAPID"

# Pairwise comparisons for time variable
casa.0alt$RAPIDr = casa.0alt$RAPID

for (i in 1:nrow(casa.0alt)){
  casa.0alt$RAPIDr[i] = casa.0alt$RAPID[i] + (runif(1)/1000) - (runif(1)/1000)
}

rapid.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    RAPIDr ~ TIME, paired = TRUE, 
    p.adjust.method = "fdr"
  )
rapid.time.pv

# Pairwise comparisons for time variable
rapid.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    RAPID ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "fdr"
  )
rapid.treatment.pv

######################### Static motility #########################
static.aov = anova_test(data = casa.trim, dv = STATIC, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(static.aov)
static.aov$CASA = "STATIC"

# Pairwise comparisons for time variable
static.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    STATIC ~ TIME, paired = TRUE, 
    p.adjust.method = "fdr"
  )
static.time.pv
# Pairwise comparisons for time variable
static.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    STATIC ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "fdr"
  )
static.treatment.pv

######################### Area #########################
area.aov = anova_test(data = casa.trim, dv = AREA, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(area.aov)
area.aov$CASA = "AREA"

# Pairwise comparisons for time variable
area.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    AREA ~ TIME, paired = TRUE, 
    p.adjust.method = "fdr"
  )
area.time.pv
# Pairwise comparisons for time variable
area.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    AREA ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "fdr"
  )
area.treatment.pv


####
# Summary statistics # And plots #
####
############## Generating summary stats for PROGRESSIVE:
prog.sum = casa %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(PROGRESSIVE), 
            s.dev = sd(PROGRESSIVE), 
            s.error = sd(PROGRESSIVE)/sqrt(3)) %>%
  arrange(TIME)

prog.sum$Time = round((prog.sum$TIME^1.32)*30, 0)
prog.sum$PARAM = "Progressive"
prog.csv = prog.sum

# Need to duplicate T0 for each rep:
prog.sum = rbind(prog.sum, prog.sum[rep(1, 3), ])
prog.sum$TREATMENT[1] = 1
prog.sum$TREATMENT[10] = 2
prog.sum$TREATMENT[11] = 3
prog.sum$TREATMENT[12] = 4
prog.sum$TREATMENT = as.factor(prog.sum$TREATMENT)

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

# Boxplot:
prog.b = ggplot(casa.trim, aes(x=TREATMENT, y=PROGRESSIVE, fill=TIME)) + 
  geom_boxplot() +
  labs(x='Treatment', y='Forward Progressive Sperm (%)') +
  scale_x_discrete(label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No supplementation')) +
  scale_fill_manual(values = c('grey50', 'grey90'), name = 'Time Elapsed', label = c('30 mins', '75 mins')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

############## Generating summary stats for MOTILE plot:
mot.sum = casa %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(MOTILE), 
            s.dev = sd(MOTILE), 
            s.error = sd(MOTILE)/sqrt(3)) %>%
  arrange(TIME)

mot.sum$Time = round((mot.sum$TIME^1.32)*30, 0)
mot.sum$PARAM = "Motile"
mot.csv = mot.sum

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

# Boxplot:
mot.b = ggplot(casa.trim, aes(x=TREATMENT, y=MOTILE, fill=TIME)) + 
  geom_boxplot() +
  labs(x='Treatment', y='Motile Sperm (%)') +
  scale_x_discrete(label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_fill_manual(values = c('grey50', 'grey90'), name = 'Time Elapsed', label = c('30 mins', '75 mins')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

############## Generating summary stats for RAPID plot:
rapid.sum = casa %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(RAPID), 
            s.dev = sd(RAPID), 
            s.error = sd(RAPID)/sqrt(3)) %>%
  arrange(TIME)

rapid.sum$Time = round((rapid.sum$TIME^1.32)*30, 0)
rapid.sum$PARAM = "Rapid"
rapid.csv = rapid.sum

# Need to duplicate T0 for each rep:
rapid.sum = rbind(rapid.sum, rapid.sum[rep(1, 3), ])
rapid.sum$TREATMENT[1] = 1
rapid.sum$TREATMENT[10] = 2
rapid.sum$TREATMENT[11] = 3
rapid.sum$TREATMENT[12] = 4
rapid.sum$TREATMENT = as.factor(rapid.sum$TREATMENT)

# Stacked line graph
rapid.p = rapid.sum %>%
  ggplot(aes(x = Time, y = avg)) +
  geom_line(aes(col=TREATMENT)) +   
  geom_point(aes(shape=TREATMENT), size = 4) +
  geom_errorbar(aes(ymin = avg-s.error, ymax = avg+s.error, col=TREATMENT), width = 1) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = 'Treatment', label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_color_manual(values=c("#000000", "#000000", "#000000", "#000000")) + 
  theme_bw() +
  scale_x_continuous(breaks=c(0,30,75)) +
  labs(y="Rapid sperm (%)", x="Time (mins)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot
rapid.b = ggplot(casa.trim, aes(x=TREATMENT, y=RAPID, fill=TIME)) + 
  geom_boxplot() +
  labs(x='Treatment', y='Rapid Sperm (%)') +
  scale_x_discrete(label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_fill_manual(values = c('grey50', 'grey90'), name = 'Time Elapsed', label = c('30 mins', '75 mins')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

############## Generating summary stats for STATIC plot:
static.sum = casa %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(STATIC), 
            s.dev = sd(STATIC), 
            s.error = sd(STATIC)/sqrt(3)) %>%
  arrange(TIME)

static.sum$Time = round((static.sum$TIME^1.32)*30, 0)
static.sum$PARAM = "Static"
static.csv = static.sum

# Need to duplicate T0 for each rep:
static.sum = rbind(static.sum, static.sum[rep(1, 3), ])
static.sum$TREATMENT[1] = 1
static.sum$TREATMENT[10] = 2
static.sum$TREATMENT[11] = 3
static.sum$TREATMENT[12] = 4
static.sum$TREATMENT = as.factor(static.sum$TREATMENT)

# Stacked line graph
static.p = static.sum %>%
  ggplot(aes(x = Time, y = avg)) +
  geom_line(aes(col=TREATMENT)) +   
  geom_point(aes(shape=TREATMENT), size = 4) +
  geom_errorbar(aes(ymin = avg-s.error, ymax = avg+s.error, col=TREATMENT), width = 1) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = 'Treatment', label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_color_manual(values=c("#000000", "#000000", "#000000", "#000000")) + 
  theme_bw() +
  scale_x_continuous(breaks=c(0,30,75)) +
  labs(y="Static sperm (%)", x="Time (mins)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot
static.b = ggplot(casa.trim, aes(x=TREATMENT, y=STATIC, fill=TIME)) + 
  geom_boxplot() +
  labs(x='Treatment', y='Static Sperm (%)') +
  scale_x_discrete(label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_fill_manual(values = c('grey50', 'grey90'), name = 'Time Elapsed', label = c('30 mins', '75 mins')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

############## Generating summary stats for AREA plot:
area.sum = casa %>%
  group_by(TREATMENT, TIME) %>%
  summarise(avg = mean(AREA), 
            s.dev = sd(AREA), 
            s.error = sd(AREA)/sqrt(3)) %>%
  arrange(TIME)

area.sum$Time = round((area.sum$TIME^1.32)*30, 0)
area.sum$PARAM = "Area"
area.csv = area.sum

# Need to duplicate T0 for each rep:
area.sum = rbind(area.sum, area.sum[rep(1, 3), ])
area.sum$TREATMENT[1] = 1
area.sum$TREATMENT[10] = 2
area.sum$TREATMENT[11] = 3
area.sum$TREATMENT[12] = 4
area.sum$TREATMENT = as.factor(area.sum$TREATMENT)

# Stacked line graph
area.p = area.sum %>%
  ggplot(aes(x = Time, y = avg)) +
  geom_line(aes(col=TREATMENT)) +   
  geom_point(aes(shape=TREATMENT), size = 4) +
  geom_errorbar(aes(ymin = avg-s.error, ymax = avg+s.error, col=TREATMENT), width = 1) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = 'Treatment', label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_color_manual(values=c("#000000", "#000000", "#000000", "#000000")) + 
  theme_bw() +
  scale_x_continuous(breaks=c(0,30,75)) +
  labs(y="Area (%)", x="Time (mins)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot
area.b = ggplot(casa.trim, aes(x=TREATMENT, y=AREA, fill=TIME)) + 
  geom_boxplot() +
  labs(x='Treatment', y='Area (%)') +
  scale_x_discrete(label = c('P4','P4 + cAMP + PTX', 'IBMX + MBCD', 'No Supplementation')) +
  scale_fill_manual(values = c('grey50', 'grey90'), name = 'Time Elapsed', label = c('30 mins', '75 mins')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

########################################



########################################
# Writing plots
# Line plots
tiff(file = "MOTILE_line.tiff", width=8, height=6, units="in", res=100, compression="lzw")
mot.p + guides(colour = "none")
dev.off()

tiff(file = "PROGRESSIVE_line.tiff", width=8, height=6, units="in", res=100, compression="lzw")
prog.p + guides(colour = "none")
dev.off()

tiff(file = "RAPID_line.tiff", width=8, height=6, units="in", res=100, compression="lzw")
rapid.p + guides(colour = "none")
dev.off()

tiff(file = "STATIC_line.tiff", width=8, height=6, units="in", res=100, compression="lzw")
static.p + guides(colour = "none")
dev.off()

tiff(file = "AREA_line.tiff", width=8, height=6, units="in", res=100, compression="lzw")
area.p + guides(colour = "none")
dev.off()

# Boxplots
tiff(file = "MOTILE_boxplot.tiff", width=8, height=6, units="in", res=100, compression="lzw")
mot.b
dev.off()

tiff(file = "PROGRESSIVE_boxplot.tiff", width=8, height=6, units="in", res=100, compression="lzw")
prog.b
dev.off()

tiff(file = "RAPID_boxplot.tiff", width=8, height=6, units="in", res=100, compression="lzw")
rapid.b
dev.off()

tiff(file = "STATIC_boxplot.tiff", width=8, height=6, units="in", res=100, compression="lzw")
static.b
dev.off()

tiff(file = "AREA_boxplot.tiff", width=8, height=6, units="in", res=100, compression="lzw")
area.b
dev.off()



# Writing files
all.times = rbind(mot.time.pv, prog.time.pv, static.time.pv, # rapid.time.pv, 
                  area.time.pv)

all.treatments = na.omit(rbind(mot.treatment.pv, prog.treatment.pv, rapid.treatment.pv, static.treatment.pv, area.treatment.pv))

all.means = na.omit(rbind(prog.csv, mot.csv, rapid.csv, static.csv, area.csv))
mycols = c("PARAM", "TREATMENT", "Time", "avg", "s.error")
all.means = all.means[,(names(all.means) %in% mycols)]
all.means$avg = round(all.means$avg, 2)
all.means$s.error = round(all.means$s.error, 2)
dfcolnames = c("Treatment", "Mean", "Standard Error", "Time", "CASA Parameter")
colnames(all.means) = dfcolnames

all.anovas = na.omit(rbind(prog.aov, mot.aov, rapid.aov, static.aov, area.aov))

write.csv(all.times, file = 'all-times.csv', row.names = FALSE) # rapid is missing
write.csv(all.treatments, file = 'all-treatmets.csv', row.names = FALSE)
write.csv(all.means, file = "all-means.csv", row.names = FALSE)
write.csv(all.anovas, file = "all-anovas.csv", row.names = FALSE)
