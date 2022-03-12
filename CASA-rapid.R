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

casa.0alt = rbind(casa, casa[rep(1, 3), ])

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

casa.0alt$TIME = as.factor(casa.0alt$TIME)
casa.0alt$DUNN = as.factor(casa.0alt$TIME)

####
# Remaining vars
####

# RAPID - *
rapid.aov = anova_test(data = casa.trim, dv = RAPID, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(rapid.aov)
# MEDIUM - (NS)
medium.aov = anova_test(data = casa.trim, dv = MEDIUM, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(medium.aov)
# SLOW - (NS)
slow.aov = anova_test(data = casa.trim, dv = SLOW, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(slow.aov)
# STATIC - *
static.aov = anova_test(data = casa.trim, dv = STATIC, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(static.aov)
# PATH_VELOCITY_ - (NS)
path.aov = anova_test(data = casa.trim, dv = PATH_VELOCITY_, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(path.aov)
# PROG_VELOCITY - (NS)
progv.aov = anova_test(data = casa.trim, dv = PROG_VELOCITY, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(progv.aov)
# TRACK_SPEED - (NS)
track.aov = anova_test(data = casa.trim, dv = TRACK_SPEED, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(track.aov)
# LATERAL_AMPLITUDE - NAs detected!
lamp.aov = anova_test(data = casa.trim, dv = LATERAL_AMPLITUDE, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(lamp.aov)
# BEAT_FREQUENCY - (NS)
beat.aov = anova_test(data = casa.trim, dv = BEAT_FREQUENCY, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(beat.aov)
# STRAIGHTNESS - (NS)
straight.aov = anova_test(data = casa.trim, dv = STRAIGHTNESS, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(straight.aov)
# LINEARITY - (NS)
linearity.aov = anova_test(data = casa.trim, dv = LINEARITY, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(linearity.aov)
# ELONGATION - (NS)
elong.aov = anova_test(data = casa.trim, dv = ELONGATION, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(elong.aov)
# AREA - *
area.aov = anova_test(data = casa.trim, dv = AREA, wid = DUNNART, within = c(TREATMENT,TIME))
get_anova_table(area.aov)


# Significant vars - RAPID, STATIC and AREA

# RAPID:
get_anova_table(rapid.aov)
# Pairwise comparisons for time variable
rapid.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    RAPID ~ TIME, paired = TRUE, 
    p.adjust.method = "none"
  )

# Pairwise comparisons for time variable
rapid.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    RAPID ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "none"
  )
rapid.treatment.pv

# STATIC:
get_anova_table(static.aov)
# Pairwise comparisons for time variable
static.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    STATIC ~ TIME, paired = TRUE, 
    p.adjust.method = "none"
  )
static.time.pv
# Pairwise comparisons for time variable
static.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    STATIC ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "none"
  )
static.treatment.pv

# AREA:
get_anova_table(area.aov)
# Pairwise comparisons for time variable
area.time.pv = casa.0alt %>%
  group_by(TREATMENT) %>%
  pairwise_t_test(
    AREA ~ TIME, paired = TRUE, 
    p.adjust.method = "none"
  )
area.time.pv
# Pairwise comparisons for time variable
area.treatment.pv = casa.0alt %>%
  group_by(TIME) %>%
  pairwise_t_test(
    AREA ~ TREATMENT, paired = TRUE, 
    p.adjust.method = "none"
  )
area.treatment.pv



# Lateral amplitude

lat.df = casa.0alt[,c("DUNNART", "TIME", "TREATMENT", "LATERAL_AMPLITUDE")]



