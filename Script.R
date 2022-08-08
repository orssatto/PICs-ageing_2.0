# Libraries
library(readxl)
library(naniar)
library(visdat)
library(dplyr)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(janitor)
library(cowplot)
library(emmeans)
library(lme4)
library(car)
library(rmcorr)
library(psycho)
library(sjstats)
library(pwr)
library(viridis)
library(tidyr)


#VIBRATOR
#load data

dsol = read_excel("Database_vibrator.xlsx", sheet = "Soleus") %>%
  clean_names() %>%
  mutate(
    group = as.factor(group),
    condition = as.factor(condition),
    name = as.factor(name))

dTA = read_excel("Database_vibrator.xlsx", sheet = "TA") %>%
  clean_names() %>%
  mutate(
    group = as.factor(group),
    condition = as.factor(condition),
    name = as.factor(name))

#Calculate mean
dTA_mean = dTA %>% group_by(condition,name,group) %>%
  summarise(delta_f = mean(delta_f, na.rm = TRUE), dr = mean(dr, na.rm = TRUE))
dTA_mean

dsol_mean = dsol %>% group_by(condition,name,group) %>%
  summarise(delta_f = mean(delta_f, na.rm = TRUE), dr = mean(dr, na.rm = TRUE))
dsol_mean


#model 2-way

  #Delta F
fitta <- lmer(delta_f ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = dTA)
fitta_no_out <- lmer(delta_f ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = dTA %>% slice(-328, -309))

fitsol <- lmer(delta_f ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = dsol)

  #Discharge rates
fittadr <- lmer(dr ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = dTA)
fitsoldr <- lmer(dr ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = dsol)


  #Recruitment threshold
fittart <- lmer(rt ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = dTA)
fitsolrt <- lmer(rt ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = dsol)

# Model diagnostics
  #Delta F
qqPlot(residuals(fitta))
summary(fitta)

qqPlot(residuals(fitsol))
summary(fitsol)

  #Discharge rates
qqPlot(residuals(fittadr))
summary(fittadr)

qqPlot(residuals(fitsoldr))
summary(fitsoldr)

  #Recruitment Threshold

qqPlot(residuals(fittart))
summary(fittart)

qqPlot(residuals(fitsolrt))
summary(fitsolrt)

#confint(fit_2way)
  #Delta F
anovafitta <- anova(fitta)
omega_sq(anovafitta)

anovafitta_no_out <- anova(fitta_no_out)
omega_sq(anovafitta_no_out)

anovafitsol <- anova(fitsol)
omega_sq(anovafitsol)

  #Discharge rates
anovafittadr <- anova(fittadr)
omega_sq(anovafittadr)

anovafitsoldr <- anova(fitsoldr)
omega_sq(anovafitsoldr)

  #Recruitment Threshold
anovafittart <- anova(fittart)
omega_sq(anovafittart)

anovafitsolrt <- anova(fitsolrt)
omega_sq(anovafitsolrt)

#emmeans pairwise
  #Delta F
fitta.emm.s <- emmeans(fitta, "condition", "group")
pairs(fitta.emm.s, adjust = "bonferroni")

fitta.emm.s <- emmeans(fitta, "group", "condition")
pairs(fitta.emm.s, adjust = "bonferroni")

fitsol.emm.s <- emmeans(fitsol, "condition", "group")
pairs(fitsol.emm.s, adjust = "bonferroni")

fitsol.emm.s <- emmeans(fitsol, "group", "condition")
pairs(fitsol.emm.s, adjust = "bonferroni")

  #Discharge rates
fittadr.emm.s <- emmeans(fittadr, "condition", "group")
pairs(fittadr.emm.s, adjust = "bonferroni")

fittadr.emm.s <- emmeans(fittadr, "group", "condition")
pairs(fittadr.emm.s, adjust = "bonferroni")

fitsoldr.emm.s <- emmeans(fitsoldr, "condition")
pairs(fitsoldr.emm.s, adjust = "bonferroni")

  #Recruitment Threshold
fittart.emm.s <- emmeans(fittart, "condition", "group")
pairs(fittart.emm.s, adjust = "bonferroni")

fittart.emm.s <- emmeans(fittart, "group", "condition")
pairs(fittart.emm.s, adjust = "bonferroni")

fitsolrt.emm.s <- emmeans(fitsolrt, "condition", "group")
pairs(fitsolrt.emm.s, adjust = "bonferroni")

fitsolrt.emm.s <- emmeans(fitsolrt, "group", "condition")
pairs(fitsolrt.emm.s, adjust = "bonferroni")

## Fitted values
  #Delta F
(refgrid <- list (condition=c("Control","Vibrator"), group=c("Older","Young")))
mar_deltaf_ta <- emmip(fitta, ~ as.factor(condition)|as.factor(group), at = refgrid, CIs = T, plotit = F)
mar_deltaf_ta

(refgrid <- list (condition=c("Control","Vibrator"), group=c("Older","Young")))
mar_deltaf_sol <- emmip(fitsol, ~ as.factor(condition)|as.factor(group), at = refgrid, CIs = T, plotit = F)
mar_deltaf_sol

  #Discharge rates
(refgrid <- list (condition=c("Control","Vibrator"), group=c("Older","Young")))
mar_dr_ta <- emmip(fittadr, ~ as.factor(condition)|as.factor(group), at = refgrid, CIs = T, plotit = F)
mar_dr_ta

(refgrid <- list (condition=c("Control","Vibrator"), group=c("Older","Young")))
mar_dr_sol <- emmip(fitsoldr, ~ as.factor(condition)|as.factor(group), at = refgrid, CIs = T, plotit = F)
mar_dr_sol

  #Recruitment threshold
(refgrid <- list (condition=c("Control","Vibrator"), group=c("Older","Young")))
mar_rt_ta <- emmip(fittart, ~ as.factor(condition)|as.factor(group), at = refgrid, CIs = T, plotit = F)
mar_rt_ta

(refgrid <- list (condition=c("Control","Vibrator"), group=c("Older","Young")))
mar_rt_sol <- emmip(fitsolrt, ~ as.factor(condition)|as.factor(group), at = refgrid, CIs = T, plotit = F)
mar_rt_sol

#Mean difference sol and TA
#Delta F
emm <- emmeans(fitta, pairwise ~ condition*group)
confint(emm)
conditional_effect <- emmeans(fitta, ~ condition*group, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fitta), edf = df.residual(fitta))

emm <- emmeans(fitsol, pairwise ~ condition*group)
confint(emm)
conditional_effect <- emmeans(fitsol, ~ condition*group, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fitsol), edf = df.residual(fitsol))

#Discharge rates
emm <- emmeans(fittadr, pairwise ~ condition*group)
confint(emm)
conditional_effect <- emmeans(fittadr, ~ condition*group, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fittadr), edf = df.residual(fittadr))

emm <- emmeans(fitsoldr, pairwise ~ condition)
confint(emm)
conditional_effect <- emmeans(fitsoldr, ~ condition, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fitsoldr), edf = df.residual(fitsoldr))

#Recruitment threshold
emm <- emmeans(fittart, pairwise ~ condition*group)
confint(emm)
conditional_effect <- emmeans(fittart, ~ condition*group, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fittart), edf = df.residual(fittart))

emm <- emmeans(fitsolrt, pairwise ~ condition*group)
confint(emm)
conditional_effect <- emmeans(fitsolrt, ~ condition*group, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fitsolrt), edf = df.residual(fitsolrt))

#plot delta f TA
ggplot(data = dTA_mean, aes(x = condition, y = delta_f)) +
  geom_jitter(width = 0.0, alpha = 1, size = 3, aes(colour = name)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = dTA_mean, aes(x = condition, y = delta_f, group = name),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_deltaf_ta, aes(x = condition, y = yvar), 
             position = position_nudge(x = -0.15), size = 3) +
  geom_errorbar(data = mar_deltaf_ta, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.15), width = 0.0, size = 1.5) +
  #ylim(-2.5,6.5) +
  scale_x_discrete(breaks=c("Control", "Vibrator"), labels=c("Control", "Vibration")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(y = "?? F (pps)", x = "Condition") +
  facet_grid(~group) -> plot_deltaf_ta
plot_deltaf_ta

#plot delta f soleus
ggplot(data = dsol_mean, aes(x = condition, y = delta_f)) +
  geom_jitter(width = 0.02, alpha = 1, size = 3, aes(colour = name)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = dsol_mean, aes(x = condition, y = delta_f, group = name),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_deltaf_sol, aes(x = condition, y = yvar), 
             position = position_nudge(x = -0.15), size = 3) +
  geom_errorbar(data = mar_deltaf_sol, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.15), width = 0.0, size = 1.5) +
  #ylim(-2.5,6.5) +
  scale_x_discrete(breaks=c("Control", "Vibrator"), labels=c("Control", "Vibration")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(y = "?? F (pps)", x = "Condition") +
  facet_grid(~group) -> plot_deltaf_sol
plot_deltaf_sol

#### Figures Ta and Soleus layout
#cowplot
deltaf_cowplot <- plot_grid(plot_deltaf_sol, plot_deltaf_ta, labels = "AUTO", label_size = 22, ncol = 1)
deltaf_cowplot
ggsave(file = "deltaf_vibrator.png", units="in", width = 8, height = 10, dpi = 300)

#
##
###
#### HANDGRIP
###
##
#

#load data
hgsol = read_excel("database_handgrip.xlsx", sheet = "HG soleus") %>%
  clean_names() %>%
  mutate(
    group = as.factor(group),
    condition = as.factor(condition),
    time = as.factor(time),
    participant = as.factor(name))

hgsol_wide = read_excel("database_handgrip.xlsx", sheet = "Hg soleus mean diff") %>%
  clean_names() %>%
  mutate(
    group = as.factor(group),
    condition = as.factor(condition),
    participant = as.factor(name))


hgta = read_excel("database_handgrip.xlsx", sheet = "HG TA") %>%
  clean_names() %>%
  mutate(
    group = as.factor(group),
    condition = as.factor(condition),
    time = as.factor(time),
    participant = as.factor(name))

hgta_wide = read_excel("database_handgrip.xlsx", sheet = "Hg TA mean diff") %>%
  clean_names() %>%
  mutate(
    group = as.factor(group),
    condition = as.factor(condition),
    participant = as.factor(name))


#calculate mean
hgta_mean = hgta %>% group_by(condition,name,group,time) %>%
  summarise(delta_f = mean(delta_f, na.rm = TRUE))
hgta_mean
write.csv(hgta_mean,"C:\\Users\\n10306200\\cloudstor\\Study 4\\Data analysis\\R\\hgta_mean.csv", row.names = TRUE)

hgtadr_mean = hgta %>% group_by(condition,name,group,time) %>%
  summarise(dr = mean(dr, na.rm = TRUE))
hgtadr_mean

hgtart_mean = hgta %>% group_by(condition,name,group,time) %>%
  summarise(rt = mean(rt, na.rm = TRUE))
hgtart_mean

hgsol_mean = hgsol %>% group_by(condition,name,group,time) %>%
  summarise(delta_f = mean(delta_f, na.rm = TRUE))
hgsol_mean
write.csv(hgsol_mean,"C:\\Users\\n10306200\\cloudstor\\Study 4\\Data analysis\\R\\hgsol_mean.csv", row.names = TRUE)

hgsoldr_mean = hgsol %>% group_by(condition,name,group,time) %>%
  summarise(dr = mean(dr, na.rm = TRUE))
hgsoldr_mean

hgsolrt_mean = hgsol %>% group_by(condition,name,group,time) %>%
  summarise(rt = mean(rt, na.rm = TRUE))
hgsolrt_mean

#model 3-way
#Delta F
fithgta <- lmer(delta_f ~ as.factor(group)*as.factor(time)*as.factor(condition) + (1 | name/mu_id1), data = hgta)
fithgsol <- lmer(delta_f ~ as.factor(group)*as.factor(time)*as.factor(condition) + (1 | name/mu_id1), data = hgsol)

#Discharge rates
fithgtadr <- lmer(dr ~ as.factor(group)*as.factor(time)*as.factor(condition) + (1 | name/mu_id1), data = hgta)
fithgsoldr <- lmer(dr ~ as.factor(group)*as.factor(time)*as.factor(condition) + (1 | name/mu_id1), data = hgsol)

#Recruitment Thresholds
fithgtart <- lmer(rt ~ as.factor(group)*as.factor(time)*as.factor(condition) + (1 | name/mu_id1), data = hgta)
fithgsolrt <- lmer(rt ~ as.factor(group)*as.factor(time)*as.factor(condition) + (1 | name/mu_id1), data = hgsol)

#model 2-way md
fithgta_md <- lmer(delta_f_md ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = hgta_wide)
fithgsol_md <- lmer(delta_f_md ~ as.factor(group)*as.factor(condition) + (1 | name/mu_id1), data = hgsol_wide)

# Model diagnostics

  #Delta F
qqPlot(residuals(fithgta))
summary(fithgta)

qqPlot(residuals(fithgsol))
summary(fithgsol)

qqPlot(residuals(fithgta_md))
summary(fithgta_md)

qqPlot(residuals(fithgsol_md))
summary(fithgsol_md)

  #Discharge rates
qqPlot(residuals(fithgtadr))
summary(fithgtadr)

qqPlot(residuals(fithgsoldr))
summary(fithgsoldr)

#Recruitment Threshold
qqPlot(residuals(fithgtart))
summary(fithgtadr)

qqPlot(residuals(fithgsolrt))
summary(fithgsolrt)

#confint(fit_3way)

  #Delta F
anovafithgsol <- anova(fithgsol)
omega_sq(anovafithgsol)

anovafithgta <- anova(fithgta)
omega_sq(anovafithgta)

  #Discharge rates
anovafithgsoldr <- anova(fithgsoldr)
omega_sq(anovafithgsoldr)

anovafithgtadr <- anova(fithgtadr)
omega_sq(anovafithgtadr)

  #Recruitment threshold
anovafithgsolrt <- anova(fithgsolrt)
omega_sq(anovafithgsolrt)

anovafithgtart <- anova(fithgtart)
omega_sq(anovafithgtart)


#confint(fit_2way)
anova(fithgsol_md)
anova(fithgta_md)

#results 3-way and 2-way does not change

#emmeans pairwise 3-way

  #Delta F
fithgta.emm.s <- emmeans(fithgta, "time","condition")
pairs(fithgta.emm.s, adjust = "bonferroni")

fithgta.emm.s <- emmeans(fithgta, "condition","time")
pairs(fithgta.emm.s, adjust = "bonferroni")

fithgsol.emm.s <- emmeans(fithgsol, "time","condition")
pairs(fithgsol.emm.s, adjust = "bonferroni")

fithgsol.emm.s <- emmeans(fithgsol, "condition","time")
pairs(fithgsol.emm.s, adjust = "bonferroni")

  #Discharge rates
fithgtadr.emm.s <- emmeans(fithgtadr, "time","condition")
pairs(fithgtadr.emm.s, adjust = "bonferroni")

fithgtadr.emm.s <- emmeans(fithgtadr, "condition","time")
pairs(fithgtadr.emm.s, adjust = "bonferroni")

fithgsoldr.emm.s <- emmeans(fithgsoldr, "time","condition")
pairs(fithgsoldr.emm.s, adjust = "bonferroni")

fithgsoldr.emm.s <- emmeans(fithgsoldr, "condition","time")
pairs(fithgsoldr.emm.s, adjust = "bonferroni")

  #Recruitment threshold
fithgtart.emm.s <- emmeans(fithgtart, "time","condition")
pairs(fithgtart.emm.s, adjust = "bonferroni")

fithgtart.emm.s <- emmeans(fithgtart, "condition","time")
pairs(fithgtart.emm.s, adjust = "bonferroni")

fithgsolrt.emm.s <- emmeans(fithgsolrt, "time","condition")
pairs(fithgsolrt.emm.s, adjust = "bonferroni")

fithgsolrt.emm.s <- emmeans(fithgsolrt, "condition","time")
pairs(fithgsolrt.emm.s, adjust = "bonferroni")

## Fitted values 3-way
#Delta F
(refgrid <- list (fithgsol, "group","condition", "time"))
mar_deltaf_hgsol <- emmip(fithgsol, ~ as.factor(group)|as.factor(condition)|as.factor(time), at = refgrid, CIs = T, plotit = F)
mar_deltaf_hgsol

(refgrid <- list (fithgta, "group","condition", "time"))
mar_deltaf_hgta <- emmip(fithgta, ~ as.factor(group)|as.factor(condition)|as.factor(time), at = refgrid, CIs = T, plotit = F)
mar_deltaf_hgta

#Discharge rates
(refgrid <- list (fithgsoldr, "group","condition", "time"))
mar_dr_hgsol <- emmip(fithgsoldr, ~ as.factor(group)|as.factor(condition)|as.factor(time), at = refgrid, CIs = T, plotit = F)
mar_dr_hgsol

(refgrid <- list (fithgtadr, "group","condition", "time"))
mar_dr_hgta <- emmip(fithgtadr, ~ as.factor(group)|as.factor(condition)|as.factor(time), at = refgrid, CIs = T, plotit = F)
mar_dr_hgta

#Recruitment threshold
(refgrid <- list (fithgsoldr, "group","condition", "time"))
mar_rt_hgsol <- emmip(fithgsolrt, ~ as.factor(group)|as.factor(condition)|as.factor(time), at = refgrid, CIs = T, plotit = F)
mar_rt_hgsol

(refgrid <- list (fithgtart, "group","condition", "time"))
mar_rt_hgta <- emmip(fithgtart, ~ as.factor(group)|as.factor(condition)|as.factor(time), at = refgrid, CIs = T, plotit = F)
mar_rt_hgta

## Fitted values 2-way
emm <- emmeans(fithgsol_md, pairwise ~ condition)
confint(emm)

emm <- emmeans(fithgta_md, pairwise ~ condition)
confint(emm)


#Mean difference sol and TA
#Delta F
emm <- emmeans(fithgta, pairwise ~ condition*time)
confint(emm)
conditional_effect <- emmeans(fithgta, ~ condition*time, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fithgta), edf = df.residual(fithgta))

emm <- emmeans(fithgsol, pairwise ~ condition*time)
confint(emm)
conditional_effect <- emmeans(fithgsol, ~ condition*time, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fithgsol), edf = df.residual(fithgsol))

#Discharge rates
emm <- emmeans(fithgtadr, pairwise ~ condition*time)
confint(emm)
conditional_effect <- emmeans(fithgtadr, ~ condition*time, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fithgtadr), edf = df.residual(fithgtadr))

emm <- emmeans(fithgsoldr, pairwise ~ condition*time)
confint(emm)
conditional_effect <- emmeans(fithgsoldr, ~ condition*time, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fithgsoldr), edf = df.residual(fithgsoldr))

#Recruitment Threshold
emm <- emmeans(fithgtart, pairwise ~ condition*time*group)
confint(emm)
conditional_effect <- emmeans(fithgtart, ~ condition*time*group, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fithgtart), edf = df.residual(fithgtart))

emm <- emmeans(fithgsolrt, pairwise ~ condition*time)
confint(emm)
conditional_effect <- emmeans(fithgsolrt, ~ condition*time, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fithgsolrt), edf = df.residual(fithgsolrt))

#plot delta f sol
ggplot(data = hgsol_mean, aes(x = time, y = delta_f)) +
  geom_jitter(width = 0.0, alpha = 1, size = 3, aes(colour = name)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_bw(base_size = 14) +
  guides(fill = 'none', color = 'none') +
  geom_line(data = hgsol_mean, aes(x = time, y = delta_f, group = name),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_deltaf_hgsol, aes(x = time, y = yvar), 
             position = position_nudge(x = -0.15), size = 3) +
  geom_errorbar(data = mar_deltaf_hgsol, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.15), width = 0.0, size = 1.5) +
  #ylim(-2.5,6.5) +
  scale_x_discrete(limits = c("Before", "After")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    strip.text.y = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(y = "?? F (pps)", x = "Time") +
  facet_grid(condition~group) -> plot_deltaf_hgsol
plot_deltaf_hgsol
ggsave(file = "deltaf_hg_sol.png", units="in", width = 8, height = 10, dpi = 300)

#plot delta f TA
ggplot(data = hgta_mean, aes(x = time, y = delta_f)) +
  geom_jitter(width = 0.0, alpha = 1, size = 3, aes(colour = name)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_bw(base_size = 14) +
  guides(fill = 'none', color = 'none') +
  geom_line(data = hgta_mean, aes(x = time, y = delta_f, group = name),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_deltaf_hgta, aes(x = time, y = yvar), 
             position = position_nudge(x = -0.15), size = 3) +
  geom_errorbar(data = mar_deltaf_hgta, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.15), width = 0.0, size = 1.5) +
  #ylim(-2.5,6.5) +
  scale_x_discrete(limits = c("Before", "After")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    strip.text.y = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(y = "?? F (pps)", x = "Time") +
  facet_grid(condition~group) -> plot_deltaf_hgta
plot_deltaf_hgta
ggsave(file = "deltaf_hg_ta.png", units="in", width = 8, height = 10, dpi = 300)

