if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}

library(tidyverse)
low <- read_csv("/Work/Research/PHD/Dissertation 2017/AXBX_2016/subjective_rating/subj_rating_LA.csv", na = ".")

low_1 <- arrange(low, phase, symbolvec, trial)

library(psych)
headTail(low_1)
str(low_1)

# need to add a column so repetition numbers can be represented. trial and trial_phase do not do the job.
## trial numbers in each phases
rep1=30
rep2=32
rep3=65
rep4=30

##
### rep1_1 = 7 + 1
### rep1_2 = 7 + 1
### rep1_3 = 7
### rep1_4 = 7
r11 = (1:8)
r12 = (1:8)
r13 = (1:7)
r14 = (1:7)
r1 = c(r11, r12, r13, r14)
r1

###rep2_1 = 8
###rep2_2 = 8
###rep2_3 = 8
###rep2_4 = 8
r21 = (1:8)
r22 = (1:8)
r23 = (1:8)
r24 = (1:8)
r2 = c(r21, r22, r23, r24)
r2

###rep3_1 = 17
###rep3_2 = 16
###rep3_3 = 16
###rep3_4 = 16
r31 = (1:17)
r32 = (1:16)
r33 = (1:16)
r34 = (1:16)
r3 = c(r31, r32, r33, r34)
r3


###rep4_1 = 7 + 1
###rep4_2 = 7 + 1
###rep4_3 = 7
###rep4_4 = 7
r41 = (1:8)
r42 = (1:8)
r43 = (1:7)
r44 = (1:7)
r4 = c(r41, r42, r43, r44)
r4

rep = c(r1, r2, r3, r4)
View(rep)

# add rep as a new column to low_1
low_2 <- mutate(low_1, rep_num = rep)
View(low_2)

# interaction plot
library(rcompanion)
Sum = groupwiseMean(s10 ~ symbolvec + rep_num,
                    data   = low_2,
                    conf   = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile  = FALSE)
Sum


# determine autocorrelation in residuals 
library(nlme)
model.a = lme(s10 ~ phase + symbolvec + phase * symbolvec,
              random = ~ rep_num | phase,
              data = low_2)
v <- ACF(model.a)
v

# run model 
model= lme(s10 ~ phase + symbolvec + phase * symbolvec,
           random = ~ rep_num | phase,
           correlation = corAR1(form = ~ trial_phase | phase, value = 0.6134),
           data = low_2,
           method = "REML")

library(car)
anova(model)          

# determine random effects by comparing model only has fixed effects
model.fixed = gls(s10 ~ phase + symbolvec + phase * symbolvec,
                  data = low_2,
                  method = "REML")
compare <- anova(model,
                 model.fixed)
compare

# multiple comparisons 
library(multcompView)

library(lsmeans)

marginal = lsmeans(model, 
                   ~ symbolvec:rep_num)

# select acq from low_2
low_acq <- filter(low_2, phase == 2)
# interaction plot for acq
library(rcompanion)
Sum_acq = groupwiseMean(s10 ~ symbolvec + rep_num,
                    data   = low_acq,
                    conf   = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile  = FALSE)

low_3 <- group_by(low_2, phase, symbolvec)
Sum2 <- summarise(low_3, mean = mean(rep_num, na.rm = TRUE))

# select data for subject 10

low_s10 <- select(low_2, phase, symbolvec,rep_num, s10)
group <- group_by(low_s10, phase, symbolvec)
sum_s10 <- summarise(group, mean = mean(s10, na.rm = TRUE))
library(ggplot2)
plot_s10 <- ggplot(low_s10, aes(fill = factor(symbolvec), x=phase, y=s10)) +
                     geom_bar(position = "dodge", stat = "identity")
plot_s10          

# add error bars
## calculate stand deviations
library(plyr)
s10_data <- ddply(low_s10, c("phase", "symbolvec"), summarise,
               N    = length(s10),
               mean = mean(s10),
               sd   = sd(s10),
               se   = sd / sqrt(N))

## draw error bars
pd <- position_dodge(0.9)
plot_s10 <- ggplot(s10_data, aes(fill = factor(symbolvec), x=phase, y=mean)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(x=phase, ymin=mean-sd, ymax=mean+sd, group=factor(symbolvec)), width=0.4, colour="orange", alpha=0.9, size=1, position = pd)
plot_s10  
