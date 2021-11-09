library(tidyverse)
wso <- read_csv("wso_data.csv")

summary(wso)
barplot(table(wso$testYear))
# three years, that is it. And we're focusing on 2019.
barplot(table(wso$installationYear)) # a large chunk of them are actually 
# really quite old. 

# CLEAN UP PROCESS #######################

### convert percentage to numeric #########
wso$percent_fix <- as.numeric(sub("%","",wso$accuracy))/100
barplot(table(wso$percent_fix))
    # I might need to remove the values that are below zero

### convert meter size to a standard unit ########
unique(wso$meterSize)
 
wso <- wso %>% dplyr::mutate(meterSize2 = ifelse(meterSize == "5/8\"", "5/8",
                          ifelse(meterSize == "1\"", "1", 
                           ifelse(meterSize == "5/8", "5/8",
                          ifelse(meterSize == "1", "1",
                                  "3/4")))))
barplot(table(wso$meterSize2))
  # almost all 5/8 
  # check to see if the problem cases are here?


### check for NAs ###########
apply(wso, 2, function(x) any(is.na(x)))
# No NAs

### Remove bad rows ########
  # first, there are N/A meterIDs (3). I will leave them in, as I don't
  # think it will affect calculations
  # second, some of my percent accurate are bizzarely less than 0%. I am
  # going to remove those.
wso <- wso %>% filter(!percent_fix < 0)
  # consider removing percent greater than 100%. It's kind of making 
  # things a little bizzare. 

### DROP DUPLICATES ###########
unique(wso)
# no dups


## FINISHED CLEAN UP ##################

### CREATE VALUES ################
# create a new value - age
wso <- wso %>% mutate(age = testYear - installationYear)
# create a new value - percent lost
wso <- wso %>%  
  mutate(water_loss_percent = ifelse(percent_fix <= 1, 1- percent_fix, NA) )
  # I chose to remove ones that are showing negative water loss, meaning,
  # there are some that have no water loss whatsoever and are in fact,
  # gaining water somehow. Perhaps this is normal?

# range
wso %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

wso %>%
  group_by(meterMake) %>%
  mutate(outlier = ifelse(is_outlier(installationYear),installationYear , as.numeric(NA))) %>%
  ggplot(., aes(x = factor(meterMake), y = installationYear)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)
# BADGER AND NEPTUNE ARE NEW. TRIDENT IS OLDDDDDD. 

wso$accuracy2 <- wso$percent_fix
library(WVPlots) 

PairPlot(wso, 
         colnames(wso)[c(11, 9)], 
         "test",
         group_var = "meterMake")

PairPlot(wso, 
         colnames(wso)[c(11, 9,10)], 
         "test",
         group_var = "meterSize2")

library(GGally)
ggpairs(wso[c(7, 9, 10)])

# check to see if the meter size is a factor? Age? 
# meter size doesn't seem to have an impact. Trident has the oldest,
# however, it is unclear if it's losing the most water.

p<-ggplot(wso, aes(x=meterMake, y=as.numeric(accuracy2)*100, color=meterMake)) +
  geom_point(trim=F)+geom_jitter()+xlab("Meter Make") + ylab("Accuracy (%)")
p + ggthemes::theme_few()+ ggthemes::scale_color_colorblind()

p<-ggplot(wso, aes(x=meterMake, y=accuracy2, color=meterMake)) +
  geom_boxplot(trim=F)+geom_j
p + ggthemes::theme_clean()+ ggthemes::scale_color_gdocs()
### badger seems best - most amount of good stuff
barplot(table(wso$meterMake))
### fair amount of them already

write_csv(wso, "wso_data_updated.csv")

## Based on the data provided, what is an estimate of the volume 
## and value of water lost to small meter inaccuracy in 2019?

  # need to multiply water loss percent with the number of gallons

# Estimated use per meter
165.4281513 Badger
140.3073498 Neptune
150.7513132 Trident

wso_question1 <- wso %>% mutate(estimate_use_year = ifelse(meterMake == "BADGER", 165.43, 
                                          ifelse(meterMake == "NEPTUNE", 140.31,
                                                 ifelse(meterMake== "TRIDENT", 150.75, NA))))
wso_question1 <- wso_question1 %>% mutate(loss_CCF = estimate_use_year*water_loss_percent)
sum(wso_question1$loss_CCF, na.rm = T)
# LOSS OF WATER: 11255.08
# 5.09/CCF 
# loss of $57,288.36 altogether
wso_question1_2019 <- wso_question1 %>% filter(testYear == 2019)
sum(wso_question1_2019$loss_CCF, na.rm = T)
table(wso_question1_2019$meterMake)
# LOSS OF WATER: 5376.92
# 5.09/CCF
# loss of $27,368.54 altogether

## What might you recommend for SWU to maintain their customer
## meters going forward based on the data provided? Consider meter 
## replacement decisions.

wso_question2 <- wso_question1

# First, regression of all the data

a <- (lm(water_loss_percent ~ age+ meterMake, wso_question2))
b <- (lm(water_loss_percent ~ age, wso_question2))
c <- (lm(water_loss_percent ~ meterMake, wso_question2)) 
anova(a,b,c)
# okay, seems like time is the biggest indicator of course. It seems like Trident clearly
# leads to some issues. And they're the oldest too.
summary(b) ## NEED TO USE THIS GRAPH IT ##################
summary(c)
# just looking at each of the meters, Trident has a significant relationship with time
badger <- wso_question2 %>% filter(meterMake== "BADGER")
neptune <- wso_question2 %>% filter(meterMake== "NEPTUNE")
trident <- wso_question2 %>% filter(meterMake== "TRIDENT")

b2 <- (lm(water_loss_percent ~ age, badger))
b3 <- (lm(water_loss_percent ~ age, neptune))
b4 <- (lm(water_loss_percent ~ age, trident))
summary(b2) # age has an effect on badger
summary(b3) # age has an effect on neptune
summary(b4) # age doesnt seem to have an effect on trident


wso_question2 %>% ggplot(aes(x = age,
                    y = water_loss_percent*100
                  ))+
  geom_point()+ #geom_smooth(method = lm, se = F, col = "purple")+geom_jitter()+
 ggthemes::theme_few()+ggthemes::scale_color_colorblind() +xlab("Age")+ylab("Percent of Water Lost (%)")+
  theme(plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"), axis.text=element_text(size=18))+
  facet_wrap(~meterMake)


wso_question2 %>% ggplot(aes(x = age,
                             y = water_loss_percent*100))+
  geom_point()+ geom_smooth(method = lm, col = "purple")+
  ggthemes::theme_few()+ggthemes::scale_color_colorblind() +xlab("Age")+ylab("Percent of Water Lost (%)")+
  theme(plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"), axis.text=element_text(size=18))
# So both are highly ugly, but really interesting


# need averages for each here
mean(badger$water_loss_percent, na.rm = T) # 0.03
median(badger$water_loss_percent, na.rm = T) # 0.01
mean(badger$age, na.rm= T) # 17 years old

mean(neptune$water_loss_percent, na.rm = T) # 0.04
median(neptune$water_loss_percent, na.rm = T) # 0.01
mean(neptune$age, na.rm= T) # 19 years old

mean(trident$water_loss_percent, na.rm = T) # 0.14
median(trident$water_loss_percent, na.rm = T) # 0.05
mean(trident$age) # 64 years old
# need percent bad here


# So now, which ones to remove? Replace with which meter?

# Easy - replace with Badger - average loss is 3% (median is 1%)

# right now, it's costing the company $27,368.54 per year.
# what I could do is see which ones are costing the company the most.
# 27,368.54/ 150 a meter (182 meters that could be replaced for that cost!)

wso_question2 <- wso_question2 %>% mutate(water_cost = loss_CCF * 5.09) %>% arrange(desc(water_cost))

barplot(table(wso_question2$water_cost))

# So if we took the top 182 meters, which ones could we replace?
wso_question2_2019 <- wso_question2 %>% filter(testYear == 2019)
wso_question2_2019_top182 <- wso_question2_2019[1:182,]
sum(wso_question2_2019_top182$water_cost, na.rm = T)
# would save $22838.35 for the 2019 meters. 
# would spend $4530.54 on top of the 27K to save 27K in the future

# Should I just be looking at the 2019 data? Maybe I should instead just look 
# at them all to pick the really bad ones.
wso_question2_top182 <- wso_question2[1:182,]
sum(wso_question2_top182$water_cost, na.rm = T)
# $41,141.59 would not be lost.
# $27,368.54 to fix 182 meters ($13,773.05 would be saved that year)
wso_question2 <- wso_question2 %>% 
  mutate(rownumber = row_number()) %>% 
  mutate(replaced = ifelse(rownumber < 183, T, F))


wso_question2 %>% ggplot(aes(x = age,
                             y = water_loss_percent*100,
                             color = factor(replaced)))+
  geom_point()+ #geom_smooth(method = lm, col = "purple")+
  scale_fill_discrete(guide = guide_legend(reverse=TRUE))+ xlab("Age")+ylab("Percent of Water Lost (%)")+
    labs(color = "Replaced \nMeters")+ guides(color = guide_legend(reverse=TRUE))+
ggthemes::theme_few()+ggthemes::scale_color_colorblind() +

    theme(legend.title = element_text(size = 18),
      legend.text = element_text(size = 15),
      plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"), axis.text=element_text(size=18))

## Additional Work - determine the amount of water lost across the population, not the sample.

wso_question2_2019 %>% filter(!is.na(water_cost)) %>% n_distinct()
wso_question2 %>% filter(!is.na(water_cost)) %>% n_distinct()

x <- wso_question2_2019 %>% filter(meterMake == "BADGER") %>% select(loss_CCF) 
mean(x$loss_CCF, na.rm = T) # 5.160382
y <- wso_question2_2019 %>% filter(meterMake == "NEPTUNE") %>% select(loss_CCF) 
mean(y$loss_CCF, na.rm = T) #5.505833
z <- wso_question2_2019 %>% filter(meterMake == "TRIDENT") %>% select(loss_CCF) 
mean(z$loss_CCF, na.rm = T) # 21.55294

## Extra - meter size importance?

summary(lm(water_loss_percent ~meterSize2, wso_question2)) 
#not significant
