library(tidyverse)
library(ggthemes)
all <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\1.21 research\\all_1.28.csv")

all[c(82:225), ] -> all_orm

# stream linear feet
x = ggplot(mapping = aes(all_orm$coincident, all_orm$riv_orm_impact_LF, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = "lm", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_tableau()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Stream Impact (Linear Ft)")+ labs(fill = "SMM in Effect") 


x + theme(plot.title = element_text(size=14, face="bold.italic"),
  axis.title.x = element_text(size=14, face="bold"),
  axis.title.y = element_text(size=14, face="bold"))
summary(lm(all_orm$riv_orm_impact_LF ~ all_orm$coincident * all_orm$after_policy))
plot(lm(all_orm$riv_orm_impact_LF ~ all_orm$coincident * all_orm$after_policy))

summary(lm(all_orm$riv_orm_impact_acres ~ all_orm$coincident * all_orm$after_policy))
plot(all_orm$coincident, all_orm$riv_orm_impact_acres)
ggplot(mapping = aes(all_orm$coincident, all_orm$riv_orm_impact_acres, fill = all_orm$after_policy_true))+geom_point()+geom_smooth(method = "lm")

# filtered stream permits
x = ggplot(mapping = aes(all_orm$coincident, all_orm$riv_orm_impact_count_permits, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_tableau()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Streams")+ labs(fill = "SMM in Effect") 

x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

# filtered stream linear feet 
x = ggplot(mapping = aes(all_orm$coincident, all_orm$riv_orm_filtered_LF, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = "lm", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_tableau()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Stream Impact (Linear Ft)")+ labs(fill = "SMM in Effect") 

x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))


summary(lm(all_orm$riv_orm_filtered_LF ~ all_orm$coincident * all_orm$after_policy))
summary(lm(all_orm$riv_orm_filtered_acres ~ all_orm$coincident * all_orm$after_policy))
# filtered stream acres 
x = ggplot(mapping = aes(all_orm$coincident, all_orm$riv_orm_filtered_acres, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = "lm", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_tableau()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Stream Impact (Acres)")+ labs(fill = "SMM in Effect")

x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

# clipped count of lienar feet of stream impacts

x = ggplot(mapping = aes(all_orm$coincident, all_orm$ORM_impacts_stream_LF_count, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Linear Ft of Streams")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

summary(lm(all_orm$ORM_impacts_stream_LF_count ~ all_orm$coincident * all_orm$after_policy_true))


# clipped count of acres of stream impacts

x = ggplot(mapping = aes(all_orm$coincident, all_orm$ORM_impacts_stream_Acres_count, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Acres of Streams")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

summary(lm(all_orm$ORM_impacts_stream_Acres_count ~ all_orm$coincident * all_orm$after_policy_true))


# clipped count of acres of palustrine impacts
x = ggplot(mapping = aes(all_orm$coincident, all_orm$ORM_impacts_wetland_Acres_count, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Wetlands in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Acres of Wetlands")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))
summary(lm(all_orm$ORM_impacts_wetland_Acres_count ~ all_orm$coincident * all_orm$after_policy_true))

# clipped count of pal impacts

x = ggplot(mapping = aes(all_orm$coincident, all_orm$pal_orm_impact_count_permits, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Wetlands in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Wetlands")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))


# filtered count of lienar feet of stream impacts

x = ggplot(mapping = aes(all_orm$coincident, all_orm$filtered_ORM_impacts_stream_LF_count, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Linear Ft of Streams")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

summary(lm(all_orm$filtered_ORM_impacts_stream_LF_count ~ all_orm$coincident * all_orm$after_policy_true))
exp()

# filtered count of acres of stream impacts

x = ggplot(mapping = aes(all_orm$coincident, all_orm$filtered_ORM_impacts_stream_acres_count, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Streams in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Acres of Streams")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))
summary(lm(all_orm$filtered_ORM_impacts_stream_acres_count ~ all_orm$coincident * all_orm$after_policy_true))
exp()

# filtered count of acres of palustrine impacts
x = ggplot(mapping = aes(all_orm$coincident, all_orm$filtered_ORM_impacts_wetland_acres_count, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Wetlands in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Acres of Wetlands")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))
summary(lm(all_orm$filtered_ORM_impacts_wetland_acres_count ~ all_orm$coincident * all_orm$after_policy_true))


# filtered count of pal impacts

x = ggplot(mapping = aes(all_orm$coincident, all_orm$pal_orm_filtered_count, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_wsj()+ ggtitle("Impact of Development on Wetlands in Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Count of Permits Impacting Wetlands")+ labs(fill = "SMM in Effect") 



x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

# ribits demand for wetland acres

  # this is a mistake - but an interesting one. It shows an incredible decline for the demand for wetland credits
x = ggplot(mapping = aes(all_orm$coincident, all_orm$ribits_est_wdr_Acres_wetlands, fill =all_orm$after_policy_true))+
  geom_point()+geom_smooth(method = MASS::glm.nb, col = "purple")+ 
    ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Demand for Wetland Credits in the Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Demand for Wetland Credits (Acres)")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))


x = ggplot(mapping = aes(all$coincident, all$ribits_est_wdr_Acres_wetlands, fill =all$after_policy_true))+
  geom_point()+geom_smooth(method = "lm", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Demand for Wetland Credits in the Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Demand for Wetland Credits (Acres)")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))


all_1 <- all %>% filter(after_policy_true == TRUE)
# riits demand for stream linear feet

x = ggplot(mapping = aes(all_1$coincident, all_1$ribits_est_wdr_LF_streams))+
  geom_point()+geom_smooth(method = "loess", col = "green")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Demand for Stream Credits in the Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Demand for Stream Credits (Linear Ft)")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))


# ribits demand for stream acres

x = ggplot(mapping = aes(all_1$coincident, all_1$attempt_acres_of_streams_ribits))+
  geom_point()+geom_smooth(method = "loess", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Demand for Stream Credits in the Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Demand for Stream Credits (Acres)")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))
summary(loess(all_1$attempt_acres_of_streams_ribits ~ all_1$coincident))
plot(all_1$coincident, all_1$attempt_acres_of_streams_ribits)
abline(loess(all_1$attempt_acres_of_streams_ribits ~ all_1$coincident))

x = ggplot(mapping = aes(all$month_as_num, all$ribits_rel_stream_credits, fill = all$after_policy_true))+
  geom_point()+geom_smooth(method = "loess", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Supply of Mitigation Credits in the Trinity River Basin") +
  xlab("Month") + ylab("Supply of Stream Credits")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))



summary(loess(all$ribits_rel_stream_credits ~ all$month_as_num))
summary(loess(all$ribits_rel_wetland_credits ~ all$month_as_num))
summary(lm(all$ribits_rel_stream_credits ~ all$month_as_num))
summary(lm(all$ribits_rel_wetland_credits ~ all$month_as_num))
supply <- all %>% select(ribits_rel_wetland_credits, ribits_rel_stream_credits, year, month_as_num, after_policy, after_policy_true)
supply_year <- aggregate(list(wetland_credits =supply$ribits_rel_wetland_credits, 
                              stream_credits = supply$ribits_rel_stream_credits), 
                         by = list(year = supply$year), na.rm = T, FUN = sum)
supply_year = supply_year %>% mutate(after_policy = year > 2013)
summary(lm(supply_year$stream_credits ~ supply_year$year * supply_year$after_policy)) # nothing for stream credits or wetland ones.

  x = ggplot(mapping = aes(all$month_as_num, all$ribits_rel_wetland_credits, fill = all$after_policy_true))+
  geom_point()+geom_smooth(method = "loess", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Supply of Mitigation Credits in the Trinity River Basin") +
  xlab("Month") + ylab("Supply of Wetland Credits")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

t.test(all$ribits_rel_wetland_credits[1:153], all$ribits_rel_wetland_credits[154:238]) # p value of 0.06
# recall df means degrees of freedom - sample size minus 2
mean(all$ribits_rel_wetland_credits[1:153], na.rm = T) # 34
median(all$ribits_rel_wetland_credits[1:153], na.rm = T) # 0
mean(all$ribits_rel_wetland_credits[154:238], na.rm = T) # 6.5
median(all$ribits_rel_wetland_credits[154:238], na.rm = T) # 0

wetland <- all %>% select(ribits_rel_wetland_credits, after_policy_true) 

 ggboxplot(wetland, x = "after_policy_true", y = "ribits_rel_wetland_credits", 
  ylab = "Number of Released or Initialized Wetland Credits Per Month", xlab = "Under New Policy" )



t.test(all$ribits_rel_stream_credits[1:153], all$ribits_rel_stream_credits[154:238]) # not significant
mean(all$ribits_rel_stream_credits[1:153], na.rm = T) # 257
median(all$ribits_rel_stream_credits[1:153], na.rm = T) # 0
mean(all$ribits_rel_stream_credits[154:238], na.rm = T) # 1129
median(all$ribits_rel_stream_credits[154:238], na.rm = T) # 0




bxp <- ggboxplot(
  genderweight, x = "group", y = "weight", 
  ylab = "Weight", xlab = "Groups", add = "jitter"
)



x = ggplot(mapping = aes(all_1$coincident, all_1$orm_filtered_mit_riverine_credits))+
  geom_point()+geom_smooth(method = "loess", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Demand for Stream Credits in the Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Demand for Stream Credits (Acres)")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))

x = ggplot(mapping = aes(all$coincident, all$orm_filtered_mit_palustrine_credits, fill =all$after_policy_true))+
  geom_point()+geom_smooth(method = "lm", col = "purple")+ 
  ggthemes::theme_few()+ggthemes::scale_fill_colorblind()+ ggtitle("Demand for Stream Credits in the Trinity River Basin") +
  xlab("Coincident Economic Index") + ylab("Demand for Stream Credits (Acres)")+ labs(fill = "SMM in Effect") 
x + theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"))
summary(lm(all$orm_filtered_mit_palustrine_credits ~all$coincident *  all$after_policy_true))

# mitigation in orm with credits (clipped)



