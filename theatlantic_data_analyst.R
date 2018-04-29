setwd('~/src/sandbox/mediaresearch')
library('dplyr')
library('reshape2')
library('ggplot2')
library('parallel')
library('multidplyr')
library('MASS')
# save.image('data.RData')
# load('data.RData')

####### import data and remove missing values ##############
data = read.csv('analyst_hw.tsv', header=T, as.is=T, sep='\t')
head(data)
dim(data)
str(data)
summary(data)

data = na.omit(data)


####### manipulate date and time variables ##############
data$visit_date = as.Date(data$visit_date)
data$visit_weekday = format(data$visit_date, '%u')

data$publication_date = as.Date(data$publication_date)
data$publication_weekday = format(data$publication_date, '%u')

n_cores = detectCores() - 1
cl = makeCluster(n_cores)
data$visit_dt = parApply(cl, data[,c('visit_date', 'visit_hour')], 1, 
             function(x) as.integer(as.Date(x[1]))+as.integer(x[2])/24 )
data$age_days = parApply(cl, data[,c('publication_date','visit_date','publication_hour','visit_hour')], 1,
          function(x) as.Date(x[2])-as.Date(x[1])+(as.integer(x[4])-as.integer(x[3]))/24 )
stopCluster(cl)


####### pageview time series by date ##############
visit_by_date = data %>% group_by(visit_date) %>% 
  summarise(pvs=n()) %>% as.data.frame()

ggplot(visit_by_date, aes(visit_date, pvs)) +
  geom_line(size=2)
ggsave('./images/pvs_by_date.png', width=16, height=9)

visit_by_dt = data %>% group_by(visit_dt) %>% 
  summarise(pvs=n()) %>% as.data.frame()

x_dt = seq(round(min(data$visit_dt)), round(max(data$visit_dt)), 7)
x_dt_minor = seq(round(min(data$visit_dt)), round(max(data$visit_dt)))

ggplot(visit_by_dt, aes(visit_dt, pvs)) +
  geom_line()+
  scale_x_continuous(breaks=x_dt, labels=x_dt, minor_breaks=x_dt_minor)
ggsave('./images/pvs_by_datetime.png', width=16, height=9)


####### compare hourly pub and visits ##############
pub_by_hour = data %>% group_by(publication_hour) %>% 
  summarise(pub_count=n_distinct(url_id)) %>% as.data.frame()
visit_by_hour = data %>% group_by(visit_hour) %>% 
  summarise(total_pvs=n(), 
            unique_visitors = n_distinct(visitor_id), 
            unique_urls = n_distinct(url_id)) %>% as.data.frame()
cmp_hour = merge(data.frame(hour=1:24), pub_by_hour, by.x='hour', by.y='publication_hour', all.x=T)
cmp_hour = merge(cmp_hour, visit_by_hour, by.x='hour', by.y='visit_hour', all.x=T)
viz = melt(cmp_hour, id='hour')

ggplot(viz, aes(factor(hour), value, fill=variable))+
  geom_bar(stat='identity') +
  facet_grid(variable~., scales='free') +
  ggtitle('Compare publications and total pageviews across hours')
ggsave('./images/cmp_pub_pvs_by_hour.png', width=16, height=9)
#!!! barely any fluctuation in pageviews. may not be the data for the entire site !!!


####### compare weekly pub and visits ##############
pub_by_weekday = data %>% group_by(publication_weekday) %>% 
  summarise(pub_count=n_distinct(url_id)) %>% as.data.frame()
visit_by_weekday = data %>% group_by(visit_weekday) %>% 
  summarise(total_pvs=n(), 
            unique_visitors = n_distinct(visitor_id), 
            unique_urls = n_distinct(url_id)) %>% as.data.frame()
cmp_weekday = merge(pub_by_weekday, visit_by_weekday, by.x='publication_weekday', by.y='visit_weekday')
names(cmp_weekday)[1] = 'weekday'
viz = melt(cmp_weekday, id='weekday')

ggplot(viz, aes(factor(weekday), value, fill=variable))+
  geom_bar(stat='identity', width=.75) +
  facet_grid(variable~., scales='free') +
  ggtitle('Compare publications and total pageviews across weekdays')
ggsave('./images/cmp_pub_pvs_by_weekday.png', width=16, height=9)


####### investigation of age ##############
ggplot(data, aes(age_days)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks=seq(0,max(data$age_days),7), labels=seq(0,max(data$age_days),7)) +
  ggtitle('Distribution of article age at the moment of visits')
ggsave('./images/age_hist.png', width=16, height=9)

ggplot(data, aes(age_days)) +
  stat_ecdf(geom='step') +
  scale_x_continuous(breaks=seq(0,max(data$age_days),7), labels=seq(0,max(data$age_days),7)) +
  scale_y_continuous(breaks=1:10/10, labels=1:10/10) +
  ylab('Empirical cumulative distribution') +
  ggtitle('ECDF of article age at the moment of visits')
ggsave('./images/age_ecdf.png', width=16, height=9)


####### investigation of sections ##############
cl = create_cluster(cores = n_cores)
by_group = data %>%
  partition(site_section, cluster = cl)
section_stats = collect(summarise(by_group, count = n()))
section_stats = section_stats %>% arrange(desc(count)) %>% as.data.frame()
viz = section_stats[1:50,]
viz$site_section = factor(viz$site_section, levels=rev(viz$site_section))

ggplot(viz, aes(site_section, count)) +
  geom_bar(stat='identity') +
  coord_flip() 
ggsave('./images/section_count.png', width=16, height=9)

ggplot(section_stats %>% filter(count>5), aes(count)) +
  geom_histogram(bins=60) +
  scale_x_log10(breaks=10^(0:6), labels=10^(0:6))
ggsave('./images/section_count_hist.png', width=16, height=9)


####### investigation of stories ##############
by_group = data %>%
  partition(url_id, cluster = cl)
story_stats = collect(summarise(by_group, count = n()))
story_stats = story_stats %>% arrange(desc(count)) %>% as.data.frame()
viz = story_stats[1:50,]
viz$url_id = factor(viz$url_id, levels=rev(viz$url_id))

ggplot(viz, aes(url_id, count)) +
  geom_bar(stat='identity') +
  scale_y_log10() +
  coord_flip() 
ggsave('./images/story_count.png', width=16, height=9)

ggplot(story_stats %>% filter(count>=5), aes(count)) +
  geom_histogram(bins=60) +
  scale_x_log10(breaks=10^(0:6), labels=10^(0:6))
ggsave('./images/story_count_hist.png', width=16, height=9)


####### prep for negative binomial regression over hourly pageviews ##############
section_filter = section_stats %>% filter(section_stats$count>1e+4) 
section_filter = section_filter %>% arrange(desc(count)) %>% as.data.frame()
top_sections = section_filter$site_section
data$section = data$site_section
data[which(! data$site %in% top_sections),'section'] = 'Others'

####### map url_id with section
url_section = data %>% group_by(url_id, section) %>% summarise(count=n()) %>% as.data.frame()

####### count pageviews for each hour
hourly_pvs = data %>% group_by(url_id, visit_date, visit_hour) %>% summarise(pvs = n()) %>% as.data.frame()
hourly_pvs = merge(hourly_pvs, url_section[,-3], by='url_id', all.x=T)
hourly_pvs$visit_weekday = weekdays(hourly_pvs$visit_date)
hourly_pvs = merge(hourly_pvs, data[,c('visit_date','visit_hour','age_days','url_id')], 
                   by=c('visit_date','visit_hour','url_id'), all.x=T)

hourly_pvs = within(hourly_pvs, {
visit_weekday = factor(visit_weekday, levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
  visit_hour = factor(visit_hour, levels=c(15:19, 8:14))
  section = factor(section)
})

hourly_pvs = hourly_pvs %>% filter(age_days>=0)
str(hourly_pvs)


####### pvs = age + section + visit_weekday + visit_hour ########
#### all predictors are significant

m1 = glm.nb(pvs ~ age_days + section + visit_weekday + visit_hour, data=hourly_pvs)
summary(m1)

# Call:
#   glm.nb(formula = pvs ~ age_days + section + visit_weekday + visit_hour, 
#          data = hourly_pvs, init.theta = 0.6145714384, link = log)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.3646  -1.2428  -0.5626   0.1402   3.4355  
# 
# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)             7.605840   0.007796  975.604  < 2e-16 ***
#   age_days               -0.070576   0.000157 -449.444  < 2e-16 ***
#   section/business/      -3.667370   0.006993 -524.408  < 2e-16 ***
#   section/education/     -4.037008   0.008907 -453.220  < 2e-16 ***
#   section/entertainment/ -3.677625   0.006205 -592.694  < 2e-16 ***
#   section/family/        -3.321943   0.012286 -270.380  < 2e-16 ***
#   section/health/        -3.643946   0.006614 -550.975  < 2e-16 ***
#   section/international/ -3.755482   0.006822 -550.492  < 2e-16 ***
#   section/magazine/      -2.371310   0.005655 -419.346  < 2e-16 ***
#   section/national/      -4.550220   0.011803 -385.531  < 2e-16 ***
#   section/photo/         -3.296409   0.008342 -395.162  < 2e-16 ***
#   section/politics/      -3.223596   0.005355 -602.018  < 2e-16 ***
#   section/science/       -3.765586   0.007564 -497.842  < 2e-16 ***
#   section/technology/    -3.405317   0.006853 -496.880  < 2e-16 ***
#   section/video/         -3.500934   0.007659 -457.076  < 2e-16 ***
#   section/world/         -1.904712   0.009511 -200.265  < 2e-16 ***
#   sectionOthers          -4.411140   0.006732 -655.215  < 2e-16 ***
#   visit_weekdayMonday     0.126952   0.005636   22.526  < 2e-16 ***
#   visit_weekdayTuesday   -0.260658   0.005820  -44.784  < 2e-16 ***
#   visit_weekdayWednesday -0.109778   0.005741  -19.123  < 2e-16 ***
#   visit_weekdayThursday  -0.152745   0.006188  -24.684  < 2e-16 ***
#   visit_weekdayFriday    -0.389921   0.006029  -64.675  < 2e-16 ***
#   visit_weekdaySaturday  -0.386950   0.006036  -64.110  < 2e-16 ***
#   visit_hour16            0.020578   0.006786    3.032  0.00243 ** 
#   visit_hour17           -0.001817   0.006790   -0.268  0.78899    
# visit_hour18            0.012985   0.006795    1.911  0.05599 .  
# visit_hour19            0.008187   0.006788    1.206  0.22775    
# visit_hour8             0.002680   0.006863    0.390  0.69623    
# visit_hour9             0.002067   0.006847    0.302  0.76269    
# visit_hour10           -0.021499   0.006833   -3.146  0.00165 ** 
#   visit_hour11           -0.010963   0.006841   -1.603  0.10904    
# visit_hour12           -0.008898   0.006832   -1.302  0.19279    
# visit_hour13           -0.013907   0.006827   -2.037  0.04164 *  
#   visit_hour14            0.004588   0.006813    0.673  0.50071    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Negative Binomial(0.6146) family taken to be 1)
# 
# Null deviance: 1991467  on 864316  degrees of freedom
# Residual deviance: 1001187  on 864283  degrees of freedom
# AIC: 7951143
# 
# Number of Fisher Scoring iterations: 1
# 
# 
# Theta:  0.614571 
# Std. Err.:  0.000845 
# 
# 2 x log-likelihood:  -7951072.531000 


#### `section` is significant
m2 = update(m1, . ~ . - section)
anova(m1, m2)

# Likelihood ratio tests of Negative Binomial Models
# 
# Response: pvs
# Model     theta Resid. df    2 x log-lik.   Test    df LR stat. Pr(Chi)
# 1           age_days + visit_weekday + visit_hour 0.3443566    864298        -8639116                              
# 2 age_days + section + visit_weekday + visit_hour 0.6145714    864283        -7951073 1 vs 2    15 688043.5       0


#### `visit_weekday` is significant
m3 = update(m1, . ~ . - visit_weekday)
anova(m1, m3)

# Likelihood ratio tests of Negative Binomial Models
# 
# Response: pvs
# Model     theta Resid. df    2 x log-lik.   Test    df LR stat. Pr(Chi)
# 1                 age_days + section + visit_hour 0.6059698    864289        -7966009                              
# 2 age_days + section + visit_weekday + visit_hour 0.6145714    864283        -7951073 1 vs 2     6 14936.56       0


#### `visit_hour` is significant
m4 = update(m1, . ~ . - visit_hour)
anova(m1, m4)

# Likelihood ratio tests of Negative Binomial Models
# 
# Response: pvs
# Model     theta Resid. df    2 x log-lik.   Test    df LR stat.      Pr(Chi)
# 1              age_days + section + visit_weekday 0.6145329    864294        -7951139                                   
# 2 age_days + section + visit_weekday + visit_hour 0.6145714    864283        -7951073 1 vs 2    11 66.45261 5.738793e-10


#### assumption holds: conditional mean <> conditional variance
m_poisson = glm(pvs ~ age_days + section + visit_weekday + visit_hour, family='poisson', data=hourly_pvs)
pchisq(2 * (logLik(m1) - logLik(m_poisson)), df=1, lower.tail=F)

# 'log Lik.' 0 (df=35)


####### coefficients and confidence intervals ########
resp_ratio = exp(cbind(Estimate = coef(m1), confint.default(m1)))
resp_ratio
#                             Estimate        2.5 %       97.5 %
#   (Intercept)            2.009900e+03 1.979423e+03 2.040847e+03
# age_days               9.318573e-01 9.315705e-01 9.321441e-01
# section/business/      2.554355e-02 2.519582e-02 2.589608e-02
# section/education/     1.765020e-02 1.734474e-02 1.796105e-02
# section/entertainment/ 2.528296e-02 2.497735e-02 2.559232e-02
# section/family/        3.608266e-02 3.522414e-02 3.696209e-02
# section/health/        2.614896e-02 2.581220e-02 2.649013e-02
# section/international/ 2.338919e-02 2.307853e-02 2.370402e-02
# section/magazine/      9.335840e-02 9.232940e-02 9.439886e-02
# section/national/      1.056488e-02 1.032330e-02 1.081212e-02
# section/photo/         3.701587e-02 3.641558e-02 3.762605e-02
# section/politics/      3.981165e-02 3.939602e-02 4.023168e-02
# section/science/       2.315405e-02 2.281333e-02 2.349986e-02
# section/technology/    3.319629e-02 3.275336e-02 3.364520e-02
# section/video/         3.016919e-02 2.971967e-02 3.062551e-02
# section/world/         1.488656e-01 1.461162e-01 1.516666e-01
# sectionOthers          1.214133e-02 1.198217e-02 1.230260e-02
# visit_weekdayMonday    1.135362e+00 1.122890e+00 1.147973e+00
# visit_weekdayTuesday   7.705445e-01 7.618044e-01 7.793849e-01
# visit_weekdayWednesday 8.960332e-01 8.860080e-01 9.061718e-01
# visit_weekdayThursday  8.583489e-01 8.480014e-01 8.688227e-01
# visit_weekdayFriday    6.771101e-01 6.691561e-01 6.851587e-01
# visit_weekdaySaturday  6.791253e-01 6.711388e-01 6.872068e-01
# visit_hour16           1.020791e+00 1.007303e+00 1.034459e+00
# visit_hour17           9.981846e-01 9.849890e-01 1.011557e+00
# visit_hour18           1.013070e+00 9.996679e-01 1.026652e+00
# visit_hour19           1.008221e+00 9.948966e-01 1.021723e+00
# visit_hour8            1.002683e+00 9.892857e-01 1.016262e+00
# visit_hour9            1.002070e+00 9.887118e-01 1.015608e+00
# visit_hour10           9.787302e-01 9.657093e-01 9.919267e-01
# visit_hour11           9.890974e-01 9.759245e-01 1.002448e+00
# visit_hour12           9.911412e-01 9.779571e-01 1.004503e+00
# visit_hour13           9.861896e-01 9.730823e-01 9.994734e-01
# visit_hour14           1.004598e+00 9.912723e-01 1.018104e+00


####### predictions and confidence intervals ########
section_input = data.frame(age_days=median(hourly_pvs$age_days), 
  section = factor(rep(c(top_sections,'Others'), each=7), levels=levels(hourly_pvs$section), labels=levels(hourly_pvs$section)),
  visit_hour = factor(rep(15,7*16), levels=levels(hourly_pvs$visit_hour)),
  visit_weekday = factor(rep(c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'),16), 
                   levels=levels(hourly_pvs$visit_weekday), labels=levels(hourly_pvs$visit_weekday)))

section_ci = cbind(section_input, predict(m1, section_input, type="link", se.fit=T))
section_ci = within(section_ci, {
  pvs = exp(fit)
  lower = exp(fit - 1.96 * se.fit)
  upper = exp(fit + 1.96 * se.fit)
})

ggplot(section_ci %>% filter(section %in% top_sections[1:4]), aes(x=visit_weekday, y=pvs, fill=section)) +
  geom_bar(stat='identity', width=.5, alpha=.75) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  facet_wrap(~section, nrow=2, scales='free') +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle('Predicted pageviews at 3 PM across sections and weekdays')
ggsave('./images/predicted_pvs_by_section_weekday.png', width=16, height=9)

age_range = seq(round(min(hourly_pvs$age_days)), round(max(hourly_pvs$age_days)+1))
age_input = data.frame(age_days=age_range, 
  section = factor(rep(c(top_sections,'Others'), each=length(age_range)), levels=top_sections, labels=top_sections),
  visit_hour = factor(rep(15,length(age_range)*16), levels=levels(hourly_pvs$visit_hour)),
  visit_weekday = factor(rep('Monday',length(age_range)*16), 
    levels=levels(hourly_pvs$visit_weekday), labels=levels(hourly_pvs$visit_weekday)))

age_ci = cbind(age_input, predict(m1, age_input, type="link", se.fit=T))
age_ci = within(age_ci, {
  pvs = exp(fit)
  lower = exp(fit - 1.96 * se.fit)
  upper = exp(fit + 1.96 * se.fit)
})

ggplot(age_ci %>% filter(section != '/' & section %in% top_sections[1:5]), aes(x=age_days, y=pvs)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=section), alpha=.2) +
  geom_line(aes(color=section))+
  theme_bw() +
  theme(legend.position = 'right') +
  ggtitle('Predicted pageviews at 3 PM across sections and weekdays')
ggsave('./images/predicted_pvs_by_section_age.png', width=16, height=9)


ggplot(hourly_pvs %>% filter(section %in% top_sections[1:4]), aes(visit_hour, pvs, fill=section)) +
  geom_boxplot() +
  facet_grid(section~., scales='free') +
  scale_y_log10()
  
ggplot(hourly_pvs %>% filter(section != '/' & section %in% top_sections[1:9] & pvs>=5), 
       aes(age_days, pvs, color=section)) +
  geom_point(alpha=.05) +
  geom_smooth(method='lm', se=F)+
  facet_wrap(~section, ncol=4) +
  scale_y_log10() +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle('Age vs. section vs. hourly pageviews')
ggsave('./images/cmp_pvs_section_age.png', width=16, height=9)

