# Greatleap online lecture 21.11.2025
# Retrospective modelling of epidemics using historical mortality data
# Instructor: Katarina Matthes (UZH)


# load libraries, plot parameters, colors, etc.
rm(list=ls())
source("code/00_setup.R")

# load data
dt <- read.csv("data/dataZH_month.csv", sep=";") %>%
  mutate(
    mortality = death/pop*100000, # Mortality per 100k
    year_month = ymd(paste0(year,"-",month,"-01")),  # define as date, makes it easier for plotting
    w = case_when(year_month > ymd("1918-05-01") & year_month < ymd("1919-03-01") |
                    year_month > ymd("1919-12-31") & year_month< ymd("1920-04-01") ~ 0, 
                  TRUE ~ 1), #  define pandemic period, 0 = pandemic
    si_one = sin(2*pi*month/12), #  define seasonality
    si_two = sin(4*pi*month/12),
    co_one = cos(2*pi*month/12),
    co_two = cos(4*pi*month/12)
  )

# plot mortality 

ggplot() +
  geom_line(data=dt ,aes(y=death,x= year_month), lwd=lwd_size) +
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("4 month"),
               expand  = c(0, 0)) +   
  annotate("rect",
           xmin=as.Date(ymd("1918-06-01")),
           xmax=as.Date(ymd("1919-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  annotate("rect",
           xmin=as.Date(ymd("1919-12-01")),
           xmax=as.Date(ymd("1920-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  xlab("Month/Year")+
  ylab("deaths")+
  # ggtitle("Monthly Mortality") +
  theme_bw()+
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size=text_size,angle=45,hjust=1),
        axis.text.y = element_text(size=text_size),
        axis.title  = element_text(size=text_size),
        title =element_text(size=title_size))

ggsave("figures/figure_death.png",h=8,w=15)


######################################
# Average over whole training period #
######################################

# calculate baseline using death average excluding pandemic years

dt2 <- dt %>%
  filter(year_month > ymd("1914-12-31") & year_month < ymd("1924-01-01")) # 3 years before and 3 years after

dt3<- dt2 %>% 
  filter(!w==0) %>% # exclude pandemic years
  mutate(
    # bsnm = mean( mortality, na.rm = TRUE),
    bsn_a = mean( death,na.rm = TRUE)
  )%>%
  full_join(dt2) %>%
  mutate(
    bsn_a =  unique(bsn_a)[1]
  )
               
# plot average 

ggplot() +
  geom_line(data=dt3 ,aes(y=death,x= year_month), lwd=lwd_size) +
  geom_hline(yintercept= unique(dt3$bsn_a), col=col5[1],lwd=lwd_size) +
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("4 month"),
               expand  = c(0, 0)) +   
  annotate("rect",
           xmin=as.Date(ymd("1918-06-01")),
           xmax=as.Date(ymd("1919-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  annotate("rect",
           xmin=as.Date(ymd("1919-12-01")),
           xmax=as.Date(ymd("1920-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  xlab("Month/Year")+
  ylab("deaths")+
  ggtitle("Average whole training period") +
  theme_bw()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size=text_size,angle=45,hjust=1),
    axis.text.y = element_text(size=text_size),
    axis.title  = element_text(size=text_size),
    title =element_text(size=title_size))

ggsave("figures/figure_average.png",h=8,w=15)   


############################
# Monthly specific average #
############################

# calculate baseline using monthly death average excluding pandemic years

dt4 <- dt3 %>%
  filter(!w==0) %>% # exclude pandemic years
  group_by(month) %>% # group  by month for monthly average
  mutate(
    bsn_am = mean(mortality,na.rm = TRUE),
    bsn_ad = mean(death,na.rm = TRUE)
    ) %>%
  ungroup() %>%
  distinct(month,.keep_all = TRUE) %>%
  select(month, bsn_am,bsn_ad) %>%
  full_join(dt3)


# plot average 

ggplot() +
  geom_line(data=dt4 ,aes(y=death,x= year_month), lwd=lwd_size) +
  geom_line(data=dt4,aes(y=  bsn_ad,x= year_month), lwd=lwd_size, col=col5[2]) +
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("4 month"),
               expand  = c(0, 0)) +   
  annotate("rect",
           xmin=as.Date(ymd("1918-06-01")),
           xmax=as.Date(ymd("1919-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  annotate("rect",
           xmin=as.Date(ymd("1919-12-01")),
           xmax=as.Date(ymd("1920-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  xlab("Month/Year")+
  ylab("deaths")+
  ggtitle("Monthly specific average of training period") +
  theme_bw()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size=text_size,angle=45,hjust=1),
    axis.text.y = element_text(size=text_size),
    axis.title  = element_text(size=text_size),
    title =element_text(size=title_size))

ggsave("figures/figure_average_monthly.png",h=8,w=15)  


#####################
# GLM quasi-poisson #
####################

# calculate baseline using glm poisson modeling

mod_glm <- glm(death ~ year_month + si_one+si_two+co_one+co_two+
        offset(log(pop)),  # add pop as offset
        weights = w, # exclude pandemic month
        data = dt4, 
      family = "quasipoisson" # using a quasi-poisson distribution to account for overdispersion
        )

mod_glm_pred <- predict(mod_glm,type = "response", se.fit = TRUE)

# obtaining estimates for the three models
dt5 <- dt4 %>% 
  mutate(
    bsn = mod_glm_pred$fit,
    se = mod_glm_pred$se.fit,
    ll = bsn - 1.96*se,
    ul = bsn + 1.96*se)

# plot poisson

ggplot(dt5) +
  geom_line(aes(y=death,x= year_month), lwd=lwd_size) +
  geom_line(aes(y= bsn,x= year_month), lwd=lwd_size, col=col5[3]) +
  geom_ribbon(aes(ymin=ll, ymax=ul,x=year_month), fill=col5[3],alpha=0.3) +
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("4 month"),
               expand  = c(0, 0)) +   
  annotate("rect",
           xmin=as.Date(ymd("1918-06-01")),
           xmax=as.Date(ymd("1919-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  annotate("rect",
           xmin=as.Date(ymd("1919-12-01")),
           xmax=as.Date(ymd("1920-04-01")),
           ymin=-Inf,
           ymax=Inf,
           alpha=0.1,
           fill="black") +
  xlab("Month/Year")+
  ylab("deaths")+
  ggtitle("GLM Serfling model") +
  theme_bw()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size=text_size,angle=45,hjust=1),
    axis.text.y = element_text(size=text_size),
    axis.title  = element_text(size=text_size),
    title =element_text(size=title_size))

ggsave("figures/figure_glm_monthly.png",h=8,w=15)  

##############
# Comparison #
##############

# obtaining estimates for the three models
dt6 <- dt5 %>% 
  mutate(
    exc_a = death - bsn_a,
    exc_ma = death - bsn_ad,
    exc_glm = death - bsn) %>%
  filter(w==0) %>%
  gather(., method, exc_death, exc_a:exc_glm) %>%
  mutate(
    exc_mx = exc_death/pop*100000
  )

    
ggplot(dt6) +
  geom_bar(aes(x = year_month, y = exc_death, fill = method),stat = "identity", position = "dodge") +
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("1 month"),
               expand  = c(0, 0)) +
  scale_fill_manual("methods:",
                    breaks=c("exc_a", "exc_glm", "exc_ma"),
                    labels =c("Simple average","Monthly average","GLM surfling"),
                    values = c(col5[1], col5[2],col5[3])) +
  xlab("Month/Year")+
  ylab("excess deaths")+
  ggtitle("Comparison excess death") +
  theme_bw()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(0.5, 0.8),
    legend.text = element_text(size=text_size),
    axis.text.x = element_text(size=text_size,angle=45,hjust=1),
    axis.text.y = element_text(size=text_size),
    axis.title  = element_text(size=text_size),
    title =element_text(size=title_size))

ggsave("figures/figure_comparison.png",h=8,w=15)  

