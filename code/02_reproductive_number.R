# Greatleap online lecture 21.11.2025
# Retrospective modelling of epidemics using historical mortality data
# Instructor: Katarina Matthes (UZH)


# load libraries, plot parameters, colors, etc.
rm(list=ls())
source("code/00_setup.R")

#############################################
# plot example for basis reproduction number#
#############################################

# plot example for basis reproduction number:
#  parameters 
  
R0 <- 12        # basis reproduction number
GT <- 10        # generation time (days)
I0 <- 1        # initial cases at time 0
t_max <- 30   # days to simulate
times <- seq(0, t_max, by = 1)

# exponential model
r_growth <- log(R0) / GT # growth rate

incidence <- I0 * exp(r_growth  * times) # incidence per day given R0=2 

df <- data.frame(
  time = times,
  incidence = incidence
)

# doubling time, after how many days are the number duplicated?
doubling_time <- log(2) / r_growth 

# plot
ggplot(df) +
  geom_line(aes(x = time, y = incidence),size = lwd_size) +
  geom_point(aes(x = time, y = incidence),size = point_size) +
  xlab("Time (days)")+
  ylab("Incidence") + 
  theme_bw()+
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size=text_size),
    axis.text.y = element_text(size=text_size),
    axis.title  = element_text(size=text_size),
    title =element_text(size=title_size))

ggsave("figures/figure_exponential_measles30_4.png",h=8,w=8)


#############################################
# plot example for basis reproduction number#
#############################################

dt <- read.csv("data/daily.csv", sep=";") %>%
  mutate(
    date =  dmy(date)
  )


before <- 7 # how many days included before
after <- 7 # how many days included after
begin <- ymd(18891210 ) # begin estimation, you cannot start with zero, will lead to an error
end <- ymd(18900131)  #  end estimation

GT <- 3 # generation time
GT_sd <- 1 # generation time sd
rate <- GT/GT_sd^2 # calculate rate
shape <- GT^2/GT_sd^2 # calculate shape



dt_re <- data.frame(date = seq(begin + before, end - after, 1),
                    Re = NA,
                    Re_lower = NA,
                    Re_upper = NA)


for(i in 1:length(dt_re$date)) {
  set <- subset(dt, date >= (dt_re$date[i] - before) & date <= (dt_re$date[i] + after))
  fit <- glm(death ~ date, family = quasipoisson, data = set)
  # fit <- glm(death ~ date, family = poisson, data = set)
  # fit <- glm.nb(death ~ date, data = set)
  fit.ci <- confint(fit)
  dt_re[i, 2:4] <-c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape) 
}

dt_re

ggplot(dt_re) +
  geom_hline(yintercept=1, col="darkgrey", lwd=0.8)+
  geom_line(aes(y=Re ,x= date), linetype="solid",lwd=lwd_size) + 
  geom_ribbon(aes(ymin=Re_lower, ymax=Re_upper,x=date, y=Re), alpha=0.3) + 
  scale_x_date(labels = date_format("%d/%m/%Y"), breaks = date_breaks("4 days")) +
  xlab("Date")+
  ylab("Reproduction number") +
  ggtitle(paste0("Reproduction number")) +
  theme_bw()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size=text_size,angle=45,hjust=1),
    axis.text.y = element_text(size=text_size),
    axis.title  = element_text(size=text_size),
    title =element_text(size=title_size))

ggsave("figures/figure_re.png",h=8,w=15)
