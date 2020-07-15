
library(tidyverse)
library(magrittr)



# Exercise 0.1 ------------------------------------------------------------

#We begin by reading in the data into R
#Make sure that the file "blank.csv" is in your working directory
PMdat <- read_csv("blank.csv", 
                  col_types = cols(Date = col_date(format = "%m/%d/%Y"),
                                   Area = col_character(),
                                   Location = col_character()
                                   )
                  )


#Before we explore our data, let's check the area where recoding was made
PMdat %>% 
  group_by(Area) %>% 
  summarize(nobs = n())


#We will try to group areas into zones. We actually come up with haphazard zoing
#To recode multiple categories of a column, we can use case_when(condition ~ "new category")
PMdat <- PMdat %>% 
  mutate(Zones = case_when(
    Area %in% c("Klongsan", "Krungton", "Wuttakat", "Bangwa", "Charoennakorn") ~ "Thonburi",
    Area %in% c("Samyan", "Sapankwai", "Sukumwit", "Siam") ~ "Business",
    Area %in% c("Silom", "Satorn") ~ "Silom",
    Area %in% c("Bangbon", "Bangkae") ~ "Bangkae")
    )


#We split our column time (which is in Hr:Min:Sec format) 
PMdat <- PMdat %>% 
  separate(col = Time, into = c("Hour", "Minute","Second"), sep = ":", remove = FALSE)


#We then convert time into morning, afternoon, evening
PMdat <- PMdat %>% 
  mutate(TimeOfDay = case_when(Hour <  12 ~ "Morning",
                               Hour >= 12 & Hour <= 17 ~ "Afternoon",
                               Hour >  17 ~ "Evening")
  )


#To explore data, we will generate some plots with ggplot

#Our first plot concerns the time of day each recording was done
PMdat %>% 
  filter(!is.na(PM_level)) %>% 
  ggplot(aes(x = Date, y = PM_level, color = TimeOfDay)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_y_continuous(name = "PM concentration", breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_date(name = "Date", date_breaks = "1 week", date_labels = "%b %d")


#We move on to the second plot and this time zones were added in
PMdat %>% 
  filter(!is.na(PM_level)) %>% 
  ggplot(aes(x = Date, y = PM_level, color = TimeOfDay)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_y_continuous(name = "PM concentration", breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_date(name = "Date", date_breaks = "1 week", date_labels = "%b %d") +
  facet_wrap(~Zones)


#In our last example, we look at wind speed (which was centered)
PMdat %>% 
  filter(!is.na(PM_level)) %>% 
  mutate(Wind_centered = Wind_speed_mph - mean(Wind_speed_mph)) %>% 
  ggplot(aes(x = Wind_centered, y = PM_level)) +
  geom_point() +
  #geom_smooth(method = "lm") +
  theme_bw() +
  scale_y_continuous(name = "PM concentration", breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(name = "Centered wind speed", breaks = c(-6,-3, 0, 3, 6, 9, 12))










# Example 2 -------------------------------------------------------------

#Various distributions in R [normal, t distribution, f distribution]
ggplot(data = tibble(x = -5:5), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
  theme_bw() +
  labs(x = "Normal (Gaussian)") +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid")
        ) +
  ggsave("Normal.png")
  

ggplot(data = tibble(x = -5:5), aes(x = x)) +
  stat_function(fun = dt, args = list(df = 5)) +
  theme_bw() +
  labs(x = "(Student's) t") +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid")
        ) +
  ggsave("t.png")


ggplot(data = tibble(x = 0:6), aes(x = x)) +
  stat_function(fun = df, args = list(df1 = 3, df2 = 36)) +
  theme_bw() +
  labs(x = "F") +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid")
  ) +
  ggsave("f.png")



#Example 2.1: Plot the distribution of speech error
ggplot(data = tibble(x = 9:22), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 16, sd = 2)) +
  geom_segment(aes(x = 16, y = 0, xend = 16, yend = 0.20), linetype = "dashed", color = "red") +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid")
        ) +
  ggsave("NormalMean16.png")



ggplot(data = tibble(x = 9:22), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 16, sd = 2)) +
  stat_function(fun = dnorm, args = list(mean = 16, sd = 2), geom = "area", xlim = c(20,22)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid")
        )
  
  
#Find out how much probability lies between a range of value
#So to calculate how much probability exceeding a given value, we minus that from 1
pnorm(20, mean = 16, sd = 2)

1-pnorm(20, mean = 16, sd = 2)




#Example 2.2: Generate random samples
scores <- round(rnorm(n = 500, mean = 550, sd = 100), digits = 2)
sample <- replicate(n = 30, sample(scores, size = 30, replace = FALSE))
sample <- as_tibble(sample, .name_repair = "unique")
sample <- sample %>% 
  rename_all(list(~ str_c("Exp", c("01", "02", "03", "04", "05", "06", "07","08", "09", 10:30), 
                          sep = "_")
                  )
             )

#with new dplyr
sample <- sample %>% rename(across(everything()),
                                  str_c("Exp", c("01", "02", "03", "04", "05", "06", "07","08", "09", 10:30),
                                        sep = "_")
                           )


#Switch it to a long format
sample <- sample %>% 
  pivot_longer(cols = everything(), names_to = "Experiment", values_to = "Scores")


#Select one random sample. Let's say it's Exp_06
sample_sum <- sample %>% 
  filter(Experiment == "Exp_06") %>% 
  summarize(m = mean(Scores), sd = sd(Scores))


sample %>% 
  filter(Experiment == "Exp_06") %>%
  ggplot(aes(x = Scores)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "white", color = "grey") + 
  geom_density() +
  geom_segment(aes(x = sample_sum$m, y = 0, xend = sample_sum$m, yend = 0.005),
              linetype = "dashed") +
  theme_bw() +
  labs(x = "M = 526.72 (SD = 92.29)") +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )


#Plot the 30 random samples
sample %>% 
  ggplot(aes(x = Scores)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "white", color = "grey") + 
  geom_density() +
  theme_bw() +
  labs(x = element_blank(), y = element_blank()) +
  facet_wrap(~Experiment)


#Find summary statistics of the 30 samples
full_sample <- sample %>% 
  group_by(Experiment) %>% 
  summarize(m = mean(Scores), sd = sd(Scores))

full_sample %>% 
  summarize(m = mean(m))

full_sample %>% 
  ggplot(aes(x = m)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "white", color = "grey") + 
  geom_density() +
  xlim(c(500, 600)) +
  theme_bw() +
  labs(x = "M = 553 ms") +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )









ggplot(data = tibble(x = 150:190), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 170.3, sd = 6.3)) +
  scale_x_continuous(breaks = c(150, 160, 170, 180, 190)) +
  annotate(geom = "point", x = 170.3, y = 0, color = "red", size = 6, shape = 18) +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )


ggplot(data = tibble(x = 150:190), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 170.3, sd = 6.3)) +
  stat_function(fun = dnorm, args = list(mean = 170.3, sd = 6.3), geom = "area", xlim = c(158, 183)) +
  scale_x_continuous(breaks = c(150, 160, 170, 180, 190)) +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )



ggplot(data = tibble(x = 0:100), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 50, sd =10), aes(color = "10")) +
  stat_function(fun = dnorm, args = list(mean = 50, sd =15), aes(color = "20")) +
  labs(x = expression(paste("Normal with ", mu, " = 50"))) +
  scale_color_manual(name = "SD", values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = c(0.85,0.8), 
        text = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )


ggplot(data = tibble(x = 0:100), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 40, sd =15), aes(color = "40")) +
  stat_function(fun = dnorm, args = list(mean = 60, sd =15), aes(color = "60")) +
  labs(x = expression(paste("Normal with ", sigma, " = 15"))) +
  scale_color_manual(name = "Mu", values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = c(0.85,0.8), 
        text = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"))


ggplot(data = tibble(x = 80:220), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 170, sd = 20), aes(color = "170")) +
  stat_function(fun = dnorm, args = list(mean = 150, sd = 20), aes(color = "150")) +
  scale_color_manual(values = c("red", "blue")) +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        legend.position = "none"
        )



ggplot(data = tibble(x = rnorm(n = 40, mean = 170.3, sd = 6.3)), aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "white", color = "black") +
  geom_density() +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )



samp_mu <- numeric(length = 1000)
samp_sd <- numeric(length = 1000)

for(i in 1:1000) {
  x <- rnorm(n = 40, mean = 170.3, sd = 6.3)
  samp_mu[i] <- mean(x)
  samp_sd[i] <- sd(x)
  }


ggplot(data = tibble(mean = samp_mu), aes(x = mean)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.25, color = "black", fill = "white") +
  geom_density() +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )
  





ggplot(data = tibble(x = -4:4), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
  annotate(geom = "point", x = 1.75, y = 0, size = 4, shape = 18, color = "red") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.4), linetype = "dashed") +
  theme_bw() +
  labs(x = "H0 distribution") +
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )

#Solve where 97.5% is so we can split 
qt(p = 0.975, df = 28)

ggplot(data = tibble(x = -4:4), aes(x = x)) +
  stat_function(fun = dt, args = list(df = 28)) +
  stat_function(fun = dt, args = list(df = 28), geom = "area", xlim = c(-4, -2)) +
  stat_function(fun = dt, args = list(df = 28), geom = "area", xlim = c(2, 4)) +
  labs(x = "t distribution") +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )



ggplot(data = tibble(x = 0:5, y = 0:5), aes(x = x, y = y)) + 
  geom_segment(aes(x = 0, y = 0.8, xend = 4.5, yend = 5)) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )



dat <- tibble(Control = rnorm(n = 30, mean = 550, sd = 30),
              Experiment = rnorm(n = 30, mean = 500, sd = 30)
              )

dat %>% 
  summarize(across(.fns = c(mean, sd)))

 
dat <- dat %>% 
  pivot_longer(cols = everything(), names_to = "Group", values_to = "RT")



dat %>% 
  ggplot(aes(x = Group, y = RT)) +
  geom_point() +
  stat_summary(fun = "mean", geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), color = "red") +
  scale_y_continuous(breaks = seq(420, 620, by = 20)) +
  geom_segment(aes(x = 1, xend = 2, y = 545, yend = 495), color = "blue", ) +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.title = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid")
        )

dat %$% t.test(RT ~ Group)
dat_sum <- lm(RT ~ 1 + Group, data = dat)
summary(dat_sum)

