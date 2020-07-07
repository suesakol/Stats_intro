
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






#We begin by creating some data in a tibble
tib_t <- tibble(Group = c(rep("Group1", times = 30), rep("Group2", times = 30)),
                Scores = c(rnorm(n = 30, mean = 450, sd = 50), rnorm(n = 30, mean = 550, sd = 50))
)

tib_t %$%
  t.test(Scores ~ Group)

mod1 <- tib_t %$%
  lm(Scores ~ Group)

summary(mod1)