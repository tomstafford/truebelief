#'''
#load actual data and test
#'''

library(tidyverse)

# get data



dataloc <- "/home/tom/Downloads/indrostudy1data/study1_vaccine conspiracy_expressive responding_nums.csv"

df <- read_csv(dataloc)


# clean data

df <- df %>%
  filter(Progress==100) %>% 
  filter(Status==0) %>%
  filter(as.Date(EndDate) == as.Date("2022-08-02"))

df <- df %>%
  rename(Duration = `Duration (in seconds)`) %>%
  mutate(Duration = readr::parse_integer(Duration))

#find lower quartile of Duration
lower_quartile <- quantile(df$Duration, probs = 0.25)

df <- df %>% filter(Duration> lower_quartile*0.5)

# attention check & other data quality measures??

df <- df %>% 
  filter(Vaccine_conspiracy_7=="2") %>%
  filter(Vaccine_att_7=="5")

# n = 500

#drop att check columns

df <- df %>%
  select(!Vaccine_conspiracy_7) %>%
  select(!Vaccine_att_7)

# convert to integers

df <- df %>%
  mutate(across(Vaccine_conspiracy_1:Vaccine_conspiracy_8, as.integer)) %>%
  mutate(across(Vaccine_att_1:Vaccine_att_11, as.integer)) %>%
  mutate(across(Trust_NHS_1:Trust_UKGov_1, as.integer))

df$Year_born <- as.integer(df$Year_born)

# construct summary vars

df <- df %>%
  mutate(V = rowMeans(select(., starts_with("Vaccine_conspiracy_")), na.rm = TRUE)) %>%
  mutate(T = rowMeans(select(., starts_with("Trust_")), na.rm = TRUE)) %>%
  mutate(Vaccine_att_1 = 8 - Vaccine_att_1) %>% # reverse code so higher numbers are more skeptical attitudes
  mutate(Vaccine_att_2 = 8 - Vaccine_att_2) %>%
  mutate(C = rowMeans(select(., starts_with("Vaccine_att_")), na.rm = TRUE)) %>%
  mutate(A = 2023 - Year_born)

# sanity checks

min(df$A)
max(df$A)

p <- ggplot(df,aes(x=A))
p + geom_density()

p <- ggplot(df,aes(x=V))
p + geom_density()

p <- ggplot(df,aes(x=T))
p + geom_histogram()

p <- ggplot(df,aes(x=C))
p + geom_density()


# tests
# C  
# Vaccine_conspiracy_1
# Vaccine_conspiracy_7 #att check
# Vaccine_conspiracy_8
# 
# A
# Year_born
# 
# T
# Trust_NHS_1
# Trust_UKGov_1
# 
# V
# Vaccine_att_1
# Vaccine_att_2
# Vaccine_att_3 4 5 6 8 9 10 11 #reverse
# Vaccine_att_7 # att check

# Without controlling for V

m0 <- lm(data = df, C ~ A) # not controlling for V
summary(m0)

# A 0.005 (p = 0.023) - weak but significant

m1 <- lm(data = df, C ~ A + V) # controlling for V
summary(m1)

# A -0.001 (p = 0.3)
# V 0.39 (p<0.00001)

################# --- substituting in T(rust) for V(accine belief)

m0 <- lm(data = df, C ~ T) # not controlling for V
summary(m0)

# T -0.35 (p<0.0001)

m1 <- lm(data = df, C ~ A + T) # controlling for V
summary(m1)

# A 0.000 (p = 0.821)
# T -0.351 (p<0.0001)

write_csv(df %>% select(V,A,C,T),'obsdat.csv')