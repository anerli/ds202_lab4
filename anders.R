library(tidyverse)

def <- readxl::read_excel('cyclonesFootball2019.xlsx', sheet='Defensive')
ofn <- readxl::read_excel('cyclonesFootball2019.xlsx', sheet='Offensive')
bio <- readxl::read_excel('cyclonesFootball2019.xlsx', sheet='Biography')

str(def)
str(ofn)
str(bio)

# === PART ONE: CLEANING =======================================================

# Make player names and opponent names factors

def <- def %>% 
  mutate(Name=as.factor(Name), Opponent_Opponent=as.factor(Opponent_Opponent))
ofn <- ofn %>% 
  mutate(Name=as.factor(Name), Opponent_Opponent=as.factor(Opponent_Opponent))
bio <- bio %>% 
  mutate(Name=as.factor(Name))

str(def)
str(ofn)
str(bio)

# Make statistics numerical
def <- def %>%
  mutate(across(Tackles_Solo:Pass_PB, as.numeric))
str(def)
ofn <- ofn %>%
  mutate(across(Rushing_ATT:Passing_INT, as.numeric))
str(ofn)

# Make height in bio numerical
str(bio)

bio <- bio %>%
  separate(Height, into=c('HeightFeet', 'HeightInches'), sep='-') %>%
  mutate(across(c(HeightFeet, HeightInches), as.numeric)) %>%
  mutate(Height=12*HeightFeet + HeightInches) %>%
  select(-HeightFeet, -HeightInches)

# Now height is just in inches
str(bio)

# === PART TWO: TIDYING ========================================================


