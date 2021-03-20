library(tidyverse)

def <- readxl::read_excel('cyclonesFootball2020.xlsx', sheet='Defensive')
ofn <- readxl::read_excel('cyclonesFootball2020.xlsx', sheet='Offensive')
bio <- readxl::read_excel('cyclonesFootball2020.xlsx', sheet='Biography')

str(def)
str(ofn)
str(bio)

# === PART ONE: CLEANING =======================================================

# 1
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

# 2
# Make statistics numerical
def <- def %>%
  mutate(across(Tackles_Solo:Pass_PB, as.numeric))
str(def)
ofn <- ofn %>%
  mutate(across(Receiving_REC:Passing_INT, as.numeric))
str(ofn)

# 3
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

# 1

offClean <- ofn %>%
  pivot_longer(Receiving_REC:Passing_INT, names_to="stat")
str(offClean)

defClean <- def %>%
  pivot_longer(Tackles_Solo:Pass_PB, names_to="stat")
str(defClean)

# Unsure what bioClean would entail

# 2

offClean %>% ggplot(aes(x=value)) + geom_histogram() + facet_wrap(~stat) +
  xlab('Value of Statistic') + ylab('Number of Players with Value')

'
We see that the number of Receiving_REC, Receiving_TD, and Rushing_TD is relatively
low across players, whereas statistics like Receiving_YDS, Rushing_ATT, and
Rushing_YDS are more broadly distributed.
'

#View(offClean)

# 3


offClean %>%
  filter(Opponent_Opponent %in% c('Oregon', 'Oklahoma')) %>%
  filter(stat=='Receiving_YDS') %>%
  distinct(Name, Opponent_Opponent, .keep_all=TRUE) %>%
  pivot_wider(names_from=Opponent_Opponent, values_from=value) %>%
  ggplot(aes(x=Oregon, y=Oklahoma)) + geom_point() +
  xlab('Receiving Yards against Oregon') +
  ylab('Receiving Yards against Oklahoma') 
  

'
It seems as though in general, ISU did better against Oklahoma, since
there were a few players who did around 40 yards against Oklahoma as opposed
to around 20 against Oregon, as well as a couple outliers who had around 60 and 80
against Oklahoma nd only around 50 against Oregon.
'






"
dat <- offClean %>% 
  filter(Opponent_Opponent %in% c('Oregon', 'Oklahoma')) %>%
  filter(stat=='Receiving_YDS')
dat

# Add ID to keep duplicate rows
dat1 <- dat %>%
  mutate(id=row_number())
dat1

# Put Oklahoma Receiving YDS and Oregon Recieving YDS into their own columns (wider)
dat2 <- dat1 %>%
  pivot_wider(names_from=Opponent_Opponent, values_from=value) %>%
  # Remove id again
  select(-id)
dat2

dat2 %>% ggplot(aes(x=Oregon, y=Oklahoma))
"

# 4
bio1 <- bio %>%
  separate(Hometown, into=c('City', 'State'), sep=', ')

# 5

bio1 %>%
  ggplot(aes(x=fct_infreq(State))) + geom_bar() + coord_flip() +
  ylab('Number of Players') + xlab('State')

"
bio1 %>%
  group_by(State) %>%
  summarize(count = n()) %>%
  ggplot(aes(x=desc(reorder(State, count)), y=count)) + geom_bar(stat='identity')
"

# === PART TWO: JOINING ========================================================

# 1

# States of Defensive players
def_states <- def %>% left_join(bio1) %>% distinct(Name, .keep_all=TRUE)
str(def_states)

def_states %>%
  ggplot(aes(x=fct_infreq(State))) + geom_bar() + coord_flip() +
  ylab('Number of Defensive Players') + xlab('State')

# States of Offensive players
ofn_states <- ofn %>% left_join(bio1) %>% distinct(Name, .keep_all=TRUE)
str(ofn_states)

ofn_states %>%
  ggplot(aes(x=fct_infreq(State))) + geom_bar() + coord_flip() +
  ylab('Number of Offensive Players') + xlab('State')

# 2

purdy_def <- def %>% filter(Name == 'Purdy, Brock')

not_zero <- function(x){return(x != 0)}

purdy_def %>% mutate(across(Tackles_Solo:Pass_PB, not_zero))

purdy_def

'
We can see there is only one nonzero statistic for Purdy on defense, so there
is not enough data to try and determine his defensive contribution.
Therefore we will focus on his offensive contributions.
'

purdy_off <- ofn %>% filter(Name == 'Purdy, Brock')
purdy_off
View(purdy_off)

'
I am unfamiliar with Football in general and with what these statistics mean,
but Receiving_REC, Receiving_YDS, Receiving_TD, and Passing_CMP-ATT are all NA
for Purdy, so clearly they will not help us figure out his overall contribution
to the team.
'
purdy_off1 <- purdy_off %>% select(-Receiving_REC, -Receiving_YDS, -Receiving_TD, -`Passing_CMP-ATT`)
purdy_off1



