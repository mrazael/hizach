top16subs_2014 = c("AskReddit", "funny", "pics", "AdviceAnimals", "WTF", "IAmA", "videos", "gaming", "todayilearned",
         "leagueoflegends", "worldnews", "twitchplayspokemon", "nfl", "dogecoin", "nba", "hockey")

top16subs_2015 = c('AskReddit', 'nfl', 'funny', 'leagueoflegends', 'pics', 'worldnews',
                   'DestinyTheGame', 'todayilearned', 'AdviceAnimals', 'videos',
                   'pcmasterrace', 'nba', 'SquaredCircle', 'hockey', 'news', 'CFB')

samplesubs_2015 = c('AskReddit', 'AskMen', 'rwbyRP', 'rupaulsdragrace', 'DebateReligion', 'Frugal',
               'talesfromtechsupport', 'steroids', 'melbourne', 'GetMotivated', 'spikes', 'history',
               'Coachella', 'jobs', 'mexico', 'MorbidReality', 'gardening', 'blog', 'community',
               'fitnesscirclejerk', 'WWE', 'TrueAtheism', 'TalesFromYourServer', 'bostonceltics',
               'PrivateFiction', 'saplings', 'freemasonry', 'gundeals', 'coins', 'Turkey',
               'FallOutBoy', 'finance', 'HongKong', 'TheWire', 'Overwatch', 'peloton', 'MAA',
               'BeautyBoxes', 'gaybrosgonemild', 'PerfectTiming', '52book', 'rccars', 'twinks',
               'FarCry4', 'YouEnterADungeon', 'birthcontrol', 'ChronicPain', 'WebGames',
               'MedievalEngineers', 'Comcast', 'bdsm', 'Hair', 'HaloPlayers', 'exo', 'autism',
               'Ooer', 'latin', 'straya', 'FolkPunk', 'JuggaloTuggalo', 'BeardPorn', 'providence',
               'ginger', 'ShittyLifeProTips', 'circojeca', 'awesome', 'Equestrian', 'changetip',
               'nl_Kripparrian', 'MHOCPress')

sample16 = c("AskMen", "talesfromtechsupport", "Coachella", "DebateReligion", "TrueAtheism", "WWE",
             "PerfectTiming", "saplings", "gaybrosgonemild", "awesome", "FarCry4", "bostonceltics",
             "FallOutBoy", "Ooer", "changetip", "JuggaloTuggalo")

options(scipen = 999) #disable scientific notation

###Libraries###
library(tidyverse)
library(jsonlite)
library(tidytext)
library(stringr)
library(sjPlot)
library(ggResidpanel)
get_sentiments("afinn")
###############

```
df <- read.delim("/home/mrazael/Desktop/University of Edinburgh/github/uoe/files/2015/top16.csv", quote = "", fill = T, header = T)
df_sample <- read.delim("/home/mrazael/Desktop/University of Edinburgh/github/uoe/files/2015/redditsyssample.csv", quote = "", fill = T, header = T)
df_main <- read.delim("/home/mrazael/Desktop/University of Edinburgh/github/uoe/files/2015/mainsubs.csv", quote = "", fill = T, header = T)
```

df <- df_main %>% #Extract 'Day' from 'created_utc'
  mutate(Day = as.integer(substring(created_utc, 9, 10))) %>%
  select(Day, author, body, score, subreddit)

ranked_users <- df %>%
  group_by(Day, author) %>%
  summarise(n = n(),
            karma = sum(score)) %>%
  ungroup() %>%
  arrange(desc(n)) %>% #ranks numbers and posts counts for each user in each subreddit!!!
  group_by(Day) %>%
  mutate(rank = row_number()) %>%
  ungroup()

ranked_users %>% #Confirming that a single user has multiple entries
  filter(author == "Late_Night_Grumbler")

### Creaeting muslim/islam subset###

muslim <- df %>%
  filter(str_detect(c("islam", "muslim"), body))

charlie_ranked <- muslim %>% #Counting how many mentions comes from each subgroup
  group_by(Day, author) %>%
  summarise(n = n(),
            karma = sum(score)) %>%
  ungroup() %>%
  arrange(desc(n)) %>% #ranks numbers and posts counts for each user in each subreddit!!!
  group_by(Day) %>%
  mutate(rank = row_number()) %>%
  mutate(Group = factor(ifelse(rank <= max(rank)*0.01, "Superuser",
                               ifelse(rank <= max(rank)*0.10, "Contributor", "Lurker")),
                        levels = c("Lurker", "Contributor", "Superuser")),
         Rank = factor(ifelse(Group == "Lurker", "Bottom 90%", "Top 10%"), levels = c("Top 10%", "Bottom 90%"))) %>%
  ungroup()

charlie_sum <- charlie_ranked %>%
  group_by(Day, Rank) %>%
  mutate(posts = sum(n),
         karma = sum(karma)) %>%
  select(Day, Rank, posts, karma) %>%
  distinct(., Day, .keep_all = T)

charlie_sum %>%
  group_by(Rank) %>%
  ggplot(., aes(x = Day, y = posts, fill = Rank)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  scale_x_continuous("\nDay (January 2015)", breaks = seq(from = 1, to = 31, by = 1)) +
  ylab("Submissions") +
  theme_minimal()

namedata <- charlie_ranked %>%
  select(Day, author, Rank)

##SENTIMENT ANALYSIS##

data(stop_words)

muslim_sent <- muslim %>%
  unnest_tokens(word, body) %>%
  anti_join(stop_words)

bing <- get_sentiments("bing")

whole <- inner_join(charlie_sent, bing) %>%
  group_by(author, Day) %>%
  summarise(positive = sum(ifelse(sentiment == "positive", 1, 0)),
            negative = sum(ifelse(sentiment == "negative", -1, 0))
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "sentiment")

whole <- inner_join(whole, namedata)

whole_sum <- whole %>%
  group_by(Day, Rank, sentiment) %>%
  summarise(sentiment = sentiment,
            value = sum(value)) %>%
  distinct(., Day, .keep_all = T) %>%
  mutate(my_alpha = ifelse(Rank == "Top 10%", 0.50, 0.75))

whole_sum %>%
  ggplot(., aes(x = Day, y = value, group = Rank, fill = sentiment)) +
  geom_col(aes(alpha = I(my_alpha)), color = "black")

whole_sum %>%
  ggplot(., aes(x = Day, y = value, group = Rank, fill = sentiment)) +
  geom_col(aes(alpha = I(my_alpha)), color = "black") +
  scale_fill_manual(values = c("red", "cadetblue3")) +
  theme_minimal() +
  scale_x_continuous("\nDay (January 2015)", breaks = seq(from = 1, to = 31, by = 1)) +
  scale_y_continuous("Sentiment (count)\n", breaks = seq(from = -40000, to = 20000, by = 10000)) +
  ylab("Sentiment (count)\n")

###########################

ranked_sample <- df_sample %>%
  group_by(subreddit, author) %>%
  summarise(n = n(),
            karma = sum(score)) %>%
  ungroup() %>%
  arrange(desc(n)) %>% #ranks numbers and posts counts for each user in each subreddit!!!
  group_by(subreddit) %>%
  mutate(rank = row_number()) %>%
  ungroup()

values <- vector()

for (sred in samplesubs_2015) {
  mdl <- ranked_sample %>%
  filter(subreddit == sred) %>%
  lm(log(n) ~ log(rank), data = .)
  listtmp = round(summary(mdl)$r.squared, 3)
  values <- append(values, listtmp)
  
  print(c(sred, listtmp)) #gives R^2 for all individual subreddits
} #this was my first ever for-loop I've created in R!

mdl <- lm(nlog ~ 0 + ranklog*subreddit, data = ranked_sample)

mdl_sample <- ranked_sample %>%
  sample_frac(0.10) %>%
  lm(nlog ~ 0 + ranklog*subreddit, data = .)


"To assess the viability of our model, we will check the model assumptions:
linearity and equal variance (via plot of residuals vs fitted values, expecting a
horizontal line), independence (with the previous plot and a plot of residuals vs index,
expecting a horizontal line on the former and a randomised spread on the latter), and
normality of errors (via a qqplot and histogram, expecting a diagonal line and normal distribution)."


### R² values plots with samples of 2.5% of users ###

set.seed(nchar("https://www.youtube.com/watch?v=dQw4w9WgXcQ"))
values <- vector()
for (sred in sample(samplesubs_2015, 16)) {
  set.seed(nchar("https://www.youtube.com/watch?v=dQw4w9WgXcQ")) 
  mdl <- ranked_sample %>%
    filter(subreddit == sred) %>%
    lm(log(n) ~ log(rank), data = .)
  listtmp = round(summary(mdl)$r.squared, 3)
  values <- c(values, listtmp)
  
  print(c(sred, listtmp))
}


#####################################################

for (sred in sample16) {
  mdl <- samplesubs16 %>%
    filter(subreddit == sred) %>%
    lm(log(n) ~ log(rank), data = .)
  listtmp = round(summary(mdl)$r.squared, 3)

  print(c(sred, listtmp))
}

##WORLDNEWS POST-HOC##

wnews <- read_csv("/home/mrazael/Desktop/University of Edinburgh/github/uoe/files/2015/worldnews.csv")

wnews <- wnews %>%
  mutate(Day = as.integer(substring(date_created, 9, 10)),
         Month = as.integer(substring(date_created, 6, 7)),
         Year = as.integer(substring(date_created, 1, 4))) %>%
  select(Year, Month, Day, up_votes, title, author)

wnews %>% #We tried a lot of things on the fly while doing this so it kind of evolved into a monster
  filter(Year == 2015, Month == 1) %>%
  group_by(Day, author) %>%
  summarise(n = n(),
            karma = sum(up_votes)) %>%
  arrange(desc(n)) %>%
  group_by(Day) %>%
  mutate(rank = row_number(),
         Group = factor(ifelse(rank <= max(rank)*0.01, "Superuser",
                               ifelse(rank <= max(rank)*0.10, "Contributor", "Lurker")),
                        levels = c("Lurker", "Contributor", "Superuser")),
         Rank = factor(ifelse(Group == "Lurker", "Bottom 90%", "Top 10%"), levels = c("Top 10%", "Bottom 90%"))) %>%
  select(-rank) %>%
  group_by(Day, Rank) %>%
  summarise(posts = sum(n),
            karma = sum(karma)) %>%
  ggplot(., aes(x = Day, y = posts, fill = Rank)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  scale_x_continuous("\nDay (January 2015)", breaks = seq(from = 1, to = 31, by = 1)) +
  ylab("Submissions") +
  theme_minimal()

### Creating a summary table of Group ratios ###

wnews %>%
  group_by(Year, Month, Day, author) %>%
  summarise(n = n(),
            karma = sum(up_votes)) %>%
  arrange(desc(n)) %>%
  group_by(Day) %>%
  mutate(rank = row_number(),
         Group = factor(ifelse(rank <= max(rank)*0.01, "Superuser",
                               ifelse(rank <= max(rank)*0.10, "Contributor", "Lurker")),
                        levels = c("Lurker", "Contributor", "Superuser")),
         Rank = factor(ifelse(Group == "Lurker", "Bottom 90%", "Top 10%"), levels = c("Top 10%", "Bottom 90%"))) %>%
  select(-rank) %>%
  group_by(Year, Rank) %>%
  summarise(posts = sum(n)) %>%
  group_by(Year) %>%
  mutate(prop = posts/sum(posts)) %>%
  select(-posts) %>%
  ungroup() %>%
  pivot_wider(names_from = Year, values_from = prop) %>%
  kbl(digits = 3,
      escape = F,
      align = "c",
      booktabs = T,
      linesep = "") %>%
  kable_paper(c("hover", "responsive")) %>%
  kable_styling(font_size = 10,
                latex_options = "HOLD_position")


### RESIDUAL PANELS ###

mdl_sample2 <- new %>%
  sample_frac(0.05) %>%
  lm(nlog ~ ranklog, data = .)


### MODELS ###

new <- df_sample %>%
  group_by(author) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number(),
         nlog = log(n),
         ranklog = log(rank))

mdl <- lm(nlog ~ ranklog*subreddit, data = ranked_sample)
mdl2 <- lm(nlog ~ ranklog, data = new)

hist(coef(mdl)[72:140], col = "skyblue3", breaks = 20, xlab = "Estimate", main = "")
mean(coef(mdl)[72:140])
sd(coef(mdl)[72:140])