# Code for `It Takes a Motive: Communal and Agentic Articulated Interest and Candidate Emergence`
# By Meredith Conroy and Jon Green

# Below is the code use to produce the tables, figures, and models in the paper
# To protect respondent privacy and in accordance with our IRB, 
  # we are unable to post accompanying data
# Interested researchers will need to acquire their own IRB 
  # and agreement with Run for Something in order to access the underlying data

# load useful packages
library(MASS)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ldatuning)
library(Zelig)
library(quanteda)

set.seed(12345)

load(paste0(file_path, "rfs_data.RData"))

# identify and assign tags
tags <- unique(tolower(unlist(strsplit(rfs_sub$can2_user_tags, split = ",", fixed = TRUE))))

call <- c(5, 35)
one_on_one <- c(7)

rfs_sub$call <- NULL
rfs_sub$call[grepl(tags[5], tolower(rfs_sub$can2_user_tags))] <- 1
rfs_sub$call[grepl(tags[35], tolower(rfs_sub$can2_user_tags))] <- 1
rfs_sub$call[is.na(rfs_sub$call)] <- 0

rfs_sub$one_on_one <- NULL
rfs_sub$one_on_one[grepl(tags[7], tolower(rfs_sub$can2_user_tags))] <- 1
rfs_sub$one_on_one[is.na(rfs_sub$one_on_one)] <- 0

rfs_sub <- rfs_sub %>%
  mutate(action_ordinal = case_when(
    call == 1 & one_on_one == 0 & running == 0 ~ 1,
    one_on_one == 1 & running == 0 ~ 2,
    running == 1 ~ 3,
    call == 0 & running == 0 ~ 0
  ))

rfs.matched <- rfs.matched %>% left_join(rfs_sub[,c("uuid","action_ordinal")], by = "uuid")

# define suburban as reference category
rfs.matched$USR <- relevel(factor(rfs.matched$USR), ref = "S")

# convert text column to character
rfs.matched$whyrun <- as.character(rfs.matched$whyrun)

# make corpus
cor <- quanteda::corpus(rfs.matched, text_field = "whyrun", 
                        docid_field = "uuid")

# make tokens, remove stopwords
tok <- quanteda::tokens(cor, remove_punct = T, remove_symbols = T) %>%
  quanteda::tokens_remove(stopwords(source = "snowball")) %>%
  quanteda::tokens_ngrams(1)

# make dfm, convert to dataframe
dfm.full <- quanteda::dfm(tok, verbose = T, tolower = T)
dfm.df <- data.frame(dfm.full)

# define dictionaries
trapnell.agent <- c("wealth","wealthy",
                    "pleasure","pleasures","pleasurable",
                    "influence","influences","influential","influencing","influencer",
                    "competent","competence",
                    "achieve","achieving","achieved",
                    "ambition","ambitions","ambitious",
                    "power","powerful","excitement","excite","excited",
                    "status","recognition","recognized","recognize",
                    "superior","superiority","autonomous","autonomy") 

gebauer.agent <- c("adventurous","adventure","adventures","adventurer","adventuring",
                   "bossy","boss","clever","competitive","competition","compete",
                   "dominant","dominance","dominate","dominates",
                   "leader","leadership","lead","leads","led",
                   "outgoing","rational","wise","wiser")

trapnell.communal <- c("forgive","forgiving","forgave","forgives",
                       "trust","trusting","trusts","entrust","entrusts",
                       "equal","equality","humility","humble",
                       "altruism","altruistic","altruist","loyal","loyalty",
                       "polite","politeness","politely",
                       "honest","honesty","compassion"," compassionate",
                       "civil","civility","harmony","harmonious",
                       "tradition","traditions","traditional")

gebauer.communal <- c("affectionate","affection","affections",
                      "caring","care","cares","faithful","faith",
                      "kind","kindness","patient","patience",
                      "sensitive","sensitivity",
                      "trust","trusting","trusts","entrust","entrusts",
                      "understand","understands","understanding")

# flag which columns in dfm are dictionary terms
which.communal.narrow <- which(names(dfm.df) %in% trapnell.communal)
which.agentic.narrow <- which(names(dfm.df) %in% trapnell.agent)

which.communal.full <- which(names(dfm.df) %in% unique(c(trapnell.communal, gebauer.communal)))
which.agentic.full <- which(names(dfm.df) %in% unique(c(trapnell.agent, gebauer.agent)))

# get counts of terms in each sub-dictionary by response
dfm.df$sum.communal.narrow <- sapply(1:nrow(dfm.df), function(x){
  sum(dfm.df[x,which.communal.narrow])
})

dfm.df$sum.communal.full <- sapply(1:nrow(dfm.df), function(x){
  sum(dfm.df[x,which.communal.full])
})

dfm.df$sum.agentic.narrow <- sapply(1:nrow(dfm.df), function(x){
  sum(dfm.df[x,which.agentic.narrow])
})

dfm.df$sum.agentic.full <- sapply(1:nrow(dfm.df), function(x){
  sum(dfm.df[x,which.agentic.full])
})

# binary flags for if use is greater than 0
dfm.df$use.communal.narrow <- with(dfm.df, ifelse(sum.communal.narrow > 0, 1, 0))
dfm.df$use.agentic.narrow <- with(dfm.df, ifelse(sum.agentic.narrow > 0, 1, 0))

dfm.df$use.communal.full <- with(dfm.df, ifelse(sum.communal.full > 0, 1, 0))
dfm.df$use.agentic.full <- with(dfm.df, ifelse(sum.agentic.full > 0, 1, 0))

# categorical variables for whether a respondent used one, the other, or both
  # narrowly defined and fully defined
dfm.df$lang.cat.narrow <- with(dfm.df, case_when(
  use.communal.narrow == 0 & use.agentic.narrow == 0 ~ "neither",
  use.communal.narrow == 1 & use.agentic.narrow == 0 ~ "communal",
  use.communal.narrow == 0 & use.agentic.narrow == 1 ~ "agentic",
  use.communal.narrow == 1 & use.agentic.narrow == 1 ~ "both"
))

dfm.df$lang.cat.full <- with(dfm.df, case_when(
  use.communal.full == 0 & use.agentic.full == 0 ~ "neither",
  use.communal.full == 1 & use.agentic.full == 0 ~ "communal",
  use.communal.full == 0 & use.agentic.full == 1 ~ "agentic",
  use.communal.full == 1 & use.agentic.full == 1 ~ "both"
))

# sums of usage
agentic.sum.narrow <- data.frame(usage = colSums(dfm.df[,which.agentic.narrow]))
communal.sum.narrow <- data.frame(usage = colSums(dfm.df[,which.communal.narrow]))

agentic.sum.full <- data.frame(usage = colSums(dfm.df[,which.agentic.full]))
communal.sum.full <- data.frame(usage = colSums(dfm.df[,which.communal.full]))

# usage tables
usage.table.narrow <- bind_rows(communal.sum.narrow, agentic.sum.narrow)
usage.table.narrow$word <- c(rownames(communal.sum.narrow), rownames(agentic.sum.narrow))
usage.table.narrow$category <- c(rep("communal", nrow(communal.sum.narrow)), rep("agentic", nrow(agentic.sum.narrow)))

usage.table.narrow <- bind_rows(
  usage.table.narrow %>%
    filter(category == "communal") %>%
    arrange(desc(usage)),
  usage.table.narrow %>%
    filter(category == "agentic") %>%
    arrange(desc(usage))
)

usage.table.full <- bind_rows(communal.sum.full, agentic.sum.full)
usage.table.full$word <- c(rownames(communal.sum.full), rownames(agentic.sum.full))
usage.table.full$category <- c(rep("communal", nrow(communal.sum.full)), rep("agentic", nrow(agentic.sum.full)))

# how often is each dictionary word used?
usage.table.full <- bind_rows(
  usage.table.full %>%
    filter(category == "communal") %>%
    arrange(desc(usage)),
  usage.table.full %>%
    filter(category == "agentic") %>%
    arrange(desc(usage))
)

# append to df
rfs.matched <- bind_cols(rfs.matched, dfm.df[,c("sum.communal.narrow","sum.agentic.narrow","use.communal.narrow","use.agentic.narrow",
                                                "sum.communal.full","sum.agentic.full","use.communal.full","use.agentic.full",
                                                "lang.cat.narrow","lang.cat.full")])

# make using neither term category the reference
rfs.matched$lang.cat.full <- relevel(factor(rfs.matched$lang.cat.full), ref = "neither")
rfs.matched$lang.cat.narrow <- relevel(factor(rfs.matched$lang.cat.narrow), ref = "neither")

# basic descriptives
xtabs(~ use.communal.narrow + running, data = rfs.matched)
xtabs(~ use.agentic.narrow + running, data = rfs.matched)
xtabs(~ use.communal.narrow + use.agentic.narrow, data = rfs.matched)

xtabs(~ use.communal.full + running, data = rfs.matched)
xtabs(~ use.agentic.full + running, data = rfs.matched)
xtable::xtable(xtabs(~ use.communal.full + use.agentic.full, data = rfs.matched))

# descriptive tables
usage.tab <- 
  rfs.matched %>% group_by(running, gen.a, lang.cat.full) %>%
  summarise(n = n()) %>%
  mutate(rg = paste0(running, gen.a)) %>%
  group_by(rg) %>%
  mutate(prop = n / sum(n))

usage.tab.racegen <- 
  rfs.matched %>% group_by(running, gen.a, race.a, lang.cat.full) %>%
  summarise(n = n()) %>%
  mutate(rg = paste0(running, gen.a)) %>%
  group_by(race.a, rg) %>%
  mutate(prop = n / sum(n))

rfs.matched %>% filter(gen.a == 0) %>%
  group_by(running) %>%
  summarise(prop.agen = sum(use.agentic.full) / n())

# make figure 3
usage.group.desc.racegen <- 
  usage.tab.racegen %>% 
  filter(!lang.cat.full == "neither") %>%
  ggplot(aes(x = factor(lang.cat.full, 
                        levels = c("communal","agentic","both")),
             y = prop,
             fill = factor(rg)))+
  facet_wrap(~race.a, labeller = as_labeller(c("1" = "White", "0" = "Non-White")))+
  geom_bar(position = "dodge", stat = "identity", col = "black")+
  ylim(c(0,.25))+
  scale_fill_manual(name = "Group",
                    breaks = c("00","01","10","11"),
                    values = c("white","lightgrey","darkgrey","black"),
                    labels = c("Female, Didn't Run","Male, Didn't Run",
                               "Female, Ran","Male, Ran"))+
  scale_x_discrete(name = "Term Category Use",
                   breaks = c("communal","agentic","both","neither"),
                   labels = c("Only Communal","Only Agentic","Both","Neither"))+
  labs(y = "Proportion",
       title = "Term Category Use by Candidate Emergence, Race, and Sex",
       subtitle = "Proportion using neither term category omitted for clarity")+
  theme_bw()+
  theme(text = element_text(size = 16, family = "serif"),
        strip.text = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16)
  )
ggsave(usage.group.desc.racegen, file = "~/Desktop/DfP/rfs/figs/figure_3new_commagen_usage_group_desc.png", width = 16, height =  8)

# define emphasis variables (dictionary term count / total word count)
rfs.matched$emph.communal.narrow <- with(rfs.matched, sum.communal.narrow / why.wordcount)
rfs.matched$emph.agentic.narrow <- with(rfs.matched, sum.agentic.narrow / why.wordcount)

rfs.matched$emph.communal.full <- with(rfs.matched, sum.communal.full / why.wordcount)
rfs.matched$emph.agentic.full <- with(rfs.matched, sum.agentic.full / why.wordcount)

emph.tab <- 
  rfs.matched %>% 
  group_by(running, gen.a) %>%
  summarise(n = n(),
            emph.comm = mean(emph.communal.full),
            emph.agen = mean(emph.agentic.full)) %>%
  mutate(rg = paste0(running, gen.a))

emph.tab.racegen <- 
  rfs.matched %>% 
  group_by(running, gen.a, race.a) %>%
  summarise(n = n(),
            emph.comm = mean(emph.communal.full),
            emph.agen = mean(emph.agentic.full)) %>%
  mutate(rg = paste0(running, gen.a))

# make figure 4
emph.desc.racegen <- 
  emph.tab.racegen %>%
  reshape2::melt(id.vars = c("running","gen.a","race.a", "rg","n")) %>%
  ggplot(aes(x = factor(rg),
             y = value, 
             fill = factor(variable)))+
  facet_wrap(~race.a, labeller = as_labeller(c("1" = "White", "0" = "Non-White")))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_x_discrete(name = "Group",
                   breaks = c("00","01","10","11"),
                   labels = c("Female, Didn't Run","Male, Didn't Run",
                              "Female, Ran","Male, Ran"))+
  scale_fill_manual(name = "Term Category",
                    breaks = c("emph.comm","emph.agen"),
                    values = c("grey","black"),
                    labels = c("Communal","Agentic"))+
  labs(y = "Mean Emphasis",
       title = "Term Category Emphasis by Candidate Emergence, Race, and Sex",
       subtitle = "Emphasis measured by sum of words in term category divided by word count")+
  theme_bw()+
  theme(text = element_text(size = 18, family = "serif"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size =24)
  )
ggsave(emph.desc.racegen, file = "~/Desktop/DfP/rfs/figs/figure_4new_commagen_emphasis_group_desc.png", width = 16, height =  8)


### DESCRIPTIVES
prop.gen <- mean(rfs.matched$gen.comb.imp)
prop.race <- mean(rfs.matched$race.comb)
prop.usr <- prop.table(table(rfs.matched$USR))

prop.gen.r <- mean(rfs.matched$gen.comb.imp[rfs.matched$running == 1])
prop.race.r <- mean(rfs.matched$race.comb[rfs.matched$running == 1])
prop.usr.r <- prop.table(table(rfs.matched$USR[rfs.matched$running == 1]))

# proportion inferred
prop.igen <- rfs.matched %>% group_by(gen.comb.imp > 0 & gen.comb.imp < 1) %>%
  summarise(prop = n() / nrow(rfs.matched))
prop.irace <- rfs.matched %>% group_by(race.comb > 0 & race.comb < 1) %>%
  summarise(prop = n() / nrow(rfs.matched))

prop.igen.r <-  rfs.matched %>% filter(running == 1) %>%
  group_by(gen.comb.imp > 0 & gen.comb.imp < 1) %>%
  summarise(prop = n() / nrow(rfs.matched %>% filter(running == 1)))
prop.irace.r <-rfs.matched %>% filter(running == 1) %>%
  group_by(race.comb > 0 & race.comb < 1) %>%
  summarise(prop = n() / nrow(rfs.matched %>% filter(running == 1)))

# build descriptive table
desc.gen <- data.frame(prop.male = rbind(prop.gen, prop.gen.r),
                       prop.inf = rbind(prop.igen$prop[2], prop.igen.r$prop[2]))
desc.race <- data.frame(prop.white = rbind(prop.race, prop.race.r),
                        prop.inf = rbind(prop.irace$prop[2], prop.irace.r$prop[2]))


desctab <- data.frame(status = c("overall","running"))
desctab$race <- desc.race$`Proportion White`
desctab$gender <- desc.gen$`Proportion Female`

desc.usr <- data.frame(rbind(prop.usr, prop.usr.r))

names(desc.gen) <- c("Proportion Female","Proportion Inferred")
names(desc.race) <- c("Proportion White","Proportion Inferred")
names(desc.usr) <- c("Unknown", "Rural","Suburban","Urban")

# build p(Run | Race x Gender) table
p.run <- data.frame(rbind(prop.table(table(rfs.matched$running[rfs.matched$gen.a == 0 & rfs.matched$race.a == 0])),
                          prop.table(table(rfs.matched$running[rfs.matched$gen.a == 1 & rfs.matched$race.a == 0])),
                          prop.table(table(rfs.matched$running[rfs.matched$gen.a == 0 & rfs.matched$race.a == 1])),
                          prop.table(table(rfs.matched$running[rfs.matched$gen.a == 1 & rfs.matched$race.a == 1]))
))

names(p.run) <- c("Race/Gender", "Pct. Running")
p.run$`Race/Gender` <- c("Women of Color", "Men of Color", "White Women","White Men")

rgn <- rfs.matched %>% group_by(gen.a, race.a) %>%
  summarise(n = n())
rgnr <- rfs.matched %>% filter(running == 1) %>%
  group_by(gen.a, race.a) %>%
  summarise(n = n())

p.run$n <- NULL
p.run$n[1] <- rgn$n[rgn$gen.a == 0 & rgn$race.a == 0]
p.run$n[2] <- rgn$n[rgn$gen.a == 1 & rgn$race.a == 0]
p.run$n[3] <- rgn$n[rgn$gen.a == 0 & rgn$race.a == 1]
p.run$n[4] <- rgn$n[rgn$gen.a == 1 & rgn$race.a == 1]

p.run$r <- NULL
p.run$r[1] <- rgnr$n[rgnr$gen.a == 0 & rgnr$race.a == 0]
p.run$r[2] <- rgnr$n[rgnr$gen.a == 1 & rgnr$race.a == 0]
p.run$r[3] <- rgnr$n[rgnr$gen.a == 0 & rgnr$race.a == 1]
p.run$r[4] <- rgnr$n[rgnr$gen.a == 1 & rgnr$race.a == 1]

p.run$p <- with(p.run, r / n)


rfs.matched %>% group_by(race.comb > 0 & race.comb < 1,
                         gen.comb.imp > 0 & gen.comb.imp < 1) %>%
  summarise(n = n(),
            prop = n() / nrow(rfs.matched))

# build figure 1
overall.desc.plot <- 
  rfs.matched %>% group_by(running) %>%
  summarise(prop.white = mean(race.comb),
            prop.male = mean(gen.comb.imp),
            prop.race.inf = mean(race.comb < 1 & race.comb > 0),
            prop.gen.inf = mean(gen.comb.imp < 1 & gen.comb.imp > 0)) %>%
  reshape2::melt(id.vars = c("running","prop.race.inf","prop.gen.inf")) %>%
  mutate(inf.text = ifelse(variable == "prop.white", paste0("(", round(prop.race.inf, 2), ")"),
                           paste0("(", round(prop.gen.inf, 2), ")"))) %>%
  ggplot(aes(x = forcats::fct_rev(variable), y = value, label = inf.text))+
  facet_wrap(~running, nrow = 2, ncol = 1, 
             labeller = as_labeller(c("0" = "Did Not Run",
                                      "1" = "Ran")))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(hjust = -.2)+
  ylim(c(0,1))+
  coord_flip()+
  scale_x_discrete(name = "",
                   breaks = c("prop.white","prop.male"),
                   labels = c("White","Male"))+
  labs(y = "Proportion",
       title = "Demographics of Run for Something Respondents by Candidate Emergence",
       subtitle = "Proportion of group with inferred demographic characteristic shown in parentheses")+
  theme_bw()+
  theme(text = element_text(size = 16, family = "serif"),
        strip.text = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12, hjust = 0)
  )
ggsave(overall.desc.plot, file = "~/Desktop/DfP/rfs/figs/figure_1_desc_racegen.png", width = 12, height = 6)

# build figure 2
p.run.plot <- 
  rfs.matched %>% group_by(race.a, gen.a) %>%
  summarise(n = n(),
            cands = sum(running),
            p.run = sum(running) / n()) %>%
  mutate(rg = paste0(gen.a, race.a),
         proptext = paste0(cands," / ",n)) %>%
  arrange(p.run) %>%
  ggplot(aes(x = forcats::fct_inorder(rg), y = p.run, label = proptext))+
  geom_bar(stat = "identity")+
  geom_text(hjust = -.175)+
  scale_x_discrete(name = "",
                   breaks = c("00","10","01","11"),
                   labels = c("Women of Color","Men of Color","White Women","White Men"))+
  ylim(c(0,.16))+
  coord_flip()+
  labs(y = "Proportion Running",
       title = "Candidate Emergence by Race and Sex",
       subtitle = "Number of emerged candidates and total respondents in each group labeled",
       caption = "Note: 6943 respondents (67%) have both race and sex either reported or identified on the voter file; remaining respondents assigned probabilistically.")+
  theme_bw()+
  theme(text = element_text(size = 16, family = "serif"),
        strip.text = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12, hjust = 0)
  )
ggsave(p.run.plot, file = "~/Desktop/DfP/rfs/figs/figure_2_desc_prun.png", width = 12, height = 6)

# models
rfs.matched$USR <- relevel(rfs.matched$USR, ref = "S")

# ordinal logit for actions
actions_model.f <- polr(factor(action_ordinal) ~ race.a + std.age + USR +
                        log_why_wordcount,
                      data = rfs.matched[rfs.matched$gen.a == 0,])

actions_model.m <- polr(factor(action_ordinal) ~ race.a + std.age + USR +
                        log_why_wordcount,
                      data = rfs.matched[rfs.matched$gen.a == 1,])

summary(actions_model.f)
summary(actions_model.m)

# viz predicted probability of actions by demos
pmat.f <- expand.grid(race.a = c(0,1),
                      std.age = seq(from = -2, to = 2, by = 1),
                      USR = c("U","S","R"),
                      log_why_wordcount = median(rfs.matched$log_why_wordcount))
f.pred <- predict(actions_model.f, newdata = pmat.f, type = "probs")

pmat.f <- data.frame(cbind(pmat.f, f.pred))
pmat.f$gen.a <- 0

pmat.m <- expand.grid(race.a = c(0,1),
                      std.age = seq(from = -2, to = 2, by = 1),
                      USR = c("U","S","R"),
                      log_why_wordcount = median(rfs.matched$log_why_wordcount))
m.pred <- predict(actions_model.m, newdata = pmat.m, type = "probs")

pmat.m <- data.frame(cbind(pmat.m, m.pred))
pmat.m$gen.a <- 1


pmat.comb <- bind_rows(pmat.f, pmat.m)

pmat.comb %>% 
  reshape2::melt(id.vars = c("race.a","gen.a","USR","log_why_wordcount","std.age")) %>%
  filter(race.a == 1 & USR == "S") %>%
  ggplot(aes(x = std.age, y = value, col = factor(gen.a), group = factor(gen.a)))+
  facet_wrap(~variable, nrow = 2, ncol = 2)+
  geom_line()+
  theme_bw()


# logit for using communal vs. agentic language
mod.communal.full <- glm(use.communal.full ~ std.age + USR +
                           race.comb + gen.comb.imp + log_why_wordcount + running,
                         data = rfs.matched, 
                         family = "binomial")
mod.agentic.full <- glm(use.agentic.full ~ std.age + USR +
                          race.comb + gen.comb.imp + log_why_wordcount + running,
                        data = rfs.matched, 
                        family = "binomial")

# with race x gender interaction
mod.communal.full.rgint <- glm(use.communal.full ~ std.age + USR +
                                 race.comb * gen.comb.imp + log_why_wordcount + running,
                               data = rfs.matched, 
                               family = "binomial")
summary(mod.communal.full.rgint)
mod.agentic.full.rgint <- glm(use.agentic.full ~ std.age + USR +
                                race.comb * gen.comb.imp + log_why_wordcount + running,
                              data = rfs.matched, 
                              family = "binomial")
summary(mod.agentic.full.rgint)

# subset by gender
mod.communal.full.women <- glm(use.communal.full ~ std.age + USR +
                                 race.comb + log_why_wordcount + running,
                               data = rfs.matched[rfs.matched$gen.a == 0,], 
                               family = "binomial")
summary(mod.communal.full.women)
mod.communal.full.men <- glm(use.communal.full ~ std.age + USR +
                               race.comb + log_why_wordcount + running,
                             data = rfs.matched[rfs.matched$gen.a == 1,], 
                             family = "binomial")
summary(mod.communal.full.men)

mod.agentic.full.women <- glm(use.agentic.full ~ std.age + USR +
                                race.comb + log_why_wordcount + running,
                              data = rfs.matched[rfs.matched$gen.a == 0,], 
                              family = "binomial")
summary(mod.agentic.full.women)
mod.agentic.full.men <- glm(use.agentic.full ~ std.age + USR +
                              race.comb + log_why_wordcount + running,
                            data = rfs.matched[rfs.matched$gen.a == 1,], 
                            family = "binomial")
summary(mod.agentic.full.men)


# take out word count
mod.communal.nv.full <- glm((lang.cat.full == "communal") ~ std.age + USR +
                              race.comb + gen.comb.imp + running,
                            data = rfs.matched, 
                            family = "binomial")
mod.agentic.nv.full <- glm((lang.cat.full == "agentic") ~ std.age + USR +
                             race.comb + gen.comb.imp + running,
                           data = rfs.matched, 
                           family = "binomial")

summary(mod.communal.full)
summary(mod.agentic.full)

# final models pseudo-R-squared
pscl::pR2(mod.communal.full)[4]
pscl::pR2(mod.agentic.full)[4]
pscl::pR2(mod.communal.nv.full)[4]
pscl::pR2(mod.agentic.nv.full)[4]

pscl::pR2(mod.communal.full.men)[4]
pscl::pR2(mod.communal.full.women)[4]
pscl::pR2(mod.agentic.full.men)[4]
pscl::pR2(mod.agentic.full.women)[4]

# export table
apsrtable::apsrtable(mod.communal.full.men, mod.communal.full.women, 
                     mod.agentic.full.men, mod.agentic.full.women)

# alternative versions of the outcome variable
# negative binomial for token counts
nb.comm <- MASS::glm.nb(sum.communal.full ~ std.age + USR +
                          race.comb + gen.comb.imp + running + log_why_wordcount,
                        data = rfs.matched)
nb.agent <- MASS::glm.nb(sum.agentic.full ~ std.age + USR +
                           race.comb + gen.comb.imp + running + log_why_wordcount,
                         data = rfs.matched)

summary(nb.comm)
summary(nb.agent)


# lm for emphasis
mod.emph.commnal.full <- lm(emph.communal.full~ std.age + USR +
                              race.comb + gen.comb.imp + running, 
                            data = rfs.matched)
mod.emph.agentic.full <- lm(emph.agentic.full~ std.age + USR +
                              race.comb + gen.comb.imp + running,
                            data = rfs.matched)
summary(mod.emph.commnal.full)
summary(mod.emph.agentic.full)


# candidate emergence as the outcome variable
# narrow dictionaries
emergemod.lang.f.narrow <- glm(running ~ std.age + USR +
                                 log_why_wordcount + 
                                 race.comb +
                                 lang.cat.full,
                               data = rfs.matched[rfs.matched$gen.a == 0,],
                               family = "binomial")

emergemod.lang.f.nv.narrow <- glm(running ~ std.age + USR +
                                    race.comb +
                                    use.agentic.narrow + use.communal.narrow,
                                  data = rfs.matched[rfs.matched$gen.a == 0,],
                                  family = "binomial")

emergemod.lang.m.narrow <- glm(running ~ std.age + USR +
                                 log_why_wordcount + 
                                 race.comb +
                                 (use.agentic.narrow == 1 & use.both.narrow == 0) + 
                                 (use.communal.narrow == 1 & use.both.narrow == 0) +
                                 (use.both.narrow == 1),
                               data = rfs.matched[rfs.matched$gen.a == 1,],
                               family = "binomial")
emergemod.lang.m.nv.narrow <- glm(running ~ std.age + USR +
                                    race.comb +
                                    use.agentic.narrow + use.communal.narrow,
                                  data = rfs.matched[rfs.matched$gen.a == 1,],
                                  family = "binomial")
summary(emergemod.lang.f.narrow)
summary(emergemod.lang.m.narrow)

# full dictionaries
emergemod.lang.f.full <- glm(running ~ std.age + USR +
                               log_why_wordcount + 
                               race.comb +
                               lang.cat.full,
                             data = rfs.matched[rfs.matched$gen.a == 0,],
                             family = "binomial")

emergemod.lang.m.full <- glm(running ~ std.age + USR +
                               log_why_wordcount + 
                               race.comb +
                               lang.cat.full,
                             data = rfs.matched[rfs.matched$gen.a == 1,],
                             family = "binomial")

emergemod.lang.f.nv.full <- glm(running ~ std.age + USR +
                                  race.comb +
                                  lang.cat.full,
                                data = rfs.matched[rfs.matched$gen.a == 0,],
                                family = "binomial")

emergemod.lang.m.nv.full <- glm(running ~ std.age + USR +
                                  race.comb +
                                  lang.cat.full,
                                data = rfs.matched[rfs.matched$gen.a == 1,],
                                family = "binomial")
summary(emergemod.lang.f.full)
summary(emergemod.lang.m.full)

# test whether coefficients across subsetted models are significantly different from each other
car::linearHypothesis(emergemod.lang.f.full, "lang.cat.fullcommunal = -.32")
car::linearHypothesis(emergemod.lang.m.full, "lang.cat.fullcommunal = -.10")

# pseudo-R-squared for final models
pscl::pR2(emergemod.lang.f.full)[4]
pscl::pR2(emergemod.lang.m.full)[4]

pscl::pR2(emergemod.lang.f.nv.full)[4]
pscl::pR2(emergemod.lang.m.nv.full)[4]

# export table
apsrtable::apsrtable(emergemod.lang.f.full, emergemod.lang.m.full)

save.image(file = paste0(file_path, "rfs_appdendix.RData"))