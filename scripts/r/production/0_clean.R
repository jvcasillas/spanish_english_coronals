# cleanup global environment
rm(list = ls(all = TRUE))

setwd("~/Box\ Sync/Spanish\ English\ Coronals\ Simonet\ Casillas\ Diaz/stats/1_production/")

library(stringr); library(ggplot2); library(lme4)
library(multcomp); library(MuMIn); library(lmerTest)
library(caret); library(MASS); library(car)
library(QuantPsyc); library(relaimpo); library(rms)
library(randomForest); library(pander); library(dplyr)


# read data
temp <- read.csv("coronals.csv", header = TRUE, na.strings = "")

# create participant column
temp$participant <- substr(temp$prefix, start=1, stop=11)
temp$participant <- gsub("[BIL]|[NSP]|[NEN]|_", "", paste(temp$participant))
temp$participant <- as.factor(temp$participant)


# create group column
temp$group <- substr(temp$prefix, start=1, stop=3)
temp$group <- as.factor(temp$group)

# create lang column
temp$lang <- str_sub(temp$prefix, start= -12)
temp$lang <- gsub("_|[0-9]", "", paste(temp$lang))
temp$lang <- str_sub(temp$lang, start= -7)
temp$lang <- as.factor(temp$lang)

# create word column
temp$word <- temp$prefix
temp$word <- gsub("_[0-9]+$|spanish|english", "", paste(temp$word))
temp$word <- gsub("\\_(.*?)\\_", "", paste(temp$word))
temp$word <- gsub("[BIL]|[NSP]|[NEN]|_", "", paste(temp$word))
temp$word <- gsub("[0-9]", "", paste(temp$word))
temp$word <- as.factor(temp$word)

# create consonant column
temp$phon <- substr(temp$word, start=1, stop=1)
temp$phon <- as.factor(temp$phon)

# create phoneme type column
temp$phonType <- NA
temp[temp$lang == "spanish" & temp$phon == "d", 16] <- "dSp"
temp[temp$lang == "spanish" & temp$phon == "t", 16] <- "tSp"
temp[temp$lang == "english" & temp$phon == "d", 16] <- "dEn"
temp[temp$lang == "english" & temp$phon == "t", 16] <- "tEn"
temp$phonType <- as.factor(temp$phonType)

# reorder
temp <- temp[,c(1, 11:16, 2:10)]
df <- temp

df$group <- droplevels(df$group)
df$participant <- droplevels(df$participant)


# Add new group column to divide bilinguals
df$group2 <- NA

df[df$group == 'NEN', 17] <- 'NEN'
df[df$group == 'NSP', 17] <- 'NSP'

df[df$participant == 'priscil' |
   df$participant == 'melinda' |
   df$participant == 'melissa' |
   df$participant == 'serenaV' |
   df$participant == 'lindao'  |
   df$participant == 'barbara' |
   df$participant == 'veronic' |
   df$participant == 'viviana', 17] <- 'biEng'

# bieng
# Rivera, Priscilla > priscil
# Porta, Melinda    > melinda
# Porta, Melissa    > melissa
# Valle, Serena     > serenaV
# Lopez, linda      > lindao
# Padilla, Barbara  > barbara
# Tanco, Veronica   > veronic
# Gracia, Viviana   > viviana


df[df$participant == 'izamarM' |
   df$participant == 'jeanett' |
   df$participant == 'yamileD' |
   df$participant == 'paulina' |
   df$participant == 'fernand' |
   df$participant == 'zaidaVe' |
   df$participant == 'genesis' |
   df$participant == 'erikaRe', 17] <- 'biEsp'

# biesp
# Murrieta, Izamar   > izamarM
# Valencia, Jeanette > jeanett
# Diaz, Yamile       > yamileD
# Bueno, Paulina     > paulina
# Bueno, Fernanda    > fernand
# Vega, Zaida        > zaidaVe
# Cubillas, Genesis  > genesis
# Redford, Erika     > erikaRe

df <- df[df$participant != 'karlaa', ]


df$group2 <- as.factor(df$group2)

df <- df[,c(1:2, 17, 4:16)]

# rename column
names(df)[names(df)=="group2"] <- "group"

# Clean word column
df[df$word == 'damnaton', 5] <- 'damnation'
df[df$word == 'dancet', 5] <- 'dancette'
df[df$word == 'danceur', 5] <- 'danseur'
df[df$word == 'dancinge', 5] <- 'dancing'
df[df$word == 'daniella', 5] <- 'danielle'
df[df$word == 'deltonian', 5] <- 'daltonian'
df[df$word == 'tablet', 5] <- 'tabloid'
df[df$word == 'tamourine', 5] <- 'tambourine'
df[df$word == 'tamper', 5] <- 'tabard'
df[df$word == 'tatooing', 5] <- 'tattooing'
df[df$word == 'tattoing', 5] <- 'tattooing'

df$word <- droplevels(df$word)
df$participant <- droplevels(df$participant)

# Fix language column
df[df$word == 'dagger', 4] <- 'english'
df[df$word == 'dakota', 4] <- 'english'
df[df$word == 'daltonian', 4] <- 'english'
df[df$word == 'damage', 4] <- 'english'
df[df$word == 'damnation', 4] <- 'english'
df[df$word == 'damper', 4] <- 'english'
df[df$word == 'dancette', 4] <- 'english'
df[df$word == 'dancing', 4] <- 'english'
df[df$word == 'danielle', 4] <- 'english'
df[df$word == 'danseur', 4] <- 'english'
df[df$word == 'dapper', 4] <- 'english'
df[df$word == 'dazzle', 4] <- 'english'
df[df$word == 'tabloid', 4] <- 'english'
df[df$word == 'taboo', 4] <- 'english'
df[df$word == 'tacit', 4] <- 'english'
df[df$word == 'tackle', 4] <- 'english'
df[df$word == 'tactics', 4] <- 'english'
df[df$word == 'tambourine', 4] <- 'english'
df[df$word == 'tanker', 4] <- 'english'
df[df$word == 'tantrum', 4] <- 'english'
df[df$word == 'tapioca', 4] <- 'english'
df[df$word == 'tattoo', 4] <- 'english'
df[df$word == 'tattooing', 4] <- 'english'
df[df$word == 'tabard', 4] <- 'english'

df[df$word == 'daba', 4] <- 'spanish'
df[df$word == 'dado', 4] <- 'spanish'
df[df$word == 'daga', 4] <- 'spanish'
df[df$word == 'daltonico', 4] <- 'spanish'
df[df$word == 'dama', 4] <- 'spanish'
df[df$word == 'danar', 4] <- 'spanish'
df[df$word == 'danes', 4] <- 'spanish'
df[df$word == 'danesa', 4] <- 'spanish'
df[df$word == 'danino', 4] <- 'spanish'
df[df$word == 'dano', 4] <- 'spanish'
df[df$word == 'danza', 4] <- 'spanish'
df[df$word == 'danzar', 4] <- 'spanish'
df[df$word == 'taberna', 4] <- 'spanish'
df[df$word == 'tabla', 4] <- 'spanish'
df[df$word == 'tabu', 4] <- 'spanish'
df[df$word == 'taco', 4] <- 'spanish'
df[df$word == 'tactil', 4] <- 'spanish'
df[df$word == 'tamano', 4] <- 'spanish'
df[df$word == 'tambien', 4] <- 'spanish'
df[df$word == 'tampoco', 4] <- 'spanish'
df[df$word == 'tanque', 4] <- 'spanish'
df[df$word == 'tanto', 4] <- 'spanish'
df[df$word == 'taza', 4] <- 'spanish'
df[df$word == 'tabaco', 4] <- 'spanish'



str(df)
dim(df)
summary(df)


# write table
write.table(df, "coronals_clean.csv", row.names = F, quote = F, sep = ",")




