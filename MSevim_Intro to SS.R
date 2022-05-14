# Introduction to R for social sciences

#**1. Reading in and checking data**

install.packages("dplyr") 
library("dplyr")
install.packages("digest")
library("digest") 

# In order to distinguish questions from comments, I will show the questions as "1.1.", "1.2."... "2.4.", etc.
# For example, "1.1." means the first question of the first part, "2.4." means the fourth question of the second part and so on.

# 1.1. Read in the two files "nodes" and "edges" and assign appropriate names. 
setwd("C:/Users/admin/Desktop/exam")
library(readr)
search()
nodes <- read_csv("nodes.csv")
head(nodes)
edges <- read_csv("edges.csv")
head(edges)
View(nodes)
View(edges)

# I rename nodes as "actors"; and edges as "connections".
actors <- nodes
View(actors)
connections <- edges
View(connections)

# 1.2. Apply the common data consistency checks, 
# i.e. rows/columns, names, object classes, values and value ranges, NAs. 
actors
connections
is.data.frame(actors)
is.data.frame(connections)
length(actors)
length(connections)

# for a general overview we can use str() or glipmse() functions
str(actors)
str(connections)
glimpse(actors)
glimpse(connections)

# rows/columns of actors(nodes) and connections(edges)
nrow(actors)
ncol(actors)
nrow(connections)
ncol(connections)
# or 
dim(actors)
dim(connections)

# names
attributes(actors)
attributes(connections)

# classes for general data 
class(actors)
typeof(actors)
class(connections)
typeof(connections)

# object classes for each variable in actors(nodes)
class(actors$doc_id)
class(actors$domain)
class(actors$orga_id)
class(actors$orga_name)
class(actors$orga_pos)
class(actors$orga_type)
class(actors$orga_ctr)
# object classes for each variable in connections(edges)
class(connections$src_orga_id)
class(connections$trg_orga_id)
# we can also see the object classes in a different way 
sapply(actors, class)
sapply(connections, class)
# or
t(t(sapply(actors, class)))
t(t(sapply(connections, class)))

# values and value ranges of actors(nodes) and connections(edges)
summary(actors)
summary(connections)

# Total of NAs
sum(is.na(actors))
sum(is.na(connections))
# Total of NAs per column
colSums(is.na(actors))
colSums(is.na(connections))
# If there are NAs, we can find where the NAs are
which(is.na(actors))
which(is.na(connections))
# What are the complete cases?
complete.cases(actors)
complete.cases(connections)

# 1.3. Should the objects of any variables be changed? Which ones and why?
class(actors$doc_id)
is.numeric(actors$doc_id)
class(actors$domain)
is.character(actors$domain)
class(actors$orga_id)
is.numeric(actors$orga_id)
class(actors$orga_name)
is.character(actors$orga_name)
class(actors$orga_pos)
is.numeric(actors$orga_pos)
class(actors$orga_type)
is.numeric(actors$orga_type)
class(actors$orga_ctr)
is.numeric(actors$orga_ctr)
class(connections$src_orga_id)
is.numeric(actors$orga_type)
class(connections$trg_orga_id)
is.numeric(actors$orga_type)
# After checking object classes of all variables 
# and ask whether it is TRUE or FALSE, we see that there is no FALSE.
# Normally, data in "orga_type" and "orga_ cts" could be written by names instead of numbers.
# For example, for "orga_type" it could be media, government agency and etc.
# And "orga_cts" could be arranged with the actor`s country name.
# In this case, we would assign them as character variable instead of numeric.
# Also, data in "orga_pos" could be written as TRUE/FALSE, instead of 1/2.
# In this case, the object class of "orga_pos" would be logical, not numeric.
# However, in some cases, it is easy to assign and code them as numeric instead of character or logical. 
# So, we do not need to change any object classes of any variables.

#     *******

# **2. Data Manipulation**

# ***Nodes

# 2.1. Delete the columns "doc_id" and "domain"
actors[c(1, 2)] <- list(NULL)
head(actors)
# or we can delete them by their names
# actors[c("doc_id", "domain")] <- list(NULL)
# head(actors)

# 2.2. Order the columns as follows:
# "orga_id", "orga_name", "orga_type", "orga_ctr", "orga_pos".
# There are a few ways to do it. We can reorder them by names shown as follows:
actors <- actors[c("orga_id", "orga_name", "orga_type", "orga_ctr", "orga_pos")]
head(actors)
# or shortly we could write
# actors <- actors[c(1,2,4,5,3)]
# another way to reorder them
# actors[ , c(1,2,4,5,3)] 
# Also, we could use select() or subset() functions.

# 2.3. Order the data in ascending order by their "orga_id".
actors <- actors[order(actors$orga_id), ]
head(actors)
# or we could use sort() for ascending order

# 2.4. We made a mistake when we specified the countries of actors: 
# those with the code 180 should actually get a 999. Recode the corresponding entries. 

# First, I want to check the 180 and 999 entries in "orga_ctr".
str(actors[actors$orga_ctr == 180,]) 
str(actors[actors$orga_ctr == 999,])
actors$orga_ctr[actors$orga_ctr == 180] <- as.integer(999)
actors$orga_ctr
str(actors[actors$orga_ctr == 180,])
# Now, there is no 180 entries in "orga_ctr".

# 2.5. Missing values: Replace 99 and 999 entries in "orga_pos" and "orga_type" by NA.
# First, I will check the entries. Another way to see the number of specific entries is sum() function.
sum(actors$orga_pos %in% c(99,999))
sum(actors$orga_type %in% c(99,999))
actors$orga_pos[actors$orga_pos %in% c(99,999)] <- NA
sum(actors$orga_pos %in% c(99,999))
actors$orga_type[actors$orga_type %in% c(99,999)] <- NA
sum(actors$orga_type %in% c(99,999))
# Now, there are no more(0) 99 and 999 entries in both "orga_pos" and "orga_type". 

# 2.6. We forgot to include an entry, the organization "KlimaHandel". 
# Insert the appropriate information after the last line: 
# orga_type = 2, orga_ctr = 2, orga_pos = 2.

# I assign an "orga_id" for "KlimaHandel": 441
klima_handel <- data.frame(441, "KlimaHandel", 2, 2, 2) 
names(klima_handel) <- c("orga_id", "orga_name", "orga_type", "orga_ctr", "orga_pos") 
actors <- rbind(actors, klima_handel) 
tail(actors)

# 2.7. We forgot a second entry, the organization "ClimateProbability". 
# We decide to assign random values for each variable.

# I assign an orga_id for "ClimateProbability": 442
climateprob <- data.frame(442, "ClimateProbability", 5, 4, 1) 
names(climateprob) <- c("orga_id", "orga_name", "orga_type", "orga_ctr", "orga_pos") 
actors <- rbind(actors, climateprob) 
tail(actors)

# 2.8. We want new column names: instead of "orga_..." we want "actor_...".
# Change the names accordingly. 
colnames(actors) = c("actor_id", "actor_name", "actor_type", "actor_ctr", "actor_pos") 
colnames(actors)

# 2.9. Are there certain lines in which there are a lot of missing values?
sum(is.na(actors))
# In total there are 275 missing values.
is.na(actors)
# TRUEs in the list show the missing values. Another way to find them is...
which(is.na(actors))
# We can see that there are some subsequent numbers showing missing values in the table.

# missing values according to variables/columns
which(is.na(actors$actor_id))
which(is.na(actors$actor_name))
which(is.na(actors$actor_type))
which(is.na(actors$actor_ctr))
which(is.na(actors$actor_pos))
# There are a lot of missing values in "actor_type" and "actor_pos",
# whereas there are no missing values in "actor_id", "actor_name" and "actor_ctr". 

# ***Edges

# 2.10. From "KlimaHandel" as a sender there are connections to the organizations "BBC" and "Plunge". 
# As a recipient, "KlimaHandel" itself is linked from "The Guardian". 

# from "KlimaHandel"(441) to "BBC"(27)
khandel_bbc <- data.frame(441, 27) 
names(khandel_bbc) <- c("src_orga_id", "trg_orga_id") 
connections <- rbind(connections, khandel_bbc) 
tail(connections)

# from "KlimaHandel"(441) to "Plunge"(268)
khandel_plunge <- data.frame(441, 268) 
names(khandel_plunge) <- c("src_orga_id", "trg_orga_id") 
connections <- rbind(connections, khandel_plunge) 
tail(connections)

# As a recipient, "KlimaHandel"(441) is linked with "The Guardian"(349).
guard_khandel <- data.frame(349, 441) 
names(guard_khandel) <- c("src_orga_id", "trg_orga_id") 
connections <- rbind(connections, guard_khandel) 
tail(connections)

# 2.11. "ClimateProbability" has no incoming connections as a receiver, 
# but three outgoing ones: to "UK Parliament", "Watts Up With That?" and "WWF UK".
cprob_ukpar <- data.frame(442, 399) 
names(cprob_ukpar) <- c("src_orga_id", "trg_orga_id") 
connections <- rbind(connections, cprob_ukpar) 
tail(connections)

# ...to"Watts Up With That?"(420);
cprob_wattsup <- data.frame(442, 420) 
names(cprob_wattsup) <- c("src_orga_id", "trg_orga_id") 
connections <- rbind(connections, cprob_wattsup) 
tail(connections)

#...to "WWF UK"(434).
cprob_wwfuk <- data.frame(442, 434) 
names(cprob_wwfuk) <- c("src_orga_id", "trg_orga_id") 
connections <- rbind(connections, cprob_wwfuk) 
tail(connections)

#     *******

# **3. Descriptive Statistics**

# ***Nodes

# 3.1. How many climate change skeptics(act_pos = 1), climate change advocates(2) 
# and actors without a clear position(99)? 

# Just for the clarification, in the second part we changed the all column names from "orga_" to "actor_".
# Since the new column name is "actor_pos", I use this name instead of "act_pos", and continue to use "actor_..." in the following parts.
table(actors$actor_pos)
# So, there are 686 skeptics(1) and 1350 advocates(2).
# Since we replaced previously all 99 entries by NA in "actor_pos" (orga_pos) variable,
# when we count NAs, we can find the number of 99s, thus, all actors without a clear position.
sum(is.na(actors$actor_pos))
# There are 162 NAs or previously 99s, so,the number of actors without a clear position is 162.
# Simply we could find it in one step:
table(actors$actor_pos, exclude = NULL)
#Again, 686 skeptics(1); 1350 advocates(2); and 162 NA(99),that is, actors without clear position.

# 3.2. What is the ratio of climate change advocates and skeptics in the US(act_ctr =3)?
usactors <- filter(actors, actor_ctr == 3)
table(usactors$actor_pos, exclude = NULL)
prop.table(table(usactors$actor_pos, exclude = NULL))
# Out of 521 there are 315 climate change skeptics(1), 186 climate change advocates(2) and 20 NA in the US.
# And the ratio of skeptics = 0.60460653; advocates = 0.35700576; actors without clear position (NA) = 0.03838772

# ***Edges

# 3.3. What are the top 5 actors by their activity as senders?
senders5 <- connections %>%
  group_by(src_orga_id) %>%
  summarise(app = n()) %>%
  arrange(desc(app)) %>%
  slice(1:5)
senders5
View(senders5)

# 3.4. Which are the top 5 actors measured by their popularity as a recipients?
recipients5 <- connections %>%
  group_by(trg_orga_id) %>%
  summarise(app = n()) %>%
  arrange(desc(app)) %>%
  slice(1:5)
recipients5
View(recipients5)

# 3.5. Create a new object "edg_sm" that contains all the actors in "nodes" sorted by their act_id in ascending order
# as well as the sum of their outgoing connections(act_sm) and the sum of their received connections(pop_sm). 
# Attention: Not all actors are active as senders and not all actors receive a connection from others.

# I understand this question in two ways.
# Based on my first understanding, I arrange "edg_sm" with three columns including all actors with unique "actor_id"s (not repeating).  
# To do that, I will first create three independent objects, then combine them with join () as "edg_sm".
# All the actors in "nodes" sorted by actor_id in ascending order:
actors_df1<- actors %>%
  group_by(actor_id) %>%
  count(actor_id) %>%
  arrange((actor_id))

# the sum of outgoing connections (act_sm):
actors_df2 <- connections %>%
  group_by(src_orga_id) %>%
  summarise(act_sm = n())

# the sum of received connections (pop_sm):
actors_df3 <- connections %>%
  group_by(trg_orga_id) %>%
  summarise(pop_sm = n())

# In order to use join(), I change the column names and create a common column in three new objects. 
# Since they have id`s in common, I want to join them by using the column "actor_id". 
# So, I need to change the column names just in the second and third objects.
colnames(actors_df2) = c("actor_id","act_sm") 
actors_df2
colnames(actors_df3) = c("actor_id", "pop_sm") 
actors_df3

# Now join them over the common "actor_id" and create "edg_sm" object.
edg_sm <- full_join(actors_df2, actors_df3, by = "actor_id")
View(edg_sm)
# After joining, the ascending order of "actor_id" a little bit changed. So, I fix it.
edg_sm<- edg_sm %>%
  group_by(actor_id) %>%
  arrange((actor_id))
head(edg_sm)
nrow(edg_sm)
length(unique(edg_sm$actor_id))
View(edg_sm)
# Now it shows "act_sm" and "pop_sm" according to "actor_id" in ascending order.

# Based on my second understanding, I create a new object with "actor_id" in ascending order, but some may repeat.
# In order to distinguish it , I name it this time "edg_sm1".
# I write it as comments in order to avoid any confusion:
# outgoing_connections <- connections %>%      
#  group_by(src_orga_id) %>%               
#  count() 
# received_connections <- connections %>%     
#  group_by(trg_orga_id) %>%             
#  count() 
# edg_sm1 <- left_join(actors,   
#                 outgoing_connections, 
#                 by = c("actor_id" = "src_orga_id")) %>%
#    left_join(received_connections, by = c("actor_id" = "trg_orga_id")) 

# I rename the outgoing_connections as act_sm and received_connections as pop_sm
# names(edg_sm1) <- c("actor_id", "actor_name", "actor_type",                 
#                "actor_ctr", "actor_pos", "act_sm", "pop_sm") 
# edg_sm1

# I need a little correction in ascending order on the bottom of "actor_id".
# edg_sm1<- edg_sm1 %>%
#  group_by(actor_id) %>%
#  arrange((actor_id))
# View(edg_sm1)

# 3.6. Add the two columns "act_sm", and "pop_sm" of "edg_sm" to "nodes".
#In both data "actors"(nodes) and "edg_sm", we have already a common column "actor_id" to join them.
actors <- full_join(actors, edg_sm, by = "actor_id")
actors
head(actors)
View(actors)

# 3.7. We want to find out who, relative to the own activity as a sender, receives a lot attention as a receiver.
# Create a new column "str" in "nodes", which indicates the ratio "act_sm" and "pop_sm".

# the ratio between "act_sm" and "pop_sm" in a new column "str" in "nodes".  
actors$str <- actors$act_sm/actors$pop_sm
head(actors)

# In a different way:
# actors <- within(actors, str <- act_sm / pop_sm)

#     *******
# **4. Visualizations**

# 4.1. Create a scatterplot of the variables "act_sm" and "pop_sm" in base R. Label the axes reasonably.
plot(actors$act_sm, actors$pop_sm, 
    main="Scatterplot of the senders and receivers",   
    xlab="senders", 
    ylab="receivers", pch=19)

# 4.2. Use ggplot2 and create another scatterplot of the variables "act_sm" and "pop_sm".
# Actors from the US(3), UK(4), and transnational actors(888) should be indicated with different colors.
# Make sure there is a legend to the plot. 
# Label the axes reasonably.
library(ggplot2)

color_ctr <- actors$actor_ctr
color_ctr[color_ctr!=3 & color_ctr!=4 & color_ctr!=888]<-"Other"
color_ctr[color_ctr==3] <- "US" 
color_ctr[color_ctr==4] <- "UK" 
color_ctr[color_ctr==888] <- "Transnational Actors"
actorname <- as.factor(color_ctr)

ggplot(actors, aes(x=act_sm, y=pop_sm, shape=actorname, color=actorname)) +  
    ggtitle("Actors by countries") + 
    xlab("senders") +
    ylab("receivers") +
    geom_point()

# 4.3. The plot created above seems confusing to us. 
# Create separate plots(a.k.a facets) in one graphic for the countries mentioned above.
# Choose a specific color for each country when you specify the scatterplots.
# Add a trend line for each plot, a country label in the header of each plot and label the axes.

color_ctr <- actors$actor_ctr 
color_ctr[color_ctr!=3 & color_ctr!=4 & color_ctr!=888]<-"Other" 
color_ctr[color_ctr==3] <- "US" 
color_ctr[color_ctr==4] <- "UK" 
color_ctr[color_ctr==888] <- "Transnational Actors" 
actorname <- as.factor(color_ctr) 
actors$actorname<-actorname 
p <- ggplot(actors, aes(x=act_sm, y=pop_sm, shape=actorname, color=actorname)) +  
    geom_smooth(mapping = aes(x = act_sm, y = pop_sm),
                method = "lm", se = FALSE) + 
    ggtitle("Actors by countries") + 
    xlab("senders") +
    ylab("receivers") +
    geom_point() 
p + facet_wrap(~actorname)

#*****
# Muhammed Sevim
