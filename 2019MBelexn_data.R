library(tidyverse)
d <- read_csv("~/Desktop/2019MBpoli.csv")
d

#popular vote vs actual vote count
pop <- tibble(Party = c("PC", "NDP", "LIB", "Green", "Other"), Popular = c(221007, 147337, 67978, 30175, 3080), numSeats = c(36, 18, 3, 0, 0))
pop$pop_percen <- pop$Popular/sum(pop$Popular)
pop$actual_percen <- pop$numSeats/sum(pop$numSeats)

pop_tab <- data.frame(Party = c(pop$Party, pop$Party), Type = c(rep("popular", 5), c(rep("actual", 5))), frac = c(pop$pop_percen, pop$actual_percen))

pie(subset(pop_tab, Type =="popular")$frac, labels = "", col = c("blue", "orange", "red", "forestgreen", "grey"), init.angle=90)

pie(subset(pop_tab, Type =="actual")$frac, labels = "", col = c("blue", "orange", "red", "forestgreen", "grey"), init.angle=90)

#Is there a difference in which party secured a majority of the popular vote to secure their seat?
table(d$Majority)

table(d$Majority, d$Party)

mat <- matrix(c(9, 10, 12, 26), 2, byrow = TRUE)
#     LIB+NDP PC
#no      9    10
#yes    12    26

fisher.test(mat)
#p = 0.26

#Is there a "Winnipeg" effect in whether there was a single candidate with a majority of the popular vote?
table(majority = d$Majority, Winnipeg = d$Winnipeg)

#           Winnipeg
# majority  no  yes
#       no   1 18
#       yes 23 15
fisher.test(table(d$Majority, d$Winnipeg))

subset(d, Winnipeg == 0 & Majority == 0)$Riding
#Spruce Woods

#Yes a very large effect. All non-Winnipeg ridings except for one (Spruce Woods) elected a single candidate with a majority of the popular vote.

d_sum <- d %>%
  group_by(Winnipeg, Majority) %>%
  summarize(Num_Ridings = n())

dSum_fig <- ggplot(d_sum, mapping = aes(Winnipeg, Num_Ridings, fill= Majority)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "place of residence", y = "Number of Ridings") +
  #scale_fill_discrete(name = "Majority vote?")
  #scale_x_discrete(breaks = c("0", "1"), label=c("other", "Winnipeg")) +
  scale_fill_manual(values=c("no"="darkred",
                             "yes"="cadetblue4"), name = "Majority Vote?") +
  annotate("text", x = 0.94, y = 33, label = "Fisher-exact test") +
  annotate("text", x = 0.78, y = 31, label = "p < 0.0001")

fisher.test(table(d$Majority, d$Gender_elected))
# p-value = 0.5408

#no influence of gender on likelihood of popular vote majority
d_sum_gen <- d %>%
  group_by(Gender_elected, Majority) %>%
  summarize(Num_Ridings = n())

d_sum_gen_fig <- ggplot(d_sum_gen, mapping = aes(Gender_elected, Num_Ridings, fill= Majority)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Gender elected", y = "Number of Ridings") +
  #scale_fill_discrete(name = "Majority vote?") +
  #scale_x_discrete(breaks = c("0", "1"), label=c("other", "Winnipeg")) +
  scale_fill_manual(values=c("no"="darkred",
                             "yes"="cadetblue4"), name = "Majority Vote?") +
  annotate("text", x = 0.94, y = 40, label = "Fisher-exact test") +
  annotate("text", x = 0.84, y = 38, label = "p = 0.54")

#Gender by incumbency - no effect
d_sum_gen_inc <- d %>%
  group_by(Gender_elected, NewRiding) %>%
  summarize(Num_Ridings = n())

fisher.test(table(d$NewRiding, d$Gender_elected))
#p = 0.15

fisher.test(table(d$Winnipeg, d$Gender_elected))
p = 0.01954

d_sum_gen_win <- d %>%
  group_by(Gender_elected, Winnipeg) %>%
  summarize(Num_Ridings = n())

d_sum_gen_part_win <- d %>%
  group_by(Gender_elected, Party) %>%
  summarize(Num_Ridings = n())

d_sum_gen_win_fig <- ggplot(d_sum_gen_win, mapping = aes(Winnipeg, Num_Ridings, fill=Gender_elected)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Residence", y = "Number of Ridings") +
  #scale_fill_discrete(name = "Majority vote?") +
  #scale_x_discrete(breaks = c("0", "1"), label=c("other", "Winnipeg")) +
  scale_fill_manual(values=c("F"= "#CC0066",
                             "M"="#0066CC"), name = "Gender elected") +
  annotate("text", x = 0.94, y = 40, label = "Fisher-exact test") +
  annotate("text", x = 0.74, y = 37, label = "p = 0.02")

#Influence of party
table(Party = d$Party, Gender = d$Gender_elected)

mat2 <- matrix(c(9, 8, 12, 28), 2, byrow = TRUE)
#         LIB+NDP PC
#women      9    8
#men      12    28

fisher.test(mat2)
#No. Fisher exact test, p = 0.21

d_sum_gen_party <- d %>%
  group_by(Gender_elected, Party) %>%
  summarize(Num_Ridings = n())

d_sum_gen_party_fig <- ggplot(d_sum_gen_party, mapping = aes(Party, Num_Ridings, fill=Gender_elected)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Party", y = "Number of Ridings") +
  #scale_fill_discrete(name = "Majority vote?") +
  #scale_x_discrete(breaks = c("0", "1"), label=c("other", "Winnipeg")) +
  scale_fill_manual(values=c("F"= "#CC0066",
                             "M"="#0066CC"), name = "Gender elected")

