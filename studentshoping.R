library(ggplot2)
library(tidyverse)
#install.packages("forcats")
library(forcats)

##What students want out of the program
Track123Q5Comb <- Track123_Combined_Qs %>% 
  separate_rows(Q5Comb, sep = ";")

ggplot(subset(Track123Q5Comb, !is.na(Q5Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q5Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("What Students Are Hoping to Get from Participating in Duke Experiential Programs") +
  theme(plot.title = element_text(hjust = 1.4, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17))

#By track
Track123Q5Comb %>% 
  group_by(Q5Comb, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q5Comb) %>%
  ungroup()%>%
  filter(!Q5Comb == "Opportunities to work closely with or be mentored by Duke alumni") %>%
  filter(!Q5Comb == "Opportunities to deepen connections with existing friends") %>%
  filter(!Q5Comb == "Opportunities to work closely with the Durham community") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q5Comb, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("What Students Are Hoping to Get from Participating in Duke Experiential Programs By Track") +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided.")) +
  theme(plot.title = element_text(hjust = 1.15, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17))

geom_text(aes(label = ifelse(y > 1, y, "")), 
          position = position_stack(vjust = 0.5),
          size = 3)
#By race
Track123Q5Comb %>% 
  group_by(Q5Comb, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q5Comb) %>%
  ungroup() %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  filter(!Q5Comb == "Opportunities to work closely with or be mentored by Duke alumni") %>%
  filter(!Q5Comb == "Opportunities to deepen connections with existing friends") %>%
  filter(!Q5Comb == "Opportunities to work closely with the Durham community") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q5Comb, n)), y = n, fill = factor(biodemo_ethnic_group, levels =
                                                                      c("White",
                                                                        "Multi-racial", "Hispanic/Latino",
                                                                        "Black/African American", "Asian",
                                                                        "American Indian/Alaska Native"))))+
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("What Students Are Hoping to Get from Participating in Duke Experiential Programs By Race") +
  scale_fill_discrete(name = "Race", breaks = c("American Indian/Alaska Native", "Asian", 
                                               "Black/African American", "Hispanic/Latino", "Multi-racial",
                                               "White")) +
  theme(plot.title = element_text(hjust = 1.15, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17))


#By sex
Track123Q5Comb %>% 
  group_by(Q5Comb, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q5Comb) %>%
  ungroup() %>%
  filter(!biodemo_sex == "Not Specified") %>%
  filter(!Q5Comb == "Opportunities to work closely with or be mentored by Duke alumni") %>%
  filter(!Q5Comb == "Opportunities to deepen connections with existing friends") %>%
  filter(!Q5Comb == "Opportunities to work closely with the Durham community") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q5Comb, n)), y = n, fill = factor(biodemo_sex, levels =
                                                                      c("Male", "Female"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("What Students Are Hoping to Get from Participating in Duke Experiential Programs By Sex") +
  scale_fill_discrete(name = "Sex", breaks = c("Female", "Male")) +
  theme(plot.title = element_text(hjust = 1.15, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17))
#legend.title = element_text(size = 17),
#legend.text = element_text(size = 17))

#By financial need
Track123Q5Comb %>%
  group_by(Q5Comb, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q5Comb) %>%
  ungroup() %>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q5Comb == "Opportunities to work closely with or be mentored by Duke alumni") %>%
  filter(!Q5Comb == "Opportunities to deepen connections with existing friends") %>%
  filter(!Q5Comb == "Opportunities to work closely with the Durham community") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q5Comb, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                      c("Inst Need Highest",
                                                                        "Inst Need High", "Inst Need Mid-High",
                                                                        "Inst Need Mid", "Inst Need Mid-Low",
                                                                        "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("What Students Are Hoping to Get from Participating in Duke Experiential Programs By Financial Need Level") +
  scale_fill_discrete(name = "Level of Financial Need", breaks = c("No Need", "Inst Need Low", "Inst Need Mid-Low",
                                                 "Inst Need Mid", "Inst Need Mid-High",
                                                 "Inst Need High", "Inst Need Highest")) +
  theme(plot.title = element_text(hjust = 1, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
legend.title = element_text(size = 17),
legend.text = element_text(size = 17))
