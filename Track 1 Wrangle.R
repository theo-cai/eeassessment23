
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

##Reasons Track 1 students do not participate in experiential 
Track1Q2 <- Track123_Combined_Qs %>% 
  separate_rows(Q2, sep = ";")

ggplot(subset(Track1Q2, !is.na(Q2))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q2)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Why Students Do Not Participate in Experiential Programs")+
  theme(plot.title = element_text(hjust = 2.6, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17))


#Need to reduce the length of these options


## Reasons by Race
Track1Q2 %>% 
  group_by(Q2, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q2) %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  filter(!Q2 == "Other") %>%
  filter(!Q2 == "My family is not supportive") %>%
  filter(!Q2 == "I worry about my health and/or safety during programs that have a travel component") %>%
  filter(!Q2 == "I need or prefer virtual and/or hybrid options") %>%
  filter(!Q2 == "My friends are not planning to participate in the same opportunities") %>%
  filter(!Q2 == "I can't find programs that travel to locations that interest me") %>%
  filter(!Q2 == "Experiential programs at Duke do not offer programming and/or trainings that interest me") %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q2, n)), y = n, fill = factor(biodemo_ethnic_group, levels =
                                                                      c("Not Specified", "White",
                                                                        "Multi-racial", "Hispanic/Latino",
                                                                        "Black/African American", "Asian",
                                                                        "American Indian/Alaska Native"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.125, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Why Students Do Not Participate in Experiential Programs By Race") +
  scale_fill_discrete(name = "Race", breaks = c("American Indian/Alaska Native", "Asian", 
                                                "Black/African American", "Hispanic/Latino", "Multi-racial",
                                                "White", "Not Specified")) +
  theme(plot.title = element_text(hjust = 1.85, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17))

## Reasons by Sex
Track1Q2 %>% 
  group_by(Q2, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q2) %>%
  filter(!biodemo_sex == "Not Specified") %>%
  filter(!Q2 == "Other") %>%
  filter(!Q2 == "My family is not supportive") %>%
  filter(!Q2 == "I worry about my health and/or safety during programs that have a travel component") %>%
  filter(!Q2 == "I need or prefer virtual and/or hybrid options") %>%
  filter(!Q2 == "My friends are not planning to participate in the same opportunities") %>%
  filter(!Q2 == "I can't find programs that travel to locations that interest me") %>%
  filter(!Q2 == "Experiential programs at Duke do not offer programming and/or trainings that interest me") %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q2, n)), y = n, fill = factor(biodemo_sex, levels =
                                                                  c("Not Specified", "Male", "Female")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.125, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Why Students Do Not Participate in Experiential Programs By Sex") +
  scale_fill_discrete(name = "Sex", breaks = c("Female", "Male", "Not Specified")) +
  theme(plot.title = element_text(hjust = 1.9, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17))


## Reasons By Fin Need
Track1Q2 %>% 
  group_by(Q2, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q2) %>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q2 == "Other") %>%
  filter(!Q2 == "My family is not supportive") %>%
  filter(!Q2 == "I worry about my health and/or safety during programs that have a travel component") %>%
  filter(!Q2 == "I need or prefer virtual and/or hybrid options") %>%
  filter(!Q2 == "My friends are not planning to participate in the same opportunities") %>%
  filter(!Q2 == "I can't find programs that travel to locations that interest me") %>%
  filter(!Q2 == "Experiential programs at Duke do not offer programming and/or trainings that interest me") %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q2, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                  c("Not Specified", "Inst Need Highest",
                                                                    "Inst Need High", "Inst Need Mid-High",
                                                                    "Inst Need Mid", "Inst Need Mid-Low",
                                                                    "Inst Need Low", "No Need")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.125, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size=5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Why Students Do Not Participate in Experiential Programs By Financial Need") +
  scale_fill_discrete(name = "Level of Financial Need", breaks = c("No Need", "Inst Need Low", "Inst Need Mid-Low",
                                                "Inst Need Mid", "Inst Need Mid-High",
                                                "Inst Need High", "Inst Need Highest",
                                                "Not Specified")) +
  theme(plot.title = element_text(hjust = 1.5, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17))

