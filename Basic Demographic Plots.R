library(ggplot2)
library(tidyverse)
library(forcats)

##Track
Results_CLEANED_2 %>%
  count(Q34_7_TEXT, Q1) %>%
  group_by(Q34_7_TEXT) %>%
  mutate(pct=prop.table(n)*100) %>%
  #filter(!Q1 == "Not Specified") %>%
  ggplot()+ aes(Q34_7_TEXT, pct, fill = factor(Q1, levels =
                                                 c("No, and I don't plan to/I am undecided.",
                                                   "Not yet, but I plan to.",
                                                   "Yes, I have/Yes, I am currently participating in one."))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("") +
  geom_text(aes(label= if_else(pct > 5, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  ggtitle("EE Survey Participants by Track") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                "Not yet, but I plan to.",
                                                "No, and I don't plan to/I am undecided."))


##Race
#All Tracks
Results_CLEANED_2 %>%
  count(Q34_7_TEXT, biodemo_ethnic_group) %>%
  group_by(Q34_7_TEXT) %>%
  mutate(pct=prop.table(n)*100) %>%
  #filter(!biodemo_ethnic_group == "Not Specified") %>%
ggplot()+ aes(Q34_7_TEXT, pct, fill = factor(biodemo_ethnic_group, levels =
                                              c("White",
                                                "Multi-racial", "Hispanic/Latino",
                                                "Black/African American", "Asian",
                                                "American Indian/Alaska Native"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("") +
  geom_text(aes(label= if_else(pct > 5, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  ggtitle("EE Survey Participants by Race") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Race", breaks = c("American Indian/Alaska Native", "Asian", 
                                "Black/African American", "Hispanic/Latino", "Multi-racial",
                                "White"))

#By Track
Results_CLEANED_2 %>%
  count(Q1, biodemo_ethnic_group) %>%
  group_by(Q1) %>%
  mutate(pct=prop.table(n)*100) %>%
  drop_na() %>%
  ggplot()+ aes(Q1, pct, fill=factor(biodemo_ethnic_group, levels =
                                       c("Not Specified", "White",
                                         "Multi-racial", "Hispanic/Latino",
                                         "Black/African American", "Asian",
                                         "American Indian/Alaska Native"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("Track") +
  geom_text(aes(label= if_else(pct > 5, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Racial Make Up of EE Survey Participants By Track") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Race", breaks = c("American Indian/Alaska Native", "Asian", 
                                                "Black/African American", "Hispanic/Latino", "Multi-racial",
                                                "White", "Not Specified"))

##Sex
#All Tracks
Results_CLEANED_2 %>%
  count(Q34_7_TEXT, biodemo_sex) %>%
  group_by(Q34_7_TEXT) %>%
  mutate(pct=prop.table(n)*100) %>%
  #filter(!biodemo_sex == "Not Specified") %>%
  ggplot()+ aes(Q34_7_TEXT, pct, fill = factor(biodemo_sex, levels =
                                                 c("Not Specified", "Male", "Female"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("") +
  geom_text(aes(label= if_else(pct > 0, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  ggtitle("EE Survey Participants by Sex") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Sex", breaks = c("Female", "Male", "Not Specified"))

#By Track
Results_CLEANED_2 %>%
  count(Q1, biodemo_sex) %>%
  group_by(Q1) %>%
  mutate(pct=prop.table(n)*100) %>%
  ggplot()+ aes(Q1, pct, fill=factor(biodemo_sex, levels =
                                       c("Not Specified", "Male", "Female"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("Track") +
  geom_text(aes(label= if_else(pct > 5, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Sex Make Up of EE Survey Participants By Track") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Sex", breaks = c("Female", "Male", "Not Specified"))

##Year
#All Tracks
Results_CLEANED_2 %>%
  count(Q34_7_TEXT, `Year in School`) %>%
  group_by(Q34_7_TEXT) %>%
  mutate(pct=prop.table(n)*100) %>%
  #filter(!`Year in School` == "Unknown") %>%
  ggplot()+ aes(Q34_7_TEXT, pct, fill = factor(`Year in School`, levels =
                                                 c("Not Specified", "Senior",
                                                   "Junior", "Sophomore", "First Year"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("") +
  geom_text(aes(label= if_else(pct > 0, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  ggtitle("EE Survey Participants by Year in School") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Year in School", breaks = c("First Year", "Sophomore",
                                                          "Junior", "Senior", 
                                                          "Not Specified"))

#By Track
Results_CLEANED_2 %>%
  count(Q1, `Year in School`) %>%
  group_by(Q1) %>%
  mutate(pct=prop.table(n)*100) %>%
  ggplot()+ aes(Q1, pct, fill=factor(`Year in School`, levels =
                                       c("Not Specified", "Senior",
                                         "Junior", "Sophomore", "First Year"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("Track") +
  geom_text(aes(label= if_else(pct > 5, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Year in School Make Up of EE Survey Participants By Track") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Year in School", breaks = c("First Year", "Sophomore",
                                                          "Junior", "Senior", 
                                                          "Not Specified"))

##Financial Status
#All Tracks
Results_CLEANED_2 %>%
  count(Q34_7_TEXT, avg_need_cat_IM) %>%
  group_by(Q34_7_TEXT) %>%
  mutate(pct=prop.table(n)*100) %>%
  #filter(!avg_need_cat_IM == "Not Specified") %>%
  ggplot()+ aes(Q34_7_TEXT, pct, fill = factor(avg_need_cat_IM, levels =
                                                 c("Not Specified", "Inst Need Highest",
                                                   "Inst Need High", "Inst Need Mid-High",
                                                   "Inst Need Mid", "Inst Need Mid-Low",
                                                   "Inst Need Low", "No Need"))) + # Need to maybe rename these categories
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("") +
  geom_text(aes(label= if_else(pct > 0, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  ggtitle("EE Survey Participants by Level of Financial Need") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Level of Financial Need", breaks = c("No Need", "Inst Need Low", "Inst Need Mid-Low",
                                                                   "Inst Need Mid", "Inst Need Mid-High",
                                                                   "Inst Need High", "Inst Need Highest",
                                                                   "Not Specified"))
#By Track
Results_CLEANED_2 %>%
  count(Q1, avg_need_cat_IM) %>%
  group_by(Q1) %>%
  mutate(pct=prop.table(n)*100) %>%
  ggplot()+ aes(Q1, pct, fill=factor(avg_need_cat_IM, levels =
                                       c("Not Specified", "Inst Need Highest",
                                         "Inst Need High", "Inst Need Mid-High",
                                         "Inst Need Mid", "Inst Need Mid-Low",
                                         "Inst Need Low", "No Need"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Proportion of Survey Participants") +
  xlab("Track") +
  geom_text(aes(label= if_else(pct > 5, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Financial Need Make Up of EE Survey Participants By Track") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Level of Financial Need", breaks = c("No Need", "Inst Need Low", "Inst Need Mid-Low",
                                                          "Inst Need Mid", "Inst Need Mid-High",
                                                          "Inst Need High", "Inst Need Highest",
                                                          "Not Specified"))

##Athlete Status


##Majors
#All Tracks
MajorsComb <- pivot_longer(Track123_Combined_Qs, major1_descr:major2_descr, names_to = "MajorPosition", values_to = "Major")

ggplot(subset(MajorsComb, !is.na(Major))) +
  geom_bar(mapping = aes(x=fct_rev(fct_infreq(Major))), stat="count") +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("EE Survey Participants by Declared Majors") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text= element_text(size = 14),
        axis.title.x = element_text(size = 17))
  


#By Track


##First Gen?
