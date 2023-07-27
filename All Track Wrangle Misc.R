library(ggplot2)
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)

##Topic/discipline preference
Track123Q8Comb <- Track123_Combined_Qs %>%
  separate_rows(Q8Comb, sep = ";")

ggplot(subset(Track123Q8Comb, !is.na(Q8Comb)))+
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q8Comb)))) + 
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Academic Disciplines, Themes, and Topics of Interest to Students") +
  theme(plot.title = element_text(hjust = -1.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17))

#By track
Track123Q8Comb %>% 
  group_by(Q8Comb, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q8Comb) %>%
  ungroup()%>%
  filter(!Q8Comb == "Other") %>%
  filter(!Q8Comb == "   Gender and sexuality") %>%
  filter(!Q8Comb == "   Fintech") %>%
  filter(!Q8Comb == "   Immigration or human migration") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q8Comb, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  theme(plot.title = element_text(hjust = 6.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Academic Disciplines, Themes, and Topics of Interest to Students By Track") +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

#By race
Track123Q8Comb %>% 
  group_by(Q8Comb, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q8Comb) %>%
  ungroup() %>%
  filter(!biodemo_ethnic_group == "Not Specified")%>%  
  filter(!Q8Comb == "Other") %>%
  filter(!Q8Comb == "   Gender and sexuality") %>%
  filter(!Q8Comb == "   Fintech") %>%
  filter(!Q8Comb == "   Immigration or human migration") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q8Comb, n)), y = n, fill = factor(biodemo_ethnic_group, levels =
                                                                      c("Not Specified", "White",
                                                                        "Multi-racial", "Hispanic/Latino",
                                                                        "Black/African American", "Asian",
                                                                        "American Indian/Alaska Native"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.14, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7) +
  theme(plot.title = element_text(hjust = 8, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Academic Disciplines, Themes, and Topics of Interest to Students By Race") +
  scale_fill_discrete(name = "Race", breaks = c("American Indian/Alaska Native", "Asian", 
                                                "Black/African American", "Hispanic/Latino", "Multi-racial",
                                                "White", "Not Specified"))

#By sex
Track123Q8Comb %>% 
  group_by(Q8Comb, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q8Comb) %>%
  ungroup() %>% 
  filter(!biodemo_sex == "Not Specified")%>%  
  filter(!Q8Comb == "Other") %>%
  filter(!Q8Comb == "   Gender and sexuality") %>%
  filter(!Q8Comb == "   Fintech") %>%
  filter(!Q8Comb == "   Immigration or human migration") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q8Comb, n)), y = n, fill = factor(biodemo_sex, levels =
                                                                      c("Not Specified", "Male", "Female"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.14, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5), size = 7.5) +
  theme(plot.title = element_text(hjust = 29, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Academic Disciplines, Themes, and Topics of Interest to Students By Sex") +
  scale_fill_discrete(name = "Sex", breaks = c("Female", "Male", "Not Specified"))

#By financial aid
Track123Q8Comb %>% 
  group_by(Q8Comb, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q8Comb) %>%
  ungroup() %>% 
  filter(!avg_need_cat_IM == "Not Specified")%>% 
  filter(!Q8Comb == "Other") %>%
  filter(!Q8Comb == "   Gender and sexuality") %>%
  filter(!Q8Comb == "   Fintech") %>%
  filter(!Q8Comb == "   Immigration or human migration") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q8Comb, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                      c("Not Specified", "Inst Need Highest",
                                                                        "Inst Need High", "Inst Need Mid-High",
                                                                        "Inst Need Mid", "Inst Need Mid-Low",
                                                                        "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.104, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5), size = 5) +
  theme(plot.title = element_text(hjust = 1.35, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Academic Disciplines, Themes, and Topics of Interest to Students By Financial Need Level") +
  scale_fill_discrete(name = "Level of Financial Need", breaks = c("No Need", "Inst Need Low", "Inst Need Mid-Low",
                                                                   "Inst Need Mid", "Inst Need Mid-High",
                                                                   "Inst Need High", "Inst Need Highest",
                                                                   "Not Specified"))

#By major 1
ggplot(subset(Track123Q8Comb, !is.na(Q8Comb))) +
  geom_bar(mapping = aes(fill = major1_descr, y = fct_rev(fct_infreq(Q8Comb))))


##Program Length Pref
Track123Q3Comb <- Track123_Combined_Qs %>%
  separate_rows(Q3Comb, sep = ";")

ggplot(subset(Track123Q3Comb, !is.na(Q3Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q3Comb)))) + #orders from most freq to least
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Experiential Program Length Preference")+
  theme(plot.title = element_text(hjust = 0.3, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

#By Track
Track123Q3Comb %>% 
  group_by(Q3Comb, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q3Comb) %>%
  ungroup()%>%
  filter(!Q3Comb == "1 week in the summer") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q3Comb, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Experiential Program Length Preference By Track") +
  theme(plot.title = element_text(hjust = 0.25, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

##Program Timing Pref
Track123Q4Comb <- Track123_Combined_Qs %>%
  separate_rows(Q4Comb, sep = ";")

ggplot(subset(Track123Q4Comb, !is.na(Q4Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q4Comb)))) + #orders from most freq to least
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Experiential Program Timing Preference")+
  theme(plot.title = element_text(hjust = 0.3, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

#By Track
Track123Q4Comb %>% 
  group_by(Q4Comb, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q4Comb) %>%
  ungroup()%>%
  filter(!Q4Comb == "Senior Spring") %>%
  filter(!Q4Comb == "Senior Fall") %>%
  filter(!Q4Comb == "I'm not sure") %>%
  filter(!Q4Comb == "Spring of First Year") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q4Comb, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Experiential Program Timing Preference By Track") +
  theme(plot.title = element_text(hjust = 0.2, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

##Program Location Pref
Track123Q6Comb <- Track123_Combined_Qs %>%
  separate_rows(Q6Comb, sep = ";")

ggplot(subset(Track123Q6Comb, !is.na(Q6Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q6Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Experiential Program Location Preference")+
  theme(plot.title = element_text(hjust = 0.1, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

#By Track
Track123Q6Comb %>% 
  group_by(Q6Comb, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q6Comb) %>%
  ungroup()%>%
  filter(!Q6Comb == "Other") %>%
  filter(!Q6Comb == "I would prefer a virtual/hybrid experience") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q6Comb, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Experiential Program Location Preference By Track") +
  theme(plot.title = element_text(hjust = 0.3, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

##U.S. Region Pref
Track123Q7Comb <- Track123_Combined_Qs %>%
  separate_rows(Q7Comb, sep = ";")

ggplot(subset(Track123Q7Comb, !is.na(Q7Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q7Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("U.S. Region Preference")+
  theme(plot.title = element_text(hjust = 0.45, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

##Virtual & Hybrid Pref
Track123Q9Comb <- Track123_Combined_Qs %>%
  separate_rows(Q9Comb, sep = ";")

ggplot(subset(Track123Q9Comb, !is.na(Q9Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q9Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Hybrid and Virtual Program Preference")+
  theme(plot.title = element_text(hjust = -2.25, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

#By Track
Track123Q9Comb %>% 
  group_by(Q9Comb, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q9Comb) %>%
  ungroup()%>%
  filter(!Q9Comb == "Virtual or hybrid service learning or volunteering") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q9Comb, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Hybrid and Virtual Program Preference By Track") +
  theme(plot.title = element_text(hjust = -160, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

##Program Outcome Pref
Track123Q10Comb <- Track123_Combined_Qs %>%
  separate_rows(Q10Comb, sep = ";")

ggplot(subset(Track123Q10Comb, !is.na(Q10Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q10Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Experiential Program Outcome Preference")+
  theme(plot.title = element_text(hjust = .25, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

#By Track
Track123Q10Comb %>% 
  group_by(Q10Comb, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q10Comb) %>%
  ungroup()%>%
  filter(!Q10Comb == "LinkedIn badge") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q10Comb, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Experiential Program Outcome Preference By Track") +
  theme(plot.title = element_text(hjust = .2, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))


##Program Delivery Pref/Ranking
#Q18_1-Q18_3 for track 2
#Q31_1-Q31_3 for track 3

#Hist for each choice?
inperson <- ggplot(Track123_Combined_Qs, aes(x = Q18_1)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..count..)) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1" = "First Choice", "2" ="Second Choice", "3" = "Third Choice")) + 
  labs(x = "Preference for Fully In-Person Programs", y = "") +
  theme(axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 17))

hybrid <- ggplot(Track123_Combined_Qs, aes(x = Q18_2)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..count..)) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1" = "First Choice", "2" ="Second Choice", "3" = "Third Choice")) + 
  labs(x = "Preference for Fully Hybrid Programs", y = "") +
  theme(axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 17))

virtual <- ggplot(Track123_Combined_Qs, aes(x = Q18_3)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..count..)) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1" = "First Choice", "2" ="Second Choice", "3" = "Third Choice")) + 
  labs(x = "Preference for Fully VirtualPrograms", y = "") +
  theme(axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 17))

#Composite
compQ18Comb <- plot_grid(inperson, hybrid, virtual,
                         ncol = 1, nrow = 3)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=20), rot = 90)

title_a <- textGrob("Student Rankings of Program Delivery Types",
                    gp=gpar(fontsize=20))

grid.arrange(arrangeGrob(compQ18Comb, left = y.grob, top = title_a))

