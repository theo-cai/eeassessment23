stuath <- Track23_Combined_Qs %>%
  filter(athlete_at_matric == "1")

stuath2 <- Track123_Combined_Qs %>%
  filter(athlete_at_matric == "1")

stuath3 <- Results_CLEANED_2 %>%
  filter(athlete_at_matric == "1")

## STUDENT-ATHLETE Track Breakdown
stuath3 %>%
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
  ylab("Proportion of Student-Athlete Survey Participants") +
  xlab("") +
  geom_text(aes(label= if_else(pct > 5, paste0(sprintf("%1.1f", pct), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  ggtitle("Student-Athlete EE Survey Participants by Track") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.",
                                                 "No, and I don't plan to/I am undecided."))

## Most Important Decision-Making Factors
Q1326_1plot <- ggplot(stuath, 
                      aes(x=factor(Q1326_1, levels = 
                                     c("Not Important",
                                       "Neither Important nor Not Important",
                                       "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_1plot

Q1326_2plot <- ggplot(stuath, aes(x=factor(Q1326_2, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Length")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_2plot

Q1326_3plot <- ggplot(stuath, aes(x=factor(Q1326_3, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_4plot <- ggplot(stuath, aes(x=factor(Q1326_4, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcome")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_5plot <- ggplot(stuath, aes(x=factor(Q1326_5, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Academic Discipline, Theme, or Topic") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_6plot <- ggplot(stuath, aes(x=factor(Q1326_6, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Application Process") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_7plot <- ggplot(stuath, aes(x=factor(Q1326_7, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) + 
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Faculty") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_8plot <- ggplot(stuath, aes(x=factor(Q1326_8, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Durham Community Partners") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_9plot <- ggplot(stuath, aes(x=factor(Q1326_9, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Faculty Member")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_10plot <- ggplot(stuath, aes(x=factor(Q1326_10, levels = 
                                                           c("Not Important",
                                                             "Neither Important nor Not Important",
                                                             "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Non-Faculty Staff") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_11plot <- ggplot(stuath, aes(x=factor(Q1326_11,levels = 
                                                           c("Not Important",
                                                             "Neither Important nor Not Important",
                                                             "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Friends/Word of Mouth") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_12plot <- ggplot(stuath, aes(x=factor(Q1326_12, levels = 
                                                           c("Not Important",
                                                             "Neither Important nor Not Important",
                                                             "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_13plot <- ggplot(stuath, aes(x=factor(Q1326_13,levels = 
                                                           c("Not Important",
                                                             "Neither Important nor Not Important",
                                                             "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Friends' Plans") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_13plot

Q1326_14plot <- ggplot(stuath, aes(x=factor(Q1326_14,levels = 
                                                           c("Not Important",
                                                             "Neither Important nor Not Important",
                                                             "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Personal Health and Safety") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

#Composites
#Diversity of Topics, Program Length, Program Delivery, and Program Outcome
title_a <- textGrob("Importance of...to Student-Athlete Decision Making",
                    gp=gpar(fontsize = 20))

compQ1326_a <- plot_grid(Q1326_1plot, Q1326_2plot, Q1326_3plot, Q1326_4plot,
                         ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=20), rot = 90)

grid.arrange(arrangeGrob(compQ1326_a, left = y.grob, top = title_a))

#Academic topic, Application process, Opportunities to work with faculty, Opportunities to work with Durham
compQ1326_b <- plot_grid(Q1326_5plot, Q1326_6plot, Q1326_7plot, Q1326_8plot,
                         ncol=2, nrow=2)
grid.arrange(arrangeGrob(compQ1326_b, left = y.grob, top = title_a))

#Faculty rec, staff rec, friend rec, friends' plans
compQ1326_c <- plot_grid(Q1326_9plot, Q1326_10plot, Q1326_11plot, Q1326_13plot,
                         ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1326_c, left = y.grob, top = title_a))

#finance, personal health and safety
compQ1326_d <- plot_grid(Q1326_12plot, Q1326_14plot,
                         ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1326_d, left = y.grob, top = title_a))


#Satisfaction of STUDENT ATHLETES with aspects of the current landscape of Duke 
##opportunities.

Q1223_1plot <- ggplot(stuath, aes(x=factor(Q1223_1, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Disciplines, Themes, and Topics") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_1plot

Q1223_2plot <- ggplot(stuath, aes(x=factor(Q1223_2, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Type") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_3plot <- ggplot(stuath, aes(x=factor(Q1223_3, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Locations") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_3plot

Q1223_4plot <- ggplot(stuath, aes(x=factor(Q1223_4, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_5plot <- ggplot(stuath, aes(x=factor(Q1223_5, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcomes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_6plot <- ggplot(stuath, aes(x=factor(Q1223_6, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Lengths") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_7plot <- ggplot(stuath, aes(x=factor(Q1223_7, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_8plot <- ggplot(stuath, aes(x=factor(Q1223_8, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Health and Safety Support") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_9plot <- ggplot(stuath, aes(x=factor(Q1223_9, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Application Processes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_10plot <- ggplot(stuath, aes(x=factor(Q1223_10, levels =
                                                           c("Dissatisfied",
                                                             "Neither Satisfied nor Dissatisfied",
                                                             "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Number of Summer Opportunities") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_11plot <- ggplot(stuath, aes(x=factor(Q1223_11, levels =
                                                           c("Dissatisfied",
                                                             "Neither Satisfied nor Dissatisfied",
                                                             "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Number of Semester/Academic Year Opportunities") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_12plot <- ggplot(stuath, aes(x=factor(Q1223_12, levels =
                                                           c("Dissatisfied",
                                                             "Neither Satisfied nor Dissatisfied",
                                                             "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Discoverability of Opportunities") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_13plot <- ggplot(stuath, aes(x=factor(Q1223_13, levels =
                                                           c("Dissatisfied",
                                                             "Neither Satisfied nor Dissatisfied",
                                                             "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Level of Interaction with Faculty Program Leads") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

##Composites

#Grid A
compQ1223_a <- plot_grid(Q1223_1plot, Q1223_2plot, Q1223_3plot, Q1223_4plot,
                         ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=20), rot = 90)

title_a <- textGrob("Student-Athlete Satisfaction with the Diversity of...",
                    gp=gpar(fontsize=20))

grid.arrange(arrangeGrob(compQ1223_a, left = y.grob, top = title_a))

#Grid B
compQ1223_b <- plot_grid(Q1223_5plot, Q1223_6plot,
                         ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1223_b, left = y.grob, top = title_a))

#Grid C
compQ1223_c <- plot_grid(Q1223_10plot, Q1223_11plot, Q1223_12plot, Q1223_9plot,
                         ncol=2, nrow=2)

title_c <- textGrob("Student-Athlete Satisfaction with...", 
                    gp=gpar(fontsize=20))

grid.arrange(arrangeGrob(compQ1223_c, left = y.grob, top = title_c))

#Grid D
compQ1223_d <- plot_grid(Q1223_7plot, Q1223_8plot, Q1223_13plot,
                         ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1223_d, left = y.grob, top = title_c))


##What STUDENT-ATHLETES want out of the program
Track123Q5Comb <- stuath2 %>% 
  separate_rows(Q5Comb, sep = ";")

ggplot(subset(Track123Q5Comb, !is.na(Q5Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q5Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("What Student-Athletes Are Hoping to Get from Participating in Duke Experiential Programs") +
  theme(plot.title = element_text(hjust = 1.4, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17))


## Why STUDENT-ATHLETES do not participate in programs

Track1Q2 <- stuath2 %>% 
  separate_rows(Q2, sep = ";")

ggplot(subset(Track1Q2, !is.na(Q2))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q2)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Why Students Do Not Participate in Experiential Programs")+
  theme(plot.title = element_text(hjust = 2.6, size = 20, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17))

## STUDENT-ATHLETE Topic/discipline preference
Track123Q8Comb <- stuath2 %>%
  separate_rows(Q8Comb, sep = ";")

ggplot(subset(Track123Q8Comb, !is.na(Q8Comb)))+
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q8Comb)))) + 
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Academic Disciplines, Themes, and Topics of Interest to Student-Athletes") +
  theme(plot.title = element_text(hjust = -1.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17))

## STUDENT-ATHLETE Program Length Pref
Track123Q3Comb <- stuath2 %>%
  separate_rows(Q3Comb, sep = ";")

ggplot(subset(Track123Q3Comb, !is.na(Q3Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q3Comb)))) + #orders from most freq to least
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Student-Athlete Experiential Program Length Preference")+
  theme(plot.title = element_text(hjust = 0.3, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

## STUDENT-ATHLETE Program Timing Pref
Track123Q4Comb <- stuath2 %>%
  separate_rows(Q4Comb, sep = ";")

ggplot(subset(Track123Q4Comb, !is.na(Q4Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q4Comb)))) + #orders from most freq to least
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Student-Athlete Experiential Program Timing Preference")+
  theme(plot.title = element_text(hjust = 0.3, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

## STUDENT-ATHLETE Program Location Pref
Track123Q6Comb <- stuath2 %>%
  separate_rows(Q6Comb, sep = ";")

ggplot(subset(Track123Q6Comb, !is.na(Q6Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q6Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Student-Athlete Experiential Program Location Preference")+
  theme(plot.title = element_text(hjust = 0.1, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

## STUDENT-ATHLETE U.S. Region Pref

# Not super useful information here

Track123Q7Comb <- stuath2 %>%
  separate_rows(Q7Comb, sep = ";")

ggplot(subset(Track123Q7Comb, !is.na(Q7Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q7Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("U.S. Region Preference")+
  theme(plot.title = element_text(hjust = 0.45, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

## STUDENT-ATHLETE Virtual/Hybrid Pref
Track123Q9Comb <- stuath2 %>%
  separate_rows(Q9Comb, sep = ";")

ggplot(subset(Track123Q9Comb, !is.na(Q9Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q9Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Hybrid and Virtual Program Preference")+
  theme(plot.title = element_text(hjust = -2.25, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

## STUDENT-ATHLETE Program Outcome Pref
Track123Q10Comb <- stuath2 %>%
  separate_rows(Q10Comb, sep = ";")

ggplot(subset(Track123Q10Comb, !is.na(Q10Comb))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q10Comb)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Experiential Program Outcome Preference")+
  theme(plot.title = element_text(hjust = .25, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

##Program Delivery Pref/Ranking

# No interesting data here, they prefer in-person. Also, sample size is too small.

#Q18_1-Q18_3 for track 2
#Q31_1-Q31_3 for track 3

#Hist for each choice?
inperson <- ggplot(stuath2, aes(x = Q18_1)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..count..)) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1" = "First Choice", "2" ="Second Choice", "3" = "Third Choice")) + 
  labs(x = "Preference for Fully In-Person Programs", y = "") +
  theme(axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 17))

hybrid <- ggplot(stuath2, aes(x = Q18_2)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..count..)) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1" = "First Choice", "2" ="Second Choice", "3" = "Third Choice")) + 
  labs(x = "Preference for Fully Hybrid Programs", y = "") +
  theme(axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 17))

virtual <- ggplot(stuath2, aes(x = Q18_3)) + 
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

title_a <- textGrob("Student-Athlete Rankings of Program Delivery Types",
                    gp=gpar(fontsize=20))

grid.arrange(arrangeGrob(compQ18Comb, left = y.grob, top = title_a))
