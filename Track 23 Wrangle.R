library(ggplot2)
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)

#sinstall.packages("scales")
library(scales)

##Track 3 Exclusives
#Q24 - Participation in various programs
Track23Q24 <- Track23 %>%
  separate_rows(Q24, sep=";")

ggplot(subset(Track23Q24, !is.na(Q24))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q24)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Student Participation in Duke Experiential Programs") +
  theme(plot.title = element_text(hjust = -0.6, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))


#Q25 Would students recommend program
ggplot(subset(Track23Q24, !is.na(Q25))) +
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(Q25)))) +
  xlab("Count of Survey Responses") +
  ylab("") +
  ggtitle("Would Students Recommend Their Duke Experiential Program(s) to Their Friends?") +
  theme(plot.title = element_text(hjust = 1.6, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

#Q25 would students recommend program by program
Track23Q24 %>% # Really need to space out the percentages here
  group_by(Q25, Q24) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q25) %>%
  ungroup() %>% 
  filter(!Q25 == "No; not to any of them") %>%
  filter(!Q25 == "Yes; but only to a select few") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q25, n)), y = n, fill = factor(Q24, levels =
                                                                   c("Other Duke-sponsored experiential program(s)",
                                                                     "Global Education Office",
                                                                     "DukeEngage", "Duke Immerse",
                                                                     "Data+/Story+/Code+/Climate+/Ethics+",
                                                                     "Bass Connections"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.05, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5), size = 5.5) +
  coord_flip() +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Would Students Recommend their Duke Experiential Programs(s) (By Program)?")+
  scale_fill_discrete(name = "Program", breaks = c("Bass Connections", "Data+/Story+/Code+/Climate+/Ethics+",
                                                   "Duke Immerse", "DukeEngage",
                                                   "Global Education Office",
                                                   "Other Duke-sponsored experiential program(s)")) +
  theme(plot.title = element_text(hjust = 1.7, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size  = 17))

Q25plotprog <- ggplot(subset(Track23Q24, !is.na(Q25)), aes(fill=Q24, x=Q25)) +
  geom_bar(position="stack")
Q25plotprog + coord_flip()

##Most important decision-making factors
Q1326_1plot <- ggplot(Track23_Combined_Qs, 
                      aes(x=factor(Q1326_1, levels = 
                                     c("Not Important",
                                       "Neither Important nor Not Important",
                                       "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  
Q1326_1plot

Q1326_2plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_2, levels = 
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

Q1326_3plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_3, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_4plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_4, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcome")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_5plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_5, levels = 
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

Q1326_6plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_6, levels = 
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

Q1326_7plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_7, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) + 
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Faculty") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_8plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_8, levels = 
                                                          c("Not Important",
                                                            "Neither Important nor Not Important",
                                                            "Important")))) +
  geom_bar() +
    xlab("") +
    ylab("") +
    ggtitle("Opportunities to Work Closely with Durham Community Partners") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_9plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_9, levels = 
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

Q1326_10plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_10, levels = 
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

Q1326_11plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_11,levels = 
                                                           c("Not Important",
                                                             "Neither Important nor Not Important",
                                                             "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Friends/Word of Mouth") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_12plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_12, levels = 
                                                           c("Not Important",
                                                             "Neither Important nor Not Important",
                                                             "Important")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5))

Q1326_13plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_13,levels = 
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

Q1326_14plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1326_14,levels = 
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
title_a <- textGrob("Importance of...to Student Decision Making",
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

## [BY TRACK] Most important decision making factors
Q1326_1plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_1, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  filter(!Q1326_1 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_1, levels = 
                                     c("Not Important",
                                       "Neither Important nor Not Important",
                                        "Important")),
                          y = n,
                          fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_1plottrack

Q1326_2plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_2, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  filter(!Q1326_2 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_2, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Length")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_2plottrack

Q1326_3plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_3, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  filter(!Q1326_3 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_3, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_3plottrack

Q1326_4plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_4, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  filter(!Q1326_4 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_4, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcome")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_5plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_5, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  filter(!Q1326_5 == "Not Important") %>%
  filter(!Q1326_5 == "Neither Important nor Not Important") %>%
  ggplot(aes(x=factor(Q1326_5, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Academic Discipline, Theme, or Topic")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_6plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_6, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_6, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Application Process")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_7plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_7, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_7, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Faculty")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_7plottrack

Q1326_8plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_8, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_8, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Durham Community Partners")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_9plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_9, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_9, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Faculty Member")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_10plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_10, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_10, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Non-Faculty Staff")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_11plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_11, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_11, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Friends/Word of Mouth")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_12plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_12, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_12, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_13plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_13, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_13, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Friends' Plans")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c("darkseagreen3", "tomato"))

Q1326_13plottrack

Q1326_14plottrack <- Track23_Combined_Qs%>% 
  group_by(Q1326_14, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_14, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = Q1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100), "%")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Personal Health and Safety")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(name = "Track", values = c("darkseagreen3", "tomato"))

Q1326_14plottrack

## [BY TRACK] Composites
#Diversity of Topics, Program Length, Program Delivery, and Program Outcome
title_b <- textGrob("Importance of...to Student Decision Making By Track", 
                    gp=gpar(fontsize=15))

compQ1326track_a <- plot_grid(Q1326_1plottrack, Q1326_2plottrack, Q1326_3plottrack, Q1326_4plottrack,
                         ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

grid.arrange(arrangeGrob(compQ1326track_a, left = y.grob, top = title_b)) 

#Academic topic, Application process, Opportunities to work with faculty, Opportunities to work with Durham
compQ1326track_b <- plot_grid(Q1326_5plottrack, Q1326_6plottrack, Q1326_7plottrack, Q1326_8plottrack,
                         ncol=2, nrow=2)
grid.arrange(arrangeGrob(compQ1326track_b, left = y.grob, top = title_b))

#Faculty rec, staff rec, friend rec, friends' plans
compQ1326track_c <- plot_grid(Q1326_9plottrack, Q1326_10plottrack, Q1326_11plottrack, Q1326_13plottrack,
                         ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1326track_c, left = y.grob, top = title_b))

#finance, personal health and safety
compQ1326track_d <- plot_grid(Q1326_12plottrack, Q1326_14plottrack,
                         ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1326track_d, left = y.grob, top = title_b))


# [BY RACE] Decision-Making Factors
Q1326_1plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_1, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_1 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_1, levels = 
                        c("Not Important",
                          "Neither Important nor Not Important",
                          "Important")),
             y = n,
             fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual (name = "Race",
                      values = c("#C49A00","#53B400","#00C094", "#00B6EB",
                                "#A58AFF", "#FB61D7"))

Q1326_1plotrace

#Legend colors??
#show_col(hue_pal()(7))

Q1326_2plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_2, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_2 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_2, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.106, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Length")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_2plotrace

Q1326_3plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_3, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_3 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_3, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_3plotrace

Q1326_4plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_4, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_4 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_4, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcome")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_4plotrace

Q1326_5plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_5, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_5 == "Not Important") %>%
  filter(!Q1326_5 == "Neither Important nor Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_5, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Academic Discipline, Theme, or Topic")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_5plotrace

Q1326_6plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_6, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_6, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("") +
  ggtitle("Application Process")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5),
        axis.ticks.x = element_blank())

Q1326_6plotrace

Q1326_7plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_7, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_7 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_7, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Faculty")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_8plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_8, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_8, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Durham Community Partners")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_9plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_9, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_9, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Faculty Member")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_10plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_10, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_10, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Non-Faculty Staff")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_11plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_11, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_11 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_11, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Friends/Word of Mouth")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_manual (name = "Race",
                     values = c("#C49A00","#53B400","#00C094", "#00B6EB",
                                "#A58AFF", "#FB61D7"))

Q1326_11plotrace

Q1326_12plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_12, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!Q1326_12 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_12, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5),
        axis.ticks.x = element_blank())

Q1326_13plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_13, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_13, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Friends' Plans")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_13plotrace

Q1326_14plotrace <- Track23_Combined_Qs%>% 
  group_by(Q1326_14, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_ethnic_group) %>%
  ungroup()%>% 
  filter(!Q1326_14 == "Not Important") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=factor(Q1326_14, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.19, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("") +
  ggtitle("Personal Health and Safety")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 13.5),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Race")

Q1326_14plotrace

## [BY RACE] Composites
#Diversity of Topics, Program Length, Program Delivery, and Program Outcome
title_c <- textGrob("Importance of...to Student Decision Making By Race",
                    gp=gpar(fontsize=20, face = "bold"))

compQ1326race_a <- plot_grid(Q1326_1plotrace, Q1326_2plotrace, Q1326_3plotrace, Q1326_4plotrace,
                              ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=20), rot = 90)

grid.arrange(arrangeGrob(compQ1326race_a, left = y.grob, top = title_c)) #Removed first bars for all

#Academic topic, Application process, Opportunities to work with faculty, Opportunities to work with Durham
compQ1326race_b <- plot_grid(Q1326_5plotrace, Q1326_6plotrace, Q1326_7plotrace, Q1326_8plotrace,
                              ncol=2, nrow=2)
grid.arrange(arrangeGrob(compQ1326race_b, left = y.grob, top = title_c)) #Remove first and second bars for Q1326_5plotrace?

#Faculty rec, staff rec, friend rec, friends' plans
compQ1326race_c <- plot_grid(Q1326_9plotrace, Q1326_10plotrace, Q1326_11plotrace, Q1326_13plotrace,
                              ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1326race_c, left = y.grob, top = title_c)) 

#finance, personal health and safety
compQ1326race_d <- plot_grid(Q1326_12plotrace, Q1326_14plotrace,
                              ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1326race_d, left = y.grob, top = title_c)) #Remove first bars for both?


#Spacial report composite
compreport <- plot_grid(Q1326_4plotrace, Q1326_12plotrace,
                        Q1326_14plotrace, Q1326_6plotrace,
                        ncol=2, nrow=2)

grid.arrange(arrangeGrob(compreport, left = y.grob, top = title_c))


# [BY SEX] Decision-Making Factors
Q1326_1plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_1, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  filter(!Q1326_1 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_1, levels = 
                        c("Not Important",
                          "Neither Important nor Not Important",
                          "Important")),
             y = n,
             fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_1plotsex

Q1326_2plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_2, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  filter(!Q1326_2 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_2, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.106, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Length")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_2plotsex

Q1326_3plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_3, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  filter(!Q1326_3 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_3, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_3plotsex

Q1326_4plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_4, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  filter(!Q1326_4 == "Not Important") %>%
  ggplot(aes(x=factor(Q1326_4, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcome")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_5plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_5, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  filter(!Q1326_5 == "Not Important") %>%
  filter(!Q1326_5 == "Neither Important nor Not Important") %>%
  ggplot(aes(x=factor(Q1326_5, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Academic Discipline, Theme, or Topic")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_5plotsex

Q1326_6plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_6, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_6, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Application Process")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_7plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_7, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_7, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Faculty")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_8plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_8, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_8, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Durham Community Partners")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_9plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_9, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_9, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Faculty Member")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_10plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_10, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_10, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Non-Faculty Staff")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_11plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_11, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_11, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Friends/Word of Mouth")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_12plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_12, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_12, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_13plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_13, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_13, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Friends' Plans")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_13plotsex

Q1326_14plotsex <- Track23_Combined_Qs%>% 
  group_by(Q1326_14, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(biodemo_sex) %>%
  ungroup()%>% 
  ggplot(aes(x=factor(Q1326_14, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Personal Health and Safety")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_discrete(name = "Sex")

Q1326_14plotsex

## [BY SEX] Composites
#Diversity of Topics, Program Length, Program Delivery, and Program Outcome
title_d <- textGrob("Importance of...to Student Decision Making By Sex",
                    gp = gpar(fontsize=15))

compQ1326sex_a <- plot_grid(Q1326_1plotsex, Q1326_2plotsex, Q1326_3plotsex, Q1326_4plotsex,
                             ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

grid.arrange(arrangeGrob(compQ1326sex_a, left = y.grob, top = title_d))

#Academic topic, Application process, Opportunities to work with faculty, Opportunities to work with Durham
compQ1326sex_b <- plot_grid(Q1326_5plotsex, Q1326_6plotsex, Q1326_7plotsex, Q1326_8plotsex,
                             ncol=2, nrow=2)
grid.arrange(arrangeGrob(compQ1326sex_b, left = y.grob, top = title_d)) #

#Faculty rec, staff rec, friend rec, friends' plans
compQ1326sex_c <- plot_grid(Q1326_9plotsex, Q1326_10plotsex, Q1326_11plotsex, Q1326_13plotsex,
                             ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1326sex_c, left = y.grob, top = title_d)) 

#finance, personal health and safety
compQ1326sex_d <- plot_grid(Q1326_12plotsex, Q1326_14plotsex,
                             ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1326sex_d, left = y.grob, top = title_d)) 


# [BY FINANCIAL STATUS] Decision-Making Factors
Q1326_1plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_1, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  filter(!Q1326_1 == "Not Important") %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_1, levels = 
                        c("Not Important",
                          "Neither Important nor Not Important",
                          "Important")),
             y = n,
             fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_1plotfin

Q1326_2plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_2, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  filter(!Q1326_2 == "Not Important") %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_2, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.106, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Length")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_2plotfin

Q1326_3plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_3, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  filter(!Q1326_3 == "Not Important") %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_3, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_3plotfin

Q1326_4plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_4, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  filter(!Q1326_4 == "Not Important") %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_4, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcome")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_4plotfin

Q1326_5plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_5, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  filter(!Q1326_5 == "Not Important") %>%
  filter(!Q1326_5 == "Neither Important nor Not Important") %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_5, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Academic Discipline, Theme, or Topic")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

#scale_fill_manual(values = c())
show_col(hue_pal()(7))

Q1326_5plotfin

Q1326_6plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_6, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_6, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Application Process")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_6plotfin

Q1326_7plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_7, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_7, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Faculty")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_7plotfin

Q1326_8plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_8, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_8, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Opportunities to Work Closely with Durham Community Partners")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_8plotfin

Q1326_9plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_9, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_9, levels =  c("Not Important",
                                           "Neither Important nor Not Important",
                                           "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Faculty Member")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_9plotfin

Q1326_10plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_10, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_10, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Non-Faculty Staff")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1326_10plotfin

Q1326_11plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_11, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_11, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Recommendation from Friends/Word of Mouth")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_11plotfin

Q1326_12plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_12, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_12, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_12plotfin

Q1326_13plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_13, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>%
  ggplot(aes(x=factor(Q1326_13, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Friends' Plans")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1326_13plotfin

Q1326_14plotfin <- Track23_Combined_Qs%>% 
  group_by(Q1326_14, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(avg_need_cat_IM) %>%
  ungroup()%>% 
  ggplot(aes(x=factor(Q1326_14, levels =  c("Not Important",
                                            "Neither Important nor Not Important",
                                            "Important")),
             y = n, fill = avg_need_cat_IM)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")), 
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Personal Health and Safety")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_discrete(name = "Level of Financial Need")

Q1326_14plotfin

## [BY FINANCIAL NEED] Composites
#Diversity of Topics, Program Length, Program Delivery, and Program Outcome
title_e <- textGrob("Importance of...to Student Decision Making By Level of Financial Need",
                    gp=gpar(fontsize=15))

compQ1326fin_a <- plot_grid(Q1326_1plotfin, Q1326_2plotfin, Q1326_3plotfin, Q1326_4plotfin,
                            ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

grid.arrange(arrangeGrob(compQ1326fin_a, left = y.grob, top = title_e))#Remove first and potentially second bars for all except Q1326_4plotfin

#Academic topic, Application process, Opportunities to work with faculty, Opportunities to work with Durham
compQ1326fin_b <- plot_grid(Q1326_5plotfin, Q1326_6plotfin, Q1326_7plotfin, Q1326_8plotfin,
                            ncol=2, nrow=2)
grid.arrange(arrangeGrob(compQ1326fin_b, left = y.grob, top = title_e)) #Remove first and second bars of Q1326_5

#Faculty rec, staff rec, friend rec, friends' plans
compQ1326fin_c <- plot_grid(Q1326_9plotfin, Q1326_10plotfin, Q1326_11plotfin, Q1326_13plotfin,
                            ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1326fin_c, left = y.grob, top = title_e))#Remove first bar of Q1326_11plotfin

#finance, personal health and safety
compQ1326fin_d <- plot_grid(Q1326_12plotfin, Q1326_14plotfin,
                            ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1326fin_d, left = y.grob, top = title_e)) 

