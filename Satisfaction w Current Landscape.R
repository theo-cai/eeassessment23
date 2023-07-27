library(ggplot2)
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)

##Satisfaction with aspects of the current landscape of Duke opportunities

Q1223_1plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_1, levels =
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

Q1223_2plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_2, levels =
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

Q1223_3plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_3, levels =
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

Q1223_4plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_4, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_5plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_5, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcomes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_6plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_6, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Lengths") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_7plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_7, levels =
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

Q1223_8plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_8, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Health and Safety Support") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_9plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_9, levels =
                                                          c("Dissatisfied",
                                                            "Neither Satisfied nor Dissatisfied",
                                                            "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Program Application Processes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_10plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_10, levels =
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

Q1223_11plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_11, levels =
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

Q1223_12plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_12, levels =
                                                           c("Dissatisfied",
                                                             "Neither Satisfied nor Dissatisfied",
                                                             "Satisfied")))) +
  geom_bar() +
  xlab("") +
  ylab("") +
  ggtitle("Discoverability of Opportunities") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15))

Q1223_13plot <- ggplot(Track23_Combined_Qs, aes(x=factor(Q1223_13, levels =
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

title_a <- textGrob("Student Satisfaction with the Diversity of...",
                    gp=gpar(fontsize=20))

grid.arrange(arrangeGrob(compQ1223_a, left = y.grob, top = title_a))

#Grid B
compQ1223_b <- plot_grid(Q1223_5plot, Q1223_6plot,
          ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1223_b, left = y.grob, top = title_a))

#Grid C
compQ1223_c <- plot_grid(Q1223_10plot, Q1223_11plot, Q1223_12plot, Q1223_9plot,
          ncol=2, nrow=2)

title_c <- textGrob("Student Satisfaction with...", 
                    gp=gpar(fontsize=20))

grid.arrange(arrangeGrob(compQ1223_c, left = y.grob, top = title_c))

#Grid D
compQ1223_d <- plot_grid(Q1223_7plot, Q1223_8plot, Q1223_13plot,
          ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1223_d, left = y.grob, top = title_c))

#By track
Q1223_1track <- Track23_Combined_Qs %>% 
  group_by(Q1223_1, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_1) %>%
  ungroup()%>%
  filter(!Q1223_1 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_1, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Disciplines, Themes and Topics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_1track

Q1223_2track <- Track23_Combined_Qs %>% 
  group_by(Q1223_2, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_2) %>%
  ungroup()%>%
  filter(!Q1223_2 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_2, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_3track <- Track23_Combined_Qs %>% 
  group_by(Q1223_3, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_3) %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_3, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))  

Q1223_4track <- Track23_Combined_Qs %>% 
  group_by(Q1223_4, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_4) %>%
  ungroup()%>%
  filter(!Q1223_4 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_4, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_5track <- Track23_Combined_Qs %>% 
  group_by(Q1223_5, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_5) %>%
  ungroup()%>%
  filter(!Q1223_5 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_5, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcomes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_6track <- Track23_Combined_Qs %>% 
  group_by(Q1223_6, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_6) %>%
  ungroup()%>%
  filter(!Q1223_6 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_6, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Lengths") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_7track <- Track23_Combined_Qs %>% 
  group_by(Q1223_7, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_7) %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_7, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_8track <- Track23_Combined_Qs %>% 
  group_by(Q1223_8, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_8) %>%
  ungroup()%>%
  filter(!Q1223_8 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_8, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Health and Safety Support") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_9track <- Track23_Combined_Qs %>% 
  group_by(Q1223_9, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_9) %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_9, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Applicaton Processes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_10track <- Track23_Combined_Qs %>% 
  group_by(Q1223_10, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_10) %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_10, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Summer Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_11track <- Track23_Combined_Qs %>% 
  group_by(Q1223_11, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_11) %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_11, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Semester/Academic Year Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_11track

Q1223_12track <- Track23_Combined_Qs %>% 
  group_by(Q1223_12, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_12) %>%
  ungroup()%>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_12, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Discoverability of Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("mediumpurple1", "tan1"))

Q1223_13track <- Track23_Combined_Qs %>% 
  group_by(Q1223_13, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_13) %>%
  ungroup()%>%
  filter(!Q1223_13 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_13, n)), y = n, fill = Q1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Level of Interaction with Faculty Program Leads") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  scale_fill_manual(name = "Track", values = c("mediumpurple1", "tan1"))

Q1223_13track

##Composites

#Grid A
compQ1223_a <- plot_grid(Q1223_1track, Q1223_2track, Q1223_3track, Q1223_4track,
                         ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

title_a <- textGrob("Student Satisfaction with the Diversity of...By Track",
                   gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_a, left = y.grob, top = title_a))

#Grid B
compQ1223_b <- plot_grid(Q1223_5track, Q1223_6track,
                         ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1223_b, left = y.grob, top = title_a))

#Grid C
compQ1223_c <- plot_grid(Q1223_10track, Q1223_11track, Q1223_12track, Q1223_9track,
                         ncol=2, nrow=)

title_c <- textGrob("Student Satisfaction with...By Track", 
                    gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_c, left = y.grob, top = title_c))

#Grid D
compQ1223_d <- plot_grid(Q1223_7track, Q1223_8track, Q1223_13track,
                         ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1223_d, left = y.grob, top = title_c))

# BY SEX
Q1223_1sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_1, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_1) %>%
  ungroup()%>%
  filter(!Q1223_1 == "Dissatisfied") %>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_1, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Disciplines, Themes and Topics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_discrete(name = "Sex")

Q1223_1sex

Q1223_2sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_2, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_2) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  filter(!Q1223_2 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_2, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_3sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_3, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_3) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_3, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_4sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_4, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_4) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  filter(!Q1223_4 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_4, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_5sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_5, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_5) %>%
  ungroup()%>%
  filter(!Q1223_5 == "Dissatisfied") %>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_5, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcomes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_6sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_6, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_6) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  filter(!Q1223_6 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_6, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Lengths") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_7sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_7, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_7) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_7, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_8sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_8, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_8) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  filter(!Q1223_8 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_8, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Health and Safety Support") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_9sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_9, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_9) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_9, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Applicaton Processes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_10sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_10, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_10) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_10, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Summer Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_11sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_11, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_11) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_11, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Semester/Academic Year Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_11sex

Q1223_12sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_12, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_12) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_12, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Discoverability of Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_13sex <- Track23_Combined_Qs %>% 
  group_by(Q1223_13, biodemo_sex) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_13) %>%
  ungroup()%>%
  filter(!biodemo_sex == "Not Specified") %>%
  filter(!Q1223_13 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_13, n)), y = n, fill = biodemo_sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Level of Interaction with Faculty Program Leads") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

##Composites

#Grid A
compQ1223_asex <- plot_grid(Q1223_1sex, Q1223_2sex, Q1223_3sex, Q1223_4sex,
                         ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

title_d <- textGrob("Student Satisfaction with the Diversity of...By Sex",
                    gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_asex, left = y.grob, top = title_d))

#Grid B
compQ1223_bsex <- plot_grid(Q1223_5sex, Q1223_6sex,
                         ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1223_bsex, left = y.grob, top = title_d))

#Grid C
compQ1223_csex <- plot_grid(Q1223_10sex, Q1223_11sex, Q1223_12sex, Q1223_9sex,
                         ncol=2, nrow=2)

title_e <- textGrob("Student Satisfaction with...By Sex", 
                    gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_csex, left = y.grob, top = title_e))

#Grid D
compQ1223_dsex <- plot_grid(Q1223_7sex, Q1223_8sex, Q1223_13sex,
                         ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1223_dsex, left = y.grob, top = title_e))

# BY RACE
Q1223_1race <- Track23_Combined_Qs %>% 
  group_by(Q1223_1, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_1) %>%
  ungroup()%>%
  filter(!Q1223_1 == "Dissatisfied") %>%
  filter(!Q1223_1 == "Neither Satisfied nor Dissatisfied") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_1, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Disciplines, Themes and Topics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_discrete(name = "Race")

Q1223_1race

Q1223_2race <- Track23_Combined_Qs %>% 
  group_by(Q1223_2, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_2) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  filter(!Q1223_2 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_2, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_3race <- Track23_Combined_Qs %>% 
  group_by(Q1223_3, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_3) %>%
  ungroup()%>%
  filter(!Q1223_3 == "Dissatisfied") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_3, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_4race <- Track23_Combined_Qs %>% 
  group_by(Q1223_4, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_4) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  filter(!Q1223_4 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_4, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_5race <- Track23_Combined_Qs %>% 
  group_by(Q1223_5, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_5) %>%
  ungroup()%>%
  filter(!Q1223_5 == "Dissatisfied") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_5, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcomes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_6race <- Track23_Combined_Qs %>% 
  group_by(Q1223_6, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_6) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  filter(!Q1223_6 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_6, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Lengths") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_7race <- Track23_Combined_Qs %>% 
  group_by(Q1223_7, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_7) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_7, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Financial Support") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_8race <- Track23_Combined_Qs %>% 
  group_by(Q1223_8, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_8) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  filter(!Q1223_8 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_8, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Health and Safety Support") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_9race <- Track23_Combined_Qs %>% 
  group_by(Q1223_9, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_9) %>%
  ungroup()%>%
  filter(!Q1223_9 == "Dissatisfied") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_9, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Applicaton Processes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_10race <- Track23_Combined_Qs %>% 
  group_by(Q1223_10, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_10) %>%
  ungroup()%>%
  filter(!Q1223_10 == "Dissatisfied") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_10, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Summer Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_11race <- Track23_Combined_Qs %>% 
  group_by(Q1223_11, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_11) %>%
  ungroup()%>%
  filter(!Q1223_11 == "Dissatisfied") %>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_11, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Semester/Academic Year Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_11race

Q1223_12race <- Track23_Combined_Qs %>% 
  group_by(Q1223_12, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_12) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_12, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Discoverability of Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_13race <- Track23_Combined_Qs %>% 
  group_by(Q1223_13, biodemo_ethnic_group) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_13) %>%
  ungroup()%>%
  filter(!biodemo_ethnic_group == "Not Specified") %>%
  filter(!Q1223_13 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_13, n)), y = n, fill = biodemo_ethnic_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Level of Interaction with Faculty Program Leads") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

##Composites

#Grid A
compQ1223_arace <- plot_grid(Q1223_1race, Q1223_2race, Q1223_3race, Q1223_4race,
                            ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

title_f <- textGrob("Student Satisfaction with the Diversity of...By Race",
                    gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_arace, left = y.grob, top = title_f))

#Grid B
compQ1223_brace <- plot_grid(Q1223_5race, Q1223_6race,
                            ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1223_brace, left = y.grob, top = title_f))

#Grid C
compQ1223_crace <- plot_grid(Q1223_10race, Q1223_11race, Q1223_12race, Q1223_9race,
                            ncol=2, nrow=2)

title_f <- textGrob("Student Satisfaction with...By Race", 
                    gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_crace, left = y.grob, top = title_f))

#Grid D
compQ1223_drace <- plot_grid(Q1223_7race, Q1223_8race, Q1223_13race,
                            ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1223_drace, left = y.grob, top = title_f))

# BY FINANCIAL NEED
Q1223_1fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_1, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_1) %>%
  ungroup()%>%
  filter(!Q1223_1 == "Dissatisfied") %>%
  filter(!Q1223_1 == "Neither Satisfied nor Dissatisfied") %>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_1, n)), y = n, fill =factor(avg_need_cat_IM, levels =
                                                                      c("Inst Need Highest",
                                                                        "Inst Need High", "Inst Need Mid-High",
                                                                        "Inst Need Mid", "Inst Need Mid-Low",
                                                                        "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Disciplines, Themes and Topics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank()) +
  scale_fill_discrete(name = "Level of Financial Need", breaks = c("Inst Need Highest",
                                                                   "Inst Need High", "Inst Need Mid-High",
                                                                   "Inst Need Mid", "Inst Need Mid-Low",
                                                                   "Inst Need Low", "No Need"))

Q1223_1fin

Q1223_2fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_2, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_2) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q1223_2 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_2, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_3fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_3, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_3) %>%
  ungroup()%>%
  filter(!Q1223_3 == "Dissatisfied") %>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_3, n)), y = n, fill =factor(avg_need_cat_IM, levels =
                                                                      c("Inst Need Highest",
                                                                        "Inst Need High", "Inst Need Mid-High",
                                                                        "Inst Need Mid", "Inst Need Mid-Low",
                                                                        "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Location") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_4fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_4, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_4) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q1223_4 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_4, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Delivery") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_5fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_5, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_5) %>%
  ungroup()%>%
  filter(!Q1223_5 == "Dissatisfied") %>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_5, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Outcomes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_6fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_6, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_6) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q1223_6 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_6, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Lengths") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_6fin

Q1223_7fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_7, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_7) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  ggplot(aes(x=factor(Q1223_7, levels =  c("Dissatisfied",
                                           "Neither Satisfied nor Dissatisfied",
                                           "Satisfied")), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity") +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("Count of Survey Responses") +
  ggtitle("Satisfaction with Financial Support") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 17),
          axis.title.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Level of Financial Need")

Q1223_7fin

Q1223_8fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_8, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_8) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q1223_8 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_8, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Health and Safety Support") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_9fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_9, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_9) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q1223_9 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_9, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Program Applicaton Processes") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_10fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_10, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_10) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q1223_10 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_10, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                        c("Inst Need Highest",
                                                                          "Inst Need High", "Inst Need Mid-High",
                                                                          "Inst Need Mid", "Inst Need Mid-Low",
                                                                          "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Summer Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_11fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_11, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_11) %>%
  ungroup()%>%
  filter(!Q1223_11 == "Dissatisfied") %>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_11, n)), y = n, fill =factor(avg_need_cat_IM, levels =
                                                                       c("Inst Need Highest",
                                                                         "Inst Need High", "Inst Need Mid-High",
                                                                         "Inst Need Mid", "Inst Need Mid-Low",
                                                                         "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Number of Semester/Academic Year Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Q1223_11fin

Q1223_12fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_12, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_12) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_12, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                        c("Inst Need Highest",
                                                                          "Inst Need High", "Inst Need Mid-High",
                                                                          "Inst Need Mid", "Inst Need Mid-Low",
                                                                          "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Discoverability of Opportunities") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

Q1223_13fin <- Track23_Combined_Qs %>% 
  group_by(Q1223_13, avg_need_cat_IM) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  drop_na(Q1223_13) %>%
  ungroup()%>%
  filter(!avg_need_cat_IM == "Not Specified") %>%
  filter(!Q1223_13 == "Dissatisfied") %>%
  ggplot(aes(x=fct_rev(fct_infreq(Q1223_13, n)), y = n, fill = factor(avg_need_cat_IM, levels =
                                                                        c("Inst Need Highest",
                                                                          "Inst Need High", "Inst Need Mid-High",
                                                                          "Inst Need Mid", "Inst Need Mid-Low",
                                                                          "Inst Need Low", "No Need"))))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Level of Interaction with Faculty Program Leads") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

##Composites

#Grid A
compQ1223_afin <- plot_grid(Q1223_1fin, Q1223_2fin, Q1223_3fin, Q1223_4fin,
                             ncol=2, nrow=2)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

title_g <- textGrob("Student Satisfaction with the Diversity of...By Level of Financial Need",
                    gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_afin, left = y.grob, top = title_g))

#Grid B
compQ1223_bfin <- plot_grid(Q1223_5fin, Q1223_6fin,
                             ncol=2, nrow=1)

grid.arrange(arrangeGrob(compQ1223_bfin, left = y.grob, top = title_g))

#Grid C
compQ1223_cfin <- plot_grid(Q1223_10fin, Q1223_11fin, Q1223_12fin, Q1223_9fin,
                             ncol=2, nrow=2)

title_h <- textGrob("Student Satisfaction with...By Level of Financial Need", 
                    gp=gpar(fontsize=15))

grid.arrange(arrangeGrob(compQ1223_cfin, left = y.grob, top = title_h))

#Grid D
compQ1223_dfin <- plot_grid(Q1223_7fin, Q1223_8fin, Q1223_13fin,
                             ncol=2, nrow=2)

grid.arrange(arrangeGrob(compQ1223_dfin, left = y.grob, top = title_h))
