library(ggplot2)
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)

## Q35_1-Q35_6
# GEO
Q35_1plot <- ggplot(Results_CLEANED_3,
       aes(x=factor(Q35_1, levels = 
                      c("I know this office/program well",
                        "I have a fair understanding of this office/program",
                        "I have heard of this office/program, but I don't know many details",
                        "I have never heard of this office/program")))) +
  geom_bar() +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with the Global Education Office") +
  theme(plot.title = element_text(hjust = -7, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17))

Q35_1plot
  

# Duke Immerse
Q35_2plot <- ggplot(Results_CLEANED_3,
       aes(x=factor(Q35_2, levels = 
                      c("I know this office/program well",
                        "I have a fair understanding of this office/program",
                        "I have heard of this office/program, but I don't know many details",
                        "I have never heard of this office/program")))) +
  geom_bar() +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with Duke Immerse") +
  theme(plot.title = element_text(hjust = -1, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17))
        #axis.ticks.y = element_blank())
Q35_2plot

# DukeEngage
Q35_3plot <- ggplot(Results_CLEANED_3,
       aes(x=factor(Q35_3, levels = 
                      c("I know this office/program well",
                        "I have a fair understanding of this office/program",
                        "I have heard of this office/program, but I don't know many details",
                        "I have never heard of this office/program")))) +
  geom_bar() +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with DukeEngage") +
  theme(plot.title = element_text(hjust = -0.75, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17))



Q35_3plot

# Bass Connections
Q35_4plot <- ggplot(Results_CLEANED_3,
       aes(x=factor(Q35_4, levels = 
                      c("I know this office/program well",
                        "I have a fair understanding of this office/program",
                        "I have heard of this office/program, but I don't know many details",
                        "I have never heard of this office/program")))) +
  geom_bar() +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with Bass Connections") +
  theme(plot.title = element_text(hjust = -1.25, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17))
        #axis.ticks.y = element_blank())

Q35_4plot

# Climate+/Code+/Data+/Ethics+/Story+
Q35_5plot <- ggplot(Results_CLEANED_3,
       aes(x=factor(Q35_5, levels = 
                      c("I know this office/program well",
                        "I have a fair understanding of this office/program",
                        "I have heard of this office/program, but I don't know many details",
                        "I have never heard of this office/program")))) +
  geom_bar() +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with Plus Programs") +
  theme(plot.title = element_text(hjust = -0.75, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17))

Q35_5plot

# Duke Summer Experiences
Q35_6plot <- ggplot(Results_CLEANED_3,
       aes(x=factor(Q35_6, levels = 
                      c("I know this office/program well",
                        "I have a fair understanding of this office/program",
                        "I have heard of this office/program, but I don't know many details",
                        "I have never heard of this office/program")))) +
  geom_bar() +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with Duke Summer Experiences") +
  theme(plot.title = element_text(hjust = -6, size = 20, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.text.y = element_text(size = 17))
        #axis.ticks.y = element_blank())

Q35_6plot

#Composites
title <- ggdraw() + draw_label("Student Familiarity of Various Duke Experiential Programs and Offices")


compQ35 <- plot_grid(Q35_1plot, Q35_2plot, Q35_3plot, Q35_4plot, Q35_5plot, Q35_6plot,
                         ncol=2, nrow=3)

y.grob <- textGrob("Count of Survey Responses",
                   gp=gpar(fontsize=15), rot = 90)

grid.arrange(arrangeGrob(compQ35, top = title))


#By Track
# GEO
Q35_1plot <- Results_CLEANED_3 %>%
  group_by(Q35_1, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=factor(Q35_1, levels = 
                                   c("I know this office/program well",
                                     "I have a fair understanding of this office/program",
                                     "I have heard of this office/program, but I don't know many details",
                                     "I have never heard of this office/program")),
             y = n, fill = factor(Q1, levels =
                                    c("No, and I don't plan to/I am undecided.",
                                      "Not yet, but I plan to.",
                                      "Yes, I have/Yes, I am currently participating in one.")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.143, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with the Global Education Office By Track") +
  theme(plot.title = element_text(hjust = 3.7, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))
Q35_1plot

# Duke Immerse
Q35_2plot <- Results_CLEANED_3 %>%
  group_by(Q35_2, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  filter(!Q35_2 == "I know this office/program well") %>%
  ungroup() %>%
  ggplot(aes(x=factor(Q35_2, levels = 
                        c("I know this office/program well",
                          "I have a fair understanding of this office/program",
                          "I have heard of this office/program, but I don't know many details",
                          "I have never heard of this office/program")),
             y = n, fill = factor(Q1, levels =
                                    c("No, and I don't plan to/I am undecided.",
                                      "Not yet, but I plan to.",
                                      "Yes, I have/Yes, I am currently participating in one.")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.15, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 6) +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with Duke Immerse By Track") +
  theme(plot.title = element_text(hjust = -3, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))
Q35_2plot

# DukeEngage
Q35_3plot <- Results_CLEANED_3 %>%
  group_by(Q35_3, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  filter(!Q35_3 == "I have never heard of this office/program") %>%
  ungroup() %>%
  ggplot(aes(x=factor(Q35_3, levels = 
                        c("I know this office/program well",
                          "I have a fair understanding of this office/program",
                          "I have heard of this office/program, but I don't know many details",
                          "I have never heard of this office/program")),
             y = n, fill = factor(Q1, levels =
                                    c("No, and I don't plan to/I am undecided.",
                                      "Not yet, but I plan to.",
                                      "Yes, I have/Yes, I am currently participating in one.")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with DukeEngage By Track") +
  theme(plot.title = element_text(hjust = -2.5, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))
Q35_3plot

# Bass Connections
Q35_4plot <- Results_CLEANED_3 %>%
  group_by(Q35_4, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  filter(!Q35_4 == "I have never heard of this office/program") %>%
  ungroup() %>%
  ggplot(aes(x=factor(Q35_4, levels = 
                        c("I know this office/program well",
                          "I have a fair understanding of this office/program",
                          "I have heard of this office/program, but I don't know many details",
                          "I have never heard of this office/program")),
             y = n, fill = factor(Q1, levels =
                                    c("No, and I don't plan to/I am undecided.",
                                      "Not yet, but I plan to.",
                                      "Yes, I have/Yes, I am currently participating in one.")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.1, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with Bass Connections By Track") +
  theme(plot.title = element_text(hjust = -8.75, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

Q35_4plot

# Climate+/Code+/Data+/Ethics+/Story+
Q35_5plot <- Results_CLEANED_3 %>%
  group_by(Q35_5, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  #filter(!Q35_5 == "I have never heard of this office/program") %>%
  ungroup() %>%
  ggplot(aes(x=factor(Q35_5, levels = 
                        c("I know this office/program well",
                          "I have a fair understanding of this office/program",
                          "I have heard of this office/program, but I don't know many details",
                          "I have never heard of this office/program")),
             y = n, fill = factor(Q1, levels =
                                    c("No, and I don't plan to/I am undecided.",
                                      "Not yet, but I plan to.",
                                      "Yes, I have/Yes, I am currently participating in one.")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.15, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 5.5) +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with the Plus Programs By Track") +
  theme(plot.title = element_text(hjust = -9, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

Q35_5plot

# Duke Summer Experiences
Q35_6plot <- Results_CLEANED_3 %>%
  group_by(Q35_6, Q1) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  filter(!Q35_6 == "I know this office/program well") %>%
  ungroup() %>%
  ggplot(aes(x=factor(Q35_6, levels =
                        c("I know this office/program well",
                          "I have a fair understanding of this office/program",
                          "I have heard of this office/program, but I don't know many details",
                          "I have never heard of this office/program")),
             y = n, fill = factor(Q1, levels =
                                    c("No, and I don't plan to/I am undecided.",
                                      "Not yet, but I plan to.",
                                      "Yes, I have/Yes, I am currently participating in one.")))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label= if_else(percent > 0.15, paste0(sprintf("%1.1f", percent*100), "%"), "")),
            position=position_stack(vjust=0.5), size = 7.5) +
  xlab("") +
  ylab("Count of Survey Responses") +
  coord_flip() +
  ggtitle("Student Familiarity with Duke Summer Experiences By Track") +
  theme(plot.title = element_text(hjust = 4, size = 20, face = "bold"),
        axis.title.x = element_text(size= 17),
        axis.text.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_fill_discrete(name = "Track", breaks = c("Yes, I have/Yes, I am currently participating in one.",
                                                 "Not yet, but I plan to.", 
                                                 "No, and I don't plan to/I am undecided."))

Q35_6plot
