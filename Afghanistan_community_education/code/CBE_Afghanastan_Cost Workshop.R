############      Afghanistan Community-based Education (CBE) Cost Analysis ##########
############                   Results for the 2020 Cost Workshop           ##########
############                           Yilin Pan                            ##########


# Preparation
rm (list = ls())
setwd("C:/Users/WB508861/OneDrive - WBG/5_Afghanistan/Class_level_dataset")
suppressMessages(stopifnot(require(dplyr)))
suppressMessages(stopifnot(require(tidyr)))
suppressMessages(stopifnot(require(ggplot2)))
suppressMessages(stopifnot(require(ggcorrplot)))
suppressMessages(stopifnot(require(ggrepel)))
suppressMessages(stopifnot(require(ggthemes)))
suppressMessages(stopifnot(require(ggalt)))
suppressMessages(stopifnot(require(gridExtra)))
suppressMessages(stopifnot(require(ggpubr)))

# Waffle graph: Number of Out-of-school Children in Afghanistan #

parts <- c(`In School`=(3.7/0.437-3.7)*10, 
           `Out of school: Girls`=(3.7*0.6)*10, 
           `Out of school: Boys`= 3.7*0.4*10)

waffle <- waffle::waffle(parts, rows = 10, size = 1, 
                         colors = c("#009bda", "#6e6e6e", "#bababa"), 
       xlab = "One square == 100,000 children aged 7-17 years old")

ggsave(
  "waffle.png",
  waffle,
  width = 6.5,
  height = 3,
  units = "in", 
  dpi = 1200
)

# Dumbbell graph: Self-reported cost vs. reestimated cost

data <- read.csv("summary.csv", na.strings = c(""), stringsAsFactors = FALSE)

data$agency <- factor(data$agency, levels = c("MoE", "UN", "INGO1", "INGO2", "INGO3", 
                                              "LNGO1", "LNGO2"))
data$group <- factor(data$group, levels = c("MoE", "UN", "INGO", "LNGO"))

dumbbell <- data
dumbbell <- dumbbell[order(dumbbell$average_cost_self.reported), ]
dumbbell$agency <- factor(dumbbell$agency, levels = c("LNGO2", "LNGO1", "UN", "INGO1", "MoE", "INGO3", "INGO2"))
self_reported <- ggplot(dumbbell) + 
  geom_dumbbell(aes(x = average_cost_self.reported, 
                    xend = average_cost, y = agency, group = group),
    color="gray60", size = 1.5,  
    colour_x = "darkorange1", colour_xend = "cyan4", 
    size_x	= 3, size_xend = 3) + 
  scale_x_continuous() + 
  geom_text(data = data[1, ], 
    aes(x = average_cost_self.reported, y = agency),
            label = "Self-reported", fontface = "bold",
            color = "darkorange1",
            hjust = 0.2,
            vjust = -1) +
  geom_text(data = data[1, ], 
    aes(x = average_cost, y = agency),
            label = "Reestimated", fontface = "bold",
            color = "cyan4",
            vjust = 1.5) +
  labs(x = "Average cost per student", 
       y =  NULL, 
       title="Average cost per student: Self-reported vs. reestimated", 
       caption = "Source: Authors' calculation") 

ggsave(
  "self_reported.png",
  self_reported,
  width = 6.5,
  height = 3,
  units = "in", 
  dpi = 1200
)


# Bar charts: benchmark the estimated cost against the costing framework

bm <- read.csv("benchmark.csv", na.strings = c(""), stringsAsFactors = FALSE)
bm$agency <- factor(bm$agency, levels = c("MoE", "UN", "INGO1", "INGO2", "INGO3", "LNGO1", "LNGO2"))
bm$group <- factor(bm$group, levels = "MoE", "UN", "INGO", "LNGO")

## Table 1
var <- "average_cost"
min <- c(99, 115, 130)
max <- c(162, 185, 208)

data_cleaning_fun <- function(var, min, max){
  
  cost1_3 <- c(min[1], max[1], ifelse(bm$grade1_3 == 1, bm[, which(colnames(bm) == var)], NA))
  cost4_6 <- c(min[2], max[2], ifelse(bm$grade4_6 == 1, bm[, which(colnames(bm) == var)], NA))
  cost7_9 <- c(min[3], max[3], ifelse(bm$grade7_9 == 1, bm[, which(colnames(bm) == var)], NA))
  agency <- c("Min", "Max", "MoE", "UN", "INGO1", "INGO2", "INGO3", "LNGO1", "LNGO2")
  
  cost <- as.data.frame(rbind(cbind(cost1_3, rep("Grade 1-3", 9), agency), 
                              cbind(cost4_6, rep("Grade 4-6", 9), agency), 
                              cbind(cost7_9, rep("Grade 7-9", 9), agency)))
  colnames(cost) <- c("cost", "grade", "agency")
  cost$cost <- sapply(cost$cost, function(x) {as.numeric(levels(x))[x]})
  cost$agency <- factor(cost$agency, levels = c("MoE", "UN", "INGO1", "INGO2", "INGO3", "LNGO1", "LNGO2", "Min", "Max"))
  cost$group <- c(rep(c("NA", "NA", "MoE", "UN", "INGO", "INGO", "INGO", "LNGO", "LNGO"), 3))
  cost$group <- factor(cost$group, levels = c("MoE", "UN", "INGO", "LNGO", "NA"))
  return(cost)
}

cost <- data_cleaning_fun(var, min, max)

bm_average_cost_dot <- ggplot(cost[cost$agency %in% c("Min", "Max", "OB", "HB", "INGO", "LNGO") & is.na(cost$cost) == FALSE, ], 
                   aes(x = cost, y = grade, shape = agency)) + 
  geom_point(aes(col = agency), size = 3) +
  theme_economist() + scale_fill_economist() +
  theme(plot.title = element_text(family = "OfficinaSanITC-Book"),
        text = element_text(family = "OfficinaSanITC-Book"), 
        legend.text = element_text(family = "OfficinaSanITC-Book")) +
  geom_text_repel(aes(label = agency), family = 'OfficinaSanITC-Book', size = 3) +
  geom_label_repel(aes(label = paste("$", round(cost, 0))), family = 'OfficinaSanITC-Book', size = 3) +
  labs(title="Cost of teacher training", 
       y = "Average cost of training per student", 
       x = "Number of training days per year", 
       caption = "Source: Authors' calculation")

bm_table1 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO")& is.na(cost$cost) == FALSE, ], 
       aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 0))), size = 3) +
  labs(title = "CBE unit cost per student, per year (program + admin + overhead)", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation") + 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO")) +
  theme_grey(base_size = 10) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  "bm_table1.png",
  bm_table1,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)



## Table 2
var <- "average_cost_program"
min <- c(69, 80, 91)
max <- c(101, 117, 133)

cost <- data_cleaning_fun(var, min, max)

bm_table2 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO") & is.na(cost$cost) == FALSE, ], 
                          aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 0))), size = 3) +
  labs(title = "CBE unit cost per student, per year (not incl. admin. cost & overhead)", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  "bm_table2.png",
  bm_table2,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

## Table 3

var <- "teacher_salary"
min <- c(36, 45, 54)
max <- c(48, 60, 72)

cost <- data_cleaning_fun(var, min, max)

bm_table3 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO") & is.na(cost$cost) == FALSE, ], 
                          aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 0))), size = 3) +
  labs(title = "Teacher salaries", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  "bm_table3.png",
  bm_table3,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)


## Table 4

var <- "teacher_training"
min <- c(10, 10, 10)
max <- c(12, 11.7, 11.7)

cost <- data_cleaning_fun(var, min, max)

bm_table4 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO") & is.na(cost$cost) == FALSE, ], 
                          aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Teacher training", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
  "bm_table4.png",
  bm_table4,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)


## Table 5

var <- "classroom_kit"
min <- c(2, 2, 2)
max <- c(3, 2.7, 2.7)

cost <- data_cleaning_fun(var, min, max)

bm_table5 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO") & is.na(cost$cost) == FALSE, ], 
                          aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Classroom kits", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation") + 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
  "bm_table5.png",
  bm_table5,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

## Table 6

var <- "student_kit"
min <- c(5, 6, 7)
max <- c(10, 12, 14)

cost <- data_cleaning_fun(var, min, max)

bm_table6 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO") & is.na(cost$cost) == FALSE, ], 
                          aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Student kits", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
  "bm_table6.png",
  bm_table6,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

## Table 7

var <- "teacher_kit"
min <- c(1, 1.3, 1.3)
max <- c(2, 2, 2)

cost <- data_cleaning_fun(var, min, max)

bm_table7 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO"), ], 
                          aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Teacher kits", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
  "bm_table7.png",
  bm_table7,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

## Table 8

var <- "textbook"
min <- c(5, 6, 7)
max <- c(10, 12, 14)

cost <- data_cleaning_fun(var, min, max)

bm_table8 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO"), ], 
                          aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Textbooks", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  "bm_table8.png",
  bm_table8,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)


## Table 10

var <- "heating"
min <- c(NA, NA, NA)
max <- c(2.5, 2.5, 2.5)

cost <- data_cleaning_fun(var, min, max)

bm_table10 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO"), ], 
                    aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Classroom heating/cooling system", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
  "bm_table10.png",
  bm_table10,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

## Table 11

var <- "shura_training"
min <- c(NA, NA, NA)
max <- c(10, 10, 10)

cost <- data_cleaning_fun(var, min, max)

bm_table11 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO"), ], 
                    aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Community mobilization training", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
  "bm_table11.png",
  bm_table11,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

## Table 12

var <- "average_cost_administration"
min <- c(22.78, 26.4, 30.01)
max <- c(37.29, 42.55, 47.81)

cost <- data_cleaning_fun(var, min, max)

bm_table12 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO"), ], 
                    aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 2))), size = 3) +
  labs(title = "Administrative cost", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  "bm_table12.png",
  bm_table12,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

## Table 13

var <- "average_cost_overhead"
min <- c(7, 8, 9)
max <- c(11, 13, 15)

cost <- data_cleaning_fun(var, min, max)

bm_table13 <- ggplot(cost[cost$group %in% c("MoE", "UN", "INGO", "LNGO"), ], 
                    aes(y = cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ grade) + 
  geom_hline(data = cost[c(1, (nrow(cost)/3+1), (nrow(cost)/3*2+1)), 1:2], aes(yintercept = cost), color = "blue", linetype = "dashed", size = 1.5) + 
  geom_hline(data = cost[c(2, (nrow(cost)/3+2), (nrow(cost)/3*2+2)), 1:2], aes(yintercept = cost), color = "red", linetype = "solid", size = 1.5) + 
  geom_text_repel(aes(label = paste("$", round(cost, 1))), size = 3) +
  labs(title = "Overheads", 
       y = "Average cost per student per year", 
       x = "", 
       caption = "Source: Authors' calculation")+ 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



ggsave(
  "bm_table13.png",
  bm_table13,
  width = 6.5,
  height = 3,
  units = "in",  
  dpi = 1200
)

# Heatmap: Credibility of data
credibility <- read.csv("credibility.csv", na.strings = c(""), stringsAsFactors = FALSE)
cred_long <- credibility %>% gather(key = agency, value = credibility, -1)
cred_long$agency <- factor(cred_long$agency, levels = c("MoE", "UN", "INGO1", "INGO2","INGO3","LNGO1", "LNGO2"))
cred_long$Item <- factor(cred_long$Item, levels = c(
  "Overhead cost",
  "Project monitoring staffing and activities",
  "Project implementation staff",
  "Potable water, sanitation and hygiene facilities",
  "Maintenance cost (classes)",
  "Repairs cost", 
  "Rent of building", 
  "Shura/community mobilization training",
  "Heating/cooling system", 
  "Textbooks", 
  "Teacher kits", 
  "Student kits",
  "Classroom kits",
  "Teacher training",
  "Teacher salary"))

cred <- ggplot(cred_long, aes(x = agency, y = Item)) + 
  geom_tile(aes(fill = credibility), colour = "white") + 
  scale_fill_gradient(low = "white", high = "aquamarine3") + 
  xlab("") +
  ylab("")

ggsave(
  "cred.png",
  cred,
  width = 8,
  height = 4,
  units = "in",  
  dpi = 1200
)


bar <- ggplot(bm, aes(y = average_cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ylab("Average cost") +
  xlab("") + 
  scale_fill_discrete(name = "Model", labels = c("MoE", "UN", "INGO: Int'l NGO", "LNGO: Local NGO"))+
  theme_grey(base_size = 10)
  
data_quality <- ggarrange(bar, cred, nrow = 2, ncol = 1, heights = c(1, 3), align = "v")

ggsave(
  "data_quality.png",
  data_quality,
  width = 8,
  height = 4.5,
  units = "in",  
  dpi = 1200
)

# Heatmap: Decomposition of cost per student by category by implementing agency
cost_heatmap <- read.csv("cost_heatmap_data.csv", na.strings = c(""), stringsAsFactors = FALSE)
cost_heatmap_long <- cost_heatmap %>% gather(key = agency, value = cost_per_student_per_year, -1)
cost_heatmap_long$agency <- factor(cost_heatmap_long$agency, levels = c("MoE", "UN", "INGO1", "INGO2","INGO3","LNGO1", "LNGO2"))
cost_heatmap_long$Item <- factor(cost_heatmap_long$Item, levels = c(
  "Overhead",
  "Administration",
  "Potable water, sanitation and hygiene facilities",
  "Maintenance cost (classes)",
  "Repairs cost", 
  "Rent of building", 
  "Shura/community mobilization training",
  "Heating/cooling system", 
  "Textbooks", 
  "Teacher kits", 
  "Student kits",
  "Classroom kits",
  "Teacher training",
  "Teacher salary"))

cost_heatmap <- ggplot(cost_heatmap_long, aes(x = agency, y = Item)) + 
  geom_tile(aes(fill = cost_per_student_per_year), colour = "white") + 
  scale_fill_gradient(low = "white", high = "red")+
  xlab("") +
  ylab("")

cost <- read.csv("benchmark.csv")
cost$agency <- factor(cost$agency, levels = c("MoE", "UN", "INGO1", "INGO2","INGO3","LNGO1", "LNGO2"))
cost$group <- factor(cost$group, levels = c("MoE", "UN", "INGO", "LNGO"))
bar <- ggplot(cost, aes(y = average_cost, x = agency, fill = group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ylab("Average cost") +
  xlab("") 

decomp <- ggarrange(bar, cost_heatmap, nrow = 2, ncol = 1, heights = c(1, 3), align = "v")

ggsave(
  "decomp.png",
  decomp,
  width = 9,
  height = 6,
  units = "in",  
  dpi = 1200
)

# Scatter plot: teacher salary against teacher salary per student

scatter_tch_salary <- ggplot(data, aes(x = teacher_salary, y = teacher_salary_per_student, shape = group)) + 
  geom_point(aes(col = group), size = 4) +   
  geom_label_repel(aes(label = agency)) +
  expand_limits(x = 0, y = 0)+
  geom_smooth(method = "lm", se=F)+
  theme_classic() +  
  labs(title="Teacher annual salary", 
       y="Teacher salary per student per year", 
       x="Teacher annual salary", 
       caption = "Source: Authors' calculation")
ggsave(
  "scatter_tch_salary.png",
  scatter_tch_salary,
  width = 4.5,
  height = 4,
  units = "in",  
  dpi = 1200
)


# Scatter plot: Class size against teacher salary per student
scatter_class_size <- ggplot(data, aes(x = class_size, y = teacher_salary_per_student, shape = group)) + 
  geom_point(aes(col = group), size = 4) +   
  geom_label_repel(aes(label = agency)) +
  expand_limits(x = 0, y = 0)+
  geom_smooth(method = "lm", se=F)+
  theme_classic() +  
  labs(title="Class size", 
       y="Teacher salary per student per year", 
       x="Class size", 
       caption = "Source: Authors' calculation")
ggsave(
  "scatter_class_size.png",
  scatter_class_size,
  width = 4.5,
  height = 4,
  units = "in",  
  dpi = 1200
)

# Scatter plot: Number of teachers per CBE against teacher salary per student
scatter_n_teacher_per_cbe <- ggplot(data, aes(x = n_teacher_per_cbe, y = teacher_salary_per_student, shape = group)) + 
  geom_point(aes(col = group), size = 4) +   
  geom_label_repel(aes(label = agency)) +
  expand_limits(x = 0, y = 0)+
  geom_smooth(method = "lm", se=F)+
  theme_classic() +  
  labs(title="Number of teachers per CBE", 
       y="Teacher salary per student per year", 
       x="Number of teachers per CBE", 
       caption = "Source: Authors' calculation")
ggsave(
  "scatter_n_teacher_per_cbe.png",
  scatter_n_teacher_per_cbe,
  width = 4.5,
  height = 4,
  units = "in",  
  dpi = 1200
)


