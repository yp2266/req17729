######## Cost of PADIN in Brazil   #########
########         Yilin Pan         #########

rm (list = ls())
setwd("C:/Users/WB508861/OneDrive - WBG/8_Brazil/Data")
suppressMessages(stopifnot(require(dplyr)))
suppressMessages(stopifnot(require(tidyr)))
suppressMessages(stopifnot(require(ggplot2)))
suppressMessages(stopifnot(require(ggcorrplot)))
suppressMessages(stopifnot(require(ggrepel)))
suppressMessages(stopifnot(require(ggthemes)))
suppressMessages(stopifnot(require(ggalt)))
suppressMessages(stopifnot(require(gridExtra)))
suppressMessages(stopifnot(require(ggpubr)))

# Heatmap: Decomposition of average cost by ingredients

data <- read.csv("heatmap.csv", na.strings = c(""), stringsAsFactors = FALSE)
municipality <- data$ï..Municipality

data_long <- data %>% gather(key = category, value = `Cost per child`, Supervisor:Supplies)
data_long$category <- factor(data_long$category, levels = 
                             c("ADIs", "Supervisor", "Transportation", "Food", 
                               "Training", "Supplies"))
data_long$ï..Municipality <- factor(data_long$ï..Municipality, 
                                    levels = municipality)

decomposition_average_cost <- ggplot(data_long, aes(x = category, y = ï..Municipality)) + 
  geom_tile(aes(fill = `Cost per child`), colour = "white") + 
  scale_fill_gradient(low = "white", high = "darkcyan") + 
  xlab("") +
  ylab("")

ggsave(
  "decomposition_average_cost.png",
  decomposition_average_cost,
  width = 8,
  height = 4.5,
  units = "in",  
  dpi = 1200
)


# Scatterplot: Supervisor's salary vs. % of time devoted to PADIN

data <- read.csv("scatterplot_supervisor.csv", na.strings = c(""), stringsAsFactors = FALSE)
data$Top_10_supervisor_cost <- factor(data$Top_10_supervisor_cost, levels = c("Yes", "No"))
data$percent <- as.numeric(data$percent)

supervisor_cost <- ggplot(data, aes(x = percent, y = salary, shape = Top_10_supervisor_cost)) + 
  geom_point(aes(col = Top_10_supervisor_cost), size = 3) +  
  theme_economist() + scale_fill_economist() +
  theme(plot.title = element_text(family = "OfficinaSanITC-Book"),
        text = element_text(family = "OfficinaSanITC-Book"), 
        legend.text = element_text(family = "OfficinaSanITC-Book")) +
  geom_text_repel(aes(label = ï..Municipality), family = 'OfficinaSanITC-Book', size = 3) +
  scale_x_continuous(labels = scales::percent) + 
  labs(title="", 
       y = "Supervisor's monthly compensation from municipality", 
       x = "% of supervisor's time devoted to PADIN", 
       caption = "Source: Authors' calculation")

ggsave(
  "supervisor_cost.png",
  supervisor_cost,
  width = 8,
  height = 4.5,
  units = "in",  
  dpi = 1200
)


# Piechart: Distribution of transportation type

data <- read.csv("transportation.csv", na.strings = c(""), stringsAsFactors = FALSE)
data$type <- factor(data$type, levels = c("Own vehicle without cost aid", 
                                          "Own vehicle with cost aid", 
                                          "Municipality vehicle", 
                                          "Own vehicle with cost aid or in municipality vehicle"))

n <- data %>% group_by(type) %>% summarize(n = n()) %>% arrange(desc(type)) %>%
  mutate(prop = n / sum(n) *100) %>% 
  mutate(ypos = cumsum(prop)- 0.5*prop ) 

library(RColorBrewer)

transportation_dist <- ggplot(n, aes(x = "", y = prop, fill = type)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = ypos, label = n), color = "white", size = 5) +
  geom_text(aes(y = ypos, label = n), color = "white", size = 5) +
  coord_polar("y", start = 0) + 
  labs(title="", 
       y = "", 
       x = "", 
       caption = "")

ggsave(
  "transportation_dist.png",
  transportation_dist,
  width = 8,
  height = 4,
  units = "in",  
  dpi = 1200
)


# Bar charts by group: Range of transportation cost by transportation type
data <- data %>% group_by(type) %>% arrange(cost, .by_group = TRUE)

panel1 <- ggplot(data[data$type == "Own vehicle with cost aid", ] %>% arrange(cost), 
                 aes(y = cost, x = reorder(Municipality, cost))) + 
  geom_bar(position="dodge", stat="identity", color = "gray", fill = "yellow") + 
  ylim(0, max(data$cost)) +
  theme(legend.position="none") +
  labs(title="Own vehicle with cost aid", 
       y = "", 
       x = "", 
       caption = "")

ggsave(
  "panel1.png",
  panel1,
  width = 8,
  height = 2.5,
  units = "in",  
  dpi = 1200
)

panel2 <- ggplot(data[data$type == "Municipality vehicle", ] %>% arrange(cost), 
                 aes(y = cost, x = reorder(Municipality, cost))) + 
  geom_bar(position="dodge", stat="identity", color = "gray", fill = "gray") + 
  ylim(0, max(data$cost)) +
  theme(legend.position="none") +
  labs(title="Municipality vehicle", 
       y = "", 
       x = "", 
       caption = "")

ggsave(
  "panel2.png",
  panel2,
  width = 4,
  height = 2.5,
  units = "in",  
  dpi = 1200
)

panel3 <- ggplot(data[data$type == "Own vehicle with cost aid or in municipality vehicle", ] %>% arrange(cost), 
                 aes(y = cost, x = reorder(Municipality, cost))) + 
  geom_bar(position="dodge", stat="identity", color = "gray", fill = "orange") + 
  ylim(0, max(data$cost)) +
  theme(legend.position="none") +
  labs(title="Own vehicle with cost aid or in municipality vehicle", 
       y = "", 
       x = "", 
       caption = "")

ggsave(
  "panel3.png",
  panel3,
  width = 4,
  height = 2.5,
  units = "in",  
  dpi = 1200
)


# Jitter chart: Range of transportation cost by group

transportation_range <- ggplot(data = data, aes(x = type, y = cost, color = type)) +
  geom_jitter(position = position_jitter(0.2)) + 
 # stat_summary(fun = "mean", colour = "red", shape = 18, size = 0.8) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="", 
       y = "Cost of transportation per child per month", 
       x = "", 
       caption = "")
  

ggsave(
  "transportation_range.png",
  transportation_range,
  width = 8,
  height = 4,
  units = "in",  
  dpi = 1200
)

# Piecharts: Frequency of community meetings
data <- read.csv("community_meetings.csv", na.strings = c(""), stringsAsFactors = FALSE)
data$Frequency <- factor(data$Frequency, levels = c("Two to three times a month", 
                                          "Twice a month", 
                                          "Once a month", 
                                          "Once every two months", 
                                          "Once every six months"))

n <- data %>% group_by(Frequency) %>% summarize(n = n()) %>% arrange(desc(Frequency)) %>%
  mutate(prop = n / sum(n) *100) %>% 
  mutate(ypos = cumsum(prop)- 0.5*prop ) 

community_meetings_piechart <- ggplot(n, aes(x = "", y = prop, fill = Frequency)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = ypos, label = n), color = "white", size = 5)+
  geom_text(aes(y = ypos, label = n), color = "white", size = 5)+
coord_polar("y", start = 0) + 
  ggpubr::fill_palette("Dark2") +
  labs(title="", 
       y = "", 
       x = "", 
       caption = "")

ggsave(
  "community_meetings_piechart.png",
  community_meetings_piechart,
  width = 8,
  height = 4,
  units = "in",  
  dpi = 1200
)


# Dotplot: Days of supervisors' training

data <- read.csv("supervisor_training.csv", na.strings = c(""), stringsAsFactors = FALSE)


supervisor_training_days <- ggplot(data, aes(x = N_times_ongoing)) +
  geom_dotplot(method = "histodot", fill = "gold", 
               color = "black") + 
  scale_x_continuous(breaks = seq(0, 12, 1), limits = c(0, 12)) + 
  theme(legend.position = "none",
       axis.text.y = element_blank()) + 
  labs(title = "",
       y = "",
       x = "Number of days per year for ongoing training")

ggsave(
  "supervisor_training_days.png",
  supervisor_training_days,
  width = 8,
  height = 4,
  units = "in",  
  dpi = 1200
)

# Barchart: Supervisor's daily rate
data <- data %>% arrange(daily_rate)
label <- data$Municipality
data$Municipality <- factor(data$Municipality, levels = label)

supervisor_daily_rate <- ggplot(data, aes(x = daily_rate, y = Municipality)) + 
  geom_bar(stat="identity", fill = "aquamarine3") + 
  geom_hline(aes(yintercept = mean(daily_rate, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) + 
  labs(title = "",
       y = "",
       x = "Daily rate for supervisor's training")
  

ggsave(
  "supervisor_daily_rate.png",
  supervisor_daily_rate,
  width = 4,
  height = 4,
  units = "in",  
  dpi = 1200
)
