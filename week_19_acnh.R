#libraries and initial cleaning ----
#Libraries and data import
library(readr)
library(tidyverse)
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
options(scipen = 999)

#Cleaning buy_currency and filtering on:
  #1. buy_value isn't NA since we want to calculate profits
  #2. buy currency must be bells since we want to calculate profits
items <- items %>% mutate(buy_currency = ifelse(is.na(buy_currency) == TRUE,sell_currency,buy_currency)) %>%
  filter(!is.na(buy_value), buy_currency == "bells")

#average plot ====
#calculate profits then group by category and get the average
tom_profits <- items %>% mutate(profits = buy_value - sell_value) %>% 
  select(category,profits) %>% 
  filter(!is.na(category),!is.na(profits)) %>%
  group_by(category) %>% 
  dplyr::summarize(avg = mean(profits))

#reorder in descending order and then add the repeating sequence so the colours repeat in stripes
tom_profits <- tom_profits[order(tom_profits$avg),] %>% mutate(col_group = rep(c("a","b","c"),6))

#plot
p <- tom_profits %>% ggplot(aes(x = reorder(category,avg), y = avg, fill = col_group)) +
  #labels and titles
  xlab("Category") + ylab("Average Profit") + ggtitle("Tom Nook's Average Profits","by Item Category") +
  #y axis and x axis breaks
  geom_hline(yintercept = seq(0,13000,1000), 
             linetype = "dotted", 
             color = "#5E778E",
             size = 0.5) +
  scale_y_continuous(breaks = seq(0, 13000, by = 1000)) +
  #adding the bars. Stat = identity because we already have a count
  geom_bar(stat = "identity") +
  #these are the three colours that will repeat 
  scale_fill_manual(values = c("#8a83ce","#ff9391","#ffb1c1")) +
  #formatting
  theme(
    legend.position = "blank",
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_line(colour = "white"),
    text = element_text(size = 10, colour = "#5E778E",family = "Sofia Pro Medium"),
    plot.title = element_text(size = 20, hjust = 0.5, colour = "#404F5E"),
    plot.subtitle = element_text(size = 12, hjust = 0.145),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.4)
  )

#save plot with date in name
ggsave(filename = paste("ac_avg_tom_profits_",as.character(Sys.Date()),".png",sep = ""),
       plot = p,
       width = 5,
       height = 4,
       dpi = 300,
       units = "in",
       device = "png")

#median plot ====
#calculate profits then group by category and get the median
tom_profits_med <- items %>% mutate(profits = buy_value - sell_value) %>% 
  select(category,profits) %>% 
  filter(!is.na(category),!is.na(profits)) %>%
  group_by(category) %>% 
  dplyr::summarize(median_profit = median(profits))

tom_profits_med <- tom_profits_med[order(tom_profits_med$median_profit),] %>% mutate(col_group = rep(c("a","b","c"),6))

p <- tom_profits_med %>% ggplot(aes(x = reorder(category,median_profit), y = median_profit, fill = col_group)) +
  #labels and titles
  xlab("Category") + ylab("Median Profit") + ggtitle("Tom Nook's Median Profits","by Item Category") +
  #y axis and x axis breaks
  geom_hline(yintercept = seq(0,3000,500), 
             linetype = "dotted", 
             color = "#5E778E",
             size = 0.5) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  #adding the bars. Stat = identity because we already have a count
  geom_bar(stat = "identity") +
  #these are the three colours that will repeat
scale_fill_manual(values = c("#8a83ce","#ff9391","#ffb1c1")) +
  #formatting
  theme(
    legend.position = "blank",
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_line(colour = "white"),
    text = element_text(size = 10, colour = "#5E778E",family = "Sofia Pro Medium"),
    plot.title = element_text(size = 20, hjust = 0.5, colour = "#404F5E"),
    plot.subtitle = element_text(size = 12, hjust = 0.145),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.4)
  )
p
#save plot with date in name
ggsave(filename = paste("ac_median_tom_profits_",as.character(Sys.Date()),".png",sep = ""),
       plot = p,
       width = 5,
       height = 4,
       dpi = 300,
       units = "in",
       device = "png")

##box plot ----
#same as before but don't group so that we can see the distribution in the boxplot
tom_profits_bp <- items %>% mutate(profits = buy_value - sell_value) %>% 
  select(category,profits) %>% 
  filter(!is.na(category),!is.na(profits))

#here i join the previous df so that i can get the same colour convetions and then drop the average since it's not useful
tom_profits_bp <- merge(tom_profits_bp,tom_profits, by = "category") %>% select(-avg)

tom_profits_bp %>% ggplot(aes(x = reorder(category,-profits), y = profits, fill = col_group)) + scale_y_log10(breaks = 10^(1:10)) +
  #labels and titles
  xlab("Category") + ylab("Median Profit (log transformation)") + ggtitle("Tom Nook's Profits","by Item Category") +
  geom_hline(yintercept = 10^(1:6), 
           linetype = "dotted", 
           color = "#5E778E",
           size = 0.5) +
  #adding the bars. Stat = identity because we already have a count
  geom_boxplot() +
  #these are the three colours that will repeat
scale_fill_manual(values = c("#8a83ce","#ff9391","#ffb1c1")) +
  #formatting
  theme(
    legend.position = "blank",
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_line(colour = "white"),
    text = element_text(size = 10, colour = "#5E778E",family = "Sofia Pro Medium"),
    plot.title = element_text(size = 20, hjust = 0.5, colour = "#404F5E"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.4)
  )
ggsave(filename = paste("ac_tom_profits_",as.character(Sys.Date()),".png",sep = ""),
       plot = last_plot(),
       width = 5,
       height = 4,
       dpi = 300,
       units = "in",
       device = "png")
