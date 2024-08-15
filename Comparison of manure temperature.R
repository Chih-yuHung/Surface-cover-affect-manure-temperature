library(tidyverse);library(ggpubr); library(reshape2)

result <- "Results/"

#Read data for graph
VA <- read_csv("Input/VA/Manure temp_VA.csv")
VA.temp <- VA %>%
  mutate(Date = paste(Year,"-",Month,"-",Day, sep = "")) %>%
  select(Date, VA.Temp.0.5 = temp0.5, VA.Temp.1.5 = temp1.5, VA.Temp.2.5 = temp2.5)
VA.temp$Date <- as.Date(VA.temp$Date)

VAC <- read_csv("Input/VAC/Manure temp_VAC.csv")
VAC.temp <- VAC %>%
  mutate(Date = paste(Year,"-",Month,"-",Day, sep = "")) %>%
  select(Date, VAC.Temp.0.5 = temp0.5, VAC.Temp.1.5 = temp1.5, VAC.Temp.2.5 = temp2.5)
VAC.temp$Date <- as.Date(VAC.temp$Date)

#Prepare data for graph
Comparison.temp <- merge(VA.temp,VAC.temp, by = c("Date"))
Comparison.temp <- Comparison.temp %>%
  mutate(Diff.0.5 = VAC.Temp.0.5-VA.Temp.0.5) %>%
  mutate(Diff.1.5 = VAC.Temp.1.5-VA.Temp.1.5) %>%
  mutate(Diff.2.5 = VAC.Temp.2.5-VA.Temp.2.5) %>%
  pivot_longer(cols = c('VA.Temp.0.5', 'VA.Temp.1.5', 'VA.Temp.2.5',
                        'VAC.Temp.0.5', 'VAC.Temp.1.5', 'VAC.Temp.2.5',
                        'Diff.0.5', 'Diff.1.5', 'Diff.2.5'),
               names_to = 'Tank',
               values_to = 'Temperature')

Depth.0.5 <- Comparison.temp %>%
  filter(Tank == 'VA.Temp.0.5' | Tank == 'VAC.Temp.0.5' | Tank == 'Diff.0.5')
Depth.0.5$Tank[Depth.0.5$Tank == 'VA.Temp.0.5'] <- 'No cover'
Depth.0.5$Tank[Depth.0.5$Tank == 'VAC.Temp.0.5'] <- 'Cover'
Depth.0.5$Tank[Depth.0.5$Tank == 'Diff.0.5'] <- 'Difference'
Depth.1.5 <- Comparison.temp %>%
  filter(Tank == 'VA.Temp.1.5' | Tank == 'VAC.Temp.1.5' | Tank == 'Diff.1.5')
Depth.1.5$Tank[Depth.1.5$Tank == 'VA.Temp.1.5'] <- 'No cover'
Depth.1.5$Tank[Depth.1.5$Tank == 'VAC.Temp.1.5'] <- 'Cover'
Depth.1.5$Tank[Depth.1.5$Tank == 'Diff.1.5'] <- 'Difference'
Depth.2.5 <- Comparison.temp %>%
  filter(Tank == 'VA.Temp.2.5' | Tank == 'VAC.Temp.2.5' | Tank == 'Diff.2.5')
Depth.2.5$Tank[Depth.2.5$Tank == 'VA.Temp.2.5'] <- 'No cover'
Depth.2.5$Tank[Depth.2.5$Tank == 'VAC.Temp.2.5'] <- 'Cover'
Depth.2.5$Tank[Depth.2.5$Tank == 'Diff.2.5'] <- 'Difference'

Comparison.cover <- Comparison.temp[Comparison.temp$Date >= "2020-06-18" & Comparison.temp$Date <= "2020-11-10", ]
Depth.0.5.c <- Comparison.cover %>%
  filter(Tank == 'VA.Temp.0.5' | Tank == 'VAC.Temp.0.5' | Tank == 'Diff.0.5')
Depth.0.5.c$Tank[Depth.0.5.c$Tank == 'VA.Temp.0.5'] <- 'No cover'
Depth.0.5.c$Tank[Depth.0.5.c$Tank == 'VAC.Temp.0.5'] <- 'Cover'
Depth.0.5.c$Tank[Depth.0.5.c$Tank == 'Diff.0.5'] <- 'Difference'
Depth.1.5.c <- Comparison.cover %>%
  filter(Tank == 'VA.Temp.1.5' | Tank == 'VAC.Temp.1.5' | Tank == 'Diff.1.5')
Depth.1.5.c$Tank[Depth.1.5.c$Tank == 'VA.Temp.1.5'] <- 'No cover'
Depth.1.5.c$Tank[Depth.1.5.c$Tank == 'VAC.Temp.1.5'] <- 'Cover'
Depth.1.5.c$Tank[Depth.1.5.c$Tank == 'Diff.1.5'] <- 'Difference'
Depth.2.5.c <- Comparison.cover %>%
  filter(Tank == 'VA.Temp.2.5' | Tank == 'VAC.Temp.2.5' | Tank == 'Diff.2.5')
Depth.2.5.c$Tank[Depth.2.5.c$Tank == 'VA.Temp.2.5'] <- 'No cover'
Depth.2.5.c$Tank[Depth.2.5.c$Tank == 'VAC.Temp.2.5'] <- 'Cover'
Depth.2.5.c$Tank[Depth.2.5.c$Tank == 'Diff.2.5'] <- 'Difference'

Comparison.no.c <- Comparison.temp[Comparison.temp$Date >= "2020-11-11" & Comparison.temp$Date <= "2021-05-27", ]
Depth.0.5.n <- Comparison.no.c %>%
  filter(Tank == 'VA.Temp.0.5' | Tank == 'VAC.Temp.0.5' | Tank == 'Diff.0.5')
Depth.0.5.n$Tank[Depth.0.5.n$Tank == 'VA.Temp.0.5'] <- 'No cover'
Depth.0.5.n$Tank[Depth.0.5.n$Tank == 'VAC.Temp.0.5'] <- 'Cover'
Depth.0.5$Tank[Depth.0.5$Tank == 'Diff.0.5'] <- 'Difference'
Depth.1.5.n <- Comparison.no.c %>%
  filter(Tank == 'VA.Temp.1.5' | Tank == 'VAC.Temp.1.5' | Tank == 'Diff.1.5')
Depth.1.5.n$Tank[Depth.1.5.n$Tank == 'VA.Temp.1.5'] <- 'No cover'
Depth.1.5.n$Tank[Depth.1.5.n$Tank == 'VAC.Temp.1.5'] <- 'Cover'
Depth.1.5.n$Tank[Depth.1.5.n$Tank == 'Diff.1.5'] <- 'Difference'
Depth.2.5.n <- Comparison.no.c %>%
  filter(Tank == 'VA.Temp.2.5' | Tank == 'VAC.Temp.2.5' | Tank == 'Diff.2.5')
Depth.2.5.n$Tank[Depth.2.5.n$Tank == 'VA.Temp.2.5'] <- 'No cover'
Depth.2.5.n$Tank[Depth.2.5.n$Tank == 'VAC.Temp.2.5'] <- 'Cover'
Depth.2.5.n$Tank[Depth.2.5.n$Tank == 'Diff.2.5'] <- 'Difference'



#Paired t-test
#Cover
t.test.0.5.c <- Depth.0.5.c %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.0.5.c$`No cover`, t.test.0.5.c$Cover,
       paired = TRUE, conf.level = 0.95, alternative = "less") #During cover at 0.5 m

t.test.1.5.c <- Depth.1.5.c %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.1.5.c$`No cover`, t.test.1.5.c$Cover,
       paired = TRUE, conf.level = 0.95, alternative = "less") #During cover at 1.5 m

t.test.2.5.c <- Depth.2.5.c %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.2.5.c$`No cover`, t.test.2.5.c$Cover,
       paired = TRUE, conf.level = 0.95, alternative = "less") #During cover at 2.5 m

#No cover
t.test.0.5.n <- Depth.0.5.n %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.0.5.n$`No cover`, t.test.0.5.n$Cover,
       paired = TRUE, conf.level = 0.95, alternative = "less") #During cover at 0.5 m

t.test.1.5.n <- Depth.1.5.n %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.1.5.n$`No cover`, t.test.1.5.n$Cover,
       paired = TRUE, conf.level = 0.95, alternative = "less") #During cover at 1.5 m

t.test.2.5.n <- Depth.2.5.n %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.2.5.n$`No cover`, t.test.2.5.n$Cover,
       paired = TRUE, conf.level = 0.95, alternative = "less") #During cover at 2.5 m



#Graph
date_range <- which(Depth.0.5$Date %in% as.Date(c("2020-06-18","2020-11-10")))

Comparison.0.5 <- ggplot(data = Depth.0.5,
                           aes(x = Date, 
                               y = Temperature,
                               color = Tank)) +
  geom_line(lwd = 1.1) +
  labs(x = "Date", y = "Temperature (°C)", title = "Manure, 0.5 m") +
  scale_color_manual(values = c('No cover' = "#fcab42", 'Cover' = "#42bd42", 'Difference' = "grey53")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.95),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-6, 20), 
                     breaks = seq(-6, 20, by = 4),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0,
             lwd = 1.1,
             color = "grey") +
  geom_vline(xintercept = as.numeric(Depth.0.5$Date[date_range]),
             lwd = 1.1,
             color = "grey") 
Comparison.0.5

Comparison.1.5 <- ggplot(data = Depth.1.5,
                         aes(x = Date, 
                             y = Temperature,
                             color = Tank)) +
  geom_line(lwd = 1.1) +
  labs(x = "Date", y = "Temperature (°C)", title = "Manure, 1.5 m") +
  scale_color_manual(values = c('No cover' = "#fcab42", 'Cover' = "#42bd42", 'Difference' = "grey53")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-6, 20), 
                     breaks = seq(-6, 20, by = 4),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0,
             lwd = 1.1,
             color = "grey") +
  geom_vline(xintercept = as.numeric(Depth.0.5$Date[date_range]),
             lwd = 1.1,
             color = "grey") 
Comparison.1.5

Comparison.2.5 <- ggplot(data = Depth.2.5,
                         aes(x = Date, 
                             y = Temperature,
                             color = Tank)) +
  geom_line(lwd = 1.1) +
  labs(x = "Date", y = "Temperature (°C)", title = "Manure, 2.5 m") +
  scale_color_manual(values = c('No cover' = "#fcab42", 'Cover' = "#42bd42", 'Difference' = "grey53")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-6, 20), 
                     breaks = seq(-6, 20, by = 4),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0,
             lwd = 1.1,
             color = "grey") +
  geom_vline(xintercept = as.numeric(Depth.0.5$Date[date_range]),
             lwd = 1.1,
             color = "grey") 
Comparison.2.5

ggsave("Results/Temperature comparison between cover and no cover.png",
       ggarrange(Comparison.0.5, Comparison.1.5, Comparison.2.5,
                 ncol = 1, nrow = 3),
       width = 15, height = 24, units = "cm",
       dpi = 300)
