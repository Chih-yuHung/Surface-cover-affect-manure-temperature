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
#Depth 0.5m
t.test.0.5.c <- Depth.0.5.c %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.0.5.c$`No cover`, t.test.0.5.c$Cover,
       paired = TRUE, conf.level = 0.95, alternative = "less") #During cover at 0.5 m
t.test.0.5.spring <- t.test.0.5 %>%
  filter(t.test.0.5$Season == "Spring")
t.test(t.test.0.5.spring$`No cover`, t.test.0.5.spring$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Spring
t.test.0.5.summer <- t.test.0.5 %>%
  filter(t.test.0.5$Season == "Summer")
t.test(t.test.0.5.summer$`No cover`, t.test.0.5.summer$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Summer
t.test.0.5.fall <- t.test.0.5 %>%
  filter(t.test.0.5$Season == "Fall")
t.test(t.test.0.5.fall$`No cover`, t.test.0.5.fall$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Fall
t.test.0.5.winter <- t.test.0.5 %>%
  filter(t.test.0.5$Season == "Winter")
t.test(t.test.0.5.winter$`No cover`, t.test.0.5.winter$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Winter

#Depth 1.5m
t.test.1.5 <- Depth.1.5 %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.1.5$`No cover`, t.test.1.5$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Year
t.test.1.5.spring <- t.test.1.5 %>%
  filter(t.test.1.5$Season == "Spring")
t.test(t.test.1.5.spring$`No cover`, t.test.1.5.spring$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Spring
t.test.1.5.summer <- t.test.1.5 %>%
  filter(t.test.1.5$Season == "Summer")
t.test(t.test.1.5.summer$`No cover`, t.test.1.5.summer$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Summer
t.test.1.5.fall <- t.test.1.5 %>%
  filter(t.test.1.5$Season == "Fall")
t.test(t.test.1.5.fall$`No cover`, t.test.1.5.fall$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Fall
t.test.1.5.winter <- t.test.1.5 %>%
  filter(t.test.1.5$Season == "Winter")
t.test(t.test.1.5.winter$`No cover`, t.test.1.5.winter$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Winter

#Depth 2.5 m
t.test.2.5 <- Depth.2.5 %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
t.test(t.test.2.5$`No cover`, t.test.2.5$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Year
t.test.2.5.spring <- t.test.2.5 %>%
  filter(t.test.2.5$Season == "Spring")
t.test(t.test.2.5.spring$`No cover`, t.test.2.5.spring$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Spring
t.test.2.5.summer <- t.test.2.5 %>%
  filter(t.test.2.5$Season == "Summer")
t.test(t.test.2.5.summer$`No cover`, t.test.2.5.summer$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Summer
t.test.2.5.fall <- t.test.2.5 %>%
  filter(t.test.2.5$Season == "Fall")
t.test(t.test.2.5.fall$`No cover`, t.test.2.5.fall$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Fall
t.test.2.5.winter <- t.test.2.5 %>%
  filter(t.test.2.5$Season == "Winter")
t.test(t.test.2.5.winter$`No cover`, t.test.2.5.winter$Cover, 
       paired = TRUE,
       conf.level = 0.95) #Winter



#Graph
Comparison.0.5 <- ggplot(data = Depth.0.5,
                           aes(x = Date, 
                               y = Temperature,
                               color = Tank)) +
  geom_line(lwd = 1.1) +
  labs(x = "Date", y = "Temperature (°C)", title = "Manure, 0.5 m") +
  scale_color_manual(values = c('No cover' = "#fcab42", 'Cover' = "#42bd42")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.95),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-10, 21), 
                     breaks = seq(-10, 21, by = 4),
                     expand = c(0, 0)) +
  geom_vline(xintercept = c('2020-06-18','2020-11-10'),
             lwd = 1.1,
             color = "grey") 
Comparison.0.5

Comparison.1.5 <- ggplot(data = Depth.1.5,
                         aes(x = Date, 
                             y = Temperature,
                             color = Tank)) +
  geom_line(lwd = 1.1) +
  labs(x = "Date", y = "Temperature (°C)", title = "Manure, 1.5 m") +
  scale_color_manual(values = c('No cover' = "#fcab42", 'Cover' = "#42bd42")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-10, 21), 
                     breaks = seq(-10, 21, by = 4),
                     expand = c(0, 0))
Comparison.1.5

Comparison.2.5 <- ggplot(data = Depth.2.5,
                         aes(x = Date, 
                             y = Temperature,
                             color = Tank)) +
  geom_line(lwd = 1.1) +
  labs(x = "Date", y = "Temperature (°C)", title = "Manure, 2.5 m") +
  scale_color_manual(values = c('No cover' = "#fcab42", 'Cover' = "#42bd42")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-10, 21), 
                     breaks = seq(-10, 21, by = 4),
                     expand = c(0, 0))
Comparison.2.5

ggsave("Results/Temperature comparison between cover and no cover.png",
       ggarrange(Comparison.0.5, Comparison.1.5, Comparison.2.5,
                 ncol = 1, nrow = 3),
       width = 15, height = 24, units = "cm",
       dpi = 300)

