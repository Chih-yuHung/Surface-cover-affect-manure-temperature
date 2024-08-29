library(tidyverse) 
library(broom)
library(ggpmisc)    # For stat_poly_eq and stat_poly_line
#library(patchwork) # For column graph title
library(ggpubr)     # For ggarrange
#library(grid)      # For graph alignment
library(openxlsx2)

#Read data for graph
Air_temp_VA_data <- read_csv("Input/VA/Air temp_VA.csv")

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

# Add air temperature to the VA data 
Air_temp_VA <- Air_temp_VA_data %>%
  select(c(Date = DATUM, "Air temperature" = XTEMP)) %>%
  mutate(Date = strptime(Date, "%Y%m%d", tz = "UCT")) %>%
  mutate(Date = as.Date(Date, "%Y%m%d"))

# Wide dataframe with all temperatures
VA.VAC.temp <- VA.temp %>%
  merge(Air_temp_VA, by = 'Date', all.x = TRUE) %>%
  merge(VAC.temp, by = 'Date', all.x = TRUE) %>%
  select(c("Date", "VA.Temp.0.5", "VA.Temp.1.5", "VA.Temp.2.5",
           "VAC.Temp.0.5", "VAC.Temp.1.5", "VAC.Temp.2.5", Air.temp = "Air temperature")) %>%
  # Add Manure - Air columns for each depths
  mutate(across(-matches("Air.temp"), ~ . - Air.temp,
                .names = "Difference {sub('VA.Temp', 'VA-Air', col)}")) %>%
  # Add Cover - No cover columns for each depths
  mutate(across(starts_with('VA.Temp'), 
                ~ get(gsub('VA.Temp', 'VAC.Temp', cur_column())) - .,
                .names = "Difference {sub('VA.Temp', 'VAC-VA', col)}")) %>%
  select(c("Date","VA.Temp.0.5", "VA.Temp.1.5", "VA.Temp.2.5",
           "VAC.Temp.0.5", "VAC.Temp.1.5", "VAC.Temp.2.5", 
           "Air temperature" = Air.temp,
           "Difference VA-Air 0.5" = "Difference VA-Air.0.5", 
           "Difference VA-Air 1.5" = "Difference VA-Air.1.5", 
           "Difference VA-Air 2.5" = "Difference VA-Air.2.5",
           "Difference VAC-Air 0.5" = "Difference VAC.Temp.0.5", 
           "Difference VAC-Air 1.5" = "Difference VAC.Temp.1.5", 
           "Difference VAC-Air 2.5" = "Difference VAC.Temp.2.5",
           "Difference VAC-VA 0.5" = "Difference VAC-VA.0.5", 
           "Difference VAC-VA 1.5" = "Difference VAC-VA.1.5", 
           "Difference VAC-VA 2.5" = "Difference VAC-VA.2.5"))
write.csv(VA.VAC.temp,"Results/All temperature observations and differences for VA and VAC.csv",row.names = FALSE)

# Prepare for shading VAC-VA
# 0.5 m depth
VA.VAC.temp.shade.0.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VAC-VA 0.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Tank = "Depth 0.5")
VA.VAC.temp.shade.0.5$Date <- as.Date(VA.VAC.temp.shade.0.5$Date)

# 1.5 m depth 
VA.VAC.temp.shade.1.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VAC-VA 1.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Tank = "Depth 1.5")
VA.VAC.temp.shade.1.5$Date <- as.Date(VA.VAC.temp.shade.1.5$Date)

# 2.5 m depth
VA.VAC.temp.shade.2.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VAC-VA 2.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Tank = "Depth 0.5")
VA.VAC.temp.shade.2.5$Date <- as.Date(VA.VAC.temp.shade.2.5$Date)

# Separate data by depth
Comparison.temp <- VA.VAC.temp %>%
  pivot_longer(cols = starts_with(c("VA.Temp", "VAC.Temp", "Diff", "Air")),
               names_to = 'Tank',
               values_to = 'Temperature')

# Function to process Depth data
process_depth <- function(data, depth) {
  # Define the tank types for the depth-specific filters
  depth_specific_tanks <- c("VA.Temp.", "VAC.Temp.", "Difference VAC-VA ")
    data <- data %>%
      filter(Tank %in% paste0(depth_specific_tanks, depth))
  # Apply the labels to the filtered data
  data %>%
    mutate(Tank = case_when(
      str_detect(Tank, "VA.Temp") ~ 'No cover',
      str_detect(Tank, "VAC.Temp") ~ 'Cover',
      str_detect(Tank, "Difference VAC-VA") ~ 'Difference',
      Tank == "Air temperature" ~ 'Air'
    ))
}

# Process Depth data
Depth.0.5 <- process_depth(Comparison.temp, "0.5")
Depth.1.5 <- process_depth(Comparison.temp, "1.5")
Depth.2.5 <- process_depth(Comparison.temp, "2.5")

# Filtered Comparison data
Comparison.cover <- filter(Comparison.temp, Date >= "2020-06-18" & Date <= "2020-11-10")
Comparison.no.c <- filter(Comparison.temp, Date >= "2020-11-11" & Date <= "2021-05-27")

# Process filtered Depth data
Depth.0.5.c <- process_depth(Comparison.cover, "0.5")
Depth.1.5.c <- process_depth(Comparison.cover, "1.5")
Depth.2.5.c <- process_depth(Comparison.cover, "2.5")

Depth.0.5.n <- process_depth(Comparison.no.c, "0.5")
Depth.1.5.n <- process_depth(Comparison.no.c, "1.5")
Depth.2.5.n <- process_depth(Comparison.no.c, "2.5")

# Prepare for shading area in graph
# 0.5 m depth
VA.temp.shade.0.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VA-Air 0.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Depth = "Depth 0.5")
VA.temp.shade.0.5$Date <- as.Date(VA.temp.shade.0.5$Date)

VAC.temp.shade.0.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VAC-Air 0.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Depth = "Depth 0.5")
VAC.temp.shade.0.5$Date <- as.Date(VAC.temp.shade.0.5$Date)

# 1.5 m depth 
VA.temp.shade.1.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VA-Air 1.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Depth = "Depth 1.5")
VA.temp.shade.1.5$Date <- as.Date(VA.temp.shade.1.5$Date)

VAC.temp.shade.1.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VAC-Air 1.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Depth = "Depth 1.5")
VAC.temp.shade.1.5$Date <- as.Date(VAC.temp.shade.1.5$Date)

# 2.5 m depth
VA.temp.shade.2.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VA-Air 2.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Depth = "Depth 0.5")
VA.temp.shade.2.5$Date <- as.Date(VA.temp.shade.2.5$Date)

VAC.temp.shade.2.5 <- VA.VAC.temp %>%
  select(c("Date", "Temperature" = "Difference VAC-Air 2.5")) %>% 
  mutate(Date = as_datetime(Date),
         under = Temperature < 0,
         over = ifelse(!under, Temperature, 0),
         under = ifelse(under, Temperature, 0),
         Depth = "Depth 2.5")
VAC.temp.shade.2.5$Date <- as.Date(VAC.temp.shade.2.5$Date)

#Dataframe for VAC-VA vs Time, Air
VA.VAC.temp.diff <- VA.VAC.temp %>%
  select(c("Date","Difference VAC-VA 0.5", "Difference VAC-VA 1.5", "Difference VAC-VA 2.5")) %>%
  pivot_longer(cols = (c("Difference VAC-VA 0.5", "Difference VAC-VA 1.5", "Difference VAC-VA 2.5")),
               names_to = 'Depth',
               values_to = 'Temperature')
VA.VAC.temp.diff$Date <- as.Date(VA.VAC.temp.diff$Date)
VA.temp <- VA.VAC.temp %>%
  select(c("Date", "Depth 0.5" = "VA.Temp.0.5", "Depth 1.5" = "VA.Temp.1.5", 
           "Depth 2.5" = "VA.Temp.2.5", "Air temperature")) %>%
  pivot_longer(cols = (c("Depth 0.5", "Depth 1.5", "Depth 2.5", "Air temperature")),
               names_to = 'Depth',
               values_to = 'Temperature')

# Dataframe for Manure, Manure-Air vs Air
VA.Air.manure <- VA.VAC.temp %>%
  select(c("Date","VA.Temp.0.5", "VA.Temp.1.5", "VA.Temp.2.5", "Air temperature",
           "Difference VA-Air 0.5", "Difference VA-Air 1.5", "Difference VA-Air 2.5")) 
VA.Air.manure.diff <- VA.Air.manure %>%
  select(c(Air.temp = "Air temperature", "Difference VA-Air 0.5", "Difference VA-Air 1.5", "Difference VA-Air 2.5")) %>%
  pivot_longer(cols = c("Difference VA-Air 0.5", "Difference VA-Air 1.5", "Difference VA-Air 2.5"),
               names_to = 'Depth',
               values_to = 'Temperature') %>%
  arrange(factor(Depth, levels = c("Difference VA-Air 0.5", "Difference VA-Air 1.5", "Difference VA-Air 2.5")))
VA.Air.manure.diff.time <- VA.Air.manure %>%
  select(c("Date", "Difference VA-Air 0.5", "Difference VA-Air 1.5", "Difference VA-Air 2.5")) %>%
  pivot_longer(cols = c("Difference VA-Air 0.5", "Difference VA-Air 1.5", "Difference VA-Air 2.5"),
               names_to = 'Depth',
               values_to = 'Temperature') %>%
  arrange(factor(Depth, levels = c("Difference VA-Air 0.5", "Difference VA-Air 1.5", "Difference VA-Air 2.5")))
VA.Air.manure <- VA.Air.manure %>%
  select(c(Air.temp = "Air temperature", VA.Temp.0.5, VA.Temp.1.5, VA.Temp.2.5)) %>%
  pivot_longer(cols = c(VA.Temp.0.5, VA.Temp.1.5, VA.Temp.2.5),
               names_to = 'Depth',
               values_to = 'Temperature') %>%
  arrange(factor(Depth, levels = c('VA.Temp.0.5', 'VA.Temp.1.5', 'VA.Temp.2.5')))

# VA vs Air
VA.temp <- VA.temp %>%
  pivot_wider(names_from = Depth, values_from = Temperature)
VA.temp.0.5 <- VA.temp %>%
  select(c(`Depth 0.5`, `Air temperature`))
VA.temp.1.5 <- VA.temp %>%
  select(c(`Depth 1.5`, `Air temperature`))
VA.temp.2.5 <- VA.temp %>%
  select(c(`Depth 2.5`, `Air temperature`))

# VAC vs Air
VAC.temp <- VAC.temp %>%
  merge(Air_temp_VA, by = 'Date', all.x = TRUE) %>%
  select("Depth 0.5" = "VAC.Temp.0.5", "Depth 1.5" = "VAC.Temp.1.5", 
         "Depth 2.5" = "VAC.Temp.2.5", "Air temperature")
VAC.temp.0.5 <- VAC.temp %>%
  select(c(`Depth 0.5`, `Air temperature`))
VAC.temp.1.5 <- VAC.temp %>%
  select(c(`Depth 1.5`, `Air temperature`))
VAC.temp.2.5 <- VAC.temp %>%
  select(c(`Depth 2.5`, `Air temperature`))









# Graph
# Manure and air observations (VA and VAC difference) vs time
# Define a function to create the plot for a given depth
create_comparison_plot <- function(depth_data, shade_data, depth_label) {
  ggplot(data = depth_data, aes(x = Date, y = Temperature, color = Tank)) +
    geom_ribbon(data = shade_data, aes(ymin = 0, ymax = under), color = "grey53", fill = alpha("grey53", 0.6)) +
    geom_ribbon(data = shade_data, aes(ymin = 0, ymax = over), color = "grey53", fill = alpha("grey53", 0.6)) +
    geom_line(lwd = 1.1) +
    labs(x = "Date", y = "Temperature (°C)", title = paste("Manure,", depth_label)) +
    scale_color_manual(breaks = c('Air','No cover', 'Cover', 'Difference'),
                       values = c('Air' = "deepskyblue",'No cover' = "#fcab42", 
                                  'Cover' = "#42bd42", 'Difference' = "grey53")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          legend.title = element_blank(),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.line = element_line(color = "black")) +
    scale_x_date(limits = c(as.Date("2020-06-18"), as.Date("2021-08-01"))) +
    scale_y_continuous(limits = c(-12, 24), 
                       breaks = seq(-12, 24, by = 4),
                       expand = c(0, 0)) +
    geom_hline(yintercept = 0, lwd = 1.1, color = "grey") +
    geom_vline(xintercept = as.numeric(Depth.0.5$Date[date_range]), lwd = 1.1, color = "grey") +
    annotate("text", x = as.Date(18808), y = 1,
             label = "Cover warmer", color = "black", size = 4) +
    annotate("text", x = as.Date(18814), y = -1,
             label = "No cover warmer", color = "black", size = 4)
}

# Define the date range
date_range <- which(Depth.0.5$Date %in% as.Date(c("2020-06-18", "2020-11-10")))

# Create plots for each depth
Comparison.0.5 <- create_comparison_plot(Depth.0.5, VA.VAC.temp.shade.0.5, "0.5 m")
Comparison.1.5 <- create_comparison_plot(Depth.1.5, VA.VAC.temp.shade.1.5, "1.5 m")
Comparison.2.5 <- create_comparison_plot(Depth.2.5, VA.VAC.temp.shade.2.5, "2.5 m")

Comparison.0.5 
Comparison.1.5 
Comparison.2.5

# Graph of difference of air temperature and manure temperature vs time
VA.Air.diff <- ggplot(data = VA.Air.manure.diff.time, aes(x = Date, y = Temperature, color = Depth)) +
  geom_line(lwd = 1) +
  geom_ribbon(data = VA.temp.shade.0.5, aes(ymin = 0, ymax = under), color = "#fcab42", fill = alpha("#fcab42", 0.6)) +
  geom_ribbon(data = VA.temp.shade.1.5, aes(ymin = 0, ymax = under), color = "#42bd42", fill = alpha("#42bd42", 0.6)) +
  geom_ribbon(data = VA.temp.shade.2.5, aes(ymin = 0, ymax = under), color = "grey53", fill = alpha("grey53", 0.6)) +
  geom_ribbon(data = VA.temp.shade.0.5, aes(ymin = 0, ymax = over), color = "#fcab42", fill = alpha("#fcab42", 0.6)) +
  geom_ribbon(data = VA.temp.shade.1.5, aes(ymin = 0, ymax = over), color = "#42bd42", fill = alpha("#42bd42", 0.6)) +
  geom_ribbon(data = VA.temp.shade.2.5, aes(ymin = 0, ymax = over), color = "grey53", fill = alpha("grey53", 0.6)) +
  labs(x = "Date", y = "Temperature (°C)") +
  scale_color_manual(values = c("Difference VA-Air 0.5" = "#fcab42", 
                                "Difference VA-Air 1.5" = "#42bd42", 
                                "Difference VA-Air 2.5" = "grey53"),
                     labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_hline(yintercept = 0, lwd = 1.1, color = "grey") +
  scale_x_date(limits = c(as.Date("2020-06-18"), as.Date("2021-07-31"))) +
  scale_y_continuous(limits = c(-12, 24), 
                     breaks = seq(-12, 24, by = 5),
                     expand = c(0, 0)) +
  annotate("text", x = as.Date(18808), y = 1,
           label = "Cover warmer", color = "black", size = 4) +
  annotate("text", x = as.Date(18802), y = -1,
           label = "Air warmer", color = "black", size = 4)
VA.Air.diff

#Manure Temperature vs Air temperature
VA.manure.to.air.0.5 <- ggplot(data = VA.temp.0.5, 
                               aes(x = `Air temperature`, y = `Depth 0.5`)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  geom_abline(linetype = 2, lwd = 1.1) +
  labs(x = "Temperature of air (°C)", y = "Temperature of manure (°C)") +
  scale_color_manual(labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 24), 
                     breaks = seq(-2, 24, by = 5),
                     expand = c(0, 0))
VA.manure.to.air.0.5

VA.manure.to.air.1.5 <- ggplot(data = VA.temp.1.5, 
                               aes(x = `Air temperature`, y = `Depth 1.5`)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  geom_abline(linetype = 2, lwd = 1.1) +
  labs(x = "Temperature of air (°C)", y = "Temperature of manure (°C)") +
  scale_color_manual(labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 24), 
                     breaks = seq(-2, 24, by = 5),
                     expand = c(0, 0)) 
VA.manure.to.air.1.5

VA.manure.to.air.2.5 <- ggplot(data = VA.temp.2.5, 
                               aes(x = `Air temperature`, y = `Depth 2.5`)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  geom_abline(linetype = 2, lwd = 1.1) +
  labs(x = "Temperature of air (°C)", y = "Temperature of manure (°C)") +
  scale_color_manual(labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 24), 
                     breaks = seq(-2, 24, by = 5),
                     expand = c(0, 0))
VA.manure.to.air.2.5

VAC.manure.to.air.0.5 <- ggplot(data = VAC.temp.0.5, 
                                aes(x = `Air temperature`, `Depth 0.5`)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  geom_abline(linetype = 2, lwd = 1.1) +
  labs(x = "Temperature of air (°C)", y = "Temperature of manure (°C)") +
  scale_color_manual(labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 24), 
                     breaks = seq(-2, 24, by = 5),
                     expand = c(0, 0))
VAC.manure.to.air.0.5

VAC.manure.to.air.1.5 <- ggplot(data = VAC.temp.1.5, 
                                aes(x = `Air temperature`, y = `Depth 1.5`)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  geom_abline(linetype = 2, lwd = 1.1) +
  labs(x = "Temperature of air (°C)", y = "Temperature of manure (°C)") +
  scale_color_manual(labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 24), 
                     breaks = seq(-2, 24, by = 5),
                     expand = c(0, 0)) 
VAC.manure.to.air.1.5

VAC.manure.to.air.2.5 <- ggplot(data = VAC.temp.2.5, 
                                aes(x = `Air temperature`, y = `Depth 2.5`)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  geom_abline(linetype = 2, lwd = 1.1) +
  labs(x = "Temperature of air (°C)", y = "Temperature of manure (°C)") +
  scale_color_manual(labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 24), 
                     breaks = seq(-2, 24, by = 5),
                     expand = c(0, 0))
VAC.manure.to.air.2.5

Manure.to.air.plot <- ggarrange(VA.manure.to.air.0.5, VA.manure.to.air.1.5, VA.manure.to.air.2.5, 
                                VAC.manure.to.air.0.5, VAC.manure.to.air.1.5, VAC.manure.to.air.2.5, 
                                ncol = 3, nrow = 2) 
annotate_figure(Manure.to.air.plot, 
                top = "0.5 m                                                                1.5 m                                                                2.5 m",
                left = "VAC                                                                 VA")




VA.manure.to.air <- ggplot(data = VA.Air.manure, 
                           aes(x = Air.temp, y = Temperature, color = Depth)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  labs(x = "Temperature of air (°C)", y = "Temperature of manure (°C)") +
  scale_color_manual(values = c('VA.Temp.0.5' = "#fcab42", 'VA.Temp.1.5' = "#42bd42", 
                                'VA.Temp.2.5' = "grey53"),
                     labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-12, 40), 
                     breaks = seq(-12, 40, by = 5),
                     expand = c(0, 0)) 
VA.manure.to.air

#Manure - Air Temperature vs Air temperature
VA.diff.to.air <- ggplot(data = VA.Air.manure.diff, 
                         aes(x = Air.temp, y = Temperature, color = Depth)) +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label(c("eq", "R2"))) +
  geom_point() +
  labs(x = "Temperature of air (°C)", y = bquote('Temperature difference '[T[m]-T[a]])) +
  scale_color_manual(values = c('Difference VA-Air 0.5' = "#fcab42", 'Difference VA-Air 1.5' = "#42bd42", 
                                'Difference VA-Air 2.5' = "grey53"),
                     labels = c('Depth 0.5', 'Depth 1.5','Depth 2.5')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-12, 40), 
                     breaks = seq(-12, 40, by = 5),
                     expand = c(0, 0)) 
VA.diff.to.air



# Save graphs
grid.arrange (arrangeGrob(c[[1]], top="CTitle1", left="RTitle1"),
              arrangeGrob(c[[2]],top="CTitle2"),
              arrangeGrob(c[[3]],top="CTittle3"),
              arrangeGrob(c[[4]], left="RTitle2"),c[[5]],c[[6]],
              arrangeGrob(c[[7]],left="RTitle3"),c[[8]],c[[9]],
              ncol=3, nrow=3, widths = c(4,4,4), heights = c(4,4,4))


Manure.to.air.plot.list <- list(VA.manure.to.air.0.5, VA.manure.to.air.1.5, 
                                VA.manure.to.air.2.5, VAC.manure.to.air.0.5, 
                                VAC.manure.to.air.1.5, VAC.manure.to.air.2.5)

N <- length(Manure.to.air.plot.list)
nr <- 3
nc <- 3


combine <- rbind(tableGrob(t(c("VAC", "VA")), theme = theme_minimal(), rows = ""), 
                 cbind(tableGrob(t(c("0.5 m", "1.5 m", "2.5 m"), theme = theme_minimal()), 
                                 arrangeGrob(grobs = Manure.to.air.plot.list),  size = "last"), size = "last"))
grid.newpage()
grid.draw(combine)

set.seed()
pl <- replicate(6, VA.manure.to.air.0.5, FALSE)
pl <- list(VA.manure.to.air.0.5, VA.manure.to.air.1.5, 
           VA.manure.to.air.2.5, VAC.manure.to.air.0.5, 
           VAC.manure.to.air.1.5, VAC.manure.to.air.2.5)
N <- length(pl)
nr <- 3
nc <- 3


combine <- rbind(tableGrob(t(c("VAC", "VA", "LA")), rows = ""), 
                 cbind(tableGrob(LETTERS[1:nr]), 
                       arrangeGrob(grobs = pl),  size = "last"), size = "last")

grid.newpage()
grid.draw(combine)




# Create list of plots
Manure.to.air.plot.list <- list(VA.manure.to.air.0.5, VA.manure.to.air.1.5, 
                                VA.manure.to.air.2.5, VAC.manure.to.air.0.5, 
                                VAC.manure.to.air.1.5, VAC.manure.to.air.2.5)

# Create row and column titles
col.titles = c("0.5 m", "1.5 m", "2.5 m")
row.titles = c("VAC", "VA")

# Add row titles
Manure.to.air.plot[1:3] = lapply(1:3, function(i) arrangeGrob(Manure.to.air.plot[[i]], left=row.titles[i]))

# Add column titles and lay out plots
grid.arrange(grobs=lapply(c(1,4,7), function(i) {
  arrangeGrob(grobs=Manure.to.air.plot[i:(i+2)], top=col.titles[i/3 + 1], ncol=1)
}), ncol=3)




ggsave("Results/Temperature_observations_Tm_Ta_VA_VAC.png", Manure.to.air.plot, 
       width = 30, height = 24, units = "cm", dpi = 300)

# VA and VAC comparison
ggsave("Results/Temperature_observations_VA_VAC.png", 
       ggarrange(Comparison.0.5, Comparison.1.5, Comparison.2.5, ncol = 1, nrow = 3,
                 common.legend = TRUE, legend = "bottom"), 
       width = 20, height = 24, units = "cm", dpi = 300)

# VA
ggsave("Results/Graph_observations_VA.png", 
       ggarrange(VA.manure.to.air, VA.Air.diff, VA.diff.to.air, 
                 ncol = 1, nrow = 3,
                 common.legend = TRUE, legend = "bottom"), 
       width = 24, height = 38, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_air_VA.png", VA.manure.to.air,
       width = 15, height = 10, units = "cm", dpi = 300)
ggsave("Results/Temperature_difference_time_VA.png", VA.Air.diff,
       width = 15, height = 10, units = "cm", dpi = 300)
ggsave("Results/Temperature_difference_air_VA.png", VA.diff.to.air,
       width = 15, height = 10, units = "cm", dpi = 300)

# VAC
ggsave("Results/Graph_observations_VA.png", 
       ggarrange(VA.manure.to.air, VA.Air.diff, VA.diff.to.air, 
                 ncol = 1, nrow = 3,
                 common.legend = TRUE, legend = "bottom"), 
       width = 24, height = 38, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_air_VA.png", VA.manure.to.air,
       width = 15, height = 10, units = "cm", dpi = 300)
ggsave("Results/Temperature_difference_time_VA.png", VA.Air.diff,
       width = 15, height = 10, units = "cm", dpi = 300)
ggsave("Results/Temperature_difference_air_VA.png", VA.diff.to.air,
       width = 15, height = 10, units = "cm", dpi = 300)



# Save data
Comparison.temp <- Comparison.temp %>%
  pivot_wider(names_from = Tank, values_from = Temperature)
write.csv(Comparison.temp,"Results/Temperature observations and differences for VA and VAC.csv",row.names = FALSE)

# Statistics
# Function to perform paired t-test and extract results
perform_paired_t_test <- function(data, depth) {
  data_wide <- data %>%
    pivot_wider(names_from = Tank, values_from = Temperature)
  
  test_result <- t.test(data_wide$`No cover`, data_wide$Cover,
                        paired = TRUE, conf.level = 0.95, alternative = "less")
  
  # Extract relevant statistics and format them
  tidy(test_result) %>%
    mutate(Depth = depth)
}

# Apply the function to each dataset and combine results
stat.results <- bind_rows(
  perform_paired_t_test(Depth.0.5.c, "0.5 - Cover"),
  perform_paired_t_test(Depth.1.5.c, "1.5 - Cover"),
  perform_paired_t_test(Depth.2.5.c, "2.5 - Cover"),
  perform_paired_t_test(Depth.0.5.n, "0.5 - No cover"),
  perform_paired_t_test(Depth.1.5.n, "1.5 - No cover"),
  perform_paired_t_test(Depth.2.5.n, "2.5 - No cover")
)
colnames(stat.results) <- c("Mean difference", "t value", "p value", "Sample size",
                            "Confidence low", "Confidence high", "Method", 
                            "Alternative", "Depth")

# Anova test
## Fit linear model and perform anova for temperature difference between air and manure
VA.air.manure <- VA.air.depth %>%
  filter(str_detect(Depth, "No cover"))
VA <- lm(Temperature ~ Depth, data=VA.air.manure)
anova(VA)

VAC.air.manure <- VA.air.depth %>%
  filter(str_detect(Depth, "Cover"))
VAC <- lm(Temperature ~ Depth, data=VAC.air.manure)
anova(VAC)

## don't forget diagnostics
par(mfrow=c(2, 2))
plot(m)

# Export statistic results
write.csv(stat.results,"Results/Statistics for observations.csv",row.names = FALSE)








VA.depth <- VA %>%
  mutate(Date = paste(Year,"-",Month,"-",Day, sep = "")) %>%
  select(Date, "VA.depth" = Depth)
VA.depth$Date <- as.Date(VA.depth$Date)

VAC.depth <- VAC %>%
  mutate(Date = paste(Year,"-",Month,"-",Day, sep = "")) %>%
  select(Date, "VAC.depth" = Depth)
VAC.depth$Date <- as.Date(VAC.depth$Date)

VA.VAC.depth <- merge(VA.depth, VAC.depth, by = 'Date', all.x = TRUE)


# Add air temperature to the VA data 
Air_temp_VA <- Air_temp_VA_data %>%
  select(c(Date = DATUM, "Air temperature" = XTEMP)) %>%
  mutate(Date = strptime(Date, "%Y%m%d", tz = "UCT")) %>%
  mutate(Date = as.Date(Date, "%Y%m%d"))

# Wide dataframe with all temperatures
VA.VAC.depth <- VA.VAC.depth %>%
  merge(Air_temp_VA, by = 'Date', all.x = TRUE) %>%
  pivot_longer(cols = c("VA.depth", "VAC.depth"),
               names_to = 'Tank',
               values_to = 'Depth') %>%
  arrange(factor(Tank, levels = c("VA.depth", "VAC.depth")))

  

# Graph of depths in VA and VAC
depth.graph <- ggplot(data = VA.VAC.depth, aes(x = Date, y = Depth, color = Tank)) +
  geom_line(lwd = 1.1) +
  labs(x = "Date", y = "Depth (m)") +
  scale_color_manual(breaks = c('Air','No cover', 'Cover', 'Difference'),
                     values = c('VA.depth' = "deepskyblue",'VAC.depth' = "#fcab42")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 3), 
                     breaks = seq(0, 3, by = 0.5), 
                     expand = c(0, 0))
depth.graph

# Save graph
ggsave("Results/Depth_observations_VA_VAC.png", depth.graph, 
       width = 20, height = 12, units = "cm", dpi = 300)
