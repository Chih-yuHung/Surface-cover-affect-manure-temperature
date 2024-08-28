library(tidyverse); library(zoo); library(ggpmisc); library(ggpubr); library(scales)

# Air temperature
Air_temp_VA_data <- read_csv("Input/VA/Air temp_VA.csv")
Air_temp_VA <- Air_temp_VA_data %>%
  select(c(Date = DATUM, "Air temperature" = XTEMP)) %>%
  mutate(Date = strptime(Date, "%Y%m%d", tz = "UCT")) %>%
  mutate(Date = as.Date(Date, "%Y%m%d"))
Air_temp_LA_data <- read_csv("Input/LA/Air temp_LA.csv")
Air_temp_LA <- Air_temp_LA_data %>%
  select(c(Date = DATUM, "Air temperature" = XTEMP)) %>%
  mutate(Date = strptime(Date, "%Y%m%d", tz = "UCT")) %>%
  mutate(Date = as.Date(Date, "%Y%m%d"))

# Manure temperature for VA, VAC
process_temp_data <- function(file_path, air_temp_data) {
  read_csv(file_path) %>%
    mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
    select(Date, Temp.0.5 = temp0.5, Temp.1.5 = temp1.5, Temp.2.5 = temp2.5) %>%
    merge(air_temp_data, by = 'Date', all.x = TRUE) %>%
    mutate(Air = `Air temperature`) %>%
    group_by(Year = year(Date), Week = week(Date)) %>%
    summarise(
      Date = Date,
      Weekly.average.0.5 = mean(Temp.0.5, na.rm = TRUE),
      Weekly.average.1.5 = mean(Temp.1.5, na.rm = TRUE),
      Weekly.average.2.5 = mean(Temp.2.5, na.rm = TRUE),
      Air = mean(Air, na.rm = TRUE)
    ) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Year = as.character(Year))
}

# Process VA and VAC temperature data
VA.temp <- process_temp_data("Input/VA/Manure temp_VA.csv", Air_temp_VA)
VAC.temp <- process_temp_data("Input/VAC/Manure temp_VAC.csv", Air_temp_VA)

# Manure temperature for LA
LA.temp <- read_csv("Input/LA/Manure temp_LA.csv") %>%
  separate(Time, into = c("Month", "Day", "Year"),sep = "/") %>%
  separate(Year, into = c("Year", "Time"),sep = " ") %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  select(Date, Temp.0.5 = Temp0.5, Temp.1.5 = Temp1.5, Temp.2.5 = Temp2.5) %>%
  merge(Air_temp_LA, by = 'Date', all.x = TRUE) %>%
  mutate(Air = `Air temperature`) %>%
  group_by(Year = year(Date), Week = week(Date)) %>%
  summarise(
    Date = Date,
    Weekly.average.0.5 = mean(Temp.0.5, na.rm = TRUE),
    Weekly.average.1.5 = mean(Temp.1.5, na.rm = TRUE),
    Weekly.average.2.5 = mean(Temp.2.5, na.rm = TRUE),
    Air = mean(Air, na.rm = TRUE)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Year = as.character(Year))

# Separate cooling and warming periods
process_weekly_data <- function(data, avg_col, depth, cooling, warming) {
  data %>%
    select(Date, Year, Week, avg_col, Air) %>%
    mutate(Cooling = ifelse(Date %within% cooling, !!sym(avg_col), NA)) %>%
    mutate(Warming = ifelse(Date %within% warming, !!sym(avg_col), NA)) %>%
    pivot_longer(cols = c(Warming, Cooling), names_to = 'Trend', values_to = 'Temperature') %>%
    arrange(factor(Trend, levels = c("Cooling", "Warming"))) %>%
    drop_na()
}

# Process the data for different depths
VA.weekly.0.5 <- process_weekly_data(VA.temp, "Weekly.average.0.5", "0.5", interval("2020-06-18", "2021-02-19"), interval("2021-02-19", "2021-05-28"))
VA.weekly.1.5 <- process_weekly_data(VA.temp, "Weekly.average.1.5", "1.5", interval("2020-06-18", "2021-02-12"), interval("2021-02-12", "2021-05-28"))
VA.weekly.2.5 <- process_weekly_data(VA.temp, "Weekly.average.2.5", "2.5", interval("2020-06-18", "2021-03-12"), interval("2021-03-12", "2021-05-28"))

VAC.weekly.0.5 <- process_weekly_data(VAC.temp, "Weekly.average.0.5", "0.5", interval("2020-06-18", "2021-02-19"), interval("2021-02-19", "2021-05-28"))
VAC.weekly.1.5 <- process_weekly_data(VAC.temp, "Weekly.average.1.5", "1.5", interval("2020-06-18", "2021-02-19"), interval("2021-02-19", "2021-05-28"))
VAC.weekly.2.5 <- process_weekly_data(VAC.temp, "Weekly.average.2.5", "2.5", interval("2020-06-18", "2021-02-19"), interval("2021-02-19", "2021-05-28"))

LA.weekly.0.5 <- process_weekly_data(LA.temp, "Weekly.average.0.5", "0.5", interval("2020-09-11", "2021-03-05"), interval("2021-03-05", "2021-08-27"))
LA.weekly.1.5 <- process_weekly_data(LA.temp, "Weekly.average.1.5", "1.5", interval("2020-09-11", "2021-03-05"), interval("2021-03-05", "2021-07-02"))
LA.weekly.2.5 <- process_weekly_data(LA.temp, "Weekly.average.2.5", "2.5", interval("2020-09-11", "2021-03-05"), interval("2021-03-05", "2021-07-02"))

VAC.weekly.0.5 <- VAC.weekly.0.5 %>%
  filter(Date %within% interval("2020-06-18", "2020-11-10"))
VAC.weekly.1.5 <- VAC.weekly.1.5 %>%
  filter(Date %within% interval("2020-06-18", "2020-11-10"))
VAC.weekly.2.5 <- VAC.weekly.2.5 %>%
  filter(Date %within% interval("2020-06-18", "2020-11-10"))


# Graphs
# Average manure temperature vs time
# Define a function to create the plots
create_weekly_plot <- function(data, y_col, title) {
  ggplot(data = data, aes(x = Date, y = !!sym(y_col), color = Trend)) +
    geom_point() +
    labs(x = "Week", y = "Average temperature of manure (°C)") +
    scale_color_manual(values = c(Cooling = "lightblue", Warming = "lightcoral")) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0, size = 18, face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10, colour = "black"),
      axis.text = element_text(size = 12, colour = "black"),
      axis.text.x = element_text(angle = 90, size = 12, colour = "black"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.line = element_line(color = "black")
    ) +
    scale_x_date(date_breaks = "2 week", labels = date_format("%W")) +
    scale_y_continuous(limits = c(-2, 20), breaks = seq(-2, 20, by = 5), expand = c(0, 0)) +
    facet_grid(~ fct(Year), scales = "free", space = "free", switch = "x") +
    ggtitle(title)
}

# Create the plots using the function
VA.graph.weekly.0.5 <- create_weekly_plot(VA.weekly.0.5, "Weekly.average.0.5", "VA Graph Weekly 0.5")
VA.graph.weekly.1.5 <- create_weekly_plot(VA.weekly.1.5, "Weekly.average.1.5", "VA Graph Weekly 1.5")
VA.graph.weekly.2.5 <- create_weekly_plot(VA.weekly.2.5, "Weekly.average.2.5", "VA Graph Weekly 2.5")

VAC.graph.weekly.0.5 <- create_weekly_plot(VAC.weekly.0.5, "Weekly.average.0.5", "VAC Graph Weekly 0.5")
VAC.graph.weekly.1.5 <- create_weekly_plot(VAC.weekly.1.5, "Weekly.average.1.5", "VAC Graph Weekly 1.5")
VAC.graph.weekly.2.5 <- create_weekly_plot(VAC.weekly.2.5, "Weekly.average.2.5", "VAC Graph Weekly 2.5")

LA.graph.weekly.0.5 <- create_weekly_plot(LA.weekly.0.5, "Weekly.average.0.5", "LA Graph Weekly 0.5")
LA.graph.weekly.1.5 <- create_weekly_plot(LA.weekly.1.5, "Weekly.average.1.5", "LA Graph Weekly 1.5")
LA.graph.weekly.2.5 <- create_weekly_plot(LA.weekly.2.5, "Weekly.average.2.5", "LA Graph Weekly 2.5")

VA.graph.weekly.0.5
VA.graph.weekly.1.5
VA.graph.weekly.2.5

VAC.graph.weekly.0.5
VAC.graph.weekly.1.5
VAC.graph.weekly.2.5

LA.graph.weekly.0.5
LA.graph.weekly.1.5
LA.graph.weekly.2.5

# Average manure temperature vs air temperature
create_plot <- function(data, depth, trend, color_values) {
  ggplot(data %>% filter(Trend == trend), 
         aes(x = Air, y = get(paste0("Weekly.average.", depth)),
             color = Trend)) +
    geom_point() +
    geom_smooth(method = lm, formula = 'y ~ x', se = TRUE, fullrange = TRUE, color = "blue") +
    geom_abline(linetype = 2, lwd = 1.1, color = "grey") +
    geom_hline(yintercept = 0, lwd = 1.1, color = "grey") +
    stat_poly_eq(formula = y ~ x, 
                 color = "black",
                 use_label("eq"),
                 label.y = c(0.95, 0.5)) +
    stat_poly_eq(formula = y ~ x, 
                 color = "black",
                 use_label("R2"),
                 label.y = c(0.9, 0.5)) +
    stat_poly_eq(formula = y ~ x, 
                 color = "black",
                 use_label("f"),
                 label.y = c(0.85, 0.5)) +
    stat_poly_eq(formula = y ~ x, 
                 color = "black",
                 use_label("p"),
                 label.y = c(0.8, 0.5)) +
    labs(x = "Average temperature of air (°C)", y = "Average temperature of manure (°C)") +
    scale_color_manual(values = color_values) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),
          legend.position = "none",
          axis.text = element_text(size = 12, colour = "black"),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.line = element_line(color = "black")) +
    scale_x_continuous(limits = c(-12, 24), 
                       breaks = seq(-12, 24, by = 4),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(-2, 20), 
                       breaks = seq(-2, 20, by = 4),
                       expand = c(0, 0)) +
    facet_grid(~ fct(trend), scales = "free", space = "free") +
    theme(strip.text = element_text(size = 14, color = "black", face = "bold"))
}

# Define color values for Cooling and Warming
color_values <- c(Cooling = "lightblue", Warming = "lightcoral")

# Create plots for VA data
VA.graph.cooling.0.5 <- create_plot(VA.weekly.0.5, "0.5", "Cooling", color_values)
VA.graph.warming.0.5 <- create_plot(VA.weekly.0.5, "0.5", "Warming", color_values)
VA.graph.cooling.1.5 <- create_plot(VA.weekly.1.5, "1.5", "Cooling", color_values)
VA.graph.warming.1.5 <- create_plot(VA.weekly.1.5, "1.5", "Warming", color_values)
VA.graph.cooling.2.5 <- create_plot(VA.weekly.2.5, "2.5", "Cooling", color_values)
VA.graph.warming.2.5 <- create_plot(VA.weekly.2.5, "2.5", "Warming", color_values)

VA.graph.cooling.0.5
VA.graph.warming.0.5
VA.graph.cooling.1.5
VA.graph.warming.1.5
VA.graph.cooling.2.5
VA.graph.warming.2.5

# Create plots for VAC data
VAC.graph.cooling.0.5 <- create_plot(VAC.weekly.0.5, "0.5", "Cooling", color_values)
VAC.graph.warming.0.5 <- create_plot(VAC.weekly.0.5, "0.5", "Warming", color_values)
VAC.graph.cooling.1.5 <- create_plot(VAC.weekly.1.5, "1.5", "Cooling", color_values)
VAC.graph.warming.1.5 <- create_plot(VAC.weekly.1.5, "1.5", "Warming", color_values)
VAC.graph.cooling.2.5 <- create_plot(VAC.weekly.2.5, "2.5", "Cooling", color_values)
VAC.graph.warming.2.5 <- create_plot(VAC.weekly.2.5, "2.5", "Warming", color_values)

VAC.graph.cooling.0.5
VAC.graph.warming.0.5
VAC.graph.cooling.1.5
VAC.graph.warming.1.5
VAC.graph.cooling.2.5
VAC.graph.warming.2.5

# Create plots for LA data
LA.graph.cooling.0.5 <- create_plot(LA.weekly.0.5, "0.5", "Cooling", color_values)
LA.graph.warming.0.5 <- create_plot(LA.weekly.0.5, "0.5", "Warming", color_values)
LA.graph.cooling.1.5 <- create_plot(LA.weekly.1.5, "1.5", "Cooling", color_values)
LA.graph.warming.1.5 <- create_plot(LA.weekly.1.5, "1.5", "Warming", color_values)
LA.graph.cooling.2.5 <- create_plot(LA.weekly.2.5, "2.5", "Cooling", color_values)
LA.graph.warming.2.5 <- create_plot(LA.weekly.2.5, "2.5", "Warming", color_values)

LA.graph.cooling.0.5
LA.graph.warming.0.5
LA.graph.cooling.1.5
LA.graph.warming.1.5
LA.graph.cooling.2.5
LA.graph.warming.2.5



# Save graphs
ggsave("Results/Temperature_observations_VA_0_5.png", 
       ggarrange(VA.graph.weekly.0.5, ggarrange(VA.graph.cooling.0.5, VA.graph.warming.0.5, 
                                                ncol = 2, nrow = 1), 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 20, height = 24, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_VA_1_5.png", 
       ggarrange(VA.graph.weekly.1.5, ggarrange(VA.graph.cooling.1.5, VA.graph.warming.1.5, 
                                                ncol = 2, nrow = 1), 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 20, height = 24, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_VA_2_5.png", 
       ggarrange(VA.graph.weekly.1.5, ggarrange(VA.graph.cooling.2.5, VA.graph.warming.2.5, 
                                                ncol = 2, nrow = 1), 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 20, height = 24, units = "cm", dpi = 300)



ggsave("Results/Temperature_observations_VAC_0_5.png", 
       ggarrange(VAC.graph.weekly.0.5, VAC.graph.cooling.0.5, 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 15, height = 24, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_VAC_1_5.png", 
       ggarrange(VAC.graph.weekly.1.5, VAC.graph.cooling.1.5, 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 15, height = 24, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_VAC_2_5.png", 
       ggarrange(VAC.graph.weekly.2.5, VAC.graph.cooling.2.5, 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 15, height = 24, units = "cm", dpi = 300)



ggsave("Results/Temperature_observations_LA_0_5.png", 
       ggarrange(LA.graph.weekly.0.5, ggarrange(LA.graph.cooling.0.5, LA.graph.warming.0.5, 
                                                ncol = 2, nrow = 1), 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 20, height = 24, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_LA_1_5.png", 
       ggarrange(LA.graph.weekly.1.5, ggarrange(LA.graph.cooling.1.5, LA.graph.warming.1.5, 
                                                ncol = 2, nrow = 1), 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 20, height = 24, units = "cm", dpi = 300)
ggsave("Results/Temperature_observations_LA_2_5.png", 
       ggarrange(LA.graph.weekly.1.5, ggarrange(LA.graph.cooling.2.5, LA.graph.warming.2.5, 
                                                ncol = 2, nrow = 1), 
                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 20, height = 24, units = "cm", dpi = 300)



ggsave("Results/Temperature_observations_VAC_all.png", 
       ggarrange(VAC.graph.weekly.0.5, VAC.graph.weekly.1.5, VAC.graph.weekly.2.5,
                 VAC.graph.cooling.0.5, VAC.graph.cooling.1.5, VAC.graph.cooling.2.5,
                 ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom"), 
       width = 30, height = 24, units = "cm", dpi = 300)

ggsave("Results/Temperature_observations_VA_all.png", 
       ggarrange(ggarrange(VA.graph.weekly.0.5, ggarrange(VA.graph.cooling.0.5, VA.graph.warming.0.5, 
                                                          ncol = 2, nrow = 1), 
                           ncol = 1, nrow = 2, common.legend = FALSE, legend = "bottom"), 
                 ggarrange(VA.graph.weekly.1.5, ggarrange(VA.graph.cooling.1.5, VA.graph.warming.1.5, 
                                                          ncol = 2, nrow = 1), 
                           ncol = 1, nrow = 2, common.legend = FALSE, legend = "bottom"), 
                 ggarrange(VA.graph.weekly.2.5, ggarrange(VA.graph.cooling.2.5, VA.graph.warming.2.5, 
                                                          ncol = 2, nrow = 1), 
                           ncol = 1, nrow = 2, common.legend = FALSE, legend = "bottom"), 
                 ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom"), 
       width = 45, height = 24, units = "cm", dpi = 300)

ggsave("Results/Temperature_observations_LA_all.png", 
       ggarrange(ggarrange(LA.graph.weekly.0.5, ggarrange(LA.graph.cooling.0.5, LA.graph.warming.0.5, 
                                                          ncol = 2, nrow = 1), 
                           ncol = 1, nrow = 2, common.legend = FALSE, legend = "bottom"), 
                 ggarrange(LA.graph.weekly.1.5, ggarrange(LA.graph.cooling.1.5, LA.graph.warming.1.5, 
                                                          ncol = 2, nrow = 1), 
                           ncol = 1, nrow = 2, common.legend = FALSE, legend = "bottom"), 
                 ggarrange(LA.graph.weekly.2.5, ggarrange(LA.graph.cooling.2.5, LA.graph.warming.2.5, 
                                                          ncol = 2, nrow = 1), 
                           ncol = 1, nrow = 2, common.legend = FALSE, legend = "bottom"), 
                 ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom"), 
       width = 45, height = 24, units = "cm", dpi = 300)
