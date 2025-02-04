library(ggplot2)
library(gtools)
library(dplyr)
library(gridExtra)

setwd("/Users/ainefernandez/documents/F1DataScience")

#Data 
datapaper<-read.csv("PaperFullData2.csv")
table(datapaper$GapToKnockoutQ1,datapaper$MadeItToQ2)
year_of_interest <- 2020

# Subset the data for the specific year and select the GP column
gps_for_year <- datapaper[datapaper$Year == year_of_interest, "GP"]

# Get unique GP names
unique_gps_for_year <- unique(gps_for_year)

# Print unique GP names
print(unique_gps_for_year)

datapaper <- datapaper[!(datapaper$Year == 2023 & datapaper$GP == "Imola"), ]
datapaper <- datapaper[!(datapaper$Year == 2023 & datapaper$GP == "China"), ]
datapaper <- datapaper[!(datapaper$Year == 2022 & datapaper$GP == "China"), ]
datapaper <- datapaper[!(datapaper$Year == 2022 & datapaper$GP == "Qatar"), ]
datapaper <- datapaper[!(datapaper$Year == 2020 & datapaper$GP == "Canada"), ]



write.csv(datapaper, "PaperFullData.csv", row.names = FALSE)






# Create the scatter plot with local polynomial smoothing

# Load the necessary library
library(ggplot2)
min(datapaper$GapToKnockoutQ2, na.rm = TRUE)
# Create the scatter plot
ggplot(datapaper, aes(x = GapToKnockoutQ1, y =PositionDifference )) +
  geom_point(size = 3) +  # Plot points without color by team
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +  # Add vertical line at x = 0
  labs(title = "Race Position vs Gap to Knockout in Qualifying",
       x = "Gap to Knockout in Qualifying (seconds)",
       y = "Race Position") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_x_continuous(limits = c(-3, 2), breaks = seq(-3, 2, by = 0.5))  # Limit x-axis and set ticks



# Assuming gap_to_knockout is in seconds
h <- 0.05  # Example binwidth of 0.01 seconds (10 milliseconds)

# Define parameters based on your choice
cutoff <- 0  # The cutoff point is 0
K0 <- 5 # Number of bins to the left of the cutoff
K1 <- 5  # Number of bins to the right of the cutoff

# Define bin edges around the cutoff
bin_edges <- seq(cutoff - K0 * h, cutoff + K1 * h, by = h)
# Initialize vectors to store the bin midpoints and the average outcomes
bin_midpoints <- numeric(length(bin_edges) - 1)
average_outcome <- numeric(length(bin_edges) - 1)

# Calculate the number of observations and the average outcome for each bin
for (k in 1:(length(bin_edges) - 1)) {
  in_bin <- datapaper$GapToKnockoutQ2 > bin_edges[k] & datapaper$GapToKnockoutQ2 <= bin_edges[k + 1]
  bin_midpoints[k] <- (bin_edges[k] + bin_edges[k + 1]) / 2
  average_outcome[k] <- mean(datapaper$RacePosition[in_bin], na.rm = TRUE)
}

# Plot the data
ggplot(data.frame(bin_midpoints, average_outcome), aes(x = bin_midpoints, y = average_outcome)) +
  geom_point(size = 3) + 
  geom_line() + 
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") + 
  labs(title = "Outcomes by Gap to Knockout",
       x = "Gap to Knockout (Seconds)",
       y = "Average Final Points") +
  theme_minimal()


# Load necessary libraries
library(ggplot2)
library(dplyr)


datapaper <- datapaper[!is.na(datapaper$GapToKnockoutQ2) & 
                         is.finite(datapaper$GapToKnockoutQ2), ]

# Assuming your data is in a data frame named `data` and `GapToKnockoutQ2` is the column name
bin_width <- 0.5 # Choose an appropriate bin width
bins <- seq(min(datapaper$GapToKnockoutQ2), max(datapaper$GapToKnockoutQ2), by = bin_width)

# Create a new column with the midpoints of the bins
datapaper$bin_midpoints <- cut(datapaper$GapToKnockoutQ2, breaks = bins, labels = FALSE)
datapaper$bin_midpoints <- bins[datapaper$bin_midpoints] + bin_width/2

# Calculate the number of observations in each bin
bin_counts <- aggregate(datapaper$GapToKnockoutQ2, by = list(datapaper$bin_midpoints), FUN = length)
colnames(bin_counts) <- c("bin_midpoints", "count")

ggplot(bin_counts, aes(x = bin_midpoints, y = count)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Gap to Knockout (Q2) Midpoints", y = "Number of Observations",
       title = "Distribution of Gap to Knockout (Q2) Bins") +
  theme_minimal()

library(rddensity)
density_test <- rddensity(X = datapaper$GapToKnockoutQ1, c = 0)

# Print the results
summary(density_test)

rdplotdensity(density_test, datapaper$GapToKnockoutQ1)

library(ggplot2)



# Filter the data for GapToKnockoutQ1 within the range of -10 to 10
filtered_data_q1 <- datapaper %>%
  filter(abs(GapToKnockoutQ1) <= 5)
table(filtered_data_q1$PositionQ1,filtered_data_q1$MadeItToQ2)

ggplot(filtered_data_q1, aes(x = GapToKnockoutQ1, y = MadeItToQ2)) +
  geom_point(size = 3,color="skyblue") +  # Add transparency to points to avoid overlap
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Cutoff line
  scale_x_continuous(breaks = seq(-5, 5, by = .5)) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Custom labels for y-axis
  labs(title = "Q1: Gap to Knockout vs Qualification",
       x = "Gap to Knockout (Q1)",
       y = "Made it to Q2") +
  theme_bw()+
  theme(plot.title=element_text(size = 18, hjust = 0.5))

# Filter the data for GapToKnockoutQ2 within the range of -10 to 10
filtered_data_q2 <- datapaper %>%
  filter(abs(GapToKnockoutQ2) <= 5)

# Scatter plot for Q2
ggplot(filtered_data_q2, aes(x = GapToKnockoutQ2, y = MadeItToQ3)) +
  geom_point(size = 3,color="skyblue") +  # Add transparency to points to avoid overlap
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Cutoff line
  scale_x_continuous(breaks = seq(-5, 5, by = .5)) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Custom labels for y-axis
  labs(title = "Q2: Gap to Knockout vs Qualification",
       x = "Gap to Knockout (Q2)",
       y = "Made it to Q3") +
  theme_bw()+
  theme(plot.title=element_text(size = 18, hjust = 0.5))




# Create the first plot
plot1 <- ggplot(filtered_data_q1, aes(x = GapToKnockoutQ1, y = MadeItToQ2)) +
  geom_point(size = 3, color = "skyblue") +  # Add transparency to points to avoid overlap
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Cutoff line
  scale_x_continuous(breaks = seq(-5, 5, by = .5)) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Custom labels for y-axis
  labs(title = "Q1: Gap to Knockout vs Qualification",
       x = "Gap to Knockout (Q1)",
       y = "Made it to Q2") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

# Create the second plot
plot2 <- ggplot(filtered_data_q2, aes(x = GapToKnockoutQ2, y = MadeItToQ3)) +
  geom_point(size = 3, color = "skyblue") +  # Add transparency to points to avoid overlap
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Cutoff line
  scale_x_continuous(breaks = seq(-5, 5, by = .5)) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Custom labels for y-axis
  labs(title = "Q2: Gap to Knockout vs Qualification",
       x = "Gap to Knockout (Q2)",
       y = "Made it to Q3") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

grid.arrange(plot1, plot2, ncol = 2)



