# Most popular genres that emerged from the pandemic (by month)
# Non-Fiction vs Fiction

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Data frames
spl_10_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Add to data frames
spl_10_df <- spl_10_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_10_df$date <- as.Date(spl_10_df$date, format = "%Y-%m-%d")

# Variables
nf_checkouts <- spl_10_df %>% filter(str_detect(Subjects, "Nonfiction")) %>% group_by(date) %>% summarise(nonfiction_checkouts = sum(Checkouts, na.rm = TRUE))

f_checkouts <- spl_10_df %>% filter(str_detect(Subjects, "Fiction")) %>% group_by(date) %>% summarise(fiction_checkouts = sum(Checkouts, na.rm = TRUE))

# Plot 
ggplot() + 
  geom_line(data = nf_checkouts, aes(x = date, y = nonfiction_checkouts, color = "Non-fiction")) + 
  geom_line(data = f_checkouts, aes(x = date, y = fiction_checkouts, color = "Fiction")) + 
  scale_color_manual(values=c("#0000ff", "#ff0000")) + 
  labs(title = "Non-fiction vs Fiction Checkouts 2017-2023",
       x = "Date",
       y = "Number of checkouts",
       color = "Legend")
