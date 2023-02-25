# Non-Fiction and Fiction Checkouts In The Past Six Years

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# Data frame (At least 10 checkouts a month in the past six years)
spl_10_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Add date to data frames
spl_10_df <- spl_10_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_10_df$date <- as.Date(spl_10_df$date, format = "%Y-%m-%d")

# Number of non-fiction (nf) checkouts each month in the past six years
nf_checkouts <- spl_10_df %>% 
  filter(str_detect(Subjects, "Nonfiction")) %>% 
  group_by(date) %>% 
  summarise(nonfiction_checkouts = sum(Checkouts, na.rm = TRUE))

# Number of fiction (f) checkouts each month in the past six years
f_checkouts <- spl_10_df %>% 
  filter(str_detect(Subjects, "Fiction")) %>% 
  group_by(date) %>% 
  summarise(fiction_checkouts = sum(Checkouts, na.rm = TRUE))

# Line plot
ggplot() + 
  geom_line(data = nf_checkouts, 
            aes(x = date, y = nonfiction_checkouts, color = "Non-fiction")) + 
  geom_line(data = f_checkouts, 
            aes(x = date, y = fiction_checkouts, color = "Fiction")) + 
  scale_color_manual(values=c("#0000ff", "#ff0000")) + 
  scale_y_continuous(labels = label_number_si()) + 
  labs(title = "Non-fiction And Fiction Checkouts In The Past Six Years",
       x = "Year",
       y = "Number of Checkouts",
       color = "Genre")
