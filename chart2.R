# eBook, Book, Audio book, And Magazine Checkouts Over The Past Six Years

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

# Number of eBook checkouts
num_ebooks <- spl_10_df %>% 
  filter(str_detect(MaterialType, "EBOOK")) %>% 
  group_by(date) %>% 
  summarise(ebooks = sum(Checkouts, na.rm = TRUE))

# Number of Book checkouts
num_books <- spl_10_df %>% 
  filter(str_detect(MaterialType, "BOOK")) %>% 
  group_by(date) %>% 
  summarise(books = sum(Checkouts, na.rm = TRUE))

# Number of Audio Book checkouts
num_audiobooks <- spl_10_df %>% 
  filter(str_detect(MaterialType, "AUDIOBOOK")) %>% 
  group_by(date) %>% 
  summarise(audiobooks = sum(Checkouts, na.rm = TRUE))

# Number of Magazine checkouts
num_magazines <- spl_10_df %>% 
  filter(str_detect(MaterialType, "MAGAZINE")) %>% 
  group_by(date) %>% 
  summarise(magazines = sum(Checkouts, na.rm = TRUE))

# Line plot
ggplot() + 
  geom_line(data = num_audiobooks, 
            aes(x = date, y = audiobooks, color = "Audiobooks")) + 
  geom_line(data = num_books, 
            aes(x = date, y = books, color = "Books")) + 
  geom_line(data = num_ebooks, 
            aes(x = date, y = ebooks, color = "eBooks")) + 
  geom_line(data = num_magazines, 
            aes(x = date, y = magazines, color = "Magazines")) + 
  scale_color_manual(values=c("#0000ff", "#ff0000", "#008000", "#a020f0")) + 
  scale_y_continuous(labels = label_number_si()) + 
  labs(title = "Different Type of Publication Checkouts",
       x = "Year",
       y = "Number of Checkouts",
       color = "Type")
