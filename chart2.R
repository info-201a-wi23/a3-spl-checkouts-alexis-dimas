# Look at trends for
# Material.Type
# EBOOK, BOOK, AUDIOBOOK, MAGAZINE

# I feel like the pandemic changed things. Before my textbooks were hard copies but now all I look for is digital.

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

spl_10_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_10_df <- spl_10_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_10_df$date <- as.Date(spl_10_df$date, format = "%Y-%m-%d")

num_ebooks <- spl_10_df %>% filter(str_detect(MaterialType, "EBOOK")) %>% group_by(date) %>% summarise(ebooks = sum(Checkouts, na.rm = TRUE))

num_books <- spl_10_df %>% filter(str_detect(MaterialType, "BOOK")) %>% group_by(date) %>% summarise(books = sum(Checkouts, na.rm = TRUE))

num_audiobooks <- spl_10_df %>% filter(str_detect(MaterialType, "AUDIOBOOK")) %>% group_by(date) %>% summarise(audiobooks = sum(Checkouts, na.rm = TRUE))

num_magazines <- spl_10_df %>% filter(str_detect(MaterialType, "MAGAZINE")) %>% group_by(date) %>% summarise(magazines = sum(Checkouts, na.rm = TRUE))

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
  labs(title = "Different types of books",
       x = "Date",
       y = "Number of checkouts",
       color = "Legend")
