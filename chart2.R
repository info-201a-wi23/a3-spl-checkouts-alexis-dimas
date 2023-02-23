library("dplyr")
library("stringr")
library("ggplot2")

spl_10_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_10_df <- spl_10_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_10_df$date <- as.Date(spl_10_df$date, format = "%Y-%m-%d")

orwell_df <- spl_10_df %>% filter(str_detect(Creator, "George Orwell"))

book_1984_checkouts <- orwell_df %>% filter(str_detect(Title, "1984")) %>% group_by(date) %>% summarise(sum_checkouts = sum(Checkouts, na.rm = TRUE))

book_AF_checkouts <- orwell_df %>% filter(str_detect(Title, "Animal Farm")) %>% group_by(date) %>% summarise(sum_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot() + 
  geom_line(data = book_1984_checkouts, aes(x = date, y = sum_checkouts, color = "1984")) + 
  geom_line(data = book_AF_checkouts, aes(x = date, y = sum_checkouts, color = "Animal Farm")) + 
  scale_color_manual(values=c("#0000ff", "#ff0000")) + 
  labs(title = "1984 vs Animal Farm Checkouts 2017-2023",
       x = "Date",
       y = "Number of checkouts",
       color = "Legend")
