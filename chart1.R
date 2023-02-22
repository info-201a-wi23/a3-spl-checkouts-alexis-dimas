# Ideas
library("dplyr")
library("stringr")
library("ggplot2")

# Trends from the COVID-19 Pandemic


spl_10_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)
spl_5_df <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)
spl_All_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_10_df <- spl_10_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_10_df$date <- as.Date(spl_10_df$date, format = "%Y-%m-%d")



orwell_df <- spl_10_df %>% filter(str_detect(Creator, "George Orwell"))

checkouts_per_month <- orwell_df %>% group_by(date) %>% summarise(sum_checkouts = sum(Checkouts, na.rm = TRUE))

book_1984_checkouts <- orwell_df %>% filter(str_detect(Title, "1984")) %>% group_by(date) %>% summarise(sum_checkouts = sum(Checkouts, na.rm = TRUE))
book_AF_checkouts <- orwell_df %>% filter(str_detect(Title, "Animal Farm")) %>% group_by(date) %>% summarise(sum_checkouts = sum(Checkouts, na.rm = TRUE))


ggplot() + 
  geom_line(data = book_1984_checkouts, aes(x = date, y = sum_checkouts), color = "blue") + 
  geom_line(data = book_AF_checkouts, aes(x = date, y = sum_checkouts), color = "red") + 
  labs(title = "1984 vs Animal Farm Checkouts 2017-2023",
       x = "Date",
       y = "Number of checkouts")




ggplot(data = checkouts_per_month, aes(x = date, y = sum_checkouts)) + 
  geom_line() + 
  labs(title = "George Orwell Checkouts 2022-2023",
       x = "Date",
       y = "Number of checkouts")

