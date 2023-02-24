# The Greatest Books (Top 5)
# Title
# In Search of Lost Time
# Ulysses
# Don Quixote
# One Hundred Years of Solitude
# The Great Gatsby

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

spl_5_df <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

spl_5_df <- spl_5_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_5_df$date <- as.Date(spl_5_df$date, format = "%Y-%m-%d")

# Have been abbreviated
lost_time_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "In Search of Lost Time")) %>% 
  summarise(lost_time_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(lost_time_checkouts)

ulysses_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "Ulysses")) %>% 
  summarise(ulysses_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(ulysses_checkouts)

quixote_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "Don Quixote")) %>% 
  summarise(quixote_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(quixote_checkouts)


solitude_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "One Hundred Years of Solitude")) %>% 
  summarise(solitude_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(solitude_checkouts)

gatsby_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "The Great Gatsby")) %>% 
  summarise(gatsby_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(gatsby_checkouts)


books <- c("In Search of Lost Time", "Ulysses", "Don Quixote", 
           "One Hundred Years of Solitude", "The Great Gatsby")
top_5_books <- data.frame(
  checkouts = c(lost_time_checkouts, ulysses_checkouts, quixote_checkouts, 
                solitude_checkouts, gatsby_checkouts),
  books = factor(books, levels = books)
)

ggplot(data = top_5_books, aes(x = checkouts, y = books)) + 
  geom_col(aes(checkouts, reorder(books, +checkouts)), fill = "#0000ff", width = 0.5) + 
  geom_text(aes(label = checkouts), vjust = 0.5, hjust = -0.1) + 
  labs(title = "The Greatest Books Checkouts In The Past Decade",
       x = "Number of Checkouts",
       y = "Books")
