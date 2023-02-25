# The Greatest Books (Top 5) Checkouts
# 1. In Search of Lost Time
# 2. Ulysses
# 3. Don Quixote
# 4. One Hundred Years of Solitude
# 5. The Great Gatsby

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# Data frame (At least 5 checkouts a month in the past decade)
spl_5_df <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# Number of In Search of Lost Time checkouts
lost_time_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "In Search of Lost Time")) %>% 
  summarise(lost_time_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(lost_time_checkouts)

# Number of Ulysses checkouts
ulysses_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "Ulysses")) %>% 
  summarise(ulysses_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(ulysses_checkouts)

# Number of Don Quixote checkouts
quixote_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "Don Quixote")) %>% 
  summarise(quixote_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(quixote_checkouts)

# Number of One Hundred Years of Solitude checkouts
solitude_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "One Hundred Years of Solitude")) %>% 
  summarise(solitude_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(solitude_checkouts)

# Number of The Great Gatsby checkouts
gatsby_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "The Great Gatsby")) %>% 
  summarise(gatsby_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(gatsby_checkouts)

# Vector of Top 5 Titles
books <- c("In Search of Lost Time", "Ulysses", "Don Quixote", 
           "One Hundred Years of Solitude", "The Great Gatsby")

# Data on Top 5
top_5_books <- data.frame(
  checkouts = c(lost_time_checkouts, ulysses_checkouts, quixote_checkouts, 
                solitude_checkouts, gatsby_checkouts),
  books = factor(books, levels = books)
)

# Bar Plot
ggplot(data = top_5_books, aes(x = checkouts, y = books)) + 
  geom_col(aes(checkouts, reorder(books, +checkouts)), 
           fill = "#0000ff", width = 0.5) + 
  geom_text(aes(label = checkouts), vjust = 0.5, hjust = -0.1) + 
  labs(title = "Top Five Greatest Books Checkouts In The Past Decade",
       x = "Number of Checkouts",
       y = "Book Title")
