# Load libraries
library("dplyr")
library("stringr")

# Data frame (All checkouts in the past year)
spl_All_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Data frame (At least 5 checkouts a month in the past decade)
spl_5_df <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# Total number of non-fiction (nf) books checked out in the past decade
total_nf_checkouts <- spl_5_df %>% 
  filter(str_detect(Subjects, "Nonfiction")) %>% 
  summarise(total_nf_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(total_nf_checkouts)

# Total number of fiction (f) books checked out in the past decade
total_f_checkouts <- spl_5_df %>% 
  filter(str_detect(Subjects, "Fiction")) %>% 
  summarise(total_f_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(total_f_checkouts)

# Number of checkouts for Lifespan by David A. Sinclair in the past year
lifespan_checkouts <- spl_All_df %>% 
  filter(str_detect(Title, "Lifespan")) %>% 
  filter(str_detect(Creator, "Sinclair")) %>% 
  summarise(lifespan_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(lifespan_checkouts)

# Number of checkouts for The Hobbit by J. R. R. Tolkien (Book and eBook only) 
# in the past year
hobbit_checkouts <- spl_All_df %>% 
  filter(str_detect(Title, "The Hobbit")) %>% 
  filter(str_detect(Creator, "Tolkien")) %>% 
  filter(str_detect(MaterialType, "AUDIO", negate = TRUE)) %>% 
  summarise(hobbit_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(hobbit_checkouts)

# Favorite book that I read in 2022, Sapiens by Yuval Noah Harari. 
# Number of checkouts in the past decade
sapiens_checkouts <- spl_5_df %>% 
  filter(str_detect(Title, "Sapiens")) %>% 
  filter(str_detect(Creator, "Harari")) %>% 
  filter(str_detect(Title, "Homo deus", negate = TRUE)) %>% 
  summarise(sapiens_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(sapiens_checkouts)
