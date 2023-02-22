# Most popular genres that emerged from the pandemic (by month)
# Non-Fiction vs Fiction

spl_10_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_10_df <- spl_10_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_10_df$date <- as.Date(spl_10_df$date, format = "%Y-%m-%d")

nonfiction_checkouts <- spl_10_df %>% filter(str_detect(Subjects, "Nonfiction")) %>% group_by(date) %>% summarise(sum_checkouts = sum(Checkouts, na.rm = TRUE))

fiction_checkouts <- spl_10_df %>% filter(str_detect(Subjects, "Fiction")) %>% group_by(date) %>% summarise(sum_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot() + 
  geom_line(data = nonfiction_checkouts, aes(x = date, y = sum_checkouts), color = "blue") + 
  geom_line(data = fiction_checkouts, aes(x = date, y = sum_checkouts), color = "red") + 
  labs(title = "Nonfiction vs Fiction Checkouts 2017-2023",
       x = "Date",
       y = "Number of checkouts")
