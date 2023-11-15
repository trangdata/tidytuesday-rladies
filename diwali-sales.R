
library(tidyverse)

theme_set(theme_minimal())
tuesdata <- tidytuesdayR::tt_load('2023-11-14')
sales <- tuesdata$diwali_sales_data




sales %>%
  mutate(Marital_Status = as_factor(Marital_Status)) %>%
  filter(Age <= 52) %>%
  ggplot() +
  aes(x = Age, y = Amount, color = Marital_Status) +
  geom_point() +
  geom_smooth()

names(sales)
# [1] "User_ID"          "Cust_name"        "Product_ID"       "Gender"
# [5] "Age Group"        "Age"              "Marital_Status"   "State"
# [9] "Zone"             "Occupation"       "Product_Category" "Orders"
# [13] "Amount"
sales %>%
  # mutate(Marital_Status = as_factor(Marital_Status)) %>%
  # filter(Age <= 52) %>%
  mutate(Product_Category = fct_reorder(Product_Category, Amount, max)) %>%
  ggplot() +
  aes(y = Product_Category, x = Amount,
      fill = Product_Category) +
  geom_boxplot(alpha = 1) +
  guides(fill = "none") +
  # rcartocolor::scale_color_carto_d() +
  scale_fill_manual(values = colorRampPalette(cartocolors[10, "n12"][[1]])(18)) +
  labs(y = NULL)


tuesdata <- tidytuesdayR::tt_load(2023, week = 42)

taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

taylor_all_songs %>%
  drop_na(key_name, mode_name) %>%
  add_count(key_name, mode_name) %>%
  ggplot() +
  aes(x = key_name, y = mode_name, fill = n) +
  geom_tile() +
  # scale_fill_carto_c(type = "diverging", palette = "TealRose") +
  scale_fill_gradient(low = "#f1eac8", high = "#d0587e") +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL)

library(ggfortify)
mypca <- taylor_album_songs %>%
  group_by(album_name) %>%
  summarise(across(danceability:tempo, \(x) median(x, na.rm = TRUE))) %>%
  column_to_rownames("album_name") %>%
  select(danceability:tempo, -mode) %>%
  na.omit() %>%
  prcomp(center = TRUE, scale. = TRUE)

autoplot(mypca, loadings = TRUE, loadings.colour = '#009392',
         loadings.label = TRUE, loadings.label.size = 3, label = TRUE)
