library(tidyverse)
library(scales)
library(readxl)
library(GGally)

# download the data from
# https://informationisbeautiful.us6.list-manage.com/track/click?u=ddd60592f59ea1712ed3d1937&id=ce9293eb48&e=b67810f07f

sheet2 <- read_excel("data/WDVP-Datasets.xlsx", sheet = "what makes a 'good' government")
names(sheet2)[1:4] <- c("country", "iso", "population", "area")

data2 <- sheet2[-(1:4), ] %>%
  gather(indicator, value, -country, -iso) %>%
  mutate(value = as.numeric(value),
         indicator = gsub("[\\%\\&\\(\\) ]", " ", tolower(indicator)),
         indicator = str_squish(indicator),
         indicator = gsub(" ", "_", indicator)) %>%
  filter(!grepl("^x_", indicator)) %>%
  spread(indicator, value)

  
data2 %>%
  select(-country, -iso) %>%
  ggpairs()
  

x <- data2 %>%
  select(-country, -iso) %>%
  cor(use = "pairwise.complete.obs")

x %>%
  as.data.frame() %>%
  mutate(var1 = dimnames(x)[[1]]) %>%
  gather(var2, correlation, -var1) %>%
  as_tibble() %>%
  filter(correlation != 1) %>%
  arrange(desc(correlation)) %>%
  mutate(id = 1:n()) %>%
  filter(id %% 2 == 0) %>%
  select(-id) %>%
  View
