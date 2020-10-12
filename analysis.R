library(tidyverse)
library(rethinking)
library(dplyr)

# Load programming language popularity data.
langs <- read_csv("pypl.csv", col_types = cols(.default = col_double(), date = col_date()))
names(langs)[names(langs) == "Abap"] <- "ABAP"
names(langs)[names(langs) == "C/C++"] <- "C"

# Load programming language and string interpolation support data.
langs_and_si <- read_csv("languages_and_si.csv", col_types = cols(.default = col_character(), string_interp_year = col_integer(), created_year = col_integer()))
precis(langs_and_si)

language_names <- colnames(langs)[colnames(langs) != 'date']
prev_by_year <- langs %>%
  group_by(year=substring(langs$date, 0, 4)) %>%
  summarise_at(language_names, mean) %>%
  pivot_longer(cols = -year) %>%
  left_join(langs_and_si, by = c("name" = "language")) %>%
  mutate(string_interp = ifelse(year >= string_interp_year & !is.na(string_interp_year), 1, 0)) %>%
  group_by(year) %>%
  summarise(string_interp_prevalence = sum(value * string_interp) / sum(value))

ggplot(prev_by_year, aes(year, string_interp_prevalence, group=1)) + 
  geom_line() + 
  ylim(0, 1) + 
  labs(x = "Year", y = "Relative prevalence of string interpolation") + 
  theme_minimal() +
  theme(text = element_text(size = 20))

