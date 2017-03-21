library(tidyverse)
library(tidytext)
library(rematch2)

wcma <- read_csv("~/Development/wcmaart/wcma-collection.csv")

credit_tokens <- wcma %>%
  select(accession_number, credit_line) %>%
  unnest_tokens(credit_token, credit_line) %>%
  count(credit_token, sort = TRUE)

wcma_dim <- wcma %>%
  bind_re_match(dimensions, "\\((?<dimension1>\\d+\\.?\\d*) x (?<dimension2>\\d+\\.?\\d*)(?: x (?<dimension3>\\d+\\.?\\d*))? (?<unit>\\w+)") %>%
  filter(unit %in% c("cm", "m")) %>%
  mutate_at(vars(dimension1, dimension2, dimension3), funs(as.numeric(if_else(. == "", NA_character_, .)))) %>%
  mutate_at(vars(dimension1, dimension2, dimension3), funs(if_else(unit == "m", . * 100, .))) %>%
  mutate(area = dimension1 * dimension2) %>%
  mutate(
    accession_year_string = str_match(accession_number, "\\d{2}|\\d{4}"),
    accession_year_num = as.numeric(accession_year_string),
    accession_year = if_else(accession_year_num < 1000, accession_year_num + 1900, accession_year_num))


ggplot(wcma_dim, aes(xmin = 0, xmax = dimension1, ymin = 0, ymax = dimension2, fill = classification)) +
  geom_rect(alpha = 0.1) +
  facet_wrap(~ classification) +
  scale_fill_discrete(guide = FALSE)

wcma_dim %>%
  group_by(accession_year, classification) %>%
  summarize(med_area = median(area, na.rm = TRUE)) %>%
  ggplot(aes(x = accession_year, y = med_area)) +
  geom_line() +
  facet_wrap(~classification, scales = "free_y")


