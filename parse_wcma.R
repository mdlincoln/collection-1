library(tidyverse)
library(tidytext)

wcma <- read_csv("~/Development/wcmaart/wcma-collection.csv")

credit_tokens <- wcma %>%
  select(accession_number, credit_line) %>%
  unnest_tokens(credit_token, credit_line) %>%
  count(credit_token, sort = TRUE)
