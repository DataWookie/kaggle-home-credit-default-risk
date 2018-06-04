library(dplyr)

load_application <- function(filename) {
  read.csv(filename) %>%
    setNames(names(.) %>% tolower())
}

data_train <- load_application("data/application_train.csv")
data_test  <- load_application("data/application_test.csv")
