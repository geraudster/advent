library(tidyverse)

data  <- rbind(0, read_csv('~/Downloads/input', col_names = FALSE))

mutate(data, cumul = cumsum(X1)) %>%
    add_count(cumul) %>%
    filter(n > 1) %>%
    print(n = 50)

%>%
    summarise(count = n()) %>%
    filter(count > 1)

mutate(data, cumul = factor(cumsum(X1))) %>%
    add_count(cumul) %>%
    arrange(desc(n)) %>%
    print(n = 50)
