library(tidyverse)

### Day 1
data1  <- rbind(0, read_csv('~/Downloads/input', col_names = FALSE))

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


### Day 4
library(lubridate)
library(padr)
data4 <- read_csv('advent-clj/input4', col_names = FALSE)

guard_and_minute  <- data4 %>%
    extract(X1, into = c('event_time', 'event'), regex = "\\[(.*)\\] (.*)") %>%
    mutate(event_time = parse_date_time(event_time, "%Y-%m-%d %H:%M")) %>%
    arrange(event_time) %>%
    extract(event, into = c('id'), regex = "#(\\d+) ", remove = FALSE) %>%
    pad() %>% # Add minutes in intervals
    mutate(
        shift = ifelse(str_detect(event, 'shift'), 'shift', 'other'),
        minute = ifelse(hour(event_time) == 23, -1, 1) * minute(event_time)) %>%
    mutate(minute = ifelse(minute < 0, 0, minute),
           event = ifelse(shift == 'shift', 'wakes up', event)) %>%
    fill(id, event) %>%
    filter(event == 'falls asleep') %>%
    group_by(id, minute) %>%
    summarise(n=n()) %>%
    group_by(id) %>%
    summarise(by_guard=sum(n), minute_max_by_guard=minute[which.max(n)]) %>%
    filter(by_guard == max(by_guard)) %>%
    ungroup()

as.integer(guard_and_minute$id) * guard_and_minute$minute_max_by_guard

guard_and_minute  <- data4 %>%
    extract(X1, into = c('event_time', 'event'), regex = "\\[(.*)\\] (.*)") %>%
    mutate(event_time = parse_date_time(event_time, "%Y-%m-%d %H:%M")) %>%
    arrange(event_time) %>%
    extract(event, into = c('id'), regex = "#(\\d+) ", remove = FALSE) %>%
    pad() %>% # Add minutes in intervals
    mutate(
        shift = ifelse(str_detect(event, 'shift'), 'shift', 'other'),
        minute = ifelse(hour(event_time) == 23, -1, 1) * minute(event_time)) %>%
    mutate(minute = ifelse(minute < 0, 0, minute),
           event = ifelse(shift == 'shift', 'wakes up', event)) %>%
    fill(id, event) %>%
    filter(event == 'falls asleep') %>%
    group_by(id, minute) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    filter(n == max(n))

as.integer(guard_and_minute$id) * guard_and_minute$minute

### Day 5

gsub("([a-z])$1", '', 'dabAcCaCBAcCcaDA')
