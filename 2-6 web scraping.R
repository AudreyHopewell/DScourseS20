#mw-content-text > div > table:nth-child(8) > tbody > tr:nth-child(5) > td:nth-child(4)
m100 <- read_html("https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression")
view(m100)
listoutput <- m100 %>% html_nodes("#mw-content-text > div > table:nth-child(8)") %>% html_table(fill = TRUE)
table <- listoutput[[1]]
view(table)

list_2 <- m100 %>% html_nodes("#mw-content-text > div > table:nth-child(14)") %>% html_table(fill = TRUE)
table_2 <- list_2[[1]]
