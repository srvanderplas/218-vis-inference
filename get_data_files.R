library(rvest)
library(tidyverse)

fix_data <- function(tbl) {
  if (!is.data.frame(tbl)) return(tbl)
  
  if (ncol(tbl) > 1) return(tbl)
  
  if (!str_detect(names(tbl)[1], "\t")) {
    if (!str_count(tbl[1], " ") > 1) {
      # replace space with \t and then read the thing in with read_tsv
      tbl[1] <- str_replace(tbl[1], " ", "\t")
      tmp <- paste(tbl, collapse = "\n") %>% read_tsv
      return(tmp)
    } else {
      return(tbl)
    }
  }
  return(tbl)
}

read_data <- function(link) {
  if (str_detect(link, "Gettysburg")) {
    dat <- read_lines(link)
  } else if (str_detect(link, "GoodandBad")) {
    dat <- read_lines(link) %>%
      str_replace("\\t", " ") %>%
      str_remove_all("[[:punct:]]") %>%
      paste(collapse = "\n") %>% 
      read_delim(delim = " ")
  } else if (str_detect(link, "Stop")) {
    dat <- read_lines(link) %>%
      str_replace("\\s", " ") %>%
      paste(collapse = "\n") %>% 
      read_delim(delim = " ")
  } else {
    tmp <- read_lines(link)
    tmp[1] <- str_replace(tmp[1], "^\t", "Var\t")
    dat <- str_replace(tmp, "\\s{1,}", "\t") %>%
      paste(collapse = "\n") %>%
      read_tsv()
    #if (str_count(tmp[1], "\\t") >= 1) 
    # dat <- read_tsv(link)
  }
  return(dat)
}



isi_data <- read_html("http://www.isi-stats.com/isi/applets.html") %>%
  xml_nodes("a") %>%
  map_dfr(~tibble(link = xml_attr(., "href"), value = xml_text(.))) %>%
  mutate(value = str_replace_all(value, "\\s", " ") %>% str_trim()) %>%
  mutate(type = str_extract(link, "(applets|data)"),
         chapter = str_extract(value, "(Exploration|Example) [P0-9]{1,2}\\.\\d[AB]?") %>%
           if_else(is.na(.), str_extract(link, "(chap\\d{1,}|prelim)"), .) %>%
           str_remove(., "chap"),
         type2 = str_extract(chapter, "Exploration|Example")) %>%
  filter(rowSums(is.na(.)) == 0 & type == "data") %>%
  mutate(data = purrr::map(link, read_data))

save(isi_data, file = "isi_data.Rdata")
