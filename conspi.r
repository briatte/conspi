#===============================================================================
# LOAD PACKAGES
#===============================================================================

library(dplyr)
library(readr)
library(rvest)
library(lubridate)
library(stringr)
library(igraph)

#===============================================================================
# GET THE DATA
#===============================================================================

if(!file.exists("conspi.csv")) {

  d1 = data_frame()

  for(i in 1:4) {

    h = html(paste0("http://confusionnisme.info/page/", i))

    d1 = rbind(d1, data_frame(
      url = html_nodes(h, "#content .entry-title a") %>% html_attr("href"),
      title = html_nodes(h, "#content .entry-title a") %>% html_text,
      date = html_nodes(h, "#content header.entry-header time") %>% html_text,
      tags = html_nodes(h, ".tag-links") %>%
        sapply(function(x) html_nodes(x, "a") %>%
                 html_text %>%
                 paste0(collapse = ";"))
    ))

  }

  d1$date = parse_date_time(d1$date, "%d %m %Y", locale = "fr_FR") %>% as.Date

  d2 = data_frame()

  for(i in 1:5) {

    h = html(paste0("http://conspishorsdenosvies.noblogs.org/page/", i))

    d2 = rbind(d2, data_frame(
      url = html_nodes(h, "#content .entry-title a") %>% html_attr("href"),
      title = html_nodes(h, "#content .entry-title a") %>% html_text,
      date = html_nodes(h, "#content .entry-date") %>% html_text,
      tags = html_nodes(h, ".tag-links") %>%
        sapply(function(x) html_nodes(x, xpath = "a[@rel='tag']") %>%
                 html_text %>%
                 paste0(collapse = ";"))
    ))

  }

  d2$date = parse_date_time(d2$date, "%d/%m/%Y") %>% as.Date

  d3 = data_frame()

  for(i in seq(0, 1180, 20)) {

    h = html(paste0("http://www.conspiracywatch.info/?start=", i))

    d3 = rbind(d3, data_frame(
      url = html_nodes(h, "#mod_1260437 .titre a") %>% html_attr("href"),
      title = html_nodes(h, "#mod_1260437 .titre") %>% html_text %>% str_trim,
      date = html_nodes(h, "#mod_1260437 .cel_pied .date") %>% html_text,
      tags = html_nodes(h, "#mod_1260437 .cel_pied") %>%
        sapply(function(x) html_nodes(x, ".objet-tag a") %>%
                 html_text %>%
                 paste0(collapse = ";"))
    ))

  }

  d3$url = paste0("http://www.conspiracywatch.info", d3$url)
  d3$date = parse_date_time(d3$date, "%d %m %Y", locale = "fr_FR") %>% as.Date

  d = rbind(d1, d2, d3) %>% arrange(date)
  d$tags = gsub("-", " ", d$tags) %>% tolower

  write_csv(d, "conspi.csv")

}

#===============================================================================
# FIND ALL EDGES
#===============================================================================

d = read_csv("conspi.csv")
table(substr(d$date, 1, 4))

e = c()

for(i in d$tags) {

  e = c(e, strsplit(i, ";") %>%
          unlist %>%
          unique %>%
          expand.grid(i = ., j = ., stringsAsFactors = FALSE) %>%
          filter(i != j) %>%
          apply(., 1, function(x) paste0(sort(x), collapse = "///")) %>%
          unique)

}

e = data.frame(table(e)) %>%
  mutate(i = gsub("(.*)///(.*)", "\\1", e),
         j = gsub("(.*)///(.*)", "\\2", e)) %>%
  select(i, j, n = Freq) %>%
  arrange(-n)

#===============================================================================
# WEIGHT THE EDGES
#===============================================================================

n = table((unlist(strsplit(d$tags, ";"))))

e$n_i = n[ e$i ] %>% as.integer
e$n_j = n[ e$j ] %>% as.integer
e$min = apply(select(e, n_i, n_j), 1, min)
e$w = e$n / e$min

table(cut(e$n, c(0:4 * 10, max(e$n))))

#===============================================================================
# DRAW SAMPLE NETWORKS
#===============================================================================

col = colorRampPalette(c("grey50", "black"))(3)

for(i in c(1:4 * 10)) {

  sub = e[ e$n >= i, ]

  n = graph.edgelist(sub[, 1:2 ] %>% as.matrix, directed = FALSE)
  E(n)$weight = e[ e$n >= i, ]$w

  # delete a few stopwords
  n = delete.vertices(n, which(V(n)$name %in% c("conspirationnisme", "rumeur",
                                                "rumeurs", "internet")))

  k = multilevel.community(n) %>% membership

  cat("Network at", i, "+ ties:", vcount(n), "nodes", ecount(n), "edges",
      n_distinct(k), "groups\n")

  pdf(paste0("conspi_net_", i, ".pdf"), width = 12, height = 12)
  plot(n, vertex.size = 6, vertex.color = "grey90", vertex.frame.color = "grey90",
       vertex.label.color = col[ cut(degree(n), c(0, 2, 4, Inf)) %>% as.numeric ],
       vertex.label.family = "sans", vertex.label.color = "black",
       edge.color = ifelse(E(n)$weight == 1, "grey50", "grey75"),
       mark.groups = lapply(1:n_distinct(k), function(x) which(k == x)),
       mark.col = "grey90", mark.border = "grey80")
  dev.off()

}

# kthxbye
