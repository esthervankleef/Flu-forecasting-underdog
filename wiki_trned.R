##### Example of how to download and plot some wiki data
# sadly this only works language specific
# even worse, the source stats.grok.se has stopped data collection Jan 2016. 
# Damn shit

## title: "Wiki trends"
## author: "Underdog"
## date: "19 October 2016"

# empty workspace
rm(list = ls())
script_name <- "wiki_trend"
library(wikipediatrend)
#
# load the data from the server, give word, language, dates 
page_views <- 
  wp_trend(
    page = "Influenza", 
    lang = "en",
    from = "2016-01-01",
    to   = "2016-07-07"
  )

library(ggplot2)

for(i in unique(page_views$lang) ){
  iffer <- page_views$lang==i
  page_views[iffer, ]$count <- scale(page_views[iffer, ]$count)
}

ggplot(page_views, aes(x=date, y=count, group=lang, color=lang)) + 
  geom_line(size=1.2, alpha=0.5) + 
  ylab("standardized count\n(by lang: m=0, var=1)") +
  theme_bw() + 
  scale_colour_brewer(palette="Set1") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) 