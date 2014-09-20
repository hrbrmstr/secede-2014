library(rvest)
library(dplyr)
library(tidyr)
library(gpclib)
library(rgeos)
library(sp)
library(maptools)
library(rgdal) # needs gdal > 1.11.0
library(ggplot2)
library(reshape2)
library(gridExtra)

# extract data from rvested <div>'s and clean it up a bit
# pass in the rvested HTML object and the CSS selector to extract

extractAndCleanup <- function(data, selector, make_numeric=FALSE) {
  x <- data %>% html_nodes(selector) %>% html_text()
  x <- gsub("^[[:punct:][:space:]]*|[[:punct:][:space:]]*$", "", x)
  if (make_numeric) x <- as.numeric(gsub("[,[:space:]]*", "", x))
  x
}

# if we've already grabbed the data, then don't waste bandwidth

if (!file.exists("data/secede.rda")) {

  # grab the BBC voting site HTML
  bbc_vote <- html("http://www.bbc.com/news/events/scotland-decides/results")

  # make the data frame by capturing the data in the <div>s, converting some to
  # numeric and leaving others as text. we can simplify the data frame creation
  # code by putting the rvest field extraction code into one fucnction

  secede <- data.frame(
    council=bbc_vote %>% extractAndCleanup(".body-row__cell--council"),
    electorate=bbc_vote %>% extractAndCleanup(".body-row__cell--electorate", TRUE),
    yes=bbc_vote %>% extractAndCleanup(".body-row__cell--yes", TRUE),
    no=bbc_vote %>% extractAndCleanup(".body-row__cell--no", TRUE),
    stringsAsFactors=FALSE)

  # we may use the "gone" column at a later date so keep it
  # the BBC colors were horrible, so we use a muted Union Jack red for the
  # seceding Counties and a Scottish blue for the ones who stayed in
  # we'll also use hex colors so they can be readily applied to base
  # graphics maps (if we choose to use base graphis)

  secede <- secede %>% mutate(gone=yes>no,
                              color=ifelse(gone, "#0065BD", "#CF142B77"))

  # normalize the Council names to match the ones in the TopoJSON file

  secede$council <- gsub("&", "and", secede$council)
  secede[secede$council=="Edinburgh",]$council = "City of Edinburgh"
  secede[secede$council=="Glasgow",]$council = "Glasgow City"
  secede[secede$council=="Comhairle nan Eilean Siar",]$council = "Na h-Eileanan an Iar"

  # save the data to avoid a call out to the BBC later
  # this way if they nuke the voting page, we still have the data
  save(secede, file="data/secede.rda")

} else {

  load("data/secede.rda")

}

# decent (minimal) scotland shapefiles are a pain to find, so we'll borrow the
# one from the BBC and remove the javascript "cruft" from the actual TopoJSON data
# and revel in the fact that gdal can read Topo/GeoJSON as of v1.11.0
# we'll not do this if we've already done it, too

if (!file.exists("data/scotland.json")) {
  GET("http://static.bbci.co.uk/news/1.46.4-1166/js/data/maps/l/scotland-elections.js",
      write_disk("data/scotland.json"), progress())
  tmp <- readLines("data/scotland.json")
  writeLines(tmp[2], "data/scotland.json")
}

# read in our new shape file

map = readOGR("data/scotland.json", "scotland-elections")

# we have to sort our data frame by the order of the Council names
# from the TopoJSON file since we're using base graphics to do the
# plotting (we're using base since it's quick and easy and it didn't
# work cleanly in ggplot when i tried it :-)

secede$council <- factor(secede$council, map@data$name, ordered=TRUE)
secede <- secede %>% arrange(council)


# due to TopoJSON errors, base graphics is the easiest solution
# this makes the choropleth, hacking "asp" vs dealing with projections
# par(mar=c(0,0,0,0))
# plot(map, col=secede$color, border="white", asp=1.75)

# we're still going to try to use ggplot

# fortify(map, region="name") (or "id") errors out do to some issues with
# one polygon in this TopoJSON file. So, we work around it by merging our
# Council names and secession data with the fortified map data frame

map_df <- fortify(map)

# manually associate the map id's with the Council names and vote data
councils <- data.frame(id=0:(length(map@data$name)-1),
                       council=as.character(map@data$name))
map_df <- merge(map_df, councils, by="id")
map_df <- merge(map_df, secede, by="council")

# now we can work with the map using ggplot

gg <- ggplot()
gg <- gg + geom_map(data=map_df, map=map_df,
                    aes(map_id=id, x=long, y=lat, group=group, fill=color),
                    color="white", size=0.25)
gg <- gg + scale_fill_manual(values=rev(unique(secede$color)),
                             labels=c("Yes", "No"), name="Secede?")
gg <- gg + xlim(extendrange(r=range(coordinates(map)[,1]), f=0.15))
gg <- gg + ylim(extendrange(r=range(coordinates(map)[,2]), f=0.07))
gg <- gg + coord_map()
gg <- gg + labs(x="", y="")
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(legend.position="none")
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())

secede_m <- secede %>%
  gather(variable, value, -council) %>%
  filter(variable %in% c("yes", "no")) %>%
  mutate(value=as.numeric(value))

# get things sorted to the bars display properly

  secede_m <- merge(secede_m, secede, by="council")
  secede_m$variable <- factor(secede_m$variable,
                              levels=c("yes", "no"), ordered=TRUE)
  secede_m <- secede_m %>% arrange(no, variable)
  secede_m$council <- factor(secede_m$council,
                             unique(secede_m$council), ordered=TRUE)

gg1 <- ggplot(secede_m, aes(x=council, y=value, fill=factor(variable)))
gg1 <- gg1 + geom_bar(stat="identity", position="fill")
gg1 <- gg1 + scale_fill_manual(values=rev(unique(secede$color)),
                             labels=c("Yes", "No"), name="Secede?")
gg1 <- gg1 + coord_flip()
gg1 <- gg1 + labs(x="", y="")
gg1 <- gg1 + theme_bw()
gg1 <- gg1 + theme(panel.grid=element_blank())
gg1 <- gg1 + theme(legend.position="top")
gg1 <- gg1 + theme(panel.border=element_blank())
gg1 <- gg1 + theme(axis.ticks=element_blank())
gg1 <- gg1 + theme(axis.text.x=element_blank())

vote <- arrangeGrob(gg1, gg, ncol=2,
                     main=textGrob("Scotland Votes", gp=gpar(fontsize=20)))

ggsave("plots/vote.svg", vote, width=9, height=6.5)
