library(rvest)
library(dplyr)
library(gpclib)
library(rgeos)
library(sp)
library(maptools)
library(rgdal)

# we'll need this to clear out the "cruft" from the <div>s
trim <- function(x, make_numeric=FALSE) {
  x <- gsub("^[[:punct:][:space:]]*|[[:punct:][:space:]]*$", "", x)
  if (make_numeric) x <- gsub("[,[:space:]]*", "", x)
  x
}

# if we've already grabbed the data, then don't waste bandwidth

if (!file.exists("data/secede.rda")) {

  # grab the BBC voting site
  bbc_vote <- html("http://www.bbc.com/news/events/scotland-decides/results")

  # make the data frame by capturing the data in the <div>s, converting some to
  # numeric and leaving others as text

  secede <- data.frame(
    council=bbc_vote %>% html_nodes(".body-row__cell--council") %>% html_text() %>% trim(),
    electorate=bbc_vote %>% html_nodes(".body-row__cell--electorate") %>% html_text() %>% trim(TRUE),
    yes=bbc_vote %>% html_nodes(".body-row__cell--yes") %>% html_text() %>% trim(TRUE),
    no=bbc_vote %>% html_nodes(".body-row__cell--no") %>% html_text() %>% trim(TRUE),
    stringsAsFactors=FALSE)

  # we may use the "gone" column at a later date so keep it
  # the BBC colors were horrible, so we use a muted Union Jack for the seceding Counties
  # and a Scottish blue for the ones who stayed in

  secede <- secede %>% mutate(gone=yes>no, color=ifelse(gone, "#0065BD", "#CF142B77"))

  # normalize the Council names to match the ones in the TopoJSON file

  secede$council <- gsub("&", "and", secede$council)
  secede[secede$council=="Edinburgh",]$council = "City of Edinburgh"
  secede[secede$council=="Glasgow",]$council = "Glasgow City"
  secede[secede$council=="Comhairle nan Eilean Siar",]$council = "Na h-Eileanan an Iar"

} else {

  load("data/secede.rda")

}

# decent (minimal) scotland shapefiles are a pain to find, so borrow the
# one from the BBC and remove the javascript "cruft" from the actual TopoJSON data
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
secede$council <- factor(secede$council, map$NAME, ordered=TRUE)
secede <- secede %>% arrange(council)

# save the data (if we haven't before) just in case we want it later
if (!file.exists("data/secede.rda")) save(secede, file="data/secede.rda")

# make the choropleth, hacking "asp" vs dealing with projections
plot(map, col=secede$color, border="white", asp=1.75)


