library(leaflet)

ojo_map <- function() {
  county_shp <- rgdal::readOGR(dsn = "data/okcounties", layer = "okcounties", verbose = FALSE) %>%
    sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

  connect_ojo()
  d <- ojo_tbl("oscn_crim_disps") %>%
    filter(file_year == 2019, casetype == "CF") %>%
    group_by(court) %>%
    summarize(n_cases = max(casenum)) %>%
    collect() %>%
    mutate(n_cases = str_sub(n_cases, 9, 13) %>%
             as.numeric)

  shp <- county_shp %>%
    sp::merge(d %>% rename(name = court), all.x = T)

  pal <-  colorBin(palette = "RdBu",
                   domain = c(min(d$n_cases), max(d$n_cases)),
                   reverse = FALSE)

  # pal <-  colorBin(palette = "RdBu",
  #                  domain = c(-100, 400),
  #                  bins = c(-100, -50, -25, -10, 10, 25, 50, 500),
  #                  reverse = FALSE)

  leaflet() %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = shp,
                fillColor = ~pal(shp@data$n_cases),
                weight = 1,
                opacity = .9,
                color = "black",
                fillOpacity = 0.7,
                label = paste("County:", shp@data$name),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
}
