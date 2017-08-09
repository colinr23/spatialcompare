library(rgeos)
mp <- readWKT("MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)),((15 5, 40 10, 10 20, 5 10, 15 5)))")

# total area
sapply(slot(mp, "polygons"), slot, "area")

# get list of individual polys
p <- lapply(mp@polygons , slot , "Polygons")

# areas of individual polygons
lapply(p[[1]], function(x) slot(x, "area"))
