#scenarios.r
library(rgdal)
s1 <- readOGR("./data-raw/scenarios/", "scenario1_poly")
s.2.1 <- readOGR("./data-raw/scenarios/", "scenario2_poly1")
s.2.2 <- readOGR("./data-raw/scenarios/", "scenario2_poly2")
s.3.1 <- readOGR("./data-raw/scenarios/", "scenario3_poly1")
s.3.2 <- readOGR("./data-raw/scenarios/", "scenario3_poly2")
s.4.1 <- readOGR("./data-raw/scenarios/", "scenario4_poly1")
s.4.2 <- readOGR("./data-raw/scenarios/", "scenario4_poly2")
s.5.1 <- readOGR("./data-raw/scenarios/", "scenario5_poly1")
s.5.2 <- readOGR("./data-raw/scenarios/", "scenario5_poly2")

names(s1) <- c("id","Name")
names(s.1.1) <- c("id","Name")
names(s.1.2) <- c("id","Name")
names(s.2.1) <- c("id","Name")
names(s.2.2) <- c("id","Name")
names(s.3.1) <- c("id","Name")
names(s.3.2) <- c("id","Name")
names(s.4.1) <- c("id","Name")
names(s.4.2) <- c("id","Name")
names(s.5.1) <- c("id","Name")
names(s.5.2) <- c("id","Name")

s1$Scenario <- 1
s1$Poly <- 1
s.2.1$Scenario <- 2
s.2.1$Poly <- 1
s.2.2$Scenario <- 2
s.2.2$Poly <- 2
s.3.1$Scenario <- 3
s.3.1$Poly <- 1
s.3.2$Scenario <- 3
s.3.2$Poly <- 2
s.4.1$Scenario <- 4
s.4.1$Poly <- 1
s.4.2$Scenario <- 4
s.4.2$Poly <- 2
s.5.1$Scenario <- 5
s.5.1$Poly <- 1
s.5.2$Scenario <- 5
s.5.2$Poly <- 2


scenarios <- rbind(s1, s.2.1, s.2.2, s.3.1, s.3.2, s.4.1, s.4.2, s.5.1, s.5.2, makeUniqueIDs = TRUE)

writeOGR(scenarios, "./data-raw/scenarios/", "scenarios", driver = "ESRI Shapefile")
devtools::use_data(scenarios, overwrite = TRUE)
