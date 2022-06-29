##############################
### Run Model 3 for Computing Cluster
##############################

### Load Packages
library(ergm)
library(dplyr)
library(igraph)
library(stats)
library(rapport)
library(rlang)

## Load Data
setwd("/Users/elizabeth/Documents/Forest Service/FS/STEWMAP")
nodedat <- read.csv("STEWMAP_ResearchDataSet.csv")
edgedat <- read.csv("2017 STEW-MAP Edgelist - Collaborate 10-16-18.csv")
SVI <- read.csv("SVI_NewYork.csv")

## Clean Data
#########################################################################
# first add column to edgedat with zipcode for each survey respondent
nyc <- nodedat$NYC_region[match(edgedat$Respondent.PopID, cbind(nodedat$PopID, nodedat$OrgZip))]

edgedat$NYC_region <- nodedat$NYC_region[match(edgedat$Respondent.PopID, nodedat$PopID)]

# Filter
nodedat<- nodedat %>% filter(NYC_region == "NYC")
edgedat <- edgedat %>% filter(NYC_region == "NYC")

# Check for number of "members"
nodedat$numMem <- nodedat$PTStaff + nodedat$FTStaff + nodedat$Members + nodedat$Volunteers
numMem <- nodedat$PTStaff + nodedat$FTStaff + nodedat$Members + nodedat$Volunteers
edgedat$matchedMem <- numMem[match(edgedat$Respondent.PopID, cbind(nodedat$PopID, nodedat$OrgZip))]

# Filter 
nodedat <- nodedat %>% filter(numMem > 1)
edgedat <- edgedat %>% filter(matchedMem > 1)

# Take out "general
edges <- cbind(edgedat$Respondent.PopID, edgedat$PopID._ALTER)
edges <- na.omit(edges)
edges <- edges[which(!(edges[,2] == "GENERAL")),]

# Replace the PopIDs that contain a /
edges[which(edges == "3201/1149")] <- "3201"
#########################################################################

## Create Basic Network
#########################################################################
tot_nodes <- c(edges) %>%
  unique()

# Try to make edges numeric matrix
edges[,1] <- as.numeric(edges[,1])
edges[,2] <- as.numeric(edges[,2])

## Run network
net <- network(x = edges,
                  vertices = nodedat$PopID,
                  directed = TRUE,
                  loops = FALSE, 
                  matrix.type = "edgelist")
#########################################################################

## Organizational Characteristic Terms
#########################################################################
## Create index to refer back to
idx <- rep(NA, length(vertex_names))
for (i in 1:length(idx)) {
  idx[i] <- which(nodedat$PopID == vertex_names[i])[1]
}
idx[which(is.na(idx))] <- 1
nodedat$OrgType[is_empty(nodedat$OrgType)] <- "Other"


## Find 501(c)(3) status
c3 <- rep(0, length(vertex_names)) # create vector to populate
# Replace NAs with "None Selected"
nodedat$OrgType[is.na(nodedat$OrgType)] <- "None Selected"
# populate vector
for(i in 1:length(c3)) {
  if (nodedat$OrgType[idx[i]] == "501(c)(3) (or has applied)") {
    c3[i] <- 1
  }
}

## Find Paid Staff
# Create vector to populate
paidstaff_num <- rep(0, length(vertex_names))
paidstaff <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$PTStaff[is.na(nodedat$PTStaff)] <- 0
nodedat$FTStaff[is.na(nodedat$FTStaff)] <- 0
# populate paidstaff vector
for (i in 1:length(vertex_names)) {
  if (nodedat$FTStaff[idx[i]] + nodedat$PTStaff[idx[i]] > 0) {
    paidstaff_num[i] <- nodedat$FTStaff[idx[i]] + nodedat$PTStaff[idx[i]]
    paidstaff[i] <- 1
  }
}


## Find Volunteers
# Create vector to populate
vol <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$Volunteers[is.na(nodedat$Volunteers)] <- 0
# populate vol vector
for (i in 1:length(vertex_names)) {
  if (nodedat$Volunteers[idx[i]] > 0) {
    vol[i] <- 1
  }
}

## Find volunteer hours
# Create vector to populate
volHours <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OccVolHrs[is.na(nodedat$OccVolHrs)] <- 0
# Populate volunteer hour vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OccVolHrs[idx[i]] > 0) {
    volHours[i] <- nodedat$OccVolHrs[idx[i]]
  }
}

# Add edge attributes
network::set.vertex.attribute(net, "501(c)(3)", c3)
network::set.vertex.attribute(net, "Paid Staff", paidstaff)
network::set.vertex.attribute(net, "Volunteers", vol)
network::set.vertex.attribute(net, "volHours", volHours)
#########################################################################   

## Organization Focus Terms
#########################################################################   
### Arts Status
# Create vector to populate
arts <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_Arts[is.na(nodedat$OF_Arts)] <- 0
# Populate arts vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_Arts[idx[i]] == 1) {
    arts[i] <- 1
  }
}

### Community Imp Status
# Create vector to populate
CommImp <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_CommImp[is.na(nodedat$OF_CommImp)] <- 0
# Populate CommImp vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_CommImp[idx[i]] == 1) {
    CommImp[i] <- 1
  }
}

### Development Status
# Create vector to populate
Dev <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_EconDev[is.na(nodedat$OF_EconDev)] <- 0
# Populate Dev vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_EconDev[idx[i]] == 1) {
    Dev[i] <- 1
  }
}

#### Education Status
# Create vector to populate
Educ <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_Educ[is.na(nodedat$OF_Educ)] <- 0
# Populate Educ vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_Educ[idx[i]] == 1) {
    Educ[i] <- 1
  }
}

### Environment status
# Create vector to populate
Env <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_Environ[is.na(nodedat$OF_Environ)] <- 0
# Populate Env vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_Environ[idx[i]] == 1) {
    Env[i] <- 1
  }
}

### Housing Status
# Create vector to populate
Housing <- rep(0, length(vertex_names))
# Replace NAs to 0
nodedat$OF_Housing[is.na(nodedat$OF_Housing)] <- 0
# Populate Housing vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_Housing[idx[i]] == 1) {
    Housing[i] <- 1
  }
}

### Human Services Status
# Create vector to populate
HumServ <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_HumServ[is.na(nodedat$OF_HumServ)] <- 0
# Populate HumServ vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_HumServ[idx[i]] == 1) {
    HumServ[i] <- 1
  }
}

### Public Health Status
# Create vector to populate
PubHealth <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_PubHlth[is.na(nodedat$OF_PubHlth)] <- 0
# Populate PubHealth vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_PubHlth[idx[i]] == 1) {
    PubHealth[i] <- 1
  }
}

### Recreation Status
# Create vector to populate
Rec <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_SprtRec[is.na(nodedat$OF_SprtRec)] <- 0
# Populate Rec vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_SprtRec[idx[i]] == 1) {
    Rec[i] <- 1
  }
}

### Religion Status
# Create vector to populate
Relig <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_Faith[is.na(nodedat$OF_Faith)] <- 0
# Populate Relig vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_Faith[idx[i]] == 1) {
    Relig[i] <- 1
  }
}

### Senior Status
# Create vector to populate
Sen <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_Senior[is.na(nodedat$OF_Senior)] <- 0
# Populate Sen vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_Senior[idx[i]] == 1) {
    Sen[i] <- 1
  }
}

### Youth Status
# Create vector to populate
Youth <- rep(0, length(vertex_names))
# Replace NAs with 0
nodedat$OF_Youth[is.na(nodedat$OF_Youth)] <- 0
# Populate Youth vector
for (i in 1:length(vertex_names)) {
  if (nodedat$OF_Youth[idx[i]] == 1) {
    Youth[i] <- 1
  }
}


## Set as attributes
network::set.vertex.attribute(net, "Arts", arts)
network::set.vertex.attribute(net, "Community Imp", CommImp)
network::set.vertex.attribute(net, "Economic Development", Dev)
network::set.vertex.attribute(net, "Education", Educ)
network::set.vertex.attribute(net, "Environment", Env)
network::set.vertex.attribute(net, "Housing", Housing)
network::set.vertex.attribute(net, "Human Services", HumServ)
network::set.vertex.attribute(net, "Public Health", PubHealth)
network::set.vertex.attribute(net, "Recreation", Rec)
network::set.vertex.attribute(net, "Religion", Relig)
network::set.vertex.attribute(net, "Senior", Sen)
network::set.vertex.attribute(net, "Youth", Youth)               
#########################################################################  

## SVI Terms
#########################################################################  
# Assign zip codes to neighborhood
Bronx_Dist8 <- c(10463, 10471)
Bronx_Dist12 <- c(10460, 10466, 10467, 10469, 10470, 10475)
Bronx_Dist10 <- c(10461, 10465, 10467, 10475)
Bronx_Dist11 <- c(10460, 10461, 10462, 10467, 10469)
Bronx_Dist3_6 <- c(10451, 10456, 10457, 10459, 10460, 10458)
Bronx_Dist7 <- c(10458, 10468, 10453, 10457)
Bronx_Dist5 <- c(10452, 10453, 10457, 10458, 10468)
Bronx_Dist4 <- c(10451, 10452, 10456)
Bronx_Dist9 <- c(10462, 10460, 10461)
Bronx_Dist1_2 <- c(10451, 10454, 10455, 10456, 10459, 10474)
Bronx <- c(Bronx_Dist8, Bronx_Dist12, Bronx_Dist10, Bronx_Dist11, Bronx_Dist3_6, Bronx_Dist7, Bronx_Dist5, Bronx_Dist4, Bronx_Dist9, Bronx_Dist1_2)
Man_Dist12 <- c(10031, 10032, 10033, 10034, 10040)
Man_Dist9 <- c(10031, 10032, 10039)
Man_Dist10 <- c(10026, 10027, 10030, 10037, 10039)
Man_Dist11 <- c(10029, 10035)
Man_Dist8 <- c(10021, 10028, 10044, 10065, 10075, 10128)
Man_Dist7 <- c(10023, 10024, 10025)
Man_Dist4_5 <- c(10001, 10011, 10018, 10019, 10020, 10036)
Man_Dist6 <- c(10010, 10016, 10017, 10022)
Man_Dist3 <- c(10002, 10003, 10009)
Man_Dist1_2 <- c(10012, 10013, 10014)
Man <- c(Man_Dist12, Man_Dist9, Man_Dist10, Man_Dist11, Man_Dist8, Man_Dist7, Man_Dist4_5, Man_Dist6, Man_Dist3, Man_Dist1_2)
StIsland_Dist3 <- c(10307, 10309)
StIsland_Dist2 <- c(10304, 10305)
StIsland_Dist1 <- c(10304, 10301)
StIsland <- c(StIsland_Dist1, StIsland_Dist2, StIsland_Dist3)
Brkln_Dist1 <- c(11222, 11211, 11206, 11249)
Brkln_Dist4 <- c(11206, 11207, 11221, 11237)
Brkln_Dist3 <- c(11203, 11205, 11206, 11216, 11221, 11233)
Brkln_Dist2 <- c(11201, 11238, 11205, 11217)
Brkln_Dist6 <- c(11215, 11217, 11231)
Brkln_Dist8 <- c(11213, 11216, 11233, 11238, 11225)
Brkln_Dist16 <- c(11212, 11233)
Brkln_Dist5 <- c(11207, 11208, 11239)
Brkln_Dist18 <- c(11236, 11234)
Brkln_Dist17 <- c(11203)
Brkln_Dist9 <- c(11213, 11216, 11233, 11238, 11225)
Brkln_Dist7 <- c(11220, 11232)
Brkln_Dist10 <- c(11209, 11220, 11228)
Brkln_Dist12 <- c(11204, 11218, 11220, 11219, 11230)
Brkln_Dist14 <- c(11226, 11210, 11230)
Brkln_Dist15 <- c(11299, 11235)
Brkln_Dist11 <- c(11204, 11214)
Brkln_Dist13 <- c(11235, 11224)
Brkln <- c(Brkln_Dist1, Brkln_Dist4, Brkln_Dist3, Brkln_Dist2, Brkln_Dist6, Brkln_Dist8, Brkln_Dist16, Brkln_Dist5, Brkln_Dist18, Brkln_Dist17, Brkln_Dist9, Brkln_Dist7, Brkln_Dist10, Brkln_Dist12, Brkln_Dist14, Brkln_Dist15, Brkln_Dist11, Brkln_Dist13)
Queens_Dist1 <- c(11101, 11102, 11103, 11105, 11106, 11109, 11120)
Queens_Dist3 <- c(11372, 11368)
Queens_Dist7 <- c(11354, 11355, 11358)
Queens_Dist11 <- c(11360, 11361, 11364)
Queens_Dist13 <- c(11427, 11428, 11429, 11411, 11422)
Queens_Dist8 <- c(11435, 11365, 11366)
Queens_Dist4 <- c(11373, 11368)
Queens_Dist6 <- c(11374, 11375)
Queens_Dist2 <- c(11101, 11104, 11377)
Queens_Dist5 <- c(11385, 11379)
Queens_Dist9 <- c(11418, 11419, 11421)
Queens_Dist12 <- c(11423, 11432, 11433, 11434, 11435, 11436, 11412)
Queens_Dist10 <- c(11416, 11417, 11414)
Queens_Dist14 <- c(11691, 11697, 11693)
Queens <- c(Queens_Dist1, Queens_Dist3, Queens_Dist7, Queens_Dist11, Queens_Dist13, Queens_Dist8, Queens_Dist4, Queens_Dist6, Queens_Dist2, Queens_Dist5, Queens_Dist9, Queens_Dist12, Queens_Dist10, Queens_Dist14)

## Census tract by city
# Bronx = 36005
# Brooklyn = 36047
# Manhattan = 36061
# Queens = 36081
# Staten Island = 36085

## Fix this -999 nonsense
SVI$RPL_THEME1[which(SVI$RPL_THEME1 < 0)] <- 0
SVI$RPL_THEME2[which(SVI$RPL_THEME2 < 0)] <- 0
SVI$RPL_THEME3[which(SVI$RPL_THEME3 < 0)] <- 0
SVI$RPL_THEME4[which(SVI$RPL_THEME4 < 0)] <- 0

Census_tract <- rep(0, length(nodedat$PopID)) 
for (i in 1:length(Census_tract)){
  if (nodedat$OrgZip[i] %in% Bronx) {
    Census_tract[i] <- 36005
  }
  else if (nodedat$OrgZip[i] %in% Brkln) {
    Census_tract[i] <- 36047
  }
  else if (nodedat$OrgZip[i] %in% Man) {
    Census_tract[i] <- 36061
  }
  else if (nodedat$OrgZip[i] %in% Queens) {
    Census_tract[i] <- 36061
  }
  else if (nodedat$OrgZip[i] %in% StIsland) {
    Census_tract[i] <- 36085
  }
}

## Join these Data sets by census tract

nodedat <- cbind(nodedat, Census_tract)
SVIDat <- left_join(nodedat, SVI, by = c("Census_tract" = "STCNTY"))

## Make these into network attributes 

### Minority SVI
# Create vector to populate
MinoritySVI <- rep(0, length(vertex_names))

# Populate theme vector
for (i in 1:length(vertex_names)) {
    MinoritySVI[i] <- SVIDat$RPL_THEME3[idx[i]]
}

## Housing SVI
# Create vector to populate
HousingSVI <- rep(0, length(vertex_names))
# Populate theme vector
for (i in 1:length(vertex_names)) {
    HousingSVI[i] <- SVIDat$RPL_THEME4[idx[i]]
}

## Socioeconomic SVI
# Create vector to populate
SocioeconomicSVI <- rep(0, length(vertex_names))
# Populate theme vector
for (i in 1:length(vertex_names)) {
    SocioeconomicSVI[i] <- SVIDat$RPL_THEME1[idx[i]]
}

## Household Composition SVI
# Create vector to populate
HouseCompSVI <- rep(0, length(vertex_names))
# Populate theme vector
for (i in 1:length(vertex_names)) {
    HouseCompSVI[i] <- SVIDat$RPL_THEME2[idx[i]]
}

## Overall SVI
# Create vector to populate
TotalSVI <- rep(0, length(vertex_names))
# Populate theme vector
for (i in 1:length(vertex_names)) {
    TotalSVI[i] <- SVIDat$RPL_THEMES[idx[i]]
}

network::set.vertex.attribute(net, "Socioeconomic SVI", SocioeconomicSVI)
network::set.vertex.attribute(net, "Household Composition SVI", HouseCompSVI)
network::set.vertex.attribute(net, "Minority SVI", MinoritySVI)
network::set.vertex.attribute(net, "Housing SVI", HousingSVI)
network::set.vertex.attribute(net, "Overall SVI", TotalSVI)
#########################################################################  

### Run the Model

# save(m3, file = "home/coxeli/stewmap/model3.Rdata")                                               