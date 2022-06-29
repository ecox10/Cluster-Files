##########################
### Elizabeth Cox ########
##########################

## Run SVI ERGM with Census Tract level data

## Load Packages

library(ergm)
library(dplyr)
library(igraph)
library(stats)
library(rapport)
library(rlang)

## Load Data

nodedat <- read.csv("RMDuplicated.csv")
edgedat <- read.csv("edgelistToRun.csv")


######################
## Clean some data
######################

# Filter
nodedat<- nodedat %>% filter(NYC_region == "NYC")

# Check for number of "members"
nodedat$numMem <- nodedat$PTStaff + nodedat$FTStaff + nodedat$Members + nodedat$Volunteers
numMem <- nodedat$PTStaff + nodedat$FTStaff + nodedat$Members + nodedat$Volunteers

# Filter member number
nodedat <- nodedat %>% filter(numMem > 1)

#########################
### Create Network Object
#########################

## Run network
net <- network(x = edges,
               vertices = nodedat$PopID,
               directed = TRUE,
               loops = FALSE, 
               matrix.type = "edgelist")
net

##########################
### Create Organization Characteristic Network Attributes
##########################

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

############################
### Create Organization Focus Network Attribute
############################

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

########################
### SVI Attributes
########################

## Census tract SVI data
THEME1 <- nodedat$RPL_THEME1.N.19.11
THEME2 <- nodedat$RPL_THEME2.N.19.11
THEME3 <- nodedat$RPL_THEME3.N.19.11
THEME4 <- nodedat$RPL_THEME4.N.19.11
Overall <- nodedat$RPL_THEMES.N.19.11

network::set.vertex.attribute(net, "Socioeconomic SVI", THEME1)
network::set.vertex.attribute(net, "Household Composition SVI", THEME2)
network::set.vertex.attribute(net, "Minority SVI", THEME3)
network::set.vertex.attribute(net, "Housing SVI", THEME4)
network::set.vertex.attribute(net, "Overall SVI", Overall)

#########################
### Run ERGM 
#########################

m_SVI <- ergm(net ~ edges 
                               + mutual 
                               + gwidegree(0.25, fixed = T)
                               + gwodegree(0.25, fixed = T)
                               + nodeofactor("501(c)(3)")
                               + nodematch("501(c)(3)", diff = T)
                               + nodeofactor("Paid Staff")
                               + nodematch("Paid Staff", diff = T)
                               + nodeofactor("Arts")
                               + nodematch("Arts", diff = T, keep = 2)
                               + nodeofactor("Community Imp")
                               + nodematch("Community Imp", diff = T, keep = 2)
                               + nodeofactor("Economic Development")
                               + nodematch("Economic Development", diff = T, keep = 2)
                               + nodeofactor("Education")
                               + nodematch("Education", diff = T, keep = 2)
                               + nodeofactor("Environment")
                               + nodematch("Environment", diff = T, keep = 2)
                               + nodeofactor("Human Services")
                               + nodematch("Human Services", diff = T, keep = 2)
                               + nodeofactor("Public Health")
                               + nodematch("Public Health", diff = T, keep = 2)
                               + nodeofactor("Recreation") 
                               + nodematch("Recreation", diff = T, keep = 2)
                               + nodeofactor("Senior")
                               + nodematch("Senior", diff = T, keep = 2)
                               + nodeofactor("Youth")
                               + nodematch("Youth", diff = T, keep = 2)
                               + nodeocov("Socioeconomic SVI")
                               + nodematch("Socioeconomic SVI")
                               + nodeocov("Household Composition SVI")
                               + nodematch("Household Composition SVI")
                               + nodeocov("Minority SVI")
                               + nodematch("Minority SVI")
                               + nodeocov("Housing SVI")
                               + nodecov("Housing SVI"),
                               constraints=~bd(maxout = net%v%"maxOut"),
                               control=control.ergm(MCMC.samplesize=25000,MCMC.burnin=50000,MCMC.interval=50000),eval.loglik = TRUE)

summary(m_SVI)
