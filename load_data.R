# Load packages
library(foreign)
library(plyr)
library(dplyr)


#-------------------
# Useful functions
#-------------------
# Calculate the duration of democracy 
# (years since a Polity IV score of 5 or higher)
demdur.calc <- function(x) {
  x <- ifelse(x > 5, FALSE, TRUE)  # Set democracies to false
  
  # Magic one-liner to figure out duration since last autocracy
  (!x) * unlist(lapply(rle(x)$lengths, seq_len))
}

# Remove "n." prefix from variables
remove.nums <- function(variable) {
  sub("^\\d+\\. ", "", variable)
}

# Use \parbox to get multiple lines in a cell
# http://tex.stackexchange.com/a/11555/11851
two.lines <- function(top, bottom) {
  paste0("\\parbox{3cm}{\\centering ", top, " \\\\ ", bottom, "}")
}

# Custom ggplot theme
theme_ath <- function(base_size=12) {
  ret <- theme_bw(base_size) + 
    theme(axis.title=element_text(vjust=0.2), legend.position="bottom")
  ret
}


#------------------------
# Load and reshape data
#------------------------
# Original Murdie:2013 data
murdie <- read.dta("Data/11558_2013_9180_MOESM1_ESM.dta")

# UDS data
uds <- read.csv("Data/uds_summary.csv.gz")
names(uds)[c(1, 4:8)] <- paste("uds", names(uds)[c(1, 4:8)], sep="_")

# Full Polity IV data
polity <- read.csv("Data/p4v2012.csv")

# Democracy duration for full Polity IV data
polity.small <- polity %.%
  group_by(ccode) %.%
  mutate(demdur=demdur.calc(polity2)) %.%
  select(cowcode=ccode, year, demdur)

# Generate full dataset
coop.data <- select(murdie, icrg_qog, humanitarian, disastersample, 
                    civilconflictsample, coopNGONGOdummy, 
                    lnaidpercap, lnsmorgs, countngo, chga_demo, 
                    lnpop, lngdppercap, cowcode, year, ht_region, 
                    uw_gini, wdi_gini, physint) %.%
  merge(uds, by=c("cowcode", "year")) %.%
  merge(polity.small) %.%
  filter(ht_region != "5. Western Europe and North America") %.%  # Exclude region
  group_by(cowcode) %.%  # Separate into country chunks
  mutate(lead.coopNGONGOdummy=lead(coopNGONGOdummy)) %.%  # Create lead variable
  ungroup() %.%
  mutate(Iyeara=factor(year)) %.%  # Make a factor
  mutate(lead.coopNGONGOdummy.factor=factor(lead.coopNGONGOdummy)) %.%
  mutate(humanitarian.factor=factor(humanitarian)) %.%
  mutate(disastersample.factor=factor(disastersample)) %.%
  mutate(civilconflictsample.factor=factor(civilconflictsample)) %.%
  mutate(poor.dem=ifelse(lngdppercap <= median(lngdppercap, na.rm=TRUE) & 
                           chga_demo=="1. Democracy", "Poor", "Not poor"))

# Add dummy variables for each factor level
coop.data <- cbind(coop.data, model.matrix(~ Iyeara -1, data=coop.data))
