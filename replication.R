# Load packages
pkgs <- c("foreign", "Zelig", "plyr", "dplyr", 
          "ggplot2", "stargazer", "car", "reshape2")
suppressPackageStartupMessages(invisible(lapply(pkgs, require, character.only = TRUE)))

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
                    lnpop, lngdppercap, cowcode, year, ht_region) %.%
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
  mutate(civilconflictsample.factor=factor(civilconflictsample))

# Add dummy variables for each factor level
coop.data <- cbind(coop.data, model.matrix(~ Iyeara -1, data=coop.data))


#---------------------------
# Initial diagnostic plots
#---------------------------

# Regime type and governance
p <- ggplot(na.omit(coop.data[,c("icrg_qog", "chga_demo")]), 
            aes(y=icrg_qog, x=chga_demo))
p + geom_violin()

# UDS scale and governance
p <- ggplot(na.omit(coop.data[,c("uds_mean", "icrg_qog", "chga_demo")]), 
            aes(x=uds_mean, y=icrg_qog, colour=chga_demo))
p + geom_point()

# Democracy duration
p <- ggplot(na.omit(coop.data[,c("demdur", "icrg_qog", "ht_region", "Iyeara")]), 
            aes(x=demdur, y=icrg_qog))
p + geom_point(aes(colour=ht_region)) + facet_wrap(~ Iyeara)


#---------
# Models
#---------
# Run original logit model
model.logit <- glm(lead.coopNGONGOdummy.factor ~ icrg_qog + humanitarian.factor +
                     lnaidpercap + lnsmorgs + countngo + chga_demo + lnpop +
                     lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                     Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                     Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                     Iyeara2002 + Iyeara2003 + disastersample.factor + 
                     civilconflictsample.factor, 
                   data=coop.data, 
                   family=binomial(link="logit"))
summary(model.logit)

# Extract data
model.data <- model.frame(model.logit)

# Sample data, with full range of qog for dictatorships and democracies
X <- with(model.data, data.frame(
  icrg_qog=rep(seq(0, 1, 0.05), 2), humanitarian.factor=factor(0), lnaidpercap=mean(lnaidpercap),
  lnsmorgs=mean(lnsmorgs), countngo=mean(countngo), chga_demo=rep(c("0. Dictatorship", "1. Democracy"), each=21), 
  lnpop=mean(lnpop), lngdppercap=mean(lngdppercap), Iyeara1990=0, Iyeara1991=0, Iyeara1992=0,
  Iyeara1993=0, Iyeara1994=0, Iyeara1995=0, Iyeara1996=0, Iyeara1998=0, Iyeara1999=0, Iyeara2000=0,
  Iyeara2001=0, Iyeara2002=0, Iyeara2003=1, disastersample.factor=factor(1), civilconflictsample.factor=factor(1)))

# Predict using sample data
predicted.logit <- cbind(X, predict(model.logit, newdata=X, type="link", se=TRUE))
predicted.logit <- predicted.logit %.%
  mutate(pred.prob=plogis(fit)) %.%
  mutate(LL=plogis(fit - (1.96 * se.fit))) %.%
  mutate(UL=plogis(fit + (1.96 * se.fit)))

# Plot predicted probabilities with SEs
p <- ggplot(predicted.logit, aes(x=icrg_qog, y=pred.prob))
p + geom_ribbon(aes(ymin=LL, ymax=UL, fill=chga_demo), alpha=.2) + geom_line(aes(colour=chga_demo), size=1)


# Expanded logit model, using UDS and duration data
logit.expanded <- glm(lead.coopNGONGOdummy.factor ~ icrg_qog + humanitarian.factor +
                        lnaidpercap + lnsmorgs + countngo + uds_mean + lnpop +
                        lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                        Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                        Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                        Iyeara2002 + Iyeara2003 + disastersample.factor + 
                        civilconflictsample.factor + demdur,   # + restrictions?
                      data=coop.data, #subset=(chga_demo=="1. Democracy"),
                      family=binomial(link="logit"))
summary(logit.expanded)
vif(logit.expanded)

# Extract data from expanded model
expanded.data <- model.frame(logit.expanded)

# Sample data, with full range of qog for dictatorships and democracies
X <- with(expanded.data, data.frame(
  icrg_qog=mean(icrg_qog), humanitarian.factor=factor(0), lnaidpercap=mean(lnaidpercap),
  lnsmorgs=mean(lnsmorgs), countngo=mean(countngo), uds_mean=seq(0, 1.5, 0.05), 
  lnpop=mean(lnpop), lngdppercap=mean(lngdppercap), Iyeara1990=0, Iyeara1991=0, Iyeara1992=0,
  Iyeara1993=0, Iyeara1994=0, Iyeara1995=0, Iyeara1996=0, Iyeara1998=0, Iyeara1999=0, Iyeara2000=0,
  Iyeara2001=0, Iyeara2002=0, Iyeara2003=1, disastersample.factor=factor(1), 
  civilconflictsample.factor=factor(1), demdur=rep(c(0, 10, 100), each=31)))  # demdur=rep(seq(0, 20, 1), each=61)

# Predict using sample data
predicted.logit <- cbind(X, predict(logit.expanded, newdata=X, type="link", se=TRUE))
predicted.logit <- predicted.logit %.%
  mutate(pred.prob=plogis(fit)) %.%
  mutate(LL=plogis(fit - (1.96 * se.fit))) %.%
  mutate(UL=plogis(fit + (1.96 * se.fit)))

# Plot predicted probabilities with SEs
p <- ggplot(predicted.logit, aes(x=uds_mean, y=pred.prob))
p + geom_ribbon(aes(ymin=LL, ymax=UL, fill=factor(demdur)), alpha=.2) + 
  geom_line(aes(colour=factor(demdur)), size=1)





# Models for each region
# Running multiple models instead of dummies + interactions
# "In general, performing separate regressions is tantamount to including all 
# possible two-way interactions with the community variable (coded as in the 
# second model, not the first) and allowing for different error distributions 
# for each community."
# http://stats.stackexchange.com/questions/17110/should-i-run-separate-regressions-for-every-community-or-can-community-simply-b

# bquote() + .() passes the actual names of the arguments into the model call; eval() actually runs the model
run.logits <- function(region) {
  model <- bquote(glm(lead.coopNGONGOdummy.factor ~ icrg_qog + chga_demo + 
                        Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                        Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                        Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                        Iyeara2002 + Iyeara2003,# + demdur,
                      data=coop.data, subset=(ht_region==.(region)),
                      family=binomial(link="logit")))
  eval(model)
}

# Create a list of models for all regions, given a model formula
regional.models <- lapply(levels(coop.data$ht_region)[-c(5, 9, 10)], FUN=run.logits)
names(regional.models) <- levels(coop.data$ht_region)[-c(5, 9, 10)]  # Name the list for convenience
lapply(regional.models, summary)


X <- with(expanded.data, data.frame(
  icrg_qog=rep(seq(0, 1, 0.05), 2),
  chga_demo=rep(c("0. Dictatorship", "1. Democracy"), each=21),
  Iyeara1990=0, Iyeara1991=0, Iyeara1992=0, Iyeara1993=0, Iyeara1994=0, 
  Iyeara1995=0, Iyeara1996=0, Iyeara1998=0, Iyeara1999=0, Iyeara2000=0,
  Iyeara2001=0, Iyeara2002=0, Iyeara2003=1))

X.big <- do.call("rbind", replicate(7, X, simplify = FALSE)) %.%
  mutate(country=rep(levels(coop.data$ht_region)[-c(5, 9, 10)], each=42))

sim.pred <- lapply(regional.models, predict, newdata=X, type="link", se.fit=TRUE)

asdf <- ldply(sim.pred, data.frame) %.%
  mutate(icrg_qog=rep(seq(0, 1, 0.05), 14)) %.%
  mutate(chga_demo=rep(rep(c("0. Dictatorship", "1. Democracy"), each=21), 7)) %.%
  mutate(pred.prob=plogis(fit)) %.%
  mutate(LL=plogis(fit - (1.96 * se.fit))) %.%
  mutate(UL=plogis(fit + (1.96 * se.fit)))

plot.asdf <- asdf %.%
  filter(.id != "8. South Asia") %.%
  mutate(region=factor(.id))
  

p <- ggplot(plot.asdf, aes(x=icrg_qog, y=pred.prob, colour=region))
p + geom_line(size=2) + facet_wrap(~ chga_demo, nrow=2)

predicted.logit <- cbind(X, predict(logit.expanded, newdata=X, type="link", se=TRUE))
predicted.logit <- predicted.logit %.%
  mutate(pred.prob=plogis(fit)) %.%
  mutate(LL=plogis(fit - (1.96 * se.fit))) %.%
  mutate(UL=plogis(fit + (1.96 * se.fit)))




stargazer(regional.models, type="text", omit="Iyeara*", digits=2, star.cutoffs=c(0.05, 0.01, 0.001))


# Countries in East Asia in the dataset
coop.data %.%
  filter(ht_region=="6. East Asia") %.%
  select(ctry) %.%
  filter(ctry==unique(ctry))




# Separation plots
separation.plot(predict(model.logit, type="response"), model.data$lead.coopNGONGOdummy.factor, line=TRUE)
separation.plot(predict(logit.expanded, type="response"), expanded.data$lead.coopNGONGOdummy.factor, line=TRUE)


predicted.values <- predict(model.logit, type="response")
actual.values <- model.data$lead.coopNGONGOdummy.factor


