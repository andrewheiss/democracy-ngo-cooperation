# Load packages and data
library(Zelig)
library(stargazer)
library(car)
library(sandwich)
library(lmtest)

source("load_data.R")


#-------------------
# Useful functions
#-------------------
# Calculate clustered robust standard errors
robust.clusterify <- function(model, dat, cluster) {
  attach(dat, warn.conflicts = F)
  require(sandwich)
  require(lmtest)
  not <- attr(model$model,"na.action")
  
  if( ! is.null(not)) {  # only drop the NA values if there are any left
    cluster <- cluster[-not]
    dat <- dat[-not,]
  }
  
  with(dat, {
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
    coefs <- coeftest(model, vcovCL, type="HC1")  # HC1 or HC0 are close to Stata
    return(list(clcov=vcovCL, coefs=coefs))
  })
}


#---------
# Models
#---------
# Original logit model
logit.orig <- glm(lead.coopNGONGOdummy.factor ~ icrg_qog + humanitarian.factor +
                    lnaidpercap + lnsmorgs + countngo + chga_demo + lnpop +
                    lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                    Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                    Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                    Iyeara2002 + Iyeara2003 + disastersample.factor + 
                    civilconflictsample.factor, 
                  data=coop.data, 
                  family=binomial(link="logit"))
summary(logit.orig)

# Calculate robust SEs
logit.orig.clustered <- robust.clusterify(logit.orig, coop.data, coop.data$cowcode)

# Rare event version of original logit model
relogit.orig <- zelig(lead.coopNGONGOdummy ~ icrg_qog + humanitarian.factor + 
                        lnaidpercap + lnsmorgs + countngo + chga_demo + lnpop + 
                        lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                        Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                        Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                        Iyeara2002 + Iyeara2003 + disastersample.factor + 
                        civilconflictsample.factor, 
                      model="relogit", robust=list(method="weave"), data=coop.data)
summary(relogit.orig)


# Extended logit model, using UDS and duration data
logit.extended <- glm(lead.coopNGONGOdummy.factor ~ icrg_qog + humanitarian.factor +
                        lnaidpercap + lnsmorgs + countngo + uds_mean + demdur + lnpop +
                        lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                        Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                        Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                        Iyeara2002 + Iyeara2003 + disastersample.factor + 
                        civilconflictsample.factor,
                      data=coop.data,
                      family=binomial(link="logit"))
summary(logit.extended)
vif(logit.extended)

# Calculate robust SEs
logit.extended.clustered <- robust.clusterify(logit.extended, coop.data, coop.data$cowcode)
logit.extended.clustered$coefs

# Rare event version of extended model
relogit.extended <- zelig(lead.coopNGONGOdummy ~ icrg_qog + humanitarian.factor +
                            lnaidpercap + lnsmorgs + countngo + uds_mean + demdur + lnpop +
                            lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                            Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                            Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                            Iyeara2002 + Iyeara2003 + disastersample.factor + 
                            civilconflictsample.factor,
                          model="relogit", robust=list(method="weave"), data=coop.data)
summary(relogit.extended)


# Extended logit model, using UDS and duration data + regional fixed effects
logit.extended.region <- glm(lead.coopNGONGOdummy.factor ~ icrg_qog + 
                               humanitarian.factor +
                               lnaidpercap + lnsmorgs + countngo + uds_mean + demdur + lnpop +
                               lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                               Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                               Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                               Iyeara2002 + Iyeara2003 + disastersample.factor + 
                               civilconflictsample.factor + ht_region,
                             data=coop.data,
                             family=binomial(link="logit"))
summary(logit.extended.region)
vif(logit.extended.region)

# Calculate robust SEs
logit.extended.region.clustered <- robust.clusterify(logit.extended.region, coop.data, coop.data$cowcode)

# Rare event version of extended + region model
relogit.extended.region <- zelig(lead.coopNGONGOdummy ~ icrg_qog + humanitarian.factor +
                                   lnaidpercap + lnsmorgs + countngo + uds_mean + demdur + lnpop +
                                   lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                                   Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                                   Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                                   Iyeara2002 + Iyeara2003 + disastersample.factor + 
                                   civilconflictsample.factor + ht_region,
                                 model="relogit", robust=list(method="weave"), 
                                 data=coop.data)
summary(relogit.extended.region)


#-------------------------
# Models for each region
#-------------------------
# Running multiple models instead of dummies + interactions
# "In general, performing separate regressions is tantamount to including all 
# possible two-way interactions with the community variable (coded as in the 
# second model, not the first) and allowing for different error distributions 
# for each community."
# http://stats.stackexchange.com/questions/17110/should-i-run-separate-regressions-for-every-community-or-can-community-simply-b

# bquote() + .() passes the actual names of the arguments into the model call; eval() actually runs the model
run.logits <- function(region) {
  model <- bquote(glm(lead.coopNGONGOdummy.factor ~ icrg_qog +
                        countngo + uds_mean + demdur + lnpop +
                        lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                        Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                        Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                        Iyeara2002 + Iyeara2003, 
                      data=coop.data, subset=(ht_region==.(region)),
                      family=binomial(link="logit")))
  eval(model)
}

# Create a list of models for all regions, given a model formula
regional.models <- lapply(levels(coop.data$ht_region)[c(2, 3, 4, 7, 6)], FUN=run.logits)
names(regional.models) <- levels(coop.data$ht_region)[c(2, 3, 4, 7, 6)]  # Name the list for convenience
lapply(regional.models, summary)


#--------------------------------------
# Output pretty tables with stargazer
#--------------------------------------
# Original models + extensions
var.names <- c("Quality of government", "Humanitarian intervention", 
               "Aid per capita (ln)", "INGO members/volunteers (ln)", 
               "Media coverage of NGO events", "Democracy", "Unified democracy score (mean)",
               "Years of democratic rule", "Population (ln)", 
               "GDP per capita (ln)", "Disaster in past 5 years", 
               "Civil conflict in past 5 years", 
               "Constant")
col.labels <- c("Original logit", "Original logit (RE)", "Extended logit", 
                "Extended logit (RE)", "Extended logit", 
                "Extended logit (RE)")

stargazer(logit.orig, relogit.orig, logit.extended, relogit.extended,
          logit.extended.region, relogit.extended.region,
          digits=2, star.cutoffs=c(0.05, 0.01, 0.001), title="Determinants of NGO-NGO cooperation",
          covariate.labels=var.names, column.labels=col.labels, model.names=FALSE,
          dep.var.caption="", dep.var.labels.include=FALSE, notes.label="Notes:",
          omit=c("Iyeara*", "ht_region*", "Constant"), omit.stat=c("aic", "ll"), #float.env="sidewaystable",
          type="latex", out="Tables/all_models.tex", font.size="footnotesize", no.space=TRUE,
          add.lines=list(c("Year fixed effects", rep("Yes", 6)),
                         c("Regional fixed effects", rep("No", 4), rep("Yes", 2))),
          se=list(logit.orig.clustered$coefs[,2], NULL, logit.extended.clustered$coefs[,2], NULL, 
                  logit.extended.region.clustered$coefs[,2], NULL),
          p=list(logit.orig.clustered$coefs[,4], NULL, logit.extended.clustered$coefs[,4], NULL, 
                 logit.extended.region.clustered$coefs[,4], NULL),
          notes.align="l", 
          notes=c("Reported coefficients are log odds. Constants have been suppressed.", 
                  "Logistic regression models use robust standard errors clustered by region. 
                  Due to technical differences", "between the implementation of rare event logistic
                  regression in Stata and R, coefficients differ slightly", "from the original paper. 
                  Additionally, rare event models reported here do not use clustered standard errors."))


# Regional models
col.labels <- c(two.lines("Eastern Europe \\&", "Post USSR"), "Latin America", 
                two.lines("North Africa \\&", "Middle East"), 
                two.lines("Sub-Saharan", "Africa"), "Southeast Asia")
var.names <- c("Quality of governance", "Media coverage of NGO events", 
               "Unified democracy score (mean)","Years of democratic rule", 
               "Population (ln)", "GDP per capita (ln)", "Constant")

stargazer(regional.models, type="latex", out="Tables/regional_models.tex", 
          title="Determinants of NGO-NGO cooperation (by region)",
          font.size="footnotesize", no.space=TRUE, omit.stat=c("aic", "ll"), 
          omit=c("Iyeara*", "Constant"), digits=2, star.cutoffs=c(0.05, 0.01, 0.001),
          covariate.labels=var.names, column.labels=col.labels, model.names=FALSE,
          dep.var.caption="", dep.var.labels.include=FALSE,
          add.lines=list(c("Year fixed effects", rep("Yes", 7))), 
          notes.align="l", notes.label="Notes:",
          notes=c("Reported coefficients are log odds. Constants have been suppressed.", 
                  "All models use logistic regression with non-robust, unclustered standard errors."))
