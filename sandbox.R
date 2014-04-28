pkgs <- c("Zelig", "stargazer", "car", "sandwich", "lmtest")
suppressPackageStartupMessages(invisible(lapply(pkgs, require, character.only=TRUE)))


# Separation plots
separation.plot(predict(model.logit, type="response"), model.data$lead.coopNGONGOdummy.factor, line=TRUE)
separation.plot(predict(logit.expanded, type="response"), expanded.data$lead.coopNGONGOdummy.factor, line=TRUE)


predicted.values <- predict(model.logit, type="response")
actual.values <- model.data$lead.coopNGONGOdummy.factor



# Predicted probabilities for regions in dictatorships and democracies
X <- with(expanded.data, data.frame(
  icrg_qog=rep(seq(0, 1, 0.05), 2),
  chga_demo=rep(c("0. Dictatorship", "1. Democracy"), each=21),
  Iyeara1990=0, Iyeara1991=0, Iyeara1992=0, Iyeara1993=0, Iyeara1994=0, 
  Iyeara1995=0, Iyeara1996=0, Iyeara1998=0, Iyeara1999=0, Iyeara2000=0,
  Iyeara2001=0, Iyeara2002=0, Iyeara2003=1))

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
pred.region.demo <- p + geom_line(size=2) + facet_wrap(~ chga_demo, nrow=2)
ggsave(pred.region.demo, filename="Figures/pred_region_demo.pdf", width=7, height=4)




stargazer(model.logit, logit.expanded, type="text", omit="Iyeara*", digits=2)

plot(icrg_qog ~ uds_mean, data=coop.data, pch=20)
plot(uds_mean ~ factor(lead.coopNGONGOdummy), data=coop.data, pch=20)
plot(icrg_qog ~ chga_demo, data=coop.data, pch=20)
plot(countngo ~ uds_mean, data=coop.data)


p <- ggplot(coop.data, aes(x=uds_mean, y=countngo))
p + geom_point()

model.nb <- glm(lead.coopNGONGOdummy ~ icrg_qog + humanitarian + 
                  lnaidpercap + lnsmorgs + countngo + chga_demo + lnpop +  
                  lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                  Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                  Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                  Iyeara2002 + Iyeara2003 + disastersample + civilconflictsample, 
                data=coop.data, 
                family = "quasipoisson")
summary(model.nb)



model.nb <- glm(lead.coopNGONGOdummy ~ icrg_qog + humanitarian + 
                  lnaidpercap + lnsmorgs + countngo + chga_demo + lnpop + 
                  lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                  Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                  Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                  Iyeara2002 + Iyeara2003 + disastersample + civilconflictsample, 
                data=coop.data, 
                family = poisson(link="log"))
summary(model.nb)


asdf <- as.data.frame(model.frame(model.nb))

model.nb <- glm.nb(lead.coopNGONGOdummy ~ icrg_qog + factor(humanitarian) + 
                     lnaidpercap + lnsmorgs + countngo + chga_demo + lnpop + 
                     lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                     Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                     Iyeara1998 + Iyeara1999 + Iyeara2000 + yeara2001 + 
                     Iyeara2002 + Iyeara2003 + factor(disastersample) + 
                     factor(civilconflictsample), 
                   data=asdf)
summary(model.nb)

# relogit can't handle factors as dependent variable apparently
#
# Also, coefficients are slightly different in R than in Stata because of algorithm differences
# http://people.iq.harvard.edu/~falimadh/inst/doc/relogit.pdf - page 9
#
# Also, relogit can't cluster (though it can do robust stuff, and method="weave" apparently picks up on latent clusters)
# It should be able to use coeftest.cluster at http://www.people.hbs.edu/igow/GOT/Code/cluster2.R.html but it breaks with this error:
#
#   Error in UseMethod("estfun") : 
#     no applicable method for 'estfun' applied to an object of class "c('zelig', 'relogit')"
#
# This person has the same problem: https://lists.gking.harvard.edu/pipermail/zelig/2013-August/001234.html
#
model.relogit <- zelig(lead.coopNGONGOdummy ~ icrg_qog + humanitarian + 
                         lnaidpercap + lnsmorgs + countngo + chga_demo + lnpop + 
                         lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
                         Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
                         Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
                         Iyeara2002 + Iyeara2003 + factor(disastersample) + 
                         factor(civilconflictsample), 
                       model="relogit", robust=list(method="weave"), data=coop.data)
summary(model.relogit)



# Just robust SEs (no clustering)
# coeftest(model.logit, vcov=vcovHC(model.logit, type="HC0"))

# Clustering + robust
robust.se <- function(dat, model, cluster){
  attach(dat, warn.conflicts = F)
  require(sandwich)
  require(lmtest)
  not <- attr(model$model,"na.action")
  
  if( ! is.null(not)){   # only drop the NA values if there are any left
    cluster <- cluster[-not]
    dat <- dat[-not,]
  }
  
  with(dat,{
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
    coeftest(model, vcovCL, type="HC1")  # HC1 or HC0 are close to Stata
  }
  )
}


model.robust <- robust.se(coop.data, model.logit, coop.data$cowcode)
model.rerobust <- robust.se(coop.data, model.relogit, coop.data$cowcode)  # Can't cluster with relogit?


example <- data.frame(year=rep(c(1989, 1990, 1991, 1992, 1993), times=3),
                      polity2=c(-5, -5, -4, -3, 1, 4, 5, 6, 7, 8, 2, 3, 4, 5, 6),
                      cowcode=rep(c(2, 20, 40), each=5))
example

example %.%
  group_by(cowcode) %.%
  mutate(demdur=demdur.calc(polity2))





demdur(example$polity2)


filter(ht_region != "5. Western Europe and North America") %.%  # Exclude region
  group_by(cowcode) %.%  # Separate into country chunks
  mutate(lead.coopNGONGOdummy=lead(coopNGONGOdummy)) %.%  # Create lead variable
  mutate(Iyeara=factor(year))  # Make a factor
