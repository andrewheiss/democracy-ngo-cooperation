# Load libraries and data
source("models.R")
library(ggplot2)
library(scales)
library(reshape2)


#------------------------------------------
# Predicted probabilities across icrg_qog
#------------------------------------------
# Extract data from model
model.data <- model.frame(logit.orig)

# Sample data, with full range of qog for dictatorships and democracies
X <- with(model.data, data.frame(
  icrg_qog=rep(seq(0, 1, 0.05), 2), humanitarian.factor=factor(0), 
  lnaidpercap=mean(lnaidpercap), lnsmorgs=mean(lnsmorgs), countngo=mean(countngo),
  chga_demo=rep(c("Dictatorship", "Democracy"), each=21), 
  lnpop=mean(lnpop), lngdppercap=mean(lngdppercap), Iyeara1990=0, Iyeara1991=0,
  Iyeara1992=0, Iyeara1993=0, Iyeara1994=0, Iyeara1995=0, Iyeara1996=0, 
  Iyeara1998=0, Iyeara1999=0, Iyeara2000=0, Iyeara2001=0, Iyeara2002=0, 
  Iyeara2003=1, disastersample.factor=factor(1), 
  civilconflictsample.factor=factor(1)))

# For manual calculation of predicted robust standard errors
X.matrix <- cbind(1, data.matrix(X))

# Predict using sample data
predicted.logit <- cbind(X, predict(logit.orig, newdata=X, type="link", se=TRUE))
predicted.logit <- predicted.logit %.%
  mutate(pred.prob=plogis(fit)) %.%
  mutate(se.rob=sqrt(diag(X.matrix %*% logit.orig.clustered$clcov %*% t(X.matrix)))) %.%
  mutate(LL=plogis(fit - (1.96 * se.rob))) %.%
  mutate(UL=plogis(fit + (1.96 * se.rob)))

# Plot predicted probabilities with SEs
p <- ggplot(predicted.logit, aes(x=icrg_qog, y=pred.prob))
pred.demo <- p + geom_ribbon(aes(ymin=LL, ymax=UL, fill=chga_demo), alpha=.2) + 
  geom_line(aes(colour=chga_demo), size=1) + scale_y_continuous(label=percent) + 
  labs(x="Quality of governance (ICRG)", y="Predicted probability of cooperation") + 
  theme_ath(8) + theme(legend.title=element_blank())
pred.demo
ggsave(pred.demo, filename="Figures/pred_demo.pdf", width=6, height=3.5)


#---------------------------------------------------------------------
# Predicted probabilities across icrg_qog for three values of demdur
#---------------------------------------------------------------------
# Extract data from expanded model
extended.data <- model.frame(logit.extended)

# Sample data, with full range of qog for dictatorships and democracies
X <- with(extended.data, data.frame(
#   icrg_qog=mean(icrg_qog), 
  icrg_qog=rep(seq(0, 1, 0.05), 2),
  humanitarian.factor=factor(0), lnaidpercap=mean(lnaidpercap),
  lnsmorgs=mean(lnsmorgs), countngo=mean(countngo), uds_mean=mean(uds_mean),#uds_mean=seq(0, 1.5, 0.05), 
  lnpop=mean(lnpop), lngdppercap=mean(lngdppercap), 
  Iyeara1990=0, Iyeara1991=0, Iyeara1992=0, Iyeara1993=0, Iyeara1994=0, 
  Iyeara1995=0, Iyeara1996=0, Iyeara1998=0, Iyeara1999=0, Iyeara2000=0,
  Iyeara2001=0, Iyeara2002=0, Iyeara2003=1, disastersample.factor=factor(1), 
  civilconflictsample.factor=factor(1), demdur=rep(c(0, 10, 100), each=42)))  # demdur=rep(seq(0, 20, 1), each=61)

# Predict using sample data
predicted.logit <- cbind(X, predict(logit.extended, newdata=X, type="link", se=TRUE))
predicted.logit <- predicted.logit %.%
  mutate(pred.prob=plogis(fit)) %.%
  mutate(LL=plogis(fit - (1.96 * se.fit))) %.%
  mutate(UL=plogis(fit + (1.96 * se.fit)))

# Plot predicted probabilities with SEs
p <- ggplot(predicted.logit, aes(x=icrg_qog, y=pred.prob))
pred.demdur <- p + geom_ribbon(aes(ymin=LL, ymax=UL, fill=factor(demdur)), alpha=.2) + 
  geom_line(aes(colour=factor(demdur)), size=1) + scale_y_continuous(label=percent) +
  labs(x="Quality of governance (ICRG)", y="Predicted probability of cooperation") + 
  scale_colour_discrete(name="Years of consecutive democracy") + 
  scale_fill_discrete(name="Years of consecutive democracy") + 
  theme_ath(8)
pred.demdur
ggsave(pred.demdur, filename="Figures/pred_demdur.pdf", width=6, height=3.5)


#---------------------------------------------------------
# Predicted probabilities for regional models across UDS
#---------------------------------------------------------
extended.data <- model.frame(logit.extended)

X <- with(extended.data, data.frame(
  icrg_qog=mean(icrg_qog), countngo=mean(countngo),
  uds_mean=rep(seq(-2, 2, 0.05), 2), demdur=mean(demdur),
  lnpop=mean(lnpop), lngdppercap=mean(lngdppercap),
  Iyeara1990=0, Iyeara1991=0, Iyeara1992=0, Iyeara1993=0, Iyeara1994=0, 
  Iyeara1995=0, Iyeara1996=0, Iyeara1998=0, Iyeara1999=0, Iyeara2000=0,
  Iyeara2001=0, Iyeara2002=0, Iyeara2003=1))

sim.pred <- lapply(regional.models, predict, newdata=X, type="link", se.fit=TRUE)

pred.plot <- ldply(sim.pred, data.frame) %.%
  mutate(uds_mean=rep(seq(-2, 2, 0.05), 10)) %.%
  mutate(demdur=rep(rep(c(1, 10), each=81), 5)) %.%
  mutate(pred.prob=plogis(fit)) %.%
  mutate(LL=plogis(fit - (1.96 * se.fit))) %.%
  mutate(UL=plogis(fit + (1.96 * se.fit))) %.%
  mutate(region=factor(.id)) %.%
  mutate(pred.prob.adj=ifelse(region=="Eastern Europe and post Soviet Union", 
                              pred.prob + .003, pred.prob))

pred.region.uds <- ggplot(pred.plot, aes(x=uds_mean, y=pred.prob.adj, color=region)) + 
  geom_line(size=1) + scale_y_continuous(label=percent) +
  labs(x="Unified democracy score", y="Predicted probability of cooperation") + 
  guides(colour=guide_legend(nrow=2, title=NULL)) +
  theme_ath(8) + theme(legend.key=element_blank())
pred.region.uds
ggsave(pred.region.uds, filename="Figures/pred_region_uds.pdf", width=6, height=3.5)
