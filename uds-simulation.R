# Load libraries and data
source("load_data.R")
library(ggplot2)
library(reshape2)

#----------------------
# Simulate UDS scores
#----------------------
set.seed(1234)
simulations <- 100

# Extract means and standard deviation into individual vectors for speed
uds.means <- coop.data$uds_mean
uds.sds <- coop.data$uds_sd

# Create a dataframe with a column per simulated score
simulated.scores <- ldply(lapply(1:nrow(coop.data), function(i) rnorm(simulations, uds.means[i], sd=uds.sds[i])))
colnames(simulated.scores) <- paste0("uds.sim", 1:simulations)

# Add simulations to main dataframe
coop.data.sim <- cbind(coop.data, simulated.scores)

# Run a model for each simulated UDS column
uds.simulate <- function(sim.column) {
  model.formula <- as.formula(paste("lead.coopNGONGOdummy.factor ~ 
    icrg_qog + humanitarian.factor +", sim.column,
    "+ lnaidpercap + lnsmorgs + countngo + lnpop +
    lngdppercap + Iyeara1990 + Iyeara1991 + Iyeara1992 + 
    Iyeara1993 + Iyeara1994 + Iyeara1995 + Iyeara1996 + 
    Iyeara1998 + Iyeara1999 + Iyeara2000 + Iyeara2001 + 
    Iyeara2002 + Iyeara2003 + disastersample.factor + 
    civilconflictsample.factor + demdur"))
  model <- glm(model.formula, data=coop.data.sim, family=binomial(link="logit"))
  return(summary(model)$coefficients)
}

# Run the simulated model function using uds.sim1-n
simulated.coefs <- lapply(paste0("uds.sim", 1:simulations), FUN=uds.simulate)

# Convert list of dataframes to a long dataframe and clean it up
simulated.coefs.long <- melt(simulated.coefs) %.%
  mutate(coef.name=factor(sub("uds.sim\\d+", "uds.sim", Var1))) %.%
  select(coef.name, parameter=Var2, estimate=value, simulation=L1)

coefs.err <- simulated.coefs.long %.%
  filter(parameter=="Std. Error" & (coef.name=="uds.sim" | coef.name=="icrg_qog"))

std.errs <- coefs.err$estimate

# Select data to plot
coefs.plot <- simulated.coefs.long %.%
  filter(parameter=="Estimate" & (coef.name=="uds.sim" | coef.name=="icrg_qog")) %.%
  # filter(!grepl("yeara", coef.name) & coef.name != "(Intercept)") %.%
  mutate(std.err=std.errs) %.%
  mutate(upper=estimate + (std.err * 1.96)) %.%
  mutate(lower=estimate - (std.err * 1.96)) %.%
  mutate(coef.name=factor(coef.name, 
                          labels=c("Quality of governance", "Unified democracy score (UDS)"))) %.%
  mutate(odds.ratio=1-exp(estimate)) %.%
  arrange(estimate)

# Plot simulated coefficients of interest
uds.sim <- ggplot(coefs.plot, aes(y=estimate, x=coef.name, fill=coef.name)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), size=4, alpha=0.008, color="black") + 
  geom_violin(scale="width", colour=NA) + geom_hline(yintercept=0) + coord_flip() + 
  labs(x=NULL, y="Coefficient estimate (log odds)") + theme_ath(8) + 
  guides(fill=FALSE)
uds.sim
ggsave(uds.sim, filename="Figures/simulated_uds.pdf", width=6, height=3.5)
