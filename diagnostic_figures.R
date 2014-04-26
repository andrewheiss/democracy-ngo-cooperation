# Load libraries and data
source("load_data.R")
library(ggplot2)
library(reshape2)


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


p <- ggplot(coop.data, aes(x=lngdppercap, y=icrg_qog, color=poor.dem))
p + geom_point()


plot.demdur <- ggplot(filter(coop.data, demdur > 0), aes(x=demdur, fill=ht_region)) + 
  geom_histogram(binwidth=1) + 
  coord_cartesian(xlim=c(0, 70)) + 
  labs(x="Years of democracy", y="Count") + 
  theme_bw() + theme(axis.title=element_text(vjust=0.2))
plot.demdur


plot.uds <- ggplot(coop.data, aes(x=uds_mean, fill=ht_region)) + 
  geom_histogram(binwidth=0.1) + 
  labs(x="UDS (mean)", y="Count") + 
  theme_bw() + theme(axis.title=element_text(vjust=0.2))
plot.uds
