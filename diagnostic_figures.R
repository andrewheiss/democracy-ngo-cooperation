# Load libraries and data
source("load_data.R")
library(ggplot2)
library(reshape2)


#---------------------------
# Initial diagnostic plots
#---------------------------
plot.demdur <- ggplot(filter(coop.data, demdur > 0), aes(x=demdur, fill=ht_region)) + 
  geom_histogram(binwidth=1) + 
  coord_cartesian(xlim=c(0, 70)) + 
  labs(x="Years of democracy", y="Count") + 
  guides(fill=guide_legend(nrow=3, title=NULL)) +
  theme_ath(8)
plot.demdur
ggsave(plot.demdur, filename="Figures/demdur.pdf", width=5, height=4, units="in")


plot.uds <- ggplot(coop.data, aes(x=uds_mean, fill=ht_region)) + 
  geom_histogram(binwidth=0.05) + 
  labs(x="UDS (mean)", y="Count") + 
  guides(fill=guide_legend(nrow=3, title=NULL)) +
  theme_ath(8)
plot.uds
ggsave(plot.uds, filename="Figures/uds_dist.pdf", width=5, height=4, units="in")
