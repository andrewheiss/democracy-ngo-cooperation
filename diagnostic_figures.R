# Load libraries and data
source("load_data.R")
library(ggplot2)
library(grid)

#---------------------------
# Initial diagnostic plots
#---------------------------
plot.demdur <- ggplot(filter(coop.data, demdur > 0), aes(x=demdur, fill=ht_region)) + 
  geom_histogram(binwidth=1) + 
  coord_cartesian(xlim=c(0, 70)) + 
  labs(x="Years of democracy", y="Count") + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", 
                             "#999999", "#a65628", "#f781bf", "#ffff33")) + 
  guides(fill=guide_legend(nrow=3, title=NULL)) +
  theme_ath(8) + theme(legend.key.size=unit(0.5, "lines"))
plot.demdur
ggsave(plot.demdur, filename="Figures/demdur.pdf", width=5, height=4, units="in")


plot.uds <- ggplot(coop.data, aes(x=uds_mean, fill=ht_region)) + 
  geom_histogram(binwidth=0.05) + 
  labs(x="UDS (mean)", y="Count") + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", 
                             "#999999", "#a65628", "#f781bf", "#ffff33")) + 
  guides(fill=guide_legend(nrow=3, title=NULL)) +
  theme_ath(8) + theme(legend.key.size=unit(0.5, "lines"))
plot.uds
ggsave(plot.uds, filename="Figures/uds_dist.pdf", width=5, height=4, units="in")
