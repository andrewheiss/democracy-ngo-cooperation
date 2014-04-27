# Load libraries and data
source("load_data.R")
library(ggplot2)
library(reshape2)

theme_ath <- function(base_size=12) {
  ret <- theme_bw(base_size) + 
    theme(axis.title=element_text(vjust=0.2), legend.position="bottom")
  ret
}

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
