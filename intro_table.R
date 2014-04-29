library(dplyr)
library(stargazer)
library(pander)

levels(coop.data$uds_country)

thing <- coop.data %.%
  filter(uds_country=="China" | uds_country=="Turkey") %.%
  select(uds_country, ht_region, icrg_qog, coopNGONGOdummy)
  
t.test(icrg_qog ~ uds_country, data=thing)
t.test(coopNGONGOdummy ~ uds_country, data=thing)


information <- coop.data %.%
  filter(uds_country=="China" | uds_country=="Turkey" | uds_country=="Japan" | 
           uds_country=="Saudi Arabia" | uds_country=="Korea South" | uds_country=="Israel") %.%
  group_by(uds_country) %.%
  summarize(region=ht_region[1],
            regime=chga_demo[1],
            gov=mean(icrg_qog, na.rm=TRUE), 
            coop.years=sum(coopNGONGOdummy, na.rm=TRUE)) %.%
  arrange(region, desc(coop.years))

colnames(information) <- c("Country", "Region", "Regime type", "Governance (ICRG)", "Years of cooperation")

cat(pandoc.table.return(information, digits=3, split.tables=Inf, 
                        emphasize.strong.rows=c(1, 5), 
                        caption="Quality of governance (0–1 scale) and years of NGO cooperation in select countries",
                        justify=c("left", "left", "left", "center", "center")), 
    file="Tables/intro_table.md")
