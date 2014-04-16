separation.plot <- function(predicted.values, actual.values, line=FALSE) {
  
  # Build separation plot data frame
  separation.frame <- data.frame(predicted=predicted.values, actual=actual.values)
  separation.frame <- separation.frame %.%
    arrange(predicted) %.%
    mutate(position=1:nrow(separation.frame))
  
  marker.position <- nrow(separation.frame) - round(sum(separation.frame$predicted))
  
  # sum under the phat curve:
  expectedevents<-round(sum(separation.frame$predicted))
  
  # calculate the new cutpoint:
  newcutpoint<-sort(separation.frame$predicted, decreasing=T)[expectedevents]
  
  # calculate the proportion correctly predicted:
  tp <- length(separation.frame$actual[separation.frame$predicted>=newcutpoint &
                                       separation.frame$actual==1])
  fp <- length(separation.frame$actual[separation.frame$predicted>=newcutpoint &
                                       separation.frame$actual==0])
  tn <- length(separation.frame$actual[separation.frame$predicted<newcutpoint & 
                                       separation.frame$actual==0])
  fn <- length(separation.frame$actual[separation.frame$predicted<newcutpoint & 
                                       separation.frame$actual==1])
  
  pcp <- (tp+tn)/length(separation.frame$actual)
  print(pcp)
  
  theme_sep <- theme_bw() + theme(line=element_blank(), rect=element_blank(),
                                  strip.text=element_blank(), 
                                  axis.text=element_blank(), 
                                  axis.text=element_blank(), 
                                  plot.title=element_blank(),
                                  axis.title=element_blank(), 
                                  plot.margin=structure(c(0, 0, -1, -1), 
                                                        unit = "lines", 
                                                        valid.unit = 3L, 
                                                        class = "unit"))
  
  p <- ggplot(separation.frame)
  p + geom_rect(aes(xmin=position - 0.5, xmax=position + 0.5, ymin=0, ymax=1, fill=actual)) + 
    geom_line(aes(x=position, y=predicted)) + 
    scale_fill_manual(values=c("lightyellow", "red"), guide=FALSE) +
    annotate("point", x=marker.position, y=-0.05, pch=17, size=4, colour="red") + 
    theme_empty

}
