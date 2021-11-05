# Returns a histogram of the ME and MAE of the SEFM models
rm(list = ls()) ; cat("\014")

# load data on MAE and ME ("perf.eval.out")
load(paste0(getwd(), "/R/summary/performances.Rdata"))

#### 1. SEFM ----

# MAE (mean absolute error)
mae <- perf.eval.out$SEFM_MAE
names(mae) <- rownames(perf.eval.out)
sort(mae) # min: 0.5539 (Comoros), max: 43.634 (Libya)
mean(mae) # 4.391

mae.no.libya <- mae[-which(names(mae) == "LIBYA")]
mean(mae.no.libya) # 3.624

# make a histogram
library(ggplot2)

pdf(file = paste0(getwd(), "/R/histograms/plots/SEFM_MAE.pdf"))
gg <- ggplot(data.frame(mae.no.libya = mae.no.libya), aes(x = mae.no.libya)) + 
  geom_histogram(color = "black", fill = "gray", bins = 15) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) +
  ylim(0, 13)
print(gg)
dev.off()


# ME (mean error)
me <- perf.eval.out$SEFM_ME
names(me) <- rownames(perf.eval.out)
sort(me) # min: -8.387 (EqGuinea), max: 7.361 (IvoryCoast)
mean(me) # -0.344

# make a histogram
pdf(file = paste0(getwd(), "/R/histograms/plots/SEFM_ME.pdf"))
gg <- ggplot(data.frame(me = me), aes(x = me)) + 
  geom_histogram(color = "black", fill = "gray", bins = 12) +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) +
  ggtitle("ME of SEFM") + 
  xlim(-10, 10)
print(gg)
dev.off()



# MAE (mean absolute error)
mae <- perf.eval.out$SEFM_MAE
names(mae) <- rownames(perf.eval.out)
sort(mae) # min: 0.5539 (Comoros), max: 43.634 (Libya)
mean(mae) # 4.391

mae.no.libya <- mae[-which(names(mae) == "LIBYA")]
mean(mae.no.libya) # 3.624

# make a histogram
library(ggplot2)

pdf(file = paste0(getwd(), "/R/histograms/plots/SEFM_MAE.pdf"))
gg <- ggplot(data.frame(mae.no.libya = mae.no.libya), aes(x = mae.no.libya)) + 
  geom_histogram(color = "black", fill = "gray", bins = 15) +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) +
  ggtitle("MAE of SEFM")
print(gg)
dev.off()


### 2. benchmark models ----
methods <- c("AR1", "MA1", "PCR", "RW")

for(method in methods){
  
  if(method == "AR1"){
    nam <- "AR(1)"
  } else if(method == "MA"){
    nam <- "IMA(1,1)"
  } else{
    nam <- method
  } 
  
  
  # ME (mean error)
  me <- perf.eval.out[, paste0(method, "_ME")]
  
  # make a histogram
  pdf(file = paste0(getwd(), "/R/histograms/plots/", method, "_ME.pdf"))
  gg <- ggplot(data.frame(me = me), aes(x = me)) + 
    geom_histogram(color = "black", fill = "gray", bins = 12) +
    theme_bw() +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14)) +
    ggtitle(paste0(" ME of ", nam))
  print(gg)
  dev.off()
  
  
  # MAE 
  mae <- perf.eval.out[, paste0(method, "_ME")]
  mae.no.libya <- mae[-which(names(mae) == "LIBYA")]

  # make a histogram
  pdf(file = paste0(getwd(), "/R/histograms/plots/", method, "_MAE.pdf"))
  gg <- ggplot(data.frame(mae.no.libya = mae.no.libya), aes(x = mae.no.libya)) + 
    geom_histogram(color = "black", fill = "gray", bins = 12) +
    theme_bw() +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14)) +
    ggtitle(paste0(" MAE of ", nam))
  print(gg)
  dev.off()
  
}

