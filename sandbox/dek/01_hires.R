# Investigate hires data

library(oce)
file <- "~/data/arctic/beaufort/2012-hires/201211_SBEDataProc_up_to_the_bin_average/201211-0047.cnv"
d <- read.oce(file)
#if (!interactive()) png("01_hires_plotscan.png")
if (!interactive()) pdf("01_hires_plotscan.pdf")
plotScan(d)
if (!interactive()) dev.off()
dd <- ctdTrim(d, method="downcast")
du <- ctdTrim(d, method="upcast")

#if (!interactive()) png("01_hires_Tprofile.png")
if (!interactive()) pdf("01_hires_Tprofile.pdf")
plotProfile(subset(dd, 300<pressure&pressure<450), xtype="CT")
lines(du[["CT"]], du[["pressure"]], col=2)
legend("bottomleft", lwd=1, col=1:2, legend=c("Downcast", "Upcast"))
if (!interactive()) dev.off()
