library(corrplot)
library(Hmisc)

# Read data-------------------------------------------
df <- read.csv("plots_data.csv")

## Get variables to perform correlation
df <- as.matrix(df[,c("Cobertura","AB_m2.ha","biomasa_mg.ha","Total_ramas","total_arboles")])

## Get correlation matrix
M<-cor(df, method = "kendall")
M

## Get significance values of the correlations
p.mat<-cor.mtest(df)$p
p.mat

# Do corrplot-----------------------------------------------
## Get corrplot using the two previously calculated matrices
corrplot(M, method="circle", type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.05)

## Save as png
png("Corr_Environ.png",
    width = 10,
    height = 12,
    #pointsize = 300,
    res = 300,
    units = "cm",
    type = "cairo")
corrplot(M, method="circle", type="upper", order="hclust",
         p.mat = p.mat, sig.level = 0.05,tl.cex = 1, number.cex = 1,cl.cex = 1, pch.cex = 1)
dev.off()

