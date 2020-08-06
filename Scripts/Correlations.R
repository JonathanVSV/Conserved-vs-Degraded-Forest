library(corrplot)

# Read data-------------------------------------------
df <- read.csv("plots_data.csv")

# Conserved and Degraded forest plots (all together)-----------------------
## Get variables to perform correlation
df_all <- as.matrix(df[,c("Cobertura","AB_m2.ha","biomasa_mg.ha","Total_ramas","total_arboles")])

## Get correlation matrix
cor(df_all, method = "kendall")

## Get significance values of the correlations
cor.mtest(df_all)$p

# Conserved forest plots -----------------------------
## Get variables to perform correlation, subsetting data to conserved forest plots
conserved <- df$tipo_bosque == "conserved"
df_conserved <- as.matrix(df[conserved,c("Cobertura","AB_m2.ha","biomasa_mg.ha","Total_ramas","total_arboles")])

## Get correlation matrix
cor(df_conserved, method = "kendall")

## Get significance values of the correlations
cor.mtest(df_conserved)$p

# Degraded forest plots --------------------------------
## Get variables to perform correlation, subsetting data to degraded forest plots
degraded <- df$tipo_bosque == "degraded"
df_degraded <- as.matrix(df[degraded,c("Cobertura","AB_m2.ha","biomasa_mg.ha","Total_ramas","total_arboles")])

## Get correlation matrix
cor(df_degraded, method = "kendall")

## Get significance values of the correlations
cor.mtest(df_degraded)$p


