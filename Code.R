########################################################
# Writing Sample
# TIMSS 2011 8th Grade Mathematics Analysis
# Scarlett Escudero
# PhD Application
# Date (version): 2024/November
########################################################

#===============================================================================
# 1. Packages and data loading
#===============================================================================

library(foreign)
library(readxl)
library(dplyr)
library(naniar)
library(CTT)
library(psych)
library(ggplot2)
library(ggthemes)
library(lavaan)
library(mirt)
library(PerFit)
library(difR)
source("extract_fit.R")
load("DataAn_TIMSS2011.RData")

######################
# South Africa       #
######################
test <- readxl::read_excel("T11_G8_ItemInformation.xlsx", sheet = "MAT")
item_ids <- test %>%
  filter(Block %in% c("M05", "M06")) %>%
  pull(`Item ID`)

southaf <- read.spss("bsazafm5.sav")
southaf <- as.data.frame(southaf[c("IDCNTRY","IDBOOK","ITSEX", item_ids)]) #Choose the columns needed
southaf <- southaf %>% filter(IDBOOK %in% c(5)) #Choose only Book 5
southaf$IDCNTRY <- "South Africa"
southaf <- southaf %>%
  mutate(across(4:ncol(.), ~ case_when(
    . %in% c("A*","B*","C*","D*") ~ 1,
    . %in% c("CORRECT RESPONSE") ~ 1,
    . %in% c("A","B","C","D","INCORRECT RESPONSE","OMITTED","NOT REACHED","INCORRECT RESPONSE_duplicated_71","INCORRECT RESPONSE_duplicated_79","PARTIALLY CORRECT RESPONSE","PARTIALLY CORRECT RESPONSE_duplicated_11") ~ 0,
    TRUE ~ NA_real_ 
  ), .names = "{.col}_r"))

southaf <- southaf %>% arrange(IDBOOK) #Total 859 individuals
vis_miss(southaf)
dim(southaf)

######################
# Canada             #
######################
test <- readxl::read_excel("T11_G8_ItemInformation.xlsx", sheet = "MAT")
item_ids <- test %>%
  filter(Block %in% c("M05", "M06")) %>%
  pull(`Item ID`)

canada_que <- read.spss("bsacqum5.sav")
canada_que <- as.data.frame(canada_que[c("IDCNTRY","IDBOOK","ITSEX", item_ids)]) #Choose the columns needed
canada_que <- canada_que %>% filter(IDBOOK %in% c(5)) #Choose only Book 5 
canada_que$IDCNTRY <- "Canada"
canada_que <- canada_que %>%
  mutate(across(4:ncol(.), ~ case_when(
    . %in% c("A*","B*","C*","D*") ~ 1,
    . %in% c("CORRECT RESPONSE") ~ 1,
    . %in% c("A","B","C","D","INCORRECT RESPONSE","OMITTED","NOT REACHED","INCORRECT RESPONSE_duplicated_71","INCORRECT RESPONSE_duplicated_79","PARTIALLY CORRECT RESPONSE","PARTIALLY CORRECT RESPONSE_duplicated_11") ~ 0,
    TRUE ~ NA_real_ 
  ), .names = "{.col}_r"))

canada_ont <- read.spss("bsacotm5.sav")
canada_ont <- as.data.frame(canada_ont[c("IDCNTRY","IDBOOK","ITSEX", item_ids)]) #Choose the columns needed
canada_ont <- canada_ont %>% filter(IDBOOK %in% c(5)) #Choose only Books 5
canada_ont$IDCNTRY <- "Canada"
canada_ont <- canada_ont %>%
  mutate(across(4:ncol(.), ~ case_when(
    . %in% c("A*","B*","C*","D*") ~ 1,
    . %in% c("CORRECT RESPONSE") ~ 1,
    . %in% c("A","B","C","D","INCORRECT RESPONSE","OMITTED","NOT REACHED","INCORRECT RESPONSE_duplicated_71","INCORRECT RESPONSE_duplicated_79","PARTIALLY CORRECT RESPONSE","PARTIALLY CORRECT RESPONSE_duplicated_11") ~ 0,
    TRUE ~ NA_real_ 
  ), .names = "{.col}_r"))

canada_alb <- read.spss("bsacabm5.sav")
canada_alb <- as.data.frame(canada_alb[c("IDCNTRY","IDBOOK","ITSEX", item_ids)]) #Choose the columns needed
canada_alb <- canada_alb %>% filter(IDBOOK %in% c(5)) #Choose only Books 5
canada_alb$IDCNTRY <- "Canada"
canada_alb <- canada_alb %>%
  mutate(across(4:ncol(.), ~ case_when(
    . %in% c("A*","B*","C*","D*") ~ 1,
    . %in% c("CORRECT RESPONSE") ~ 1,
    . %in% c("A","B","C","D","INCORRECT RESPONSE","OMITTED","NOT REACHED","INCORRECT RESPONSE_duplicated_71","INCORRECT RESPONSE_duplicated_79","PARTIALLY CORRECT RESPONSE","PARTIALLY CORRECT RESPONSE_duplicated_11") ~ 0,
    TRUE ~ NA_real_ 
  ), .names = "{.col}_r"))

canada <- rbind(canada_alb,canada_ont,canada_que)
canada <- canada %>% arrange(IDBOOK) #Total 1118 individuals
vis_miss(canada)

######################
# Complete dataset   #
######################
data <- rbind(canada, southaf)
data_com <- data[,c(1:3,36:67)]
data <- data[,c(36:67)]

item_ids_Book5 <- test %>%
  filter(Block %in% c("M05", "M06")) %>%
  pull(`Item ID`) %>%
  paste0("_r")
data_com_Book5 <- data_com[,c("IDCNTRY","IDBOOK","ITSEX",item_ids_Book5)] %>%  filter(IDBOOK==5)

#item_ids_Book6 <- test %>%
#  filter(Block %in% c("M06", "M07")) %>%
#  pull(`Item ID`) %>%
#  paste0("_r")
#data_com_Book6 <- data_com[,c("IDCNTRY","IDBOOK","ITSEX",item_ids_Book6)] %>%  filter(IDBOOK==6)

######################
# Only responses     #
######################
data_com_Book5_resp <- data_com_Book5[,-(1:3)]
#data_com_Book6_resp <- data_com_Book6[,-(1:3)]

#===============================================================================
# 2. Descriptive statistics
#===============================================================================

dim(canada %>% filter(IDBOOK==5)) #1118 in Book 5 in Canada
dim(southaf %>% filter(IDBOOK==5)) #859 in Book 5 in South Africa
dim(canada %>% filter(ITSEX=="BOY")) #548 boys in Canada
dim(canada %>% filter(ITSEX=="GIRL")) #569 girls in Canada
dim(southaf %>% filter(ITSEX=="GIRL")) #430 girls in South Africa
dim(southaf %>% filter(ITSEX=="BOY")) #429 boys in South Africa

#===============================================================================
# 3. Classic Psychometric Analysis and Unidimensionality
#===============================================================================
######################
# Item Analysis      #
######################
iteman_1 <- CTT::itemAnalysis(items = data[complete.cases(data[,1:14]), 1:14], itemReport = TRUE, hardFlag = 0.35, pBisFlag = 0.2)$itemReport 
iteman_2 <- CTT::itemAnalysis(items = data[complete.cases(data[,33:47]), 33:47], itemReport = TRUE, hardFlag = 0.35, pBisFlag = 0.2)$itemReport
iteman_3 <- CTT::itemAnalysis(items = data[,15:32], itemReport = TRUE, hardFlag = 0.35, pBisFlag = 0.2)$itemReport

iteman <- rbind(iteman_1,iteman_3,iteman_2)
describe(iteman)
mean(iteman$itemMean)
sd(iteman$itemMean)
mean(iteman$pBis)
sd(iteman$pBis)


######################
# Cronbach's a       #
######################
out.fia_book5 <- psych::alpha(data_com_Book5[,-c(1:3)], na.rm=TRUE)
round(out.fia_book5$total$raw_alpha, 3) # Cronbach's alpha for Book 5 test

out.fia_book6 <- psych::alpha(data_com_Book6[,-c(1:3)], na.rm=TRUE)
round(out.fia_book6$total$raw_alpha, 3) # Cronbach's alpha for Book 6 test


# NOTE: We conduct the following analyzes on Book 5 only but both countries
######################
# Unidimensionality  #
######################

# Traditional unidimensionality -----
out.pa <- fa.parallel(x = data_com_Book5_resp,
  fa ="pc",
  cor = "poly",
  sim = FALSE,
  correct = FALSE,
  quant = 0.95)

eigenvalues <- data.frame(emp=round(out.pa$pc.values,3),
  mean=round(apply(out.pa$values,2,mean),3),
  pc95=round(apply(out.pa$values,2,quantile,.95),3))
emp <- eigenvalues$emp
e.mean <- eigenvalues$mean
J <- ncol(data_com_Book5_resp)
df.e.values <- data.frame(dataeigen = c(rep("emp",J),rep("e.mean",J)),
 J = rep(1:J, 2), eigen = c(as.numeric(t(emp)), as.numeric(t(e.mean))))

point <- ifelse(grepl("emp", df.e.values$dataeigen), 16, 4)
pa <- ggplot(df.e.values, aes(x = J, y = eigen, color = dataeigen, shape = dataeigen)) +
  geom_line() +
  geom_point() + 
  xlim(0,15) +
  labs(x = "Component number", y = "Eigenvalues") +
  theme_few() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_blank(),
    legend.position = c(0.9, 1),  
    legend.justification = c(0.6, 1),  
    legend.background = element_rect(fill = "lightgray"),
    legend.text = element_text(size = 16)) +
  scale_color_manual(
    labels = c("PC empirical data", "PC simulated data"), 
    values = c("emp" = "blue", "e.mean" = "red")) +
  scale_shape_manual(
    labels = c("PC empirical data", "PC simulated data"), 
    values = c("emp" = 16, "e.mean" = 4)) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 4)))); pa

png("PA.png", width = 10, height = 5, res = 600, units = "in")
pa
dev.off()

round(out.pa$pc.values[1]/out.pa$pc.values[2],3) # Ratio of 8.273
out.pa$pc.values[1]/32 # 0.4292 of variance explained by the first component

# CFA unidimensionality -----
CFAmodel <- paste("F1 =~ NA*",
  paste(names(data_com_Book5_resp),collapse="+"),
  "\nF1 ~~ 1*F1",sep="")

out.CFA <- lavaan(data = data_com_Book5_resp, 
  model = CFAmodel, 
  ordered = names(data_com_Book5_resp))

mod <- modificationindices(out.CFA)
mod[order(mod$mi, decreasing = TRUE), ]#There is a Heywood case, so we must eliminate M042300a, M042300b and M042300z (they are all related)

CFAmodel2 <- "
          F1 =~ M032094_r + M032662_r + M032064_r + M032419_r + M032477_r + M032538_r+M032324_r + M032116_r + M032100_r + M032402_r + M032734_r + M032397_r + M032695_r + M032132_r + M042041_r + M042024_r + M042016_r + M042002_r + M042198A_r + M042198B_r+M042198C_r + M042077_r + M042235_r + M042067_r + M042150_r + M042260_r + M042169A_r +M042169B_r + M042169C_r
          F1 ~~ 1*F1"

out.CFA2 <- lavaan(data = data_com_Book5_resp[,-c(26,27,28)], 
  model = CFAmodel2,
  ordered = names(data_com_Book5_resp[,-c(26,27,28)]))

summary(object = out.CFA2, fit.measures = TRUE, standardized = TRUE, rsq=TRUE) 
fitmeasures(object = out.CFA2,fit.measures = c("RMSEA.robust","CFI.robust","TLI.robust")) 
round(lavResiduals(out.CFA2)$summary["usrmr","cor"],3)
0.05*mean(inspect(out.CFA2,"rsquare"))
0.10*mean(inspect(out.CFA2,"rsquare"))

loadings <- inspect(object = out.CFA2,
  what="std")$lambda; loadings 

sum(inspect(object = out.CFA2, what = "rsquare"))/29 # Proportion of explained variance by the factor (con un factor es igual al promedio de las comunalidades) # It's high (over 20%)

# Residuals (identify problematic pairs):
matrix_to_table <- function(x) {
  x[upper.tri(x)] <- NA
  inds <- which(!is.na(abs(x)), arr.ind =TRUE);inds
  tablax <- as.data.frame(list(var1   = rownames(x)[inds[,1]],
    var2   = colnames(x)[inds[,2]],
    resid = round(x[inds],3)))
  tablax$z <- round(scale(tablax$resid),3) 
  tablax
}

table_resid <- matrix_to_table(resid(out.CFA2)$cov)
hist(table_resid$z) # Residual distribution
table_resid[order(abs(table_resid$resid),decreasing =TRUE),][1:10,]

# We eliminate one by one each problematic residual:
CFAmodel3 <- "
          F1 =~ M032094_r + M032662_r + M032064_r + M032419_r + M032477_r + M032538_r+ M032324_r + M032116_r + M032100_r + M032402_r + M032734_r + M032397_r + M032695_r + M032132_r + M042041_r + M042024_r + M042016_r + M042002_r + M042198C_r + M042077_r + M042235_r  + M042150_r + M042260_r + M042169A_r  
          F1 ~~ 1*F1"

out.CFA3 <- lavaan(data = data_com_Book5_resp[,-c(26,27,28,32,20,24,31,19)], 
  model = CFAmodel3,
  ordered = names(data_com_Book5_resp[,-c(26,27,28,32,20,24,31,19)]))
summary(object = out.CFA3, fit.measures = TRUE, standardized = TRUE, rsq=TRUE) 
fitmeasures(object = out.CFA3,fit.measures = c("RMSEA.robust","CFI.robust","TLI.robust"))

table_resid <- matrix_to_table(resid(out.CFA3)$cov)
table_resid[order(abs(table_resid$resid),decreasing =TRUE),][1:10,]

# Items eliminated because of residuals:
M042169C_r
M042198B_r
M042067_r
M042169B_r
M042198A_r

data_com_Book5_resp_uni <- data_com_Book5_resp[,-c(26,27,28,32,20,24,31,19)]
colnames(data_com_Book5_resp_uni) <- gsub("_r", "", names(data_com_Book5_resp_uni))

out.pa2 <- fa.parallel(x = data_com_Book5_resp_uni,
  fa ="pc",
  cor = "poly",
  sim = FALSE,
  correct = FALSE,
  quant = 0.95) # 1 factor
eigenvalues2 <- data.frame(emp=round(out.pa2$pc.values,3),
  mean=round(apply(out.pa2$values,2,mean),3),
  pc95=round(apply(out.pa2$values,2,quantile,.95),3))
emp2 <- eigenvalues2$emp
e.mean2 <- eigenvalues2$mean
J2 <- ncol(data_com_Book5_resp_uni)
df.e.values2 <- data.frame(dataeigen = c(rep("emp",J2),rep("e.mean",J2)),
  J = rep(1:J2, 2), eigen = c(as.numeric(t(emp2)), as.numeric(t(e.mean2))))

point <- ifelse(grepl("emp", df.e.values2$dataeigen), 16, 4)
pa2 <- ggplot(df.e.values2, aes(x = J, y = eigen, color = dataeigen, shape = dataeigen)) +
  geom_line() +
  geom_point() + 
  xlim(0,15) +
  labs(x = "Component number", y = "Eigenvalues") +
  theme_few() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_blank(),
    legend.position = c(0.9, 1),  
    legend.justification = c(0.6, 1),  
    legend.background = element_rect(fill = "lightgray"),
    legend.text = element_text(size = 16)) +
  scale_color_manual(
    labels = c("PC empirical data", "PC simulated data"), 
    values = c("emp" = "blue", "e.mean" = "red")) +
  scale_shape_manual(
    labels = c("PC empirical data", "PC simulated data"), 
    values = c("emp" = 16, "e.mean" = 4)) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 4)))); pa2

png("PA2.png", width = 10, height = 5, res = 600, units = "in")
pa2
dev.off()

#===============================================================================
# 4. IRT model and fit
#===============================================================================
######################
# Model 1: Rasch and graded      #
######################

fit_model1 <- mirt(data_com_Book5_resp_uni, 
  1, 
  itemtype = c(rep("Rasch", 12), "graded", rep("Rasch", 11)), 
  SE=TRUE) 

######################
# Model 2: 2PL and graded      #
######################
fit_model2 <- mirt(data_com_Book5_resp_uni, 1, 
  itemtype=c(rep("2PL", 12), "graded", rep("2PL", 11)), 
  SE=TRUE) 

anova(fit_model1, fit_model2) # Model2 wins

######################
# Model 3: complex   #
######################
#Model 3: 3PL with priors to MC, 2PL to CR (dicho), and graded to CR (poli)
#The priors are for c and a parameters
fit_prev <- mirt(data_com_Book5_resp_uni, 
  itemtype = c("3PL", "3PL", "2PL", "3PL", "3PL", "2PL", "3PL", "3PL", "3PL", "3PL", "2PL", "3PL", "graded", "3PL", "3PL", "3PL", "3PL", "2PL", "2PL", "3PL", "3PL", "3PL","3PL","2PL"),
  SE=TRUE, 
  technical=list(NCYCLES=50000))
fit_prev_param <- coef(fit_prev, IRTpars=TRUE, simplify=TRUE)$items 

median(as.data.frame(fit_prev_param)[c(1,2,4,5,7,8,9,10,12,14,15,16,17,20,21,22,23),"a"]) #2.202
sd(as.data.frame(fit_prev_param)[c(1,2,4,5,7,8,9,10,12,14,15,16,17,20,21,22,23),"a"]) #1.01
median_ <- 2.202
sd_ <- 1.01
median_log <- log(median_^2/sqrt(median_^2+sd_^2)); round(median_log,3) #0.694
sd_log <- sqrt(log(1+sd_^2/median_^2)); round(sd_log,3) #0.437

mod3 <- "F = 1-24
        PRIOR = (1,2,4,5,7-10,12,14-17,20,21,22,23, g, norm, -1.099, 1)
        PRIOR = (1,2,4,5,7-10,12,14-17,20,21,22,23, a1, lnorm, 0.694, 0.437)"

fit_model3 <- mirt(data_com_Book5_resp_uni, 
  model = mod3, 
  itemtype = c("3PL", "3PL", "2PL", "3PL", "3PL", "2PL", "3PL", "3PL", "3PL", "3PL", "2PL", "3PL", "graded", "3PL", "3PL", "3PL", "3PL", "2PL", "2PL", "3PL", "3PL", "3PL","3PL","2PL"),
  SE = TRUE, 
  technical = list(NCYCLES=50000))

######################
# Model 3 info       #
######################
fit_model3_param <- coef(fit_model3, IRTpars=TRUE, simplify=TRUE)$items 
fit_model3_param <- round(fit_model3_param, 3)
fit_model3_param <- as.data.frame(fit_model3_param)
summary(fit_model3)

itemplot(fit_model3, 5)  
plot(fit_model3, type = "infotrace", facet_items= TRUE)
plot(fit_model3, type = "info", facet_items= TRUE)

theta_values <- seq(-6, 6, length.out = 100)
selected_items <- fit_model3_param[c(1, 4, 5, 10), ]
plot_data <- do.call(rbind, lapply(1:nrow(selected_items), function(i) {
  a <- selected_items[i, "a"]
  b <- selected_items[i, "b"]
  item_name <- rownames(selected_items)[i]
  prob <- 1 / (1 + exp(-a * (theta_values - b)))
  data.frame(theta = theta_values, prob = prob, item = item_name)}))

ICC <- ggplot(plot_data, aes(x = theta, y = prob, color = item, linetype = item)) +
  geom_line(size = 1.2) +
  labs(title = "", x = expression(theta), y = expression(P(theta))) +
  theme_few() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)) +
  scale_color_manual(labels = c("M032094", "M032402", "M032419", "M032477"), 
    values = c("deeppink", "aquamarine4", "darkgrey", "brown")) +
  scale_linetype_manual(labels = c("M032094", "M032402", "M032419", "M032477"), 
    values = c("solid", "dashed", "dotted", "dotdash")); ICC

png("ICC.png", width = 10, height = 5, res = 600, units = "in")
ICC
dev.off()

th.se.MAP <- fscores(object = fit_model3,
  full.scores.SE  = TRUE, 
  method="MAP") 
th.se.MAP <- as.data.frame(th.se.MAP)
colnames(th.se.MAP) <- c("Fscore","SE_F")
describe(th.se.MAP)
dev.off()
op <- par(mar = c(5,5,5,5))

thetas <- ggplot(th.se.MAP, aes(x = Fscore)) +
  geom_histogram(aes(y = after_stat(density)), 
    breaks = seq(-3, 3, by = 0.1), 
    fill = "skyblue", 
    color = "black", 
    alpha = 0.5) +
  geom_density(color = "red", size = 1.5) +
  stat_function(fun = dnorm, 
    args = list(mean = mean(th.se.MAP$Fscore), sd = sd(th.se.MAP$Fscore)), 
    color = "darkblue", size = 1.5) +
  labs(x = expression(theta), y = "", title = "") +
  theme_few() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)); thetas

png("theta_distribution.png", width = 10, height = 5, res = 600, units = "in")
thetas
dev.off()
######################
# Local independence #
######################
LDX2 <- residuals(fit_model3, type = "LD", df.p = TRUE, table = FALSE)
SDLX2 <- abs(LDX2$LD) # LD-X2 standardized
SDLX2[upper.tri(SDLX2)] <- NA
df <- 1
SDLX2 <- (SDLX2-df)/sqrt(2*df)

DL <- fit.DL(fit_model3) 
DL$adjusted.p <- round(p.adjust(p = DL$p, method = "BH"), 5) 
DL[order(DL$SDLX2,decreasing =TRUE),][1:10,] # We decide to not remove items with LD-X2 over 10 (justify in paper)

######################
# S-X2 (Orlando & Thissen) #
######################
mod.SX2 <- itemfit(fit_model3, na.rm=TRUE, p.adjust = "BH", mincell = 1); mod.SX2
mod.SX2[order(mod.SX2$p.S_X2,decreasing=FALSE),] #2 items with p.S_X2 < .05
round(max(mod.SX2$RMSEA.S_X2),3) #good fit

######################
# Global fit (M2 & RMSEA) #
######################
M2(obj = fit_model2,  # Model 2 fit
  calcNull = TRUE, 
  CI = 0.90,
  na.rm=TRUE)
M2(obj = fit_model3, # Model 3 fit
  calcNull = TRUE, 
  CI = 0.90,
  na.rm=TRUE) 

#===============================================================================
# 5. Person fit (lz*)
#===============================================================================

lzstar <- lzstar(matrix = data_com_Book5_resp_uni,
  Ability.PModel ="WL");lzstar
out_PF_lzstar <- lzstar$PFscores

set.seed(1234)
PerFit:::plot.PerFit(lzstar, UDlvl = -1.64) # 5.52% aberrant

which.min(lzstar$PFscores$PFscores)
which.max(lzstar$PFscores$PFscores)
length(lzstar$PFscores[lzstar$PFscores < -1.64])

#===============================================================================
# 5. DIF
#===============================================================================
data_com_Book5_uni <- data_com_Book5[,-c(29,30,31,35,23,27,34,22)]
colnames(data_com_Book5_uni) <- gsub("_r", "", names(data_com_Book5_uni))

groupCM <- data_com_Book5_uni$IDCNTRY
groupCM[groupCM=="Canada"] <- "1" # reference group
groupCM[groupCM=="South Africa"]  <- "2" # focal group

######################
# LRT in two steps   #
######################
# Step 1: Anchor test with all items  
# 1.A. Estimate model
x1.anclaje.1 <- names(data_com_Book5_resp_uni)
m_1a <- multipleGroup(data = data_com_Book5_resp_uni,
  model = 1, # assume 2PL
  group = groupCM,
  invariance = c('free_means', 'free_var',x1.anclaje.1))

par.g1  <- coef(object = extract.group(m_1a,1),
  simplify = TRUE,
  IRTpars = TRUE)$items[,c("a","b")]; par.g1 # Group 1 parameters (Canada, reference group)

par.g2  <- coef(object = extract.group(m_1a,2),
  simplify = TRUE,
  IRTpars = TRUE)$items[,c("a","b")]; par.g2 #Group 2 parameters (South Africa, focal group)

mean.var.g1 <- coef(object = extract.group(m_1a,1))$GroupPars #mean and cov fixed for Canada
mean.var.g1
#    MEAN_1 COV_11
#par      0      1
mean.var.g2 <- coef(object = extract.group(m_1a,2))$GroupPars #mean and cov estimated for Canada
mean.var.g2
#      MEAN_1   COV_11
#par -1.804129 0.7709796
sum(par.g1!=par.g2, na.rm=TRUE) # 0: so all parameters are the same 


# 1.B. Estimate models to compare (leave free the item parameters) 
m_leave_free <- DIF(MGmodel = m_1a, 
  which.par = c('a1', 'd'), 
  scheme = 'drop', 
  p.adjust="BH")
m_leave_free

# Step 2: Anchor test formed by the 5 most discriminate items without DIF
# 2.A. Select 5 anchor items 
no_DIF <- par.g1[which(m_leave_free$adj_p>.05),]
best <- order(no_DIF[,1],decreasing=TRUE)[1:5]
no_DIF[order(no_DIF[,1],decreasing=TRUE),]

x1.anclaje.2  <- rownames(no_DIF)[best]
x1.anclaje.2.n <- which((names(data_com_Book5_resp_uni) %in% x1.anclaje.2)); x1.anclaje.2.n #position of anchor items
x1.sospech.2.n <- which(!(names(data_com_Book5_resp_uni) %in% x1.anclaje.2)); x1.sospech.2.n #position of suspect items

# 2.B. Estimate base model: anchor items with same parameters and suspect items with different parameters
m.with_anchor <- multipleGroup(data = data_com_Book5_resp_uni,
  model = 1,
  group = groupCM,
  invariance = c('free_means', 'free_var',x1.anclaje.2))
m.with_anchor.par1 <- coef(object = extract.group(m.with_anchor,1), 
  simplify = TRUE,
  IRTpars = TRUE)$items[,c("a","b")]
m.with_anchor.par1 # Group 1 params
m.with_anchor.par2 <- coef(object = extract.group(m.with_anchor,2), 
  simplify=TRUE,
  IRTpars=TRUE)$items[,c("a","b")]
m.with_anchor.par2 # Group 2 params
m.with_anchor.par <- as.data.frame(cbind(m.with_anchor.par1, m.with_anchor.par2))
names(m.with_anchor.par) <- c("a.g1","b.g1","a.g2","b.g2")
m.with_anchor.par

which(m.with_anchor.par$a.g1==m.with_anchor.par$a.g2)
which((m.with_anchor.par1[,"a"] == m.with_anchor.par2[,"a"])==TRUE)
which((m.with_anchor.par1[,"b"] == m.with_anchor.par2[,"b"])==TRUE) # Make sure anchor items have same params

# 2.C. DIF in suspect items 
m.fixed_same_2 <- DIF(MGmodel = m.with_anchor, 
  which.par = c('a1', 'd'), 
  scheme = 'add',
  p.adjust="BH",
  items2test = x1.sospech.2.n) 
m.fixed_same_2 #suspect contrasts, not reliable 

# 2.D. Verify the anchor test items don't have DIF
m_leave_free.2 <- DIF(MGmodel = m.with_anchor, 
  which.par = c('a1', 'd'), 
  scheme = 'drop',
  p.adjust="BH",
  items2test = x1.anclaje.2.n) 
m_leave_free.2  # Anchor contrasts

m.conjunto <- rbind(m.fixed_same_2, m_leave_free.2)
m.conjunto <- m.conjunto[names(data_com_Book5_resp_uni),]
m.conjunto$adj_pvals <- p.adjust(m.conjunto$p,"BH") 
LRT.withDIF <- rownames(m.conjunto)[m.conjunto$adj_pvals<= .05]
LRT.no_DIF <- rownames(m.conjunto)[m.conjunto$adj_pvals > .05]

TableDIF.LRT <- as.data.frame(list(chi = round(m.conjunto$X2,3),
  df  = round(m.conjunto$df,0),
  p = round(m.conjunto$adj_pvals,3),
  tipo = ifelse(m.conjunto$adj_pvals > .05,"no DIF","DIF")),
  row.names = names(data_com_Book5_resp_uni))
TableDIF.LRT

# Final model
m.final <- multipleGroup(data = data_com_Book5_resp_uni,
  model = 1,
  group = groupCM,
  invariance = c('free_means', 'free_var', LRT.no_DIF))

# Plots for items with DIF
png("DIFitems.png", width = 12, height = 8, res = 600, units = "in")
plot(x = m.final,
  type = "trace",
  which.items = which(TableDIF.LRT$tipo == "DIF"),
  facet_items = TRUE)
dev.off()

# Plots for items without DIF
plot(x = m.final,
  type = "trace",
  which.items = which(TableDIF.LRT$tipo=="no DIF"),
  facet_items = TRUE)

#empirical_ES(m.final, DIF=TRUE,ref.group = 1,plot=TRUE)

# Effect sizes
LRT.TE <- empirical_ES(m.final, DIF=TRUE,ref.group = 1)

selec <- c("SIDS","UIDS","ESSD","mean.ES.foc","mean.ES.ref")
TableDIF.LRT[,selec] <- round(LRT.TE[,selec],3)

TableDIF.LRT$tipo <- ifelse(TableDIF.LRT$tipo =="no DIF", "no DIF",
  ifelse((abs(TableDIF.LRT$SIDS)==TableDIF.LRT$UIDS), 
    "uDIF", "nuDIF"))
TableDIF.LRT$size <- ifelse(abs(TableDIF.LRT$ESSD)  < .2, " ",
  ifelse(abs(TableDIF.LRT$ESSD) < .5, "A",
    ifelse(abs(TableDIF.LRT$ESSD) < .8, "B","C")))

TableDIF.LRT$favor <- ifelse(TableDIF.LRT$mean.ES.foc == TableDIF.LRT$mean.ES.ref, " ",
  ifelse(TableDIF.LRT$mean.ES.foc > TableDIF.LRT$mean.ES.ref, "F", "R"))
order(abs(TableDIF.LRT$ESSD),decreasing="TRUE")
TableDIF.LRT[order(abs(TableDIF.LRT$ESSD),decreasing="TRUE"),]

#===============================================================================
# Save
#===============================================================================
save(list = ls(), file = "DataAn_TIMSS2011.RData")
