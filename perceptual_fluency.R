# setwd("/Users/kakarot/Desktop/BRSM PROJECT/Data")
setwd ("C:\\Users\\Lenovo\\Documents\\RawData")

library(Hmisc)
library(ggplot2)
library(gridExtra)
library(lme4)
library(car) 
library(reshape)
library(pracma)
library('GiNA')
# library(usdm)
library(jtools)

#### PARTICIPANT NUMBERS 
n <- 164
removedParticipants <- c(97, 105, 117) # inconsistent AQ/SRS scores
participantsList <- 1:n
participantsList <- participantsList [! participantsList %in% removedParticipants]
participantsListLength <- length(participantsList)

#### IMPORT DATA
perceptualFluencyData <- NULL
for (subjectId in 1:participantsListLength){
  if (participantsList[subjectId] < 10) {
    perceptualFluencyData <- rbind(perceptualFluencyData, read.table(sprintf("PerceptualFluencyTask/fluency_ppn0%s.rtd", participantsList[subjectId]), header = TRUE))}
  if (participantsList[subjectId] > 9) {
    perceptualFluencyData <- rbind(perceptualFluencyData, read.table(sprintf("PerceptualFluencyTask/fluency_ppn%s.rtd", participantsList[subjectId]), header = TRUE))}
}

# read questionnaire data from QuestionnaireData.csv
questionnaireData <- read.csv ("QuestionnaireData.csv", header=TRUE, stringsAsFactors = FALSE)
questionnaireData <- questionnaireData[-removedParticipants,]

# Define empty data frame for reaction time means
meanReactionTime <- data.frame(ppnr = participantsList, mean = rep(NA, length(participantsList)), sd = rep(NA, length(participantsList)))

# Loop through each subject in participantsList and calculate mean and sd of their RTs
for (i in seq_along(participantsList)) {
  subject_data <- perceptualFluencyData[perceptualFluencyData$ppnr == participantsList[i], ]
  meanReactionTime$mean[i] <- mean(subject_data$rt, na.rm = TRUE)
  meanReactionTime$sd[i] <- sd(subject_data$rt, na.rm = TRUE)
}

# delete all rows in perceptualFluencyData where RT > 3 SD from individual mean
for (i in 1:nrow(perceptualFluencyData)) {
  if (perceptualFluencyData$rt[i] > (meanReactionTime$mean[meanReactionTime$ppnr ==  perceptualFluencyData$ppnr[i]] + (3 * meanReactionTime$sd[meanReactionTime$ppnr ==  perceptualFluencyData$ppnr[i]]))) {
    perceptualFluencyData[i,3:4] <- NA
  }
}

# remove rows with missing data from a data frame
perceptualFluencyDatawithNA <- perceptualFluencyData
perceptualFluencyData <- perceptualFluencyData[complete.cases(perceptualFluencyData), ]


#### PREFERENCE FOR MATCHING VS NON-MATCHING

# calculate mean preference ratings for different conditions in a data frame and store them in a new data frame for each subject
preference <- tapply(perceptualFluencyData$preference, list(perceptualFluencyData$ppnr, perceptualFluencyData$matching), mean, na.rm = TRUE)
preference <- as.data.frame(preference)
colnames(preference) <- c("nonmatching", "matching")
preference$ppnr <- row.names(preference)
preference$ppnr <- as.numeric(preference$ppnr)

# preference_range that will store the minimum and maximum rating for each participant in the subject_list
preference_range <- aggregate(preference ~ ppnr, perceptualFluencyData, function(x) c(min(x), max(x)))
preference_range <- data.frame(preference_range[,1], preference_range$preference)
colnames(preference_range) <- c("ppnr", "min", "max")

# barplot of preference ratings for matching vs non-matching
preferenceMeanPPNRAndMatching <- aggregate(preference ~ ppnr + matching, perceptualFluencyData, mean)
preferenceMean <- aggregate(preference ~ matching, preferenceMeanPPNRAndMatching, mean)
preferenceMean$sem <- aggregate(preference ~ matching, preferenceMeanPPNRAndMatching, FUN = sd)[,2]/sqrt(length(participantsList))
preferenceMean$matching <- as.factor(preferenceMean$matching)
levels(preferenceMean$matching) <- c("non-matching", "matching")

ggplot (preferenceMean) + geom_bar (aes (x = matching, y = preference), stat = "identity", fill = "gray") +
  geom_errorbar (aes (x = matching, ymin = preference - sem, ymax = preference + sem), width=0.4, size=0.4) +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
  #       axis.line.y = element_line(colour = "black"), axis.text.x = element_text(size = 18), 
  #       axis.ticks.x = element_blank(), axis.title=element_text(size=20), axis.text.y = element_text(size=20),
  #       plot.margin = unit(c(0.5,0.5,0.2,0.2),"cm"), axis.title.x = element_blank()) +
  coord_cartesian(ylim=c(3,4))

ggsave('Matching_Vs_Preference.png')

res.ftest <- var.test(preference$matching, preference$nonmatching)
res.ftest

# two-tailed t.test
t.test (preference$matching, preference$nonmatching, alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#### REACTION TIMES MATCHING VS. NON-MATCHING
# Create a data frame to store mean reaction times
RT <- data.frame(
  ppnr = participantsList,
  nonmatching = rep(NA, length(participantsList)),
  matching = rep(NA, length(participantsList))
)

# Rename the columns of the data frame
names(RT) <- c("ppnr", "nonmatching", "matching")

# Loop through each participant and calculate mean reaction times
# for (i in seq_along(participantsList)) {
#   ppnr <- participantsList[i]
  
#   # Subset fluency data for the current participant and incongruent trials
#   nonmatching_data <- subset(perceptualFluencyData, ppnr == ppnr & matching == 0)
  
#   # Subset fluency data for the current participant and congruent trials
#   matching_data <- subset(perceptualFluencyData, ppnr == ppnr & matching == 1)
  
#   # Calculate mean reaction times for incongruent and congruent trials
#   nonmatching_mean <- mean(nonmatching_data$rt)
#   matching_mean <- mean(matching_data$rt)
  
#   # Assign mean reaction times to corresponding rows in data frame
#   RT$nonmatching[i] <- nonmatching_mean
#   RT$matching[i] <- matching_mean
# }

for (k in 1:length(participantsList)){
  RT$nonmatching[k] <- mean(perceptualFluencyData$rt[perceptualFluencyData$matching == 0 & perceptualFluencyData$ppnr == participantsList[k]]) # mean RT for incongruent trials
  RT$matching[k] <- mean(perceptualFluencyData$rt[perceptualFluencyData$matching == 1 & perceptualFluencyData$ppnr == participantsList[k]]) # mean RT for congruent trials
}


# barplot
barplot (c(mean(RT$matching), mean(RT$nonmatching)), names = c("matching", "non-matching"), ylab = "Reaction time")

ggsave('Matching_Vs_RT.png')

res.ftest <- var.test(RT$matching, RT$nonmatching)
res.ftest

# t.test
t.test (RT$matching, RT$nonmatching, alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#### CORRELATIONS

abbreviateSTR <- function(value, prefix){  # format string more concisely
  lst = c()
  for (item in value) {
    if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
      lst <- c(lst, '')
      next
    }
    item <- round(item, 2) # round to two digits
    if (item == 0) { # if rounding results in 0 clarify
      item = '<.01'
    }
    item <- as.character(item)
    item <- sub("(^[0])+", "", item)    # remove leading 0: 0.05 -> .05
    item <- sub("(^-[0])+", "-", item)  # remove leading -0: -0.05 -> -.05
    lst <- c(lst, paste(prefix, item, sep = ""))
  }
  return(lst)
}

# with preference for matching over non-matching
# Create a data matrix with relevant columns from preference and questionnaire_data
corr_data <- as.matrix(cbind ((preference$matching-preference$nonmatching), questionnaireData[,c(5, 12, 17)]))
colnames(corr_data) <- c("preference_for_match", "AQ_total", "SRS_total", "factorscores")


fitScores_orig <- lm(preference_for_match ~ AQ_total + SRS_total + factorscores, as.data.frame(corr_data));

p1 <- effect_plot(fitScores_orig, pred = factorscores, interval=TRUE, plot.points = TRUE)
p2 <- effect_plot(fitScores_orig, pred = AQ_total, interval=TRUE, plot.points = TRUE)
p3 <- effect_plot(fitScores_orig, pred = SRS_total, interval=TRUE, plot.points = TRUE)

g <- grid.arrange(grobs=list(p1, p2, p3), nrows=3);

ggsave('LM_Totals_vs_matching.png')

cormatrix <- rcorr(corr_data, type = "spearman")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 2

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize)

ggsave("Correlations_of_Matching_with_totals_S.png")

cormatrix <- rcorr(corr_data, type = "pearson")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 2

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize)

ggsave("Correlations_of_Matchings_with_totals_P.png")

# scatter plot
corr_data <- as.data.frame(corr_data)
library(ggplot2)
library(gridExtra)
p1 <- ggplot(corr_data, aes(x=factorscores, y=preference_match_over_nonmatch)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("PCA factor") +
  ylab("preference difference")

p2 <- ggplot(corr_data, aes(x=AQ_total, y=preference_match_over_nonmatch)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("AQ")

p3 <- ggplot(corr_data, aes(x=SRS_total, y=preference_match_over_nonmatch)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("SRS-A")

g <- grid.arrange(p1, p2, p3, widths = c(1.075, 1, 1), nrow = 3)

ggsave(file='Preference%_vs_components.png', g)

# with preference for matching over nonmatching, all subscales
corr_data <- as.matrix(cbind((preference$matching-preference$nonmatching), questionnaireData[,c(5:10, 12:17)]))
colnames(corr_data) <- c("preference_match_over_nonmatch", "AQ_total", "AQ_soc_skills", "AQ_att_switch", "AQ_att_detail", "AQ_comm", "AQ_imag", "SRS_total", "SRS_consc", "SRS_comm", "SRS_mot", "SRS_rig", "factorscores")
cormatrix <- rcorr(corr_data, type = "spearman")

rcorr(corr_data, type = "pearson")

# with preference for matching
corr_data <- as.matrix(cbind ((preference$matching), questionnaireData[,c(5, 12, 17)]))
colnames(corr_data) <- c("preference_match", "AQ_total", "SRS_total", "factorscores")
rcorr(corr_data, type = "spearman")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 4

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize, color='white')

ggsave("Correlations_of_Matchings_with_partwise_S.png")

cormatrix <- rcorr(corr_data, type = "pearson")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 4

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize, color='white')

ggsave("Correlations_of_Matchings_with_partwise_P.png")

# with preference for non-matching
corr_data <- as.matrix(cbind ((preference$nonmatching), questionnaireData[,c(5, 12, 17)]))
colnames(corr_data) <- c("preference_nonmatch", "AQ_total", "SRS_total", "factorscores")
cormatrix <- rcorr(corr_data, type = "spearman")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 2

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize)

ggsave("Correlations_of_NonMatchings_with_totals_S.png")

cormatrix <- rcorr(corr_data, type = "pearson")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 2

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize)

ggsave("Correlations_of_NonMatchings_with_totals_P.png")

# with RT for matching - RT  for non-matching
corr_data <- as.matrix(cbind ((RT$matching-RT$nonmatching), questionnaireData[,c(5, 12, 17)])) 
colnames(corr_data) <- c("RT_match_minus_nonmatch", "AQ_total", "SRS_total", "factorscores")

cormatrix <- rcorr(corr_data, type = "spearman")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 2

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize)

ggsave("Correlations_of_RTs_with_totals_S.png")

cormatrix <- rcorr(corr_data, type = "pearson")
cordata = melt(cormatrix$r)
print(cordata)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 2

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize)

ggsave("Correlations_of_RTs_with_totals_P.png")