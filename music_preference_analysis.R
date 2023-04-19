setwd ("C:\\Users\\Lenovo\\Documents\\RawData")

library(Hmisc)
library(ggplot2)
library(lemon)
library(gridExtra)
library(lme4)
library(car) 
library(reshape)
library(pracma)
library('GiNA')
# library(usdm)
library(jtools)
library(ggcorrplot)

num_participants <- 164
remove_ids <- c(23, 28, 143, 151, 152, 97, 105, 117)

subject_ids <- 1:num_participants
subject_ids <- subject_ids [! subject_ids %in% remove_ids]

music_task_data <- NULL
for (k in 1:length(subject_ids)){
  if (subject_ids[k] < 10) {
    music_task_data <- rbind(music_task_data, read.table(strcat(strcat("MusicPreferenceTask/music_ppn0", toString(subject_ids[k])), ".rtd"), header = TRUE))}
  if (subject_ids[k] > 9) {
    music_task_data <- rbind(music_task_data, read.table(strcat(strcat("MusicPreferenceTask/music_ppn", toString(subject_ids[k])), ".rtd"), header = TRUE))}
}

colnames(music_task_data) <- c("ppnr", "trialnr", "s1.entclass", "s1.stimnr", "s2.entclass", "s2.stimnr", "r", "s1.tone1", "s1.tone2", "s1.tone3", 
                          "s1.tone4", "s1.tone5", "s1.tone6", "s1.tone7", "s1.sameentropy", "s1.nextentropy", "s1.entropy", "s2.tone", "s2.tone2",
                          "s2.tone3", "s2.tone4", "s2.tone5", "s2.tone6", "s2.tone7", "s2.sameentropy", "s2.nextentropy", "s2.entropy")

questionnaire_data <- read.csv("QuestionnaireData.csv", header=TRUE, stringsAsFactors = FALSE)
questionnaire_data <- questionnaire_data[-remove_ids,]

for (i in 1:nrow(music_task_data)){
  if (music_task_data[i,3] > music_task_data[i,5] & music_task_data[i,7] == 1) music_task_data[i,28] = 0
  if (music_task_data[i,3] < music_task_data[i,5] & music_task_data[i,7] == 2) music_task_data[i,28] = 0
  if (music_task_data[i,3] > music_task_data[i,5] & music_task_data[i,7] == 2) music_task_data[i,28] = 1
  if (music_task_data[i,3] < music_task_data[i,5] & music_task_data[i,7] == 1) music_task_data[i,28] = 1
}
colnames(music_task_data)[28] <- c("pref_pred")

preference <- matrix (nrow =length(subject_ids), ncol = 14)
colnames(preference) <- c("ppnr", "class1", "class2", "class3", "class4", "class5", "class6", "class7", "class8", "class9", "class10", "class11", "weighted_sum", "pref_pred")
preference <- as.data.frame(preference)
preference$ppnr <- subject_ids

for (k in 1:length(subject_ids)){
  preference$class1[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 1 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 1 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class2[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 2 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 2 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class3[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 3 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 3 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class4[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 4 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 4 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class5[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 5 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 5 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class6[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 6 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 6 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class7[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 7 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 7 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class8[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 8 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 8 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class9[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 9 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 9 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class10[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 10 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 10 & music_task_data$ppnr == subject_ids[k]))
  
  preference$class11[k] <- length(which(music_task_data$r == 1 & music_task_data$s1.entclass == 11 & music_task_data$ppnr == subject_ids[k])) + 
                          length(which(music_task_data$r == 2 & music_task_data$s2.entclass == 11 & music_task_data$ppnr == subject_ids[k]))
  
  preference$weighted_sum[k] <- (preference$class1[k]*11) + (preference$class2[k]*10) + (preference$class3[k]*9) + 
                                (preference$class4[k]*8) + (preference$class5[k]*7) + (preference$class6[k]*6) + (preference$class7[k]*5) + 
                                (preference$class8[k]*4) + (preference$class9[k]*3) + (preference$class10[k]*2) + (preference$class11[k]*1)

  preference$pref_pred[k] <- mean(music_task_data$pref_pred[music_task_data$ppnr == subject_ids[k]])
}

preference_mean <- as.data.frame(colMeans (preference[,c(2:12)])/110*100)
preference_mean$sem <- (apply(preference[,c(2:12)]/110*100, 2, sd))/sqrt(length(subject_ids))
preference_mean$entropy <- factor(c("0.3 - 0.45", "0.45 - 0.6", "0.6 - 0.75", "0.75 - 0.9", "0.9 - 1.05", "1.05 - 1.3", "1.3 - 1.45", "1.45 - 1.6", "1.6 - 1.75", "1.75 - 1.8", "> 1.8"), levels = c("0.3 - 0.45", "0.45 - 0.6", "0.6 - 0.75", "0.75 - 0.9", "0.9 - 1.05", "1.05 - 1.3", "1.3 - 1.45", "1.45 - 1.6", "1.6 - 1.75", "1.75 - 1.8", "> 1.8"))
colnames(preference_mean) <- c("mean", "sem", "entropy")

ggplot (preference_mean) + geom_bar (aes (x = entropy, y = mean), stat = "identity", fill = "gray") +  
  geom_errorbar (aes (x = entropy, ymin = mean - sem, ymax = mean + sem), width=0.4, size=0.4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 18), 
        axis.ticks.x = element_blank(), axis.title=element_text(size=20), axis.text.y = element_text(size=20),
        plot.margin = unit(c(0.5,0.5,0.2,0.2),"cm")) +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12), limits = c(0, 12)) +
  coord_capped_cart(ylim = c(0, 12), expand = FALSE) +
  ylab ("preference (% chosen)") +
  geom_hline(yintercept = 100/11, linetype = "dashed")

ggsave("Preferences_per_entropy_class.png")

# CORRELATIONS


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

corr_data <- as.matrix(cbind (preference$pref_pred, questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("predictable_seq_chosen", "AQ_total", "SRS_total", "factorscores")

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

ggsave("Correlations_of_preferences_with_totals_S.png")
cormatrix <- rcorr(corr_data, type = "pearson")

cordata = melt(cormatrix$r)
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

ggsave("Correlations_of_preferences_with_totals_P.png")

# with percentage of predictable sequence chosen, all subscales
corr_data <- as.matrix(cbind(preference$pref_pred, questionnaire_data[,c(5:10, 12:17)]))
colnames(corr_data) <- c("predictable_seq_chosen", "AQ_total", "AQ_soc_skills", "AQ_att_switch", "AQ_att_detail", "AQ_comm", "AQ_imag", "SRS_total", "SRS_consc", "SRS_comm", "SRS_mot", "SRS_rig", "factorscores")
cormatrix <- rcorr(corr_data, type = "spearman")
cordata = melt(cormatrix$r)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 4

# ggplot(cordata, aes(Var1, Var2)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradient(low = "white", high = "red")

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize, color='white')
ggsave("Correlations_of_preferences_with_partwise_S.png")
cormatrix <- rcorr(corr_data, type = "pearson")
cordata = melt(cormatrix$r)
cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
cordata$label = paste(cordata$labelr, "\n", 
                      cordata$labelP, sep = "")

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

txtsize <- par('din')[2] / 4

ggplot(cordata, aes(x=X1, y=X2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  geom_text(label=cordata$label, size=txtsize, color='white')
ggsave("Correlations_of_preferences_with_partwise_P.png")

corr_data <- as.matrix(cbind (preference$weighted_sum, questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("weighted_sum", "AQ_total", "SRS_total", "factorscores")
cormatrix <- rcorr(corr_data, type = "spearman")

cordata = melt(cormatrix$r)
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


ggsave("Correlations_of_weightedsum_with_totals_S.png")

cormatrix <- rcorr(corr_data, type = "pearson")
cordata = melt(cormatrix$r)
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
ggsave("Correlations_of_weightedsum_with_totals_P.png")

corr_data <- as.data.frame(cbind (preference$pref_pred, questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("predictable_seq_chosen", "AQ_total", "SRS_total", "factorscores")

fitScores_orig <- lm(predictable_seq_chosen ~ AQ_total + SRS_total + factorscores, corr_data);

p1 <- effect_plot(fitScores_orig, pred = factorscores, interval=TRUE, plot.points = TRUE)
p2 <- effect_plot(fitScores_orig, pred = AQ_total, interval=TRUE, plot.points = TRUE)
p3 <- effect_plot(fitScores_orig, pred = SRS_total, interval=TRUE, plot.points = TRUE)

g <- grid.arrange(grobs=list(p1, p2, p3), nrows=3);

ggsave('LM_Totals_vs_Preferences.png')

fitScores_orig <- lm(predictable_seq_chosen ~ AQ_total + SRS_total, corr_data);
summary(fitScores_orig)
vif(fitScores_orig)

effect_plot(fitScores_orig, pred = AQ_total, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_AQTotal.png');

effect_plot(fitScores_orig, pred = SRS_total, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_SRSTotal.png');

p1 <- ggplot(corr_data, aes(x=factorscores, y=predictable_seq_chosen)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("PCA factor") + ylab("% pred chosen")

p2 <- ggplot(corr_data, aes(x=AQ_total, y=predictable_seq_chosen)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("AQ Total") + ylab("% pred chosen")

p3 <- ggplot(corr_data, aes(x=SRS_total, y=predictable_seq_chosen)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("SRS-A Total") + ylab("% pred chosen")

# weighted sum
corr_data <- as.data.frame(cbind (preference$weighted_sum, questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("weighted_sum", "AQ_total", "SRS_total", "factorscores")

p4 <- ggplot(corr_data, aes(x=factorscores, y=weighted_sum)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("PCA factor") + ylab("weighted sum")

p5 <- ggplot(corr_data, aes(x=AQ_total, y=weighted_sum)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("AQ Total") + ylab("weighted sum")

p6 <- ggplot(corr_data, aes(x=SRS_total, y=weighted_sum)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  xlab("SRS-A Total") + ylab("weighted sum")

g <- grid.arrange(p1, p2, p3, nrow = 3)

ggsave(file="Prefpred_vs_Totals.png", g)


g <- grid.arrange(p4, p5, p6, nrow = 3)

ggsave(file="Weightedsum_vs_Totals.png", g)


corr_data <- as.data.frame(cbind (preference$pref_pred, questionnaire_data[,c(6, 7, 8, 9, 10, 11, 13, 14, 15, 16)]))
colnames(corr_data) <- c("predictable_seq_chosen", 'AQ_social',	'AQ_switch', 'AQ_detail',	'AQ_comm',	'AQ_imag', 'AQ_binary', 'SRS_consc',	'SRS_comm',	'SRS_motiv',	'SRS_rigid')

fitScores <- lm(predictable_seq_chosen ~ AQ_social + AQ_switch + AQ_detail + AQ_comm + AQ_imag + AQ_binary + SRS_consc	+ SRS_comm + SRS_motiv + SRS_rigid, corr_data);
summary(fitScores)
vif(fitScores)

print(AIC(fitScores_orig))
print(AIC(fitScores))

effect_plot(fitScores, pred = AQ_social, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_AQSocial.png');

effect_plot(fitScores, pred = AQ_switch, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_AQSwitch.png');

effect_plot(fitScores, pred = AQ_detail, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_AQDetail.png');

effect_plot(fitScores, pred = AQ_comm, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_AQComm.png');

effect_plot(fitScores, pred = AQ_imag, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_AQImag.png');

effect_plot(fitScores, pred = AQ_binary, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_AQBinary.png');

effect_plot(fitScores, pred = SRS_consc, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_SRSConsc.png');

effect_plot(fitScores, pred = SRS_comm, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_SRSComm.png');

effect_plot(fitScores, pred = SRS_motiv, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_SRSMotiv.png');

effect_plot(fitScores, pred = SRS_rigid, interval=TRUE, plot.points = TRUE)

ggsave('LM-prefPred_Vs_SRSRigid.png');

#### Linear Mixed Models Analysis
music_task_data$rb <- music_task_data$r - 1

music_task_data$s1.sameentropyC <- scale(music_task_data$s1.sameentropy, center = T, scale = T)
music_task_data$s1.nextentropyC <- scale(music_task_data$s1.nextentropy, center = T, scale = T)
music_task_data$s1.entropyC <- scale(music_task_data$s1.entropy, center = T, scale = T)
music_task_data$s2.sameentropyC <- scale(music_task_data$s2.sameentropy, center = T, scale = T)
music_task_data$s2.nextentropyC <- scale(music_task_data$s2.nextentropy, center = T, scale = T)
music_task_data$s2.entropyC <- scale(music_task_data$s2.entropy, center = T, scale = T)

music_task_data$s1.sameentropy2 <- music_task_data$s1.sameentropyC^2
music_task_data$s1.nextentropy2 <- music_task_data$s1.nextentropyC^2
music_task_data$s1.entropy2 <- music_task_data$s1.entropyC^2
music_task_data$s2.sameentropy2 <- music_task_data$s2.sameentropyC^2
music_task_data$s2.nextentropy2 <- music_task_data$s2.nextentropyC^2
music_task_data$s2.entropy2 <- music_task_data$s2.entropyC^2

music_task_data$s1.sameentropy2C <- scale(music_task_data$s1.sameentropy2, center = T, scale = T)
music_task_data$s1.nextentropy2C <- scale(music_task_data$s1.nextentropy2, center = T, scale = T)
music_task_data$s1.entropy2C <- scale(music_task_data$s1.entropy2, center = T, scale = T)
music_task_data$s2.sameentropy2C <- scale(music_task_data$s2.sameentropy2, center = T, scale = T)
music_task_data$s2.nextentropy2C <- scale(music_task_data$s2.nextentropy2, center = T, scale = T)
music_task_data$s2.entropy2C <- scale(music_task_data$s2.entropy2, center = T, scale = T)

# GLMM
fit = glmer(rb ~ (1|ppnr) + s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
summary(fit)
vif(fit)

fit = glm(rb ~ s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
summary(fit)
vif(fit)

fit1 = glmer(rb ~ (1|ppnr) + s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
fit2 = glmer(rb ~ (1 + s1.entropyC| ppnr) + s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s1.entropy2C| ppnr) + s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s2.entropyC| ppnr) + s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s2.entropy2C| ppnr) + s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)

fit = glmer(rb ~ (1 + s1.entropyC + s2.entropyC|ppnr) + s1.entropyC + s1.entropy2C + s2.entropyC + s2.entropy2C, music_task_data, family = "binomial")
summary(fit) 
Anova(fit,type=3)

# FIRST ORDER ENTROPY
music_task_data$pref_pred <- NA
for (i in 1:nrow(music_task_data)){
  if (music_task_data[i,16] > music_task_data[i,26] & music_task_data[i,7] == 1) music_task_data[i,28] = 0
  if (music_task_data[i,16] < music_task_data[i,26] & music_task_data[i,7] == 2) music_task_data[i,28] = 0
  if (music_task_data[i,16] > music_task_data[i,26] & music_task_data[i,7] == 2) music_task_data[i,28] = 1
  if (music_task_data[i,16] < music_task_data[i,26] & music_task_data[i,7] == 1) music_task_data[i,28] = 1
}

preference <- matrix (nrow =length(subject_ids), ncol = 2)
colnames(preference) <- c("ppnr", "pref_pred")
preference <- as.data.frame(preference)
preference$ppnr <- subject_ids

for (k in 1:length(subject_ids)){
  preference$pref_pred[k] <- mean (music_task_data$pref_pred[music_task_data$ppnr == subject_ids[k]], na.rm = TRUE)
}

corr_data <- as.matrix(cbind (preference$pref_pred, questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("predictable_seq_chosen", "AQ_total", "SRS_total", "factorscores")

preference_entropy <- matrix(nrow = length(unique(music_task_data$s1.sameentropy)), ncol = 3)
preference_entropy <- as.data.frame(preference_entropy)
colnames(preference_entropy) <- c("sameentropy", "mean_preference", "sem_preference")
preference_entropy$sameentropy <- unique(music_task_data$s1.sameentropy)
for (i in 1:length(unique(music_task_data$s1.sameentropy))) {
  preference_entropy$mean_preference[i] <- mean(1-(music_task_data$r[music_task_data$s1.sameentropy == preference_entropy$sameentropy[i]] - 1))
  preference_entropy$sem_preference[i] <- sd(1-(music_task_data$r[music_task_data$s1.sameentropy == preference_entropy$sameentropy[i]] - 1)) / sqrt(156)
}

print(preference_entropy)

ggplot(preference_entropy) +
  geom_ribbon(aes(x=sameentropy, ymin = (mean_preference - sem_preference) * 100, ymax = (mean_preference + sem_preference)*100), fill = "grey70") +
  geom_line(aes (x = sameentropy, y = mean_preference * 100), stat = "identity") +
  ylab ("preference (% chosen)") +
  xlab ("first-order entropy") +
  theme_classic() +
  geom_hline(yintercept = 50, linetype = "dotted") + ggtitle('Preference % vs Linear Entropy')

ggsave("Preference_Vs_Entropy.png")

# GLMM
fit1 = glmer(rb ~ (1|ppnr) + s1.sameentropyC + s1.sameentropy2C + s2.sameentropyC + s2.sameentropy2C, music_task_data, family = "binomial")
fit2 = glmer(rb ~ (1 + s1.sameentropyC| ppnr) + s1.sameentropyC + s1.sameentropy2C + s2.sameentropyC + s2.sameentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s1.sameentropy2C| ppnr) + s1.sameentropyC + s1.sameentropy2C + s2.sameentropyC + s2.sameentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s2.sameentropyC| ppnr) + s1.sameentropyC + s1.sameentropy2C + s2.sameentropyC + s2.sameentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s2.sameentropy2C| ppnr) + s1.sameentropyC + s1.sameentropy2C + s2.sameentropyC + s2.sameentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)

fit = glmer(rb ~ (1 + s1.sameentropyC + s2.sameentropyC|ppnr) + s1.sameentropyC + s1.sameentropy2C + s2.sameentropyC + s2.sameentropy2C, music_task_data, family = "binomial")
summary(fit)
Anova(fit,type=3)

#                     Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)      -0.09962    0.03148  -3.164  0.00156 ** 
#   s1.sameentropyC  -0.23576    0.02793  -8.441  < 2e-16 ***
#   s1.sameentropy2C  0.13196    0.01684   7.838 4.57e-15 ***
#   s2.sameentropyC   0.39366    0.03453  11.400  < 2e-16 ***
#   s2.sameentropy2C -0.14093    0.01724  -8.172 3.03e-16 ***

# SECOND ORDER ENTROPY
music_task_data$pref_pred <- NA
for (i in 1:nrow(music_task_data)){
  if (music_task_data[i,15] > music_task_data[i,25] & music_task_data[i,7] == 1) music_task_data[i,28] = 0
  if (music_task_data[i,15] < music_task_data[i,25] & music_task_data[i,7] == 2) music_task_data[i,28] = 0
  if (music_task_data[i,15] > music_task_data[i,25] & music_task_data[i,7] == 2) music_task_data[i,28] = 1
  if (music_task_data[i,15] < music_task_data[i,25] & music_task_data[i,7] == 1) music_task_data[i,28] = 1
}

preference <- matrix (nrow =length(subject_ids), ncol = 2)
colnames(preference) <- c("ppnr", "pref_pred")
preference <- as.data.frame(preference)
preference$ppnr <- subject_ids

for (k in 1:length(subject_ids)){
  preference$pref_pred[k] <- mean (music_task_data$pref_pred[music_task_data$ppnr == subject_ids[k]], na.rm = TRUE)
}

preference_entropy <- matrix(nrow = length(unique(music_task_data$s1.nextentropy)), ncol = 3)
preference_entropy <- as.data.frame(preference_entropy)
colnames(preference_entropy) <- c("nextentropy", "mean_preference", "sem_preference")
preference_entropy$nextentropy <- unique(music_task_data$s1.nextentropy)
for (i in 1:length(unique(music_task_data$s1.nextentropy))) {
  preference_entropy$mean_preference[i] <- mean(1-(music_task_data$r[music_task_data$s1.nextentropy == preference_entropy$nextentropy[i]] - 1))
  preference_entropy$sem_preference[i] <- sd(1-(music_task_data$r[music_task_data$s1.nextentropy == preference_entropy$nextentropy[i]] - 1)) / sqrt(156)
}

ggplot(preference_entropy) +
  geom_ribbon(aes(x=nextentropy, ymin = (mean_preference - sem_preference) * 100, ymax = (mean_preference + sem_preference)*100), fill = "grey70") +
  geom_line(aes (x = nextentropy, y = mean_preference * 100), stat = "identity") +
  ylab ("preference (% chosen)") +
  xlab ("second-order entropy") +
  theme_classic() +
  geom_hline(yintercept = 50, linetype = "dotted")

ggsave("Preference_Vs_2ndOrderEntropy.png")

# GLMM
fit = glmer(rb ~ (1|ppnr) + s1.nextentropyC + s1.nextentropy2C + s2.nextentropyC + s2.nextentropy2C, music_task_data, family = "binomial")
summary(fit)

#                     Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)      -0.08551    0.02942  -2.906 0.003658 ** 
#   s1.nextentropyC  -0.17520    0.01578 -11.100  < 2e-16 ***
#   s1.nextentropy2C  0.05927    0.01585   3.739 0.000185 ***
#   s2.nextentropyC   0.26592    0.01593  16.692  < 2e-16 ***
#   s2.nextentropy2C -0.08130    0.01617  -5.028 4.95e-07 ***

fit1 = glmer(rb ~ (1|ppnr) + s1.nextentropyC + s1.nextentropy2C + s2.nextentropyC + s2.nextentropy2C, music_task_data, family = "binomial")
fit2 = glmer(rb ~ (1 + s1.nextentropyC| ppnr) + s1.nextentropyC + s1.nextentropy2C + s2.nextentropyC + s2.nextentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s1.nextentropy2C| ppnr) + s1.nextentropyC + s1.nextentropy2C + s2.nextentropyC + s2.nextentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2) 
fit2 = glmer(rb ~ (1 + s2.nextentropyC| ppnr) + s1.nextentropyC + s1.nextentropy2C + s2.nextentropyC + s2.nextentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)
fit2 = glmer(rb ~ (1 + s2.nextentropy2C| ppnr) + s1.nextentropyC + s1.nextentropy2C + s2.nextentropyC + s2.nextentropy2C, music_task_data, family = "binomial")
anova(fit1,fit2)

fit = glmer(rb ~ (1 + s1.nextentropyC + s2.nextentropyC|ppnr) + s1.nextentropyC + s1.nextentropy2C + s2.nextentropyC + s2.nextentropy2C, music_task_data, family = "binomial")
summary(fit)
Anova(fit,type=3)

#                     Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)      -0.08834    0.03010  -2.935 0.003331 ** 
#   s1.nextentropyC  -0.18746    0.02352  -7.971 1.58e-15 ***
#   s1.nextentropy2C  0.05968    0.01615   3.694 0.000221 ***
#   s2.nextentropyC   0.28013    0.02723  10.288  < 2e-16 ***
#   s2.nextentropy2C -0.08595    0.01654  -5.198 2.01e-07 ***