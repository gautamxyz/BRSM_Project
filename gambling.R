# setwd("~/Desktop/3-2/brsm/BRSM_Project/data")
# setwd("/Users/kakarot/Desktop/BRSM PROJECT/Data")
setwd ("C:\\Users\\Lenovo\\Documents\\RawData")

library(ggplot2)
library(Hmisc)

questionnaire_data <- read.csv ("QuestionnaireData.csv", header=TRUE, stringsAsFactors = FALSE)

n <- 164 # needs to be changed
participants_to_be_removed <- c(97, 105, 117, 36,  44,  90,  93,  95, 106, 134, 136, 155) 
# according to authors participants 97, 105, 117 had inconsistent AQ/SRS scores 
# and the others didn't choose each deck at least 5 times
subject_list <- 1:n
subject_list <- subject_list [! subject_list %in% participants_to_be_removed]
gambling_data <- NULL
len_subject_list <- length(subject_list)

questionnaire_data <- questionnaire_data[-participants_to_be_removed,]

# Define a function to read data for a single participant
read_participant_data <- function(participant_id) {
  file_name <- ifelse(participant_id < 10, 
                      sprintf("GamblingTask/gambling_ppn0%d.rtd", participant_id), 
                      sprintf("GamblingTask/gambling_ppn%d.rtd", participant_id))
  read.table(file_name, header = TRUE)
}

# Create a list of data frames, with one element for each participant
participant_data_list <- lapply(subject_list, read_participant_data)

# Combine the list of data frames into a single data frame
gambling_data <- do.call(rbind, participant_data_list)


choices <- matrix (nrow =len_subject_list, ncol = 5)
colnames(choices) <- c("ppnr", "fixed", "SD10", "SD30", "SD70")
choices <- as.data.frame(choices)
choices$ppnr <- subject_list

for (k in 1:len_subject_list){
  choices$SD70[k] <- length(gambling_data$real_r[gambling_data$real_r == 3 & gambling_data$ppnr == subject_list[k]])
  choices$SD30[k] <- length(gambling_data$real_r[gambling_data$real_r == 2 & gambling_data$ppnr == subject_list[k]]) 
  choices$SD10[k] <- length(gambling_data$real_r[gambling_data$real_r == 1 & gambling_data$ppnr == subject_list[k]]) 
  choices$fixed[k] <- length(gambling_data$real_r[gambling_data$real_r == 0 & gambling_data$ppnr == subject_list[k]])
}

choices$ppnr[rowSums (choices[,2:5] < 5) > 0] # which participants chose one (or more) deck(s) less than 5 times
# so exclude these pp: 36,  44,  90,  93,  95, 106, 134, 136, 155

reaction_time_mean <- matrix (nrow =len_subject_list, ncol = 3)
colnames(reaction_time_mean) <- c("ppnr", "mean", "sd")
reaction_time_mean <- as.data.frame(reaction_time_mean)
reaction_time_mean$ppnr <- subject_list

for (k in 1:len_subject_list){
  reaction_time_mean$mean[k] <- mean(gambling_data$rt[gambling_data$ppnr == subject_list[k]])
  reaction_time_mean$sd[k] <- sd(gambling_data$rt[gambling_data$ppnr == subject_list[k]])
}

for (i in 1:nrow(gambling_data)) {
  if (gambling_data$rt[i] - (reaction_time_mean$mean[reaction_time_mean$ppnr ==  gambling_data$ppnr[i]]) > (3 * reaction_time_mean$sd[reaction_time_mean$ppnr ==  gambling_data$ppnr[i]])) {
    gambling_data[i,10:11] <- NA
  }
}
gambling_data_with_na <- gambling_data
na_rows <- c()
for (i in 1:nrow(gambling_data)) {
  if (any(is.na(gambling_data[i,]))) {
    na_rows <- c(na_rows, i)
  }
}
gambling_data <- gambling_data[-na_rows, ]

preference <- matrix (nrow =len_subject_list, ncol = 5)
colnames(preference) <- c("ppnr", "fixed", "SD10", "SD30", "SD70")
preference <- as.data.frame(preference)
preference$ppnr <- subject_list

for (k in 1:len_subject_list){
  preference$SD70[k] <- (length(which(gambling_data$real_r == 3 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k])))
  preference$SD30[k] <- (length(which(gambling_data$real_r == 2 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k]))) 
  preference$SD10[k] <- (length(which(gambling_data$real_r == 1 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k]))) 
  preference$fixed[k] <- (length(which(gambling_data$real_r == 0 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k]))) 
}

# barplot
preference_mean <- as.data.frame(colMeans (preference[,c(2:5)]*100))
preference_mean$sem <- (apply(preference[,c(2:5)]*100, 2, sd))/sqrt(len_subject_list)
preference_mean$deck <- factor(c("fixed", "SD = 10", "SD = 30", "SD = 70"), levels = c("fixed", "SD = 10", "SD = 30", "SD = 70"))
colnames(preference_mean) <- c("mean", "sem", "deck")

ggplot (preference_mean) + geom_bar (aes (x = deck, y = mean), stat = "identity", fill = "gray") +  
  geom_errorbar (aes (x = deck, ymin = mean - sem, ymax = mean + sem), width=0.4, size=0.4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.text.x = element_text(size = 16), axis.text.y =  element_text(size = 20), 
        axis.ticks.x = element_blank(), axis.title=element_text(size=20), axis.title.x = element_blank()) +
  ylab ("preference (percentage wise)")

ggsave('Preference_Vs_Deck.png')

mean(gambling_data$rt)
sd(gambling_data$rt)

reaction_time <- matrix (nrow =len_subject_list, ncol = 6)
colnames(reaction_time) <- c("ppnr", "fixed", "SD10", "SD30", "SD70", "mean")
reaction_time <- as.data.frame(reaction_time)
reaction_time$ppnr <- subject_list

for (k in 1:len_subject_list){
  reaction_time$fixed[k] <- mean(gambling_data$rt[gambling_data$real_r == 0 & gambling_data$ppnr == subject_list[k]]) 
  reaction_time$SD10[k] <- mean(gambling_data$rt[gambling_data$real_r == 1 & gambling_data$ppnr == subject_list[k]]) 
  reaction_time$SD30[k] <- mean(gambling_data$rt[gambling_data$real_r == 2 & gambling_data$ppnr == subject_list[k]]) 
  reaction_time$SD70[k] <- mean(gambling_data$rt[gambling_data$real_r == 3 & gambling_data$ppnr == subject_list[k]])
  reaction_time$mean[k] <- mean(gambling_data$rt[gambling_data$ppnr == subject_list[k]]) 
}

# barplot mean

# Create a data frame with the mean reaction times and the corresponding standard deviations
df <- data.frame(
  conditions = c("Fixed", "SD = 10", "SD = 30", "SD = 70"),
  means = c(mean(reaction_time[,2]), mean(reaction_time[,3]), mean(reaction_time[,4]), mean(reaction_time[,5])),
  sd = c(0, 10, 30, 70)
)

# Create the bar plot
ggplot(df, aes(x = conditions, y = means)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "", y = "Reaction Time") +
  ylim(0, 600) +
  theme_classic()

ggsave('Reaction_Time_Vs_Deck.png')

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


corr_data <- as.matrix(cbind (preference$fixed, questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("preference_fixed", "AQ_total", "SRS_total", "factorscores")

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

ggsave("Correlations_of_Deck_with_totals_S.png")

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

ggsave("Correlations_of_Deck_with_totals_P.png")

corr_data <- as.matrix(cbind (((preference$fixed*1) + (preference$SD10*2) + (preference$SD30*3) + (preference$SD70*4)), questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("weighted_sum_score", "AQ_total", "SRS_total", "factorscores")

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

ggsave("Correlations_of_weightedsums_with_totals_S.png")

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

ggsave("Correlations_of_weightedsums_with_totals_P.png")

reaction_time_diff <- (rowMeans(reaction_time[,3:5]) - reaction_time$fixed)
corr_data <- as.matrix(cbind (reaction_time_diff, questionnaire_data[,c(5, 12, 17)]))
colnames(corr_data) <- c("RT_diff", "AQ_total", "SRS_total", "factorscores")
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

ggsave("Correlations_of_RTDiff_with_totals_S.png")

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

ggsave("Correlations_of_RTDiff_with_totals_P.png")

corr_data <- as.data.frame(corr_data)
p1 <- ggplot(corr_data, aes(x=factorscores, y=RT_diff)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
         axis.title=element_text(size=10), axis.text = element_text(size=10), aspect.ratio=1) +
  xlab("PCA factor") +
  ylab("Reaction time difference")

p2 <- ggplot(corr_data, aes(x=AQ_total, y=RT_diff)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
         axis.title=element_text(size=10), axis.text = element_text(size=10), aspect.ratio=1,
        axis.title.y = element_blank()) +
  xlab("AQ")+ ylab("Reaction time difference")

p3 <- ggplot(corr_data, aes(x=SRS_total, y=RT_diff)) + geom_point(size = 2) +
  geom_smooth(method=lm, se=FALSE, col = "black", size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
         axis.title=element_text(size=10), axis.text = element_text(size=10), aspect.ratio=1,
        axis.title.y = element_blank()) +
  xlab("SRS-A")+ ylab("Reaction time difference")

g <- grid.arrange(p1, p2, p3, nrow = 1)
ggsave(file='RT_vs_components.png', g)

# display plots individually
corr_data <- as.matrix(cbind(reaction_time_diff, questionnaire_data[,c(5:10, 12:17)]))
colnames(corr_data) <- c("RT_diff", "AQ_total", "AQ_social", "AQ_switch", "AQ_detail", "AQ_comm", "AQ_imag", "SRS_total", "SRS_consc", "SRS_comm", "SRS_motiv", "SRS_rigid", "factorscores")
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

ggsave("Correlations_of_RTDiff_with_partwise_S.png")
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

ggsave("Correlations_of_RTDiff_with_partwise_P.png")

print(p1)
print(p2)
print(p3)
