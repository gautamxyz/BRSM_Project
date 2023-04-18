setwd ("C:\\Users\\Lenovo\\Documents\\RawData")

library(Hmisc)

# music participants
n <- 164
remove <- c (23, 28, 143, 151, 152, 97, 105, 117)
# 97, 105, 117 had inconsistent AQ/SRS scoring for the others there was a technical problem during the task
subject_list <- 1:n
subject_list <- subject_list [! subject_list %in% remove]


music_data <- NULL
for (k in 1:length(subject_list)){
  if (subject_list[k] < 10) {
    music_data <- rbind(music_data, read.table(sprintf("MusicPreferenceTask/music_ppn0%s.rtd", subject_list[k]), header = TRUE))}
  if (subject_list[k] > 9) {
    music_data <- rbind(music_data, read.table(sprintf("MusicPreferenceTask/music_ppn%s.rtd", subject_list[k]), header = TRUE))}
}

colnames(music_data) <- c("ppnr", "trialnr", "s1.entclass", "s1.stimnr", "s2.entclass", "s2.stimnr", "r", "s1.tone1", "s1.tone2", "s1.tone3", 
                          "s1.tone4", "s1.tone5", "s1.tone6", "s1.tone7", "s1.sameentropy", "s1.nextentropy", "s1.entropy", "s2.tone", "s2.tone2",
                          "s2.tone3", "s2.tone4", "s2.tone5", "s2.tone6", "s2.tone7", "s2.sameentropy", "s2.nextentropy", "s2.entropy")

# add variable preference for most predictable sequence
for (i in 1:nrow(music_data)){
  if (music_data[i,3] > music_data[i,5] & music_data[i,7] == 1) music_data[i,28] = 0
  if (music_data[i,3] < music_data[i,5] & music_data[i,7] == 2) music_data[i,28] = 0
  if (music_data[i,3] > music_data[i,5] & music_data[i,7] == 2) music_data[i,28] = 1
  if (music_data[i,3] < music_data[i,5] & music_data[i,7] == 1) music_data[i,28] = 1
}
colnames(music_data)[28] <- c("pref_pred")

# preference for most predictable sequence per participant
music_preference <- matrix (nrow =length(subject_list), ncol = 14)
colnames(music_preference) <- c("ppnr", "class1", "class2", "class3", "class4", "class5", "class6", "class7", "class8", "class9", "class10", "class11", "weighted_sum", "pref_pred")
music_preference <- as.data.frame(music_preference)
music_preference$ppnr <- subject_list

for (k in 1:length(subject_list)){
  music_preference$class1[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 1 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 1 & music_data$ppnr == subject_list[k])) # how often did they choose a sequence from class 1
  music_preference$class2[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 2 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 2 & music_data$ppnr == subject_list[k])) # how often did they choose a sequence from class 2
  music_preference$class3[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 3 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 3 & music_data$ppnr == subject_list[k])) # etc.
  music_preference$class4[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 4 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 4 & music_data$ppnr == subject_list[k]))
  music_preference$class5[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 5 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 5 & music_data$ppnr == subject_list[k]))
  music_preference$class6[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 6 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 6 & music_data$ppnr == subject_list[k]))
  music_preference$class7[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 7 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 7 & music_data$ppnr == subject_list[k]))
  music_preference$class8[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 8 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 8 & music_data$ppnr == subject_list[k]))
  music_preference$class9[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 9 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 9 & music_data$ppnr == subject_list[k]))
  music_preference$class10[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 10 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 10 & music_data$ppnr == subject_list[k]))
  music_preference$class11[k] <- length(which(music_data$r == 1 & music_data$s1.entclass == 11 & music_data$ppnr == subject_list[k])) + 
    length(which(music_data$r == 2 & music_data$s2.entclass == 11 & music_data$ppnr == subject_list[k]))
  music_preference$weighted_sum[k] <- (music_preference$class1[k]*11) + (music_preference$class2[k]*10) + (music_preference$class3[k]*9) + 
    (music_preference$class4[k]*8) + (music_preference$class5[k]*7) + (music_preference$class6[k]*6) + (music_preference$class7[k]*5) + 
    (music_preference$class8[k]*4) + (music_preference$class9[k]*3) + (music_preference$class10[k]*2) + (music_preference$class11[k]*1) # weighted sum score of the preferences for the different classes 
  music_preference$pref_pred[k] <- mean (music_data$pref_pred[music_data$ppnr == subject_list[k]]) # how often did they choose the most predictable trial
}

# add rows with NA for participants with technical problem in this task
extra <- data.frame(rbind (cbind (23, t(rep(NA, 13))),
                           cbind (28, t(rep(NA, 13))),
                           cbind (143, t(rep(NA, 13))),
                           cbind (151, t(rep(NA, 13))),
                           cbind (152, t(rep(NA, 13)))))
colnames (extra) <- c("ppnr", "class1", "class2", "class3", "class4", "class5", "class6", "class7", "class8", "class9", "class10", "class11", "weighted_sum", "pref_pred")
music_preference <- rbind (music_preference, extra)
music_preference <- music_preference[order(music_preference$ppnr),]

# fluency participants
n <- 164
remove <- c(97, 105, 117) # 3 participants with inconsistent AQ/SRS scores
subject_list <- 1:n
subject_list <- subject_list [! subject_list %in% remove]

fluency_data <- NULL
for (k in 1:length(subject_list)){
  if (subject_list[k] < 10) {
    fluency_data <- rbind(fluency_data, read.table(sprintf("PerceptualFluencyTask/fluency_ppn0%s.rtd", subject_list[k]), header = TRUE))}
  if (subject_list[k] > 9) {
    fluency_data <- rbind(fluency_data, read.table(sprintf("PerceptualFluencyTask/fluency_ppn%s.rtd", subject_list[k]), header = TRUE))}
}

# remove trials with RT > 3 SD from individual means
reaction_time_mean <- matrix (nrow =length(subject_list), ncol = 3)
colnames(reaction_time_mean) <- c("ppnr", "mean", "sd")
reaction_time_mean <- as.data.frame(reaction_time_mean)
reaction_time_mean$ppnr <- subject_list

for (k in 1:length(subject_list)){
  reaction_time_mean$mean[k] <- mean(fluency_data$rt[fluency_data$ppnr == subject_list[k]])
  reaction_time_mean$sd[k] <- sd(fluency_data$rt[fluency_data$ppnr == subject_list[k]])
}

for (i in 1:nrow(fluency_data)) {
  if (fluency_data$rt[i] > (reaction_time_mean$mean[reaction_time_mean$ppnr ==  fluency_data$ppnr[i]] + (3 * reaction_time_mean$sd[reaction_time_mean$ppnr ==  fluency_data$ppnr[i]]))) {
    fluency_data[i,3:4] <- NA
  }
}

fluency_data_with_na <- fluency_data
fluency_data <- fluency_data[complete.cases (fluency_data),]

# preference for incongruent and congruent trials per participant
fluency_preference <- matrix (nrow =length(subject_list), ncol = 3)
colnames(fluency_preference) <- c("ppnr", "nonmatching", "matching")
fluency_preference <- as.data.frame(fluency_preference)
fluency_preference$ppnr <- subject_list

for (k in 1:length(subject_list)){
  fluency_preference$nonmatching[k] <- mean(fluency_data$preference[fluency_data$matching == 0 & fluency_data$ppnr == subject_list[k]]) # mean rating for incongruent trials
  fluency_preference$matching[k] <- mean(fluency_data$preference[fluency_data$matching == 1 & fluency_data$ppnr == subject_list[k]]) # mean rating for congruent trials
}

# gambling participants
n <- 164 
remove <- c(97, 105, 117, 36,  44,  90,  93,  95, 106, 134, 136, 155) 
# according to authors participants 97, 105, 117 had inconsistent AQ/SRS scores 
# and the others didn't choose each deck at least 5 times
subject_list <- 1:n
subject_list <- subject_list [! subject_list %in% remove]

gambling_data <- NULL
for (k in 1:length(subject_list)){
  if (subject_list[k] < 10) {
    gambling_data <- rbind(gambling_data, read.table(sprintf("GamblingTask/gambling_ppn0%s.rtd", subject_list[k]), header = TRUE))}
  if (subject_list[k] > 9) {
    gambling_data <- rbind(gambling_data, read.table(sprintf("GamblingTask/gambling_ppn%s.rtd", subject_list[k]), header = TRUE))}
}

# remove trials with RT > 3 SD from individual means
reaction_time_mean <- matrix (nrow =length(subject_list), ncol = 3)
colnames(reaction_time_mean) <- c("ppnr", "mean", "sd")
reaction_time_mean <- as.data.frame(reaction_time_mean)
reaction_time_mean$ppnr <- subject_list

for (k in 1:length(subject_list)){
  reaction_time_mean$mean[k] <- mean(gambling_data$rt[gambling_data$ppnr == subject_list[k]])
  reaction_time_mean$sd[k] <- sd(gambling_data$rt[gambling_data$ppnr == subject_list[k]])
}

for (i in 1:nrow(gambling_data)) {
  if (gambling_data$rt[i] > (reaction_time_mean$mean[reaction_time_mean$ppnr ==  gambling_data$ppnr[i]] + (3 * reaction_time_mean$sd[reaction_time_mean$ppnr ==  gambling_data$ppnr[i]]))) {
    gambling_data[i,10:11] <- NA
  }
}

gambling_data_with_na <- gambling_data
gambling_data <- gambling_data[complete.cases (gambling_data),]

# preference for each deck for every participant
gambling_preference <- matrix (nrow =length(subject_list), ncol = 5)
colnames(gambling_preference) <- c("ppnr", "fixed", "SD10", "SD30", "SD70")
gambling_preference <- as.data.frame(gambling_preference)
gambling_preference$ppnr <- subject_list

for (k in 1:length(subject_list)){
  gambling_preference$fixed[k] <- (length(which(gambling_data$real_r == 0 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k]))) # percentage of trials in which they chose the fixed deck
  gambling_preference$SD10[k] <- (length(which(gambling_data$real_r == 1 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k]))) # percentage of trials in which they chose the SD=10 deck
  gambling_preference$SD30[k] <- (length(which(gambling_data$real_r == 2 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k]))) # etc.
  gambling_preference$SD70[k] <- (length(which(gambling_data$real_r == 3 & gambling_data$ppnr == subject_list[k])) / length(which(gambling_data$ppnr == subject_list[k])))
}

# add rows with NA for participants that didn't choose each deck at least 5 times
extra <- data.frame(rbind (cbind (36, t(rep(NA, 4))),
                           cbind (44, t(rep(NA, 4))),
                           cbind (90, t(rep(NA, 4))),
                           cbind (93, t(rep(NA, 4))),
                           cbind (95, t(rep(NA, 4))),
                           cbind (106, t(rep(NA, 4))),
                           cbind (134, t(rep(NA, 4))),
                           cbind (136, t(rep(NA, 4))),
                           cbind (155, t(rep(NA, 4)))))
colnames (extra) <- c("ppnr", "fixed", "SD10", "SD30", "SD70")
gambling_preference <- rbind (gambling_preference, extra)
gambling_preference <- gambling_preference[order(gambling_preference$ppnr),]


# reaction time difference per participant
gambling_reaction_time <- matrix (nrow =length(subject_list), ncol = 6)
colnames(gambling_reaction_time) <- c("ppnr", "fixed", "SD10", "SD30", "SD70", "mean")
gambling_reaction_time <- as.data.frame(gambling_reaction_time)
gambling_reaction_time$ppnr <- subject_list

for (k in 1:length(subject_list)){
  gambling_reaction_time$fixed[k] <- mean(gambling_data$rt[gambling_data$real_r == 0 & gambling_data$ppnr == subject_list[k]]) # mean reaction time for fixed deck
  gambling_reaction_time$SD10[k] <- mean(gambling_data$rt[gambling_data$real_r == 1 & gambling_data$ppnr == subject_list[k]]) # mean reaction time for SD=10 deck
  gambling_reaction_time$SD30[k] <- mean(gambling_data$rt[gambling_data$real_r == 2 & gambling_data$ppnr == subject_list[k]]) # etc.
  gambling_reaction_time$SD70[k] <- mean(gambling_data$rt[gambling_data$real_r == 3 & gambling_data$ppnr == subject_list[k]])
  gambling_reaction_time$mean[k] <- mean(gambling_data$rt[gambling_data$ppnr == subject_list[k]]) # mean reaction time over all trials
}

extra <- data.frame(rbind (cbind (36, t(rep(NA, 5))),
                           cbind (44, t(rep(NA, 5))),
                           cbind (90, t(rep(NA, 5))),
                           cbind (93, t(rep(NA, 5))),
                           cbind (95, t(rep(NA, 5))),
                           cbind (106, t(rep(NA, 5))),
                           cbind (134, t(rep(NA, 5))),
                           cbind (136, t(rep(NA, 5))),
                           cbind (155, t(rep(NA, 5)))))
colnames (extra) <- c("ppnr", "fixed", "SD10", "SD30", "SD70", "mean")
gambling_reaction_time <- rbind (gambling_reaction_time, extra)
gambling_reaction_time <- gambling_reaction_time[order(gambling_reaction_time$ppnr),]

# correlation between paradigms
gambling_reaction_time_diff <- (rowMeans(gambling_reaction_time[,3:5]) - gambling_reaction_time$fixed)

corr_data <- as.matrix (cbind (music_preference$pref_pred, 
                              music_preference$weighted_sum,
                              (fluency_preference$matching-fluency_preference$nonmatching),
                              gambling_preference$fixed,
                              (gambling_preference$fixed*4) + (gambling_preference$SD10*3) + (gambling_preference$SD30*2) + (gambling_preference$SD70*1),
                              gambling_reaction_time_diff))

colnames(corr_data) <- c("music_predictable_chosen", "music_weighted_sum", "fluency_match_over_nonmatch", "gambling_fixed", "gambling_weighted_sum", "gambling_RT_diff")
rcorr(corr_data, type = "spearman")
rcorr(corr_data, type = "pearson")