######################################
### POSTCONSONANTAL VOWEL DURATION ###
######################################
# written by E. Chodroff, edited by Ishwarya Sutharsan
# 28 November 2024

### INSTALL PACKAGES - skip if you already have these
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("sjPlot")

### IMPORT PACKAGES
require(tidyverse)
require(ggrepel)
require(ggplot2)
require(dplyr)

##################
### CHANGE ME! ###
##################
# SET PATHS TO DATA FILES - don't forget the slash at the end
file_dir <- "C:/Users/ishwa/OneDrive - Universität Zürich UZH/Desktop/Uni/Uni_ZH/Computerlinguistik/BA_Arbeit/praat/processed_files/txt-FilesSpeakerID/"
results_dir <- "C:/Users/ishwa/OneDrive - Universität Zürich UZH/Desktop/Uni/Uni_ZH/Computerlinguistik/BA_Arbeit/praat/processed_files/Results/"
##################

### IMPORT FILES
files <- list.files(file_dir, "*.txt")
df <- data.frame()
for (file in 1:length(files)) {
  d.i <- read_tsv(paste0(file_dir, files[file]))
  df <- rbind(df, d.i)
}

# collapse vowel labels into /i/, /a/, and /u/
df <- df %>%
  mutate(Vowel = gsub("ɪ", "i", Vowel),
         Vowel = gsub("ɨ", "i", Vowel),
         Vowel = gsub("ʊ", "u", Vowel),
         Vowel = gsub("ɯ", "u", Vowel),
         Vowel = gsub("ɑ", "a", Vowel),
         Vowel = gsub("æ", "a", Vowel))

# Change IPA ɡ to normal typed g
df$Consonant_before_vowel <- ifelse(df$Consonant_before_vowel == "ɡ", "g", df$Consonant_before_vowel)

### Copy the data import in case something goes wrong (you then won't have to reimport)
df_orig <- df

################################
### SET-UP IMPORTANT COLUMNS ###
################################

# normalize vowel duration by speaking rate (# of segments per second) 
df$Nvdur <- (df$"Duration_V(ms)" / df$Speaker_rate)
df$Nvdur <- round(df$Nvdur, digits = 2)
df$voicing <- ifelse(df$Consonant_before_vowel %in% c("p", "t", "k"), "vcl", "vcd")
df$height <- ifelse(df$Vowel %in% c("i", "u"), "high", "low")
df$place <- ifelse(df$Consonant_before_vowel %in% c("b", "p"), "bilabial", ifelse(df$Consonant_before_vowel %in% c("t", "d"), "alveolar", "velar"))

#######################
### STANDARDIZATION ###
#######################

# only keep data where speaker has 5 times a voiceless plosive and 5 times a voiced plosive before a vowel
spkr_counts <- df %>% 
  group_by(language, speaker_id, voicing) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = voicing, values_from = count) %>% 
  mutate(keep_spkr = ifelse(vcd < 5 | vcl < 5, "drop", "keep"))

keep_spkrs <- subset(spkr_counts, keep_spkr == "keep")$speaker_id

df <- subset(df, speaker_id %in% keep_spkrs)

##############
### COUNTS ###
##############

# How many speakers per language?
spkrs_per_lang <- df %>% 
  group_by(language) %>% 
  summarise(count = length(unique(speaker_id)))
            
# How many tokens after voiced vs voiceless per language (ignoring counts per speaker)
voice_counts <- df %>% 
  group_by(language, voicing) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = voicing, values_from = count)

summary(voice_counts$vcd)


##############
### MEDIAN ###
##############

vcd_vcl_per_speaker <- df %>%
  group_by(language, speaker_id, voicing) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = voicing, values_from = count)


# get median of voiced phones per speaker  in each language
median_per_lang <- vcd_vcl_per_speaker %>%
  group_by(language) %>%
  summarise(
    median_vcd = median(vcd),
    median_vcl = median(vcl), 
  )

# get mean of voiced phones per speaker  in each language
mean_per_lang <- vcd_vcl_per_speaker %>%
  group_by(language) %>%
  summarise(
    mean_vcd = mean(vcd),
    mean_vcl = mean(vcl), 
  )
summary(mean_per_lang)


##############################
### DESCRIPTIVE STATISTICS ###
##############################
# raw
raw_voicing_avg_lang <- df %>%
  group_by(language, speaker_id, voicing) %>%
  summarise(spkr_vdur = mean(`Duration_V(ms)`)) 

raw_voicing_avg_lang_wide <- df %>%
  group_by(language, speaker_id, voicing) %>%
  summarise(spkr_vdur = mean(`Duration_V(ms)`)) %>%
  group_by(language, voicing) %>%
  summarise(grand_mean_vdur = mean(spkr_vdur)) %>%
  pivot_wider(names_from = voicing, values_from = grand_mean_vdur) %>%
  mutate(voice_diff = vcd - vcl)
# 6/10 show the effect numerically, # 4/10 do not show the effect (negative number)

# normalized
norm_voicing_avg_lang <- df %>%
  group_by(language, speaker_id, voicing) %>%
  summarise(spkr_vdur = mean(Nvdur)) 

norm_voicing_avg_lang_wide <- df %>%
  group_by(language, speaker_id, voicing) %>%
  summarise(spkr_vdur = mean(Nvdur)) %>%
  group_by(language, voicing) %>%
  summarise(grand_mean_vdur = mean(spkr_vdur)) %>%
  pivot_wider(names_from = voicing, values_from = grand_mean_vdur) %>%
  mutate(voice_diff = vcd - vcl)
# 5/10 show the effect, 5/10 do not (numerically) 



# boxplot of distribution of speaker means per language and voicing status
ggplot(raw_voicing_avg_lang) + geom_boxplot(aes(x = language, y = spkr_vdur, color = voicing)) + 
  theme_minimal(20) + ylab("Speaker mean raw durations (ms)") + xlab("Language") +
  scale_color_discrete(name = "voicing", labels = c("Voiced", "Voiceless")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13))


ggplot(norm_voicing_avg_lang) + geom_boxplot(aes(x = language, y = spkr_vdur, color = voicing)) + 
  theme_minimal(20) + ylab("Speaker mean normalized durations") + xlab("Language") +
  scale_color_discrete(name = "voicing", labels = c("Voiced", "Voiceless")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13))


ggsave(paste0(results_dir, "raw_voweldur.jpeg"),
       plot = last_plot(), dpi = 300, height = 8, width = 8, units = "in")



# create box plot for vowel height 
norm_vowelheight <- df %>%
  group_by(language, speaker_id, height) %>%
  summarise(spkr_vdur = mean(Nvdur)) 

norm_vowelheight_wide <- df %>%
  group_by(language, speaker_id, height) %>%
  summarise(spkr_vdur = mean(Nvdur)) %>%
  group_by(language, height) %>%
  summarise(grand_mean_vdur = mean(spkr_vdur)) %>%
  pivot_wider(names_from = height, values_from = grand_mean_vdur) %>%
  mutate(height_diff = low - high)

ggplot(norm_vowelheight) + geom_boxplot(aes(x = language, y = spkr_vdur, color = height)) + 
  theme_minimal(20) + ylab("Speaker mean normalized vowel durations") + xlab("Language") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13))

ggsave(paste0(results_dir, "vowelheight_boxplot.jpeg"),
       plot = last_plot(), dpi = 300, height = 8, width = 8, units = "in")

# create box plot for POA
norm_place_articulation <- df %>%
  group_by(language, speaker_id, place) %>%
  summarise(spkr_vdur = mean(Nvdur)) 

ggplot(norm_place_articulation) + geom_boxplot(aes(x = language, y = spkr_vdur, color = place)) + 
  theme_minimal(20) + ylab("Speaker mean normalized vowel durations") + xlab("Language") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13))

ggsave(paste0(results_dir, "vowelplaceofarticulation.jpeg"),
       plot = last_plot(), dpi = 300, height = 8, width = 8, units = "in")

#########################
### LINEAR REGRESSION ###
#########################
require(lme4)
require(lmerTest)
require(sjPlot)

df$voicing <- factor(df$voicing, levels = c("vcd", "vcl"))
contrasts(df$voicing) <- contr.sum(2)
contrasts(df$voicing)

df$height <- factor(df$height, levels = c("low", "high"))
contrasts(df$height) <- contr.sum(2)
contrasts(df$height)

df$place <- factor(df$place, levels = c("bilabial", "alveolar", "velar"))
contrasts(df$place) <- contr.sum(3)
contrasts(df$place)

# without by-speaker slope for voicing
fit_norm_albanian <- lmer(Nvdur ~ voicing + height + place + (1 | speaker_id), subset(df, language == "Albanian"))
summary(fit_norm_albanian)

# with by-speaker slope for voicing: 
fit_norm_albanian <- lmer(Nvdur ~ voicing + height + place + (1  + voicing | speaker_id), subset(df, language == "Albanian"))
summary(fit_norm_albanian)
# orig: voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: negative but not significant 

fit_norm_bashkir<- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Bashkir"))
summary(fit_norm_bashkir)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+) and p < 0.05 

fit_norm_czech <- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Czech"))
summary(fit_norm_czech)
# voicing effect in the expected direction (+) but not significantly different from 0
# with by-speaker slope for voicing: voicing effect in the expected direction (+)  but not significantly different from 0

fit_norm_hungarian <- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Hungarian"))
summary(fit_norm_hungarian)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+) and p < 0.05

fit_norm_indonesian <- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Indonesian"))
summary(fit_norm_indonesian)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+)  but not significantly different from 0

fit_norm_italian <- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Italian"))
summary(fit_norm_italian)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+) and p < 0.05

fit_norm_polish <- lmer(Nvdur ~ voicing + height + place + (1 | speaker_id), subset(df, language == "Polish"))
summary(fit_norm_polish)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+) and p < 0.05

fit_norm_portuguese <- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Portuguese"))
summary(fit_norm_portuguese)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+) and p < 0.05

fit_norm_russian <- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Russian"))
summary(fit_norm_russian)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+) and p < 0.05

fit_norm_ukrainian <- lmer(Nvdur ~ voicing + height + place + (1 + voicing | speaker_id), subset(df, language == "Ukrainian"))
summary(fit_norm_ukrainian)
# voicing effect in the expected direction (+) and p < 0.05
# with by-speaker slope for voicing: voicing effect in the expected direction (+) and p < 0.05



#############
### PLOTS ###
#############

# create a plot for each language (Appendix)

plot_model(fit_norm_russian, type="pred", terms=c("voicing","speaker_id"),
           pred.type="re", ci.lvl=NA) + labs(x = "Voicing",
                                             y = "Normalized vowel duration",
                                             title = "Predicted values Nvdur Russian") +
  theme_minimal(13) + scale_color_discrete(name = "Speaker ID") + theme(legend.position = "none")

# Save the plot
ggsave(paste0(results_dir, "predicted_values_Russian.jpeg"),
       plot = last_plot(), dpi = 300, height = 8, width = 8, units = "in") 

############################
### CONFORMITY OF EFFECT ###
############################

# add a data frame to check how many times vcd was longer than vcl
speaker_data <- df %>%
  group_by(language, speaker_id, voicing) %>%
  summarise(mean_vowel_duration = mean(Nvdur)) %>%
  pivot_wider(names_from = voicing, values_from = mean_vowel_duration, names_prefix = "voicing_") %>%
  mutate(positive_effect = voicing_vcd > voicing_vcl) 

# calculate the conformity of effect per language
conformity <- speaker_data %>%
  group_by(language) %>%
  summarise(conformity_percentage = mean(positive_effect) * 100)

ggplot(conformity, aes(x = language, y = conformity_percentage, fill = language)) + 
  geom_col(fill= "steelblue") + 
  labs(x = "Language", y = "Conformity of effect (%)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13)) +  theme(legend.position = "none")

# Save the plot
ggsave(paste0(results_dir, "conformity_effectbar.jpeg"),
       plot = last_plot(), dpi = 300, height = 8, width = 8, units = "in") 
