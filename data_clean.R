######################################################################
# DATA_CLEAN.R                                                       #
# ================================================================== #
# This script takes input from Qualtrics CSV export (for pre- and    #
# post-test questionnaires) and from the Congruity stimulus software #
# itself (for sequencing) and cleans it in a reproducible way,       #
# ouputting the study data in a format suitable for analysis.        #
######################################################################

# setup -------------------------------------------------------------------
library(readr)
library(dplyr)
library(psych)
library(fuzzyjoin)

# input -------------------------------------------------------------------

# Load in pre-test questionnaire data
pre <- read_csv("data/congruity_pre.csv", 
  col_types = cols(EDU = col_factor(levels = c("High School",  "Bachelor's Degree",
                                               "Master's Degree", "Doctoral Degree")), 
                   GENDER = col_factor(levels = c("Female",  "Male", "Non-binary")), 
                   RecordedDate = col_datetime(format = "%m/%d/%y %H:%M")))

# Load in post-test questionnaire data
# (this is ultimately more important than the pre-test stuff)
post <- read_csv("congruity_post.csv", 
  col_types = cols(RecordedDate = col_datetime(format = "%m/%d/%y %H:%M"), 
                   condition = col_factor(levels = c("SENSORY", "ENVIRONMENTAL", "THEMATIC")), 
                    control = col_factor(levels = c("CONTROL", "BREACH"))))

# clean -------------------------------------------------------------------

# Remove incomplete responses
pre %>% filter(Finished) %>% select(-Finished) -> pre
post %>% filter(Finished) %>% select(-Finished) -> post

# Remove partial responses noted in study log, matching based on time window
filter_windows <- data.frame(start=c(ymd_hms("2019-11-14-12-30-00"), 
                                     ymd_hms("2019-11-18-11-30-00"),
                                     ymd_hms("2019-11-21-11-35-00"),
                                     ymd_hms("2020-02-03-10-00-00")),
                             end=c(ymd_hms("2019-11-14-12-35-59"), 
                                   ymd_hms("2019-11-18-11-45-59"),
                                   ymd_hms("2019-11-21-11-40-59"),
                                   ymd_hms("2020-02-03-10-15-59")))

for (i in 1:nrow(filter_windows)) {
  pre %>% filter(!(RecordedDate > filter_windows[i,1] & RecordedDate < filter_windows[i,2])) -> pre
}

# score -------------------------------------------------------------------

# Score ITQ items and compute reliability
itq_keys <- make.keys(pre, list(ITQ_Total=sprintf("ITQ%d", seq(1:18)),
                                ITQ_Focus=sprintf("ITQ%d", c(1, 2, 3, 8, 13)),
                                ITQ_Involvement=sprintf("ITQ%d", c(4, 5, 10, 12, 18)),
                                ITQ_Emotions=sprintf("ITQ%d", c(11, 15, 16, 17)),
                                ITQ_Jeu=sprintf("ITQ%d", c(6, 9, 14))))

itq_scores <- scoreItems(itq_keys, pre, totals=TRUE)

# Score SPQ items and compute reliability
spq_keys <- make.keys(post, list(SSM=sprintf("SS%d",seq(1:8)),
                                 SPSL=sprintf("SPSL%d", seq(1:8)),
                                 SoD=c("-SOD1", "SOD2", "-SOD3", "-SOD4", 
                                  "-SOD5", "-SOD6", "-SOD7", "SOD8"),
                                 SPQ=c(sprintf("SS%d",seq(1:8)),
                                        sprintf("SPSL%d", seq(1:8)),
                                         c("-SOD1", "SOD2", "-SOD3", "-SOD4", 
                                           "-SOD5", "-SOD6", "-SOD7", "SOD8"))))

spq_scores <- scoreItems(spq_keys, post)

# Check reliability diagnostics (Cronbach's alpha)
print(itq_scores$alpha, digits=3)
print(spq_scores$alpha, digits=3)

# Bind aggregate scores to dataset
pre %>% bind_cols(as.data.frame(itq_scores$scores)) -> pre
post %>% bind_cols(as.data.frame(spq_scores$scores)) -> post

# Calculate Simulator Sickness Scores
post %>% 
  select_at(vars(starts_with("SimSick"))) %>%  
  replace(is.na(.), 0) %>%
  transmute(SimSick = rowSums(.)) %>%
  bind_cols(post, .) -> post

# Trim redundant columns
pre %>%
  select(RecordedDate, GENDER, RACE, AGE_1, EDU,
         ITQ_Total, ITQ_Focus, ITQ_Involvement, ITQ_Emotions, ITQ_Jeu) -> pre

post %>% 
  select(RecordedDate, condition, control, SPQ, SSM, SPSL, SoD, SimSick) -> post

# merge -------------------------------------------------------------------

# Merge pre- and post-test questionnaire responses based on fuzzy 
# date matching with a window of 10 minutes (from pre-test to first post-test)

post %>% 
  arrange(RecordedDate) %>%
  group_by(id = rep(row_number(), length.out = n(), each = 3)) %>%
  mutate(RecordedDate = min(RecordedDate)) %>%
  difference_join(pre, ., by="RecordedDate", 
                mode = "right", max_dist = 10) -> joined

## Clean up variable names and order
joined %>%
  select(id, RecordedDate.x,
         GENDER, RACE, AGE_1, EDU,
         ITQ_Total, ITQ_Focus, ITQ_Involvement, ITQ_Emotions, ITQ_Jeu,
         condition, control,
         SPQ, SSM, SPSL, SoD, SimSick) -> joined

colnames(joined) <- c("id", "date",
                      "gender", "race", "age", "edu",
                      "ITQ", "ITQ_focus", "ITQ_involvement", "ITQ_emotions", "ITQ_jeu",
                      "condition", "control",
                      "SPQ", "SSM", "SPSL", "SoD", "sim_sick")

# export ------------------------------------------------------------------

# Export as RDATA and CSV
write_csv(joined, "data/congruity_data.csv", col_names = TRUE)
saveRDS(joined, "data/congruity_data.rds")
