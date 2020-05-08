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

