setwd("/Users/apple/Desktop/SOCI50112/proposal/ICPSR_36873/codes")
library(tidyverse)

# import data
load("36873-0001-Data.rda", core_data_info <- new.env())
View(core_data_info)
core_data <- get(load("36873-0001-Data.rda"))
View(core_data)

load("36873-0003-Data.rda", network_data_info <- new.env())
View(network_data_info)
network_data <- get(load("36873-0003-Data.rda"))
View(network_data)

load("36873-0011-Data.rda", covid_info <- new.env())
View(covid_info)
covid_data <- get(load("36873-0011-Data.rda"))
View(covid_data)

# find the core network
core_network <- network_data %>%
  filter(SECTION == "(1) a")





# ------------- calculate the brigding potential ------------------- #

# The second measure: whether the respondent has at least one network member who
# is not connected to any of the other network members—that is, whether ego is 
# tied to an alter who is isolated with respect to the other alters.

# not applicable because the result is all zero

# create a data frame with ID and bridging potential (default: 0)
df <- data.frame(ID = unique(core_network$ID), bridg_po = 0)

# iterate each line of TALKFREQ1-6 with the same ID, if one line is empty, then the 
# bridging potential is 1, and then break the loop

for (i in 1:nrow(df)) {
  line_id <- df$ID[i]  # Access each 'id' value from the dataframe df
  
  # Check if line_id is not NA and exists in the core_network dataframe
  if (!is.na(line_id) && length(which(core_network$ID == line_id)) > 0) {
    confidants <- core_network %>% filter(ID == line_id)
    
    for (j in 1:nrow(confidants)) {
      confidant <- confidants[j, ]
      
      if (all(is.na(confidant[c("TALKFREQ1", "TALKFREQ2", "TALKFREQ3",
                                "TALKFREQ4", "TALKFREQ5", "TALKFREQ6")]))) {
        df[i, "bridg_po"] <- 1
        break  # Exit inner loop after setting bridg_po to 1
      }
      if (all(confidant[c("TALKFREQ1", "TALKFREQ2", "TALKFREQ3",
                          "TALKFREQ4", "TALKFREQ5", "TALKFREQ6")]) == "(0) have never spoken to each other)"){
        df[i, "bridg_po"] <- 1
        break  # Exit inner loop after setting bridg_po to 1
      }
    }
  }
}



# The first measure: a dichotomous indicator of whether one serves as the sole 
# intermediary between any two alters—that is, none of the respondent’s other 
# network members serve as an alternative intermediary between the unconnected 
# pair.

# create a data frame with ID and bridging potential (default: 0)
df <- data.frame(ID = unique(core_network$ID), bridg_po = 0)

# for the i-th alter, if it has no connection with at least one alter among 
# 1~(i-1)th alters, then the bridg_po is 1.

# helper function for calculating bridg_po

calc_bridg_po <- function(confidants){
  N <- nrow(confidants)
  if (N == 1){
    return(FALSE)
  }

  for(i in 2:N) {
    confidant <- confidants[i, ]
    
    if (any(!is.na(confidant[c("TALKFREQ1", "TALKFREQ2", "TALKFREQ3",
                               "TALKFREQ4", "TALKFREQ5", "TALKFREQ6")]) &
            confidant[c("TALKFREQ1", "TALKFREQ2", "TALKFREQ3",
                        "TALKFREQ4", "TALKFREQ5", "TALKFREQ6")] == "(0) have never spoken to each other")){
      return(TRUE)
    }
  }
  return(FALSE)
}

for (i in 1:nrow(df)) {
  line_id <- df$ID[i]  # Access each 'id' value from the dataframe df
  # Check if line_id is not NA and exists in the core_network dataframe
  if (any(core_network$ID == line_id) && any(core_network$ID == line_id)) {
    confidants <- core_network %>% filter(ID == line_id)

    
    if (calc_bridg_po(confidants) == TRUE) {
      df[i, "bridg_po"] <- 1
    }
  }
}

write_csv(df, "bridging_po.csv")


# -------------------- Bonding ties --------------------------- #
core_size <- core_network %>%
  group_by(ID) %>%
  count()

df <- full_join(df, core_size, by = "ID")
df <- rename(df, bond = n)



# --------------------- Mediator: Social Support ---------------------- #
mediator <- covid_data %>% select(ID, TASKHELP_1, TASKHELP_2, SUPPORT_1, SUPPORT_2) %>%
  rename(tang_s = TASKHELP_1, tang_s_change = TASKHELP_2, emo_s = SUPPORT_1, emo_s_change = SUPPORT_2)

df <- full_join(df, mediator, by = "ID")


# --------------------- Outcome variables: health outcome ---------------------- #

outcome <- covid_data %>% select(ID, PHYSHLTH, PHYSHLTH_C19, MNTLHLTH4, MNTLHLTH_C19) %>%
  rename(phy_h = PHYSHLTH, phy_h_change = PHYSHLTH_C19, men_h = MNTLHLTH4, men_h_change = MNTLHLTH_C19)

df <- full_join(df, outcome, by = "ID")


# --------------------- Control variables ---------------------- #

control <- core_data %>% select(ID, GENDER, AGE, DEGREE_RECODE, EDUC, MARITLST, RACE_RECODE, HISPANIC, ETHGRP, JOBSTAT_1, JOBSTAT_2, JOBSTAT_3, JOBSTAT_4, JOBSTAT_5, JOBSTAT_6, HEARN_RECODE, IML50K, IML25K, IML100K, INCOME_2)

df <- full_join(df, control, by = "ID")

write_csv(df, "data.csv")



# ----------------------------------------------------------------- #
# --------------------- DATA CLEANING ----------------------------- #

# import the data
df <- read_csv("data.csv")
df <- df %>% 
  filter(!is.na(tang_s)) %>%
  filter(!is.na(tang_s_change)) %>%
  filter(!is.na(emo_s)) %>%
  filter(!is.na(emo_s_change))


# replace the value of every variable with dichotomous or categorical values

df$tang_s_di <- ifelse(df$tang_s=="(1) Yes", 1, 0) # receive or not receive
df$tang_s_lack <- ifelse(df$tang_s=="(1) Yes" | df$tang_s=="(2) No, have not needed help", 0, 1)

df$emo_s_di <- ifelse(df$emo_s=="(1) Yes", 1, 0) # receive or not receive
df$emo_s_lack <- ifelse(df$emo_s=="(1) Yes" | df$emo_s=="(2) No, have not needed help", 0, 1)

df$tang_s_change_cate <- ifelse(df$tang_s_change=="(1) Less often", 1, NA)
df$tang_s_change_cate <- ifelse(df$tang_s_change=="(2) About the same", 2, df$tang_s_change_cate)
df$tang_s_change_cate <- ifelse(df$tang_s_change=="(3) More often", 3, df$tang_s_change_cate)

df$emo_s_change_cate <- ifelse(df$emo_s_change=="(1) Less often", 1, NA)
df$emo_s_change_cate <- ifelse(df$emo_s_change=="(2) About the same", 2, df$emo_s_change_cate)
df$emo_s_change_cate <- ifelse(df$emo_s_change=="(3) More often", 3, df$emo_s_change_cate)

df$phy_h_cate <- ifelse(df$phy_h=="(1) poor", 1, NA)
df$phy_h_cate <- ifelse(df$phy_h=="(2) fair", 2, df$phy_h_cate)
df$phy_h_cate <- ifelse(df$phy_h=="(3) good", 3, df$phy_h_cate)
df$phy_h_cate <- ifelse(df$phy_h=="(4) very good", 4, df$phy_h_cate)
df$phy_h_cate <- ifelse(df$phy_h=="(5) excellent", 5, df$phy_h_cate)

df$men_h_cate <- ifelse(df$men_h=="(1) poor", 1, NA)
df$men_h_cate <- ifelse(df$men_h=="(2) fair", 2, df$men_h_cate)
df$men_h_cate <- ifelse(df$men_h=="(3) good", 3, df$men_h_cate)
df$men_h_cate <- ifelse(df$men_h=="(4) very good", 4, df$men_h_cate)
df$men_h_cate <- ifelse(df$men_h=="(5) excellent", 5, df$men_h_cate)

df$phy_h_change_cate <- ifelse(df$phy_h_change=="(1) Worse", 1, NA)
df$phy_h_change_cate <- ifelse(df$phy_h_change=="(2) About the same", 2, df$phy_h_change_cate)
df$phy_h_change_cate <- ifelse(df$phy_h_change=="(3) Better", 3, df$phy_h_change_cate)

df$men_h_change_cate <- ifelse(df$men_h_change=="(1) Worse", 1, NA)
df$men_h_change_cate <- ifelse(df$men_h_change=="(2) About the same", 2, df$men_h_change_cate)
df$men_h_change_cate <- ifelse(df$men_h_change=="(3) Better", 3, df$men_h_change_cate)

df$gender <- ifelse(df$GENDER=="(1) male", 0, NA)
df$gender <- ifelse(df$GENDER=="(2) female", 1, df$gender)

df$degree <- ifelse(df$EDUC=="(1) < hs", 1, NA)
df$degree <- ifelse(df$EDUC=="(2) hs/equiv", 2, df$degree)
df$degree <- ifelse(df$EDUC=="(3) voc cert/some college/assoc", 3, df$degree)
df$degree <- ifelse(df$EDUC=="(4) bachelors or more", 4, df$degree)

df$work <- ifelse(df$JOBSTAT_1=="(1) yes", 1, 0)
df$retired <- ifelse(df$JOBSTAT_2=="(1) yes", 1, 0)
df$disabled <- ifelse(df$JOBSTAT_3=="(1) yes", 1, 0)
df$unemployed <- ifelse(df$JOBSTAT_4=="(1) yes", 1, 0)
df$homemaker <- ifelse(df$JOBSTAT_5=="(1) yes", 1, 0)

df$income <- ifelse(df$HEARN_RECODE=="(1) 0-24,999", 1, NA)
df$income <- ifelse(df$HEARN_RECODE=="(2) 25,000-49,999", 2, df$income)
df$income <- ifelse(df$HEARN_RECODE=="(3) 50,000-99,999", 3, df$income)
df$income <- ifelse(df$HEARN_RECODE=="(4) 100k or higher", 4, df$income)

df$income2 <- ifelse(df$INCOME_2=="(1) far below average", 1, NA)
df$income2 <- ifelse(df$INCOME_2=="(2) below average", 2, df$income2)
df$income2 <- ifelse(df$INCOME_2=="(3) average", 3, df$income2)
df$income2 <- ifelse(df$INCOME_2=="(4) above average", 4, df$income2)
df$income2 <- ifelse(df$INCOME_2=="(5) far above average", 5, df$income2)

df$marriage <- ifelse(df$MARITLST=="(1) married", 1, 0)

df$race <- ifelse(df$RACE_RECODE=="(1) white/caucasian", 1, NA)
df$race <- ifelse(df$RACE_RECODE=="(2) black/african american", 2, df$race)
df$race <- ifelse(df$RACE_RECODE=="(3) asian, pacific islander, american indian or alaskan native", 3, df$race)

write_csv(df, "data_cleaned.csv")





















