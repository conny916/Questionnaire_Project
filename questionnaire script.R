## LOAD PACKAGES ####
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(readxl)
library(zoo)
library(data.table)


## LOAD IN DATA ####
#Combine csv files
filescsv <- list.files(path = "data/", pattern = "questionnaires_.*csv", full.names = TRUE)
filescsv
df_list <- lapply(filescsv, read.csv, header = TRUE, sep = "\t", fileEncoding = "utf16")
all_data <- rbindlist(df_list, fill = TRUE) 
head(data)

#Delete duplicates
nrow(all_data)
data_trimmed <- all_data %>%
  distinct(CASE, .keep_all = TRUE)
nrow(data_trimmed)
write.csv(data_trimmed, "data_trimmed.csv")

## BDI Update ####
# Recode BDI
BDI_update <- data_trimmed %>%
  mutate_at(vars(BD02, BD03, BD04, BD05, BD06, BD07, BD08, BD09, BD10,
                 BD11, BD12, BD13, BD14, BD15, BD16, BD18, BD20, BD21, BD22), 
            ~ recode(., "1"=0,"2"=1,"3"=2,"4"=3, .default = NaN)) %>%
  mutate_at(vars(BD17, BD19), ~ recode(., "1"=0,"2"=1,"3"=1,"4"=2,"5"=2,"6"=3,"7"=3, .default = NaN))

#Sum of BDI
BDI_update$BDI_sum <- BDI_update$BD02 + BDI_update$BD03 + BDI_update$BD04 + BDI_update$BD05 + BDI_update$BD06 +
                      BDI_update$BD07 + BDI_update$BD08 + BDI_update$BD09 + BDI_update$BD10 + BDI_update$BD11 +
                      BDI_update$BD12 + BDI_update$BD13 + BDI_update$BD14 + BDI_update$BD15 + BDI_update$BD16 +
                      BDI_update$BD17 + BDI_update$BD18 + BDI_update$BD19 + BDI_update$BD20 + BDI_update$BD21 +
                      BDI_update$BD22
write.csv(BDI_update, "BDI_update.csv")

## TICS Update ####
# Recode TICS
TICS_update <- BDI_update %>%
  mutate_at(vars(TI02_01, TI02_02, TI02_03, TI02_04, TI02_05, TI02_06, TI02_07, TI02_08, TI02_09, TI02_10, TI02_11,
                 TI02_12, TI02_13, TI02_14, TI02_15, TI02_16, TI02_17, TI02_18, TI02_19, TI02_20, TI03_01, TI03_02,
                 TI03_03, TI03_04, TI03_05, TI03_06, TI03_07, TI03_08, TI03_09, TI03_10, TI03_11, TI03_12, TI03_13,
                 TI03_14, TI03_15, TI04_01, TI04_02, TI04_03, TI04_04, TI04_05, TI04_06, TI04_07, TI04_08, TI04_09,
                 TI04_10, TI04_11, TI04_12, TI04_13, TI04_14, TI04_15, TI04_16, TI04_17, TI04_18, TI04_19, TI04_20,
                 TI04_21, TI04_22), ~ recode(., "1"=0,"2"=1,"3"=2,"4"=3,"5"=4, .default = NaN))
#TICS dimesions
TICS_update$TICS_UEBE <- TICS_update$TI02_01 + TICS_update$TI02_04 + TICS_update$TI02_17 + TICS_update$TI03_07 + 
                    TICS_update$TI04_03 + TICS_update$TI04_09 + TICS_update$TI04_15 + TICS_update$TI02_19 

TICS_update$TICS_SOUE <- TICS_update$TI02_07 + TICS_update$TI02_19 + TICS_update$TI03_08 + TICS_update$TI04_04 + 
                    TICS_update$TI04_14 + TICS_update$TI04_22 

TICS_update$TICS_ERDR <- TICS_update$TI02_08 + TICS_update$TI02_12 + TICS_update$TI02_14 + TICS_update$TI03_02 + 
                    TICS_update$TI03_03 + TICS_update$TI03_10 + TICS_update$TI03_12 + TICS_update$TI04_05 +
                    TICS_update$TI04_08 

TICS_update$TICS_UNZU <- TICS_update$TI02_05 + TICS_update$TI02_10 + TICS_update$TI02_13 + TICS_update$TI03_01 +
                    TICS_update$TI04_02 + TICS_update$TI04_06 + TICS_update$TI04_13 + TICS_update$TI04_18 

TICS_update$TICS_UEFO <- TICS_update$TI02_03 + TICS_update$TI02_20 + TICS_update$TI03_04 + TICS_update$TI03_15 +
                    TICS_update$TI04_12 + TICS_update$TI04_20 

TICS_update$TICS_MANG <- TICS_update$TI02_02 + TICS_update$TI02_18 + TICS_update$TI03_11 + TICS_update$TI04_11 

TICS_update$TICS_SOZS <- TICS_update$TI02_06 + TICS_update$TI02_15 + TICS_update$TI03_06 + TICS_update$TI03_13 +
                    TICS_update$TI04_10 + TICS_update$TI04_17 

TICS_update$TICS_SOZI <- TICS_update$TI02_11 + TICS_update$TI03_09 + TICS_update$TI03_14 + TICS_update$TI04_07 +
                    TICS_update$TI04_16 + TICS_update$TI04_21

TICS_update$TICS_SORG <- TICS_update$TI02_09 + TICS_update$TI02_16 + TICS_update$TI03_05 + TICS_update$TI04_01 

TICS_update$TICS_SSCS <- TICS_update$TI02_09 + TICS_update$TI02_16 + TICS_update$TI02_18 + TICS_update$TI03_05 +
                    TICS_update$TI03_11 + TICS_update$TI03_15 + TICS_update$TI04_01 + TICS_update$TI04_03 +
                    TICS_update$TI04_09 + TICS_update$TI04_12 + TICS_update$TI04_19 + TICS_update$TI04_22 
write.csv(TICS_update, "TICS_update.csv")                     

## PQSI Update ####
#Component 1
PSQI_update <- TICS_update %>%
  mutate_at(vars(PS10_01), ~ recode(., "1"=0,"2"=1,"3"=2,"4"=3, .default = NaN))
PSQI_update$PSQI_Comp_1 <- PSQI_update$PS10_01

#Antworten vor dem 17.05.21 müssen im Datenset noch umgeschrieben werden.
#Folgener Code ist bei Variable PS03_01 anzuwenden: ≤15 min = 1 ; 16 - 30 min = 2 ;  31 - 59 min = 3 ; >60 min = 4
#Component 2
PSQI_update <- PSQI_update %>%
  mutate_at(vars(PS03_01, PS07_01), ~recode(., "1"=0,"2"=1,"3"=2,"4"=3, .default = NaN))
PSQI_update$PSQI_Comp_2 <- PSQI_update$PS03_01 + PSQI_update$PS07_01 
PSQI_update <- PSQI_update %>%
mutate_at(vars(PSQI_Comp_2), ~recode(., "0"=0,"1"=1,"2"=1,"3"=2,"4"=2,"5"=3,"6"=3))

#Hier muss vorher noch das Datenset angepasst werden und die Antworten auf 0.5 Stunden genau angegeben werden (PS05_01).
#WICHTIG: Angaben bitte nur in 0.5 Schritten, mit einem Punkt statt Komma und nur die Zahl selber (e.g 7 oder 7.5)
#Component 3
PSQI_update$PSQI_Comp_3 <- PSQI_update$PS05_01
PSQI_update <- PSQI_update %>%
  mutate_at(vars(PSQI_Comp_3), ~recode(., "0"=3,"0.5"=3,"1"=3,"1.5"=3,"2"=3,"2.5"=3,"3"=3,"3.5"=3,"4"=3,"4.5"=3,
                                          "5"=2,"5.5"=2,"6"=1,"6.5"=1,"7"=0,"7.5"=0,"8"=0,"8.5"=0,"9"=0,"9.5"=0,
                                          "10"=0,"10.5"=0,"11"=0,"11.5"=0,"12"=0,"12.5"=0,"13"=0,"13.5"=0,"14"=0,
                                          "14.5"=0,"15"=0, .default = NaN))
  
#Component 4
#?

#Component 5 
PSQI_update$PS09_01 <- ifelse(PSQI_update$QUESTNNR == 'PRE_TAG_1' & is.na(PSQI_update$PS09_01), 1, PSQI_update$PS09_01)
PSQI_update <- PSQI_update %>%
  mutate_at(vars(PS07_02, PS07_03, PS07_04, PS07_05, PS07_06, PS07_07, PS07_08, PS07_09, PS09_01),
            ~recode(., "1"=0,"2"=1,"3"=2,"4"=3,))
PSQI_update$PSQI_Comp_5 <- rowSums(PSQI_update[,c("PS07_02", "PS07_03", "PS07_04",
                                             "PS07_05", "PS07_06", "PS07_07",
                                             "PS07_08", "PS07_09", "PS09_01")])
PSQI_update <- PSQI_update %>%
mutate_at(vars(PSQI_Comp_5), ~recode(., "0"=0,"1"=1,"2"=1,"3"=1,"4"=1,"5"=1,"6"=1,"7"=1,"8"=1,"9"=1,
                                   "10"=2,"11"=2,"12"=2,"13"=2,"14"=2,"15"=2,"16"=2,"17"=2,"18"=2,
                                   "19"=3,"20"=3,"21"=3,"22"=3,"23"=3,"24"=3,"25"=3,"26"=3,"27"=3, .default = NaN))

#Component 6
PSQI_update <- PSQI_update %>%
  mutate_at(vars(PS11_01), ~recode(., "1"=0,"2"=1,"3"=2,"4"=3, .default = NaN))
PSQI_update$PSQI_Comp_6 <- PSQI_update$PS11_01

#Component 7
PSQI_update <- PSQI_update %>%
  mutate_at(vars(PS12_01, PS13_01), ~recode(., "1"=0,"2"=1,"3"=2,"4"=3, .default = NaN))
PSQI_update$PSQI_Comp_7 <- PSQI_update$PS12_01 + PSQI_update$PS13_01
PSQI_update <- PSQI_update %>%
mutate_at(vars(PSQI_Comp_7), ~recode(., "0"=0,"1"=1,"2"=1,"3"=2,"4"=2,"5"=3,"6"=3))

#Sum of PSQI
#PSQI_update$PSQI_sum <- PSQI_update$Comp_1 + PSQI_update$Comp_2 + PSQI_update$Comp_3 + PSQI_update$Comp_4
#                        PSQI_update$Comp_5 + PSQI_update$Comp_6 + PSQI_update$Comp_7
write.csv(PSQI_update, "PSQI_update.csv")

## STAI Update ####
PSQI_update$STAI_StateAngst <- 5 - PSQI_update$ST02_01 + 5 - PSQI_update$ST02_02 + PSQI_update$ST02_03 + PSQI_update$ST02_04 +
                           5 - PSQI_update$ST02_05 + PSQI_update$ST02_06 + PSQI_update$ST02_07 + 5 - PSQI_update$ST02_08 +
                           PSQI_update$ST02_09 + 5 - PSQI_update$ST02_10 + 5 - PSQI_update$ST02_11 + PSQI_update$ST02_12 +
                           PSQI_update$ST02_13 + PSQI_update$ST02_14 + 5 - PSQI_update$ST02_15 + 5 - PSQI_update$ST02_16 +
                           PSQI_update$ST02_17 + PSQI_update$ST02_18 + 5 - PSQI_update$ST02_19 + 5 - PSQI_update$ST02_20
                           
PSQI_update$STAI_TraitAngst <- 5 - PSQI_update$ST04_01 + PSQI_update$ST04_02 + PSQI_update$ST04_03 + PSQI_update$ST04_04 +                            
                           PSQI_update$ST04_05 + 5 - PSQI_update$ST04_06 + 5 - PSQI_update$ST04_07 + PSQI_update$ST04_08 + 
                           PSQI_update$ST04_09 + 5 - PSQI_update$ST04_10 + PSQI_update$ST04_11 + PSQI_update$ST04_12 + 
                           5 - PSQI_update$ST04_13 + PSQI_update$ST04_14 + PSQI_update$ST04_15 + 5 - PSQI_update$ST04_16 + 
                           PSQI_update$ST04_17 + PSQI_update$ST04_18 + 5 - PSQI_update$ST04_19 + PSQI_update$ST04_20
write.csv(PSQI_update, "STAI_update.csv")                       

## Explicit knowledge ####
PSQI_update$Schlafh <- PSQI_update$EK02_01
PSQI_update$Schlaf_Qualität <- PSQI_update$EK03
PSQI_update$Idee_Aufgabe <- PSQI_update$EK04
PSQI_update$Überraschung <- PSQI_update$EK06
write.csv(PSQI_update, "EK_update.csv")       

## Drop columns ####
New_columns <- PSQI_update %>%
  select(-CASE, -QUESTNNR, -REF, -MODE, -STARTED, -FINISHED, -Q_VIEWER, -MAXPAGE, -MISSING , -MISSREL , -TIME_RSI, -DEG_TIME, -LASTPAGE)

## Add VP_ID + Merging rows ####
#Adding VP_ID Variable
VP_ID <- read_excel(path = "data/VP_IDs + Seriennummern.xlsx")
names(VP_ID)[names(VP_ID) == "Seriennummer"] <- "SERIAL"
VP_ID <- VP_ID %>%
  select(-...4, -...5, -...6, -Condition)
head(VP_ID)
VP_ID <- VP_ID %>%
  inner_join(New_columns, VP_ID, by = "SERIAL")
write.csv(VP_ID, "VP_ID.csv")

#Merging rows
VP_ID <- VP_ID %>%
  group_by(VP) %>%
  mutate_each(funs(na.locf(., na.rm = FALSE, fromLast = FALSE))) %>%
  filter(row_number()==n())
final_set <- VP_ID %>%
  select(-SERIAL, -PS03, -PS03_02, -PS03_03, -PS03_04)
write.csv(final_set, "final_data.csv")





