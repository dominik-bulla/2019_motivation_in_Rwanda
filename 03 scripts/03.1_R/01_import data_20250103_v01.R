### environment ----------------------- ----------------------- ----------------------- -----------------------
setwd("C:/Users/domin/GitHub/2019_motivation_in_Rwanda")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



### Packages ----------------------- ----------------------- ----------------------- -----------------------

library(readxl)
library(dplyr)
library(psych)



### Load data ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- read_excel("01 raw data/motivation in Rwanda_data_motivation and confidence data_20250103_V01.xlsx")
motivation_confidence <- motivation_confidence %>%
  rename("type" = `DATA TYPE`, 
         "questionnaire" = `TYPE`,
         "school" = `SCHOOL`,
         "sno_school" = `SNO`,
         "sno_statement" = `SNO_IN`,
         "sno_student" = `# STUDENT`,
         "sno" = `COMBINED \r\nSNO`, 
         "statement_first" = `UNIQUE \r\n(STUDENT)`,
         "statement_multiple" = `UNIQUE \r\n(STATEMENT)`, 
         "statement_coded_multiple" = `MULTIPLE CODING`, 
         "name" = `NAME`,
         "age" = `AGE`, 
         "gender" = `GENDER`,
         "school_grade" = `LEVEL`, 
         "consent_given" = `CONSENT \r\nFORM`, 
         "what subjects listed" = `Subject`, 
         "what follow-up statenents given" = `FOLLOW-UP\r\nSTATEMENT`,
         "coding_round_1_coder_1" = `MAKES SENSE FINAL \r\n[ROUND 1; CODER 1]`, 
         "coding_round_1_coder_2" = `MAKES SENSE FINAL \r\n[ROUND 1; CODER 2]`, 
         "coding_round_1_final_coding" = `MAKES SENSE FINAL \r\n[ROUND 1; FINAL]`, 
         "coding_round_1_coder_agreement" = `MAKES SENSE FINAL \r\n[ROUND 1; COMMENT]`, 
         "coding_round_2_coder_1" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 2; CODER 1]`, 
         "coding_round_2_coder_2" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 2; CODER 2]`, 
         "coding_round_2_final_coding" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 2; FINAL]`, 
         "coding_round_2_coder_agreement" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 2; COMMENT]`, 
         "coding_round_3_coder_1_answer_1" = `CATEGORY \r\n[ROUND 3; CODER 1.1]`, 
         "coding_round_3_coder_1_answer_2" = `CATEGORY \r\n[ROUND 3; CODER 1.2]`, 
         "coding_round_3_coder_1_answer_3" = `CATEGORY \r\n[ROUND 3; CODER 1.3]`, 
         "coding_round_3_coder_1_answer_4" = `CATEGORY \r\n[ROUND 3; CODER 1.4]`, 
         "coding_round_3_coder_2_answer_1" = `CATEGORY \r\n[ROUND 3; CODER 2.1]`, 
         "coding_round_3_coder_2_answer_2" = `CATEGORY \r\n[ROUND 3; CODER 2.2]`, 
         "coding_round_3_coder_2_answer_3" = `CATEGORY \r\n[ROUND 3; CODER 2.3]`, 
         "coding_round_3_coder_2_answer_4" = `CATEGORY \r\n[ROUND 3; CODER 2.4]`, 
         "coding_round_3_coder_agreement" = `CATEGORY \r\n[ROUND 3; COMMENT]`, 
         "coding_round_3_final_coding_overall" = `CATEGORY \r\n[ROUND 3; FINAL]`, 
         "coding_round_3_final_coding_separate" = `CATEGORY \r\n[ROUND 3; FINAL (INDIVIDUAL)]`, 
         "coding_round_4.1_coder_1_answer_1" = `CATEGORY \r\n[ROUND 4.1; CODER 1.1]`, 
         "coding_round_4.1_coder_1_answer_2" = `CATEGORY \r\n[ROUND 4.1; CODER 1.2]`,
         "coding_round_4.1_coder_1_answer_3" = `CATEGORY \r\n[ROUND 4.1; CODER 1.3]`, 
         "coding_round_4.1_coder_2_answer_1" = `CATEGORY \r\n[ROUND 4.1; CODER 2.1]`, 
         "coding_round_4.1_coder_2_answer_2" = `CATEGORY \r\n[ROUND 4.1; CODER 2.2]`, 
         "coding_round_4.1_coder_2_answer_3" = `CATEGORY \r\n[ROUND 4.1; CODER 2.3]`, 
         "coding_round_4.1_coder_agreement" = `CATEGORY \r\n[ROUND 4.1; COMMENT]`, 
         "coding_round_4.1_final_coding_overall" = `CATEGORY \r\n[ROUND 4.1; FINAL]`, 
         "coding_round_4.1_final_coding_separate" = `CATEGORY \r\n[ROUND 4.1; FINAL (INDIVIDUAL)]`, 
         "coding_round_4.2_coder_1_answer_1" = `CATEGORY \r\n[ROUND 4.2; CODER 1.1]`, 
         "coding_round_4.2_coder_1_answer_2" = `CATEGORY \r\n[ROUND 4.2; CODER 1.2]`, 
         "coding_round_4.2_coder_2_answer_1" = `CATEGORY \r\n[ROUND 4.2; CODER 2.1]`, 
         "coding_round_4.2_coder_2_answer_2" = `CATEGORY \r\n[ROUND 4.2; CODER 2.2]`, 
         "coding_round_4.2_coder_agreement" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 4.2; COMMENT]`, 
         "coding_round_4.2_final_coding_overall" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 4.2; FINAL]`, 
         "coding_round_4.2_final_coding_separate" = `CATEGORY_FINAL`, 
         "comment" = `FINAL COMMENT`, 
         "additional comment" = `COMMENT`, 
         "statement_Kinyarwanda" = `ANSWER (Kinyarwanda)`, 
         "statement_English" = `ANSWER (English)`,
         "statement_stem" = `STEM`, 
         "statement_in_first_person" = `FIRST PERSON`, 
         "auxiliary_verb_used" = `AUXILARY`, 
         "verb_used" = `VERB`, 
         "final_verb" = `FINAL VERB`, 
         "verb_negated" = `Negative`, 
         "adverb_used" = `Adverb`, 
         "object_1_used" = `Object`, 
         "object_2_used" = `Object2`, 
         "object_3_used" = `Object3`, 
         "delete" = `...67`) %>%
  select(-c(final_verb, delete)) %>%
  mutate(type = tolower(coding_round_2_final_coding), 
         questionnaire = tolower(questionnaire),
         coding_round_2_final_coding = tolower(coding_round_2_final_coding)) %>%
  filter(type != "no")



### Anonymize data set ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  select(-c(name)) %>%
  mutate(school = ifelse(school == "Maranyundo", "A", "B"))



### Create data set for 2019 mixed-method study on "Achievement motivation amongst Rwandan students" ----------------------- ----------------------- ----------------------- -----------------------

motivation <- motivation_confidence %>%
  filter(questionnaire == "demotivation" | questionnaire == "motivation") %>%
  select(-c(statement_Kinyarwanda, statement_English, statement_stem, statement_in_first_person, 
            auxiliary_verb_used, verb_used, verb_negated, adverb_used, 
            object_1_used, object_2_used, object_3_used))



### Save data ----------------------- ----------------------- ----------------------- -----------------------

write.csv(motivation, "02 processed data/motivation_in_Rwanda_20250103_v01.csv")




