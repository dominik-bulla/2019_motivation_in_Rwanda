### environment ----------------------- ----------------------- ----------------------- -----------------------

setwd("C:/Users/domin/GitHub/2019_motivation_in_Rwanda")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



### Packages ----------------------- ----------------------- ----------------------- -----------------------

library(readxl)
library(dplyr)
library(psych)



### import data ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- read_excel("01 raw data/motivation in Rwanda_data_motivation and confidence data_20250103_V01.xlsx")
schoolmarks <- read.csv("01 raw data/school marks_20220213.csv")                          

### we manually code the subjects students listed as related to (a)motivation. See column 'what_subjects_listed'
subjectlist <- read.csv("01 raw data/subject coding_20220213_Version04.csv")



### clean data ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  rename("questionnaire_classification_final" = `DATA TYPE`, 
         "questionnaire_type_initial" = `TYPE`,
         "school" = `SCHOOL`,
         "sno_questionnaire" = `SNO`,
         "sno_statement" = `SNO_IN`,
         "sno_student" = `# STUDENT`,
         "sno" = `COMBINED \r\nSNO`, 
         "statement_first" = `UNIQUE \r\n(STUDENT)`,
         "statement_unique" = `UNIQUE \r\n(STATEMENT)`, 
         "statement_coded_multiple" = `MULTIPLE CODING`, 
         "name" = `NAME`,
         "age" = `AGE`, 
         "gender" = `GENDER`,
         "school_grade" = `LEVEL`, 
         "consent_given" = `CONSENT \r\nFORM`, 
         "what_subjects_listed" = `Subject`, 
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
         questionnaire_classification_final = tolower(questionnaire_classification_final),
         questionnaire_type_initial = tolower(questionnaire_type_initial),
         coding_round_1_final_coding = tolower(coding_round_1_final_coding),
         coding_round_2_final_coding = tolower(coding_round_2_final_coding),
         statement_first = tolower(statement_first),
         statement_unique = tolower(statement_unique),
         statement_coded_multiple = tolower(statement_coded_multiple)) 



### clean snos ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  mutate(sno_statement = paste0(substr(school, 1, 1), substr(questionnaire_classification_final, 1, 1), sno_statement)) %>%
  group_by(sno_statement) %>%
  mutate(sno_segment = paste0(sno_statement, "_", row_number())) %>%
  ungroup() %>%
  select(questionnaire_classification_final, questionnaire_type_initial, school, sno_questionnaire,
         sno_student, sno_statement, sno_segment, everything())



### clean remaining data ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  mutate(school_grade = ifelse(grepl("1", school_grade),"1", school_grade)) %>%
  mutate(school_grade = ifelse(grepl("2", school_grade),"2", school_grade)) %>%
  mutate(school_grade = ifelse(grepl("3", school_grade),"3", school_grade)) %>%
  mutate(school_grade = ifelse(school_grade == "?",NA, school_grade)) 

motivation_confidence <- motivation_confidence %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "OPPORTUNITY","COSTS/OPPORTUNITY", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "OUTSIDE","COSTS/OUTSIDE", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "NEGATIVE","COSTS/EMOTIONAL", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "TASK DIFFICULTY","COSTS/EFFORT", coding_round_4.2_final_coding_separate))

motivation_confidence <- motivation_confidence %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "POSITIVE","VALUE/POSITIVE", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "UTILITY (DAILY LIFE)", "VALUE/UTILITY/DAILY LIFE", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "UTILITY (GENERAL)", "VALUE/UTILITY/GENERAL", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "UTILITY (LEARNING UTILITY)", "VALUE/UTILITY/LEARNING UTILITY", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "UTILITY (OTHER)", "VALUE/UTILITY/OTHER", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "UTILITY (SCHOOL UTILITY)", "VALUE/UTILITY/SCHOOL UTILITY", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "UTILITY (SOCIAL UTILITY)", "VALUE/UTILITY/SOCIAL UTILITY", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "UTILITY (USELESS)", "VALUE/UTILITY/USELESS", coding_round_4.2_final_coding_separate)) 

motivation_confidence <- motivation_confidence %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (EVALUATIVE)","GOALS/evaluative", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (FAMILY SUPPORT)","GOALS/family support", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (MASTERY)","GOALS/mastery", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (NORMATIVE)","GOALS/normative", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (OUTCOME)","GOALS/outcome", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (SOCIAL APPROVAL)","GOALS/social approval", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (SOCIAL CONCERN)","GOALS/social concern", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (SOCIAL RESPONSIBILITY)","GOALS/social responsibility", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(coding_round_4.2_final_coding_separate == "GOALS (SOCIAL STATUS)","GOALS/social status", coding_round_4.2_final_coding_separate)) %>%
  mutate(coding_round_4.2_final_coding_separate = tolower(coding_round_4.2_final_coding_separate))



### change ability into expectancies ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  mutate(coding_round_4.2_final_coding_separate = ifelse(grepl("goals", coding_round_4.2_final_coding_separate), "goals", coding_round_4.2_final_coding_separate))



### change ability into expectancies ----------------------- ----------------------- ----------------------- -----------------------

columns <- colnames(motivation_confidence)
columns <- columns[grep("coding", columns)]
for (col in 1 : length(columns)) {
  print(col)
  motivation_confidence[, col] <- gsub("ABILITIES", " EXPECTANCY", motivation_confidence[, col])
  motivation_confidence[, col] <- gsub("ABILITY", " EXPECTANCY", motivation_confidence[, col])
}



### merge with school marks and subject listing ----------------------- ----------------------- ----------------------- -----------------------

schoolmarks <- schoolmarks %>%
  rename(`sno_student` = Student) %>%
  select(-c(NAME))
colnames(schoolmarks) <- tolower(colnames(schoolmarks))

subjectlist <- subjectlist %>%
  rename(`sno_student` = Student) %>%
  select(-c(Name, Age, Gender, Grade, School))
colnames(subjectlist) <- tolower(colnames(subjectlist))

motivation_confidence <- merge(motivation_confidence, subjectlist, by = "sno_student")
motivation_confidence <- merge(motivation_confidence, schoolmarks, by = "sno_student")
rm(schoolmarks, subjectlist)

motivation_confidence <- motivation_confidence %>%
  arrange(sno_student)



### anonymise data set ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  select(-c(name)) %>%
  mutate(school = ifelse(school == "Maranyundo", "B", "A"))



### create data set for 2019 mixed-method study on "Achievement motivation amongst Rwandan students" ----------------------- ----------------------- ----------------------- -----------------------

write.csv(motivation, "aaa.csv")


motivation <- motivation_confidence %>%
  filter(questionnaire_classification_final == "demotivation" | questionnaire_classification_final == "motivation") %>%
  select(-c(statement_Kinyarwanda, statement_English, statement_stem, statement_in_first_person, 
            auxiliary_verb_used, verb_used, verb_negated, adverb_used, 
            object_1_used, object_2_used, object_3_used))



### Save data ----------------------- ----------------------- ----------------------- -----------------------

write.csv(motivation, "02 processed data/motivation_in_Rwanda_20250115_v01.csv", row.names = FALSE)




