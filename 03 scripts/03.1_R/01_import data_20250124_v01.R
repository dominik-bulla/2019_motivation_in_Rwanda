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

motivation_confidence <- read_excel("01 raw data/motivation in Rwanda_raw data_20250127_v01.xlsx")
schoolmarks <- read.csv("01 raw data/school marks_20220213.csv")                          

### we manually code the subjects students listed as related to (a)motivation. See column 'what_subjects_listed'
subjectlist <- read.csv("01 raw data/subject coding_20220213_Version04.csv")



### clean data ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  select(-c(`FINAL VERB`, `...65`, SNO, `SNO_IN`, `COMBINED \r\nSNO`)) %>%
  rename("sno" = key_sno,
         "data_type" = `DATA TYPE`, 
         "questionnaire_type" = `TYPE`,
         "school" = `SCHOOL`,
         "sno_student" = `# STUDENT`,
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
         "coding_round_4.1_coder_1_answer_1" = `CATEGORY \r\n[ROUND 4.1; CODER 1.1]`, 
         "coding_round_4.1_coder_1_answer_2" = `CATEGORY \r\n[ROUND 4.1; CODER 1.2]`,
         "coding_round_4.1_coder_1_answer_3" = `CATEGORY \r\n[ROUND 4.1; CODER 1.3]`, 
         "coding_round_4.1_coder_2_answer_1" = `CATEGORY \r\n[ROUND 4.1; CODER 2.1]`, 
         "coding_round_4.1_coder_2_answer_2" = `CATEGORY \r\n[ROUND 4.1; CODER 2.2]`, 
         "coding_round_4.1_coder_2_answer_3" = `CATEGORY \r\n[ROUND 4.1; CODER 2.3]`, 
         "coding_round_4.1_coder_agreement" = `CATEGORY \r\n[ROUND 4.1; COMMENT]`, 
         "coding_round_4.1_final_coding_overall" = `CATEGORY \r\n[ROUND 4.1; FINAL]`, 
         "coding_round_4.2_coder_1_answer_1" = `CATEGORY \r\n[ROUND 4.2; CODER 1.1]`, 
         "coding_round_4.2_coder_1_answer_2" = `CATEGORY \r\n[ROUND 4.2; CODER 1.2]`, 
         "coding_round_4.2_coder_2_answer_1" = `CATEGORY \r\n[ROUND 4.2; CODER 2.1]`, 
         "coding_round_4.2_coder_2_answer_2" = `CATEGORY \r\n[ROUND 4.2; CODER 2.2]`, 
         "coding_round_4.2_coder_agreement" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 4.2; COMMENT]`, 
         "coding_round_4.2_final_coding_overall" = `CLASSIFICATION OF STATEMENT \r\n[ROUND 4.2; FINAL]`, 
         "final_coding" = `CATEGORY_FINAL`, 
         "statement_Kinyarwanda" = `ANSWER (Kinyarwanda)`, 
         "statement_English" = `ANSWER (English)`,
         "statement_stem" = `STEM`, 
         "statement_in_first_person" = `FIRST PERSON`, 
         "auxiliary_verb_used" = `AUXILARY`, 
         "verb_used" = `VERB`, 
         "verb_negated" = `Negative`, 
         "adverb_used" = `Adverb`, 
         "object_1_used" = `Object`, 
         "object_2_used" = `Object2`, 
         "object_3_used" = `Object3`) %>%
  mutate(type = tolower(coding_round_2_final_coding), 
         questionnaire_type = tolower(questionnaire_type),
         data_type = tolower(data_type),
         coding_round_1_final_coding = tolower(coding_round_1_final_coding),
         coding_round_2_final_coding = tolower(coding_round_2_final_coding),
         statement_first = tolower(statement_first),
         statement_unique = tolower(statement_unique),
         statement_coded_multiple = tolower(statement_coded_multiple)) 



### clean remaining data ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  mutate(school_grade = ifelse(grepl("1", school_grade),"1", school_grade)) %>%
  mutate(school_grade = ifelse(grepl("2", school_grade),"2", school_grade)) %>%
  mutate(school_grade = ifelse(grepl("3", school_grade),"3", school_grade)) %>%
  mutate(school_grade = ifelse(school_grade == "?",NA, school_grade)) 

motivation_confidence <- motivation_confidence %>%
  mutate(final_coding = gsub("\\/ ","-", final_coding))

motivation_confidence <- motivation_confidence %>%
  mutate(final_coding = gsub("ABILITY","EXPECTANCIES", final_coding))

motivation_confidence <- motivation_confidence %>%
  mutate(final_coding = gsub("POSITIVE","VALUE/INTRINSIC", final_coding)) %>%
  mutate(final_coding = gsub("UTILITY \\(DAILY LIFE\\)", "VALUE/UTILITY/DAILY LIFE", final_coding)) %>%
  mutate(final_coding = gsub("UTILITY \\(GENERAL\\)", "VALUE/UTILITY/GENERAL", final_coding)) %>%
  mutate(final_coding = gsub("UTILITY \\(LEARNING UTILITY\\)", "VALUE/UTILITY/LEARNING UTILITY", final_coding)) %>%
  mutate(final_coding = gsub("UTILITY \\(OTHER\\)", "VALUE/UTILITY/OTHER", final_coding)) %>%
  mutate(final_coding = gsub("UTILITY \\(SCHOOL UTILITY\\)", "VALUE/UTILITY/SCHOOL UTILITY", final_coding)) %>%
  mutate(final_coding = gsub("UTILITY \\(SOCIAL UTILITY\\)", "VALUE/UTILITY/SOCIAL UTILITY", final_coding)) %>%
  mutate(final_coding = gsub("UTILITY \\(USELESS\\)", "VALUE/UTILITY/USELESS", final_coding)) 

motivation_confidence <- motivation_confidence %>%
  mutate(final_coding = gsub("OPPORTUNITY","COSTS/OPPORTUNITY", final_coding)) %>%
  mutate(final_coding = gsub("OUTSIDE","COSTS/OUTSIDE", final_coding)) %>%
  mutate(final_coding = gsub("NEGATIVE","COSTS/EMOTIONAL", final_coding)) %>%
  mutate(final_coding = gsub("TASK DIFFICULTY","COSTS/EFFORT", final_coding))

motivation_confidence <- motivation_confidence %>%
  mutate(final_coding = gsub("GOALS \\(EVALUATIVE\\)","GOALS/EVALUATIVE", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(FAMILY SUPPORT\\)","GOALS/FAMILY SUPPORT", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(MASTERY\\)","GOALS/MASTERY", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(NORMATIVE\\)","GOALS/NORMATIVE", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(OUTCOME\\)","GOALS/OUTCOME", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(SOCIAL APPROVAL\\)","GOALS/SOCIAL APPROVAL", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(SOCIAL CONCERN\\)","GOALS/SOCIAL CONCERN", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(SOCIAL RESPONSIBILITY\\)","GOALS/SOCIAL RESPONSIBILITY", final_coding)) %>%
  mutate(final_coding = gsub("GOALS \\(SOCIAL STATUS\\)","GOALS/SOCIAL STATUS", final_coding)) 

motivation_confidence <- motivation_confidence %>%
  mutate(final_coding = gsub("SOURCE","OTHER FACTORS", final_coding))



### make all entries to lower caps ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- as.data.frame(lapply(motivation_confidence, function(x) {
  if (is.character(x)) {
    return(tolower(x))
  } else {
    return(x)
  }
}))



# create separate coding categories --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

categories <- unique(motivation_confidence$final_coding)
categories <- categories[!is.na(categories)]
categories <- unlist(strsplit(categories, "-"))
categories <- unique(categories)
categories <- categories[order(categories)]

for (element in categories) {
  motivation_confidence <- motivation_confidence %>%
    mutate(new = ifelse(grepl(element, final_coding, fixed = TRUE), 1, 0)) 
  colnames(motivation_confidence)[colnames(motivation_confidence) == "new"] <- element
}



# create categories for 'utility', value', 'costs', and 'goals' --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

motivation_confidence <- motivation_confidence %>%
  mutate(`value/utility` = ifelse(coding_round_1_final_coding != "no", 
                                  pmax(`value/utility/daily life`, 
                                       `value/utility/general`,
                                       `value/utility/learning utility`,
                                       `value/utility/other`,
                                       `value/utility/school utility`,
                                       `value/utility/social utility`,
                                       `value/utility/useless`), NA),
         value = ifelse(coding_round_1_final_coding != "no", 
                        pmax(`value/intrinsic`, 
                             `value/utility`), NA),
         costs = ifelse(coding_round_1_final_coding != "no", 
                        pmax(`costs/effort`, 
                             `costs/emotional`,
                             `costs/opportunity`,
                             `costs/outside`), NA),
         goals = ifelse(coding_round_1_final_coding != "no", 
                        pmax(`goals/evaluative`, `goals/family support`, `goals/mastery`,
                             `goals/normative`, `goals/outcome`, `goals/social approval`,
                             `goals/social concern`, `goals/social responsibility`,
                             `goals/social status`), NA)) 



### merge with school marks and subject listing ----------------------- ----------------------- ----------------------- -----------------------

schoolmarks <- schoolmarks %>%
  rename(`sno_student` = Student) %>%
  select(-c(NAME))
colnames(schoolmarks) <- tolower(colnames(schoolmarks))

subjectlist <- subjectlist %>%
  rename(`sno_student` = Student) %>%
  select(-c(Name, Age, Gender, Grade, School))
colnames(subjectlist) <- tolower(colnames(subjectlist))
colnames(subjectlist) <- gsub("_c", "_con", colnames(subjectlist))
colnames(subjectlist) <- gsub("_u", "_uncon", colnames(subjectlist))
colnames(subjectlist) <- gsub("_m", "_mot", colnames(subjectlist))
colnames(subjectlist) <- gsub("_d", "_demot", colnames(subjectlist))

motivation_confidence <- merge(motivation_confidence, subjectlist, by = "sno_student")
motivation_confidence <- merge(motivation_confidence, schoolmarks, by = "sno_student")
rm(schoolmarks, subjectlist)

motivation_confidence <- motivation_confidence %>%
  arrange(sno_student)



### anonymise data set ----------------------- ----------------------- ----------------------- -----------------------

motivation_confidence <- motivation_confidence %>%
  select(-c(name)) %>%
  mutate(school = ifelse(school == "maranyundo", "B", "A"))



### order columns ----------------------- ----------------------- ----------------------- -----------------------

colnames(motivation_confidence)

motivation_confidence <- motivation_confidence %>%
  select(sno_student, sno, 
         data_type, questionnaire_type,
         school, age, gender, school_grade, consent_given, 
         what.follow.up.statenents.given, 
         coding_round_1_coder_1, coding_round_1_coder_2, coding_round_1_final_coding, coding_round_1_coder_agreement, 
         coding_round_2_coder_1, coding_round_2_coder_2, coding_round_2_final_coding, coding_round_2_coder_agreement, 
         coding_round_3_coder_1_answer_1, coding_round_3_coder_1_answer_2, coding_round_3_coder_1_answer_3, coding_round_3_coder_1_answer_4, 
         coding_round_3_coder_2_answer_1, coding_round_3_coder_2_answer_2, coding_round_3_coder_2_answer_3, coding_round_3_coder_2_answer_4, 
         coding_round_3_coder_agreement, coding_round_3_final_coding_overall, 
         coding_round_4.1_coder_1_answer_1, coding_round_4.1_coder_1_answer_2, coding_round_4.1_coder_1_answer_3, 
         coding_round_4.1_coder_2_answer_1, coding_round_4.1_coder_2_answer_2, coding_round_4.1_coder_2_answer_3, 
         coding_round_4.1_coder_agreement, coding_round_4.1_final_coding_overall, 
         coding_round_4.2_coder_1_answer_1, coding_round_4.2_coder_1_answer_2, 
         coding_round_4.2_coder_2_answer_1, coding_round_4.2_coder_2_answer_2, 
         coding_round_4.2_coder_agreement, coding_round_4.2_final_coding_overall, 
         final_coding, 
         expectancies,
         value, `value/intrinsic`, `value/utility`,
         `value/utility/daily life`, `value/utility/general`, `value/utility/learning utility`, `value/utility/other`, 
         `value/utility/school utility`, `value/utility/social utility`, `value/utility/useless`, `value/utility`,
         costs, `costs/effort`, `costs/emotional`, , `costs/opportunity`, `costs/outside`,
         goals,
         `goals/evaluative`, `goals/family support`, , `goals/mastery`, , `goals/normative`, `goals/outcome`, `goals/social approval`, 
         `goals/social concern`, `goals/social responsibility`, `goals/social status`,
         `other factors`, consequence,  
         statement_Kinyarwanda, statement_English, 
         statement_stem, statement_in_first_person, auxiliary_verb_used, verb_used, verb_negated, 
         adverb_used, object_1_used, object_2_used, object_3_used, type, 
         what_subjects_listed, subjects.confidence, subjects.demotivation, subjects.motivation, subjects.unconfidence, 
         biology_con, biology_demot, biology_mot, biology_uncon, 
         chemistry_con, chemistry_demot, chemistry_mot, chemistry_uncon, 
         english_con, english_demot, english_mot, english_uncon, 
         entrepreneurship_con, entrepreneurship_demot, entrepreneurship_mot, entrepreneurship_uncon, 
         french_con, french_demot, french_mot, french_uncon, 
         geography_con, geography_demot, geography_mot, geography_uncon, 
         history_con, history_demot, history_mot, history_uncon, 
         ict_con, ict_demot, ict_mot, ict_uncon, 
         kinyarwanda_con, kinyarwanda_demot, kinyarwanda_mot, kinyarwanda_uncon, 
         kiswahili_con, kiswahili_demot, kiswahili_mot, kiswahili_uncon, 
         literature_con, literature_demot, literature_mot, literature_uncon, 
         mathematics_con, mathematics_demot, mathematics_mot, mathematics_uncon, 
         physics_con, physics_demot, physics_mot, physics_uncon, 
         religion_con, religion_demot, religion_mot, religion_uncon, 
         sports_con, sports_demot, sports_mot, sports_uncon, 
         mathematics_grades, physics_grades, chemistry_grades, biology_grades, kinyarwanda_grades, english_grades, 
         geography_grades, history_grades, entrepreneurship_grades, ict_grades, religion_grades, 
         french_grades, kiswahili_grades, literature_grades, library.and.clubs_grades, sports_grades)



### create data set for 2019 mixed-method study on "Achievement motivation amongst Rwandan students" ----------------------- ----------------------- ----------------------- -----------------------

motivation <- motivation_confidence %>%
  filter(questionnaire_type == "demotivation" | questionnaire_type == "motivation") %>%
  select(-c(what.follow.up.statenents.given, statement_stem, statement_in_first_person, 
            auxiliary_verb_used, verb_used, verb_negated, adverb_used, 
            object_1_used, object_2_used, object_3_used, 
            type, subjects.confidence, subjects.unconfidence)) %>%
  select(-contains("_con")) %>%
  select(-contains("_uncon"))



### Save data ----------------------- ----------------------- ----------------------- -----------------------

write.csv(motivation, "02 processed data/motivation_in_Rwanda_20250124_v01.csv", row.names = FALSE)
write.csv(motivation, "C:/Users/domin/GitHub/2024_article_mixed-method_study_on_motivation_in_Rwanda/01 raw data/motivation_in_Rwanda_20250124_v01.csv", row.names = FALSE)
write.csv(motivation_confidence, "02 processed data/motivation_in_Rwanda_full_20250124_v01.csv", row.names = FALSE)
