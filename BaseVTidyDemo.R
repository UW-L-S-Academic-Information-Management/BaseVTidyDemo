library(ROracle)
library(tidyverse)
library(rJava)
library(xlsxjars)
library(xlsx)
library(readxl)
library(reshape2)
library(readxl)
library(readr)
library(EncryptDF)
library(CurricDataTools)

CourseGrades 
SemesterData 
StudentData 

CourseGrades$GradingType <- "HARD"
CourseGrades[which(CourseGrades$SEMESTER >= 20131),]$GradingType <- "EASY"
CourseGrades$GradingType <- as.factor(CourseGrades$GradingType)
prop.table(table(CourseGrades$GradingType, CourseGrades$OFFICIAL_GRADE), 1)




CourseGrades <- CourseGrades %>%
                inner_join(.,SemesterData, by = c("STU_NUM", "SEMESTER")) %>%
                inner_join(.,StudentData, by = "STU_NUM")



prop.table(table(CourseGrades[which(CourseGrades$GradingType == "HARD"),]$UNDERREP, 
                 CourseGrades[which(CourseGrades$GradingType == "HARD"),]$OFFICIAL_GRADE), 1)

CourseGrades <- CourseGrades[which(substr(CourseGrades$SEMESTER, 5, 5) %in% c("1", "2")),]
