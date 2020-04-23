library(tidyverse)


CourseGrades <- read.csv("~/Documents/CourseGrades.csv", 
                         stringsAsFactors=FALSE)
SemesterData <- read.csv("~/Documents/SemesterData.csv", 
                         stringsAsFactors=FALSE)
StudentData <- read.csv("~/Documents/StudentData.csv", 
                        stringsAsFactors=FALSE)


CourseGrades$GradingType <- "HARD"
CourseGrades[which(CourseGrades$SEMESTER >= 20131),]$GradingType <- "EASY"
CourseGrades$GradingType <- as.factor(CourseGrades$GradingType)

CourseGrades_Orig <- CourseGrades

###MERGE###

#BASE 
CourseGrades_BASE <- merge(x = CourseGrades, y = SemesterData, by = c("STU_NUM", "SEMESTER"))
CourseGrades_BASE <- merge(x = CourseGrades_BASE, y = StudentData, by = c("STU_NUM"))

#TIDY
CourseGrades <- CourseGrades %>%
                inner_join(.,SemesterData, by = c("STU_NUM", "SEMESTER")) %>%
                inner_join(.,StudentData, by = "STU_NUM")


###AGGREGATE###

#BASE
CourseGrades_DistinctSTU <- unique(CourseGrades_BASE[,c("STU_NUM", "GradingType", 
                                                        "Cumulative_CREDITS", "GENDER")])
MEAN_Credits <- aggregate(CourseGrades_DistinctSTU$Cumulative_CREDITS, 
                          by = list(CourseGrades_DistinctSTU$GradingType, 
                                    CourseGrades_DistinctSTU$GENDER), FUN = mean)
colnames(MEAN_Credits) <- c("GradingType", "GENDER", "MEAN")

SD_Credits <- aggregate(CourseGrades_DistinctSTU$Cumulative_CREDITS, 
                        by = list(CourseGrades_DistinctSTU$GradingType, 
                                  CourseGrades_DistinctSTU$GENDER), FUN = sd)
colnames(SD_Credits) <- c("GradingType", "GENDER", "SD")

COUNT_Credits <- as.data.frame(table(CourseGrades_DistinctSTU$GradingType, CourseGrades_DistinctSTU$GENDER))
colnames(COUNT_Credits) <- c("GradingType", "GENDER", "Count")

BASE_Credits_Summary <- merge(x = MEAN_Credits, y = SD_Credits, by = c("GradingType", "GENDER"))
BASE_Credits_Summary <- merge(x = BASE_Credits_Summary, y = COUNT_Credits, by = c("GradingType", "GENDER"))

#Tidy
Tidy_Credits_Summary <- CourseGrades %>%
                        distinct(.,STU_NUM, GradingType, Cumulative_CREDITS, GENDER) %>%
                        group_by(GradingType, GENDER) %>%
                        summarize(.,
                               MEAN = mean(Cumulative_CREDITS),
                               SD = sd(Cumulative_CREDITS),
                               COUNT = n())




#Alternative tidy Put it all together.

Tidy_Credits_Summary_AtOnce <- CourseGrades_Orig %>%
                              inner_join(.,SemesterData, by = c("STU_NUM", "SEMESTER")) %>%
                              inner_join(.,StudentData, by = "STU_NUM") %>%
                              distinct(.,STU_NUM, GradingType, Cumulative_CREDITS, GENDER) %>%
                              group_by(GradingType, GENDER) %>%
                              summarize(.,
                              MEAN = mean(Cumulative_CREDITS),
                              SD = sd(Cumulative_CREDITS),
                              COUNT = n())

                        

