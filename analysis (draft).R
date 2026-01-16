attach(campus_integration)
dataset1 <- read_excel("campus_integration.xlsx")
str(dataset1)
summary(dataset1$campus_integration_score)

cor.test(dataset1$no_clubs, dataset1$campus_integration_score, method = "spearman")

boxplot(campus_integration~no_clubs, data = dataset1, xlab = "Number of Clubs", ylab = "Campus Integeration Score", main = "Campus Integeration by Number of Clubs")

str(dataset1)

dataset1$no_clubs <- factor(  dataset1$no_clubs,  levels = c(0, 1, 2, "3 or more"))
names(dataset1)

#objective 3

#campus integration by club participation
anova_clubs <- aov(campus_integration_score ~ no_clubs, data = dataset1)
summary(anova_clubs)
boxplot(campus_integration_score ~ no_clubs, data = dataset1,xlab="Number of Clubs", ylab = "Campus Integration Score", main = "Campus Integration by Club Participation")
summary(anova_clubs)

anova_hours_clubs <- aov(campus_integration_score ~ hours_clubs, data = dataset1)

#campus integration by number of sports
dataset1$no_sports <- factor(  dataset1$no_clubs,  levels = c(0, 1, 2, "3 or more"))

anova_sports <- aov(campus_integration_score ~ no_sports, data = dataset1)
summary(anova_sports)
boxplot(campus_integration_score ~ no_sports, data = dataset1,xlab="Number of Sports", ylab = "Campus Integration Score", main = "Campus Integration by Sports Participation")

#campus integration with event frequency
anova_event <- aov(campus_integration_score~event_frequency, data = dataset1)
summary(anova_event)
#normality assumption check
shapiro.test(residuals(anova_event))
shapiro.test(residuals(anova_clubs))
shapiro.test(residuals(anova_sports))

#homogeinity of variance
library(car)
leveneTest(campus_integration_score ~ event_frequency, data = dataset1)
leveneTest(campus_integration_score ~ no_clubs, data = dataset1)
leveneTest(campus_integration_score ~ no_sports, data = dataset1)


oneway.test(campus_integration_score~event_frequency, data = dataset1,var.equal = FALSE)

kruskal.test(campus_integration_score~ event_frequency,data=dataset1)
#Dunn's Test to find which gruop is statistically significant
install.packages("FSA")
library(FSA)
dunnTest(campus_integration_score~event_frequency, data = dataset1, method = "bonferroni")
boxplot(campus_integration_score~event_frequency,dataset1,xlab="Event Participation Frequency",ylab="Campus Integration Score",main="Campus Integration by Event Participation")

#objective 4
anova_gender <- aov(campus_integration_score ~ gender, data = dataset1)
summary(anova_gender)
shapiro.test(residuals(anova_gender))
leveneTest(campus_integration_score ~ gender, data = dataset1)
boxplot(campus_integration_score~gender,dataset1,xlab="Gender",ylab="Campus Integration Score",main="Campus Integration by Gender")

anova_rel <- aov(campus_integration_score ~ relationship_status, data = dataset1)
summary(anova_rel)
shapiro.test(residuals(anova_rel))
leveneTest(campus_integration_score ~ relationship_status, data = dataset1)
boxplot(campus_integration_score~relationship_status,dataset1,xlab="Relationship Status",ylab="Campus Integration Score",main="Campus Integration by Relationship Status")

anova_residence <- aov(campus_integration_score ~ residence_type, data = dataset1)
summary(anova_residence)
shapiro.test(residuals(anova_residence))
leveneTest(campus_integration_score ~ residence_type, data = dataset1)
boxplot(campus_integration_score~residence_type,dataset1,xlab="Residence Type",ylab="Campus Integration Score",main="Campus Integration by Residence Type")
