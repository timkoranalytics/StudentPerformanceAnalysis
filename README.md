# student-s-performance-dataset
Student's performance dataset analysis
## Reseacrh problem:

This project aims to identify factors that can influence students' final exam scores. Based on the ready-made dataset we will examine different variables and correlation between them to identify which of them influence the final exam scores.  


## Research question:

"What factors may affect students' final exam scores?" 


## About data set: 

This data set was syntactically generated for individuals who are practicing different data analysis methods. It’s available by the following link: https://www.kaggle.com/datasets/lainguyn123/student-performance-factors/data . 

In that case, data was not gathered from any resources, according to its author statement. However, related on the type of data we see it might be collected through different questionnaires among students and their parents (if we talk about categorical variables that are coded as factor level, and numeric variables that represents some discrete numbers such as Sleep_hours and etc. ) as well as from e-journals using its API (for numeric variables that represents scores for exams). 

The data set contains 6607 observations and 20 variables (13 categorical, 7 numeric)


## Variables description:

Hours_Studied (Numeric, Continuous)	- Number of hours spent studying per week.

Attendance (Numeric, Continuous)	- Percentage of classes attended.

Parental_Involvement (Categorical, Ordinal)	- Level of parental involvement in the student's education (Low, Medium, High).

Access_to_Resources (Categorical, Ordinal)	- Availability of educational resources (Low, Medium, High).

Extracurricular_Activities (Categorical, Nominal) - Participation in extracurricular activities (Yes, No).

Sleep_Hours	(Numeric, Continuous) - Average number of hours of sleep per night.

Previous_Scores (Numeric, Continuous) - Scores from previous exams.

Motivation_Level(Categorical, Ordinal) - Student's level of motivation (Low, Medium, High).

Internet_Access (Categorical, Nominal)	- Availability of internet access (Yes, No).

Tutoring_Sessions (Numeric, Discrete)	- Number of tutoring sessions attended per month.

Family_Income (Categorical, Ordinal) - Family income level (Low, Medium, High).

Teacher_Quality (Categorical, Ordinal) - Quality of the teachers (Low, Medium, High).

School_Type (Categorical, Nominal) - Type of school attended (Public, Private).

Peer_Influence (Categorical, Ordinal)	- Influence of peers on academic performance (Positive, Neutral, Negative).

Physical_Activity (Numeric, Discrete) - 	Average number of hours of physical activity per week.

Learning_Disabilities	(Categorical, Nominal) - Presence of learning disabilities (Yes, No).

Parental_Education_Level (Categorical, Ordinal)	- Highest education level of parents (High School, College, Postgraduate).

Distance_from_Home (Categorical, Ordinal) -	Distance from home to school (Near, Moderate, Far).

Gender (Categorical, Nominal) -	Gender of the student (Male, Female).

Exam_Score (Numeric, Continuous) - 	Final exam score.



For our analysis we choose the following variables: Exam_Score, Hours_Studied, Sleep_Hours, Attendance, Previous_Scores, School_Type, Parental_Involvement. 


## Descriptive statistics, plots and correlation matrix: 

Firstly, I think it is quite important to look at central tendencies measures of our variables, for this purpose we will calculate mean, median and mode for numeric variables of our interest (Exam_Score, Hours_Studied, Sleep_Hours, Attendance, Previous_Scores), after it we will build histograms for these variables to make it more visual. For categorical variables of our (School_Type, Parental_Involvement) bar and pie charts will be built. Then we will build a correlation matrix to see if there are any relationships between our variables. And also some plots to make these correlations more visual. 



```{r}
options(repos = c(CRAN = "https://cloud.r-project.org"))

library(tidyverse)

StudentPerformanceFactors2 <- read.csv("C:\\Users\\Гадза\\Documents\\StudentPerformanceFactors.csv")



proj_data <- StudentPerformanceFactors2 %>%
  select(Exam_Score, Hours_Studied, Sleep_Hours, Attendance, Previous_Scores, Parental_Involvement, School_Type) 

proj_data_missing <- sum(is.na(proj_data)) #no missing values 

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary_proj_data <- proj_data %>% 
  summarize(
    mean_Exam_Score = mean(Exam_Score),
    median_Exam_Score = median(Exam_Score),
    mode_Exam_Score = getmode(Exam_Score),
    mean_Sleep_Hours = mean(Sleep_Hours),
    median_Sleep_Hours = median(Sleep_Hours),
    mode_Sleep_Hours = getmode(Sleep_Hours),
    mean_Attendance = mean(Attendance),
    median_Attendance = median(Attendance),
    mode_Attendance = getmode(Attendance),
    mean_Previous_Scores = mean(Previous_Scores),
    median_Previous_Scores = median(Previous_Scores),
    mode_Previous_Scores = getmode(Previous_Scores),
    mean_Hours_Studied = mean(Hours_Studied),
    median_Hours_Studied = median(Hours_Studied),
    mode_Hours_Studied = getmode(Hours_Studied)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Statistic", "Variable"),
    names_pattern = "^(mean|median|mode)_(.+)$" 
  ) %>%
  pivot_wider(
    names_from = "Statistic",
    values_from = "value"
  )

proj_data_missing
summary_proj_data
```
From calculated central tendencies we can already make several conclusions about how our data is distributed: 

1. For "Exam_Score" the distribution looks symmetrical(Normal), since mean (67,23), median (67) are almost equal, however, it might be slightly skewed to the right side because of the mode (68). 
2. For "Sleep_Hours" the distribution is also symmetrical(Normal), its mean (7,02), median (7) and mode (7) are approximately equal.
3. For "Attendance" the distribution is asymmetrical, since mode (67) is lower than its mean (80) and median(80). 
4. For "Previous_Scores" the distribution is also asymmetrical, mode (66) is lower than its median (75) and mean (75.1). 
5. For "Hours_Studied" the distribution looks normal, since mean (20), median (20) and mode (20) are approximately equal. 
 
Now, let's build histograms for our variables to support central tendencies calculation.

## Distributions graphs: 

ggplot(data = proj_data, aes(x = Exam_Score)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'orange') +
  ggtitle('Final Exam Scores distribution') +
  ylab('N of Students')+
  xlab('Scores') +
  theme_light()
```

Indeed, for variable Exam_Score we encounter a right-skewed distribution, what was expected after measuring central tendencies. 
```{r}
ggplot(data = proj_data, aes(x = Previous_Scores)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'yellow') +
  ggtitle('Previous Exam Scores distribution') +
  ylab('N of Students')+
  xlab('Scores') +
  theme_light()
```

For Previous_Scores we were not right about its distribution, on the plot we can see that it is quite symmetrical and it is approximately uni-form. 
```{r}
ggplot(data = proj_data, aes(x = Sleep_Hours)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'lightblue') +
  ggtitle('Sleep Hours distribution') +
  ylab('N of Students')+
  xlab('Hours of Sleep') +
  theme_light()
```

For Sleep_Hours distribution is obviously normal. 
```{r}
ggplot(data = proj_data, aes(x = Hours_Studied)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'green') +
  ggtitle('Hours Studied distribution') +
  ylab('N of Students')+
  xlab('Hours Studied') +
  theme_light()
```

For Hours_Studied the distribution is also normal, what was expected after calculated central tendencies measures. 
```{r}
ggplot(data = proj_data, aes(x = Attendance)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'purple') +
  ggtitle('Attendance distribution') +
  ylab('N of Students') +
  xlab('Attendance (%)') +
  theme_light()
```


Here we also encounter approximately uni-form distribution for Attendance variable, thus we were not right, calling this distribution asymmetrical. 

Now let’s look at categorical variables distributions: 

```{r}
ggplot(data = proj_data, aes(x = Parental_Involvement)) +
  geom_bar(color = "black", fill = "skyblue") +
  labs(
    title = "Distribution of Parental Involvement Levels",
    x = "Parental Involvement Level",
    y = "N of Students"
  ) +
  theme_light()

```For Parental_Involvement we can see from the bar chart that the main part of students, approximately 3400 students, have a medium parental involvement level, the second biggest group of students, about 2000, have a high level of parental involvement, the least group, approximately 1200 students, have a low parental involvement. 

```{r}
school_type_data <- proj_data %>%
  count(School_Type) %>%
  mutate(Percentage = n / sum(n) * 100, Label = paste0(School_Type, " (", round(Percentage, 1), "%)"))


ggplot(school_type_data, aes(x = "", y = Percentage, fill = School_Type)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Proportion of School Types",
    fill = "School Type"
  ) +
  theme_void() 
  
```

For School_Type - the majority of students, almost 70%,  study in the public schools, while 30% of students in the private ones.

## Correlation Matrix 

```{r}
install.packages("corrplot")
library(corrplot)

numeric_data <- proj_data[sapply(proj_data, is.numeric)]

cor_matrix <- cor(numeric_data, use = "complete.obs")

cor_matrix
corrplot(cor_matrix, method = "number")

```

Based on the correlation matrix results we can see that the maximum positive correlation is between Attendance and Exam_Score (0.58). The weakest negative correlation is between Sleep_hours and Exam_Score (-0.02). The weakest positive correlation is between Previous_Scores and Exam_Score (0.18).Hours_Studied also has a significant correlation in relation to Exam_Score (0.45).

## Scatter and Box plots:

```{r}
ggplot(data = proj_data, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Hours Studied vs. Exam Score with Regression Line") +
  xlab("Hours Studied") +
  ylab("Exam Score") +
  theme_minimal() 
```

We can see on a scatter plot that the regression line has a positive slope as well as almost all observations are concentrated around the line. Thus, if students study more hours before an exam their final exam score will be higher.

```{r}
ggplot(data = proj_data, aes(x = Previous_Scores, y = Exam_Score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Previous Scores vs. Exam Score with Regression Line") +
  xlab("Previous Scores") +
  ylab("Exam Score") +
  theme_minimal()  
```

The slope of the regression line is positive. But the angle is very small and observations are widely scattered from the line. However, there is still some kind of correlation, but it is weak (we saw that on the correlation matrix the coefficient was pretty small too). In that case, previous scores have some effect on final exam scores, however it is not significant. 

```{r}
ggplot(data = proj_data, aes(x = Sleep_Hours, y = Exam_Score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Sleep Hours vs. Exam Score with Regression Line") +
  xlab("Sleep Hours") +
  ylab("Exam Score") +
  theme_minimal() 

```


It's clearly seen from the graph that the regression line is almost horizontal as well as observations spread too far from the line, indicating that there is no correlation. Correlation matrix displayed negative correlation, however the coefficient is too small, that is why we encounter an approximately horizontal regression line on a scatter plot. For this reason, we can say that the sleep hours of students do not really affect their performance at final exam. 

```{r}
ggplot(data = proj_data, aes(x = Attendance, y = Exam_Score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Attendance vs. Exam Score with Regression Line") +
  xlab("Attendance") +
  ylab("Exam Score") +
  theme_minimal() 

```

We can see on a plot that the regression line also has a positive slope as well as almost all observations are concentrated near the line. Thus, we can say that the more frequently students attend their classes the higher exam score they get. 

```{r}
ggplot(data = proj_data, aes(x = Parental_Involvement, y = Exam_Score, fill = Parental_Involvement)) +
  geom_boxplot() +
  labs(title = "Exam Scores by Parental Involvement",
       x = "Parental Involvement",
       y = "Exam Score") +
  theme_minimal()
```

To evaluate the effect of Parental involvement on a final exam score a box plot was built. We wanted to check whether the average scores vary for different levels of parental involvement. Based on this plot, we can see that the higher level of parental involvement is the higher the score, but the difference is not that big, as it can be seen on the graph that for all 3 groups the average score is between 65 and 70 points.

```{r}
ggplot(data = proj_data, aes(x = School_Type, y = Exam_Score, fill = School_Type)) +
  geom_boxplot() +
  labs(title = "Exam Scores by School Type",
       x = "School Type",
       y = "Exam Score") +
  theme_minimal()
```

The same graph was built to check how school types affect the final exam score. However, we didn't find any big differences. The medians for both groups are almost equal. Thus, we can say that school type does not really affect the final exam score. 



H1: Hours studied have a positive effect on final exam scores.

H2: Previous scores also positively impact the final exam score.

H3: Sleep hours do not affect final exam scores.

H4: Students' attendance will also contribute to a better exam score.

H5: Students with higher parental involvement rate will most likely encounter better average exam scores.

H6: The average score for the final exam does not differ for private and public schools.
