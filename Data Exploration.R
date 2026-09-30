# Load packages (install if missing)
packages <- c("dplyr", "ggplot2", "tidyr", "stringr", "forcats", 
              "plm", "data.table", "broom")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(data.table)
library(gt)

df <- read.csv("C:/Users/IAN/Downloads/econ_stats_data.csv", stringsAsFactors = TRUE)
glimpse(df)

# Q1. checking Dimensions
dim(df)

# Q2. counting Occupation counts
df %>% count(occupation, sort = TRUE)

# Q3. Summary statisitcs for wages
df %>% 
  summarise(
    mean_wage   = mean(hourly_wage),
    median_wage = median(hourly_wage),
    sd_wage     = sd(hourly_wage),
    mean_salary = mean(annual_salary),
    median_salary = median(annual_salary)
  )

# Q4.check Missing values
colSums(is.na(df))

# Q5.creating frequency table of Remote work by gender with proportions
df %>% 
  count(gender, remote_work) %>% 
  group_by(gender) %>% 
  mutate(prop = n / sum(n))

# Q6. Average salary by occupation sorting from highest to lowest
df %>% 
  group_by(occupation) %>% 
  summarise(
    avg_salary = mean(annual_salary, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(avg_salary)) %>% gt()

# Q7. Create a new variable called experience_group:"Junior" if experience ≤ 5, "Mid" if experience between 6 and 15, "Senior" if experience ≥ 16
#Then calculate the average hourly_wage for each group.
df <- df %>% 
  mutate(experience_group = case_when(
    experience <= 5  ~ "Junior",
    experience <= 15 ~ "Mid",
    TRUE             ~ "Senior"
  ))
#Then calculate the average hourly_wage for each group.
df %>% 
  group_by(experience_group) %>% 
  summarise(avg_wage = mean(hourly_wage), n = n())

# Q8. t-test
t.test(hourly_wage ~ has_certification, data = df)
#Conclusion: Reject H₀. There is a statistically significant difference in mean hourly wage between the two groups.

# Q9. Simple regression $ Interpret the coefficients
model1 <- lm(hourly_wage ~ education_years + experience, data = df)
summary(model1)
#Intercept (20.77): The predicted hourly wage for a worker with 0 years of education and 0 years of experience.

#education_years (1.86): Holding experience constant, each additional year of education is associated with an increase of $1.86 per hour in wage.

#experience (0.59): Holding education constant, each additional year of experience is associated with an increase of $0.59 per hour in wage.
#Both education and experience have strong, independent positive effects on wage. The education effect is roughly 3× larger than the experience effect  in this model.

tidy(model1)   # broom version

# Q10.Create a scatter plot of `education_years` vs `hourly_wage`, colored by `gender`, and add linear trend lines.
ggplot(df, aes(education_years, hourly_wage, color = gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Wage vs Education by Gender")

# Q11. Multiple log regression + diagnostics
model2 <- lm(log(hourly_wage) ~ education_years + experience + I(experience^2) +
               gender + has_certification + occupation, data = df)
summary(model2) 
tidy(model2, conf.int = TRUE)

par(mfrow = c(2,2))
plot(model2)

# Q12. Handle the missing values in `job_satisfaction` and `years_in_role` in Listwise deletion  :  
df_complete <- df %>% drop_na(job_satisfaction, years_in_role) %>% gt()
#mean
mean(df_complete$job_satisfaction)

# Q13. Dodged bar chart showing average `annual_salary` by `sector` and `remote_work`.
df %>% 
  group_by(sector, remote_work) %>% 
  summarise(avg_salary = mean(annual_salary), .groups = "drop") %>% 
  ggplot(aes(sector, avg_salary, fill = remote_work)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Avg Salary by Sector & Remote Work", y = "Average Salary")

# Q14. Chi-square Perform a Chi-square test to check if the distribution of `remote_work` differs by `sector`.
chisq.test(table(df$sector, df$remote_work))
#Interpretation: There is no statistically significant evidence that sector and remote_work are associated 
#— the distribution of remote work status does not differ meaningfully across sectors in this sample.

# Q15. Create a career_score:=education_years*2 + experience + has_certification*5 + years_in_role (treat missing years_in_role as 0)  
# and Show the top 10 people by this score (id, occupation, sector, score, salary).
df %>% 
  mutate(career_score = education_years * 2 + experience + 
           has_certification * 5 + replace_na(years_in_role, 0)) %>% 
  arrange(desc(career_score)) %>% 
  select(id, occupation, sector, career_score, annual_salary) %>% 
  head(10) %>% gt()


# T1. create a summary table by `occupation` and `gender` that shows:  
# number of people, mean wage, median wage, mean education, and % with certification.
df %>% 
  group_by(occupation, gender) %>% 
  summarise(
    n = n(),
    mean_wage = mean(hourly_wage),
    median_wage = median(hourly_wage),
    mean_edu = mean(education_years),
    pct_certified = mean(has_certification) * 100,
    .groups = "drop"
  ) %>% 
  arrange(occupation, gender)



# T2. For each sector, show the top 3 highest paid people (sector, occupation, annual_salary, education_years, experience)
df %>% 
  group_by(sector) %>% 
  arrange(desc(annual_salary)) %>% 
  slice_head(n = 3) %>% 
  select(sector, occupation, annual_salary, education_years, experience)

