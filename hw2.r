df <- read.csv("Downloads/ak91.csv")
# Question 5:
# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

# Find college graduates
has_college_degree <- yrs_educ == 16
collegesubset <- subset(df, yrs_educ == 16)
#Part A:
mu_x = mean(has_college_degree)

#Part B:
mu_college = mean(collegesubset$WKLY_WAGE)

# Part C:
n = length(df$WKLY_WAGE)
se_college = 1/sqrt(n)*sqrt((sd(collegesubset$WKLY_WAGE)^2)/mu_x)

# Part D: 
CILower = (mu_college - (1.96*se_college))
CIUpper = (mu_college + (1.96*se_college))

# Part E:

T1 = (mu_college - 600)/se_college

# Part F:
T2 = (mu_college - 595)/se_college
