# Reads in data and gets variables
data = read.csv('Heritage_Cleaned_Again.csv')
tax = data$Tax.Burden...of.GDP
tariffs = data$Tariff.Rate....
gdp_per_capita = data$GDP.per.Capita..PPP.
score = data$Score
year = data$Year
inflation = data$Inflation....
pop_growth = data$Pop_growth
fdi_inflow = data$FDI.Inflow..Millions.
debt = data$Public.Debt....of.GDP.
unemployment = data$Unemployment....
corporate_tax = data$Corporate.Tax.Rate....
growth = data$GDP.Growth.Rate....

# Derives columns
mean_growth = mean(growth)
median_growth = median(growth)



# Creates simple linear regression model, first shot
reg = lm(growth ~ score + inflation +
           gdp_per_capita + tariffs + pop_growth 
         + fdi_inflow + debt + 
           unemployment + corporate_tax + factor(Year) + 
           factor(Region), data=data)
summary(reg)


hist(growth, breaks=80)

# Simpler linear regression
reg2 = lm(growth ~ tariffs + tax)
summary(reg2)

# Derives income level (approximate...)
data$high_growth = ifelse(data$GDP.Growth.Rate.... >= median_growth, 1, 0)

growth_level = data$high_growth
hist(data$high_growth)

# Create categories based on Value
data$Category <- cut(data$GDP.Growth.Rate...., 
                     breaks = c(-Inf, 20, 40, Inf),
                     labels = c("Low", "Medium", "High"))


# Fit logistic regression model
logit_model <- glm(growth_level ~ score + 
                     pop_growth + gdp_per_capita + debt
                   + unemployment + corporate_tax + fdi_inflow,
                   family = binomial)

summary(logit_model)



  
