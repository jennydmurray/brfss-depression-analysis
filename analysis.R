library(dplyr)
library(ggplot2)

brfss_emp_dep <- brfss2013 %>%
  select(employ1, misdeprd) %>%
  na.omit()

dep_df <- brfss_emp_dep %>%
  group_by(employ1, misdeprd) %>%
  count() %>%
  group_by(employ1) %>%
  mutate(Freq = n / sum(n)) %>%
  ungroup()

dep_df$employ1 <- factor(
  dep_df$employ1,
  levels = c(
    "Employed for wages",
    "Self-employed",
    "A homemaker",
    "A student",
    "Retired",
    "Out of work for less than 1 year",
    "Out of work for 1 year or more",
    "Unable to work"
  )
)

ggplot(dep_df, aes(x = employ1, y = Freq, fill = misdeprd)) +
  geom_col() +
  labs(
    title = "Depression distribution by employment status",
    x = "Employment status",
    y = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


brfss_emp_inc_dep <- brfss2013 %>%
  select(employ1, income2, misdeprd) %>%
  na.omit() %>%
  mutate(
    income_group = case_when(
      income2 %in% c("Less than $10,000",
                     "Less than $15,000",
                     "Less than $20,000") ~ "Low income",
      
      income2 %in% c("Less than $25,000",
                     "Less than $35,000",
                     "Less than $50,000") ~ "Middle income",
      
      TRUE ~ "High income"
    ),
    
    employ_group = case_when(
      employ1 %in% c("Out of work for less than 1 year",
                     "Out of work for 1 year or more") ~ "Unemployed",
      TRUE ~ as.character(employ1)
    )
  )

income_emp_dep_df <- brfss_emp_inc_dep %>%
  group_by(employ_group, income_group, misdeprd) %>%
  count() %>%
  group_by(employ_group, income_group) %>%
  mutate(Freq = n / sum(n)) %>%
  ungroup()

ggplot(income_emp_dep_df,
       aes(x = employ_group, y = Freq, fill = misdeprd)) +
  geom_col() +
  facet_wrap(~ income_group) +
  labs(
    title = "Depression by employment status and income",
    x = "Employment status",
    y = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


brfss_emp_edu_dep <- brfss2013 %>%
  select(employ1, educa, misdeprd) %>%
  na.omit() %>%
  mutate(
    edu_group = case_when(
      educa %in% c("Never attended school or only kindergarten",
                   "Grades 1 through 8 (Elementary)",
                   "Grades 9 though 11 (Some high school)") ~ "Less than high school",
      
      educa == "Grade 12 or GED (High school graduate)" ~ "High school",
      
      educa == "College 1 year to 3 years (Some college or technical school)" ~ "Some college",
      
      educa == "College 4 years or more (College graduate)" ~ "College graduate"
    ),
    
    employ_group = case_when(
      employ1 %in% c("Out of work for less than 1 year",
                     "Out of work for 1 year or more") ~ "Unemployed",
      TRUE ~ as.character(employ1)
    )
  )

edu_emp_dep_df <- brfss_emp_edu_dep %>%
  group_by(employ_group, edu_group, misdeprd) %>%
  count() %>%
  group_by(employ_group, edu_group) %>%
  mutate(Freq = n / sum(n)) %>%
  ungroup()

ggplot(edu_emp_dep_df,
       aes(x = employ_group, y = Freq, fill = misdeprd)) +
  geom_col() +
  facet_wrap(~ edu_group) +
  labs(
    title = "Depression by employment status and education",
    x = "Employment status",
    y = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
