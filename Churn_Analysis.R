# information upload
raw_data <- read.csv("Churn_Modelling.csv")

# Show first rows
head(raw_data)

# data structure
glimpse(raw_data)

# count NA values per column
raw_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

# before deleting it, the customer ID is saved for analysis by person, if necessary
directory_data <- raw_data %>%
  select(CustomerId, Surname)

directory_data

# remove unnecessary columns, useless for analysis
df_clean <- raw_data %>%
  select(-CustomerId, -Surname, -RowNumber) %>%
  # Transform the category variables into a factor
  mutate(
    Geography = as.factor(Geography),
    Gender = as.factor(Gender),
    HasCrCard = as.factor(HasCrCard),
    IsActiveMember = as.factor(IsActiveMember),
    Exited = factor(Exited, levels=c(0,1), labels= c("Stays", "Leaves"))) %>%
  
  # rename the columns name to make them easier to understand
  rename(Churn = Exited)

# Show clean data
colSums(is.na(df_clean))

# Finding outliers with a boxplot
ggplot(df_clean, aes(y= Balance, x=Churn, fill= Churn)) +
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Balance Distribution by Costumer Status", subtitle = "Detection of Outliers")

# Que cidad pierde más clientes por dia
df_clean %>% 
  group_by(Geography, Churn) %>%
  summarise(n=n()) %>%
  mutate(porcentaje = n / sum(n) *100)

# Classes Balance
ggplot(df_clean, aes(x= Churn, fill = Churn))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), vjust=-0.5)+
  theme_minimal()+
  labs(title = "Churn Distribution", y= "Number of clientes")

#Comparison by country
ggplot(df_clean, aes(x=Geography, fill = Churn))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title = "Dropout Rate by Country", y= "Percentaje")

# Analysis of Numerical Variables
# Is customer churn due the credit score ?
ggplot(df_clean, aes(x= Churn, y=CreditScore , fill= Churn))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Credit Score vs Customer Churn")

# Corr Matrix, we need to convert the text data into a numeric data
cor_data <- df_clean %>%
  select(CreditScore, Age, Tenure, Balance, NumOfProducts, EstimatedSalary) %>%
  mutate(across(everything(), as.numeric))

cor_data

cor_matrix <- cor(cor_data)
cor_matrix

cor_df <- as.data.frame(cor_matrix)
cor_df <- rownames_to_column(cor_df, var= "Variable1")

cor_df

cor_long <- pivot_longer(cor_df, cols = -Variable1, names_to = "Variable2", values_to = "Correlacion")
cor_long


ggplot(cor_long, aes(x = Variable1, y = Variable2, fill = Correlacion)) +
  geom_tile() +
  geom_text(aes(label = round(Correlacion, 2)), size = 3) + 
  scale_fill_gradient2(low = "#075AFF", high = "#FF0000", mid = "white", limit = c(-1,1)) +
  theme_minimal()