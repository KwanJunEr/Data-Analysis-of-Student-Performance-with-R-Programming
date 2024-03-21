
#Name : Kwan Jun Er 
#TP Number : TP066265

#Libraries Utilized 


install.packages("ggpubr")
library(ggpubr)
library(plotly)



# 2.0 Data Preparation 
# Import data

#Setting the working directory in the following directory 
setwd("D:/PFDA Assignment")
student_data = read.csv("student_prediction.csv", header=TRUE)
student_data


# Explore data by viewing its number of rows, columns, attributes, data values, and data types
glimpse(student_data)


#Check for duplicated rows 

duplicates = student_data %>%
  duplicated()
duplicates

duplicate_counts = duplicates %>%
    table()
duplicate_counts

View(duplicate_counts)

#Check for Null Values

is.na(student_data)


student_data
# Remove duplicate rows
student_data <- student_data %>%
  distinct(STUDENTID, .keep_all = TRUE)

nrow(student_data)




# Rename columns

student_data <- student_data %>%
  rename(MOTHER_EDUCATION = MOTHER_EDU, WEEKLY_STUDY_HOURS = STUDY_HRS, MIDTERM_PREPERATION_APPROACH = PREP_STUDY, 
         CLASS_NOTE_TAKING = NOTES, STUDENT_GRADE = GRADE)



# Replace values in the "Mother's Education" column
student_data <- student_data %>%
  mutate(MOTHER_EDUCATION = case_match(
    MOTHER_EDUCATION,
    1 ~ "Primary",
    2 ~ "Secondary",
    3 ~ "High School",
    4 ~ "University",
    5 ~ "Masters",
    6 ~ "PhD"
  ))

# Replace values in the "WEEKLY_STUDY_HOURS" column
student_data <- student_data %>%
  mutate(WEEKLY_STUDY_HOURS = case_match(
    WEEKLY_STUDY_HOURS,
    1 ~ "None",
    2 ~ "<5 hours",
    3 ~ "6-10 hours",
    4 ~ "11-20 hours",
    5 ~ ">20 hours"
  ))

# Remove all '3' in "MIDTEMR_STUDY_APPROACH" column


# Replace values in "MIDTEMR_STUDY_APPROACH" column
student_data <- student_data %>%
  mutate(MIDTERM_PREPERATION_APPROACH = case_match(
    MIDTERM_PREPERATION_APPROACH,
    1 ~ "Alone",
    2 ~ "Group",
    3 ~ "Not Applicable"
  ))

# Replace values in "CLASS_NOTE_TAKING" column
student_data <- student_data %>%
  mutate(CLASS_NOTE_TAKING = case_match(
    CLASS_NOTE_TAKING,
    1 ~ "Never",
    2 ~ "Sometimes",
    3 ~ "Always",
  ))

#Replace values in "STUDENT_GRADE" column
student_data <- student_data %>%
  mutate(STUDENT_GRADE = case_when(
    STUDENT_GRADE %in% c(0) ~ "Fail",
    STUDENT_GRADE %in% c(1) ~ "Pass",
    STUDENT_GRADE %in% c(2, 3) ~ "Merit",
    STUDENT_GRADE %in% c(4, 5) ~ "Credit",
    STUDENT_GRADE %in% c(6, 7) ~ "Distinction"
  ))

# Add new column called "GRADE_LEVEL" to signify low and high grade
student_data <- student_data %>%
  mutate(GRADE_LEVEL = case_when(
    STUDENT_GRADE %in% c("Fail", "Pass", "Merit") ~ "Low",
    STUDENT_GRADE %in% c("Credit", "Distinction") ~ "High"
  ))

# Add new column called "MOTHER_EDUCATION_LEVEL" to signify low and high education level
student_data <- student_data %>%
  mutate(MOTHER_EDUCATION_LEVEL = case_when(
    MOTHER_EDUCATION %in% c("Primary", "Secondary", "High School") ~ "Low",
    MOTHER_EDUCATION %in% c("University", "Masters", "PhD") ~ "High"
  ))

View(student_data)

#3.0 Analysis 
#Objective: To analyze how weekly study hours impact student's grades
#3.2.1	Question 1: What is the distribution of student grades?


install.packages("plotly")
library(plotly)
#Calculate number of students for each grade category
grades_count <- student_data %>%
  group_by(STUDENT_GRADE) %>%
  summarize(count = n())
#data exploration
head(grades_count)

#Calculate the percentage of students for each grade category and round them to 1 d.p.
grades_count$pct <- round(grades_count$count 
                          / sum(grades_count$count) * 100, 1)
grades_count$new_labels <- paste0(grades_count$STUDENT_GRADE, ": ",
                                  grades_count$count)



#Create donut pie chart with representation of percentage 
#distribution og student grades 
piedonut_chart <- plot_ly(
  labels = grades_count$STUDENT_GRADE,
  values = grades_count$pct,
  type = "pie",
  text = grades_count$new_labels,  # Updated text argument
  textposition = "outside",
  hole = 0.4
) %>%
  layout(
    title = "Percentage of Students for Each Grade Attained",
    scene = list(
      camera = list(
        eye = list(x = 1.25, y = 1.25, z = 1.25) 
      )
    )
  )
print(piedonut_chart)

#3.2.2 Question 2 : What is the distribution between high and low grade students?

library(plotrix)
#Create new data frame by getting the frequnecy of students for each
#grade level category of high and low
grades_summary <- student_data %>%
  group_by(GRADE_LEVEL) %>%
  summarise(count = n())
tail(grades_summary)

# Calculate percentages of high and low grade students 
grades_summary$percentage <- 
  grades_summary$count / sum(grades_summary$count) * 100

# Create a 3D pie chart with percentage labelling 
pie3D(
grades_summary$count,
labels = paste(grades_summary$GRADE_LEVEL, "
                 Grade\n",
               round(grades_summary$percentage, 1), "%"),
col = c("skyblue", "lightcoral"),
main = "Percentage of Students Achieving Low Grade 
  and High Grade",
explode = 0.5,
radius = 1.5
)

#3.2.3	Question 3: How Weekly Study Hours Affect Students in Attaining High Grade?

library(tidyverse)
library(ggthemes)
library(ggplot2)
#Data Frame which calculate count of students based on weekly study 
#hours and grade level by grouping then calculate the rounded percentages
labelling1 <- student_data %>%
  group_by(WEEKLY_STUDY_HOURS, GRADE_LEVEL) %>%
  summarise(percentage = round(n() 
                               /nrow(student_data) * 100, 1),
            number = n())

summary(labelling1)


# Create the  bar chart with percentage and count labels
visualized <- ggplot(student_data, aes(x = WEEKLY_STUDY_HOURS, 
                                       fill = GRADE_LEVEL)) +
  geom_bar(position = "dodge") +
  geom_text(data = labelling1,
            aes(label = paste0(percentage, "%\n", number), 
                y = after_stat(count) + 5),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, stat = "count") +
  labs(
    title = "Relationship between Students' 
    Grade Level and Weekly Study Hours",
    subtitle = "Source: Student.csv",
    x = "Weekly Study Hours",
    fill = "Grade Level",
    y = "Number of Students"
  ) +
  scale_fill_manual(values = c("#1F78B4", "#E31A1C")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = 
                                "bold", hjust = 0.5, margin = 
                                margin(b = 10), color = "blue"),
    axis.title.x = element_text(size = 14,
                                face = "bold", color = "blue"),
    axis.title.y = element_text(size = 14, 
                                face = "bold", color = "red"),
    axis.text.x = element_text(size = 12, color = "purple"),
    axis.text.y = element_text(size = 12, color = "green"),
    legend.title = element_text(size = 14, face = "bold",
                                color = "brown"),
    legend.text = element_text(size = 12, color = "black")
  ) 
# Display the plot
print(visualized)


#3.2.4	Question 4: What is the relationship between high grade students and their weekly study hours with their mother education level?

library(sqldf)
# Filter for high-grade students
sml1 <- sqldf("SELECT WEEKLY_STUDY_HOURS, MOTHER_EDUCATION_LEVEL, 
  GRADE_LEVEL FROM newdataframe WHERE 
  GRADE_LEVEL = 'High'")

# Group by WEEKLY_STUDY_HOURS and MOTHER_EDUCATION_LEVEL, count the occurrences
sml2 <- sqldf("SELECT WEEKLY_STUDY_HOURS, MOTHER_EDUCATION_LEVEL, 
COUNT(*) as n FROM sml1 GROUP 
BY WEEKLY_STUDY_HOURS, MOTHER_EDUCATION_LEVEL")

# Function to check if count of students lesser than 30,then will pass the red color value 
# to the font of count label then if false,it will pass black color

glimpse(sml2) # For Data Exploration

highlight_color <- function(n) {
  ifelse(n < 30, "red", "black")
}
# Create a heatmap using ggplot2 with a color palette and count labels
visualized3 <- ggplot(sml2, aes(x = MOTHER_EDUCATION_LEVEL, y = WEEKLY_STUDY_HOURS, fill = n)) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = n),
            fontface = "bold",
            color = sml2$n %>% highlight_color(),
            position = position_dodge(width = 0),
            vjust = 1.5,
            size = 4) +
  scale_fill_viridis_c(name = "Number of High Grade Students") +
  labs(title = "Distribution of High Grade Students by Mother's Education Level and Weekly Study Hours",
       x = "Mother's Education Level",
       y = "Weekly Study Hours") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10), color = "blue"),
    axis.title.x = element_text(size = 14, face = "bold", color = "blue"),
    axis.title.y = element_text(size = 14, face = "bold", color = "red"),
    axis.text.x = element_text(size = 12, color = "purple"),
    axis.text.y = element_text(size = 12, color = "orange"),
    legend.title = element_text(size = 14, face = "bold", color = "brown"),
    legend.text = element_text(size = 12, color = "black")
  )

# Display the heatmap
print(visualized3)

#3.2.5	Question 5: How does the combination of weekly study hours and their note taking frequency affect students attaining high grade?


#Creating a new data frame combining >20 hours with 11-20 Hours
newdataframe <- student_data %>%
  mutate(
    WEEKLY_STUDY_HOURS = case_when(
      WEEKLY_STUDY_HOURS %in% c("11-20 hours", ">20 hours") ~ "11+ hours",
      TRUE ~ as.character(WEEKLY_STUDY_HOURS)  # Keep other categories unchanged
    )
  )
#create new data frames which is filtered for high grade only 
TCS = newdataframe %>%
  filter(GRADE_LEVEL == "High")
#create data frame consisting of number of high grade 
#students based on weekly study hours and class note taking category
TCS1 = TCS %>%
  group_by(WEEKLY_STUDY_HOURS,CLASS_NOTE_TAKING) %>%
  summarize(n = n())
TCS1

head(TCS1) # Data Exploration 

# create bubble plot with count labels denoted by color scaling 
bubble_plot <- ggplot(TCS1, aes(x = WEEKLY_STUDY_HOURS, y = CLASS_NOTE_TAKING, 
                                size = n, fill = n)) +
  geom_point(shape = 21, alpha = 0.7, stroke = 0.5) +
  geom_text(aes(label = n), size = 3, vjust = 1.5) +  # Add this line for labels
  scale_size_continuous(range = c(5, 20)) +
  scale_fill_viridis_c() +
  labs(
    title = "Relationship between High Grade Students 
    and Their Weekly Study Hours\nand Class Note Taking Frequency",
    x = "Weekly Study Hours",
    y = "Class Note Taking",
    size = "Number of High Grade Students",
    fill = "Number of High Grade Students"
  ) +
  guides(size = "none") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, 
                              margin = margin(b = 10), color = "blue"),
    axis.title.x = element_text(size = 14, face = "bold", color = "blue"),
    axis.title.y = element_text(size = 14, face = "bold", color = "red"),
    axis.text.x = element_text(size = 12, color = "purple"),
    axis.text.y = element_text(size = 12, color = "orange"),
    legend.title = element_text(size = 14, face = "bold", color = "brown"),
    legend.text = element_text(size = 12, color = "black")
  ) +   
  theme(legend.position = "top")


# Display the plot
print(bubble_plot)


#3.2.6	Question 6: How weekly study hours with midterm preparation approach impact the attainment of high grade in students?

install.packages("ggmosaic")
library(ggmosaic)
#Create data frame to filter for only high grade students
newinvestigation = newdataframe %>%
  filter(GRADE_LEVEL == "High")
#Create data frame consist of count of high grade students based on weekly study hours 
#and midterm preparation approach 
grouping = newinvestigation %>%
  group_by(WEEKLY_STUDY_HOURS,MIDTERM_PREPERATION_APPROACH) %>%
  summarize(count = n())

#For Data Exploration 
install.packages("Hmisc")
library(Hmisc)
describe(grouping)

#Create mosiac plot
mp <- mosaicplot(table(newinvestigation$WEEKLY_STUDY_HOURS, 
                       newinvestigation$MIDTERM_PREPERATION_APPROACH),
                 main = "Relationship between High 
                 Grade Students and Their 
                 Weekly Study Hours and\nMidterm 
                 Preparation Approach",
                 xlab = "Weekly Study Hours",
                 ylab = "Midterm Preparation Approach",
                 col = c("#9FB1C2", "#A8D8B9", "#D8A8A8"))


#3.2.7	Question 7: What is the distribution of different  grade students with high weekly study hours with different ways of midterm preparation approach?

#Create data frame to categorize high and low weekly study hours
newframe = student_data %>%
  mutate(
    Weekly_Study_Hours_Level = case_when(
      WEEKLY_STUDY_HOURS %in% c( "11-20 hours", ">20 hours") ~ "High Weekly Study Hours",
      WEEKLY_STUDY_HOURS %in% c("None", "<5 hours","6-10 hours") ~ "Low Weekly Study Hours",
      TRUE ~ "Other"  # Add a default case if none of the conditions are met
    )
  )
#Create data frame to filter for high grade level and high weekly study hours
#then count the number of students based on the midterm preparation appraoch
filter1 <- newframe %>%
  filter(GRADE_LEVEL == "High" & Weekly_Study_Hours_Level == "High Weekly Study Hours") %>%
  group_by(MIDTERM_PREPERATION_APPROACH) %>%
  summarise(count = n())
head(filter1) #Data Exploration

# Create a bar plot
ggplot(filter1, aes(x = MIDTERM_PREPERATION_APPROACH, y = count,
                    fill = MIDTERM_PREPERATION_APPROACH)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +  # Add count labels
  labs(title = "Midterm Preparation Approach Distribution for 
       High-Grade Students with High Weekly Study Hours",
       x = "Midterm Preparation Approach",
       y = "Number of Students") +theme_minimal() +
  theme(legend.position = "none")

#3.2.8	Question 8: How a combination of Weekly Study Hours, Midterm Preparation Approach, Class Note Taking plays a role in students getting high grade?

#Create data frame to filter for high grade student only 
highgradefiltered = student_data %>%
  filter(GRADE_LEVEL == "High")
#Create data frame for number of high grade students based on the group_by categories 
testing3 = highgradefiltered %>%
  group_by(WEEKLY_STUDY_HOURS,MIDTERM_PREPERATION_APPROACH,CLASS_NOTE_TAKING) %>%
  summarise(count = n())
glimpse(testing3) #Data exploration

#Create new columns to combine values of midterm prepration approach and class note taking
combinedfactor2 = highgradefiltered %>%
  mutate(
    twofactorscombined = case_when(
      MIDTERM_PREPERATION_APPROACH == "Alone" & CLASS_NOTE_TAKING == "Always" ~ 
        "Alone Midterm Preparation Approach, Always taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Alone" & CLASS_NOTE_TAKING == "Never" ~
        "Alone Midterm Preparation Approach, Never taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Alone" & CLASS_NOTE_TAKING == "Sometimes" ~ 
        "Alone Midterm Preparation Approach, Sometimes taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Group" & CLASS_NOTE_TAKING == "Always" ~ "
      Group Midterm Preparation Approach, Always taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Group" & CLASS_NOTE_TAKING == "Never" ~ 
        "Group Midterm Preparation Approach, Never taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Group" & CLASS_NOTE_TAKING == "Sometimes" ~ 
        "Group Midterm Preparation Approach, Sometimes taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Not Applicable" & CLASS_NOTE_TAKING == "Always" ~ 
        "Not Applicable Midterm Preparation Approach, Always taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Not Applicable" & CLASS_NOTE_TAKING == "Never" ~ 
        "Not Applicable Midterm Preparation Approach, Never taking class notes",
      MIDTERM_PREPERATION_APPROACH == "Not Applicable" & CLASS_NOTE_TAKING == "Sometimes" ~ 
        "Not Applicable Midterm Preparation Approach, Sometimes taking class notes",
      TRUE ~ "Other"
    )
  )


#vector storing the colors for sections of stacked bar chart 
custom_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", 
                    "#ffd92f", "#e5c494", "#b3b3b3", "#99ccff")

#creating stacked bar chart 
visualizedforcombined <- ggplot(data = combinedfactor2, aes(x = WEEKLY_STUDY_HOURS, 
                                                            fill = twofactorscombined)) +
  geom_bar(color = "white") +
  geom_text(stat = "count", aes(label = ..count..), position =
              position_stack(vjust = 0.5)) +
  labs(title = "Relationship between High Grade Students and 
  Weekly Study Hours, Midterm Preparation Approach, 
       Class Note Taking",
       x = "Weekly Study Hours",
       y = "Number of High Grade Students",
       fill = "Two Other Factors") +
  theme_bw() +
  scale_fill_manual(values = custom_palette) +  # Use the custom color palette
  coord_flip() + #make the stack bar chart inverted
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, 
                              margin = margin(b = 10), color = "blue"),
    axis.title.x = element_text(size = 14, face = "bold", color = "blue"),
    axis.title.y = element_text(size = 14, face = "bold", color = "red"),
    axis.text.x = element_text(size = 12, color = "purple"),
    axis.text.y = element_text(size = 12, color = "darkblue"),
    legend.title = element_text(size = 14, face = "bold", color = "brown"),
    legend.text = element_text(size = 10, color = "black")
  )  

visualizedforcombined


#3.2.9	Question 9: How a combination of Weekly Study Hours, and Midterm Preparation Approach By Mother Education Level have an impact in students attaining high grade?


install.packages("treemapify")
library(treemapify)

#Create data frame filtered to only have high grade student
highfitlration = student_data %>%
  filter(GRADE_LEVEL == "High")
#create data frame to create new column for high and weekly study hours 
newdataframetree <- highfitlration %>%
  mutate(
    Weekly_Study_Hours_Level = case_when(
      WEEKLY_STUDY_HOURS %in% c("11-20 hours", ">20 hours") ~ "High Weekly Study Hours",
      WEEKLY_STUDY_HOURS %in% c("None", "<5 hours","6-10 hours") ~ "Low Weekly Study Hours",
      TRUE ~ "Other"  # Add a default case if none of the conditions are met
    )
  )
#create new column combining the value of both midterm prepration approach & weekly study hours level 
newdataframetree <- newdataframetree %>%
  mutate(
    combined90 = case_when(
      Weekly_Study_Hours_Level == "High Weekly Study Hours" &
        MIDTERM_PREPERATION_APPROACH == "Group" ~ "High Study Hours and Group Midterm Preparation ",
      Weekly_Study_Hours_Level == "High Weekly Study Hours" & 
        MIDTERM_PREPERATION_APPROACH == "Alone" ~ "High Study Hours and Alone Midterm Preparation ",
      Weekly_Study_Hours_Level == "High Weekly Study Hours" &
        MIDTERM_PREPERATION_APPROACH == "Not Applicable" ~ "High Study Hours and NA  Midterm Preparation ",
      Weekly_Study_Hours_Level == "Low Weekly Study Hours" &
        MIDTERM_PREPERATION_APPROACH == "Group" ~ "Low Study Hours and Group Midterm Preparation ",
      Weekly_Study_Hours_Level == "Low Weekly Study Hours" & 
        MIDTERM_PREPERATION_APPROACH == "Alone" ~ "Low Study Hours and Alone Mideterm Preparation",
      Weekly_Study_Hours_Level == "Low Weekly Study Hours" & 
        MIDTERM_PREPERATION_APPROACH == "Not Applicable" ~ "Low Study Hours and NA Midterm Preparation ",
      TRUE ~ NA_character_  # Add a default case if none of the conditions are met
    )
  )
names(newdataframetree) # data exploration 


#create data frame to calculate number of students based on group by category and the percentage 
framefortreemap <- newdataframetree %>%
  group_by(MOTHER_EDUCATION_LEVEL, combined90) %>%
  summarize(count = n()) %>%
  group_by(MOTHER_EDUCATION_LEVEL) %>%
  mutate(percentage = count / sum(count) * 100)

# Plot the treemap with labels of percentages and counts of students
treemap1 <- ggplot(framefortreemap, aes(area = count, fill = combined90)) +
  geom_treemap() +
  geom_treemap_text(
    aes(label = paste0(combined90, "\n", "Count: ", count, "\n", round(percentage, 1), "%")),
    place = "centre",
    reflow = TRUE
  ) +
  facet_wrap(~MOTHER_EDUCATION_LEVEL) +
  labs(title = "Relationship between High Grade Students and Midterm Preparation 
       Approach,Weekly Study Hours and Mother Education Level  ",
       fill = "Combined Factors",
       caption = "Count and Percentage by Mother Education Level") + 
  theme(strip.text = element_text(face = "italic",size = 12,color = "darkblue"))

# Print the treemap
print(treemap1)

#3.2.10	Question 10 What is the distribution of high grade students with 
#group midterm preparation approach,high weekly study hours with frequency of taking class notes?

#create data frame that filer for high weekly study hours and group midterm preparation approach 
#then proceed to count number of high grade students based on class note taking category
filter2 <- newframe %>%
  filter(GRADE_LEVEL == "High" & Weekly_Study_Hours_Level == "High Weekly Study Hours" 
         & MIDTERM_PREPERATION_APPROACH == "Group")  %>% 
  group_by(CLASS_NOTE_TAKING) %>%
  summarise(count = n())

tail(filter2) # data exploration

#create bar chart with count label
ggplot(filter2, aes(x = CLASS_NOTE_TAKING, y = count, fill = CLASS_NOTE_TAKING)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label  = count), vjust = -0.5, color = "black") +  # Add count labels
  labs(title = "Class Note Taking Distribution for High-Grade Students with 
       \n High Weekly Study Hours and Group Midterm Preparation Approach",
       x = "Class Note Taking Habit",
       y = "Number of Students") + theme_minimal() + theme(legend.position = "none")

filter2

#3.2.11	Question 11 Investigating the Relationship between High Grade Students and Midterm Preparation Approach, Mother Education Level, Class Note Taking

library(networkD3)
install.packages("remotes")
install.packages("ggsankey")
library(ggsankey)
#create data frame filtered for only high grade student 
highgradefiltrationonly1 = student_data %>%
  filter(GRADE_LEVEL == "High")
#createdata framem to count number of high grade students 
#based on group_by categories
groupingtestforSankey = highgradefiltrationonly1 %>%
  group_by(MIDTERM_PREPERATION_APPROACH,
           MOTHER_EDUCATION_LEVEL,CLASS_NOTE_TAKING,
           GRADE_LEVEL) %>%
  summarize(count = n())
summary(groupingtestforSankey) # data exploration 

#conversion into long format for creating Sankey Diagram 
sankey = highgradefiltrationonly1 %>%
  make_long(MIDTERM_PREPERATION_APPROACH,
            MOTHER_EDUCATION_LEVEL,CLASS_NOTE_TAKING,
            GRADE_LEVEL)
#calculate counts for each unique node grouped
dagg = sankey %>%
  dplyr::group_by(node) %>%
  tally()
#merge the sankey data frame and dagg data frame based on node column
sankey2 = merge(sankey,dagg,by.x = 'node',by.y = 'node',all.x = TRUE)

#create sankey diagram 
visualized80 = ggplot(sankey2,aes(x = x,
                                  next_x = next_x,
                                  node = node,
                                  next_node = next_node,
                                  fill = factor(node),
                                  label = paste0(node,",n=",n))
)
visualized80 = visualized80 + geom_sankey(flow.alpha = 0.5,
                                          node.color = "black",
                                          show.legend = FALSE)
visualized80 = visualized80 + geom_sankey_label(size = 3,
                                                color = "black",fill = 
                                                  "white",hjust = -0.5)
visualized80 = visualized80 + theme_bw() +
  theme(legend.position = "none") 
visualized80 = visualized80 + theme(axis.title = element_blank(),
                                    axis.text.y = element_blank(),
                                    axis.ticks = element_blank(),
                                    panel.grid = element_blank())
visualized80 = visualized80 + scale_fill_viridis_d(option = "inferno")
visualized80 = visualized80 + labs(
  title = "Relationship between High Grade Students and Midterm Preparation Approach, 
  Mother Education Level, Class Note Taking",
  subtitle = "using David Sjoberg's ggsankey package",
  fill = 'Nodes'
)
visualized80

#3.2.12	Question 12: What is the distribution of high grade students with high weekly study hours, 
#group midterm preparation approach, always taking class notes with different mother education levels?

#create data frame filtered only for high grade level,high weekly study hours level,
#group midterm preparation approach,always taking class notes then count the number o
#of students based on mother education level
filter3 = newframe %>%
  filter(GRADE_LEVEL == "High" & Weekly_Study_Hours_Level 
         == "High Weekly Study Hours" &
           MIDTERM_PREPERATION_APPROACH == "Group" & 
           CLASS_NOTE_TAKING == "Always") %>%
  group_by(MOTHER_EDUCATION_LEVEL) %>%
  summarise(count = n())

head(filter3)  # data exploration 

#creating the bar plot with the count label 
ggplot(filter3, aes(x = MOTHER_EDUCATION_LEVEL, y = count,
                    fill = MOTHER_EDUCATION_LEVEL)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +  # Add count labels
  labs(title = "Mother Education Level Distribution for
       High-Grade Students with \n High Weekly Study Hour, 
       Group Midterm Preparation Approach,
       and High Mother Education Level",
       x = "Mother Education Level",
       y = "Number of Students") + theme_minimal() +
  theme(legend.position = "none")


#3.2.13	Machine Learning and Testing

#Naives Bayes Model 

#Naives Bayes Algorithm

install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)


# Convert categorical variables to factors if needed
student_data$MIDTERM_PREPERATION_APPROACH <- as.factor(student_data$MIDTERM_PREPERATION_APPROACH)
student_data$WEEKLY_STUDY_HOURS <- as.factor(student_data$WEEKLY_STUDY_HOURS)
student_data$CLASS_NOTE_TAKING <- as.factor(student_data$CLASS_NOTE_TAKING)
student_data$MOTHER_EDUCATION_LEVEL <- as.factor(student_data$MOTHER_EDUCATION_LEVEL)
student_data$GRADE_LEVEL <- as.factor(student_data$GRADE_LEVEL)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
indices <- createDataPartition(student_data$GRADE_LEVEL, p = 0.7, list = FALSE)
train_data <- student_data[indices, ]
test_data <- student_data[-indices, ]

# Create a Naive Bayes model
nb_model <- naiveBayes(GRADE_LEVEL ~ MIDTERM_PREPERATION_APPROACH + WEEKLY_STUDY_HOURS + CLASS_NOTE_TAKING + MOTHER_EDUCATION_LEVEL, 
                       data = train_data)

# Print the summary of the model
print(nb_model)

# Make predictions on the test dataset
predictions <- predict(nb_model, test_data)

# Print the confusion matrix
confusion_matrix <- table(predictions, test_data$GRADE_LEVEL)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

#Chi-Squared Test for All Individual Variable 
contingency_table = table(student_data$GRADE_LEVEL,
                    student_data$WEEKLY_STUDY_HOURS)
chi_squared_result = chisq.test(contingency_table)
chi_squared_result

contingency_table1 = table(student_data$GRADE_LEVEL,
                    student_data$MIDTERM_PREPERATION_APPROACH)
chi_squared_result1 = chisq.test(contingency_table1)
chi_squared_result1

contingency_table2 = table(student_data$GRADE_LEVEL,
                    student_data$MOTHER_EDUCATION_LEVEL)
chi_squared_result2 = chisq.test(contingency_table2)
chi_squared_result2

contingency_table3 = table(student_data$GRADE_LEVEL,
                    student_data$CLASS_NOTE_TAKING)
chi_squared_result3 = chisq.test(contingency_table3)
chi_squared_result3


#3.2.14	Conclusion 

library(ggsankey)


#Create new data frame and create new column for high and low weekly study hours 
newframe1 = student_data %>%
  mutate(
    Weekly_Study_Hours_Level = case_when(
      WEEKLY_STUDY_HOURS %in% c("11-20 hours", ">20 hours") ~ "High Weekly Study Hours",
      WEEKLY_STUDY_HOURS %in% c("None", "<5 hours", "6-10 hours") ~ "Low Weekly Study Hours",
      TRUE ~ "Other"  # Add a default case if none of the conditions are met
    )
  )


#filter for high grade level then select the five columns then modify the values 
# in Weeky Study Hours Level and mother education level and convert to long form 
#for creation of sankey diagram 
highgradesummary = newframe1 %>%
  filter(GRADE_LEVEL == "High") %>%
  select(GRADE_LEVEL, MOTHER_EDUCATION_LEVEL, Weekly_Study_Hours_Level, 
         MIDTERM_PREPERATION_APPROACH, CLASS_NOTE_TAKING) %>%
  mutate(Weekly_Study_Hours_Level = case_when(
    Weekly_Study_Hours_Level == "Low Weekly Study Hours" ~ "Low Hours",
    Weekly_Study_Hours_Level == "High Weekly Study Hours" ~ "High Hours",
    TRUE ~ as.character(Weekly_Study_Hours_Level)  
  )) %>%
  mutate(MOTHER_EDUCATION_LEVEL = case_when(
    MOTHER_EDUCATION_LEVEL %in% c("Low") ~ "Low Education",
    MOTHER_EDUCATION_LEVEL %in% c("High") ~ "High Education"
  )) %>%
  make_long(GRADE_LEVEL,MOTHER_EDUCATION_LEVEL,Weekly_Study_Hours_Level,
            CLASS_NOTE_TAKING,MIDTERM_PREPERATION_APPROACH)

set.seed(111)
TotalCount = nrow(student_data)
#create count for the unique nodes 
high_grade_students_agg = highgradesummary %>%
  group_by(node) %>%
  tally()
#Calculate percentages 
high_grade_students_agg = high_grade_students_agg %>%
  group_by(node) %>%
  mutate(pct = n/TotalCount)

#merge both highgradesummary data framen and high_grade_students_agg data frame 
high_grade_students_values1 = merge(highgradesummary,high_grade_students_agg,
                                    by.x = 'node',by.y = 'node',all.x = TRUE)
#create sankey diagram 
ggplot(high_grade_students_values1,aes(x = x,next_x = next_x,node = node,
                                       next_node = next_node,fill = factor(node),
                                       label = paste0(node," n= ", n, '(', round(pct * 100,1), '%)'))) +
  geom_sankey(flow.alpha = 0.5, color = "gray30",show.legend = TRUE) +
  geom_sankey_label(size = 3,color = "black",fill = "white",hjust = 0.2) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "High Grade Students Distribution Across All Four Study Factors") +
  labs(fill = 'Nodes') +
  scale_fill_manual(values = c(
    'Group' = 'blue',
    'High Hours' = 'blue',
    'High Education' = 'blue',
    'High' = 'blue',
    'Always' = 'blue'
  )) +
  theme(plot.title = element_text(hjust = 0.5,family = "font",face = "bold"))

#4.0 	External Factors


#4.1	Investigating Notes Taking Habits, Has Partner and Scholarship Type in High Grade Students

#External Factor 1 & 2 : Having a Partner and ScholarShip Type
#Investigate Always taking class notes,having a partner and scholarship type
View(student_data)

#Replace values and Renaming Partner Column
student_data <- student_data %>%
  mutate(HasAPartner = case_when(
    HasAPartner %in% c(1) ~ "Yes",
    HasAPartner %in% c(2) ~ "No"
  ))

#Replace values and Renaming Partner Column  
student_data = student_data %>% 
  rename(Scholarship_Type = SCHOLARSHIP) %>%
  mutate(Scholarship_Type = case_when(
    Scholarship_Type %in% c(1) ~ "None",
    Scholarship_Type %in% c(2) ~ "25%",
    Scholarship_Type %in% c(3) ~ "50%",
    Scholarship_Type %in% c(4) ~ "75%",
    Scholarship_Type %in% c(5) ~ "Full"
))

#Filter for High Grade Students Only
conclusion1 = student_data %>%
  filter(GRADE_LEVEL == "High") %>%
  select(GRADE_LEVEL,HasAPartner,Scholarship_Type,CLASS_NOTE_TAKING)

#Conversion 
conclusion1 = conclusion1 %>%
  make_long(CLASS_NOTE_TAKING,Scholarship_Type,HasAPartner,GRADE_LEVEL)
set.seed(111)

TotalCount1 <- nrow(student_data)

# Create the aggregated data
conclusion1_agg <- conclusion1 %>%
  group_by(node) %>%
  tally()
# Calculate percentages
conclusion1_agg <- conclusion1_agg %>%
  mutate(pct = n / TotalCount1)
# Merge aggregated data with the original data
conclusion1_values <- merge(conclusion1, conclusion1_agg,
                            by.x = 'node', by.y = 'node', all.x = TRUE)

# Create a Sankey diagram
sankey_plot <- ggplot(conclusion1_values, aes(
  x = x, next_x = next_x, node = node, next_node = next_node,
  fill = factor(node),
  label = paste0(node, " n= ", n, ' (', round(pct * 100, 1), '%)')
)) +
  geom_sankey(
    flow.alpha = 0.5,
    color = "gray40",
    show.legend = TRUE
  ) + geom_sankey_label(size = 3,color = "black",fill = "white",hjust = 0.2) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "High Grade Students Distribtion across Notes Taking Habits,Has A Partner and ScholarShip Type") +
  labs(fill = "Nodes") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))

# Display the Sankey diagram
print(sankey_plot)


#4.2	Investigating High Grade Students Who Always Take Notes with Attendance to Classes and Listening to Class Habit

#External Factor 3 & 4 : Listening to Class & Attendance to Classses
#Investigating the relationship of students who has high grade and always take class notes with listening  to class and attendance to classes

#Replace values and Renaming LISTENS Column
student_data <- student_data %>%
  rename(Listening_In_Class = LISTENS) %>%
mutate(Listening_In_Class = case_when(
  Listening_In_Class %in% c(1) ~ "Never",
  Listening_In_Class %in% c(2) ~ "Sometimes",
  Listening_In_Class %in% c(3) ~ "Always"
))

#Replace values and Renaming ATTEND column
student_data = student_data %>%
  rename(Attendance_to_Classes = ATTEND) %>%
  mutate(Attendance_to_Classes = case_when(
    Attendance_to_Classes %in% c(1) ~ "Always",
    Attendance_to_Classes %in% c(2) ~ "Sometimes",
    Attendance_to_Classes %in% c(3) ~ "Never"
  ))

View(student_data)
#Filtering to Include Only High Grade Students with Always Class Note Taking 
conclusion2 = student_data %>%
  filter(GRADE_LEVEL == "High" & CLASS_NOTE_TAKING == "Always") %>%
  group_by(Attendance_to_Classes,Listening_In_Class) %>%
  summarize(count = n())
conclusion2

# vector consisting of colors for bars 
custom_colors <- c("lightblue", "lightcoral")

# Create the bar chart with custom colors
conclusion2_chart <- ggplot(conclusion2, aes(
  x = Listening_In_Class,
  y = count,
  fill = factor(Attendance_to_Classes)
)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Specify custom colors
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Distribution of High Grade Students who Always Take \nClass Notes 
    with Listening to Classes and Attending Classes",
    x = "Listening In Class",
    y = "Number of Students",
    fill = "Attendance to Classes"
  ) +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_minimal()

# Display the bar chart
print(conclusion2_chart)

# Display the bar chart
print(conclusion2_chart)
