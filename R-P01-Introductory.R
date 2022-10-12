
# R-P01-Introductory

# Step 1: Set Work Directory

setwd("E:/Mine/Maktab Khooneh/Data Analysis/R - Beginner/Final Project")
getwd()


# Step 2: Import Data

df = read.csv("DirectMarketing.csv", header = T)
head(df)


# Step 3: Answering Questions
    # Question 1: Summary

summary(df)


    # Question 2: Female Percentage

sum(is.na(df$Gender))
        # There is no NA in Gender column

f_count = sum(df$Gender == "Female")
m_count = sum(df$Gender == "Male")
f_perc = 100*f_count/(f_count + m_count)
f_perc
        # 50.6% of customers are females


    # Question 3: Percentage of married males with +50k salary

mm_count = sum(df$Gender == "Male" & df$Married == "Married")
mm50_count = sum(df$Gender == "Male" & df$Married == "Married" & df$Salary > 50000)
mm50_perc = 100*mm50_count/mm_count
mm50_perc
        #87.36% of married males gain over 50k


    #Question 4: Bar chart - Age

oa = table(df$Age)
oa = as.data.frame(oa)
class(oa)
colnames(oa) = c("Age_Category","Count")
oa = oa[order(oa$Count),]

library(ggplot2)

ggplot(oa,aes(x = reorder(Age_Category,Count),y = Count)) +
  geom_bar(stat = "identity",fill=rainbow(3)) +
  ggtitle("Age Distribution") +
  xlab("Age Category") +
  ylim(0,600)


    # Question 5: Stacked Bar Chart - Age

oa2 = table(df$Age,df$Gender)
oa2 = oa2[order(oa2[,1]+oa2[,2]),]
barplot(t(oa2),col=c("Green","Red"),legend = (colnames(oa2)),ylim = c(0,600),main = "Distribution by Age and Gender",xlab = "Age Category")


    # Question 6: Density Chart - AmountSpent

as = df$AmountSpent
hist(as, freq = F,breaks = 50,col = "Sky Blue", xlab = "Record", ylab = "Spent Amount",main = "Spent Amount in $")

dx = seq(min(as),max(as),length = 50)
dy = dexp(dx,rate = 1/mean(as))
lines(dx,dy,col = "red",lwd=3)


    # Question 7: Box Chart - AmountSpent ~ Catalogs

boxplot(df$AmountSpent ~ df$Catalogs,ylab = "Spent Amount in $", xlab = "Number of Catalogs", main = "Box Chart - Spent Amount in $ over Number of Catalogs",col=df$Catalogs)


    # Question 8: Dot chart - Salary vs. AmountSpent

ggplot(df,aes(Salary,AmountSpent)) +
  geom_point(size = 1, col = "blue") +
  geom_smooth(formula = y~x, method = lm, col = "red", lwd = 2, se = F) +
  ggtitle("Salary vs. Spent Amount in $")


    # Question 9: AmountSpent vs. Gender
        # A: Box Chart - Gendernt vs. AmountSpe

asm = df[-which(df$Gender == "Female"),10]
asf = df[-which(df$Gender == "Male"),10]

ggplot(df, aes(Gender,AmountSpent,fill = Gender)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(asm)),col = "Blue", size = 2,linetype = "dashed") +
  geom_hline(aes(yintercept = mean(asf)),col = "Red",size = 2,linetype = "dashed") +
  ylab("Spent Amount in $") +
  ggtitle("Spent Amount in $ vs. Gender")
            # It seems males are more inclined toward paying more compared to women.  


        # B: Box Chart - Gender vs. Salary

scm = df[-which(df$Gender == "Female"),]
scf = df[-which(df$Gender == "Male"),]


ggplot(df, aes(Gender,Salary,fill = Gender)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(scm[,6])),col = "Blue", size = 2,linetype = "dashed") +
  geom_hline(aes(yintercept = mean(scf[,6])),col = "Red",size = 2,linetype = "dashed") +
  ylab("Salary in $") +
  ggtitle("Salary in $ vs. Gender")
            # According to the Chart, there is a considerable difference in males and females' income. I think more analysis will be needed.


        # C: Box Chart - Gender vs. Salary

quantile(df$Salary,0.5)
df$sc = ifelse(df$Salary > 53700, "High Income","Low Income")

lowi = df[df$sc == "Low Income",]
higi = df[df$sc == "High Income",]

ggplot(lowi, aes(Gender,Salary,fill = Gender)) +
  geom_boxplot() +
  stat_summary(fun = mean,geom = "Point", size = 2)
            # The chart has illustrated that there is a slight difference between males and females (about 5k) among the Low Income ones.

ggplot(higi, aes(Gender,Salary,fill = Gender)) +
  geom_boxplot() +
  stat_summary(fun = mean,geom = "Point", size = 2)
            # In the High Income portion, it is obvious that males and females salaries are almost equal (despite the little distribution difference) and it seems to have no impact on the main question.
            # Conclusion: Although Low Income females gain nearly 5k less than males, High Income ones' salaries are almost identical. It may have a slight impact on their inclination to spend money. However, I reckon it cannot be the only reason for the difference and genders' biases and characteristics can play a role here.
