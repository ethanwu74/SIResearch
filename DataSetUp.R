#initialize the data set
yrbssTemp <- read.csv("C:/Users/User/OneDrive/Desktop/yrbss.csv")
#removes all the data from before 2019
yrbss = subset(yrbssTemp, yrbssTemp$year == 2019) 
yrbss = within(yrbss, rm(qnfrcgr, qntb2, qntb3, qntb4))
View(yrbss)
#package for ordinal logistic regression function
install.packages("MASS")
#package for data visualization
install.packages("reshape2")
lapply(yrbss[, c("q26", "q27", "q28")], table)
x = c()
#loop through every single row in the data set
for(i in 1:56770) {
  #categorizes every cell into 5 levels of suicidal ideation based on question results
  if(is.na(yrbss[i, 48]) || is.na(yrbss[i, 47])|| is.na(yrbss[i, 46])) {
    x = append(x, "1")
    next
  }
  else {
    if(yrbss[i, 48]==2) {
      x = append(x, "3")
    } 
    else if(yrbss[i, 48] == 3) {
      x = append(x, "4")
    }
    else if(yrbss[i, 48] == 4 || yrbss[i, 48] == 5) {
      x = append(x, "5")
    }
    else {
      if(yrbss[i, 47] == 1) {
        x = append(x, "2")
      }
      else {
        x = append(x, "1")
      }
    }
  }
}
#added the new recategorized SI variable into the data set
x= as.factor(x)
yrbss = cbind(yrbss, x)



