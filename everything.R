#initialize the data set
yrbssTemp <- read.csv("C:/Users/User/OneDrive/Desktop/yrbss.csv")
#removes all the data from before 2019
yrbss = subset(yrbssTemp, yrbssTemp$year == 2019) 
yrbss = within(yrbss, rm(qnfrcgr, qntb2, qntb3, qntb4))
View(yrbss)
#package for ordinal logistic regression function
install.packages("MASS")
install.packages("Hmisc")
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
avgInc = c()
inc = c()
lapply(yrbss["sitename"], table)
for(i in 1:56770) {
  if(yrbss$sitename[i] == "Albuquerque, NM (AB)") {
    avgInc = append(avgInc, 56366)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Borough of Brooklyn, NY (NYG)") {
    avgInc = append(avgInc, 67567)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Borough of Bronx, NY (NYH)") {
    avgInc = append(avgInc, 43011)
    inc = append(inc, "low")
  }
  else if(yrbss$sitename[i] == "Borough of Manhattan, NY (NYI)") {
    avgInc = append(avgInc, 84435)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Borough of Queens, NY (NYJ)") {
    avgInc = append(avgInc, 73262)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Borough of Staten Island, NY (NYK)") {
    avgInc = append(avgInc, 86054)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Broward County, FL (FT)") {
    avgInc = append(avgInc, 64522)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Chicago, IL (CH)") {
    avgInc = append(avgInc, 65781)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Cleveland, OH (CE)") {
    avgInc = append(avgInc, 33678)
    inc = append(inc, "low")
  }
  else if(yrbss$sitename[i] == "Duval County, FL (DU)") {
    avgInc = append(avgInc, 59541)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Eaton Consortium, MI (EA)") {
    avgInc = append(avgInc, 72173)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Fort Worth, TX (FW)") {
    avgInc = append(avgInc, 67927)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Gaston County, NC (GS)") {
    avgInc = append(avgInc, 56819)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Genesee Consortium, MI (GE)") {
    avgInc = append(avgInc, 54052)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Hillsborough County, FL (HL)") {
    avgInc = append(avgInc, 64164)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Los Angeles, CA (LO)") {
    avgInc = append(avgInc, 69778)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "New York City, NY (NYC)") {
    avgInc = append(avgInc, 70663)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Newark, NJ (NW)") {
    avgInc = append(avgInc, 41,335)
    inc = append(inc, "low")
  }
  else if(yrbss$sitename[i] == "Oakland, CA (OA)") {
    avgInc = append(avgInc, 85,628)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Orange County, FL (OL)") {
    avgInc = append(avgInc, 65,784)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Palm Beach County, FL (PB)") {
    avgInc = append(avgInc, 68,874)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Pasco County, FL (PS)") {
    avgInc = append(avgInc, 58,084)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Philadelphia, PA (PH)") {
    avgInc = append(avgInc, 52,649)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Portland, OR (PO))") {
    avgInc = append(avgInc, 78,476)
  }
  else if(yrbss$sitename[i] == "San Diego, CA (SA)") {
    avgInc = append(avgInc, 89,457)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "San Francisco, CA") {
    avgInc = append(avgInc, 126,187)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Seattle, WA (SE)") {
    avgInc = append(avgInc, 105,391)
    inc = append(inc, "high")
  }
  else if(yrbss$sitename[i] == "Shelby County, TN (ST)") {
    avgInc = append(avgInc, 55,015)
    inc = append(inc, "mid")
  }
  else if(yrbss$sitename[i] == "Spartanburg County, SC (SP)") {
    avgInc = append(avgInc, 57,627)
    inc = append(inc, "mid")
  }
  else {
    avgInc = append(avgInc, 0)
    inc = append(inc, "low")
  }
  
}
inc = as.factor(inc)
yrbss = cbind(yrbss, inc)
require(MASS)
m = polr(as.factor(x) ~ as.factor(inc) + as.factor(sexid2) + as.factor(sexpart)
+ weight + as.factor(sex) + as.factor(qntb4) + as.factor(race7) + as.factor(grade) 
,data = yrbss, Hess = TRUE)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
confint.default(m)


