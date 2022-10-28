myName <- "JiajunChen"
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
#Warm Up
#1
print_order <- function(x1){
  if(x1[1] < x1[2] | x1[1] < x1[3] ){
    x2 <- c(max(x1),median(x1),min(x1))
  print(x2)
  }
  
}

#2
print_string <- function(x){
  for(i in 1:x){
    if((i%%3 ==0 | i%%5 == 0) |(i%%3 ==0 & i%%5 ==0)){
      }
      else {
        print(i)
      }
    if(i%%3 ==0){
      print("Yes")
    }
    if(i%%5 ==0){
      print("No")
    }
    if(i%%3 ==0 & i%%5 ==0){
      print("Unknown")
    }
    }
  }

#3
calc_sum_of_factor <- function(x){
  
  
}

#4
find_intersect <- function(x,y,z){
 
}

#5

factorial_base <- function(x){
  s<-numeric()
  s[1]=1
  for(i in 2:x){
    s[i]=i*s[i-1]
    return(s[x])
  }
}

#6
T <- function(n){
  2*n = x*(x+1) 
  print(x)
}

  
 
#H-1B
h1b_2022 <- read_csv("https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv")

na_num <- sum(is.na(h1b_2022))


hib_2022a <- h1b_2022 %>% na.omit() %>% filter(City != "-",State != "-")

df_num <- hib_2022a %>% select(State,`Initial Approval`,`Initial Denial`,`Continuing Approval`,`Continuing Denial`,NAICS) %>% group_by(State) %>% 
  summarise(
    `Initial Approval` = sum(`Initial Approval`),
    `Initial Denial` = sum(`Initial Denial`),
    `Continuing Approval` = sum(`Continuing Approval`),
    `Continuing Denial` = sum(`Continuing Denial`)
    
  ) %>% 
  transmute(State = State, `Init App` = `Initial Approval` +`Initial Denial`, `Conti App` = `Continuing Approval` +  `Continuing Denial`,
            `Approve` = `Initial Approval`, `Denial` = `Initial Denial`)

app_num <- sum(df_num$Approve)
den_num <- sum(df_num$Denial)

city_num <- hib_2022a %>% select(City,State,`Initial Approval`,`Initial Denial`,`Continuing Approval`,`Continuing Denial`,NAICS) %>% group_by(City) %>% 
  summarise(
    `Initial Approval` = sum(`Initial Approval`),
    `Initial Denial` = sum(`Initial Denial`),
    `Continuing Approval` = sum(`Continuing Approval`),
    `Continuing Denial` = sum(`Continuing Denial`)
    
  ) %>% 
  transmute(City = City, `Init App` = `Initial Approval` +`Initial Denial`, `Conti App` = `Continuing Approval` +  `Continuing Denial`,
            `Approve` = `Initial Approval`, `Denial` = `Initial Denial`) %>% transmute(City = City, Count =`Approve` +  `Denial`)

visa_num <- hib_2022a %>% select(State,`Initial Approval`,`Initial Denial`,`Continuing Approval`,`Continuing Denial`,NAICS) %>% group_by(NAICS) %>% 
  summarise(
    `Initial Approval` = sum(`Initial Approval`),
    `Initial Denial` = sum(`Initial Denial`),
    `Continuing Approval` = sum(`Continuing Approval`),
    `Continuing Denial` = sum(`Continuing Denial`)
    ) %>% 
  transmute(NAICS =NAICS,`Init App` = `Initial Approval` +`Initial Denial`, `Conti App` = `Continuing Approval` +  `Continuing Denial`,
            `Approve` = `Initial Approval`, `Denial` = `Initial Denial`) %>% 
  transmute(NAICS = NAICS, Number =`Init App`+`Conti App`,percentage = Number/sum(Number)*100)
 
   






