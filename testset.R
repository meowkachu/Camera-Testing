#set dates
start_date <- as.Date("2020/08/10")
first_date <- rep(start_date, each = 2880)
next_date <- as.Date("2020/08/11")
second_date <- rep(next_date, each = 2880)
logData <- data.frame(date = c(first_date, second_date))

#08/10
seqmin <- rep(0:59, each = 2)
#set.seed(100)
#rndsec1 <- sample(0:59, 120, replace = T)
sec <- c(29,59)
sechr <- rep(0:23, each = 120)
tempdata1 <- data.frame(hour = sechr, min = seqmin, sec = sec)
#write.csv(tempdata1,"tempdata1.csv")
#data1 <- read.csv("tempdata1.csv")
excTime1 <- chron(time = paste(tempdata1$hour, ':', tempdata1$min, ':', tempdata1$sec))

#08/11
tempdata2 <- data.frame(hour = sechr, min = seqmin, sec = sec)
excTime2 <- chron(time = paste(tempdata2$hour, ':', tempdata2$min, ':', tempdata2$sec))

logData$time <- c(excTime1,excTime2)

#result
set.seed(100)
result_prob <- c(0.7, 0.1, 0.1, 0.1)
result <- as.factor(sample(c("Normal", "Alarm", "Ignore", "Recheck"), 5760, replace = TRUE, prob = result_prob))
logData$result <- result

#camera
camera <- as.factor(sample(c("Camera_1", "Camera_2", "Camera_3", "Camera_4"), 5760, replace = TRUE))
logData$camera <- camera

#detection
detection <- as.factor(sample(c("First_check", "Second_check"), 5760, replace = TRUE))
logData$detection <- detection

#Daily total
daily_total <- rep(1:2880)
logData$dailyTotal <- daily_total

#NG
ng <- "initialize"
for(i in 1:5760){
  if(logData[i,3] == 'Normal'){
    ng[i] <- 0} 
  else if(logData[i,3] == 'Alarm'){
    ng[i] <- 1}
  else if(logData[i,3] == 'Recheck'){
    ng[i] <- 1}
  else if(logData[i,3] == 'Ignore'){
    ng[i] <- 0
  }
}

logData$NG <- ng

camera1NG <- "initialize"
camera2NG <- "initialize"
camera3NG <- "initialize"
camera4NG <- "initialize"
for(k in 1:5760){
  if(logData[k,3] == 'Normal' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 0
  }
  else if(logData[k,3] == 'Alarm' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 1
  }
  else if(logData[k,3] == 'Recheck' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 1
  }
  else if(logData[k,3] == 'Ignore' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 0
  }
  else{
    camera1NG[k] <- 0
  }
}
for(k in 1:5760){
  if(logData[k,3] == 'Normal' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 0
  }
  else if(logData[k,3] == 'Alarm' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 1
  }
  else if(logData[k,3] == 'Recheck' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 1
  }
  else if(logData[k,3] == 'Ignore' && logData[k,4] == 'Camera_1'){
    camera1NG[k] <- 0
  }
  else{
    camera1NG[k] <- 0
  }
}

for(k in 1:5760){
if(logData[k,3] == 'Normal' && logData[k,4] == 'Camera_2'){
  camera2NG[k] <- 0
}
else if(logData[k,3] == 'Alarm' && logData[k,4] == 'Camera_2'){
  camera2NG[k] <- 1
}
else if(logData[k,3] == 'Recheck' && logData[k,4] == 'Camera_2'){
  camera2NG[k] <- 1
}
else if(logData[k,3] == 'Ignore' && logData[k,4] == 'Camera_2'){
  camera2NG[k] <- 0
}
else{
  camera2NG[k] <- 0
}
}

for(k in 1:5760){
  if(logData[k,3] == 'Normal' && logData[k,4] == 'Camera_3'){
    camera3NG[k] <- 0
  }
  else if(logData[k,3] == 'Alarm' && logData[k,4] == 'Camera_3'){
    camera3NG[k] <- 1
  }
  else if(logData[k,3] == 'Recheck' && logData[k,4] == 'Camera_3'){
    camera3NG[k] <- 1
  }
  else if(logData[k,3] == 'Ignore' && logData[k,4] == 'Camera_3'){
    camera3NG[k] <- 0
  }
  else{
    camera3NG[k] <- 0
  }
}


for(k in 1:5760){
  if(logData[k,3] == 'Normal' && logData[k,4] == 'Camera_4'){
    camera4NG[k] <- 0
  }
  else if(logData[k,3] == 'Alarm' && logData[k,4] == 'Camera_4'){
    camera4NG[k] <- 1
  }
  else if(logData[k,3] == 'Recheck' && logData[k,4] == 'Camera_4'){
    camera4NG[k] <- 1
  }
  else if(logData[k,3] == 'Ignore' && logData[k,4] == 'Camera_4'){
    camera4NG[k] <- 0
  }
  else{
    camera4NG[k] <- 0
  }
}

firstCheckNG <- "initialize"
secondCheckNG <- "initialize"
for(k in 1:5760){
  if(logData[k,3] == 'Normal' && logData[k,5] == 'First_check'){
    firstCheckNG[k] <- 0
  }
  else if(logData[k,3] == 'Alarm' && logData[k,5] == 'First_check'){
    firstCheckNG[k] <- 1
  }
  else if(logData[k,3] == 'Recheck' && logData[k,5] == 'First_check'){
    firstCheckNG[k] <- 1
  }
  else if(logData[k,3] == 'Ignore' && logData[k,5] == 'First_check'){
    firstCheckNG[k] <- 0
  }
  else{
    firstCheckNG[k] <- 0
  }
}

for(k in 1:5760){
  if(logData[k,3] == 'Normal' && logData[k,5] == 'Second_check'){
    secondCheckNG[k] <- 0
  }
  else if(logData[k,3] == 'Alarm' && logData[k,5] == 'Second_check'){
    secondCheckNG[k] <- 1
  }
  else if(logData[k,3] == 'Recheck' && logData[k,5] == 'Second_check'){
    secondCheckNG[k] <- 1
  }
  else if(logData[k,3] == 'Ignore' && logData[k,5] == 'Second_check'){
    secondCheckNG[k] <- 0
  }
  else{
    secondCheckNG[k] <- 0
  }
}

#load 0 & 1s
logData$camera1NG <- camera1NG  
logData$camera2NG <- camera2NG  
logData$camera3NG <- camera3NG  
logData$camera4NG <- camera4NG  
logData$firstCheckNG <- firstCheckNG
logData$secondCheckNG <- secondCheckNG


write.csv(logData, "data.csv")

