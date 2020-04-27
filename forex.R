#https://blog.quantinsti.com/machine-learning-application-forex-markets-working-models/

#how to model divergences (price making lower low, buy indicators making a higher low)

library(pracma)
library(TTR)
library(e1071)

dk_golf<-read.csv('~/forex.csv', stringsAsFactors = FALSE)
eur <- dk_golf[dk_golf$Currency=="EUR",1:2]
eur$DayChange <- c(NA,diff(eur$Rate))
eur <- eur[!is.na(eur$DayChange),]
eur$PctChange <- eur$DayChange/(eur$Rate-eur$DayChange)
eur$UpDown <- (eur$DayChange>0)*1
fibs <- c(2,3,5,8,13,21,34,55,89,144)
for (fib in fibs) {
  eur[,1+ncol(eur)] <- c(NA,head(movavg(eur$Rate,fib, type="e"),-1))  #creating a lag to avoid look ahead bias, therefore all indicators will be previous days indicators
  names(eur)[ncol(eur)] <- paste0("ema",fib)
}
macd_12_26_9 <- MACD(eur$Rate, 12,26,9,type="EMA", percent = FALSE)[,1]-MACD(eur$Rate, 12,26,9,type="EMA", percent = FALSE)[,2]
macd_8_17_9 <- MACD(eur$Rate, 8,17,9,type="EMA", percent = FALSE)[,1]-MACD(eur$Rate, 8,17,9,type="EMA", percent = FALSE)[,2]
macd_3_10_16 <- MACD(eur$Rate, 3,10,16,type="EMA", percent = FALSE)[,1]-MACD(eur$Rate, 3,10,16,type="EMA", percent = FALSE)[,2]
RSI <- RSI(eur$Rate, matype="EMA")
eur$Previous_Close <- c(NA, head(eur$Rate,-1))
eur$macd_12_26_9 <- c(NA, head(macd_12_26_9 ,-1))  #creating a lag to avoid look ahead bias
eur$macd_8_17_9  <- c(NA, head(macd_8_17_9,-1))
eur$macd_3_10_16 <- c(NA, head(macd_3_10_16,-1))
eur$RSI <- c(NA, head(RSI,-1))

eur$macd_12_26_9_ind <- (eur$macd_12_26_9 > 0)*1
eur$macd_8_17_9_ind <- (eur$macd_8_17_9 > 0)*1
eur$macd_3_10_16_ind <- (eur$macd_3_10_16 > 0)*1

eur$rsi_ind <- (eur$RSI> 70)*1
eur$rsi_ind[eur$RSI< 30] <- -1

for (fib1 in fibs[1:(-1+length(fibs))]) {
  eur[,1+ncol(eur)] <- (eur[,paste0("ema",fib1)] > eur$Previous_Close)*1
  eur[,1+ncol(eur)] <- (eur[,paste0("ema",fib1)] < eur$Previous_Close)*1
  names(eur)[ncol(eur)-1] <- paste0(fib1,"over_Close")
  names(eur)[ncol(eur)] <- paste0(fib1,"under_Close")
  for (fib2 in fibs[(which(fib1 == fibs)+1):length(fibs)]) {
    eur[,1+ncol(eur)] <- (eur[,paste0("ema",fib1)] > eur[,paste0("ema",fib2)])*1
    eur[,1+ncol(eur)] <- (eur[,paste0("ema",fib1)] < eur[,paste0("ema",fib2)])*1
    names(eur)[ncol(eur)-1] <- paste0(fib1,"over",fib2)
    names(eur)[ncol(eur)] <- paste0(fib1,"under",fib2)
  }
}
eur[,1+ncol(eur)] <- (eur[,paste0("ema",fib2)] > eur$Previous_Close)*1
eur[,1+ncol(eur)] <- (eur[,paste0("ema",fib2)] < eur$Previous_Close)*1
names(eur)[ncol(eur)-1] <- paste0(fib2,"over_Close")
names(eur)[ncol(eur)] <- paste0(fib2,"under_Close")

distance_names <- c(grep("ind",names(eur)),grep("over",names(eur)),grep("under",names(eur)))
for (dn in distance_names) {
  val <- NULL
  tmp <- eur[,dn]
  na_count <- length(tmp[is.na(tmp)])
  tmp <- tmp[(1+na_count):length(tmp)]
  #record distance from cross
  for (i in 1:length(rle(tmp)$lengths)) {
    if (rle(tmp)$values[i]==1) {
      val <- c(val,seq(rle(tmp)$lengths[i]))
    } else {
      val <- c(val,rep(0,rle(tmp)$lengths[i]))
    }
  }
  eur[,1+ncol(eur)] <- c(rep(NA,na_count),val)
  names(eur)[ncol(eur)] <- paste0(names(eur)[dn],"_streak")
}

eur_clean <- eur[max(fibs):nrow(eur),]

drop_cols <- c("Date..GMT.", "Rate", "DayChange" , "PctChange")
model_data <- eur[!(names(eur) %in% drop_cols)]
breakpoint <- nrow(model_data) * 0.7
train_data <- model_data[1:breakpoint,]
test_data <- model_data[(1+breakpoint):nrow(model_data),]
SVM = svm(UpDown~., data=train_data, kernel="radial", cost=1, gamma=1/2)
train_predictions <- predict(SVM, train_data, type="class")
preds <- as.data.frame(train_predictions)
preds$id <- rownames(preds)
train_data$id <- rownames(train_data)
train_preds <- merge(train_data, preds, by="id")
train_preds$prediction <- 1*(train_preds$train_predictions>0.5)
accuracy=(sum(train_preds$prediction==train_preds$UpDown)/nrow(train_preds))*100

test_preds <-  predict(SVM, test_data, type="class")
test_preds <- as.data.frame(test_preds)
test_preds$id <- rownames(test_preds)
test_data$id <- rownames(test_data)
test_preds <- merge(test_preds, test_data, by="id")
test_preds$prediction <- 1*(test_preds$test_preds>0.5)
accuracy=(sum(test_preds$prediction==test_preds$UpDown)/nrow(test_preds))*100

