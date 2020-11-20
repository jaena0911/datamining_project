# 라이브러리 import
library(ggplot2) # 시각화
library(dplyr) # groupby 메소드
library(caret) # one hot encoding 전처리 기법
library(randomForest) # 랜덤포레스트
library(ROCR) # roc auc 평가지표
setwd('C:\\Users\\dswook\\datamining_project\\data')
psycho <- read.csv('train.csv')
head(psycho)

str(psycho)


# nerdiness 분포확인
counts <- table(psycho$nerdiness)
barplot(counts, xlab='is nerdiness')

# 성별은 중요한 것이 아니니 날리자
table(psycho$gender)
group_gender <- psycho %>%
  group_by(gender) %>%
  summarise(mean_nerdiness=mean(nerdiness))
group_gender

# age 이상치 대체
boxplot(psycho$age)
quantile(psycho$age)
psycho$age <- ifelse(psycho$age > 27, NA, psycho$age)
psycho$age <- ifelse(is.na(psycho$age), 20, psycho$age)

# education 
table(psycho$education)
psycho$education <- ifelse(psycho$education == 0, 1, psycho$education)
group_education <- psycho %>%
  group_by(education) %>%
  summarise(mean_nerdiness=mean(nerdiness))
group_education

# familysize -> 대가족인지 핵가족인지 구별하는 feature 생성도 생각해보자
table(psycho$familysize)
quantile(psycho$familysize)
psycho$familysize <- ifelse(psycho$familysize > 14, NA, psycho$familysize)
psycho$familysize <- ifelse(is.na(psycho$familysize), 2, psycho$familysize)
barplot(table(psycho$familysize))


# urban
table(psycho$urban)
psycho$urban <- ifelse(psycho$urban == 0, 2, psycho$urban)
group_urban <- psycho %>%
  group_by(urban) %>%
  summarise(mean_nerdiness=mean(nerdiness))
group_urban

# hand -> 오른손잡이는 중요한것이 아니니 날리자
table(psycho$hand)

# 성적취향
table(psycho$orientation)
psycho$orientation <- ifelse(psycho$orientation == 0, 1, psycho$orientation)
group_orientation <- psycho %>%
  group_by(orientation) %>%
  summarise(mean_nerdiness=mean(nerdiness))
group_orientation

# label encoding 기법 활용하기
factors <- factor(psycho$country)
psycho$country <- as.integer(factors)

factors <- factor(psycho$major)
psycho$major <- as.integer(factors)



# train test split
set.seed(2020)
train_idx <- sample(1:nrow(psycho), size=0.7*nrow(psycho), replace=F)
test_idx <- (-train_idx)
X_train <- psycho[train_idx,]
X_test <- psycho[test_idx,]
y_test <- psycho$nerdiness[test_idx]
X_train <- select(X_train, -c('index', 'hand', 'gender'))
X_test <- select(X_test, -c('index', 'hand', 'gender', 'nerdiness'))

# 모델링
rf.fit <- randomForest(nerdiness ~ ., data=X_train, ntree=50, importance=T)
y_pred <- predict(rf.fit, X_test, type='response') # 확률값 예측
pr <- prediction(y_pred, y_test)
prf <- performance(pr, measure='tpr', x.measure = 'fpr')
plot(prf, main='ROC Curve')
auc <- performance(pr, measure = 'auc')
auc <- auc@y.values[[1]]
auc

