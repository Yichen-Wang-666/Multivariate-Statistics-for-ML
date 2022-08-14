#load relevant packages
library(readr)
library(dplyr)
library(skimr)

#import data
facebook_data <- read.csv("C:/YICHEN WANG/Course/2022 WINTER/MRKT 671/ASSIGNMENT/social/cdf_train.csv")

## Preprocessing

## drop column
facebook_data = subset(facebook_data,select = -c(Page.Name,User.Name,Facebook.Id,Page.Description,Post.Created,Link,Final.Link,URL,Link.Text,Description,Sponsor.Id,Sponsor.Name,Sponsor.Category))

##drop na col
facebook_data = subset(facebook_data, select = -c(Followers.at.Posting,Video.Share.Status,Is.Video.Owner.,Video.Length,Image.Text))

## drop col
facebook_data = subset(facebook_data, select = -c(Page.Admin.Top.Country,Page.Created,Post.Created.Date))

## drop row
facebook_data[facebook_data$Type == 0,]
facebook_data[-c(6869),]

facebook_data <- facebook_data[!(facebook_data$Type == 0 |facebook_data$Type == '11:20:00'|
                                 facebook_data$Type == ''|facebook_data$Type == '12:21:00'|
                                   facebook_data$Type == '13:30:00'|facebook_data$Type == '17:30:00'|
                                   facebook_data$Type == '17:30:00'|facebook_data$Type == '20:15:03'|
                                   facebook_data$Type == '20:15:03'|facebook_data$Type == '22:30:04'|
                                   facebook_data$Page.Category == ''),]

## create factor vars
facebook_data <- facebook_data %>%
  mutate_at(vars(Type,Page.Category),
            as.factor)

## delete
facebook_data = subset(facebook_data, select = -c(Message))

## create time categorical variables
library(dplyr)
facebook_data = facebook_data %>% mutate(daytime_post = case_when(
  Post.Created.Time <= "06:00:00" ~ 'midnight', Post.Created.Time <= '12:00:00' ~ 'morning',
  Post.Created.Time <= '18:00:00' ~ 'afternoon', TRUE ~ 'evening'))

facebook_data <- facebook_data %>%
  mutate_at(vars(daytime_post),
            as.factor)
## description data

## wc variable cleaning
facebook_data <- facebook_data[!(facebook_data$WC == 'HOME_SITE' |facebook_data$WC == 'PERSONAL_INJURY_LAWYER'|
                                   facebook_data$WC == ''|facebook_data$WC == 'TOPIC_DOCTOR'),]

facebook_data <- facebook_data %>%
  mutate_at(vars(WC,Dash),
            as.numeric)
## detect highly correlated vars
library("imputeTS")
num_vars = facebook_data[,c(18:110)]
num_vars <- na.replace(num_vars, 0)
sum(is.na(num_vars))

matriz_cor <- cor(num_vars)
matriz_cor

for (i in 1:nrow(matriz_cor)){
  correlations <-  which((matriz_cor[i,] > 0.7) & (matriz_cor[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(num_vars)[i])
    print(correlations)
  }
}

## drop column
facebook_data = subset(facebook_data,select = -c(Clout,Authentic,Tone,Dic,pronoun,
                                                 posemo,percept,informal,AllPunc))

facebook_data$y = facebook_data$Comments+facebook_data$Likes+facebook_data$Shares
 
facebook_data = na.omit(facebook_data)

write.csv(facebook_data, file = "C:/YICHEN WANG/Course/2022 WINTER/MRKT 671/ASSIGNMENT/social/facebook_data.csv")

x = facebook_data[,c(18:101)]
#x$y = facebook_data$Total.Interactions
#x$daytime_post = facebook_data$daytime_post
#x$type = facebook_data$Type
#x$category = facebook_data$Page.Category
#x$likes_at_posting = facebook_data$Likes.at.Posting

#x <- x %>%
#  mutate_at(vars(likes_at_posting),as.numeric)

#a = lm(y~.-y,data = x)

## pca
library(BBmisc)
x= normalize(x, method = "standardize")
pca=prcomp(x, scale=TRUE)
pve=(pca$sdev^2)/sum(pca$sdev^2)

pca_data = pca$x[,1:45]
# dataframe


pca_data = data.frame('1' = pca$x[,1])

for (i in 2:45){
  name = as.character(i)
  new_df = data.frame(name = pca$x[,i])
  pca_data = cbind(pca_data, new_df)
}

name_lis = as.character(c(1:45))
colnames(pca_data) = name_lis

pca_data$daytime_post = facebook_data$daytime_post
pca_data$type = facebook_data$Type
pca_data$category = facebook_data$Page.Category
pca_data$likes_at_posting = facebook_data$Likes.at.Posting
pca_data$y = facebook_data$Total.Interactions

pca_data <- pca_data %>%
    mutate_at(vars(likes_at_posting),as.numeric)

pca_data <- pca_data %>%
  mutate_at(vars(y),as.numeric)
library(boot)
fit=glm(y~.-y,data = pca_data)
mse=cv.glm(pca_data, fit, K=5)$delta[1]
mse

a = lm(y~.-y,data = pca_data)
