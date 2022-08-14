#install.packages("skimr")
#install.packages("readxl")
#load relevant packages
library(readr)
library(dplyr)
library(skimr)
library(readxl)
# MRKT671 code from live Q&A session on Jan 19 2022
# Bruce Doré

# Guide to Variables
# id: customer id number
# trans_after: Number of transactions after the experiment
# revenue_after: Total revenue after the experiment
# minority: demographic -- member of a minority group (1=yes; 0=no)
# female: demographic -- gender (female 1; non-female:0)
# test_coupon: Whether the user received a coupon (1=yes; 2=no)
# channel_acq: Channel of acquisition for the customer when they first signed up to Artea (1: Google 2: Facebook 3: Instagram 4: Referral 5: Other)
# num_past_purch: Number of previous purchases
# spent_last_purchase: Total spent in previous purchase (USD)
# weeks_since_visit: number of weeks since last visit 
# browsing_minutes: Time spend on website in last visit (minutes)
# shopping_cart:  Whether the user added a product to cart but didn't but (1=Yes, 2=NO)

#import data, with all columns imported as character variables
artea_ab_test <- read.csv("/Users/corrine/Desktop/MRKT671/AB_test.csv")

#convert the numeric variables to numeric & factor variables to factors 
artea_ab_test = artea_ab_test %>% 
  mutate_at(vars(trans_after,revenue_after,num_past_purch,spent_last_purchase,
                 weeks_since_visit, browsing_minutes),as.numeric) %>% 
  mutate_at(vars(id,minority,female,test_coupon, channel_acq, shopping_cart),
            as.factor)

#recode the channel_acq factor to set an appropriate baseline
levels(artea_ab_test$channel_acq) <- c("Google", "Facebook", "Instragram","Referral", "Other")

#quick summary of all the variables
head(artea_ab_test)
skim(artea_ab_test)

#histogram of a key variable
hist(artea_ab_test$revenue_after)

#randomization check
# Separate into two groups
no_coupon = artea_ab_test[artea_ab_test[,"test_coupon"] == 0,]
yes_coupon = artea_ab_test[artea_ab_test[,"test_coupon"] == 1,]
# number of observation in group
count(no_coupon)
count(yes_coupon)

count(yes_coupon$id)
## factor variable proportions
## gender
no_coupon %>% group_by(female) %>% summarise(proportion = n()/count(no_coupon))
yes_coupon %>% group_by(female) %>% summarise(proportion = n()/count(yes_coupon))

## channel
no_coupon %>% group_by(channel_acq) %>% summarise(proportion = n()/count(no_coupon))
yes_coupon %>% group_by(channel_acq) %>% summarise(proportion = n()/count(yes_coupon))

## minority
no_coupon %>% group_by(minority) %>% summarise(proportion = n()/count(no_coupon))
yes_coupon %>% group_by(minority) %>% summarise(proportion = n()/count(yes_coupon))

## shopping cart
no_coupon %>% group_by(shopping_cart) %>% summarise(proportion = n()/count(no_coupon))
yes_coupon %>% group_by(shopping_cart) %>% summarise(proportion = n()/count(yes_coupon))

## numerical variable mean and std
## number of purches
mean(no_coupon$num_past_purch)
sd(no_coupon$num_past_purch)

mean(yes_coupon$num_past_purch)
sd(yes_coupon$num_past_purch)

## last purchase spent
mean(no_coupon$spent_last_purchase)
sd(no_coupon$spent_last_purchase)

mean(yes_coupon$spent_last_purchase)
sd(yes_coupon$spent_last_purchase)

## week since last visit
mean(no_coupon$weeks_since_visit)
sd(no_coupon$weeks_since_visit)

mean(yes_coupon$weeks_since_visit)
sd(yes_coupon$weeks_since_visit)

## Browsing Minutes
mean(no_coupon$browsing_minutes)
sd(no_coupon$browsing_minutes)

mean(yes_coupon$browsing_minutes)
sd(yes_coupon$browsing_minutes)

## effectiveness of coupon
no_coupon_rev = sum(no_coupon$revenue_after)
yes_coupon_rev = sum(yes_coupon$revenue_after)

purch_cus_no_coupon = no_coupon %>%filter(trans_after > 0)%>%count()%>%pull()
non_purch_cus_no_coupon = no_coupon %>%filter(trans_after == 0)%>%count()%>%pull()

purch_cus_yes_coupon = yes_coupon %>%filter(trans_after > 0)%>%count()%>%pull()
non_purch_cus_yes_coupon = yes_coupon %>%filter(trans_after == 0)%>%count()%>%pull()

## construct descriptive analysis output
df = data.frame(c(no_coupon_rev,yes_coupon_rev),c(purch_cus_no_coupon,purch_cus_yes_coupon)
                ,c(non_purch_cus_no_coupon,non_purch_cus_yes_coupon))
colnames(df) = c("revenue","number of purchased customer","number of unpurchsed customer")
rownames(df) = c("no coupon","with coupon")

## extract df
#library(gridExtra)
#pdf("/Users/hong/Desktop/McGill/MKT671/question2.pdf", height=11, width=8.5)
#grid.table(df)
#dev.off()

##Q2
## hypothesis testing
prop.test(x = c(purch_cus_yes_coupon,purch_cus_no_coupon),
          n = c(2502, 2498),alternative="greater")

#revenue_diff = mean(yes_coupon$revenue_after)-mean(no_coupon$revenue_after)
t.test(no_coupon$revenue_after,yes_coupon$revenue_after)

#Which customers do you think Artea should be targeting? ('Identify Alternatives + Recommend a Strategy')
rev_no_coupon_ = no_coupon %>%filter(trans_after > 0)%>%count()%>%pull()
non_purch_cus_no_coupon = no_coupon %>%filter(trans_after == 0)%>%count()%>%pull()
t.test(no_coupon$revenue_after,yes_coupon$revenue_after)

##Q3
##hypothesis testing
#gender
#1.2 不是很好讲，可以粗略说一下
#1st female with coupon and female without coupon
rev_no_coupon_female = no_coupon[no_coupon[,"female"] == 1,]$revenue_after
rev_yes_coupon_female = yes_coupon[yes_coupon[,"female"] == 1,]$revenue_after
t.test(rev_no_coupon_female,rev_yes_coupon_female)
#2st male with coupon and male without coupon
rev_no_coupon_male = no_coupon[no_coupon[,"female"] == 0,]$revenue_after
rev_yes_coupon_male = yes_coupon[yes_coupon[,"female"] == 0,]$revenue_after
t.test(rev_no_coupon_male,rev_yes_coupon_male)

##3.4 可以结合起来说，f/m都没coupon的时候购买力没有显著差异，但是两者都有coupon之后，
#female明显比male有更强的购买力
#3rd female with coupon and male with coupon *** significant
#给female发coupon会比给男的发coupon带来更多收益
rev_yes_coupon_female = yes_coupon[yes_coupon[,"female"] == 1,]$revenue_after
rev_yes_coupon_male = yes_coupon[yes_coupon[,"female"] == 0,]$revenue_after
t.test(rev_yes_coupon_female,rev_yes_coupon_male)
#4th female without coupon and male without coupon
rev_no_coupon_female = no_coupon[no_coupon[,"female"] == 1,]$revenue_after
rev_no_coupon_male = no_coupon[no_coupon[,"female"] == 0,]$revenue_after
t.test(rev_no_coupon_female,rev_no_coupon_male)


#minority
#1st minority with coupon and minority without coupon   *** significant
#给minority发coupon并不能带来更多收益，不发反而更多
rev_no_coupon_minority = no_coupon[no_coupon[,"minority"] == 1,]$revenue_after
rev_yes_coupon_minority = yes_coupon[yes_coupon[,"minority"] == 1,]$revenue_after
t.test(rev_no_coupon_minority,rev_yes_coupon_minority)
#2st majority with coupon and majority without coupon
rev_no_coupon_majority = no_coupon[no_coupon[,"minority"] == 0,]$revenue_after
rev_yes_coupon_majority = yes_coupon[yes_coupon[,"minority"] == 0,]$revenue_after
t.test(rev_no_coupon_majority,rev_yes_coupon_majority)
#3rd minority with coupon and majority with coupon     *** significant
#给majority发coupon会带来更多收益
rev_yes_coupon_minority = yes_coupon[yes_coupon[,"minority"] == 1,]$revenue_after
rev_yes_coupon_majority = yes_coupon[yes_coupon[,"minority"] == 0,]$revenue_after
t.test(rev_yes_coupon_minority,rev_yes_coupon_majority)

#shopping cart
##1st 给购物车里有东西的人比没东西的人发coupon会带来更多收益    *** significant
rev_yes_coupon_cart = yes_coupon[yes_coupon[,"shopping_cart"] == 1,]$revenue_after
rev_yes_coupon_uncart = yes_coupon[yes_coupon[,"shopping_cart"] == 0,]$revenue_after
t.test(rev_yes_coupon_cart,rev_yes_coupon_uncart)

#num past purchase
#给过去购买过的发coupon比给没购买过的人发更有用      *** significant
rev_yes_coupon_pur = yes_coupon[yes_coupon[,"num_past_purch"] > 0,]$revenue_after
rev_yes_coupon_unpur = yes_coupon[yes_coupon[,"num_past_purch"] == 0,]$revenue_after
t.test(rev_yes_coupon_pur,rev_yes_coupon_unpur)

##weeks_since_vist
#给过去三周没浏览过网站的人发coupon更有用
rev_yes_coupon_vist = yes_coupon[yes_coupon[,"num_past_purch"] <= 3,]$revenue_after
rev_yes_coupon_unvist = yes_coupon[yes_coupon[,"num_past_purch"] > 3,]$revenue_after
t.test(rev_yes_coupon_vist,rev_yes_coupon_unvist)


##regression
#not using a
a = lm(revenue_after ~ minority + female + test_coupon+channel_acq+shopping_cart+num_past_purch+spent_last_purchase+
   weeks_since_visit+browsing_minutes, data= artea_ab_test)
summary(a)

#use b for Q3
artea_ab_test$trans = ifelse(artea_ab_test$trans_after == 0,0,1)
yes_coupon_m = artea_ab_test[artea_ab_test[,"test_coupon"] == 1,]
b = glm(trans ~ minority+female+channel_acq+shopping_cart+num_past_purch+spent_last_purchase+
         weeks_since_visit+browsing_minutes, data= yes_coupon_m, family = "binomial")
summary(b)








