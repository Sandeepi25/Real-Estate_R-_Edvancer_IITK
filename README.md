# Real-Estate_R-_Edvancer_IITK
Real Estate
Price of a property is one of the most important decision criterion when people buy homes. Real state firms need to be consistent in their pricing in order to attract buyers . Having a predictive model for the same will be great tool to have , which in turn can also be used to tweak development of properties , putting more emphasis on qualities which increase the value of the property.



We have given you two datasets , housing_train.csv and housing_test.csv . You need to use data housing_train to build predictive model for response variable "Price". Housing_test data contains all other factors except "Price", you need to predict that using the model that you developed and submit your predicted values in a csv files.


library(dplyr)
setwd(&quot;C:/Users/kiit1/Desktop/R/Project&quot;)
getwd()
train=read.csv(&quot;housing_train.csv&quot;,stringsAsFactors = FALSE,header = T )
test=read.csv(&quot;housing_test.csv&quot;,stringsAsFactors = FALSE,header = T )
apply(train,2,function(x)sum(is.na(x)))
train$Bedroom2[is.na(train$Bedroom2)]=median(train$Bedroom2,na.rm=T)
apply(train,2,function(x)sum(is.na(x)))
train$Bathroom[is.na(train$Bathroom)]=round(mean(train$Bathroom,na.rm=T),0)
apply(train,2,function(x)sum(is.na(x)))
train$Car[is.na(train$Car)]=round(mean(train$Car,na.rm=T),0)
apply(train,2,function(x)sum(is.na(x)))
train$Landsize[is.na(train$Landsize)]=round(mean(train$Landsize,na.rm=T),0)
apply(train,2,function(x)sum(is.na(x)))
train$BuildingArea[is.na(train$BuildingArea)]=round(mean(train$BuildingArea,na.rm=T),0)
apply(train,2,function(x)sum(is.na(x)))
train$YearBuilt[is.na(train$YearBuilt)]=round(mean(train$YearBuilt,na.rm=T),0)
apply(train,2,function(x)sum(is.na(x)))
apply(test,2,function(x)sum(is.na(x)))
test$Bedroom2[is.na(test$Bedroom2)]=median(test$Bedroom2,na.rm=T)
apply(test,2,function(x)sum(is.na(x)))
test$Bathroom[is.na(test$Bathroom)]=round(mean(test$Bathroom,na.rm=T),0)
apply(test,2,function(x)sum(is.na(x)))
test$Car[is.na(test$Car)]=round(mean(test$Car,na.rm=T),0)
apply(test,2,function(x)sum(is.na(x)))
test$Landsize[is.na(test$Landsize)]=round(mean(test$Landsize,na.rm=T),0)
apply(test,2,function(x)sum(is.na(x)))
test$BuildingArea[is.na(test$BuildingArea)]=round(mean(test$BuildingArea,na.rm=T),0)

apply(test,2,function(x)sum(is.na(x)))
test$YearBuilt[is.na(test$YearBuilt)]=round(median(test$YearBuilt,na.rm=T),0)

test$Price=NA
train$data=&#39;train&#39;
test$data=&#39;test&#39;
all_data=rbind(train,test)
apply(all_data,2,function(x)sum(is.na(x)))
glimpse(all_data)

t=table(all_data$Suburb)
View(t)
t1=round(tapply(all_data$Price,all_data$Suburb,mean,na.rm=T),0)
View(t1)
t1=sort(t1)

all_data=all_data %&gt;%
mutate(
sub_1=as.numeric(Suburb%in%c(&quot;Campbellfield&quot;,&quot;Jacana&quot;)),
sub_2=as.numeric(Suburb%in%c(&quot;Kealba&quot;,&quot;Brooklyn&quot;,&quot;Albion&quot;,&quot;Sunshine
West&quot;,&quot;Ripponlea&quot;,&quot;Fawkner&quot;)),
sub_3=as.numeric(Suburb%in%c(&quot;Glenroy&quot;,&quot;Southbank&quot;,&quot;Sunshine North&quot;,&quot;Keilor Park&quot;,&quot;Heidelberg
West&quot;,&quot;Reservoir&quot;,&quot;Braybrook&quot;,&quot;Kingsbury&quot;,&quot;Gowanbrae&quot;,&quot;Hadfield&quot;,&quot;Watsonia&quot;,&quot;Footscray&quot;,&quot;South
Kingsville&quot;,&quot;Balaclava&quot;,&quot;Melbourne&quot;,&quot;Maidstone&quot;,&quot;Sunshine&quot;)),
sub_4=as.numeric(Suburb%in%c(&quot;Airport West&quot;,&quot;Heidelberg Heights&quot;,&quot;Pascoe Vale&quot;,&quot;West
Footscray&quot;,&quot;Altona North&quot;,&quot;Williamstown North&quot;,&quot;Brunswick West&quot;,&quot;Keilor East&quot;,&quot;Oak
Park&quot;,&quot;Maribyrnong&quot;,&quot;Altona&quot;,&quot;Flemington&quot;,&quot;Coburg North&quot;,&quot;Yallambie&quot;,&quot;Avondale
Heights&quot;,&quot;Bellfield&quot;)),
sub_5=as.numeric(Suburb%in%c(&quot;Strathmore Heights&quot;,&quot;Glen Huntly&quot;,&quot;Kensington&quot;,&quot;Essendon
North&quot;,&quot;St Kilda&quot;,&quot;Preston&quot;,&quot;North Melbourne&quot;,&quot;Coburg&quot;,&quot;Kingsville&quot;,&quot;Collingwood&quot;,&quot;Brunswick
East&quot;,&quot;Gardenvale&quot;,&quot;Thornbury&quot;,&quot;Niddrie&quot;,&quot;West Melbourne&quot;,&quot;Viewbank&quot;)),
sub_6=as.numeric(Suburb%in%c(&quot;Spotswood&quot;,&quot;Carnegie&quot;,&quot;Elwood&quot;,&quot;Heidelberg&quot;,&quot;Moorabbin&quot;,&quot;Oakleig

h&quot;,&quot;Rosanna&quot;,&quot;Docklands&quot;,&quot;Yarraville&quot;,&quot;Cremorne&quot;,&quot;Seddon&quot;,&quot;Brunswick&quot;,&quot;Oakleigh South&quot;,&quot;Ascot
Vale&quot;,&quot;Windsor&quot;,&quot;Caulfield&quot;,&quot;Essendon West&quot;,&quot;Newport&quot;)),
sub_7=as.numeric(Suburb%in%c(&quot;Chadstone&quot;,&quot;South Yarra&quot;,&quot;Essendon&quot;,&quot;Bentleigh
East&quot;,&quot;Murrumbeena&quot;,&quot;Hughesdale&quot;,&quot;Fairfield&quot;,&quot;Ashwood&quot;,&quot;Clifton Hill&quot;,&quot;Caulfield
North&quot;,&quot;Abbotsford&quot;,&quot;Carlton&quot;,&quot;Prahran&quot;,&quot;Fitzroy&quot;,&quot;Ivanhoe&quot;,&quot;Hampton East&quot;,&quot;Caulfield East&quot;)),
sub_8=as.numeric(Suburb%in%c(&quot;Richmond&quot;,&quot;Travancore&quot;,&quot;Templestowe
Lower&quot;,&quot;Ormond&quot;,&quot;Caulfield South&quot;,&quot;Moonee Ponds&quot;,&quot;Hawthorn&quot;,&quot;Box
Hill&quot;,&quot;Bulleen&quot;,&quot;Burnley&quot;,&quot;Burwood&quot;,&quot;Strathmore&quot;,&quot;Port Melbourne&quot;,&quot;Fitzroy North&quot;,&quot;Alphington&quot;)),
sub_9=as.numeric(Suburb%in%c(&quot;Doncaster&quot;,&quot;South
Melbourne&quot;,&quot;Northcote&quot;,&quot;Aberfeldie&quot;,&quot;Elsternwick&quot;,&quot;Bentleigh&quot;,&quot;Kooyong&quot;,&quot;Parkville&quot;)),
sub_10=as.numeric(Suburb%in%c(&quot;Williamstown&quot;,&quot;East Melbourne&quot;,&quot;Seaholme&quot;)),
sub_11=as.numeric(Suburb%in%c(&quot;Malvern East&quot;,&quot;Carlton North&quot;,&quot;Hawthorn East&quot;,&quot;Surrey Hills&quot;)),
sub_12=as.numeric(Suburb%in%c(&quot;Princes Hill&quot;,&quot;Mont Albert&quot;,&quot;Armadale&quot;,&quot;Kew East&quot;,&quot;Glen
Iris&quot;,&quot;Ashburton&quot;)),
sub_13=as.numeric(Suburb%in%c(&quot;Brighton East&quot;,&quot;Eaglemont&quot;,&quot;Hampton&quot;)),
sub_14=as.numeric(Suburb%in%c(&quot;Toorak&quot;,&quot;Ivanhoe East&quot;,&quot;Camberwell&quot;,&quot;Balwyn North&quot;,&quot;Kew&quot;)),
sub_15=as.numeric(Suburb%in%c(&quot;Brighton&quot;,&quot;Middle Park&quot;)),
sub_16=as.numeric(Suburb%in%c(&quot;Albert Park&quot;,&quot;Balwyn&quot;,&quot;Malvern&quot;))
) %&gt;%
select(-Suburb)

glimpse(all_data)

all_data=all_data %&gt;%
select(-Address)
glimpse(all_data)

table(all_data$Type)

all_data=all_data %&gt;%
mutate(Type_t=as.numeric(Type==&quot;t&quot;),

type_u=as.numeric(Type==&quot;u&quot;))
all_data=all_data %&gt;%
select(-Type)

glimpse(all_data)

table(all_data$Method)

all_data=all_data %&gt;%
mutate(Method_PI=as.numeric(Method==&quot;PI&quot;),
Method_SA=as.numeric(Method==&quot;SA&quot;),
Method_SP=as.numeric(Method==&quot;SP&quot;),
Method_VB=as.numeric(Method==&quot;VB&quot;)) %&gt;%
select(-Method)

glimpse(all_data)

t=table(all_data$SellerG)
sort(t)

all_data=all_data %&gt;%
mutate(Gnelson=as.numeric(SellerG==&quot;Nelson&quot;),
GJellis=as.numeric(SellerG==&quot;Jellis&quot;),
Ghstuart=as.numeric(SellerG==&quot;hockingstuart&quot;),
Gbarry=as.numeric(SellerG==&quot;Barry&quot;),
GMarshall=as.numeric(SellerG==&quot;Marshall&quot;),
GWoodards=as.numeric(SellerG==&quot;Woodards&quot;),
GBrad=as.numeric(SellerG==&quot;Brad&quot;),
GBiggin=as.numeric(SellerG==&quot;Biggin&quot;),

GRay=as.numeric(SellerG==&quot;Ray&quot;),
GFletchers=as.numeric(SellerG==&quot;Fletchers&quot;),
GRT=as.numeric(SellerG==&quot;RT&quot;),
GSweeney=as.numeric(SellerG==&quot;Sweeney&quot;),
GGreg=as.numeric(SellerG==&quot;Greg&quot;),
GNoel=as.numeric(SellerG==&quot;Noel&quot;),
GGary=as.numeric(SellerG==&quot;Gary&quot;),
GJas=as.numeric(SellerG==&quot;Jas&quot;),
GMiles=as.numeric(SellerG==&quot;Miles&quot;),
GMcGrath=as.numeric(SellerG==&quot;McGrath&quot;),
GHodges=as.numeric(SellerG==&quot;Hodges&quot;),
GKay=as.numeric(SellerG==&quot;Kay&quot;),
GStockdale=as.numeric(SellerG==&quot;Stockdale&quot;),
GLove=as.numeric(SellerG==&quot;Love&quot;),
GDouglas=as.numeric(SellerG==&quot;Douglas&quot;),
GWilliams=as.numeric(SellerG==&quot;Williams&quot;),
GVillage=as.numeric(SellerG==&quot;Village&quot;),
GRaine=as.numeric(SellerG==&quot;Raine&quot;),
GRendina=as.numeric(SellerG==&quot;Rendina&quot;),
GChisholm=as.numeric(SellerG==&quot;Chisholm&quot;),
GCollins=as.numeric(SellerG==&quot;Collins&quot;),
GLITTLE=as.numeric(SellerG==&quot;LITTLE&quot;),
GNick=as.numeric(SellerG==&quot;Nick&quot;),
GHarcourts=as.numeric(SellerG==&quot;Harcourts&quot;),
GCayzer=as.numeric(SellerG==&quot;Cayzer&quot;),
GMoonee=as.numeric(SellerG==&quot;Moonee&quot;),
GYPA=as.numeric(SellerG==&quot;YPA&quot;)
) %&gt;%
select(-SellerG)

glimpse(all_data)

table(all_data$CouncilArea)

all_data=all_data %&gt;%
mutate(CA_Banyule=as.numeric(CouncilArea==&quot;Banyule&quot;),
CA_Bayside=as.numeric(CouncilArea==&quot;Bayside&quot;),
CA_Boroondara=as.numeric(CouncilArea==&quot;Boroondara&quot;),
CA_Brimbank=as.numeric(CouncilArea==&quot;Brimbank&quot;),
CA_Darebin=as.numeric(CouncilArea==&quot;Darebin&quot;),
CA_Glen_Eira=as.numeric(CouncilArea==&quot;Glen Eira&quot;),
CA_Monash=as.numeric(CouncilArea==&quot;Monash&quot;),
CA_Melbourne=as.numeric(CouncilArea==&quot;Melbourne&quot;),
CA_Maribyrnong=as.numeric(CouncilArea==&quot;Maribyrnong&quot;),
CA_Manningham=as.numeric(CouncilArea==&quot;Manningham&quot;),
CA_Kingston=as.numeric(CouncilArea==&quot;Kingston&quot;),
CA_Hume=as.numeric(CouncilArea==&quot;Hume&quot;),
CA_HobsonsB=as.numeric(CouncilArea==&quot;Hobsons Bay&quot;),
CA_MoonValley=as.numeric(CouncilArea==&quot;Moonee Valley&quot;),
CA_Moreland=as.numeric(CouncilArea==&quot;Moreland&quot;),
CA_PortP=as.numeric(CouncilArea==&quot;Port Phillip&quot;),
CA_Stonnington=as.numeric(CouncilArea==&quot;Stonnington&quot;),
CA_Whitehorse=as.numeric(CouncilArea==&quot;Whitehorse&quot;),
CA_Yarra=as.numeric(CouncilArea==&quot;Yarra&quot;)) %&gt;%
select(-CouncilArea)

glimpse(all_data)

train=all_data %&gt;%
filter(data==&#39;train&#39;) %&gt;%
select(-data)
test=all_data %&gt;%
filter(data==&#39;test&#39;) %&gt;%
select(-data,-Price)
glimpse(train)
glimpse(test)

set.seed(123)
s=sample(1:nrow(train),0.75*nrow(train))
train_75=train[s,] #5652
test_25=train[-s,] #1884

library(car)

LRf=lm(Price ~ .,data=train_75)
summary(LRf)

a=vif(LRf)
sort(a,decreasing = T)[1:3]

LRf=lm(Price ~ .-Postcode-sub_3,data=train_75)
summary(LRf)

a=vif(LRf)
sort(a,decreasing = T)[1:3]

summary(LRf)

LRf=lm(Price ~ .-Landsize-GRaine-GMoonee-CA_Bayside-GLITTLE-Gnelson-GSweeney-Ghstuart-
CA_Kingston-Gbarry-GRay-GStockdale-GNoel-GJas-GBiggin-GYPA-CA_PortP-CA_Whitehorse-GRendina-
GFletchers-GBrad-GHodges-GVillage-GLove-sub_4-GGary-CA_Hume-CA_Boroondara-Method_SA-
GWilliams-GHarcourts-GNick-GGreg-CA_Monash-GWoodards-CA_Stonnington-GCayzer-Postcode-
sub_3,data=train_75)
summary(LRf)

PP_test_25=predict(LRf,newdata =test_25)
PP_test_25=round(PP_test_25,1)
class(PP_test_25)

plot(test_25$Price,PP_test_25)

res=test_25$Price-PP_test_25 #(real value-predicted value)
#root mean square error is as follows
RMSE_test_25=sqrt(mean(res^2))
RMSE_test_25
212467/RMSE_test_25

library(ggplot2)
d=data.frame(real=test_25$Price,predicted=PP_test_25)
ggplot(d,aes(x=real,y=predicted))+geom_point()
plot(LRf,which = 1) #gives residual vz fitted plot
plot(LRf,which = 2) #gives q-q-plot
plot(LRf,which = 3) #gives scale-location plot
plot(LRf,which = 4) #gives cooks distance

SandeepiSrimayee_Mohakud=predict(LRf,newdata =test)
SandeepiSrimayee_Mohakud=round(SandeepiSrimayee_Mohakud,1)

class(SandeepiSrimayee_Mohakud)

write.csv(SandeepiSrimayee_Mohakud, &quot;SandeepiSrimayee_Mohakud.csv&quot;)
summary(LRf)
