#Exploring Orders in Prior DataSet
####Loading and merging datasets ####
Products<-read.csv("file:///C:/Users/swapn/Downloads/Analytics/Kaggle/InstakartMBA/InstakartMBA/products.csv")
orders<-read.csv("file:///C:/Users/swapn/Downloads/Analytics/Kaggle/InstakartMBA/InstakartMBA/fread(-")
prior<-read.csv("file:///C:/Users/swapn/Downloads/Analytics/Kaggle/InstakartMBA/InstakartMBA/View")
aisles<-read.csv("file:///C:/Users/swapn/Downloads/Analytics/Kaggle/InstakartMBA/InstakartMBA/aisles.csv")
departments<-read.csv("file:///C:/Users/swapn/Downloads/Analytics/Kaggle/InstakartMBA/InstakartMBA/departments.csv")
View(orders)
View(Products)
View(prior)
dim(Products)
dim(aisles)
dim(departments)
#Products_Aisles_Department-Merged file
Products_Aisles<-merge(Products,aisles,by="aisle_id")
Products_Aisles_Departments<-merge(Products_Aisles,departments,"department_id")
View(Products_Aisles_Departments)
#Removing Abc from product names
Products_Aisles_Departments$product_name<-gsub("Abc","",Products_Aisles_Departments$product_name)
View(Products_Aisles_Departments)
#Prior with product aisle departmnt

####Aisle with number of products####
library(magrittr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
Number_of_Product_each_Aisle<-Products_Aisles_Departments%>%group_by(aisle)%>%summarise(Number_of_Products=n())%>%arrange(desc(Number_of_Products))
#Top 20 Aisle by number of product offerings
Top_20<-head(Number_of_Product_each_Aisle,n=20)
#Bottom 20 Aisle by number of product offerings
Bottom_20<-tail(Number_of_Product_each_Aisle,n=20)
#Plotting Number of Products in each aisle in decreasing order(Top 20)
ggplot(Top_20, aes(x = reorder(aisle,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)))) +
  geom_bar(stat = "identity")+coord_flip()+
  labs(title="Top 20 Aisle by Variety of Product Offering",y="Number of Products",x="Aisle")+
  geom_text(nudge_y = 35)
#Plotting Number of Products in each aisle in decreasing order(Bottom 20)
ggplot(Bottom_20, aes(x = reorder(aisle,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)))) +
  geom_bar(stat = "identity")+coord_flip()+labs(title="Bottom 20 Aisle by Variety of Product offering",y="Number of Products",x="Aisle")+
  geom_text(nudge_y = 3.5)
#Products in missing aisle and department
Products_Aisles_Departments_subset<-subset(Products_Aisles_Departments,Products_Aisles_Departments$aisle=="missing")
View(Products_Aisles_Departments_subset)
#### Department with number of Product ####
#Number of Products in each department
Number_of_Product_each_department<-Products_Aisles_Departments%>%group_by(department)%>%summarise(Number_of_Products=n())%>%arrange(desc(Number_of_Products))
View(Number_of_Product_each_department)
#Vis--Bar chart for number of products in each department
ggplot(Number_of_Product_each_department, aes(x = reorder(department,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)))) +
  geom_bar(stat = "identity")+coord_flip()+labs(title="Department by Variety of Product offering",y="Number of Products",x="Department")+
  geom_text(nudge_y = 250)

###comparing with mean number of products offered by each aisle (),   try to plot graph that is clear####
Average_NumberofProductsby_Aisle<-Number_of_Product_each_Aisle%>%mutate(Avg=mean(Number_of_Products,na.rm=TRUE),
                                           Above_Average=ifelse(Number_of_Products-Avg>0,TRUE,FALSE),aisle=factor(aisle,levels=.$aisle))
                                                        
ggplot(Average_NumberofProductsby_Aisle,aes(Number_of_Products,aisle,color=Above_Average,label=paste0(round(Number_of_Products,0))))+geom_segment(aes(x=Avg,y=aisle,xend=Number_of_Products,yend=aisle),color="grey50")+geom_point(size=1)+geom_text(nudge_x = 1.5)+labs(
  title = "Number of Products offered Across Aisles", y="Aisle",x="Number of Products")

####Department-->Aisles-->Products####
Department_Aisles_Products<-Products_Aisles_Departments%>%group_by(department,aisle)%>%summarise(Number_of_Products=n())%>%
  mutate(Percentage_of_Total_products=round(((Number_of_Products*100)/nrow(Products_Aisles_Departments)),2))%>%
  arrange(desc(Number_of_Products))
#View(Department_Aisles_Products)

#Vis----Stack Bars of Department-->Aisle-->Number_of_Products
library(ggthemes)
merged_dep_file<-merge(Department_Aisles_Products,Number_of_Product_each_department,by="department")%>%arrange(desc(Percentage_of_Total_products))
#View(merged_dep_file)

depart_aisle_prod_plot<-ggplot()+
  geom_bar(aes(x=reorder(department,Number_of_Products.y),y=Percentage_of_Total_products,fill=aisle),data=merged_dep_file,stat="identity")+
  labs(title="Department-Aisle-Product offering",y="Percent of Total Products",x="Department")+theme_economist()
depart_aisle_prod_plot

####Orders EDA####
View(orders)
#Data distribution over prior,train and test
orders%>%group_by(eval_set)%>%summarise(Number_of_Orders=n())%>%mutate(Percentage_of_orders=(Number_of_Orders*100/nrow(orders)))%>%arrange(desc(Number_of_Orders))
#Number of Orders at every hour
Orders_everyhour<-orders%>%group_by(order_hour_of_day)%>%summarise(Number_of_Orders=n())%>%mutate(Percentage_of_orders=(Number_of_Orders*100/nrow(orders)))
View(Orders_everyhour)
#### Visualization for number of orders at every hour of the day and every day of the week####
# referenced from: http://zoonek2.free.fr/UNIX/48_R/03.html
x<-Orders_everyhour$Percentage_of_orders
clock.plot <- function (x, col = rainbow(n,s=1,v=1,start=0,end=max(1,n-1)/n,alpha=0.5), ...) {
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, 
       type = 'n', # do not plot anything
       xlim = c(-m,m), ylim = c(-m,m), 
       axes = F, xlab = '', ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a) )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), 
            (1-v)*cos(a), (1-v)*sin(a) )
  segments( cos(a), sin(a), 
            0, 0, 
            col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0),
             c(0, x[i]*sin(a+ca), 0),
             col=col[i] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
}
clock.plot(x, 
           main = "Peak Ordering Hours")


#Number of Orders every day of the week
Orders_everyday<-orders%>%group_by(order_dow)%>%summarise(Number_of_Orders=n())%>%mutate(Percentage_of_orders=(Number_of_Orders*100/nrow(orders)))
View(Orders_everyday)
#Visualizing Number of Orders by day of the week
ggplot(Orders_everyday,aes(x=order_dow,y=Percentage_of_orders,label=paste0(round(Percentage_of_orders,1))))+
  geom_bar(stat = "identity")+labs(title="% of Orders by day of the Week",y="Percentage of Total Orders",x="Day of the Week : 0 denotes Sunday ")+
  geom_text(nudge_y = .5)
#### Orders everyday everyhour data preperation####
Dow_hod_orders<-orders%>%group_by(order_dow,order_hour_of_day)%>%
  summarise(Number_of_Orders=n())

Dow_hod_orders_combined<-merge(Dow_hod_orders,Orders_everyday,by="order_dow",all.x = TRUE)%>%
  mutate(Percentage_by_doy=Number_of_Orders.x*100/Number_of_Orders.y)
View(Dow_hod_orders_combined)

####Visualizing orders by dow-->hod,     try to find correct percentage####
library(ggrepel)
ggplot(Dow_hod_orders_combined, aes(x = Dow_hod_orders_combined$order_hour_of_day, y = Dow_hod_orders_combined$Percentage_by_doy,label=paste0(round(Percentage_by_doy,1)))) +
  geom_bar(stat="identity") +
  labs(title="Visualizing orders by hour of day for each day of week with 0 representing Sunday",x="0-24 represents hours of the day",y="Percentage of orders for the day")+
  geom_text_repel(nudge_y = 0.05)+facet_grid(~ Dow_hod_orders_combined$order_dow)

ggplot(Dow_hod_orders_combined, aes(x = Dow_hod_orders_combined$order_hour_of_day, y = Dow_hod_orders_combined$Percentage_by_doy)) +
  geom_bar(stat="identity") +
  labs(title="Visualizing orders by hour of day for each day of week with 0 representing Sunday",x="0-24 represents hours of the day",y="Percentage of orders for the day")+
  facet_grid(~ Dow_hod_orders_combined$order_dow)


####Reordering Days analysis####
#Mode of the column will represent how long most people take between two orders............. do again
library(plyr)
library(dplyr)
Reordering_Gap<-count(orders,'days_since_prior_order')%>%arrange(desc(freq))%>%mutate(Percent_orders=round(freq*100/nrow(orders)),2)
#Inference: 11 % of the time people reorder monthly(after 30 days), and 9 % of the time weekly. This shows there is a section of people who refill their groceries every month and other who refills every week. Frequency of NA represents total number of unique users and its their first order.
#Visualizing reordering Gap
library(ggthemes)
barfill <- "gold1"
barlines <- "goldenrod2"

Reordering_Gap_plot<-ggplot(orders,aes(x=days_since_prior_order))+
  geom_histogram(aes(fill=..count..),binwidth=1)+
  scale_x_continuous(name = "Days Since Prior Order",breaks = seq(0, 30, 1))+
  scale_y_continuous(name = "Frequency of Orders",breaks=seq(0,1000000,100000))+
  ggtitle("Gap between two orders?")+
  labs(x="Days Since Prior Order")+
  theme_update()
Reordering_Gap_plot


####Prior Data Analysis####
#Top 50 Products ORdered
top50_products<-count(prior$product_id)%>%arrange(desc(freq))%>%head(50)
View(top50_products)
colnames(top50_products)[1]<-'product_id'
Top50Products<-merge(top50_products,Products_Aisles_Departments,by='product_id')%>%arrange(desc(freq))
View(Top50Products)
#Visualization of top 50 products
ggplot(Top50Products, aes(x = reorder(product_name,freq), y = freq,label=paste0(round(freq,0)))) +
  geom_bar(stat = "identity")+coord_flip()+labs(title="Top 50 most ordered Products ",y="Number of orders",x="product_name")+
  geom_text(nudge_y = 20000)
#Bottom 50 (least ordered products)
bottom50_products<-count(prior$product_id)%>%arrange(desc(freq))%>%tail(50)
colnames(bottom50_products)[1]<-'product_id'
bottom50Products<-merge(bottom50_products,Products_Aisles_Departments,by='product_id')%>%arrange(freq)
View(bottom50Products)
#Top product of every department
TopProduct_department<-count(prior$product_id)%>%arrange(desc(freq))
colnames(TopProduct_department)[1]<-'product_id'
TopProduct_department<-merge(TopProduct_department,Products_Aisles_Departments,by='product_id')%>%arrange(freq)
View(TopProduct_department)
library(tcltk)
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
Dep_top<-sqldf('select department,product_name,max(freq) as Total_Orders from TopProduct_department
      group by department order by max(freq) desc')
View(Dep_top)
#Top product for every aisle
aisle_top<- sqldf('select aisle,product_name,max(freq) as Total_Orders from TopProduct_department
      group by aisle order by max(freq) desc')
View(aisle_top)


#### Market basket analysis####
install.packages("arules")
library(Matrix)
library(arules)
prior4mba<-split(prior$product_id,prior$order_id)
transaction_prior<-as(prior4mba,"transactions")
dim(transaction_prior)
View(Products_Aisles_Departments)
#frequent product ids in the transactions
itemFrequencyPlot(transaction_prior,support=0.05,cex.names=0.8)
#Apriori algorithm
basket_rules<-apriori(transaction_prior,parameter = list(sup=0.00001,conf=0.6,maxlen=3,target="rules"))
summary(basket_rules)
inspect(head(basket_rules))
#Visualizing rules
install.packages("arulesViz")
library(arulesViz)

#Number of Products per basket
hist(size(transaction_prior), breaks = 0:150, xaxt="n", ylim=c(0,250000), col = "grey",
     main = "Number of Products per Order", xlab = "Order Size:Number of Products")
axis(1, at=seq(0,160,by=10), cex.axis=0.8)
mtext(paste("Total:", length(transaction_prior), "Orders,", sum(size(transaction_prior)), "Products"))

#Frequently ordered products
item_frequencies <- itemFrequency(transaction_prior, type="a")
support <- 0.03
freq_items <- sort(item_frequencies, decreasing = F)
freq_items <- freq_items[freq_items>support*length(transaction_prior)]

par(mar=c(2,10,2,2)); options(scipen=5)
barplot(freq_items, horiz=T, las=1, main="Frequent Items", cex.names=.8, xlim=c(0,500000))
mtext(paste("support:",support), padj = .8)
abline(v=support*length(transaction_prior), col="red")
View(Products_Aisles_Departments)
#Frequent items bought together
basket_rules<-apriori(transaction_prior,parameter = list(sup=0.0003, conf=0.5, target="rules"))
summary(basket_rules)
inspect(sort(basket_rules,by="lift")[1:10])
plot(basket_rules)
plot(head(sort(basket_rules,by="lift"),10),method="graph")
plot(basket_rules,method="grouped")
