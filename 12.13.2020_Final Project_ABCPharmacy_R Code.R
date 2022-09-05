# load packages
library(dplyr)
library(Hmisc)
library(DBI)
library(stringr)

# Disable scientific notation
options(scipen = 999)  

# Set working directory
setwd("C:/Users/OwenEvans/Desktop/DSA5010 - Data Management/Final Project Work/Data/ABCPharmacy")

# Load data and assign to variables
major_product_cat <- 
  read.csv ("MAJOR_PROD_CAT.csv", stringsAsFactors = FALSE)

phrmcy_master <- 
  read.csv ("PHRMCY_MASTER.csv", stringsAsFactors = FALSE)

pos_trans <- 
  read.csv ("POS_TRANS.csv", stringsAsFactors = FALSE)

prod_cat <- 
  read.csv ("PROD_CAT.csv", stringsAsFactors = FALSE)

prod_master <-
  read.csv ("PROD_MASTER.csv", stringsAsFactors = FALSE)

prod_seg <- 
  read.csv ("PROD_SEG.csv", stringsAsFactors = FALSE)

prod_sub_cat <-
  read.csv ("PROD_SUB_CAT.csv", stringsAsFactors = FALSE)

# Fix date formats
pos_trans <- transform(pos_trans, 
                       SLS_DTE_NBR = as.Date(as.character(SLS_DTE_NBR), "%Y%m%d"))


# Set up SQLite database for possible SQL queries, alternative to R
ABCdb <- dbConnect(RSQLite::SQLite(), "r_my_ABCdb.sqlite")
dbWriteTable(ABCdb, "major_prod_cat", major_product_cat)
dbWriteTable(ABCdb, "phrmcy_master", phrmcy_master)
dbWriteTable(ABCdb, "pos_trans", pos_trans)
dbWriteTable(ABCdb, "prod_cat", prod_cat)
dbWriteTable(ABCdb, "prod_master", prod_master)
dbWriteTable(ABCdb, "prod_seg", prod_seg)
dbWriteTable(ABCdb, "prod_sub_cat", prod_sub_cat)

# EXPLORING THE DATA

# How best to define gross sales? 
# What does SLS_QTY and SLS_AMT tell us?
# (+) ,  (+) <- SLSQTY, SLS AMOUNT = Gross Sales - positive cash flow
# (-) ,  (-) <- SLSQTY, SLS AMOUNT = Product Return, return to inventory
# (+) ,  (-) <- SLSQTY, SLS AMOUNT = Product Return, remove from inventory
# (-) ,  (+) <- SLSQTY, SLS AMOUNT = ? Unknown, bad data entry 
# (0) or (0)  <-SLSQTY, SLS AMOUNT = Void Transactions

# GrossSales - 906,541 transactions in this category
GrossSales <- length(which(pos_trans$SLS_QTY > 0 & 
                      pos_trans$EXT_SLS_AMT > 0)) 

# Returns1 - 4076 transactions in this category
Returns1 <- length (which(pos_trans$SLS_QTY < 0 &
                           pos_trans$EXT_SLS_AMT <0)) # Returns, return to inventory

# Returns2 - 1 transaction in this category
Returns2 <- length(which(pos_trans$SLS_QTY > 0 &
                          pos_trans$EXT_SLS_AMT < 0)) # Returns, remove from inventory

# Other - Negative sales quantity, positive cash flow.
# 624 transactions in this category
# Bad data entry, exclude from analysis
Other <- length(which(pos_trans$SLS_QTY <0 &
                        pos_trans$EXT_SLS_AMT >0)) # Other, bad data

# Voids - transactions with no value, exclude from sales analysis
# 4592 transactions in this category
Voids    <- length(which(pos_trans$EXT_SLS_AMT==0 | pos_trans$SLS_QTY ==0)) # Void transactions

A <- sum(Voids, Other, Returns1, Returns2, GrossSales)
B <- length(pos_trans$EXT_SLS_AMT)
A == B  # All transactions accounted for and categorized.

# Summarize transaction type data
POS_Summary <- data.frame(c('GrossSales', 
                            'Returns - Returned To Inventory',
                            'Returns - Removed From Inventory',
                            'Voids',
                            'Other'),
                          c(GrossSales,Returns1,Returns2, Voids, Other))

names(POS_Summary) = c('Transaction Type', '# of Transactions')
write.csv(POS_Summary, "POS_summary.csv")

# Cleanup variables no longer needed.
remove(A, B, Voids, Other, Returns1, Returns2, GrossSales, POS_Summary)

# Exploring transactions in "Other" category in more detail
other_df <- pos_trans[(pos_trans$EXT_SLS_AMT>0 & 
                         pos_trans$SLS_QTY<0),]

other_geo_merge <- merge(other_df, phrmcy_master, 
                         by='PHRMCY_NBR')

other_locations <- other_geo_merge %>% group_by(PHRMCY_NAM, ST_CD) %>% tally()
write.csv(other_locations, "otherLocations.csv")
# other transactions centrally located to NY and NJ
# recommend a review of POS practices to ensure
# accuracy and consistency of transactions and inventory management

# Are these "other" transactions related to any specific products?
other_prod_merge <- merge(other_df, prod_master, by='PROD_NBR')
other_prod_merge <- other_prod_merge %>% rename(SEG_CD = SEGMENT_CD) 
other_prod_merge <- merge(other_prod_merge, prod_seg, by = 'SEG_CD')
other_prod_merge <- merge(other_prod_merge, prod_sub_cat, by = 'SUB_CAT_CD')
other_prod_merge <- merge(other_prod_merge, prod_cat, by = 'CAT_CD')
other_prod_merge <- other_prod_merge %>% group_by(CAT_DESC) %>% 
  summarise(Total = n()) %>% arrange(desc(Total))

write.csv(other_prod_merge, "OtherTransactionSummary.csv")

# Clean up "other" variables
remove(other_prod_merge, other_locations, other_df, other_geo_merge)

# Explore transactions in void category in more detail
void_df <- pos_trans[(pos_trans$EXT_SLS_AMT==0 | pos_trans$SLS_QTY ==0),]
void_geo_merge <- merge(void_df, phrmcy_master, by='PHRMCY_NBR')
void_locations <- void_geo_merge %>% group_by(PHRMCY_NAM, ST_CD) %>% 
  summarise(total=n()) %>% arrange(desc(total))

write.csv(void_locations, "void_locations.csv")
# Void transactions from 57 locations
# Well distributed across all locations

# Are these Void transactions related to any specific products?
void_prod_merge <- merge(void_df, prod_master, by='PROD_NBR')
void_prod_merge <- void_prod_merge %>% rename(SEG_CD = SEGMENT_CD) 
void_prod_merge <- merge(void_prod_merge, prod_seg, by = 'SEG_CD')
void_prod_merge <- merge(void_prod_merge, prod_sub_cat, by = 'SUB_CAT_CD')
void_prod_merge <- merge(void_prod_merge, prod_cat, by = 'CAT_CD')
void_prod_merge <- void_prod_merge %>% group_by(CAT_DESC) %>% 
  summarise(Total = n()) %>% arrange(desc(Total))

write.csv(void_prod_merge, "VoidTransactionSummary.csv")

# Clean up void variables
remove(void_prod_merge, void_locations, void_df, void_geo_merge)

# GROSS SALES
# Calculate gross sales as sales quantity times sales amount
# for transactions that have positive EXT_SLS_AMT and SLS_QTY only. 
# All other transactions are voids, returns or others

# Filter pos_trans dataframe for these transactions only
gross_sales <- pos_trans %>% filter(pos_trans$EXT_SLS_AMT>0 & pos_trans$SLS_QTY>0)

# Add gross sales and month columns
gross_sales$GrossSales <- gross_sales$EXT_SLS_AMT * gross_sales$SLS_QTY
gross_sales$month <- format(as.Date(gross_sales$SLS_DTE_NBR), "%b")

# Write to gross_sales to database
dbRemoveTable(ABCdb, "gross_sales")
dbWriteTable(ABCdb, "gross_sales", gross_sales)

# Exploration of Gross Sales Data in more detail
# Look for any unusual (outliers) SLS_AMT values
# Lets look at transactions with a total value in excess of $50,000
# grouped by product number and description
# High value transactions are largely dominated by money orders and lottery

HighValueProds <- dbGetQuery(ABCdb, "SELECT gross_sales.PROD_NBR, SUM(GrossSales) 
                             AS Total_Value, sum(SLS_QTY) AS Tally, prod_master.PROD_DESC,
                             round((sum(GrossSales)/sum(SLS_QTY)),2) AS Average_Price,
                             round((sum(GrossSales)/17199290*100),1) AS PercentOfGross
                             FROM gross_sales JOIN prod_master
                             ON gross_sales.PROD_NBR = prod_master.PROD_NBR
                             GROUP BY gross_sales.PROD_NBR
                             HAVING Total_Value>500000
                             ORDER BY Total_Value DESC")

write.csv(HighValueProds, "HighValueProds.csv")
  
MoneyOrderLocations <- prod_master %>% filter(PROD_DESC == "MONEY ORDER") %>%
  merge(gross_sales, by="PROD_NBR") %>% 
  group_by(PHRMCY_NBR, PROD_NBR) %>%
  summarise(total_sales = sum(GrossSales)) %>%
  merge(phrmcy_master, by='PHRMCY_NBR') %>% merge (prod_master, by='PROD_NBR') %>%
  select(PROD_DESC, total_sales,PHRMCY_NAM, ST_CD) %>% arrange(desc(total_sales))

write.csv(MoneyOrderLocations, "MoneyOrderLocations.csv")

# Gross Sales - Data Explore
# Explore gross sales variable in more detail

describe(gross_sales$GrossSales)
# 906,451 individual transactions, no NA's

median(gross_sales$GrossSales)
mean(gross_sales$GrossSales)
# Median transaction value is $5.25 
# Mean transaction value is $18.97
# Heavy right skew in histogram.  
# Lots of little transactions, a few large.  Log transform.

# Histogram of POS transaction values for more detail
gross_sales_hist <- ggplot(aes(x = GrossSales), data = gross_sales) +
  geom_histogram(color="darkblue", fill="lightblue") +
  scale_x_log10() + 
  xlab("Sales Transaction Value ($)") +
  ylab("Frequency Count") +
  ggtitle("Value of Individual Gross Sales for ABC Pharmacy Chain")+
  theme(axis.title = element_text(size = 22)) +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(plot.title=element_text(size=26))

length(unique(gross_sales$PHRMCY_NBR))
# Transactions come from 106 unique stores
# There are 1097 stores listed in pharmacy master DF.
# POS transaction register is thus a sampling of the entire chain
# Is this sampling representative?
# Can our analysis be generalized to the whole company?


# What pharmacies are NOT included in POS data?

# Join gross sales DF with pharmacy master to identify stores
# This only includes stores reporting POS transactions
Stores_w_Sales <- inner_join(gross_sales, phrmcy_master, by="PHRMCY_NBR") 
Stores_w_Sales <- distinct(Stores_w_Sales, PHRMCY_NBR, .keep_all = TRUE)
Stores_w_Sales <- Stores_w_Sales %>%
                  select(PHRMCY_NBR, PHRMCY_NAM,ST_CD, ZIP_3_CD) %>%
                  mutate(ID="Stores Reporting Sales")

All_Stores <- phrmcy_master %>%
  select(PHRMCY_NBR, PHRMCY_NAM, ST_CD, ZIP_3_CD) %>%
  mutate(ID="All Stores")

Store_Breakdown <- union(All_Stores, Stores_w_Sales) 
dbRemoveTable(ABCdb, "Store_Breakdown")
dbWriteTable(ABCdb, "Store_Breakdown", Store_Breakdown)
Store_Tally <- dbGetQuery(ABCdb, "SELECT count(ST_CD) AS Total,
                        ST_CD, ID FROM Store_Breakdown
                         GROUP BY ST_CD, ID")

Store_Tally$Perc <- if_else(Store_Tally$ID == "All Stores",
                            Store_Tally$Total/(nrow(phrmcy_master))*100, 
                            Store_Tally$Total/(nrow(Stores_w_Sales))*100)

write.csv(Store_Tally, "Store_Tally.csv")

# Visualize geographic breakdown of stores reporting sales
# compared to all stores in the ABC pharmacy chain.
Store_Tally_bar <- ggplot(Store_Tally, aes(x = ST_CD, y=Perc, fill=ID)) +
  geom_bar(stat = "identity", position = 'dodge') +
  xlab("State") +
  ylab("Percentage") +
  ggtitle("Geographic Breakdown of ABC Pharmacies")+
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text=element_text(size=16)) +
  theme(plot.title=element_text(size=24))+
  theme(legend.text =element_text(size=16))+
  geom_text(aes(label=round(Perc,0)), 
            position=position_dodge(width=0.9), vjust=-0.25)


# Question 1	
# What is the average monthly revenue for the entire ABC Pharmacy chain 
# for the first six months of 2016?

# Aggregate gross sales data per store on a monthly basis
GrossSalesMonth <- setNames(aggregate(gross_sales$GrossSales,
                            list(gross_sales$PHRMCY_NBR,
                            gross_sales$month), FUN=sum), 
                            c('PHRMCY_NBR', 'Month', 'GrossSales'))

AggMonthSales <- GrossSalesMonth %>% group_by(Month) %>%  summarise(Med = median(GrossSales), mean=mean(GrossSales))
write.csv(AggMonthSales, "AggMonthSales.csv")
#Average sales per month for the entirety of those stores reporting

PharmNum <-GrossSalesMonth %>% group_by(Month) %>% summarise(Tally = n())
TotalMonthSales <- GrossSalesMonth %>% group_by(Month) %>%  summarise(Total = sum(GrossSales))
MonthSalesFinal <- cbind(AggMonthSales, TotalMonthSales$Total, PharmNum$Tally)
MonthSalesFinal$ID <- match(MonthSalesFinal$Month, month.abb)
MonthSalesFinal <- arrange(MonthSalesFinal, ID)
names(MonthSalesFinal) <- c('Month','Median Store Sales', 'Mean Store Sales' , 'Total Gross Sales', '# of Stores', 'ID')
write.csv(MonthSalesFinal, "monthsalesfinal.csv")

# Question 2 - What specific stores in the ABC Pharmacy are responsible 
# for the top 10% of average monthly revenue? 

# Determine average monthly gross sales for each pharmacy reporting sales
# Include details about each pharmacy
dbWriteTable(ABCdb, "GrossSalesMonth", GrossSalesMonth)
AvgMonthSales <- dbGetQuery(ABCdb, "SELECT AVG(GrossSales) AS AverageMonthlySales, 
                  phrmcy_master.PHRMCY_NAM, phrmcy_master.ST_CD, phrmcy_master.ZIP_3_CD
                  FROM GrossSalesMonth JOIN phrmcy_master ON
                  phrmcy_master.PHRMCY_NBR = GrossSalesMonth.PHRMCY_NBR
                  GROUP BY phrmcy_master.PHRMCY_NAM
                  ORDER BY AverageMonthlySales DESC")

write.csv(AvgMonthSales, "AverageMonthlySales.csv")
dbWriteTable(ABCdb, "AvgMonthSales", AvgMonthSales)

# Filter AvgMonthSales (per Store) for top 10%
# Subset relevant columns, arrange by gross sales

SaleTop10 <- AvgMonthSales %>% 
  filter(AverageMonthlySales>quantile(AverageMonthlySales, 0.9)) %>%
  select (AverageMonthlySales, PHRMCY_NAM, ST_CD, ZIP_3_CD) %>%
  arrange (desc(AverageMonthlySales))

write.csv(SaleTop10, "SaleTop10.csv")

# What items do these stores sell the most?
SalesHigh <- SaleTop10 %>% merge(phrmcy_master, by='PHRMCY_NAM') %>%
  merge(gross_sales, by= 'PHRMCY_NBR') %>%
  merge(prod_master, by='PROD_NBR') %>% rename(SEG_CD = SEGMENT_CD) %>%
  merge(prod_seg, by='SEG_CD') %>% merge(prod_sub_cat, by='SUB_CAT_CD') %>%
  merge(prod_cat, by='CAT_CD') %>%
  select(CAT_DESC, AverageMonthlySales, PHRMCY_NBR, PHRMCY_NAM, ST_CD.x, ZIP_3_CD.x) %>%
  group_by(CAT_DESC) %>% summarise(Frequency=n()) %>% arrange(desc(Frequency))

write.csv(SalesHigh, "SalesHigh.csv")

# Question 3 - Are high performing stores clustered in states or zip codes
SalesState_bar <- ggplot(SaleTop10, aes(ST_CD)) +
  geom_bar (stat='count') +
  xlab("State") +
  ylab("Frequency") +
  ggtitle("Geographic Breakdown Stores Having High Gross Sales")+
  theme(axis.title = element_text(size = 26)) +
  theme(axis.text=element_text(size=22, face='bold')) +
  theme(plot.title=element_text(size=28, face='bold'))+
  theme(legend.text =element_text(size=24))
  
# Any high sales stores operating in locations 
# with a low store density that may represent opportunity for further expansion
# Define store density as # of stores per zip3 code

CountperZip <- dbGetQuery(ABCdb, "SELECT count(*) AS StoreDensity, ZIP_3_CD FROM phrmcy_master GROUP BY ZIP_3_CD")
dbWriteTable(ABCdb, "CountperZip", CountperZip)

StoreDensity <- dbGetQuery(ABCdb, "SELECT AvgMonthSales.AverageMonthlySales, AvgMonthSales.PHRMCY_NAM, 
                          AvgMonthSales.ZIP_3_CD, AvgMonthSales.ST_CD, CountperZip.StoreDensity FROM AvgMonthSales 
                          JOIN CountPerZip ON CountPerZip.ZIP_3_CD=AvgMonthSales.ZIP_3_CD")

StoreDensityTop75 <- StoreDensity %>% filter(AverageMonthlySales>quantile(AverageMonthlySales, 0.25))


StoreDensityPlot <- ggplot(StoreDensityTop75, aes(x=StoreDensity, y=AverageMonthlySales, label=PHRMCY_NAM)) +
                    geom_point() +
                    geom_smooth(method=lm) +
                    geom_text(data=subset(StoreDensityTop75,  AverageMonthlySales > 200000), 
                    position=position_dodge(width=0.9), vjust=0, hjust=-0.1, size = 8) +
                    ggtitle("Average Monthly Gross Sales \n as a Function of Store Density") +
                    theme(axis.title = element_text(size = 20, face="bold")) +
                    theme(axis.text=element_text(size=18)) +
                    theme(plot.title=element_text(size=24, face="bold"))+
                    theme(legend.text =element_text(size=18))+
                    xlab("Store Density (#/Zip3)") +
                    ylab("Average Monthly Gross Sales")


StoreDensity1 <- StoreDensityTop75 %>% filter(AverageMonthlySales>200000)
write.csv(StoreDensity1, "StoreDensity1.csv") # opportunities for expansion

# Question 6  - Poor Performance
#  What specific stores in the ABC Pharmacy are responsible 
#  for the bottom 10% of average monthly revenue? 

SaleBottom10 <- AvgMonthSales %>% 
  filter(AverageMonthlySales<quantile(AverageMonthlySales, 0.1)) %>%
  select (AverageMonthlySales, PHRMCY_NAM, ST_CD, ZIP_3_CD) %>%
  arrange (desc(AverageMonthlySales))

write.csv(SaleBottom10, "SaleBottom10.csv")

# Where are the poor performing stores located
SalesPoor_bar <- ggplot(SaleBottom10, aes(ST_CD)) +
  geom_bar (stat='count') +
  xlab("State") +
  ylab("Frequency") +
  ggtitle("Geographic Breakdown Stores Having Low Gross Sales")+
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text=element_text(size=20)) +
  theme(plot.title=element_text(size=28))+
  theme(legend.text =element_text(size=20))

# What items do these stores sell the most?
SalesPoor <- SaleBottom10 %>% merge(phrmcy_master, by='PHRMCY_NAM') %>%
  merge(gross_sales, by= 'PHRMCY_NBR') %>%
  merge(prod_master, by='PROD_NBR') %>% rename(SEG_CD = SEGMENT_CD) %>%
  merge(prod_seg, by='SEG_CD') %>% merge(prod_sub_cat, by='SUB_CAT_CD') %>%
  merge(prod_cat, by='CAT_CD') %>%
  select(CAT_DESC, AverageMonthlySales, PHRMCY_NBR, PHRMCY_NAM, ST_CD.x, ZIP_3_CD.x) %>%
  group_by(CAT_DESC) %>% summarise(Frequency=n()) %>% arrange(desc(Frequency))

write.csv(SalesPoor, "SalesPoor.csv") # Product Categories for poorly performing stores

HomeHealthCare <- prod_cat %>% filter(CAT_DESC == 'HOME HEALTH CARE') %>%
  merge(prod_sub_cat, by='CAT_CD') %>% select(CAT_DESC, SUB_CAT_DESC)

write.csv(HomeHealthCare, "HomeHealthCare.csv")

# Question 9
# Are there any poorly performing (bottom 10%) stores in areas that 
# have a high store density (# per zip) that indicate possible cannibalization issues? 

StoreDensityBottom25 <- StoreDensity %>% filter(AverageMonthlySales<quantile(AverageMonthlySales, 0.25))


StoreDensityPlot_Poor <- ggplot(StoreDensityBottom25, aes(x=StoreDensity, y=AverageMonthlySales, label=str_remove(PHRMCY_NAM, "GNP PHARMACY "))) +
  geom_point(size=2) +
  geom_smooth(method=lm) +
  geom_text(data=subset(StoreDensityBottom25,  StoreDensity > 15), 
            position=position_dodge(width=0), vjust= -2.0, hjust=-0.0, size=5) +
  ggtitle("Average Monthly Gross Sales\n as a Function of Store Density") +
  theme(axis.title = element_text(size = 20, face='bold')) +
  theme(axis.text=element_text(size=18)) +
  theme(plot.title=element_text(size=26, face='bold'))+
  theme(legend.text =element_text(size=18))+
  xlab("Store Density (#/Zip3)") +
  ylab("Average Monthly Gross Sales")

StoreDensityBottom <- StoreDensityBottom25 %>% filter(StoreDensity>15)
write.csv(StoreDensityBottom, "StoreDensityBottom.csv") # Opportunities for Store Closings

#END

