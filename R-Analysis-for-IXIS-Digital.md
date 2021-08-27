IXIS Digital Technical Exercise - Jonathan Huber
================
## The following is a series of steps done to analyze data given in a coding technical challenge, using R to clean and visualize two data sets.


## Data Cleaning

``` r
colnames(datacounts) <- c("Browser", "Device", "Date", "Sessions", "Transactions", "QTY")
#Series of conversions in order to make the Date column properly formatted
a<-datacounts$Date
b<-as.factor(a)
c<-strptime(b,format="%m/%d/%y")
datacounts$Date<-as.Date(c,format="%y-%m-%d") 
```

## Creating the Month \* Device Data Frame

``` r
month_data <- 
  datacounts %>% 
  mutate(Month = format(Date, "%m"), Year = format(Date, "%Y")) %>% #Break the date column up into month and year
  group_by(Year,Month, Device) %>% #Sort by year, then month, then device
  summarise(Sessions = sum(Sessions), Transactions = sum(Transactions), QTY = sum(QTY), ECR = sum(Transactions/Sessions)) #New columns
```

    ## `summarise()` has grouped output by 'Year', 'Month'. You can override using the `.groups` argument.

``` r
#These columns need to be numeric
month_data$Year <- as.numeric(month_data$Year)
month_data$Month <- as.numeric(month_data$Month)
```

## Creating the Month Over Month Data Frame

``` r
merge_data <- 
  datacounts %>% 
  mutate(Month = format(Date, "%m"), Year = format(Date, "%Y")) %>% #Same conversion for month and year as above
  group_by(Year,Month) %>% #Sort just by year and month
  summarise(Sessions = sum(Sessions), Transactions = sum(Transactions), QTY = sum(QTY), ECR = sum(Transactions/Sessions)) #New columns
```

    ## `summarise()` has grouped output by 'Year'. You can override using the `.groups` argument.

``` r
#RConverting columns to numeric
merge_data$Year <- as.numeric(merge_data$Year)
merge_data$Month <- as.numeric(merge_data$Month)

#Format the addstocart database  
colnames(datacart) <- c("Year", "Month", "AddsToCart") #rename columns for merging and format

all_metrics <- 
  merge(datacart,merge_data,by="Month") %>% #merge into one data frame for analysis
  group_by(Month) %>% 
  summarise(Month = Month, AddsToCart = AddsToCart, Sessions = Sessions, Transactions = Transactions, QTY = QTY, ECR =
  sum(Transactions/Sessions), MonthValue = signif(sum((QTY/AddsToCart)*1000), digits = 5), AbsoluteChange = sum(MonthValue-377.63),
  RelativeChange = sum(((MonthValue-377.63)/377.63)*100))
#Define a couple new columns to help calculate the change from the most recent month to the previous month

colnames(all_metrics) <- c("Month", "AddsToCart", "Sessions", "Transactions", "QTY", "ECR", "MonthValue", "AbsoluteChange", "RelativeChange(%)")

recent_months <- all_metrics[5:6,] #Slice the data frame to look at the only months I am interested in
```

## Plotting Transactions per Month by Device

``` r
#Arranging Months in chronological order
month_data$Month <- as.character(month_data$Month)
month_data$Month <- factor(month_data$Month, levels=c("7", "8", "9", "10", "11", "12", "1", "2", "3", "4", "5", "6"))
p1 <- 
  ggplot(data=month_data, mapping = aes(x=Month, y=Transactions, fill=Device)) +
  geom_bar(stat="identity") + ggtitle("Transactions per Month by Device") + theme_minimal()

p1
```

![](R-Analysis-for-IXIS-Digital_files/figure-gfm/ggplot%20Transactions%20per%20Month-1.png)<!-- -->

## Plotting Quanity Purchased per Month by Device

``` r
p2 <- 
  ggplot(data=month_data, aes(x=Month, y=QTY, fill=Device)) +
  geom_bar(stat="identity") + ggtitle("Quantity Purchased per Month by Device") + viridis::scale_fill_viridis(discrete = T)

p2
```

![](R-Analysis-for-IXIS-Digital_files/figure-gfm/ggplot%20Quantiy%20per%20Month%20by%20Device-1.png)<!-- -->

## Plotting MonthValue compared with AddsToCart by Transactions

``` r
p3 <- ggplot(all_metrics, aes(fill=Transactions, y=AddsToCart, x=MonthValue)) + 
  geom_bar(stat="identity", width = 5) + ggtitle("AddsToCart by MonthValue") + viridis::scale_fill_viridis()
p3
```

![](R-Analysis-for-IXIS-Digital_files/figure-gfm/ggplot%20MonthValue%20compared%20with%20AddsToCart,%20by%20Transactions-1.png)<!-- -->

## Plotting the Transactions per Browser in an Ordered Bar Chart

``` r
df <- aggregate(datacounts$Transactions, by=list(datacounts$Browser), FUN=sum)  # aggregate
colnames(df) <- c("Browser", "Transactions")  # change column names
df <- df[order(df$Transactions), ]  # sort
df$Browser <- factor(df$Browser, levels = df$Browser)  # to retain the order in plot.

top_n(df, n=10, Transactions) %>%
  ggplot(., aes(x=Browser, y=Transactions)) + 
  geom_bar(stat="identity", width=.5, fill="blue") + 
  labs(title="Transactions per Browser") + 
  theme(axis.text.x = element_text(angle=75, vjust=0.6))
```

![](R-Analysis-for-IXIS-Digital_files/figure-gfm/ggplot%20Ordered%20Bar%20Chart%20for%20Transactions%20per%20Browser-1.png)<!-- -->

## Creating a workbook

``` r
# Return Months to numeric for xlsx format
month_data$Month <- factor(month_data$Month, levels=c("1", "2", "3", "4", "5", "6","7", "8", "9", "10", "11", "12"))
month_data$Month <- as.numeric(month_data$Month)
# Create the workbook
wb <- buildWorkbook(month_data)
renameWorksheet(wb, "Sheet 1", "Monthly Data by Device Type")
```

## Creating a second worksheet

``` r
addWorksheet(wb, "Month over Month", gridLines = TRUE)
writeData(wb, sheet = "Month over Month", recent_months)
```

## Saving the workbook

``` r
openxlsx::saveWorkbook(wb, file = "GoogleAnalytics.xlsx", overwrite=TRUE)
```

## End of script
