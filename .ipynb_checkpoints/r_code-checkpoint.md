```r

library(data.table)
library(ggplot2)
library(ggmosaic)

data <‐ fread("QVI_data.csv")

# Convert DATE column to a date format (CSV and Excel integer dates begin on December 30 1899)
data$DATE <‐ as.Date(transactionData$DATE, origin = "1899‐12‐30")

any(is.na(data))

# Total sales by LIFESTAGE and PREMIUM_CUSTOMER
sales <‐ data[, .(SALES = sum(TOT_SALES)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# Total customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <‐ data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# Plot and label with proportion of sales/customers
p <- ggplot(data=sales) + geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) + 
labs(x = 'LifeStage', y = 'Purchasing behavior', title = 'Proportion of Sales') + 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
      plot.title = element_text(size = 16, hjust = 0.5), 
      legend.position = 'none', panel.background = element_blank())

p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y =
(ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))

# Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <‐ data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <‐ data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# plot avg_price/avg_units
ggplot(data = avg_price, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
geom_bar(position = position_dodge()) +
labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# create new column YEARMONTH
data[, YEARMONTH := format(DATE, '%Y%m')]

# define monthly metrics for each store
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = .N/ uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/.N,
                            avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)),
                            by = .(STORE_NBR, YEARMONTH)][order(-YEARMONTH)]

stores_with_full_observation <- measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR]
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% stores_with_full_observation, ]

# Create a function to compute the correlation of the selected metric between trial and control stores
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {

calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
storeNumbers <- unique(inputTable[STORE_NBR != storeComparison, STORE_NBR])

for (i in storeNumbers) {
calculatedMeasure = data.table(Store1 = storeComparison, 
                               Store2 = i, 
                               corr_measure = cor(inputTable[STORE_NBR == storeComparison, get(metricCol)], 
                                                  inputTable[STORE_NBR == i, get(metricCol)])
                              )
calcCorrTable <- rbind(calcCorrTable, calculatedMeasure) 
}
return(calcCorrTable) 
}

# Create a function that shows how close the actual metric values are
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {

calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
storeNumbers <- unique(inputTable[STORE_NBR != storeComparison, STORE_NBR])

for (i in storeNumbers) {
calculatedMeasure = data.table(Store1 = storeComparison, 
                               Store2 = i, 
                               YEARMONTH = inputTable[STORE_NBR == storeComparison, YEARMONTH], 
                               measure = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)] -                                                     inputTable[STORE_NBR == i,eval(metricCol)])
                              )
calcDistTable <- rbind(calcDistTable, calculatedMeasure)
}

minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = c("Store1", "YEARMONTH")]
distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =.(Store1, Store2)]
return(finalDistTable)
}


# set the trial store 
trial_store <- 77

corr_nSales <- calculateCorrelation(preTrialMeasures, 'totSales', trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, 'nCustomers', trial_store)
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

# give equal importance to both measures
corr_weight <- 0.5

score_nSales <- merge(corr_nSales, magnitude_nSales, by = c('Store1', 'Store2'))[, scoreNSales := corr_weight * (corr_measure + mag_measure)]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c('Store1', 'Store2'))[, scoreNCusts := corr_weight * (corr_measure + mag_measure)]

score_Control <- merge(score_nSales, score_nCustomers, by = c('Store1', 'Store2'))
score_Control[, finalControlScore := corr_weight * (scoreNSales + scoreNCusts)]

# find the control store number
control_store <- score_Control[order(-finalControlScore), Store2][1] 

# copy() prevents modifying the original data
measureOverTimeSales <- copy(measureOverTime)
measureOverTimeCusts <- copy(measureOverTime)

# calculate mean monthly sales and number of customers by store type and convert YEARMONTH to date
Sales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, 'Trial', ifelse(
STORE_NBR == control_store, 'Control', 'Other stores'))
][, totSales := mean(totSales), by = .(Store_type, YEARMONTH)
][, TransactionMonth := as.Date(paste(as.numeric(YEARMONTH) %/% 100,as.numeric(YEARMONTH) %% 100, 1,sep ='-'), '%Y-%m-%d')]

Customers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, 'Trial', ifelse(
STORE_NBR == control_store, 'Control', 'Other stores'))
][, nCustomers := mean(nCustomers), by = .(Store_type, YEARMONTH)
][, TransactionMonth := as.Date(paste(as.numeric(YEARMONTH) %/% 100,as.numeric(YEARMONTH) %% 100, 1,sep ='-'), '%Y-%m-%d')]


# plot totSales and nCustomers for each Store_type by month
ggplot(Sales[YEARMONTH < 201902], aes(TransactionMonth, totSales, color = Store_type)) + geom_line() + scale_x_date(date_labels = '%b %Y', date_breaks = '1 month')

ggplot(Customers[YEARMONTH < 201902], aes(TransactionMonth, nCustomers, color = Store_type)) + geom_line() + scale_x_date(date_labels = '%b %Y', date_breaks = '1 month')


# Adjust metrics of interest for fairer comparison between trial and control stores
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store, sum(totSales)] / 
preTrialMeasures[STORE_NBR == control_store, sum(totSales)]

scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][, controlSales := totSales * scalingFactorForControlSales]


scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store, sum(nCustomers)] /
preTrialMeasures[STORE_NBR == control_store, sum(nCustomers)]

scaledControlCustomers <- measureOverTimeCusts[Store_type == 'Control',
][, controlCustomers := nCustomers * scalingFactorForControlCust]


# Calculate the percentage difference between scaled control and trial metrics
percentage_diff_sales <‐ merge(
scaledControlSales[, c("YEARMONTH", "controlSales")],
measureOverTime[STORE_NBR == trial_store, c("totSales","YEARMONTH")],
by = "YEARMONTH"
)[, percentageDiff := (abs(controlSales‐totSales)/controlSales) * 100]


percentage_diff_customers <- merge(
scaledControlCustomers[, c("YEARMONTH", "controlCustomers")], 
measureOverTimeCusts[STORE_NBR == trial_store, c("nCustomers","YEARMONTH")], 
by = "YEARMONTH"
)[, percentageDiff := (abs(controlCustomers‐nCustomers)/controlCustomers) * 100]


# Calculate standard deviation
stdDev <- sd(percentage_diff_sales[YEARMONTH < 201902, percentageDiff])

stdDev <- sd(percentage_diff_customers[YEARMONTH < 201902, percentageDiff])


# 5th and 95th percentile values of sales and customers of the control store
Sales_Controls5 <- Sales[STORE_NBR == control_store,
][, totSales := totSales * (1 - (stdDev/100) *2)
][, Store_type := 'Control 5th % confidence interval']

Sales_Controls95 <- Sales[Store_type == 'Control',
][, totSales := totSales * (1 + (stdDev/100) *2)
][, Store_type := 'Control 95th % confidence interval']


Customers_Control5 <- Customers[Store_type == 'Control',
][, nCustomers := nCustomers * (1 - (stdDev/100) * 2)
][, Store_type := 'Control 5th % confidence interval']

Customers_Control95 <- Customers[Store_type == 'Control',
][, nCustomers := nCustomers * (1 + (stdDev/100) * 2)
][, Store_type := 'Control 95th % confidence interval']


# Combine all control and trial store data and plot it
trialAssessment <- rbind(Sales[Store_type %in% c('Trial', 'Control')], Sales_Controls5, Sales_Controls95)

trialAssessment <- rbind(Customers[Store_type %in% c('Trial', 'Control')], Customers_Controls5, Customers_Controls95)



ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) + 

geom_rect(data = trialAssessment[YEARMONTH > 201901 & YEARMONTH < 201905, ], aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) + 

geom_line(aes(linetype = ifelse(Store_type %in% c("Control 95th % confidence interval", "Control 5th % confidence interval"), "dotted", "solid"))) +                    

scale_linetype_manual(values = c(solid = "solid", dotted = "dotted")) + 

guides(linetype = 'none') +

labs(x = 'Month of operation', y = 'Total sales', title = 'Total sales by month')

```