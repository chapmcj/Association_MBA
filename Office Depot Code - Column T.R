#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)


#read excel into R dataframe
retail <- read_excel("Office Depot Reference Dataset v2.xlsx")


library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(retail,c("OrderID"),
                         function(df1)paste(df1$CondensedRecodedCategories,
                                            collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
transactionData

#set column InvoiceNo of dataframe transactionData  
transactionData$OrderID <- NULL

#Show Dataframe transactionData
transactionData


write.csv(transactionData,"/Users/vinnynguyen/Documents/MBA 739 Adv Data Mining/Project/OD_transactions.csv", quote = FALSE, row.names = FALSE)
#transactionData: Data to be written
#"D:/Documents/market_basket.csv": location of file with file name to be written to
#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.

tr <- read.transactions('/Users/vinnynguyen/Documents/MBA 739 Adv Data Mining/Project/OD_transactions.csv', format = 'basket', sep=',')
#sep tell how items are separated. In this case you have separated using ','

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

# Min Support as 0.01, confidence as 0.2.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.2,target = "rules"))
summary(association.rules)
inspect(sort(association.rules[1:16], by = "lift"))

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.1]
#Plot SubRules
plot(subRules)

top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

