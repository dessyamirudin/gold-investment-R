setwd("D:/Blog Purpose/Medium")

# load library
library(readxl)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(gtable)
library(ggplot2)
library(ggrepel)

# load data
data = read_excel("Gold Price Monthly USD.xlsx",sheet="Sheet1")
str(data)

# data processing
data$Name = as.Date(data$Name,format = "%Y-%m-%d")
data_10 = data[data$Name>as.Date("2011-12-31",format = "%Y-%m-%d") & data$Name<as.Date("2022-01-01",format = "%Y-%m-%d"),]
data_10 = setNames(data_10,c("Date","Price"))

# calculating growth
col_name_lead = NULL

for (i in 1:120){
  col_name_lead=c(col_name_lead,paste0("lead_",i,"_month"))
}

# lead
for (i in 1:length(col_name_lead)){
  data_10[,col_name_lead[i]]=lead(data_10$Price,i)
}

# growth
col_growth_month = NULL

for (i in 0:120){
  col_growth_month=c(col_growth_month,paste0("growth_",i,"_month"))
}

data_10_growth = data_10[,"Date"]

for (i in 1:length(col_growth_month)){
  data_10_growth[,col_growth_month[i]]=(data_10[,(i+1)]-data_10[,2])/data_10[,2]
}

# median growth
median_growth = data_10_growth%>% summarise_if(is.numeric, median,na.rm=TRUE)
median_growth = median_growth %>% pivot_longer(
  cols=colnames(median_growth),
  names_to = "Month Lapse",
  values_to = "Growth",
  values_drop_na = TRUE
)

# mean growth
mean_growth = data_10_growth%>% summarise_if(is.numeric, mean,na.rm=TRUE)
mean_growth = mean_growth %>% pivot_longer(
  cols=colnames(mean_growth),
  names_to = "Month Lapse",
  values_to = "Growth",
  values_drop_na = TRUE
)

# combine median & mean
growth = cbind(median_growth,mean_growth[,2])
colnames(growth)=c("Month Lapse","Median Growth","Mean Growth")
month_since_invest = seq(0,119)

growth$`Months Since Purchase`=month_since_invest

# label %
data_label = growth %>% 
  filter(`Months Since Purchase` %in% c(12,24,36,48,60,72,84,96,108,119))

# inflation
year_inflation = seq(1,10)
data_compound_inflation = 1*(1.04)^(1:10)
hold_period = paste(year_inflation," year")

# combine
data_label$inflation = data_compound_inflation
data_label$growth_adjusted = data_label$`Median Growth`-(data_label$inflation-1)
data_label$hold_period = hold_period
data_label$hold_period = factor(data_label$hold_period,
                                levels=data_label$hold_period)

data_label = data_label%>% 
  mutate(growth_percent = paste0(round(growth_adjusted*100,digits = 1),'%'))

# bar chart
p1 <- data_label %>%
  ggplot( aes(x=hold_period, y=growth_adjusted)) +
  geom_bar(aes(fill = growth_adjusted),stat="identity",width=0.5)+
  scale_fill_gradient(low = "red", high = "pink", na.value = NA)+
  scale_y_continuous(labels=scales::percent)+
  geom_text_repel(data = data_label,
                  aes(label=growth_percent),
                  nudge_y = -0.02)+
  ggtitle("Return") +
  ylab("Return Inflation Adjusted")+
  xlab("Hold Period")

p1

source_string <- paste0("Source: Return in 10 Year - adjusted for inflation (amirharjo.medium.com)")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(p1))

# Make the source and note text grobs
source_text <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_text)

# save
ggsave("growth 10 years inflation adjusted.jpg", my_gtable, width = 20, height = 12, units = "cm")


# DATA SOURCE
# gold price
# https://www.gold.org/goldhub/data/gold-prices#registration-type=google&just-verified=1

# world inflation
# https://www.macrotrends.net/countries/WLD/world/inflation-rate-cpi
# https://tradingeconomics.com/country-list/inflation-rate
# https://tradingeconomics.com/country-list/inflation-rate-?continent=g20

# indonesia infation
# https://www.bi.go.id/id/statistik/indikator/data-inflasi.aspx
# https://www.bi.go.id/id/statistik/indikator/target-inflasi.aspx