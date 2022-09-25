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
data_20 = data[data$Name>as.Date("2001-12-31",format = "%Y-%m-%d") & data$Name<as.Date("2022-01-01",format = "%Y-%m-%d"),]
data_20 = setNames(data_20,c("Date","Price"))

# calculating growth
col_name_lead = NULL

for (i in 1:180){
  col_name_lead=c(col_name_lead,paste0("lead_",i,"_month"))
}

# lead
for (i in 1:length(col_name_lead)){
  data_20[,col_name_lead[i]]=lead(data_20$Price,i)
}

# growth
col_growth_month = NULL

for (i in 0:180){
  col_growth_month=c(col_growth_month,paste0("growth_",i,"_month"))
}

data_20_growth = data_20[,"Date"]

for (i in 1:length(col_growth_month)){
  data_20_growth[,col_growth_month[i]]=(data_20[,(i+1)]-data_20[,2])/data_20[,2]
}

# median growth
median_growth = data_20_growth%>% summarise_if(is.numeric, median,na.rm=TRUE)
median_growth = median_growth %>% pivot_longer(
  cols=colnames(median_growth),
  names_to = "Month Lapse",
  values_to = "Growth",
  values_drop_na = TRUE
)

# mean growth
mean_growth = data_20_growth%>% summarise_if(is.numeric, mean,na.rm=TRUE)
mean_growth = mean_growth %>% pivot_longer(
  cols=colnames(mean_growth),
  names_to = "Month Lapse",
  values_to = "Growth",
  values_drop_na = TRUE
)
  
# combine median & mean
growth = cbind(median_growth,mean_growth[,2])
colnames(growth)=c("Month Lapse","Median Growth","Mean Growth")
month_since_invest = seq(0,180)

growth$`Months Since Purchase`=month_since_invest

# scatter plot median vs mean
plot(growth$`Median Growth`, growth$`Mean Growth`)

# label %
data_label = growth %>% 
  filter(`Month Lapse`%in% c('growth_60_month','growth_120_month','growth_180_month')) %>% 
  mutate(growth_percent = paste0(round(`Median Growth`*100,digits = 1),'%'))

# line chart
p1 <- growth %>%
  ggplot( aes(x=`Months Since Purchase`, y=`Median Growth`)) +
  scale_y_continuous(labels=scales::percent,breaks=seq(0.25,2.5,by=0.25))+
  scale_x_continuous(breaks = seq(10, 180, by = 10))+
  geom_text_repel(data = data_label,
                  aes(label=growth_percent),
                  nudge_y = 0.2)+
  geom_line(color="#69b3a2") +
  geom_point(color="#69b3a2", size=2) +
  ggtitle("Return") +
  ylab("Median % Return")
p1

source_string <- paste0("Source: Monthly Growth in 20 years (amirharjo.medium.com)")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(p1))

# Make the source and note text grobs
source_text <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
                        
# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_text)

# save
ggsave("growth 15 years.jpg", my_gtable, width = 20, height = 12, units = "cm")


# DATA SOURCE
# gold price
  # https://www.gold.org/goldhub/data/gold-prices#registration-type=google&just-verified=1

# world inflation
  # https://www.macrotrends.net/countries/WLD/world/inflation-rate-cpi
  # https://tradingeconomics.com/country-list/inflation-rate
  # https://tradingeconomics.com/country-list/inflation-rate-?continent=g20

# indonesia infation
  # https://www.bi.go.id/id/statistik/indikator/data-inflasi.aspx