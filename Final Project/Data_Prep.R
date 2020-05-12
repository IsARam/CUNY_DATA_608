#Libraries
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(RColorBrewer)

#Data Load
Performance_DF = read.csv("./Performance_LIRR.csv",stringsAsFactors = FALSE)

df <- readr::read_csv("./TidyPerformance.csv")

#Clean Text
clean_text <- function(x,y) {
  z = gsub(y,'', x)
  return(z)
}

#Tidy Data
TidyPerformancedf = Performance_DF%>%
  filter(., Parent.Sequence == 20421 | Indicator.Sequence == 20421)%>%
  select(., Indicator.Name, Period.Month, Period.Year, 
         YTD.Target, YTD.Actual, Monthly.Target, Monthly.Actual, Period)%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"Branch - OTP"))%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"Branch OTP"))%>%
  mutate(., Indicator.Name = str_trim(Indicator.Name))%>%
  mutate(., Month_Name = month.name[Period.Month])

write.csv(TidyPerformancedf, file = "TidyPerformance.csv",row.names=FALSE)


#Average Branch Performance By Month
df = TidyPerformancedf%>%
  filter(., Indicator.Name == "Oyster Bay")%>%
  group_by(., Period.Month)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))

g = ggplot(df, aes(x = month.abb[Period.Month], y = Average_OTP, group = 1)) 
g + 
  #geom_bar(stat = 'identity', position = 'dodge', fill = 'Green') + 
  geom_point() +
  geom_line() +
  xlab("Month") + ylab('Percentage On-Time, (%)') + scale_x_discrete(labels=month.abb) + 
  theme(plot.title = element_text(hjust = 0.7)) +
  ggtitle('Branch OTP By Month')


#Average Branch Performance
df1 = TidyPerformancedf%>%
  filter(., Indicator.Name != 'On-Time Performance')%>%
  group_by(Indicator.Name)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(.,(Average_OTP))

#df$Average_OTP = format(round(df$Average_OTP, 2), nsmall = 2)
df1$Indicator.Name <- factor(df1$Indicator.Name, levels = df1$Indicator.Name)
g1 = ggplot(df, aes(x = Indicator.Name, y = Average_OTP, fill = Indicator.Name )) 
g1 + 
  geom_col() + coord_flip() + scale_fill_brewer(palette="Set3") +
  xlab("Branch") + ylab("Average OTP (%)") + 
  theme(plot.title = element_text(hjust = 0.7)) +
  ggtitle('Average On-Time Performance') +
  geom_text(aes(label=format(round(df1$Average_OTP, 2), nsmall = 2)), hjust = 1.5, size=3.0)
#############################################################

#Fix Plot
df_overall = TidyPerformancedf%>%
  filter(., Indicator.Name == "On-Time Performance")%>%
  group_by(., Period.Month)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(., desc(Average_OTP))
#########################################################################

#Tidy text
change_text <- function(x,y,z) {
  return(gsub(pattern = x, replacement = y, x = z))
}

#Clean up plot
for (i in 1:nrow(TidyPerformancedf)){
  TidyPerformancedf$Indicator.Name[i] = change_text("On-Time Performance",
                                                       "Overall",
                                                       TidyPerformancedf$Indicator.Name[i])
}
df_comb = df%>%
  filter(., Indicator.Name == "Montauk" | Indicator.Name == "Overall")%>%
  group_by(., Indicator.Name, Period.Year)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(., desc(Average_OTP))

df_comb2 = df_comb%>%
  filter(., Indicator.Name == "Montauk")%>%
  arrange(., desc(Average_OTP))


ggplot(df_comb, aes(x = Period.Year, y = Average_OTP)) +
  geom_line(aes(color = Indicator.Name)) +
  xlab("Month") + ylab('Percentage On-Time, (%)') +  
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Branch On-Time Performance By Year Since 2008') +
  scale_x_continuous(breaks = unique(df_comb$Period.Year))

top1 <- head(df_comb, 1)
bot1 <- tail(df_comb, 1)

##############################################################################

#Overall OTP By Month

df = TidyPerformancedf%>%
  filter(., Indicator.Name == "Port Jefferson")%>%
  group_by(., Indicator.Name, Period.Year)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(., desc(Average_OTP))

ggplot(df, aes(x = factor(Period.Year), y = Average_OTP, group = 1)) +
  geom_line(aes(color = 'red')) +
  xlab("Year") + ylab('Percentage On-Time, (%)') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Branch On-Time Performance By Year') +
  theme(legend.title = element_blank()) +
  scale_color_manual(labels = c(df$Indicator.Name), values = c("red"))

temp_df = TidyPerformancedf%>%
  filter(., Indicator.Name != "On-Time Performance")%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"Hicksville/"))%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"/Ronkonkoma"))

branches = as.data.frame(unique(temp_df$Indicator.Name))
branches = select(branches, Indicator.Name = 'unique(temp_df$Indicator.Name)')

list2 <- rep(stops_df4$stop_lat,NROW(stop_df3))
list3 <- cbind(list2, list1)

##################################


#Overall delay 
test_df = TidyPerformancedf%>%
  filter(., Indicator.Name != 'On-Time Performance')%>%
  filter(., Indicator.Name != 'Hicksville/Huntington')%>%
  group_by(., Indicator.Name)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(., desc(Average_OTP))

assoc_df1 = left_join(assoc_df, test_df, by = 'Indicator.Name' )
write.csv(assoc_df1, file = "assoc_df.csv",row.names=FALSE)


library("colorspace")
pal <- choose_palette()

