
#NEWS data
library(ggplot2)
news <- read.csv("~/Desktop/BIS 244/NEWS.csv")
summary(news)
#Bar Chart Issues
issues <- subset(news, related_topic_tags == "Negligence" | related_topic_tags == "Plastics" | related_topic_tags == "Indigenous people" | related_topic_tags == "Greenhouse gas (GHG) emissions" | related_topic_tags == "Land ecosystems")
issues$related_topic_tags <- as.factor(issues$related_topic_tags)
issues$RepRisk_ID <- as.factor(issues$RepRisk_ID)

ggplot(data = issues, mapping = aes(x=related_topic_tags, fill = RepRisk_ID)) + geom_bar(position = "dodge")+ 
  labs(x = "Issue", 
       y = "Frequency",
       title = "Number of Issues for Chevron vs Exxon") + scale_fill_manual(name = "Company", values = c("blue", "red"), labels = c("Chevron", "Exxon")) 


#RRI data
rri <- read.csv("~/Desktop/BIS 244/RRI.csv")
library(ggplot2)
#Pie Chart Percentages
percentChevron <- rri[72,] #Most recent 12/31/2020 
percentExxon <- rri[144,] #Most recent 12/31/2020
pChev <-  c(51, 31,18)
lblChev <-  c("Environmental","Social","Governance")

pie(pChev, labels = pChev, main = "Pie Chart for Chevron's Most Recent Risk Percentages",col = rainbow(length(pChev)))
legend("topright", c("Environmental","Social","Governance"), cex = 0.9,
       fill = rainbow(length(pChev)))

pExx <-  c(53, 30,17)
lblExx <-  c("Environmental","Social","Governance")

pie(pExx, labels = pExx, main = "Pie Chart for Exxon's Most Recent Risk Percentages",col = rainbow(length(pExx)))
legend("topright", c("Environmental","Social","Governance"), cex = 0.9,
       fill = rainbow(length(pExx)))

#RRI Trends Scatter plot
rriChev <- subset(rri, name == "Chevron Corp (Chevron)")
rriExx <- subset(rri, name == "Exxon Mobil Corp (ExxonMobil; Esso)")

#Chevron
p <- ggplot(data = rriChev, 
            mapping = aes(x = current_RRI, y = RRI_trend))
p + geom_point(color = "blue", alpha = 0.3) +
  geom_smooth(method = "lm", color = "yellow", fill = "red") +
  labs(x = "Current RRI", 
       y = "RRI Trend",
       title = "Chevron RRI Trend Scatterplot",
       caption = "Source: wrds.")

#Exxon
p <- ggplot(data = rriExx, 
            mapping = aes(x = current_RRI, y = RRI_trend))
p + geom_point(color = "blue", alpha = 0.3) +
  geom_smooth(method = "lm", color = "yellow", fill = "red") +
  labs(x = "Current RRI", 
       y = "RRI Trend",
       title = "Exxon RRI Trend Scatterplot",
       caption = "Source: wrds.")

#environmental percentage over rri trend
p <- ggplot(data = rriExx, 
            mapping = aes(x = environmental_percentage, y = current_RRI))
p + geom_point(color = "blue", alpha = 0.3) +
  labs(x = "Environmental Percentage", 
       y = "Current RRI",
       title = "Exxon Environmental Risk Percentage with Current RRI",
       caption = "Source: wrds.")

p <- ggplot(data = rriChev, 
            mapping = aes(x = environmental_percentage, y = current_RRI))
p + geom_point(color = "blue", alpha = 0.3) +
  labs(x = "Environmental Percentage", 
       y = "Current RRI",
       title = "Chevron Environmental Risk Percentage with Current RRI",
       caption = "Source: wrds.")

