
library(ggplot2)
library(plyr)
library(data.table)


# load the dataset from multiple files in the directory "data"
pbs_atc <- ldply(list.files("data/", full.names = TRUE), fread)

# clean up the names
colnames(pbs_atc) <- tolower(gsub("[.]*", "", colnames(pbs_atc)))

pbs_atc$atc_classification[pbs_atc$atc_classification == "Various +"] <- "Other"
pbs_atc$atc_classification[pbs_atc$atc_classification == "Other ++"] <- "Other"
pbs_atc$atc_classification[pbs_atc$atc_classification == "Anti-Parasitic Products"] <- "Other"


national <- ddply(pbs_atc, c("year", "atc_classification"),  summarise,
      year = max(year),
      total_benefits_billions = sum(benefits / 1e9),
      total_services_billions = sum(services / 1e9)
)

ggplot(national, aes(x = year, y = total_benefits_billions)) +
  geom_bar(stat = "identity", aes( fill = atc_classification)) +
  ggtitle("PBS Benefits") + 
  xlab("Year") + 
  ylab("Billions") + 
  guides(fill = guide_legend(reverse=TRUE)) + 
  scale_fill_discrete(name="ATC Classification")

ggplot(national, aes(x = year, y = total_services_billions)) +
  geom_bar(stat = "identity", aes( fill = atc_classification)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("PBS Services Paid") + 
  xlab("Year") + 
  ylab("Spending in Billions")

