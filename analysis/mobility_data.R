library(ggplot2)
library(dplyr)
# library(reshape2)
# library(RColorBrewer)

# GOOGLE DATA
g1 <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
g1$date <- lubridate::ymd(g1$date)
g1 <- filter(g1, country_region == "New Zealand" | sub_region_1 %in% c("Washington", "California", "New York", "Florida"))
g1 <- dplyr::filter(g1, date >= lubridate::ymd("2020-03-01"))
g1 <- g1 %>% mutate(region = ifelse(country_region == "New Zealand", country_region, sub_region_1))

# get daily means
g1 <- g1 %>%
  group_by(date, region) %>%
  mutate(MeanTransit = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(MeanWorkplace = mean(workplaces_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(MeanRec = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE))

# plot everything...
# g1 <- g1 %>% dplyr::select(date, region, MeanTransit, MeanWorkplace, MeanRec)
# g1 <- melt(g1, id=c('date', 'region'))
# ggplot(g1, aes(date, value)) + geom_line(colour=myblue) +
#        facet_grid(variable ~region,scales = "free_y" ) +
#        labs(y="% change from baseline", x="") + theme_sleek()



# Just transit, using mean
ggplot(g1, aes(date, MeanTransit, group = region, col = region)) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_line(alpha = 0.5) +
  labs(y = "% change from baseline", x = "") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")


# Or with SE
ggplot(g1, aes(date, transit_stations_percent_change_from_baseline, group = region, col = region)) +
  geom_smooth(method = "gam") +
  geom_line(aes(date, MeanTransit), alpha = 0.5) +
  labs(y = "% change from baseline", x = "") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")
