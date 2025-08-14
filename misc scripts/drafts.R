










wei_region_investment%>%
  filter(location == "world") %>%
  ggplot(aes(x = year, y = c()))









by_sector <- wei_region_investment %>%
 select(year, 
        location, 
        power_generation_renewables_total_usd_billion_2024_mer,
        power_generation_oil_gas_unabated_usd_billion_2024_mer,
        power_generation_coal_unabated_usd_billion_2024_mer,
        power_generation_nuclear_usd_billion_2024_mer)



placeholder <- by_sector%>%
  filter(location == "world") %>%
  pivot_longer(cols = -c(location, year),
               names_to = "Indicator",
               values_to = "figures") %>%
  mutate(year= as.integer(year),
         Indicator = as.factor(Indicator)) %>%
  mutate(Indicator = recode(Indicator,
                            power_generation_renewables_total_usd_billion_2024_mer = "Renewables",
                            power_generation_oil_gas_unabated_usd_billion_2024_mer = "Oil & Gas (Unabated)",
                            power_generation_coal_unabated_usd_billion_2024_mer = "Coal (Unabated)",
                            power_generation_nuclear_usd_billion_2024_mer = "Nuclear"
         )
         ) %>%
  mutate(Indicator =factor(Indicator,
                           levels = c("Renewables",
                                      "Oil & Gas (Unabated)",
                                      "Coal (Unabated)",
                                      "Nuclear")
  )
  )


mean_indicator <- placeholder %>%
  group_by(Indicator) %>%
  summarise(avg = mean(figures))


                            
                            
placeholder %>%
  ggplot(aes(x = year, y = figures, colour = Indicator)) +
  geom_line(size = 1) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2015:2025) +
  facet_wrap(~ Indicator,
             scales = "free") +
  geom_hline(data = mean_indicator,
             aes(yintercept = avg),
             linetype = "dashed")+
  ylab("Total investment (in bn USD)") +
  xlab("Year") +
  ggtitle("Energy investment by sector (2015 – 2025)",
          "Data from World Energy Investment 2025 datafile") +
  ggthemes::theme_clean() +
  theme(legend.position = "top") 

placeholder






  wei_region_