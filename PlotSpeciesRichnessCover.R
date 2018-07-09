#### Plot species cover and richness

### Cover in each site and by successionalStage
cover %>% 
  group_by(site, species, successionalStage) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>% 
  filter(!is.na(mean)) %>% 
  filter(site == "WAY_3100m", successionalStage == "Early") %>% 
  ggplot(aes(x = species, y = mean, ymin = mean - se, ymax = mean + se, color = species)) +
  geom_point() +
  geom_errorbar() +
  labs(x = "", y = "Mean cover in %") +
  #facet_grid(treatment ~ site) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


# Cover by functional Group
coverPlot <- cover %>% 
  group_by(site, successionalStage, functionalGroup) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>%   
  filter(!is.na(functionalGroup), successionalStage != "Recentt") %>% 
  ggplot(aes(x = functionalGroup, y = mean, ymin = mean - se, ymax = mean + se, color = functionalGroup)) +
  geom_point(size = 3) +
  geom_errorbar() +
  labs(x = "", y = "Mean cover in %") +
  facet_grid(successionalStage ~ site) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15))

ggsave(coverPlot, filename = "cover.jpeg", dpi = 300, height = 8, width = 12)

# Species richness
total <- cover %>% 
  group_by(site, successionalStage) %>% 
  distinct(species) %>% 
  summarise(total = n())

richnessPlot <- cover %>% 
  filter(!is.na(functionalGroup)) %>% 
  group_by(site, successionalStage, functionalGroup) %>% 
  summarise(count = n_distinct(species)) %>% 
  left_join(total, by = c("site", "successionalStage")) %>%
  ggplot(aes(x = functionalGroup, y = count, fill = functionalGroup)) +
  geom_bar(stat="identity") +
  labs(x = "", y = "Species richness (count)") +
  geom_text(aes(x = 4, y = 15, label = paste("total = ", total))) +
  facet_grid(successionalStage ~ site) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15))
ggsave(richnessPlot, filename = "richness.jpeg", dpi = 300, height = 8, width = 12)