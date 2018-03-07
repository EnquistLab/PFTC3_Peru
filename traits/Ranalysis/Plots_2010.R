# Plot 2010 data

### VEGETATION
ggplot(vegetation, aes(x = Fire, y = Diameter1)) +
  geom_boxplot() +
  facet_wrap(~ Fence)



abundance %>% 
  ggplot(aes(x = Species, y = Abundance, color = Fire)) +
  geom_point() +
  labs(x = "") +
  facet_grid(Fence ~ Site) +
  theme(axis.text.x = element_text(angle = 45))