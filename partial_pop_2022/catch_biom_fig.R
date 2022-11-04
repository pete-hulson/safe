library(ggplot2)
library(magrittr)
library(dplyr)
library(ggrepel)

dat <-vroom::vroom(here::here('partial_pop_2022', 'catch_biom.csv'))

dat %>%
  mutate(c_ssb = catch / tot,
         cv = tot_sd / tot,
         lci = c_ssb - 1.96 * cv * c_ssb,
         uci = c_ssb + 1.96 * cv * c_ssb,
         lci1 = c_ssb - 0.5 * 1.96 * cv * c_ssb,
         uci1 = c_ssb + 0.5 * 1.96 * cv * c_ssb,
         lci2 = c_ssb - 0.15 * 1.96 * cv * c_ssb,
         uci2 = c_ssb + 0.15 * 1.96 * cv * c_ssb) -> plot_dat

avg <- mean (plot_dat$c_ssb)
end <- plot_dat$c_ssb[length(plot_dat$c_ssb)]
prop_cg <- round(100 * (plot_dat$c_ssb[length(plot_dat$c_ssb)] - plot_dat$c_ssb[length(plot_dat$c_ssb) - 1]) / plot_dat$c_ssb[length(plot_dat$c_ssb) - 1], digits = 0)


# Plot catch / biomass

ggplot(data = plot_dat,
       aes(x = year, y = c_ssb)) +
  theme_bw(base_size = 18) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = "#B3995D"), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lci1, ymax = uci1, fill = "#AA0000"), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lci2, ymax = uci2, fill = "white"), show.legend = FALSE) +
  #geom_line(col = "#AA0000", lwd = 10) +
  geom_line(col = "white", lwd = 2) +
  geom_point(size = 2) +
  geom_hline(yintercept = avg, linetype = "dashed", size = 1.25) +
  geom_label(x = 2022, y = 0.93 * end, label = paste0(prop_cg, "%"), nudge_y = 100, size = 7.77, color = "white", fill = "#AA0000") +
  xlab("Year") +
  ylab("Catch / Biomass") +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside") +
  scale_fill_manual(values = c("#AA0000", "#B3995D", "white")) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1991, 2022.5, by = 2))


dev.print(png, file = here::here('partial_pop_2022', "catch_biom.png"), width = 700, height = 500)
dev.off()




