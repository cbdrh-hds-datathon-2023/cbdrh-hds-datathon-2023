# for (i in 1:100) {
#   cat("![](images/Lo Res/Datathon_UNSW_Credit_CassandraHannagan-", i, ".jpg){group='my-gallery'}", sep='')
# }


library(ggplot2)
library(dplyr)
library(gganimate)

df <- read.csv(here::here('raw-data/HealthGymV2_CbdrhDatathon_ART4HIV.csv')) %>%
  mutate(combo = factor(Base.Drug.Combo, levels = 0:5,
                        labels = c('FTC+\nTDF',
                                   '3TC+\nABC',
                                   'FTC+\nTAF',
                                   'DRV+\nFTC+\nTDF',
                                   'FTC+\nRTVB+\nTDF',
                                   'Other')))



id = sample(unique(df$PatientID), 4)

anim <- ggplot() +
          geom_line(data = df %>% filter(PatientID %in% id),
                     aes(x=Timestep, y = CD4),
                     size=.6, color = '#289a92') +
          geom_text(data = df %>% filter(PatientID %in% id),
                    aes(x=Timestep, y = CD4, label=combo, color=combo, group=1L),
                    size=5, nudge_x = 5) +
          scale_y_continuous(limits = c(0,NA)) +
          scale_x_continuous(limits = c(0, 65)) +
          scale_color_manual(values = c('#ED217C', '#FFFD82', '#FF9B71', '#FFCC7A', '#8eedf7', '#adaabf')) +
          facet_wrap(~PatientID) +
          labs(title = 'CD4 counts over time and base drug combination',
               subtitle = "Month: {(frame_along)}") +
          theme_void() +
          theme(
            legend.position = 'none',
            plot.title = element_text(color='#289a92', size=20),
            plot.subtitle = element_text(color = '#b2c1c4', size=18),
            plot.background = element_rect(fill='#002b36'),
            strip.text = element_text(size=0),
            panel.grid.major.y = element_line(color='#b2c1c4', size=.05)
            ) +
          transition_reveal(Timestep)


animate(anim, height = 500, width = 800, fps = 20, duration = 30,
        end_pause = 30, res = 100)
anim_save(here::here("images/cd4-over-time.gif"))


## Roughwork

# ggplot(data = df, aes(x=Timestep, y = VL, group=PatientID)) +
#   geom_line(linewidth=.2, color = '#289a92') +
#   geom_line(data = . %>% filter(PatientID==1),
#             linewidth=1, color = '#FF9B71') +
#   geom_line(data = . %>% filter(PatientID==138),
#             linewidth=1, color = '#ED217C') +
#   scale_x_continuous("Time (month)") +
#   scale_y_continuous(bquote('CD4 count '(cells/mm^3)), label = scales::comma) +
#   labs(title = 'CD4 counts over time', subtitle = 'well head') +
#   #scale_y_log10() +
#   theme(
#     legend.position = 'none',
#     plot.title = element_text(color='#289a92', size=20),
#     plot.subtitle = element_text(color = '#b2c1c4', size=18),
#     plot.background = element_rect(fill='#002b36'),
#     panel.background = element_rect(fill='#002b36', color = NA),
#     panel.grid.major.y = element_line(color='#b2c1c4', size=.05),
#     axis.text = element_text(color = '#b2c1c4', size=12),
#     axis.title = element_text(color = '#b2c1c4', size=14),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank()
#   )
#
#
#
#
# #+
# geom_text(data = df %>% filter(PatientID %in% id),
#           aes(x=Timestep, y = CD4, label=combo, color=combo, group=1L),
#           size=5, nudge_x = 5) +
#   scale_y_continuous(limits = c(0,NA)) +
#   scale_x_continuous(limits = c(0, 65)) +
#   scale_color_manual(values = c('#ED217C', '#FFFD82', '#FF9B71', '#FFCC7A', '#8eedf7', '#adaabf')) +
#   facet_wrap(~PatientID) +
#   labs(title = 'CD4 counts over time and base drug combination',
#        subtitle = "Month: {(frame_along)}") +
#   theme_void() +
#   theme(
#     legend.position = 'none',
#     plot.title = element_text(color='#289a92', size=20),
#     plot.subtitle = element_text(color = '#b2c1c4', size=18),
#     plot.background = element_rect(fill='#002b36'),
#     strip.text = element_text(size=0),
#     panel.grid.major.y = element_line(color='#b2c1c4', size=.05)
#   )
#
#
# df %>%
#   group_by(PatientID) %>%
#   mutate(mean = mean(CD4)) %>%
#   select(mean) %>%
#   slice(n=1) %>%
#   ungroup() %>%
#   arrange(-mean) %>%
#   mutate(id = row_number()) %>%
#   left_join(df) %>%
#   ggplot(aes(x=Timestep, y=PatientID, fill=combo)) +
#   geom_tile()
