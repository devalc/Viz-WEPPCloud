library(hexSticker)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)


df <- read.csv("D:\\GitHub\\Viz-WEPPCloud\\data\\lt2020_6_hill_summary_with_all_scenarios_04_15_2020.csv")

df <-  df %>%
  dplyr::filter(Watershed %in% "lt_Watershed_11_General")


df <- df %>% group_by(Scenario) %>% arrange_at(.vars = "Sediment.Yield..kg.ha."  , desc) %>%
  mutate(
    cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
    cumPercArea = cumsum(Hillslope.Area..ha.) / sum(Hillslope.Area..ha.) *
      100,
    cumRunoff.mm = cumsum(Runoff..mm.) / sum(Runoff..mm.) *
      100,
    cumLateralflow.mm = cumsum(Lateral.Flow..mm.) / sum(Lateral.Flow..mm.) *
      100,
    cumBaseflow.mm = cumsum(Baseflow..mm.) / sum(Baseflow..mm.) *
      100,
    cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.) / sum(Soil.Loss..kg.ha.) *
      100,
    cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.) /
      sum(Sediment.Deposition..kg.ha.) * 100,
    cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.) /
      sum(Sediment.Yield..kg.ha.) * 100,
    cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.) /
      sum(Solub..React..P..kg.ha.3.) * 100,
    cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.) /
      sum(Particulate.P..kg.ha.3.) * 100,
    cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.) / sum(Total.P..kg.ha.3.) *
      100,
    cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction) /
      sum(Particle.Class.1.Fraction) * 100,
    cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction) /
      sum(Particle.Class.2.Fraction) * 100,
    cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction) /
      sum(Particle.Class.3.Fraction) * 100,
    cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction) /
      sum(Particle.Class.4.Fraction) * 100,
    cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction) /
      sum(Particle.Class.5.Fraction) * 100,
    cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm) /
      sum(Particle.Fraction.Under.0.016.mm) * 100,
    cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) /
      sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) * 100
  ) %>%   ungroup()

colpal <- brewer.pal(n = 11, name = "Paired")

p1 <- df  %>% ggplot(aes(x = cumPercArea, y = cumSedYield.kg.ha, color = Scenario))+ 
  geom_line(size = 1.) + 
  scale_color_colorblind()+
  # scale_color_viridis_d(begin = 0, end = 1,direction = -1,option = "D")+
  theme_dark(base_size = 30)+  theme_transparent()+
 theme(legend.position = "none",
        axis.title = element_blank()) 


s <- sticker(p1,
             package="Viz-WEPPcloud", p_size=28, s_x=1, s_y=.8, s_width=1.4, s_height=1.,
             p_color = "#FFC600", h_fill = "#07294D", h_color = "#FFC600")

s

ggsave("C:/Users/Chinmay/Desktop/vizweppcloud_hex_dark1.png",bg = "transparent")

