library(dplyr)

data <- read.csv("C:/Chinmay/Github/Process-WEPPCloud-Outputs/data/lt2020_2_hill_summary.csv")
unique_watsheds <- as.character(unique(data$Watershed))
unique_scenario <- as.character(unique(data$Scenario))

data_subset <- subset(data, data$Watershed == unique_watsheds[1])

data_calced <- data_subset %>% group_by(Scenario) %>% arrange(desc(Runoff..mm.)) %>%
    mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
           cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
           cumRunoff.mm = cumsum(Runoff..mm.)/sum(Runoff..mm.)*100,
           cumLateralflow.mm = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
           cumBaseflow.mm = cumsum(Baseflow..mm.)/sum(Baseflow..mm.)*100,
           cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.)/sum(Soil.Loss..kg.ha.)*100,
           cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.)/sum(Sediment.Deposition..kg.ha.)*100,
           cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.)/sum(Sediment.Yield..kg.ha.)*100,
           cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.)/sum(Solub..React..P..kg.ha.3.)*100,
           cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.)/sum(Particulate.P..kg.ha.3.)*100,
           cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.)/sum(Total.P..kg.ha.3.)*100,
           cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction)/sum(Particle.Class.1.Fraction)*100,
           cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction)/sum(Particle.Class.2.Fraction)*100,
           cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction)/sum(Particle.Class.3.Fraction)*100,
           cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction)/sum(Particle.Class.4.Fraction)*100,
           cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction)/sum(Particle.Class.5.Fraction)*100,
           cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm)/sum(Particle.Fraction.Under.0.016.mm)*100,
           cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)/sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)*100) %>% 
    ungroup()

p1 <- data_calced %>% ggplot(aes(cumPercArea, cumRunoff.mm, color= Scenario)) + geom_line() 

p1
    