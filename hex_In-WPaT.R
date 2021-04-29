library(hexSticker)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rcartocolor)
library(cowplot)
library(RColorBrewer)


df <- read.csv("D:\\GitHub\\Viz-WEPPCloud\\old_data\\lt2020_6_hill_summary_with_all_scenarios_04_15_2020.csv")

df <-  df %>%
    dplyr::filter(Watershed %in% "lt_Watershed_11_General")

spdf <- readRDS("D:\\GitHub\\Viz-WEPPCloud\\old_data\\lt_202010_shps_subcatchments_wgs84_split_wshed_and_scen.RDS")

spdf <-  spdf %>%
    dplyr::filter(Watershed %in% "55_Meeks_Creek" & Scenario %in% "HighSev")


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
    theme_wsj(base_size = 30)+  theme_transparent()+
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) 
p1

ggm2 = ggplot() + 
    geom_sf(data = spdf, aes(fill = SdYd_kg_ha), size = 0.00001, alpha =1) +
    scale_fill_carto_c(palette = "SunsetDark") +
    theme_void() +  
    theme(legend.position = "none")

ggm2

p2<- cowplot::plot_grid(p1,ggm2, labels = "", rel_widths = c(2, 1.5))

s <- sticker(p2,
             package="In-WPaT", p_size=40, s_x=1., s_y=.8, s_width=1.4, s_height=1.,
             p_color = "#eb5959", h_fill = "#cfcfcf", h_color = "#023a75")
# h_fill = "#07294D"
s

ggsave("./www/In-WPaT_hex.svg",device = "svg",width = 8,height = 8,
       plot = s,
       bg = "transparent")

