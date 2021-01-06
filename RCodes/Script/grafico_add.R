library(ggplot2)
theme_set(theme_bw())
data <- data.frame(t=c(0:40), p = c(rep(1.95,5),rep(1.90,5),rep(2.00,5),rep(2.01,5),rep(2.10,5),
                                    rep(2.20,9),rep(2.25,7)),
                   s = c(rep(1.03,5),rep(0.90,5),rep(1.00,2),rep(1.02,5),rep(1.04,3),1.05,1.06,1.07,1.10,1.15,
                         rep(1.06,4),rep(1.10,4),rep(1.11,6),rep(1.14,2)))

ggplot(data) + geom_line(aes(x = t, y =p, color = "log.Precio online"), size = 1.00) +
  geom_line(aes(x = t, y = s, color = "log.Tipo de cambio nominal"), size = 1.00)+
  labs(y = NULL, x = "tiempo")+
  scale_color_manual("Leyenda",
                     values = c("log.Precio online" = "#386cb0","log.Tipo de cambio nominal" ="#fdb462"))+
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(0:40)) +
  geom_vline(xintercept = c(25,33,13,5), color="red")
  
  

