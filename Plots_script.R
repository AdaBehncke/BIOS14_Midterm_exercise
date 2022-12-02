
load("data_subsets.RData")
load("models.RData")

library(tidyverse)  #Includes ggplot 2 for plotting
library(grid)
library(gridExtra)  #Includes further plot customisation tools

invlogit <- function(data) 1/(1 + exp(-data))    #Inverse logit function. Needed to plot the model fit line

#-------------------------------------------------------------------------------------------------------------
  
# A plot is made for each population showing the data distribution, the model fit.
# The effect of variation in seed size (mean +/- sd) on the model fit is visually shown for each population
  
CCplot <- ggplot(data = CCdat, aes(timetosowing, germ2)) +
  geom_point(shape = 1, size = 2) +
  geom_line(data = predvals_CC, aes(xvals_CC, invlogit(y_hat_CC)), linewidth = 1.25) +
  geom_line(data = predvals_CC, aes(xvals_CC, invlogit(y_hat_CC_big)), linewidth = 1.0, linetype = "dashed" ) +
  geom_line(data = predvals_CC, aes(xvals_CC, invlogit(y_hat_CC_small)), linewidth = 1.0, linetype = "dashed" ) +
  labs(x = "After-ripening duration (days)", y = "Germination rate") +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[1,2], xmax = T50vals[1,2], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[1,3], xmax = T50vals[1,3], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[1,4], xmax = T50vals[1,4], ymin = -0.1, ymax = 0.5) +
  labs(title = "a)")+
  scale_x_continuous(breaks = seq(25, 175, by=25)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, vjust = 3),
        axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black'),
        axis.title.x = element_text(margin = margin(t=20, r=0, b=0, l=0)),
        axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0)),
        plot.margin = margin(0.5, 0.5, 0.1, 0, "cm") 
  )

print(CCplot)



LMplot <- ggplot(data = LMdat, aes(timetosowing, germ2)) +
  geom_point(shape = 1, size = 2) +
  geom_line(data = predvals_LM, aes(xvals_LM, invlogit(y_hat_LM)), linewidth = 1.25) +
  geom_line(data = predvals_LM, aes(xvals_LM, invlogit(y_hat_LM_big)), linewidth = 1.0, linetype = "dashed" ) +
  geom_line(data = predvals_LM, aes(xvals_LM, invlogit(y_hat_LM_small)), linewidth = 1.0, linetype = "dashed" ) +
  labs(x = "After-ripening duration (days)", y = "Germination rate") +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[2,2], xmax = T50vals[2,2], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[2,3], xmax = T50vals[2,3], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[2,4], xmax = T50vals[2,4], ymin = -0.1, ymax = 0.5) +
  labs(title = "b)")+
  scale_x_continuous(breaks = seq(25, 175, by=25)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, vjust = 3),
        axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black'),
        axis.title.x = element_text(margin = margin(t=20, r=0, b=0, l=0)),
        axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0)),
        plot.margin = margin(0.5, 0.25, 0.1, 0, "cm")
  )

print(LMplot)


PMplot <- ggplot(data = PMdat, aes(timetosowing, germ2)) +
  geom_point(shape = 1, size = 2) +
  geom_line(data = predvals_PM, aes(xvals_PM, invlogit(y_hat_PM)), linewidth = 1.25) +
  geom_line(data = predvals_PM, aes(xvals_PM, invlogit(y_hat_PM_big)), linewidth = 1.0, linetype = "dashed" ) +
  geom_line(data = predvals_PM, aes(xvals_PM, invlogit(y_hat_PM_small)), linewidth = 1.0, linetype = "dashed" ) +
  labs(x = "After-ripening duration (days)", y = "Germination rate") +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[3,2], xmax = T50vals[3,2], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[3,3], xmax = T50vals[3,3], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[3,4], xmax = T50vals[3,4], ymin = -0.1, ymax = 0.5) +
  labs(title = "c)")+
  scale_x_continuous(breaks = seq(25, 175, by=25)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, vjust = 3),
        axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black'),
        axis.title.x = element_text(margin = margin(t=20, r=0, b=0, l=0)),
        axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0)),
        plot.margin = margin(0.5, 0.5, 0.1, 0, "cm")
  )

print(PMplot)


Tplot <- ggplot(data = Tdat, aes(timetosowing, germ2)) +
  geom_point(shape = 1, size = 2) +
  geom_line(data = predvals_T, aes(xvals_T, invlogit(y_hat_T)), linewidth = 1.25) +
  geom_line(data = predvals_T, aes(xvals_T, invlogit(y_hat_T_big)), linewidth = 1.0, linetype = "dashed" ) +
  geom_line(data = predvals_T, aes(xvals_T, invlogit(y_hat_T_small)), linewidth = 1.0, linetype = "dashed" ) +
  labs(x = "After-ripening duration (days)", y = "Germination rate") +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[4,2], xmax = T50vals[4,2], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[4,3], xmax = T50vals[4,3], ymin = -0.1, ymax = 0.5) +
  annotation_custom(linesGrob(gp = gpar(lty = "dotted", lwd = 0.75)), 
                    xmin = T50vals[4,4], xmax = T50vals[4,4], ymin = -0.1, ymax = 0.5) +
  labs(title = "d)")+
  scale_x_continuous(breaks = seq(25, 175, by=25)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, vjust = 3),
        axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black'),
        axis.title.x = element_text(margin = margin(t=20, r=0, b=0, l=0)),
        axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0)),
        plot.margin = margin(0.5, 0.25, 0.1, 0, "cm")
  )

print(Tplot)

#-----------------------------------------------------------------------------------------------

#All plots are bound together into a single figure

Full_plot <- grid.arrange(CCplot, LMplot, PMplot, Tplot, ncol=2)


#-----------------------------------------------------------------------------------------------

#All graphic objects are save in a RData file

save(CCplot, LMplot, PMplot, Tplot, Full_plot, file = "figures.RData")
