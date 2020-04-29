###############################################################
#                                                             #
#                                                             #
##### Philip Jacobson Script MS IV PhD thesis, 2020-04-29 #####
#####           R version 3.6.2 (2019-12-12)              #####
#                                                             #
#                                                             #
###############################################################


##### Load packages

library(dplyr)                # v. 0.8.3
library(ggplot2)              # v. 3.2.1
library(tidyr)                # v. 1.0.0
library(performance)          # v. 0.5.1
library(tibble)               # v. 2.1.3
library(RColorBrewer)         # v. 1.1-2
library(viridis)              # v. 0.5.1


#### Function plots in appendix ####

# Resources ####
# Semi-chemostat dynamics:

# River:

# Create data:

  p<-seq(0.1,0.1, length=500)
  Rmax<-seq(30, 30, length=500)
  R<-seq(0, 30, length=500)

  # Save into the same data frame:
  
  df<-data.frame(p,Rmax,R)  
  
  # Calculate the resource growth rate:
  
  df$G_R<-(df$p*(df$Rmax-df$R)) # G_R = growth rate of resoruces depending on amount of resoruces. 
  
  # Plot the data:
  
  head(df)
  ggplot(df, aes(x=R, y=G_R))+
      geom_point() # Growth rate of the resource as a function of them self.. Should not be possible to be below 0.
  
  df$R_times_Growth<-df$G_R*df$R # Multiply the growth rate with amount of resoruces. 
  
  # Figure in Appendix:
  
  par(mar=c(5,5,5,5))
  plot(df$R, df$R_times_Growth, 
       ylim=c(0,25),
       ylab="Resource growth rate, G(R)",
       xlab="Resource density (Rriver)",
       cex.lab=2,
       text(0, 23, 
            "G(R)=p*(Rmax,river-Rriver)\n
Rmax,river=30\n
p=0.1",
            cex = 1,
            pos=4))


  
  #### Size-specific Intake rates #####
  
  # Create data:
  
  b<-seq(0.1, 0.1, length=1000)
  Exponent<-seq(-0.25,-0.25, length=1000)
  BW<-seq(0,10000,length=10000)
  
# Calculate ingestion rate
  
  df_ingestion<-data.frame(b, Exponent, BW)
  df_ingestion$Ingestion_size<-b*BW^Exponent 
  
  # Plot the data: 
  
  #x11() 
  plot(df_ingestion$BW, df_ingestion$Ingestion_size,
       ylim=c(0,0.1),
       xlab="Body weight (gram)",
       ylab="Maximum ingestion rate",
       cex.lab=2)
  points(x=7.2, y=0.1*7.2^-0.25, bg="#666666", col="white", pch=21, cex=3, lwd=2)   # Representative weight Jriver
  points(x=1488, y=0.1*1488^-0.25, bg="#7570B3", col="white", pch=21, cex=3, lwd=2) # Representative weight Jsea
  points(x=8000, y=0.1*8000^-0.25, bg="#E7298A", col="white", pch=21, cex=3,lwd=2)  # Representative weight Amat
  legend(y=0.1, x=7500, legend=c(expression(italic(J["river"])), expression(italic(J["sea"])), expression(italic(A["mat"]))),
         col=c("#666666", "#7570B3", "#E7298A"), pch=16, cex=1.5,  box.lty=0)

  
  ## Size-specific metabolism ####
  
  # Create the data:
  
  Constant<-seq(0.01, 0.01, length=1000)
  Exponent<-seq(-0.25,-0.25, length=1000)
  BW<-seq(0,10000,length=10000)
  
  # Calculate it:
  
  df_maintenance<-data.frame(Constant, Exponent, BW)
  df_maintenance$Maintenance_rate<-Constant*BW^Exponent 
  head(df_maintenance)
  
  # Plot it:
  
  plot(df_maintenance$BW, df_maintenance$Maintenance_rate,
       ylim=c(0,0.01),
       xlab="Body weight (gram)",
       ylab="Maintenance costs",
       cex.lab=2)
  points(x=7.2, y=0.01*7.2^-0.25, bg="#666666", col="white", pch=21, cex=3, lwd=2)     # Representative weight Jriver
  points(x=1488, y=0.01*1488^-0.25, bg="#7570B3", col="white", pch=21, cex=3, lwd=2)   # Representative weight Jsea
  points(x=8000, y=0.01*8000^-0.25, bg="#E7298A", col="white", pch=21, cex=3, lwd=2)   # Representative weight Amat
  legend(y=0.01, x=7500, legend=c(expression(italic(J["river"])), expression(italic(J["sea"])), expression(italic(A["mat"]))),
         col=c("#666666", "#7570B3", "#E7298A"), pch=16, cex=1.5,  box.lty=0)
  
  ## Size-specific mortality ####
  
# Create the data:
  
  Constant_mort<-seq(0.03, 0.03, length=1000)
  Exponent<-seq(-0.25,-0.25, length=1000)
  BW<-seq(0,10000,length=10000)
  
  # Calculate it:
  
  df_mortality<-data.frame(Constant_mort, Exponent, BW)
  df_mortality$Mortality_rate<-Constant_mort*BW^Exponent 
  head(df_mortality)
  
  # Plot it:
  
  plot(df_mortality$BW, df_mortality$Mortality_rate,
       ylim=c(0,0.04),
       xlab="Body weight (gram)",
       ylab="Mortality rate",
       cex.lab=2)
  points(x=7.2, y=0.03*7.2^-0.25, bg="#666666", col="white", pch=21, cex=3, lwd=2)
  points(x=1488, y=0.03*1488^-0.25, bg="#7570B3", col="white", pch=21, cex=3, lwd=2)
  points(x=8000, y=0.03*8000^-0.25, bg="#E7298A", col="white", pch=21, cex=3, lwd=2)
  legend(y=0.04, x=7500, legend=c(expression(italic(J["river"])), expression(italic(J["sea"])), expression(italic(A["mat"]))),
         col=c("#666666", "#7570B3", "#E7298A"), pch=16, cex=1.5,  box.lty=0)
  
  
## Fecundity, based on data from Fleming 1996, table 1. ####
  
  GSI<-c(24.1,21,20.3, 20.1,25.7) # Data from table 1 in Fleming 1996. https://link.springer.com/article/10.1007/BF00164323
  GSI_div_100<-GSI/100
  mean(GSI_div_100)/365 # -> g/g/day -> 0.0006
  
  (((24.1+21+20.3+20.1+25.7)/5)/100)/365 # 0.0006093151

  

 ## size-differences between stages (z) (max size/min size each stage)


# Juveniles in river (external feeding, 0.2 grams) to smolt (38 grams) size-ratio:  

0.2/38 # -> 0.0053 # = z1

# juveniles at sea (38 grams) to adult (8000 gram) size-ratio:

38/8000 # -> 0.00475 # Z2


##### Model output data visualisation #############

## Biffurcation plots ####

###### Increased river productivity ##########


# Specify stage-specific colours for plotting:

display.brewer.pal(8,"Dark2") # Show colurs in the "Dark2" palette.
brewer.pal(n = 8, name = "Dark2") # Get the HEX of these colours

# Specify the colour for each stage:

stage_specific_colour_plot<-scale_color_manual(values = c("R_river" = "#1B9E77", 
                                                          "R_sea" = "#E6AB02",
                                                          "J_river"="#666666",
                                                          "J_sea"="#7570B3",
                                                          "A_mat"="#E7298A",
                                                          "Total_biomass"= "#D95F02"))


# make legend in italics ##

lab_stages <- c(expression(italic(R["river"])),
                expression(italic(R["sea"])), 
                expression(italic(J["river"])),
                expression(italic(J["sea"])),
                expression(italic(A["mat"])),
                "Total biomass")

# Combine into a df used later on when plotting the data:

stage_specific_colour_plot<-scale_color_manual(values = c(expression(italic(R["river"]))) = "#1B9E77", 
                                               expression(italic(R["sea"])) = "#E6AB02",
                                               expression(italic(J["river"]))="#666666",
                                               expression(italic(J["sea"]))="#7570B3",
                                               expression(italic(A["mat"]))="#E7298A",
                                               "Total biomass"= "#D95F02")

# another df with labels:

stage_specific_colour_plot_with_labels<-scale_color_manual(values = c("R_river" = "#1B9E77", 
                                                                      "R_sea" = "#E6AB02",
                                                                      "J_river"="#666666",
                                                                      "J_sea"="#7570B3",
                                                                      "A_mat"="#E7298A",
                                                                      "Total_biomass"= "#D95F02"),
                                                           labels=c(expression(italic(R["river"])),
                                                                    expression(italic(R["sea"])), 
                                                                    expression(italic(J["river"])),
                                                                    expression(italic(J["sea"])),
                                                                    expression(italic(A["mat"])),
                                                                    expression(italic("Total biomass"))))


# Biff river productivity #####

# Scenario (i),no density dependence at sea


Biff_river_productivity<-read.delim("R_file_biff_river_productivity.csv", sep=";") # import data
head(Biff_river_productivity)

Biff_river_productivity<-Biff_river_productivity%>% # Rename adult stage
  rename(A_mat=A_sea)

Biff_river_productivity<-Biff_river_productivity %>% # Calculate total pop. biomass
  mutate(Total_biomass=J_river+J_sea+A_mat)

head(Biff_river_productivity) # Looks good
tail(Biff_river_productivity) # Looks good


Biff_river_productivity_long<-Biff_river_productivity %>%  # Change data format
  gather(Stage, Biomass, 1:4, 7)

str(Biff_river_productivity_long) # Check new format and data structure, looks good. 

Biff_river_productivity_long$RmaxSea<-factor(Biff_river_productivity_long$RmaxSea, # Re-organize order of factors
                                             levels=c("Rmax=2", "Rmax=10"))

Biff_river_productivity_long$Stage<-factor(Biff_river_productivity_long$Stage,     # Re-organize order of stages
                                             levels=c("R_river", "J_river", "J_sea", "A_mat", "Total_biomass"))

head(Biff_river_productivity_long)


# Plot the data:

#x11()
Biff_river_productivity_long %>% 
#filter(RmaxSea!="Rsea=5") %>% 
    ggplot(., aes(x=RmaxRiver, y=Biomass, group=Stage, colour=Stage))+
      geom_line()+
        facet_wrap(~RmaxSea)+
          theme_bw()

# Looks ok, lets fix the layout for the manuscript:

Biff_river_productivity_long$Scenario<-"Scenario (i)" # Add scenario label:

Biff_river_productivity_long<-Biff_river_productivity_long %>%  # Rename Rmax so it matches the paper.
  mutate(Labels_sea_prod=ifelse(RmaxSea=="Rmax=2", "Rmax,sea=2",
                                ifelse(RmaxSea=="Rmax=10", "Rmax,sea=10","100000")))

Biff_river_productivity_long$Labels_sea_prod<-factor(Biff_river_productivity_long$Labels_sea_prod, # Re-organize order of the new factors
                                             levels=c("Rmax,sea=2", "Rmax,sea=10"))

#Make the plot and save it into top_row:

top_row<-Biff_river_productivity_long %>% 
  #filter(RmaxSea!="Rsea=5") %>% 
  ggplot(., aes(x=RmaxRiver, y=Biomass, group=Stage, colour=Stage))+
  geom_line(size=2, alpha=0.75)+
  facet_grid(Scenario~Labels_sea_prod)+
  theme_bw()+
  scale_x_continuous(limits = c(0, 40))+
  #stage_specific_colour_plot+
  stage_specific_colour_plot_with_labels+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 20, face="bold"))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  theme(legend.text.align = 0)+
  #theme(legend.position="top")+
  guides(color=FALSE)


top_row # Looks good!
    
    
# Scenario (ii), density dependence at sea
    
Dens_dep_River_productivity_biff<-read.delim("R_file_Dens_Dep_Sea_biff_river_productivity.csv", sep=";") # Import data
    
head(Dens_dep_River_productivity_biff) # Check data
str(Dens_dep_River_productivity_biff) # Check data structure

Dens_dep_River_productivity_biff<-Dens_dep_River_productivity_biff%>% 
  rename(A_mat=A_sea)

Dens_dep_River_productivity_biff<-Dens_dep_River_productivity_biff %>% 
  mutate(Total_biomass=J_river+J_sea+A_mat)

head(Dens_dep_River_productivity_biff)
    
Dens_dep_River_prod_biff_long<-Dens_dep_River_productivity_biff %>%   # Change data format
   gather(Stage, Biomass, 1:5, 8)
    
head(Dens_dep_River_prod_biff_long)  # Check new format and data structure
str(Dens_dep_River_prod_biff_long)   # Check structure
    
Dens_dep_River_prod_biff_long$RmaxSea<-factor(Dens_dep_River_prod_biff_long$RmaxSea,  # Re-organize order of factors
                                              levels=c("Rsea=2", "Rsea=5", "Rsea=10"))
    
Dens_dep_River_prod_biff_long$Stage<-factor(Dens_dep_River_prod_biff_long$Stage,       # Re-organize order of factors
                                              levels=c("R_river", "R_sea", "J_river", "J_sea", "A_mat", "Total_biomass"))
    
# Plot the data:

Dens_dep_River_prod_biff_long %>% 
  filter(RmaxSea!="Rsea=5") %>% 
 ggplot(., aes(x=RmaxRiver, y=Biomass, group=Stage, colour=Stage))+
   geom_line()+
    facet_wrap(~RmaxSea)+
    theme_bw()

# Looks good, Fix layout:

Dens_dep_River_prod_biff_long$Scenario<-"Scenario (ii)"

Dens_dep_River_prod_biff_long<-Dens_dep_River_prod_biff_long %>% 
  mutate(Labels_sea_prod=ifelse(RmaxSea=="Rsea=2", "Rmax,sea=2",
                            ifelse(RmaxSea=="Rsea=5", "Rmax,sea=5",
                                ifelse(RmaxSea=="Rsea=10", "Rmax,sea=10","100000"))))

head(Dens_dep_River_prod_biff_long)

Dens_dep_River_prod_biff_long$Labels_sea_prod<-factor(Dens_dep_River_prod_biff_long$Labels_sea_prod, # Re-organize order of factors
                                                     levels=c("Rmax,sea=2", "Rmax,sea=5","Rmax,sea=10"))

center_row<-Dens_dep_River_prod_biff_long %>% 
  filter(RmaxSea!="Rsea=5") %>% 
  ggplot(., aes(x=RmaxRiver, y=Biomass, group=Stage, colour=Stage))+
  geom_line(size=2, alpha=0.75)+
  facet_grid(Scenario~Labels_sea_prod)+
  theme_bw()+
  scale_x_continuous(limits = c(0, 40))+
  scale_y_continuous(limits = c(0, 145))+
  #stage_specific_colour_plot+
  stage_specific_colour_plot_with_labels+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=25))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  theme(legend.text.align = 0)+
  guides(color=FALSE)

center_row # Looks good

# Scenario (iii), density dependence at sea + competition at sea among juveniles and adults
 

 Dens_sea_River_prod_comp_Biff<-read.delim("Biff_Densitydep_Comp_sea_Rmaxriver_R_file.csv", sep=";")
 
 head(Dens_sea_River_prod_comp_Biff)
 str(Dens_sea_River_prod_comp_Biff)
 
 Dens_sea_River_prod_comp_Biff<- Dens_sea_River_prod_comp_Biff%>% 
   rename(A_mat=A_sea)
 
 Dens_sea_River_prod_comp_Biff<- Dens_sea_River_prod_comp_Biff %>% 
   mutate(Total_biomass=J_river+J_sea+A_mat)
 
 head(Dens_sea_River_prod_comp_Biff)
 
 BIFF_Comp_Dens_sea_River_prod_long<-Dens_sea_River_prod_comp_Biff %>% 
   gather(Stage, Biomass, 1:5,8)
 
 head(BIFF_Comp_Dens_sea_River_prod_long)
 
 BIFF_Comp_Dens_sea_River_prod_long$RmaxSea<-factor(BIFF_Comp_Dens_sea_River_prod_long$RmaxSea,
                                                    levels=c("Rsea=2", "Rsea=5", "Rsea=10"))
 
 BIFF_Comp_Dens_sea_River_prod_long$Stage<-factor(BIFF_Comp_Dens_sea_River_prod_long$Stage,
                                                  levels=c("R_river", "R_sea", "J_river", "J_sea", "A_mat", "Total_biomass"))
 
 # Plot data:
 
 BIFF_Comp_Dens_sea_River_prod_long %>% 
   filter(RmaxSea!="Rsea=5") %>% 
 ggplot(., aes(x=RmaxRiver, y=Biomass, group=Stage, colour=Stage))+
   geom_line()+
   facet_wrap(~RmaxSea)+
   theme_bw()
 
 BIFF_Comp_Dens_sea_River_prod_long$Scenario<-"Scenario (iii)"
 
 BIFF_Comp_Dens_sea_River_prod_long<- BIFF_Comp_Dens_sea_River_prod_long %>% 
   mutate(Labels_sea_prod=ifelse(RmaxSea=="Rsea=2", "Rmax,sea=2",
                                 ifelse(RmaxSea=="Rsea=5", "Rmax,sea=5",
                                        ifelse(RmaxSea=="Rsea=10", "Rmax,sea=10","100000"))))
 
 head(BIFF_Comp_Dens_sea_River_prod_long)
 
 BIFF_Comp_Dens_sea_River_prod_long$Labels_sea_prod<-factor(BIFF_Comp_Dens_sea_River_prod_long$Labels_sea_prod, # Re-organize order of factors
                                                       levels=c("Rmax,sea=2", "Rmax,sea=5","Rmax,sea=10"))
 
 
 
 bottom_row<-BIFF_Comp_Dens_sea_River_prod_long %>% 
   filter(RmaxSea!="Rsea=5") %>% 
   ggplot(., aes(x=RmaxRiver, y=Biomass, group=Stage, colour=Stage))+
   geom_line(size=2, alpha=0.75)+
   facet_grid(Scenario~Labels_sea_prod)+
   theme_bw()+
   scale_x_continuous(limits = c(0, 40))+
   scale_y_continuous(limits = c(0, 145))+
   #stage_specific_colour_plot+
   stage_specific_colour_plot_with_labels+
   xlab("River productivity")+
   theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
   theme(panel.grid.minor = element_blank())+
   theme(axis.title.x = element_text(size=25))+
   theme(axis.title.y = element_blank())+
   theme(axis.text.x = element_text(size=15, colour="black"))+
   theme(axis.text.y = element_text(size=15, colour="black"))+
   #guides(col = guide_legend(nrow = 4))+
   #theme(legend.position="top")+
   theme(strip.background =element_rect(fill="white", colour="black"))+
   theme(strip.text = element_text(colour = "black", size = 15))+
   theme(legend.title = element_text(colour="black", size = 15))+
   theme(legend.text = element_text(colour="black", size  = 15))+
   theme(legend.text.align = 0)
   #guides(color=FALSE)
 
 bottom_row # Looks good
 
 library(patchwork) # v. 1.0.3,  Needed for combining plots:
 

 top_row+center_row+bottom_row+plot_layout(ncol=1)+plot_layout(guides="collect") & theme(legend.position = 'top', 
                                                                                         legend.title = element_text(colour="black", size = 20, face="bold"))
 


##### Biff. Increased Adult mortality #####

 # Scenario i 

Biff_Adult_mort<-read.delim("Biff_Amort_diffM2Scenarios.csv", sep=";")

head(Biff_Adult_mort)

# CHnage name of adult stage:

Biff_Adult_mort<-Biff_Adult_mort %>% 
  rename(A_mat=A_sea)

# Sum-up tot biomass:


Biff_Adult_mort<- Biff_Adult_mort %>% 
  mutate(Total_biomass=J_river+J_sea+A_mat)

head(Biff_Adult_mort)

# Change format:

Biff_Adult_mort_long<-Biff_Adult_mort %>% 
  gather(Stage, Biomass, 1:4,7)

head(Biff_Adult_mort_long)
str(Biff_Adult_mort_long)

# Chnage order:

Biff_Adult_mort_long$RmaxSea<-factor(Biff_Adult_mort_long$RmaxSea,
                                             levels=c("Rsea=2", "Rsea=5", "Rsea=10"))

# Change order:

Biff_Adult_mort_long$Stage<-factor(Biff_Adult_mort_long$Stage,
                                           levels=c("R_river", "J_river", "J_sea", "A_mat", "Total_biomass"))

# Add scenario label: 

Biff_Adult_mort_long$Scenario<-"Scenario (i)"

head(Biff_Adult_mort_long)

# Rename:

Biff_Adult_mort_long<- Biff_Adult_mort_long %>% 
  mutate(Labels_sea_prod=ifelse(RmaxSea=="Rsea=2", "Rmax,sea=2",
                                ifelse(RmaxSea=="Rsea=5", "Rmax,sea=5",
                                       ifelse(RmaxSea=="Rsea=10", "Rmax,sea=10","100000"))))


Biff_Adult_mort_long$Labels_sea_prod<-factor(Biff_Adult_mort_long$Labels_sea_prod, # Re-organize order of factors:
                                                           levels=c("Rmax,sea=2", "Rmax,sea=5","Rmax,sea=10"))
# Plot it:
  

top_row_biff_mort<-Biff_Adult_mort_long %>% 
  filter(RmaxSea!="Rsea=5") %>% 
  ggplot(., aes(x=A_mort, y=Biomass, group=Stage, colour=Stage))+
  geom_line(size=2, alpha=0.75)+
  facet_grid(Scenario~Labels_sea_prod)+
  xlab("River productivity")+
  #stage_specific_colour_plot+
  stage_specific_colour_plot_with_labels+
  theme_bw()+
  scale_y_continuous(limits = c(0, 100))+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+ # otherwise element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(color=FALSE)+
  theme(legend.text.align = 0)
  
top_row_biff_mort # Looks good

# Scenario ii, Density dependence at sea model


Dens_dep_A_mort_biff<-read.delim("R_file_Biff_Densitydepsea_A_mort.csv", sep=";")

head(Dens_dep_A_mort_biff)
str(Dens_dep_A_mort_biff)

Dens_dep_A_mort_biff<-Dens_dep_A_mort_biff %>% 
  rename(A_mat=A_sea)

Dens_dep_A_mort_biff<- Dens_dep_A_mort_biff %>% 
  mutate(Total_biomass=J_river+J_sea+A_mat)

head(Dens_dep_A_mort_biff)

Dens_dep_A_mort_biff_long<-Dens_dep_A_mort_biff %>% 
  gather(Stage, Biomass, 1:5,8)

head(Dens_dep_A_mort_biff_long)

Dens_dep_A_mort_biff_long$Rseamax<-factor(Dens_dep_A_mort_biff_long$Rseamax,
                                              levels=c("Rsea=2", "Rsea=5", "Rsea=10"))

Dens_dep_A_mort_biff_long$Stage<-factor(Dens_dep_A_mort_biff_long$Stage,
                                            levels=c("R_river", "R_sea", "J_river", "J_sea", "A_mat", "Total_biomass"))

Dens_dep_A_mort_biff_long$Scenario<-"Scenario (ii)"
head(Dens_dep_A_mort_biff_long)

Dens_dep_A_mort_biff_long<- Dens_dep_A_mort_biff_long %>% 
  mutate(Labels_sea_prod=ifelse(Rseamax=="Rsea=2", "Rmax,sea=2",
                                ifelse(Rseamax=="Rsea=5", "Rmax,sea=5",
                                       ifelse(Rseamax=="Rsea=10", "Rmax,sea=10","100000"))))

Dens_dep_A_mort_biff_long$Labels_sea_prod<-factor(Dens_dep_A_mort_biff_long$Labels_sea_prod, # Re-organize order of factors
                                             levels=c("Rmax,sea=2", "Rmax,sea=5","Rmax,sea=10"))



Dens_dep_A_mort_biff_long %>% 
  filter(Rseamax!="Rsea=5") %>% 
ggplot(., aes(x=A_mort, y=Biomass, group=Stage, colour=Stage))+
  geom_line()+
  facet_grid(Scenario~Labels_sea_prod)

head(Dens_dep_A_mort_biff_long)


Center_row_biff_mort<-Dens_dep_A_mort_biff_long%>% 
  filter(Rseamax!="Rsea=5") %>% 
  ggplot(., aes(x=A_mort, y=Biomass, group=Stage, colour=Stage))+
  geom_line(size=2, alpha=0.75)+
  facet_grid(Scenario~Labels_sea_prod)+
  xlab("River productivity")+
  #stage_specific_colour_plot+
  stage_specific_colour_plot_with_labels+
  theme_bw()+
  scale_y_continuous(limits = c(0, 100))+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+ # otherwise element_text(size=25, colour="black"))+
  theme(axis.title.y = element_text(size=25, colour="black"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  theme(legend.text.align = 0)

Center_row_biff_mort # Looks good

# Scenario iii, Density dependence at sea  + resource comp at sea


BIFF_Comp_Dens_sea_A_mort<-read.delim("Biff_Densitydep_Comp_sea_Amort_R_file.csv", sep=";")

head(BIFF_Comp_Dens_sea_A_mort)
str(BIFF_Comp_Dens_sea_A_mort)

BIFF_Comp_Dens_sea_A_mort<-BIFF_Comp_Dens_sea_A_mort %>% 
rename(A_mat=A_sea)

BIFF_Comp_Dens_sea_A_mort<- BIFF_Comp_Dens_sea_A_mort %>% 
  mutate(Total_biomass=J_river+J_sea+A_mat)

head(BIFF_Comp_Dens_sea_A_mort)

BIFF_Comp_Dens_sea_A_mort_long<-BIFF_Comp_Dens_sea_A_mort %>% 
  gather(Stage, Biomass, 1:5,8)

head(BIFF_Comp_Dens_sea_A_mort_long)


BIFF_Comp_Dens_sea_A_mort_long$RmaxSea<-factor(BIFF_Comp_Dens_sea_A_mort_long$RmaxSea,
                                                   levels=c("Rsea=2", "Rsea=5", "Rsea=10"))

BIFF_Comp_Dens_sea_A_mort_long$Stage<-factor(BIFF_Comp_Dens_sea_A_mort_long$Stage,
                                                 levels=c("R_river", "R_sea", "J_river", "J_sea", "A_mat", "Total_biomass"))


BIFF_Comp_Dens_sea_A_mort_long$Scenario<-"Scenario (iii)"

head(BIFF_Comp_Dens_sea_A_mort_long)

BIFF_Comp_Dens_sea_A_mort_long<- BIFF_Comp_Dens_sea_A_mort_long %>% 
  mutate(Labels_sea_prod=ifelse(RmaxSea=="Rsea=2", "Rmax,sea=2",
                                ifelse(RmaxSea=="Rsea=5", "Rmax,sea=5",
                                       ifelse(RmaxSea=="Rsea=10", "Rmax,sea=10","100000"))))

BIFF_Comp_Dens_sea_A_mort_long$Labels_sea_prod<-factor(BIFF_Comp_Dens_sea_A_mort_long$Labels_sea_prod, # Re-organize order of factors
                                                  levels=c("Rmax,sea=2", "Rmax,sea=5","Rmax,sea=10"))



head(BIFF_Comp_Dens_sea_A_mort_long)

BIFF_Comp_Dens_sea_A_mort_long %>% 
  filter(RmaxSea!="Rsea=5",
         A_mort>0.0001) %>% 
  ggplot(., aes(x=A_mort, y=Biomass, group=Stage, colour=Stage))+
  geom_line()+
  facet_wrap(~RmaxSea)

Bottom_row_amort_biff<-BIFF_Comp_Dens_sea_A_mort_long %>% 
  filter(RmaxSea!="Rsea=5",
         A_mort>0.001) %>% 
  ggplot(., aes(x=A_mort, y=Biomass, group=Stage, colour=Stage))+
  geom_line(size=2, alpha=0.75)+
  facet_grid(Scenario~Labels_sea_prod)+
  xlab("Adult mortality")+
  theme_bw()+
  #stage_specific_colour_plot+
  stage_specific_colour_plot_with_labels+
  scale_y_continuous(limits = c(0, 100))+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+ # otherwise element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  theme(legend.text.align = 0)

Bottom_row_amort_biff # Looks good

# Combine:


top_row_biff_mort+Center_row_biff_mort+Bottom_row_amort_biff+plot_layout(ncol=1)+plot_layout(guides="collect") & theme(legend.position = 'top', 
                                                                                        legend.title = element_text(colour="black", size = 20, face="bold"))


# Tot biomass:

head(BIFF_Comp_Dens_sea_A_mort_long)



### HEATMAPS MS IV, stage-specific #####

# J_river scenario i-iii, Rsea = 2 ####

# Scenario i

No_dens_J_river_Rsea_2<-read.delim("Matris_J_river_Rsea2_no_dens_Rfile.csv", sep=";")
head(No_dens_J_river_Rsea_2)

dens_J_river_Rsea_2<-read.delim("Matris_J_river_Condensed_R_file_Rsea2_Density_dep.csv", sep=";")
head(dens_J_river_Rsea_2)

dens_comp_J_river_Rsea_2<-read.delim("Matris_J_River_with_density_comp_Asea_Jsea_R2_Rfile.csv", sep=";")
head(dens_comp_J_river_Rsea_2)

# Make the heatmaps:

# Scenario i

str(No_dens_J_river_Rsea_2)

No_dens_J_river_Rsea_2_gathered<-No_dens_J_river_Rsea_2%>% 
  gather(A_mort, J_river, 2:13) %>% 
  mutate(R_river_Max=X)

head(No_dens_J_river_Rsea_2_gathered)
tail(No_dens_J_river_Rsea_2_gathered) # Good

No_dens_J_river_Rsea_2_gathered$A_mort=gsub("X","", No_dens_J_river_Rsea_2_gathered$A_mort)

head(No_dens_J_river_Rsea_2_gathered)

No_dens_J_river_Rsea_2_gathered_Plot_format<-No_dens_J_river_Rsea_2_gathered[,c(4,2,3)]

No_dens_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(No_dens_J_river_Rsea_2_gathered_Plot_format$R_river_Max))

head(No_dens_J_river_Rsea_2_gathered_Plot_format)

No_dens_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(No_dens_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                          levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(No_dens_J_river_Rsea_2_gathered_Plot_format)


No_dens_J_river_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (i)"
No_dens_J_river_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"

#x11()
ggplot(No_dens_J_river_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

# Scenario ii

str(dens_J_river_Rsea_2)

dens_J_river_Rsea_2_gathered<-dens_J_river_Rsea_2%>% 
  gather(A_mort, J_river, 2:13) %>% 
  mutate(R_river_Max=X)

head(dens_J_river_Rsea_2_gathered)
tail(dens_J_river_Rsea_2_gathered) # Good

dens_J_river_Rsea_2_gathered$A_mort=gsub("X","", dens_J_river_Rsea_2_gathered$A_mort)

head(dens_J_river_Rsea_2_gathered)

dens_J_river_Rsea_2_gathered_Plot_format<-dens_J_river_Rsea_2_gathered[,c(4,2,3)]

dens_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_J_river_Rsea_2_gathered_Plot_format$R_river_Max))

head(dens_J_river_Rsea_2_gathered_Plot_format)

dens_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(dens_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                         levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(dens_J_river_Rsea_2_gathered_Plot_format)


dens_J_river_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (ii)"
dens_J_river_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"

#x11()
ggplot(dens_J_river_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario iii

str(dens_comp_J_river_Rsea_2)

dens_comp_J_river_Rsea_2_gathered<-dens_comp_J_river_Rsea_2%>% 
  gather(A_mort, J_river, 2:16) %>% 
  mutate(R_river_Max=X)

head(dens_comp_J_river_Rsea_2_gathered)
tail(dens_comp_J_river_Rsea_2_gathered) # Good

dens_comp_J_river_Rsea_2_gathered$A_mort=gsub("X","", dens_comp_J_river_Rsea_2_gathered$A_mort)

head(dens_comp_J_river_Rsea_2_gathered)

dens_comp_J_river_Rsea_2_gathered_Plot_format<-dens_comp_J_river_Rsea_2_gathered[,c(4,2,3)]

dens_comp_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_comp_J_river_Rsea_2_gathered_Plot_format$R_river_Max))

head(dens_comp_J_river_Rsea_2_gathered_Plot_format)

dens_comp_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(dens_comp_J_river_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                      levels = c("1","3","6","9","12","15","18","21","24","27","30"))


dens_comp_J_river_Rsea_2_gathered_Plot_format$A_mort <- factor(dens_comp_J_river_Rsea_2_gathered_Plot_format$A_mort,
                                                                           levels = c("0.01","0.03","0.06", "0.09", "0.12", "0.15", 
                                                                                      "0.18", "0.21", "0.24", "0.30", "0.60", "0.90","3", "6", "12"))


head(dens_comp_J_river_Rsea_2_gathered_Plot_format)


dens_comp_J_river_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (iii)"
dens_comp_J_river_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"

#x11()
ggplot(dens_comp_J_river_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")




# J_river scenario i-iii, Rsea = 10 ####

No_dens_J_river_Rsea_10<-read.delim("Matris_J_river_Rsea10_no_dens_Rfile.csv", sep=";")
head(No_dens_J_river_Rsea_10)

dens_J_river_Rsea_10<-read.delim("Matris_J_river_Condensed_R_file_Rsea_10_dens_dep.csv", sep=";")
head(dens_J_river_Rsea_10)

dens_comp_J_river_Rsea_10<-read.delim("Matris_J_River_with_density_comp_Asea_Jsea_R10_Rfile.csv", sep=";")
head(dens_comp_J_river_Rsea_10)

# Scenario i:

str(No_dens_J_river_Rsea_10)

No_dens_J_river_Rsea_10_gathered<-No_dens_J_river_Rsea_10%>% 
  gather(A_mort, J_river, 2:13) %>% 
  mutate(R_river_Max=X)

head(No_dens_J_river_Rsea_10_gathered)
tail(No_dens_J_river_Rsea_10_gathered) # Good

No_dens_J_river_Rsea_10_gathered$A_mort=gsub("X","", No_dens_J_river_Rsea_10_gathered$A_mort)

head(No_dens_J_river_Rsea_10_gathered)

No_dens_J_river_Rsea_10_gathered_Plot_format<-No_dens_J_river_Rsea_10_gathered[,c(4,2,3)]

No_dens_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(No_dens_J_river_Rsea_10_gathered_Plot_format$R_river_Max))

head(No_dens_J_river_Rsea_10_gathered_Plot_format)

No_dens_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(No_dens_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                         levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(No_dens_J_river_Rsea_10_gathered_Plot_format)


No_dens_J_river_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (i)"
No_dens_J_river_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(No_dens_J_river_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

# Scenario ii:

str(dens_J_river_Rsea_10)

dens_J_river_Rsea_10_gathered<-dens_J_river_Rsea_10%>% 
  gather(A_mort, J_river, 2:13) %>% 
  mutate(R_river_Max=X)

head(dens_J_river_Rsea_10_gathered)
tail(dens_J_river_Rsea_10_gathered) # Good

dens_J_river_Rsea_10_gathered$A_mort=gsub("X","", dens_J_river_Rsea_10_gathered$A_mort)

head(dens_J_river_Rsea_10_gathered)

dens_J_river_Rsea_10_gathered_Plot_format<-dens_J_river_Rsea_10_gathered[,c(4,2,3)]

dens_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_J_river_Rsea_10_gathered_Plot_format$R_river_Max))

head(dens_J_river_Rsea_10_gathered_Plot_format)

dens_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(dens_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                          levels = c("1","3","6","9","12","15","18","21","24","27","30"))
str(dens_J_river_Rsea_10_gathered_Plot_format)

head(dens_J_river_Rsea_10_gathered_Plot_format)


dens_J_river_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (ii)"
dens_J_river_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(dens_J_river_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

# Scenario iii:

str(dens_comp_J_river_Rsea_10)

dens_comp_J_river_Rsea_10_gathered<-dens_comp_J_river_Rsea_10%>% 
  gather(A_mort, J_river, 2:16) %>% 
  mutate(R_river_Max=X)

head(dens_comp_J_river_Rsea_10_gathered)
tail(dens_comp_J_river_Rsea_10_gathered) # Good

dens_comp_J_river_Rsea_10_gathered$A_mort=gsub("X","", dens_comp_J_river_Rsea_10_gathered$A_mort)

head(dens_comp_J_river_Rsea_10_gathered)

dens_comp_J_river_Rsea_10_gathered_Plot_format<-dens_comp_J_river_Rsea_10_gathered[,c(4,2,3)]

dens_comp_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_comp_J_river_Rsea_10_gathered_Plot_format$R_river_Max))

head(dens_comp_J_river_Rsea_10_gathered_Plot_format)

dens_comp_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(dens_comp_J_river_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))

dens_comp_J_river_Rsea_10_gathered_Plot_format$A_mort <- factor(dens_comp_J_river_Rsea_10_gathered_Plot_format$A_mort,
                                                           levels = c("0.01","0.03","0.06","0.09","0.12","0.15",
                                                                      "0.18","0.21","0.24","0.30","0.60","0.90","3","6","12"))

head(dens_comp_J_river_Rsea_10_gathered_Plot_format)


dens_comp_J_river_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (iii)"
dens_comp_J_river_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(dens_comp_J_river_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# J_sea scenario i-iii, Rsea = 2 ####

No_dens_J_sea_Rsea_2<-read.delim("Matris_J_sea_Rsea2_no_dens_Rfile.csv", sep=";")
head(No_dens_J_sea_Rsea_2)

dens_J_sea_Rsea_2<-read.delim("Matris_J_sea_Condensed_R_file_Rsea2_Density_dep.csv", sep=";")
head(dens_J_sea_Rsea_2)

dens_comp_J_sea_Rsea_2<-read.delim("Matris_J_sea_with_density_comp_Asea_Jsea_R2_Rfile.csv", sep=";")
head(dens_comp_J_sea_Rsea_2)

# Scenario i

str(No_dens_J_sea_Rsea_2)
head(No_dens_J_sea_Rsea_2)

No_dens_J_sea_Rsea_2_gathered<-No_dens_J_sea_Rsea_2 %>% 
  gather(A_mort, J_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(No_dens_J_sea_Rsea_2_gathered)
tail(No_dens_J_sea_Rsea_2_gathered) # Good

No_dens_J_sea_Rsea_2_gathered$A_mort=gsub("X","", No_dens_J_sea_Rsea_2_gathered$A_mort)

head(No_dens_J_sea_Rsea_2_gathered)

No_dens_J_sea_Rsea_2_gathered_Plot_format<-No_dens_J_sea_Rsea_2_gathered[,c(4,2,3)]

No_dens_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(No_dens_J_sea_Rsea_2_gathered_Plot_format$R_river_Max))

head(No_dens_J_sea_Rsea_2_gathered_Plot_format)

No_dens_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(No_dens_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                         levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(No_dens_J_sea_Rsea_2_gathered_Plot_format)


No_dens_J_sea_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (i)"
No_dens_J_sea_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"

#x11()
ggplot(No_dens_J_sea_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario ii

str(dens_J_sea_Rsea_2)

dens_J_sea_Rsea_2_gathered<-dens_J_sea_Rsea_2 %>% 
  gather(A_mort, J_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(dens_J_sea_Rsea_2_gathered)
tail(dens_J_sea_Rsea_2_gathered) # Good

dens_J_sea_Rsea_2_gathered$A_mort=gsub("X","", dens_J_sea_Rsea_2_gathered$A_mort)

head(dens_J_sea_Rsea_2_gathered)

dens_J_sea_Rsea_2_gathered_Plot_format<-dens_J_sea_Rsea_2_gathered[,c(4,2,3)]

dens_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_J_sea_Rsea_2_gathered_Plot_format$R_river_Max))

head(dens_J_sea_Rsea_2_gathered_Plot_format)

dens_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(dens_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(dens_J_sea_Rsea_2_gathered_Plot_format)


dens_J_sea_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (ii)"
dens_J_sea_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"

#x11()
ggplot(dens_J_sea_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario iii

head(dens_comp_J_sea_Rsea_2)

str(dens_comp_J_sea_Rsea_2)

dens_comp_J_sea_Rsea_2_gathered<-dens_comp_J_sea_Rsea_2 %>% 
  gather(A_mort, J_sea, 2:16) %>% 
  mutate(R_river_Max=X)

head(dens_comp_J_sea_Rsea_2_gathered)
tail(dens_comp_J_sea_Rsea_2_gathered) # Good

dens_comp_J_sea_Rsea_2_gathered$A_mort=gsub("X","", dens_comp_J_sea_Rsea_2_gathered$A_mort)

head(dens_comp_J_sea_Rsea_2_gathered)

dens_comp_J_sea_Rsea_2_gathered_Plot_format<-dens_comp_J_sea_Rsea_2_gathered[,c(4,2,3)]

dens_comp_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_comp_J_sea_Rsea_2_gathered_Plot_format$R_river_Max))

head(dens_comp_J_sea_Rsea_2_gathered_Plot_format)

dens_comp_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(dens_comp_J_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                    levels = c("1","3","6","9","12","15","18","21","24","27","30"))


dens_comp_J_sea_Rsea_2_gathered_Plot_format$A_mort <- factor(dens_comp_J_sea_Rsea_2_gathered_Plot_format$A_mort,
                                                                levels = c("0.01","0.03","0.06","0.09","0.12","0.15",
                                                                           "0.18","0.21","0.24","0.30","0.60","0.90","3","6","12"))

head(dens_comp_J_sea_Rsea_2_gathered_Plot_format)


dens_comp_J_sea_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (iii)"
dens_comp_J_sea_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"
head(dens_comp_J_sea_Rsea_2_gathered_Plot_format)

#x11()
ggplot(dens_comp_J_sea_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# J_sea scenario i-iii, Rsea = 10 ####

No_dens_J_sea_Rsea_10<-read.delim("Matris_J_sea_Rsea10_no_dens_Rfile.csv", sep=";")
head(No_dens_J_sea_Rsea_10)

dens_J_sea_Rsea_10<-read.delim("Matris_J_sea_Condensed_R_file_Rsea_10_dens_dep.csv", sep=";")
head(dens_J_sea_Rsea_10)

dens_comp_J_sea_Rsea_10<-read.delim("Matris_J_Sea_with_density_comp_Asea_Jsea_R10_Rfile.csv", sep=";")
head(dens_comp_J_sea_Rsea_10)


# Scenario i

str(No_dens_J_sea_Rsea_10)
head(No_dens_J_sea_Rsea_10)

No_dens_J_sea_Rsea_10_gathered<-No_dens_J_sea_Rsea_10 %>% 
  gather(A_mort, J_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(No_dens_J_sea_Rsea_10_gathered)
tail(No_dens_J_sea_Rsea_10_gathered) # Good

No_dens_J_sea_Rsea_10_gathered$A_mort=gsub("X","", No_dens_J_sea_Rsea_10_gathered$A_mort)

head(No_dens_J_sea_Rsea_10_gathered)

No_dens_J_sea_Rsea_10_gathered_Plot_format<-No_dens_J_sea_Rsea_10_gathered[,c(4,2,3)]

No_dens_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(No_dens_J_sea_Rsea_10_gathered_Plot_format$R_river_Max))

head(No_dens_J_sea_Rsea_10_gathered_Plot_format)

No_dens_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(No_dens_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(No_dens_J_sea_Rsea_10_gathered_Plot_format)


No_dens_J_sea_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (i)"
No_dens_J_sea_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(No_dens_J_sea_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario ii

str(dens_J_sea_Rsea_10)
head(dens_J_sea_Rsea_10)

dens_J_sea_Rsea_10_gathered<-dens_J_sea_Rsea_10 %>% 
  gather(A_mort, J_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(dens_J_sea_Rsea_10_gathered)
tail(dens_J_sea_Rsea_10_gathered) # Good

dens_J_sea_Rsea_10_gathered$A_mort=gsub("X","", dens_J_sea_Rsea_10_gathered$A_mort)

head(dens_J_sea_Rsea_10_gathered)

dens_J_sea_Rsea_10_gathered_Plot_format<-dens_J_sea_Rsea_10_gathered[,c(4,2,3)]

dens_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_J_sea_Rsea_10_gathered_Plot_format$R_river_Max))

head(dens_J_sea_Rsea_10_gathered_Plot_format)

dens_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(dens_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                        levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(dens_J_sea_Rsea_10_gathered_Plot_format)


dens_J_sea_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (ii)"
dens_J_sea_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(dens_J_sea_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario (iii)

str(dens_comp_J_sea_Rsea_10)
head(dens_comp_J_sea_Rsea_10)

dens_comp_J_sea_Rsea_10_gathered<-dens_comp_J_sea_Rsea_10 %>% 
  gather(A_mort, J_sea, 2:16) %>% 
  mutate(R_river_Max=X)

head(dens_comp_J_sea_Rsea_10_gathered)
tail(dens_comp_J_sea_Rsea_10_gathered) # Good

dens_comp_J_sea_Rsea_10_gathered$A_mort=gsub("X","", dens_comp_J_sea_Rsea_10_gathered$A_mort)

head(dens_comp_J_sea_Rsea_10_gathered)

dens_comp_J_sea_Rsea_10_gathered_Plot_format<-dens_comp_J_sea_Rsea_10_gathered[,c(4,2,3)]

dens_comp_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_comp_J_sea_Rsea_10_gathered_Plot_format$R_river_Max))

head(dens_comp_J_sea_Rsea_10_gathered_Plot_format)

dens_comp_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(dens_comp_J_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                     levels = c("1","3","6","9","12","15","18","21","24","27","30"))

dens_comp_J_sea_Rsea_10_gathered_Plot_format$A_mort <- factor(dens_comp_J_sea_Rsea_10_gathered_Plot_format$A_mort,
                                                             levels = c("0.01","0.03","0.06","0.09","0.12","0.15",
                                                                        "0.18","0.21","0.24","0.30","0.60","0.90","3","6","12"))


head(dens_comp_J_sea_Rsea_10_gathered_Plot_format)


dens_comp_J_sea_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (iii)"
dens_comp_J_sea_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(dens_comp_J_sea_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# A_sea scenario i-iii, Rsea = 2 ####

No_dens_A_sea_Rsea_2<-read.delim("Matris_A_sea_Rsea2_no_dens_Rfile.csv", sep=";")
head(No_dens_A_sea_Rsea_2)

dens_A_sea_Rsea_2<-read.delim("Matris_A_sea_Condensed_R_file_Rsea2_Density_dep.csv", sep=";")
head(dens_A_sea_Rsea_2)

dens_comp_A_sea_Rsea_2<-read.delim("Matris_A_sea_with_density_comp_Asea_Jsea_R2_Rfile.csv", sep=";")
head(dens_comp_A_sea_Rsea_2)

# Scenario i

str(No_dens_A_sea_Rsea_2)
head(No_dens_A_sea_Rsea_2)

No_dens_A_sea_Rsea_2_gathered<-No_dens_A_sea_Rsea_2 %>% 
  gather(A_mort, A_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(No_dens_A_sea_Rsea_2_gathered)
tail(No_dens_A_sea_Rsea_2_gathered) # Good

No_dens_A_sea_Rsea_2_gathered$A_mort=gsub("X","", No_dens_A_sea_Rsea_2_gathered$A_mort)

head(No_dens_A_sea_Rsea_2_gathered)

No_dens_A_sea_Rsea_2_gathered_Plot_format<-No_dens_A_sea_Rsea_2_gathered[,c(4,2,3)]

No_dens_A_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(No_dens_A_sea_Rsea_2_gathered_Plot_format$R_river_Max))

head(No_dens_A_sea_Rsea_2_gathered_Plot_format)

No_dens_A_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(No_dens_A_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                        levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(No_dens_A_sea_Rsea_2_gathered_Plot_format)


No_dens_A_sea_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (i)"
No_dens_A_sea_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"

#x11()
ggplot(No_dens_A_sea_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

# Scenario ii

str(dens_A_sea_Rsea_2)
head(dens_A_sea_Rsea_2)

dens_A_sea_Rsea_2_gathered<-dens_A_sea_Rsea_2 %>% 
  gather(A_mort, A_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(dens_A_sea_Rsea_2_gathered)
tail(dens_A_sea_Rsea_2_gathered) # Good

dens_A_sea_Rsea_2_gathered$A_mort=gsub("X","", dens_A_sea_Rsea_2_gathered$A_mort)

head(dens_A_sea_Rsea_2_gathered)

dens_A_sea_Rsea_2_gathered_Plot_format<-dens_A_sea_Rsea_2_gathered[,c(4,2,3)]

dens_A_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_A_sea_Rsea_2_gathered_Plot_format$R_river_Max))

head(dens_A_sea_Rsea_2_gathered_Plot_format)

dens_A_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor <- factor(dens_A_sea_Rsea_2_gathered_Plot_format$R_river_max_Factor,
                                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(dens_A_sea_Rsea_2_gathered_Plot_format)


dens_A_sea_Rsea_2_gathered_Plot_format$Scenario<-"Scenario (ii)"
dens_A_sea_Rsea_2_gathered_Plot_format$Rsea<-"Rsea=2"

#x11()
ggplot(dens_A_sea_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario iii

str(dens_comp_A_sea_Rsea_2)
head(dens_comp_A_sea_Rsea_2)

dens_comp_A_sea_Rsea_2_gathered<-dens_comp_A_sea_Rsea_2 %>% 
  gather(A_mort, A_sea, 2:16) %>% 
  mutate(R_river_Max=X)

head(dens_comp_A_sea_Rsea_2_gathered)
tail(dens_comp_A_sea_Rsea_2_gathered) # Good

dens_comp_A_sea_Rsea_2_gathered$A_mort=gsub("X","", dens_comp_A_sea_Rsea_2_gathered$A_mort)

head(dens_comp_A_sea_Rsea_2_gathered)

dens_comp_A_sea_Rsea_2_gathered<-dens_comp_A_sea_Rsea_2_gathered[,c(4,2,3)]

dens_comp_A_sea_Rsea_2_gathered$R_river_max_Factor<-as.factor(as.character(dens_comp_A_sea_Rsea_2_gathered$R_river_Max))

head(dens_comp_A_sea_Rsea_2_gathered)

dens_comp_A_sea_Rsea_2_gathered$R_river_max_Factor <- factor(dens_comp_A_sea_Rsea_2_gathered$R_river_max_Factor,
                                                                          levels = c("1","3","6","9","12","15","18","21","24","27","30"))

dens_comp_A_sea_Rsea_2_gathered$A_mort <- factor(dens_comp_A_sea_Rsea_2_gathered$A_mort,
                                                              levels = c("0.01","0.03","0.06","0.09","0.12","0.15",
                                                                         "0.18","0.21","0.24","0.30","0.60","0.90","3","6","12"))
head(dens_comp_A_sea_Rsea_2_gathered)


dens_comp_A_sea_Rsea_2_gathered$Scenario<-"Scenario (iii)"
dens_comp_A_sea_Rsea_2_gathered$Rsea<-"Rsea=2"

#x11()
ggplot(dens_comp_A_sea_Rsea_2_gathered, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

# A_sea scenario i-iii, Rsea = 10 ####

No_dens_A_sea_Rsea_10<-read.delim("Matris_A_sea_Rsea10_no_dens_Rfile.csv", sep=";")
head(No_dens_A_sea_Rsea_10)

dens_A_sea_Rsea_10<-read.delim("Matris_A_sea_Condensed_R_file_Rsea_10_dens_dep.csv", sep=";")
head(dens_A_sea_Rsea_10)

dens_comp_A_sea_Rsea_10<-read.delim("Matris_A_Sea_with_density_comp_Asea_Jsea_R10_Rfile.csv", sep=";")
head(dens_comp_A_sea_Rsea_10)

# Scenario i

str(No_dens_A_sea_Rsea_10)
head(No_dens_A_sea_Rsea_10)

No_dens_A_sea_Rsea_10_gathered<-No_dens_A_sea_Rsea_10 %>% 
  gather(A_mort, A_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(No_dens_A_sea_Rsea_10_gathered)
tail(No_dens_A_sea_Rsea_10_gathered) # Good

No_dens_A_sea_Rsea_10_gathered$A_mort=gsub("X","", No_dens_A_sea_Rsea_10_gathered$A_mort)

head(No_dens_A_sea_Rsea_10_gathered)

No_dens_A_sea_Rsea_10_gathered_Plot_format<-No_dens_A_sea_Rsea_10_gathered[,c(4,2,3)]

No_dens_A_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(No_dens_A_sea_Rsea_10_gathered_Plot_format$R_river_Max))

head(No_dens_A_sea_Rsea_10_gathered_Plot_format)

No_dens_A_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(No_dens_A_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(No_dens_A_sea_Rsea_10_gathered_Plot_format)


No_dens_A_sea_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (i)"
No_dens_A_sea_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(No_dens_A_sea_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario ii

str(dens_A_sea_Rsea_10)
head(dens_A_sea_Rsea_10)

dens_A_sea_Rsea_10_gathered<-dens_A_sea_Rsea_10%>% 
  gather(A_mort, A_sea, 2:13) %>% 
  mutate(R_river_Max=X)

head(dens_A_sea_Rsea_10_gathered)
tail(dens_A_sea_Rsea_10_gathered) # Good

dens_A_sea_Rsea_10_gathered$A_mort=gsub("X","", dens_A_sea_Rsea_10_gathered$A_mort)

head(dens_A_sea_Rsea_10_gathered)

dens_A_sea_Rsea_10_gathered_Plot_format<-dens_A_sea_Rsea_10_gathered[,c(4,2,3)]

dens_A_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor<-as.factor(as.character(dens_A_sea_Rsea_10_gathered_Plot_format$R_river_Max))

head(dens_A_sea_Rsea_10_gathered_Plot_format)

dens_A_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor <- factor(dens_A_sea_Rsea_10_gathered_Plot_format$R_river_max_Factor,
                                                                        levels = c("1","3","6","9","12","15","18","21","24","27","30"))

head(dens_A_sea_Rsea_10_gathered_Plot_format)


dens_A_sea_Rsea_10_gathered_Plot_format$Scenario<-"Scenario (ii)"
dens_A_sea_Rsea_10_gathered_Plot_format$Rsea<-"Rsea=10"

#x11()
ggplot(dens_A_sea_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Scenario iii

str(dens_comp_A_sea_Rsea_10)
head(dens_comp_A_sea_Rsea_10)

dens_comp_A_sea_Rsea_10_gathered<-dens_comp_A_sea_Rsea_10 %>% 
  gather(A_mort, A_sea, 2:16) %>% 
  mutate(R_river_Max=X)

head(dens_comp_A_sea_Rsea_10_gathered)
tail(dens_comp_A_sea_Rsea_10_gathered) # Good

dens_comp_A_sea_Rsea_10_gathered$A_mort=gsub("X","", dens_comp_A_sea_Rsea_10_gathered$A_mort)

head(dens_comp_A_sea_Rsea_10_gathered)

dens_comp_A_sea_Rsea_10_gathered<-dens_comp_A_sea_Rsea_10_gathered[,c(4,2,3)]

dens_comp_A_sea_Rsea_10_gathered$R_river_max_Factor<-as.factor(as.character(dens_comp_A_sea_Rsea_10_gathered$R_river_Max))

head(dens_comp_A_sea_Rsea_10_gathered)

dens_comp_A_sea_Rsea_10_gathered$R_river_max_Factor <- factor(dens_comp_A_sea_Rsea_10_gathered$R_river_max_Factor,
                                                             levels = c("1","3","6","9","12","15","18","21","24","27","30"))

dens_comp_A_sea_Rsea_10_gathered$A_mort <- factor(dens_comp_A_sea_Rsea_10_gathered$A_mort,
                                                 levels = c("0.01","0.03","0.06","0.09","0.12","0.15",
                                                            "0.18","0.21","0.24","0.30","0.60","0.90","3","6","12"))
head(dens_comp_A_sea_Rsea_10_gathered)


dens_comp_A_sea_Rsea_10_gathered$Scenario<-"Scenario (iii)"
dens_comp_A_sea_Rsea_10_gathered$Rsea<-"Rsea=10"

head(dens_comp_A_sea_Rsea_10_gathered)

#x11()
ggplot(dens_comp_A_sea_Rsea_10_gathered, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")



#### Combined heatmaps each scenario RSea = 10 #####

# Scenario 1

# J_river:

ggplot(No_dens_J_river_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

head(No_dens_J_river_Rsea_10_gathered_Plot_format)

No_dens_J_river_Rsea_10_gathered_Plot_format$Stage<-"Jriver"

No_dens_J_river_Rsea_10_gathered_Plot_format<-No_dens_J_river_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=J_river)

# J_sea:

ggplot(No_dens_J_sea_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

head(No_dens_J_sea_Rsea_10_gathered_Plot_format)

No_dens_J_sea_Rsea_10_gathered_Plot_format$Stage<-"Jsea"

No_dens_J_sea_Rsea_10_gathered_Plot_format<-No_dens_J_sea_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=J_sea)

# A_sea

ggplot(No_dens_A_sea_Rsea_10_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

head(No_dens_A_sea_Rsea_10_gathered_Plot_format)

# Change name to A_mat

No_dens_A_sea_Rsea_10_gathered_Plot_format<-No_dens_A_sea_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=A_sea)

No_dens_A_sea_Rsea_10_gathered_Plot_format$Stage<-"Amat"

head(No_dens_A_sea_Rsea_10_gathered_Plot_format)


# Combine_datatsets:

Scenario_i_Rsea_10<-bind_rows(No_dens_J_river_Rsea_10_gathered_Plot_format, 
                              No_dens_J_sea_Rsea_10_gathered_Plot_format, 
                              No_dens_A_sea_Rsea_10_gathered_Plot_format)

head(Scenario_i_Rsea_10)

Scenario_i_Rsea_10$Stage<-factor(Scenario_i_Rsea_10$Stage,
                                   levels = c("Jriver","Jsea","Amat"))

# Make 0 boxes grey



# Plot scenario i:

head(Scenario_i_Rsea_10)

Scenario_i_Rsea_10$Biomass<-na_if(Scenario_i_Rsea_10$Biomass, 0) 

ggplot(Scenario_i_Rsea_10, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))


# Save it:

Scenario_i_Rsea_10_plot<-ggplot(Scenario_i_Rsea_10, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

Scenario_i_Rsea_10_plot # Looks good

# Scenario ii

# J_river

head(dens_J_river_Rsea_10_gathered_Plot_format)

dens_J_river_Rsea_10_gathered_Plot_format$Stage<-"Jriver"

dens_J_river_Rsea_10_gathered_Plot_format<-dens_J_river_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=J_river)

head(dens_J_river_Rsea_10_gathered_Plot_format)

# J_sea

head(dens_J_sea_Rsea_10_gathered_Plot_format)

dens_J_sea_Rsea_10_gathered_Plot_format$Stage<-"Jsea"

dens_J_sea_Rsea_10_gathered_Plot_format<-dens_J_sea_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=J_sea)

head(dens_J_sea_Rsea_10_gathered_Plot_format)


# A_mat

head(dens_A_sea_Rsea_10_gathered_Plot_format)

dens_A_sea_Rsea_10_gathered_Plot_format$Stage<-"Amat"

dens_A_sea_Rsea_10_gathered_Plot_format<-dens_A_sea_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=A_sea)

head(dens_A_sea_Rsea_10_gathered_Plot_format)


# Combine_datatsets:

Scenario_ii_Rsea_10<-bind_rows(dens_J_river_Rsea_10_gathered_Plot_format, 
                               dens_J_sea_Rsea_10_gathered_Plot_format, 
                               dens_A_sea_Rsea_10_gathered_Plot_format)

head(Scenario_ii_Rsea_10)

Scenario_ii_Rsea_10$Stage<-factor(Scenario_ii_Rsea_10$Stage,
                                 levels = c("Jriver","Jsea","Amat"))

head(Scenario_ii_Rsea_10)

# Plot scenario ii:

head(Scenario_ii_Rsea_10)

Scenario_ii_Rsea_10$Biomass<-na_if(Scenario_ii_Rsea_10$Biomass, 0) 

ggplot(Scenario_ii_Rsea_10, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "BuPu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))



# Save it:

Scenario_ii_Rsea_10_plot<-ggplot(Scenario_ii_Rsea_10, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "BuPu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=25, colour="black"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

Scenario_ii_Rsea_10_plot

# Scenario iii

#J_river:

dens_comp_J_river_Rsea_10_gathered_Plot_format$Stage<-"Jriver"

dens_comp_J_river_Rsea_10_gathered_Plot_format<-dens_comp_J_river_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=J_river)

head(dens_comp_J_river_Rsea_10_gathered_Plot_format)

#J_sea:

head(dens_comp_J_sea_Rsea_10_gathered_Plot_format)

dens_comp_J_sea_Rsea_10_gathered_Plot_format$Stage<-"Jsea"

dens_comp_J_sea_Rsea_10_gathered_Plot_format<-dens_comp_J_sea_Rsea_10_gathered_Plot_format %>% 
  rename(Biomass=J_sea)

head(dens_comp_J_sea_Rsea_10_gathered_Plot_format)

#A_sea

head(dens_comp_A_sea_Rsea_10_gathered)

dens_comp_A_sea_Rsea_10_gathered$Stage<-"Amat"

dens_comp_A_sea_Rsea_10_gathered<-dens_comp_A_sea_Rsea_10_gathered %>% 
  rename(Biomass=A_sea)

head(dens_comp_A_sea_Rsea_10_gathered)

# Combine datasets:

Scenario_iii_Rsea_10<-bind_rows(dens_comp_J_river_Rsea_10_gathered_Plot_format, 
                                dens_comp_J_sea_Rsea_10_gathered_Plot_format, 
                                dens_comp_A_sea_Rsea_10_gathered)

head(Scenario_iii_Rsea_10)

Scenario_iii_Rsea_10$Stage<-factor(Scenario_iii_Rsea_10$Stage,
                                  levels = c("Jriver","Jsea","Amat"))


# Plot scenario iii:

head(Scenario_iii_Rsea_10)

Scenario_iii_Rsea_10$Biomass<-na_if(Scenario_iii_Rsea_10$Biomass, 0) 


ggplot(Scenario_iii_Rsea_10, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "GnBu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Save it:

Scenario_iii_Rsea_10_plot<-ggplot(Scenario_iii_Rsea_10, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "GnBu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

Scenario_iii_Rsea_10_plot


## Combined plot stage-specific biomass.Rmax,sea=10, in MS IV ####

Scenario_i_Rsea_10_plot+Scenario_ii_Rsea_10_plot+Scenario_iii_Rsea_10_plot+plot_layout(ncol=1)


#### Combined heatmaps each scenario RSea = 2 #####

# Scenario i

# J_river:

ggplot(No_dens_J_river_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_river), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

head(No_dens_J_river_Rsea_2_gathered_Plot_format)

No_dens_J_river_Rsea_2_gathered_Plot_format$Stage<-"Jriver"

No_dens_J_river_Rsea_2_gathered_Plot_format<-No_dens_J_river_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=J_river)

head(No_dens_J_river_Rsea_2_gathered_Plot_format)

# J_sea:

ggplot(No_dens_J_sea_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = J_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

head(No_dens_J_sea_Rsea_2_gathered_Plot_format)

No_dens_J_sea_Rsea_2_gathered_Plot_format$Stage<-"Jsea"

No_dens_J_sea_Rsea_2_gathered_Plot_format<-No_dens_J_sea_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=J_sea)

head(No_dens_J_sea_Rsea_2_gathered_Plot_format)

# A_sea

ggplot(No_dens_A_sea_Rsea_2_gathered_Plot_format, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = A_sea), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

head(No_dens_A_sea_Rsea_2_gathered_Plot_format)

# Change name to A_mat

No_dens_A_sea_Rsea_2_gathered_Plot_format<-No_dens_A_sea_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=A_sea)

No_dens_A_sea_Rsea_2_gathered_Plot_format$Stage<-"Amat"

head(No_dens_A_sea_Rsea_2_gathered_Plot_format)


# Combine_datatsets:
head(No_dens_J_river_Rsea_2_gathered_Plot_format)
head(No_dens_J_sea_Rsea_2_gathered_Plot_format)
head(No_dens_A_sea_Rsea_2_gathered_Plot_format) # looks good

# Check structure:

str(No_dens_J_river_Rsea_2_gathered_Plot_format)
str(No_dens_J_sea_Rsea_2_gathered_Plot_format)
str(No_dens_A_sea_Rsea_2_gathered_Plot_format)

Scenario_i_Rsea_2<-bind_rows(No_dens_J_river_Rsea_2_gathered_Plot_format, 
                              No_dens_J_sea_Rsea_2_gathered_Plot_format, 
                              No_dens_A_sea_Rsea_2_gathered_Plot_format)

head(Scenario_i_Rsea_2)

Scenario_i_Rsea_2$Stage<-factor(Scenario_i_Rsea_2$Stage,
                                 levels = c("Jriver","Jsea","Amat"))
head(Scenario_i_Rsea_2)


# Plot scenario i:

head(Scenario_i_Rsea_2)

Scenario_i_Rsea_2$Biomass<-na_if(Scenario_i_Rsea_2$Biomass, 0) 


ggplot(Scenario_i_Rsea_2, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))


# Save it:

Scenario_i_Rsea_2_plot<-ggplot(Scenario_i_Rsea_2, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

Scenario_i_Rsea_2_plot

# Scenario ii, Rmax,sea=2

# J_river

head(dens_J_river_Rsea_2_gathered_Plot_format)

dens_J_river_Rsea_2_gathered_Plot_format$Stage<-"Jriver"

dens_J_river_Rsea_2_gathered_Plot_format<-dens_J_river_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=J_river)

head(dens_J_river_Rsea_2_gathered_Plot_format)

# J_sea

head(dens_J_sea_Rsea_2_gathered_Plot_format)

dens_J_sea_Rsea_2_gathered_Plot_format$Stage<-"Jsea"

dens_J_sea_Rsea_2_gathered_Plot_format<-dens_J_sea_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=J_sea)

head(dens_J_sea_Rsea_2_gathered_Plot_format)


# A_mat

head(dens_A_sea_Rsea_2_gathered_Plot_format)

dens_A_sea_Rsea_2_gathered_Plot_format$Stage<-"Amat"

dens_A_sea_Rsea_2_gathered_Plot_format<-dens_A_sea_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=A_sea)

head(dens_A_sea_Rsea_2_gathered_Plot_format)


# Combine_datatsets:

Scenario_ii_Rsea_2<-bind_rows(dens_J_river_Rsea_2_gathered_Plot_format, 
                               dens_J_sea_Rsea_2_gathered_Plot_format, 
                               dens_A_sea_Rsea_2_gathered_Plot_format)

head(Scenario_ii_Rsea_2)

Scenario_ii_Rsea_2$Stage<-factor(Scenario_ii_Rsea_2$Stage,
                                  levels = c("Jriver","Jsea","Amat"))

head(Scenario_ii_Rsea_2)

# Plot scenario ii:

head(Scenario_ii_Rsea_2)

Scenario_ii_Rsea_2$Biomass<-na_if(Scenario_ii_Rsea_2$Biomass, 0) 


ggplot(Scenario_ii_Rsea_2, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "BuPu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))



# Save it:

Scenario_ii_Rsea_2_plot<-ggplot(Scenario_ii_Rsea_2, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "BuPu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=25, colour="black"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

Scenario_ii_Rsea_2_plot # Looks good

# Scenario iii, Rmax,sea=2

#J_river:

dens_comp_J_river_Rsea_2_gathered_Plot_format$Stage<-"Jriver"

dens_comp_J_river_Rsea_2_gathered_Plot_format<-dens_comp_J_river_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=J_river)

head(dens_comp_J_river_Rsea_2_gathered_Plot_format)

#J_sea:

head(dens_comp_J_sea_Rsea_2_gathered_Plot_format)

dens_comp_J_sea_Rsea_2_gathered_Plot_format$Stage<-"Jsea"

dens_comp_J_sea_Rsea_2_gathered_Plot_format<-dens_comp_J_sea_Rsea_2_gathered_Plot_format %>% 
  rename(Biomass=J_sea)

head(dens_comp_J_sea_Rsea_2_gathered_Plot_format)

#A_sea

head(dens_comp_A_sea_Rsea_2_gathered)

dens_comp_A_sea_Rsea_2_gathered$Stage<-"Amat"

dens_comp_A_sea_Rsea_2_gathered<-dens_comp_A_sea_Rsea_2_gathered %>% 
  rename(Biomass=A_sea)

head(dens_comp_A_sea_Rsea_2_gathered)

# Combine datasets:

Scenario_iii_Rsea_2<-bind_rows(dens_comp_J_river_Rsea_2_gathered_Plot_format, 
                                dens_comp_J_sea_Rsea_2_gathered_Plot_format, 
                                dens_comp_A_sea_Rsea_2_gathered)

head(Scenario_iii_Rsea_2)

Scenario_iii_Rsea_2$Stage<-factor(Scenario_iii_Rsea_2$Stage,
                                   levels = c("Jriver","Jsea","Amat"))


# Plot scenario iii:


head(Scenario_iii_Rsea_2)

Scenario_iii_Rsea_2$Biomass<-na_if(Scenario_iii_Rsea_2$Biomass, 0) 


ggplot(Scenario_iii_Rsea_2, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "GnBu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Save it:

Scenario_iii_Rsea_2_plot<-ggplot(Scenario_iii_Rsea_2, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Biomass), colour="black")+
  scale_fill_distiller(palette = "GnBu", direction=1)+
  facet_grid(Scenario~Stage)+
  theme_bw()+
  labs(fill="Biomass")+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

Scenario_iii_Rsea_2_plot # Looks good


## Combined plot stage-specific biomass.Rmax,sea=2, in MS IV ####

Scenario_i_Rsea_2_plot+Scenario_ii_Rsea_2_plot+Scenario_iii_Rsea_2_plot+plot_layout(ncol=1)



### HEATMAPS MS IV, tot_biomass and biomass ratios #####

# Scenario 1, Rsea= 2 ####


# J river

No_dens_J_river_Rsea_2<-read.delim("Matris_J_river_Rsea2_no_dens_Rfile.csv", sep=";")
head(No_dens_J_river_Rsea_2)

# J sea

No_dens_J_sea_Rsea_2<-read.delim("Matris_J_sea_Rsea2_no_dens_Rfile.csv", sep=";")
head(No_dens_J_sea_Rsea_2)

# A sea

No_dens_A_sea_Rsea_2<-read.delim("Matris_A_sea_Rsea2_no_dens_Rfile.csv", sep=";")
head(No_dens_A_sea_Rsea_2)

# Combine data:

tot_biomass_no_dens_Rsea_2<-No_dens_J_river_Rsea_2+No_dens_J_sea_Rsea_2+No_dens_A_sea_Rsea_2

head(tot_biomass_no_dens_Rsea_2) # there we go

#Plot the data:

Scenario_i_Rsea2_tot_bio<-tot_biomass_no_dens_Rsea_2%>% 
  gather(A_mort, Total_biomass, 2:13) %>% 
  mutate(R_river_Max=X)

  head(Scenario_i_Rsea2_tot_bio)

Scenario_i_Rsea2_tot_bio$A_mort=gsub("X","", Scenario_i_Rsea2_tot_bio$A_mort)

  head(Scenario_i_Rsea2_tot_bio)

Scenario_i_Rsea2_tot_bio$R_river_Max<-Scenario_i_Rsea2_tot_bio$R_river_Max/3 # because we added 3 matrices together

  head(Scenario_i_Rsea2_tot_bio)
  
Scenario_i_Rsea2_tot_bio<-Scenario_i_Rsea2_tot_bio[,c(4,2,3)] # chnage order of columns

Scenario_i_Rsea2_tot_bio$R_river_max_Factor<-as.factor(as.character(Scenario_i_Rsea2_tot_bio$R_river_Max))

head(Scenario_i_Rsea2_tot_bio)

Scenario_i_Rsea2_tot_bio$R_river_max_Factor <- factor(Scenario_i_Rsea2_tot_bio$R_river_max_Factor,
                                                                         levels = c("1","3","6","9","12","15","18","21","24","27","30"))
head(Scenario_i_Rsea2_tot_bio)

Scenario_i_Rsea2_tot_bio$Scenario<-"Scenario (i)"
Scenario_i_Rsea2_tot_bio$Rsea<-"Rsea=2"


#x11() # If you want a separate window for the plot.
ggplot(Scenario_i_Rsea2_tot_bio, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")



# Scenario 1, Rsea= 10 ####

No_dens_J_river_Rsea_10<-read.delim("Matris_J_river_Rsea10_no_dens_Rfile.csv", sep=";")
head(No_dens_J_river_Rsea_10)

No_dens_J_sea_Rsea_10<-read.delim("Matris_J_sea_Rsea10_no_dens_Rfile.csv", sep=";")
head(No_dens_J_sea_Rsea_10)

No_dens_A_sea_Rsea_10<-read.delim("Matris_A_sea_Rsea10_no_dens_Rfile.csv", sep=";")
head(No_dens_A_sea_Rsea_10)

# Combine data:

tot_biomass_no_dens_Rsea_10<-No_dens_J_river_Rsea_10+No_dens_J_sea_Rsea_10+No_dens_A_sea_Rsea_10

head(tot_biomass_no_dens_Rsea_10) # there we go

#Plot the data:

Scenario_i_Rsea10_tot_bio<-tot_biomass_no_dens_Rsea_10%>% 
  gather(A_mort, Total_biomass, 2:13) %>% 
  mutate(R_river_Max=X)

head(Scenario_i_Rsea10_tot_bio)

Scenario_i_Rsea10_tot_bio$A_mort=gsub("X","", Scenario_i_Rsea10_tot_bio$A_mort)

head(Scenario_i_Rsea10_tot_bio)

Scenario_i_Rsea10_tot_bio$R_river_Max<-Scenario_i_Rsea10_tot_bio$R_river_Max/3 # because we added 3 matrices together

head(Scenario_i_Rsea10_tot_bio)

Scenario_i_Rsea10_tot_bio<-Scenario_i_Rsea10_tot_bio[,c(4,2,3)] # chnage order of columns

Scenario_i_Rsea10_tot_bio$R_river_max_Factor<-as.factor(as.character(Scenario_i_Rsea10_tot_bio$R_river_Max))

head(Scenario_i_Rsea10_tot_bio)

Scenario_i_Rsea10_tot_bio$R_river_max_Factor <- factor(Scenario_i_Rsea10_tot_bio$R_river_max_Factor,
                                                     levels = c("1","3","6","9","12","15","18","21","24","27","30"))
head(Scenario_i_Rsea10_tot_bio)

Scenario_i_Rsea10_tot_bio$Scenario<-"Scenario (i)"
Scenario_i_Rsea10_tot_bio$Rsea<-"Rsea=10"


#x11()
ggplot(Scenario_i_Rsea10_tot_bio, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

# Combine:

Scenario_i_comb<-rbind(Scenario_i_Rsea2_tot_bio, Scenario_i_Rsea10_tot_bio)

head(Scenario_i_comb)
str(Scenario_i_comb)

Scenario_i_comb$Rsea<-as.factor(Scenario_i_comb$Rsea)

Scenario_i_comb$Rsea<-factor(Scenario_i_comb$Rsea, # Re-organize order of factors
                                             levels=c("Rsea=2", "Rsea=10"))

Scenario_i_comb<-Scenario_i_comb %>% 
  mutate(Labels_facet=ifelse(Rsea=="Rsea=2", "Rmax,sea=2", 
                             ifelse(Rsea=="Rsea=10", "Rmax,sea=10", "1000")))

Scenario_i_comb$Labels_facet<-factor(Scenario_i_comb$Labels_facet, # Re-organize order of factors
                              levels=c("Rmax,sea=2", "Rmax,sea=10"))

#x11()
ggplot(Scenario_i_comb, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")



### Biomass ratio Scenario i, Rmax,sea =10 #### 
# Jriver, Amat


head(No_dens_J_river_Rsea_10)
head(No_dens_A_sea_Rsea_10)

No_dens_A_sea_Rsea_10[,1]

Ratio_Scenario_i_Rmaxsea_10<-No_dens_J_river_Rsea_10/No_dens_A_sea_Rsea_10
head(Ratio_Scenario_i_Rmaxsea_10)

Ratio_Scenario_i_Rmaxsea_10$X<-No_dens_A_sea_Rsea_10[,1] # to get the river-producivity estimates correct. 

head(Ratio_Scenario_i_Rmaxsea_10)

# Plot the data:

#Plot the data:

Ratio_Scenario_i_Rmaxsea_10_long<-Ratio_Scenario_i_Rmaxsea_10%>% 
  gather(A_mort, Jriver_Adult_ratio, 2:13) %>% 
  mutate(R_river_Max=X)

head(Ratio_Scenario_i_Rmaxsea_10_long)

Ratio_Scenario_i_Rmaxsea_10_long$A_mort=gsub("X","", Ratio_Scenario_i_Rmaxsea_10_long$A_mort)

head(Ratio_Scenario_i_Rmaxsea_10_long)

Ratio_Scenario_i_Rmaxsea_10_long<-Ratio_Scenario_i_Rmaxsea_10_long[,c(4,2,3)] # chnage order of columns

Ratio_Scenario_i_Rmaxsea_10_long$R_river_max_Factor<-as.factor(as.character(Ratio_Scenario_i_Rmaxsea_10_long$R_river_Max))

head(Ratio_Scenario_i_Rmaxsea_10_long)

Ratio_Scenario_i_Rmaxsea_10_long$R_river_max_Factor <- factor(Ratio_Scenario_i_Rmaxsea_10_long$R_river_max_Factor,
                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))
head(Ratio_Scenario_i_Rmaxsea_10_long)

Ratio_Scenario_i_Rmaxsea_10_long$Scenario<-"Scenario (i)"
Ratio_Scenario_i_Rmaxsea_10_long$Rsea<-"Rmax,sea=10"


#x11()
ggplot(Ratio_Scenario_i_Rmaxsea_10_long, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Jriver_Adult_ratio), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

#### Scenario ii, Rsea= 2 ####


dens_J_river_Rsea_2<-read.delim("Matris_J_river_Condensed_R_file_Rsea2_Density_dep.csv", sep=";")
head(dens_J_river_Rsea_2)

J_sea_dens_dep_Rsea2<-read.delim("Matris_J_sea_Condensed_R_file_Rsea2_Density_dep.csv", sep=";")
head(J_sea_dens_dep_Rsea2)

A_sea_dens_dep_Rsea2<-read.delim("Matris_A_sea_Condensed_R_file_Rsea2_Density_dep.csv", sep=";")
head(A_sea_dens_dep_Rsea2)

# Combine data:

tot_biomass_dens_dep_Rsea_2<-dens_J_river_Rsea_2+J_sea_dens_dep_Rsea2+A_sea_dens_dep_Rsea2

head(tot_biomass_dens_dep_Rsea_2) # there we go

#Plot the data:

Scenario_ii_Rsea2_tot_bio<-tot_biomass_dens_dep_Rsea_2%>% 
  gather(A_mort, Total_biomass, 2:13) %>% 
  mutate(R_river_Max=X)

head(Scenario_ii_Rsea2_tot_bio)

Scenario_ii_Rsea2_tot_bio$A_mort=gsub("X","", Scenario_ii_Rsea2_tot_bio$A_mort)

head(Scenario_ii_Rsea2_tot_bio)

Scenario_ii_Rsea2_tot_bio$R_river_Max<-Scenario_ii_Rsea2_tot_bio$R_river_Max/3 # because we added 3 matrices together

head(Scenario_ii_Rsea2_tot_bio)

Scenario_ii_Rsea2_tot_bio<-Scenario_ii_Rsea2_tot_bio[,c(4,2,3)] # change order of columns

Scenario_ii_Rsea2_tot_bio$R_river_max_Factor<-as.factor(as.character(Scenario_ii_Rsea2_tot_bio$R_river_Max))

head(Scenario_ii_Rsea2_tot_bio)

Scenario_ii_Rsea2_tot_bio$R_river_max_Factor <- factor(Scenario_ii_Rsea2_tot_bio$R_river_max_Factor,
                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))
head(Scenario_ii_Rsea2_tot_bio)

Scenario_ii_Rsea2_tot_bio$Scenario<-"Scenario (ii)"
Scenario_ii_Rsea2_tot_bio$Rsea<-"Rsea=2"


ggplot(Scenario_ii_Rsea2_tot_bio, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

#### Scenario ii, Rsea= 10 ####


Dens_Dep_J_river_Rsea_10<-read.delim("Matris_J_river_Condensed_R_file_Rsea_10_dens_dep.csv", sep=";")
head(Dens_Dep_J_river_Rsea_10)

Dens_Dep_J_sea_Rsea_10<-read.delim("Matris_J_sea_Condensed_R_file_Rsea_10_dens_dep.csv", sep=";")
head(Dens_Dep_J_sea_Rsea_10)

Dens_Dep_A_sea_Rsea_10<-read.delim("Matris_A_sea_Condensed_R_file_Rsea_10_dens_dep.csv", sep=";")
head(Dens_Dep_A_sea_Rsea_10)

# Combine:

tot_biomass_dens_dep_Rsea_10<-Dens_Dep_J_river_Rsea_10+Dens_Dep_J_sea_Rsea_10+Dens_Dep_A_sea_Rsea_10

head(tot_biomass_dens_dep_Rsea_10) # there we go

#Plot the data:

Scenario_ii_Rsea10_tot_bio<-tot_biomass_dens_dep_Rsea_10%>% 
  gather(A_mort, Total_biomass, 2:13) %>% 
  mutate(R_river_Max=X)

head(Scenario_ii_Rsea10_tot_bio)

Scenario_ii_Rsea10_tot_bio$A_mort=gsub("X","", Scenario_ii_Rsea10_tot_bio$A_mort)

head(Scenario_ii_Rsea10_tot_bio)

Scenario_ii_Rsea10_tot_bio$R_river_Max<-Scenario_ii_Rsea10_tot_bio$R_river_Max/3 # because we added 3 matrices together

head(Scenario_ii_Rsea10_tot_bio)

Scenario_ii_Rsea10_tot_bio<-Scenario_ii_Rsea10_tot_bio[,c(4,2,3)] # chnage order of columns

Scenario_ii_Rsea10_tot_bio$R_river_max_Factor<-as.factor(as.character(Scenario_ii_Rsea10_tot_bio$R_river_Max))

head(Scenario_ii_Rsea10_tot_bio)

Scenario_ii_Rsea10_tot_bio$R_river_max_Factor <- factor(Scenario_ii_Rsea10_tot_bio$R_river_max_Factor,
                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))
head(Scenario_ii_Rsea10_tot_bio)

Scenario_ii_Rsea10_tot_bio$Scenario<-"Scenario (ii)"
Scenario_ii_Rsea10_tot_bio$Rsea<-"Rsea=10"


#x11()
ggplot(Scenario_ii_Rsea10_tot_bio, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Combine scenario (ii)

Scenario_ii_comb<-rbind(Scenario_ii_Rsea2_tot_bio, Scenario_ii_Rsea10_tot_bio)

head(Scenario_ii_comb)
str(Scenario_ii_comb)

Scenario_ii_comb$Rsea<-as.factor(Scenario_ii_comb$Rsea)

Scenario_ii_comb$Rsea<-factor(Scenario_ii_comb$Rsea, # Re-organize order of factors
                             levels=c("Rsea=2", "Rsea=10"))

Scenario_ii_comb<-Scenario_ii_comb %>% 
  mutate(Labels_facet=ifelse(Rsea=="Rsea=2", "Rmax,sea=2", 
                             ifelse(Rsea=="Rsea=10", "Rmax,sea=10", "1000")))

Scenario_ii_comb$Labels_facet<-factor(Scenario_ii_comb$Labels_facet, # Re-organize order of factors
                              levels=c("Rmax,sea=2", "Rmax,sea=10"))

#x11()
ggplot(Scenario_ii_comb, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Labels_facet)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")



#### Biomass ratio scenario ii, Rsea,max=10 ####


head(Dens_Dep_J_river_Rsea_10)
head(Dens_Dep_A_sea_Rsea_10)

Dens_Dep_J_river_Rsea_10[,1]

Ratio_Scenario_ii_Rmaxsea_10<-Dens_Dep_J_river_Rsea_10/Dens_Dep_A_sea_Rsea_10
head(Ratio_Scenario_ii_Rmaxsea_10)

Ratio_Scenario_ii_Rmaxsea_10$X<-Dens_Dep_J_river_Rsea_10[,1] # to get the river-producivity estimates correct. 

head(Ratio_Scenario_ii_Rmaxsea_10)


#Plot the data:

Ratio_Scenario_ii_Rmaxsea_10_long<-Ratio_Scenario_ii_Rmaxsea_10%>% 
  gather(A_mort, Jriver_Adult_ratio, 2:13) %>% 
  mutate(R_river_Max=X)

head(Ratio_Scenario_ii_Rmaxsea_10_long)

Ratio_Scenario_ii_Rmaxsea_10_long$A_mort=gsub("X","", Ratio_Scenario_ii_Rmaxsea_10_long$A_mort)

head(Ratio_Scenario_ii_Rmaxsea_10_long)

Ratio_Scenario_ii_Rmaxsea_10_long<-Ratio_Scenario_ii_Rmaxsea_10_long[,c(4,2,3)] # chnage order of columns

Ratio_Scenario_ii_Rmaxsea_10_long$R_river_max_Factor<-as.factor(as.character(Ratio_Scenario_ii_Rmaxsea_10_long$R_river_Max))

head(Ratio_Scenario_ii_Rmaxsea_10_long)

Ratio_Scenario_ii_Rmaxsea_10_long$R_river_max_Factor <- factor(Ratio_Scenario_ii_Rmaxsea_10_long$R_river_max_Factor,
                                                              levels = c("1","3","6","9","12","15","18","21","24","27","30"))
head(Ratio_Scenario_ii_Rmaxsea_10_long)

Ratio_Scenario_ii_Rmaxsea_10_long$Scenario<-"Scenario (ii)"
Ratio_Scenario_ii_Rmaxsea_10_long$Rsea<-"Rmax,sea=10"


#x11()
ggplot(Ratio_Scenario_ii_Rmaxsea_10_long, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Jriver_Adult_ratio), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")





#### Scenario iii, Rsea= 2 ####

Dens_Comp_J_river_Rsea_2<-read.delim("Matris_J_River_with_density_comp_Asea_Jsea_R2_Rfile.csv", sep=";")
head(Dens_Comp_J_river_Rsea_2)

Dens_Comp_J_sea_Rsea_2<-read.delim("Matris_J_sea_with_density_comp_Asea_Jsea_R2_Rfile.csv", sep=";")
head(Dens_Comp_J_sea_Rsea_2)

Dens_Comp_A_sea_Rsea_2<-read.delim("Matris_A_sea_with_density_comp_Asea_Jsea_R2_Rfile.csv", sep=";")
head(Dens_Comp_A_sea_Rsea_2)

# Combine data:

tot_biomass_dens_dep_comp_Rsea_2<-Dens_Comp_J_river_Rsea_2+Dens_Comp_J_sea_Rsea_2+Dens_Comp_A_sea_Rsea_2

head(tot_biomass_dens_dep_comp_Rsea_2) # there we go

#Plot the data:

Scenario_iii_Rsea2_tot_bio<-tot_biomass_dens_dep_comp_Rsea_2%>% 
  gather(A_mort, Total_biomass, 2:16) %>% 
  mutate(R_river_Max=X)

head(Scenario_iii_Rsea2_tot_bio)

Scenario_iii_Rsea2_tot_bio$A_mort=gsub("X","", Scenario_iii_Rsea2_tot_bio$A_mort)

head(Scenario_iii_Rsea2_tot_bio)

Scenario_iii_Rsea2_tot_bio$R_river_Max<-Scenario_iii_Rsea2_tot_bio$R_river_Max/3 # because we added 3 matrices together

head(Scenario_iii_Rsea2_tot_bio)

Scenario_iii_Rsea2_tot_bio<-Scenario_iii_Rsea2_tot_bio[,c(4,2,3)] # chnage order of columns

Scenario_iii_Rsea2_tot_bio$R_river_max_Factor<-as.factor(as.character(Scenario_iii_Rsea2_tot_bio$R_river_Max))

head(Scenario_iii_Rsea2_tot_bio)

Scenario_iii_Rsea2_tot_bio$R_river_max_Factor <- factor(Scenario_iii_Rsea2_tot_bio$R_river_max_Factor,
                                                       levels = c("1","3","6","9","12","15","18","21","24","27","30"))
head(Scenario_iii_Rsea2_tot_bio)

Scenario_iii_Rsea2_tot_bio$Scenario<-"Scenario (iii)"
Scenario_iii_Rsea2_tot_bio$Rsea<-"Rsea=2"


#x11()
ggplot(Scenario_iii_Rsea2_tot_bio, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")

#### Scenario iii, Rsea= 10 ####

Dens_Comp_J_river_Rsea_10<-read.delim("Matris_J_River_with_density_comp_Asea_Jsea_R10_Rfile.csv", sep=";")
head(Dens_Comp_J_river_Rsea_10)

Dens_Comp_J_sea_Rsea_10<-read.delim("Matris_J_sea_with_density_comp_Asea_Jsea_R10_Rfile.csv", sep=";")
head(Dens_Comp_J_sea_Rsea_10)

Dens_Comp_A_sea_Rsea_10<-read.delim("Matris_A_sea_with_density_comp_Asea_Jsea_R10_Rfile.csv", sep=";")
head(Dens_Comp_A_sea_Rsea_10)

# Combine data:

tot_biomass_dens_dep_comp_Rsea_10<-Dens_Comp_J_river_Rsea_10+Dens_Comp_J_sea_Rsea_10+Dens_Comp_A_sea_Rsea_10

head(tot_biomass_dens_dep_comp_Rsea_10) # there we go

#Plot the data:

Scenario_iii_Rsea10_tot_bio<-tot_biomass_dens_dep_comp_Rsea_10%>% 
  gather(A_mort, Total_biomass, 2:16) %>% 
  mutate(R_river_Max=X)

head(Scenario_iii_Rsea10_tot_bio)

Scenario_iii_Rsea10_tot_bio$A_mort=gsub("X","", Scenario_iii_Rsea10_tot_bio$A_mort)

head(Scenario_iii_Rsea10_tot_bio)

Scenario_iii_Rsea10_tot_bio$R_river_Max<-Scenario_iii_Rsea10_tot_bio$R_river_Max/3 # because we added 3 matrices together

head(Scenario_iii_Rsea10_tot_bio)

Scenario_iii_Rsea10_tot_bio<-Scenario_iii_Rsea10_tot_bio[,c(4,2,3)] # change order of columns

Scenario_iii_Rsea10_tot_bio$R_river_max_Factor<-as.factor(as.character(Scenario_iii_Rsea10_tot_bio$R_river_Max))

head(Scenario_iii_Rsea10_tot_bio)

Scenario_iii_Rsea10_tot_bio$R_river_max_Factor <- factor(Scenario_iii_Rsea10_tot_bio$R_river_max_Factor,
                                                        levels = c("1","3","6","9","12","15","18","21","24","27","30"))

Scenario_iii_Rsea10_tot_bio$A_mort <- factor(Scenario_iii_Rsea10_tot_bio$A_mort,
                                                  levels = c("0.01","0.03","0.06","0.09","0.12","0.15",
                                                             "0.18","0.21","0.24","0.30","0.60","0.90","3","6","12"))
head(Scenario_iii_Rsea10_tot_bio)

Scenario_iii_Rsea10_tot_bio$Scenario<-"Scenario (iii)"
Scenario_iii_Rsea10_tot_bio$Rsea<-"Rsea=10"


#x11()
ggplot(Scenario_iii_Rsea10_tot_bio, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


# Combine scenario (iii)

Scenario_iii_comb<-rbind(Scenario_iii_Rsea2_tot_bio, Scenario_iii_Rsea10_tot_bio)

head(Scenario_iii_comb)
str(Scenario_iii_comb)

Scenario_iii_comb$Rsea<-as.factor(Scenario_iii_comb$Rsea)

Scenario_iii_comb$Rsea<-factor(Scenario_iii_comb$Rsea, # Re-organize order of factors
                              levels=c("Rsea=2", "Rsea=10"))

Scenario_iii_comb$A_mort <- factor(Scenario_iii_comb$A_mort,
                                             levels = c("0.01","0.03","0.06","0.09","0.12","0.15",
                                                        "0.18","0.21","0.24","0.30","0.60","0.90","3","6","12"))

head(Scenario_iii_comb)

Scenario_iii_comb<-Scenario_iii_comb %>% 
  mutate(Labels_facet=ifelse(Rsea=="Rsea=2", "Rmax,sea=2", 
                             ifelse(Rsea=="Rsea=10", "Rmax,sea=10", "1000")))

Scenario_iii_comb$Labels_facet<-factor(Scenario_iii_comb$Labels_facet, # Re-organize order of factors
                               levels=c("Rmax,sea=2", "Rmax,sea=10"))

#x11()
ggplot(Scenario_iii_comb, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")


#### Biomass ratio scenario (iii), Rmax,sea=10 ####

head(Dens_Comp_J_river_Rsea_10)
head(Dens_Comp_A_sea_Rsea_10)

Dens_Comp_J_river_Rsea_10[,1]

Ratio_Scenario_iii_Rmaxsea_10<-Dens_Comp_J_river_Rsea_10/Dens_Comp_A_sea_Rsea_10
head(Ratio_Scenario_iii_Rmaxsea_10)

Ratio_Scenario_iii_Rmaxsea_10$X<-Dens_Comp_J_river_Rsea_10[,1] # to get the river-producivity estimates correct. 

head(Ratio_Scenario_iii_Rmaxsea_10)


#Plot the data:

Ratio_Scenario_iii_Rmaxsea_10_long<-Ratio_Scenario_iii_Rmaxsea_10%>% 
  gather(A_mort, Jriver_Adult_ratio, 2:16) %>% 
  mutate(R_river_Max=X)

head(Ratio_Scenario_iii_Rmaxsea_10_long)

Ratio_Scenario_iii_Rmaxsea_10_long$A_mort=gsub("X","", Ratio_Scenario_iii_Rmaxsea_10_long$A_mort)

head(Ratio_Scenario_iii_Rmaxsea_10_long)

Ratio_Scenario_iii_Rmaxsea_10_long<-Ratio_Scenario_iii_Rmaxsea_10_long[,c(4,2,3)] # chnage order of columns

Ratio_Scenario_iii_Rmaxsea_10_long$R_river_max_Factor<-as.factor(as.character(Ratio_Scenario_iii_Rmaxsea_10_long$R_river_Max))

head(Ratio_Scenario_iii_Rmaxsea_10_long)

Ratio_Scenario_iii_Rmaxsea_10_long$R_river_max_Factor <- factor(Ratio_Scenario_iii_Rmaxsea_10_long$R_river_max_Factor,
                                                               levels = c("1","3","6","9","12","15","18","21","24","27","30"))

sort(unique(Ratio_Scenario_iii_Rmaxsea_10_long$A_mort))

Ratio_Scenario_iii_Rmaxsea_10_long$A_mort<- factor(Ratio_Scenario_iii_Rmaxsea_10_long$A_mort,
                                                                levels = c("0.01","0.03","0.06","0.09","0.12","0.15","0.18","0.21","0.24", 
                                                                           "0.30","0.60","0.90","3","6","12"))

head(Ratio_Scenario_iii_Rmaxsea_10_long)

Ratio_Scenario_iii_Rmaxsea_10_long$Scenario<-"Scenario (iii)"
Ratio_Scenario_iii_Rmaxsea_10_long$Rsea<-"Rmax,sea=10"




#x11()
ggplot(Ratio_Scenario_iii_Rmaxsea_10_long, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Jriver_Adult_ratio), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")



# Combine all scenarios:

All_scenarios_2_para_biff<-rbind(Scenario_i_comb, Scenario_ii_comb, Scenario_iii_comb)
head(All_scenarios_2_para_biff)



# Plot 2-way biff all scenarios tot biomass####

ggplot(All_scenarios_2_para_biff, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Rsea)+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_text(size=25, colour="black"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1)) 


# Each scenario with their own colour scheme on its own

Scenario_i_comb$Total_biomass<-na_if(Scenario_i_comb$Total_biomass, 0) 


Scenario_i_plot<-ggplot(Scenario_i_comb, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(Scenario~Labels_facet)+
  labs(fill=expression(italic("Total biomass")))+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1)) 

Scenario_ii_comb$Total_biomass<-na_if(Scenario_ii_comb$Total_biomass, 0) # Make 0-biomass boxes grey

Scenario_ii_plot<-ggplot(Scenario_ii_comb, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "RdPu", direction=1)+
  facet_grid(Scenario~Labels_facet)+
  labs(fill=expression(italic("Total biomass")))+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=25, colour="black"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1)) 


Scenario_iii_comb$Total_biomass<-na_if(Scenario_iii_comb$Total_biomass, 0) # Make 0-biomass boxes grey

Scenario_iii_plot<-ggplot(Scenario_iii_comb, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Total_biomass), colour="black")+
  scale_fill_distiller(palette = "GnBu", direction=1)+
  facet_grid(Scenario~Labels_facet)+
  labs(fill=expression(italic("Total biomass")))+
  theme_bw()+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1)) 

#library(patchwork)
Scenario_i_plot+Scenario_ii_plot+Scenario_iii_plot+plot_layout(ncol=1)



### Combine juvenile:adult biomass ratios ####

head(Ratio_Scenario_i_Rmaxsea_10_long) # Scenario i
head(Ratio_Scenario_ii_Rmaxsea_10_long) # Scenario ii
head(Ratio_Scenario_iii_Rmaxsea_10_long) # Scenario iii

Ratio_Scenario_i_Rmaxsea_10_long_plot<-Ratio_Scenario_i_Rmaxsea_10_long[,c(2,3,4,5,6)]
Ratio_Scenario_ii_Rmaxsea_10_long_plot<-Ratio_Scenario_ii_Rmaxsea_10_long[,c(2,3,4,5,6)]
Ratio_Scenario_iii_Rmaxsea_10_long_plot<-Ratio_Scenario_iii_Rmaxsea_10_long[,c(2,3,4,5,6)]

head(Ratio_Scenario_iii_Rmaxsea_10_long_plot)

All_scenarios_Juv_Adult_Ratio<-rbind(Ratio_Scenario_i_Rmaxsea_10_long_plot, 
                                     Ratio_Scenario_ii_Rmaxsea_10_long_plot, 
                                     Ratio_Scenario_iii_Rmaxsea_10_long_plot)
head(All_scenarios_Juv_Adult_Ratio)


# Scenario i

Scenario_i_plot_juv_adult_biomass<-ggplot(Ratio_Scenario_i_Rmaxsea_10_long_plot, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Jriver_Adult_ratio), colour="black")+
  scale_fill_distiller(palette = "OrRd", direction=1)+
  facet_grid(~Scenario)+
  theme_bw()+
  labs(fill=expression(italic(J["river"]):italic(A["mat"])))+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

# Scenario ii:

Scenario_ii_plot_juv_adult_biomass<-ggplot(Ratio_Scenario_ii_Rmaxsea_10_long_plot, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Jriver_Adult_ratio), colour="black")+
  scale_fill_distiller(palette = "RdPu", direction=1)+
  facet_grid(~Scenario)+
  theme_bw()+
  labs(fill=expression(italic(J["river"]):italic(A["mat"])))+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=25, colour="black"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))

# Scenario iii:

Scenario_iii_plot_juv_adult_biomass<-ggplot(Ratio_Scenario_iii_Rmaxsea_10_long_plot, aes(y=A_mort, x=R_river_max_Factor)) + 
  geom_tile(aes(fill = Jriver_Adult_ratio), colour="black")+
  scale_fill_distiller(palette = "PuOr", direction=1)+
  facet_grid(~Scenario)+
  theme_bw()+
  labs(fill=expression(italic(J["river"]):italic(A["mat"])))+
  xlab("River productivity")+
  ylab("Adult mortality")+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"))+
  theme(panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=25, colour="black"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  #guides(col = guide_legend(nrow = 4))+
  #theme(legend.position="top")+
  theme(strip.background =element_rect(fill="white", colour="black"))+
  theme(strip.text = element_text(colour = "black", size = 15))+
  theme(legend.title = element_text(colour="black", size = 15))+
  theme(legend.text = element_text(colour="black", size  = 15))+
  guides(fill = guide_colourbar(ticks.colour = "black",
                                ticks.linewidth = 1,
                                frame.colour = "black",
                                frame.linewidth = 1))


Scenario_i_plot_juv_adult_biomass+Scenario_ii_plot_juv_adult_biomass+Scenario_iii_plot_juv_adult_biomass+plot_layout(ncol=1)
