
library(tidyverse)
library(ggplot2)
library(scales)
setwd("/Users/xuzhimeng/Desktop/EPD-20y/R_analysis_files/") # set your own directory

# ENV data from 1986 to 2020 ----------------------------------------------
library(scales)
env_long = read.csv("EPD Water Quality Meta Data 1986-2020.csv")
env_long  %>% head(30)
env_long %>% colnames()

# po4
env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>% 
  mutate(PO4 = Orthophosphate_Phosphorus %>% 
           str_replace_all(c("<0.005" = "0.0025", ## use 1/2 of lowest detection limitation to replace low values
                             "<0.024" = "0.012",
                             "<0.01" = "0.005")) %>% as.numeric())  %>% 
  select(Station, Month_passed,PO4) %>% 
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  ggplot(aes(x = Month_passed, y = PO4))+
  facet_wrap(~Station, ncol = 5) +
  geom_point(alpha = 0.4, aes(color = WCZ), shape = 1)+
  geom_smooth(se=T, method = "lm",aes(color = WCZ))+
  # geom_smooth(se=T, method = "gam",aes(color = WCZ))+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 45, hjust = 1, vjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab(expression(PO[4]^{"3-"}~(mg/L)))+
  xlab("")+
  # ggpubr::stat_cor(method = "pearson", size = 5.5, color = "black")+
  scale_x_continuous(breaks= c(47, 167,287,407),
                     labels = c("1990_01", "2000_01","2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  scale_y_log10(expand = c(0, 0),
                # using trans_format from the scales package, but one can also use expressions
                labels = trans_format('log10', math_format(10^.x)), 
                breaks=c(0.001,0.01,0.01,0.1,1),
                limits = c(0.001,1))+
  theme(aspect.ratio=4/5)

#DIN
env_long %>% colnames()

env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>%
  filter(Nitrate_Nitrogen != "N/A") %>% 
  mutate(NO3_N = Nitrate_Nitrogen %>% str_replace_all(c("<0.003" = "0.0015",
                                                        "<0.034" = "0.017",
                                                        "<0.004" = "0.002",
                                                        "<0.002" = "0.001")) %>% as.numeric()) %>% 
  filter(Nitrite_Nitrogen != "N/A") %>% 
  mutate(NO2_N = Nitrite_Nitrogen %>% str_replace_all(c("<0.002" = "0.001",
                                                        "<0.02" = "0.01")) %>% as.numeric()) %>% 
  filter(Ammonia_Nitrogen != "N/A") %>% 
  mutate(NH3_N = Ammonia_Nitrogen %>% str_replace_all(c("<0.005" = "0.0025")) %>% as.numeric()) %>%
  mutate(DIN = NO3_N + NO2_N +NH3_N) %>% 
  select(Station, Month_passed,DIN) %>% 
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  ggplot(aes(x = Month_passed, y = DIN))+
  facet_wrap(~Station, ncol = 5) +
  geom_point(alpha = 0.4, aes(color = WCZ), shape = 1)+
  geom_smooth(se=T, method = "lm",aes(color = WCZ))+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 45, hjust = 1, vjust= 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab(expression(DIN~(mg/L)))+
  xlab("")+
  # ggpubr::stat_cor(method = "pearson", size = 5.5, color = "black")+
  scale_x_continuous(breaks= c(47, 167,287,407),
                     labels = c("1990_01", "2000_01","2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  scale_y_log10(expand = c(0, 0),
                # using trans_format from the scales package, but one can also use expressions
                labels = trans_format('log10', math_format(10^.x)), 
                breaks=c(0.01,0.1,1,10),
                limits = c(0.01,10))+
  theme(aspect.ratio=4/5)

## slope of DIN, PO4 AND DIN

env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>%
  filter(Nitrate_Nitrogen != "N/A") %>% 
  mutate(NO3_N = Nitrate_Nitrogen %>% str_replace_all(c("<0.003" = "0.0015",
                                                        "<0.034" = "0.017",
                                                        "<0.004" = "0.002",
                                                        "<0.002" = "0.001")) %>% as.numeric()) %>% 
  filter(Nitrite_Nitrogen != "N/A") %>% 
  mutate(NO2_N = Nitrite_Nitrogen %>% str_replace_all(c("<0.002" = "0.001",
                                                        "<0.02" = "0.01")) %>% as.numeric()) %>% 
  filter(Ammonia_Nitrogen != "N/A") %>% 
  mutate(NH3_N = Ammonia_Nitrogen %>% str_replace_all(c("<0.005" = "0.0025")) %>% as.numeric()) %>%
  mutate(DIN = NO3_N + NO2_N +NH3_N) %>% 
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  mutate(PO4 = Orthophosphate_Phosphorus %>% 
           str_replace_all(c("<0.005" = "0.0025",
                             "<0.024" = "0.012",
                             "<0.01" = "0.005",
                             "<0.002" = "<0.001")) %>% as.numeric()) %>% 
  drop_na() %>% 
  mutate(NP_ratio = (DIN/14)/(PO4/31)) %>% 
  select(Station, Month_passed,NP_ratio) %>% 
  drop_na() %>% 
  group_by(Station) %>%
  do(tidy(lm(NP_ratio ~ Month_passed, data = .))) %>%
  filter(term == "Month_passed") %>% print(n = 25)

## NP ratio
env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>%
  filter(Nitrate_Nitrogen != "N/A") %>% 
  mutate(NO3_N = Nitrate_Nitrogen %>% str_replace_all(c("<0.003" = "0.0015",
                                                        "<0.034" = "0.017",
                                                        "<0.004" = "0.002",
                                                        "<0.002" = "0.001")) %>% as.numeric()) %>% 
  filter(Nitrite_Nitrogen != "N/A") %>% 
  mutate(NO2_N = Nitrite_Nitrogen %>% str_replace_all(c("<0.002" = "0.001",
                                                        "<0.02" = "0.01")) %>% as.numeric()) %>% 
  filter(Ammonia_Nitrogen != "N/A") %>% 
  mutate(NH3_N = Ammonia_Nitrogen %>% str_replace_all(c("<0.005" = "0.0025")) %>% as.numeric()) %>%
  mutate(DIN = NO3_N + NO2_N +NH3_N) %>% 
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  mutate(PO4 = Orthophosphate_Phosphorus %>% 
           str_replace_all(c("<0.005" = "0.0025",
                             "<0.024" = "0.012",
                             "<0.01" = "0.005",
                             "<0.002" = "<0.001")) %>% as.numeric()) %>% 
  drop_na() %>% 
  mutate(NP_ratio = (DIN/14)/(PO4/31)) %>% 
  select(Station, Month_passed,NP_ratio,WCZ) %>% 
  ggplot(aes(x = Month_passed, y = NP_ratio))+
  facet_wrap(~Station, ncol = 5) +
  geom_point(alpha = 0.4, aes(color = WCZ), shape = 1)+
  geom_smooth(se=T, method = "lm",aes(color = WCZ))+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 45, hjust = 1, vjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab("N:P ratio")+
  xlab("")+
  # ggpubr::stat_cor(method = "pearson", size = 5, color = "black")+
  scale_x_continuous(breaks= c(47, 167,287,407),
                     labels = c("1990_01", "2000_01","2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  scale_y_log10(expand = c(0, 0),
                # using trans_format from the scales package, but one can also use expressions
                labels = trans_format('log10', math_format(10^.x)), 
                breaks=c(1,10,100,1000),
                limits = c(1,1000))+
  geom_hline(yintercept=16, linetype="dashed", 
             color = "grey50", linewidth=1)+
  theme(aspect.ratio=4/5)
# ggpubr::stat_cor(method = "pearson", size = 5.5, color = "black")



#Temperature
env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>%
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  select(Station, Month_passed,Temperature,WCZ) %>% 
  drop_na() %>% 
  mutate(Temperature = Temperature %>% as.numeric()) %>% 
  ggplot(aes(x = Month_passed, y = Temperature))+
  facet_wrap(~Station, ncol = 5) +
  geom_point(alpha = 0.4, aes(color = WCZ), shape = 1)+
  geom_smooth(se=T, method = "lm",aes(color = WCZ))+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 45, hjust = 1, vjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab(expression("Temperature "(degree*C)))+
  xlab("")+
  xlab("Sampling time (from 1990 to 2020)")+
  ylim(c(10,34))+
  # ggpubr::stat_cor(method = "pearson", size = 5, color = "black")+
  scale_x_continuous(breaks= c(47, 167,287,407),
                     labels = c("1990_01", "2000_01","2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(aspect.ratio=4/5)
# ggpubr::stat_cor(method = "pearson", size = 6, color = "black")

## slope of temperature
env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>%
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  select(Station, Month_passed,Temperature) %>% 
  drop_na() %>% 
  mutate(Temperature = Temperature %>% as.numeric()) %>% 
  group_by(Station) %>%
  do(tidy(lm(Temperature ~ Month_passed, data = .))) %>%
  filter(term == "Month_passed") %>% print(n = 25)

#chla
env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>%
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  select(Station, Month_passed,Chlorophyll_a,WCZ) %>% 
  drop_na() %>% 
  mutate(Chlorophyll_a = Chlorophyll_a %>% as.numeric()) %>% 
  ggplot(aes(x = Month_passed, y = Chlorophyll_a))+
  facet_wrap(~Station, ncol = 5) +
  geom_point(alpha = 0.4, aes(color = WCZ), shape = 1)+
  geom_smooth(se=T, method = "gam",aes(color = WCZ))+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 90),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab(expression("Chlorophyll_a "(mu*g/L)))+
  xlab("")+
  # ggpubr::stat_cor(method = "pearson", size = 5, color = "black")+
  scale_x_continuous(breaks= c(47, 167,287,407),
                     labels = c("1990_01", "2000_01","2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  ggpubr::stat_cor(method = "pearson", size = 5, color = "black")+
  scale_y_log10(expand = c(0, 0),
                # using trans_format from the scales package, but one can also use expressions
                labels = trans_format('log10', math_format(10^.x)), 
                breaks=c(1,10,100),  ## only 8 values exceed 100, thus excluded
                limits = c(1,100))

## compare the PO4 between TM and other regions
env_long %>% as_tibble() %>%
  filter(Water_Depth == "Surface") %>% 
  mutate(Month_passed = (Year - 1986)*12 + (Month - 2)) %>% 
  mutate(PO4 = Orthophosphate_Phosphorus %>% 
           str_replace_all(c("<0.005" = "0.0025",
                             "<0.024" = "0.012",
                             "<0.01" = "0.005")) %>% as.numeric())  %>% 
  select(Station, Month_passed,PO4) %>% 
  filter(Station %in% c("DM3","DM5", "NM1","NM3","WM2","WM3","VM1","VM4","VM7",
                        "SM3","SM6","SM17","SM19","EM2","PM3","PM7","MM2","MM4","MM14",
                        "MM16","MM17","TM3","TM4","TM6","TM8")) %>% 
  drop_na() %>% 
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  drop_na() %>% 
  filter(Month_passed > 287) %>% 
  ggplot(aes(y = PO4 %>% log(10) , x = Station))+
  geom_boxplot(aes(color = WCZ),linewidth = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 45, hjust = 1, vjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab(expression(PO[4]^{"3-"}~(mg/L*","~log[10])))+
  xlab("")





# Community composition ---------------------------------------------------

library(scales)

df = read.csv("ZM EPD longterm data.csv")

# absolute abundance of 3 major groups
df %>% as_tibble()%>%
  # filter(Species != "small flagellates") %>%
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  filter(Station != "JM4") %>% 
  mutate(Sample = paste(Station, Year, Month, sep = "_")) %>% 
  select(Sample, Species.group, Density) %>% 
  group_by(Sample, Species.group) %>% 
  summarise(group_density = sum(Density)) %>% 
  ungroup() %>% 
  mutate(Station = Sample %>% str_split("_") %>% sapply('[',1)) %>% 
  mutate(Year = Sample %>% str_split("_") %>% sapply('[',2) %>% as.numeric()) %>%
  mutate(Month = Sample %>% str_split("_") %>% sapply('[',3) %>% as.numeric()) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  select(Station, Month_passed, Species.group, group_density) %>% 
  mutate(Group = Species.group) %>% 
  ggplot(aes(x = Month_passed, y = group_density %>% log(., 10), fill=Group))+
  geom_bar(stat="identity",position="stack", width=1,size=0.25)+
  facet_wrap(~Station, ncol = 5)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 35, hjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab("Phytoplankton abundance (log10)")+
  xlab("")+
  scale_x_continuous(breaks= c(1, 121,241),
                     labels = c("2000_01", "2010_01","2020_01"))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme(strip.text.x = element_text(size = 15))+
  scale_fill_manual(values = c("lightcoral", "dodgerblue", "grey66"))

# proportion of each group
## community composition at class level: mainly diatom and dino
df %>% as_tibble()%>%
  # filter(Species != "small flagellates") %>%
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  filter(Station != "JM4") %>% 
  mutate(Sample = paste(Station, Year, Month, sep = "_")) %>% 
  select(Sample, Species.group, Density) %>% 
  group_by(Sample, Species.group) %>% 
  summarise(group_density = sum(Density)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "Species.group", values_from = "group_density", values_fill = 0) %>% 
  column_to_rownames(., var = "Sample") %>% 
  as.matrix() %>% 
  prop.table(1) %>% 
  data.frame() %>%
  rownames_to_column(., var = "Sample") %>% 
  as_tibble() %>% 
  pivot_longer(!Sample, names_to = "Phy_Group", values_to = "RA") %>% 
  mutate(Station = Sample %>% str_split("_") %>% sapply('[',1)) %>% 
  mutate(Year = Sample %>% str_split("_") %>% sapply('[',2) %>% as.numeric()) %>%
  mutate(Month = Sample %>% str_split("_") %>% sapply('[',3) %>% as.numeric()) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  ggplot(aes(x = Month_passed, y = RA, fill=Phy_Group))+
  geom_bar(stat="identity",position="stack", width=1,size=0.25)+
  facet_wrap(~Station, ncol = 5)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 35, hjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab("Phytoplankton relative abundance")+
  xlab("")+
  scale_x_continuous(breaks= c(1, 121,241),
                     labels = c("2000_01", "2010_01","2020_01"))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme(strip.text.x = element_text(size = 15))+
  scale_fill_manual(values = c("lightcoral", "dodgerblue", "grey66"))



# Diatom community  
# find the most abundant 10 diatom species
df %>% as_tibble()%>%
  filter(Species.group == "Diatom") %>% 
  mutate(Species =Species %>% str_replace("Pseudo-nitzschia delicatissima","Pseudo-nitzschia spp.")) %>% 
  mutate(Species =Species %>% str_replace("Pseudo-nitzschia pungens","Pseudo-nitzschia spp.")) %>% 
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  filter(Station != "JM4") %>% 
  mutate(Groups = `Species.group`) %>% 
  mutate(Year = Year %>% as.numeric) %>%
  mutate(Month = Month %>% as.numeric) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  select(Species, Density) %>% 
  group_by(Species) %>% 
  summarise(mean_ab = sum(Density)) %>% ## better than using mean()
  arrange(desc(mean_ab)) %>% head(10) %>% pull(Species) -> diatom_t10


# plot diatom species composition
df %>% as_tibble()%>%
  filter(Species.group == "Diatom") %>% 
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  
  filter(Station != "JM4") %>% 
  mutate(Groups = `Species.group`) %>% 
  mutate(Year = Year %>% as.numeric) %>%
  mutate(Month = Month %>% as.numeric) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) ->df_dia


df_dia$Species[!(df_dia$Species %in% diatom_t10)] <- "Others"

df_dia %>% 
  mutate(Sample = paste(Station, Year, Month, sep = "_")) %>% 
  select(Sample, Species, Density) %>% 
  group_by(Sample, Species) %>% 
  summarise(sp_density = sum(Density)) %>% 
  ungroup() %>% 
  mutate(Station = Sample %>% str_split("_") %>% sapply('[',1)) %>% 
  mutate(Year = Sample %>% str_split("_") %>% sapply('[',2) %>% as.numeric()) %>%
  mutate(Month = Sample %>% str_split("_") %>% sapply('[',3) %>% as.numeric()) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  select(Station, Month_passed, Species, sp_density) %>% 
  ggplot(aes(x = Month_passed, y = sp_density, fill = Species))+
  geom_bar(stat="identity",position="fill", width=1,size=0.5)+
  # geom_bar(stat="identity",position = "stack")+
  facet_wrap(~Station, ncol = 5)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 35, hjust = 1),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15))+
  ylab("Diatom relative abundance")+
  xlab("")+
  scale_x_continuous(breaks= c(1, 121,241),
                     labels = c("2000_01", "2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  scale_fill_brewer(palette = "Paired")

# annual change of certain diatom species 
df %>% as_tibble()%>%
  filter(Species.group == "Diatom") %>% 
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  
  filter(Station != "JM4") %>% 
  mutate(Groups = `Species.group`) %>% 
  mutate(Year = Year %>% as.numeric) %>%
  mutate(Month = Month %>% as.numeric) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  filter(Species == "Skeletonema costatum") %>% 
  select(Station,Density, Month_passed) %>% 
  group_by(Station, Month_passed) %>% 
  summarise(total_tha = sum(Density)) %>% 
  ggplot(aes(x = Month_passed, y = log(total_tha+1)))+
  geom_point()+
  facet_wrap(~Station, ncol = 5)+
  geom_smooth(se=T, method = "lm")+
  ggpubr::stat_cor(method = "spearman", size = 4, color = "black")


# by WCZ

df %>% as_tibble()%>%
  filter(Species != "small flagellates") %>%
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  
  filter(Station != "JM4") %>% 
  mutate(Groups = `Species.group`) %>% 
  mutate(Year = Year %>% as.numeric) %>%
  mutate(Month = Month %>% as.numeric) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  filter(grepl("^Scrippsiella", Species)) %>%
  select(Waterzone, Density, Month_passed) %>% 
  group_by(Waterzone, Month_passed) %>% 
  summarise(ab_wcz = sum(Density)) %>%
  ggplot(aes(x = Month_passed, y = log(ab_wcz+1,10)))+
  geom_point(aes(color=Waterzone))+
  facet_wrap(~Waterzone)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 35, hjust = 1),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15))+
  ylab("Scrippsiella (log10)")+
  xlab("")+
  scale_x_continuous(breaks= c(1, 121,241),
                     labels = c("2000_01", "2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  geom_smooth(aes(color = Waterzone),method = "lm", se=T)+
  ggpubr::stat_cor(method = "pearson", size = 6, color = "black")



# Dinoflagellate community  
# find the most abundant 10 dino species
df %>% as_tibble()%>% 
  filter(Species.group == "Dinoflagellate") %>% 
  filter(Species != "small flagellates") %>%
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  filter(Station != "JM4") %>% 
  mutate(Groups = `Species.group`) %>% 
  mutate(Year = Year %>% as.numeric) %>%
  mutate(Month = Month %>% as.numeric) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  select(Species, Density) %>% 
  group_by(Species) %>% 
  summarise(mean_ab = sum(Density)) %>% 
  arrange(desc(mean_ab)) %>% head(10) %>% pull(Species) -> dino_t10


# plot Dinoflagellate species composition
df %>% as_tibble()%>%
  filter(Species.group == "Dinoflagellate") %>% 
  filter(Species != "small flagellates") %>%
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  
  filter(Station != "JM4") %>% 
  mutate(Groups = `Species.group`) %>% 
  mutate(Year = Year %>% as.numeric) %>%
  mutate(Month = Month %>% as.numeric) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) ->df_dino


df_dino$Species[!(df_dino$Species %in% dino_t10)] <- "Others"

df_dino %>% 
  mutate(Sample = paste(Station, Year, Month, sep = "_")) %>% 
  select(Sample, Species, Density) %>% 
  group_by(Sample, Species) %>% 
  summarise(sp_density = sum(Density)) %>% 
  ungroup() %>% 
  mutate(Station = Sample %>% str_split("_") %>% sapply('[',1)) %>% 
  mutate(Year = Sample %>% str_split("_") %>% sapply('[',2) %>% as.numeric()) %>%
  mutate(Month = Sample %>% str_split("_") %>% sapply('[',3) %>% as.numeric()) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  select(Station, Month_passed, Species, sp_density) %>% 
  ggplot(aes(x = Month_passed, y = sp_density, fill = Species))+
  geom_bar(stat="identity",position="fill", width=1,size=0.5)+
  facet_wrap(~Station, ncol = 5)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 35, hjust = 1),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15))+
  ylab("Dinoflagellate relative abundance")+
  xlab("")+
  scale_x_continuous(breaks= c(1, 121,241),
                     labels = c("2000_01", "2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))

# cal proportion
df_dino %>% 
  mutate(Sample = paste(Station, Year, Month, sep = "_")) %>% 
  select(Sample, Species, Density) %>% 
  group_by(Sample, Species) %>% 
  summarise(sp_density = sum(Density)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "Species", values_from = "sp_density", values_fill = 0) %>% 
  column_to_rownames(., var = "Sample") %>% 
  as.matrix() %>% 
  prop.table(1) %>% 
  data.frame(check.names = F) %>%
  rownames_to_column(., var = "Sample") %>% 
  as_tibble() %>% 
  pivot_longer(!Sample, names_to = "Dino_spp", values_to = "RA") %>% 
  mutate(Station = Sample %>% str_split("_") %>% sapply('[',1)) %>% 
  mutate(Year = Sample %>% str_split("_") %>% sapply('[',2) %>% as.numeric()) %>%
  mutate(Month = Sample %>% str_split("_") %>% sapply('[',3) %>% as.numeric()) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1)))  %>% 
  filter(Dino_spp == "Gymnodinium spp.") %>% 
  filter(Station %>% str_detect("DM")) %>% 
  # pull(RA) %>% mean
  lm(RA~Month_passed, data = .) %>% summary()


# trend of certain dino species
df %>% as_tibble()%>%
  # filter(Species.group == "Diatom") %>% 
  # filter(Species != "small flagellates") %>%
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^01$"="1",   ## some use"1,2,3" while some use "01.02.03")
                                             "^02$"="2",
                                             "^03$"="3",
                                             "^04$"="4",
                                             "^05$"="5",
                                             "^06$"="6",
                                             "^07$"="7",
                                             "^08$"="8",
                                             "^09$"="9"))) %>%
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern Buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  filter(Station != "JM4") %>% 
  mutate(Groups = `Species.group`) %>% 
  mutate(Year = Year %>% as.numeric) %>%
  mutate(Month = Month %>% as.numeric) %>%
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  filter(Species == "Gymnodinium spp.") %>% 
  # filter(Waterzone == "Deep Bay") %>% 
  ggplot(aes(x = Month_passed, y = log(Density+1)))+
  geom_point(aes(color = WCZ),size = 1,shape = 21)+
  facet_wrap(~Station)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 35, hjust = 1),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15))+
  ylab("Gymnodinium spp. abundance")+
  xlab("")+
  scale_x_continuous(breaks= c(1, 121,241),
                     labels = c("2000_01", "2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  geom_smooth(aes(color = WCZ),method = "lm", se=T)+
  ggpubr::stat_cor(method = "spearman",size = 6)+
  theme(strip.text.x = element_text(size = 15))


## relative abundance:Scrippsiella spp.  Gymnodinium spp. ,,,,,,
df_dino %>% 
  mutate(Sample = paste(Station, Year, Month, sep = "_")) %>% 
  select(Sample, Species, Density) %>% 
  group_by(Sample, Species) %>% 
  summarise(sp_density = sum(Density)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "Species", values_from = "sp_density",values_fill = 0) %>% 
  mutate(ra = `Prorocentrum micans` /sum(`Prorocentrum micans`)) %>% ### choose a certain species
  select(Sample, ra) %>% 
  mutate(Station = Sample %>% str_split("_") %>% sapply('[',1)) %>% 
  mutate(Year = Sample %>% str_split("_") %>% sapply('[',2) %>% as.numeric()) %>% 
  mutate(Month = Sample %>% str_split("_") %>% sapply('[',3) %>% as.numeric()) %>% 
  mutate(Month_passed = ((Year - 2000)*12 + (Month-1))) %>% 
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern Buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour"))) %>% 
  ggplot(aes(x = Month_passed, y = ra ))+
  geom_point(aes(color = WCZ),size = 1,shape = 21)+
  facet_wrap(~WCZ, ncol =3)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 35, hjust = 1),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15))+
  ylab("Prorocentrum micans relative abundance")+
  xlab("")+
  scale_x_continuous(breaks= c(1, 121,241),
                     labels = c("2000_01", "2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  geom_smooth(aes(color = WCZ),method = "lm", se=T)+
  ggpubr::stat_cor(method = "spearman",size = 5)+
  theme(strip.text.x = element_text(size = 15))+
  scale_y_log10(expand = c(0, 0),
                # using trans_format from the scales package, but one can also use expressions
                labels = trans_format('log10', math_format(10^.x)))




# Beta diversity ----------------------------------------------------------
df = read.csv("ZM EPD longterm data.csv")
df %>% head(30)

sp_table<-
  df %>% as_tibble() %>% 
  filter(Species != "Mesodinium rubrum") %>%
  mutate(Species =Species %>% str_replace("Pseudo-nitzschia delicatissima","Pseudo-nitzschia spp.")) %>% 
  mutate(Species =Species %>% str_replace("Pseudo-nitzschia pungens","Pseudo-nitzschia spp.")) %>% 
  filter(Station != "Station")  %>% 
  mutate(Year = Year %>% str_remove(" ")) %>%              #names contain space
  mutate(Month = Month %>% str_remove(" ")) %>% 
  mutate(Month = Month %>% str_replace_all(c("^1$"="01",   ## some use"1,2,3" while some use "01.02.03")
                                             "^2$"="02",
                                             "^3$"="03",
                                             "^4$"="04",
                                             "^5$"="05",
                                             "^6$"="06",
                                             "^7$"="07",
                                             "^8$"="08",
                                             "^9$"="09"))) %>%
  mutate(Time = paste0(Year,Month) %>% as.numeric()) %>% 
  filter(Time != "NA") %>% 
  mutate(Sample = paste(Station, Year, Month, sep = "_")) %>% 
  select(Sample, Species, Density) %>% 
  group_by(Sample,Species) %>% 
  summarise(sum_Density = sum(Density)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = sum_Density) %>% 
  mutate_all(replace_na, 0) %>% 
  mutate(across(2:169, as.numeric)) %>% 
  column_to_rownames(var = "Sample")

sp_table %>% dim
sp_table = sp_table[,which(colSums(sp_table)>0)]
sp_table = sp_table[which(rowSums(sp_table)>0),]
sp_table %>% dim

gp<-
  data.frame(Sample = sp_table %>% rownames()) %>% 
  as_tibble() %>% 
  mutate(Year =  Sample %>% str_split("_") %>% sapply('[', 2) %>% as.numeric()) %>% 
  mutate(Month =  Sample %>% str_split("_") %>% sapply('[', 3) %>% as.numeric()) %>% 
  mutate(Month = Sample %>% str_split("_") %>% sapply('[', 3) %>% str_replace_all(c("^01$" = "Jan",
                                                                                    "^02$" = "Feb",
                                                                                    "^03$" = "Mar",
                                                                                    "^04$" = "Apr",
                                                                                    "^05$" = "May",
                                                                                    "^06$" = "Jun",
                                                                                    "^07$" = "Jul",
                                                                                    "^08$" = "Aug",
                                                                                    "^09$" = "Sep",
                                                                                    "^10$" = "Oct",
                                                                                    "^11$" = "Nov",
                                                                                    "^12$" = "Dec"))) %>% 
  mutate(Season = Month %>% str_replace_all(c("Dec" = "Winter",
                                              "Jan" = "Winter",
                                              "Feb" = "Winter",
                                              "Mar" = "Spring",
                                              "Apr" = "Spring",
                                              "May" = "Spring",
                                              "Jun" = "Summer",
                                              "Jul" = "Summer",
                                              "Aug" = "Summer",
                                              "Sep" = "Fall",
                                              "Oct" = "Fall",
                                              "Nov" = "Fall"))) %>% 
  mutate(Station =  Sample %>% str_split("_") %>% sapply('[', 1)) %>% 
  mutate(WCZ = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour", "TM4" = "Tolo Harbour","TM6" = "Tolo Harbour","TM8" = "Tolo Harbour",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern Buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour")))
library(vegan)
d = vegdist(sqrt(sp_table))
nmds = monoMDS(d)
NMDS = data.frame(nMDS1 = nmds$points[,1], nMDS2 = nmds$points[,2])
NMDS %>% head
NMDS$Year = gp$Year
NMDS$Station = gp$Station
NMDS$WCZ = gp$WCZ %>% as.factor()

NMDS$Season = factor(gp$Season, levels = c("Spring", "Summer", "Fall", "Winter"))


NMDS %>% 
  rownames_to_column(., var = "Sample") %>% 
  as_tibble() %>%
  filter(Sample %>% str_detect("JM",negate = T)) %>% 
  ggplot(aes(nMDS1,nMDS2))+
  theme_bw()+
  geom_point(aes(colour=WCZ),size=1.5)+
  theme(panel.grid=element_blank())+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15),
        panel.border = element_rect(fill=NA,color="black", linewidth=2, linetype="solid")) -> md1
md1

NMDS %>% 
  rownames_to_column(., var = "Sample") %>% 
  as_tibble() %>%
  filter(Sample %>% str_detect("JM",negate = T)) %>% 
  ggplot(aes(nMDS1,nMDS2))+
  theme_bw()+
  geom_point(aes(colour=Station),size=1.5)+
  theme(panel.grid=element_blank())+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15),
        panel.border = element_rect(fill=NA,color="black", linewidth=2, linetype="solid")) -> md2
md2

NMDS %>% 
  rownames_to_column(., var = "Sample") %>% 
  as_tibble() %>%
  filter(Sample %>% str_detect("JM",negate = T)) %>% 
  ggplot(aes(nMDS1,nMDS2))+
  theme_bw()+
  geom_point(aes(colour=Season),size=1.5)+
  theme(panel.grid=element_blank())+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15),
        panel.border = element_rect(fill=NA,color="black", linewidth=2, linetype="solid")) -> md3
md3

NMDS %>% 
  rownames_to_column(., var = "Sample") %>% 
  as_tibble() %>%
  filter(Sample %>% str_detect("JM",negate = T)) %>% 
  ggplot(aes(nMDS1,nMDS2))+
  theme_bw()+
  geom_point(aes(colour=Year),size=1.5)+
  theme(panel.grid=element_blank())+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15),
        panel.border = element_rect(fill=NA,color="black", linewidth=2, linetype="solid")) -> md4
md4

ggarrange(md1,md3,md2,md4,ncol = 2,nrow = 2,align = "v")
anosim(d, gp$Year)
anosim(d, gp$Season)
anosim(d, gp$Station)
anosim(d, gp$WCZ)


NMDS %>% as_tibble()
NMDS = NMDS %>% 
  mutate(WCZ1 = Station %>% 
           str_replace_all(c("TM3" = "Tolo Harbour-Inner", "TM4" = "Tolo Harbour-Inner","TM6" = "Tolo Harbour-Outer","TM8" = "Tolo Harbour-Outer",
                             "SM3" = "Southern", "SM6" = "Southern", "SM17" = "Southern", "SM19" = "Southern", 
                             "PM3" = "Port Shelter","PM7" = "Port Shelter",
                             "DM3" = "Deep Bay", "DM5" = "Deep Bay",
                             "MM2" = "Mirs Bay",  "MM4" = "Mirs Bay", "MM14" = "Mirs Bay", "MM16" = "Mirs Bay", "MM17" = "Mirs Bay",
                             "NM1" = "North Western","NM3" = "North Western",
                             "WM2" = "Western Buffer", "WM3" = "Western Buffer",
                             "EM2" = "Eastern Buffer",
                             "VM1" = "Victoria Harbour","VM4" = "Victoria Harbour","VM7" = "Victoria Harbour")))

NMDS %>% as_tibble() %>% 
  filter(WCZ != "JM4") %>% 
  ggplot(aes(x = nMDS1, y = nMDS2))+
  geom_point(aes(color = Year), size = 2)+
  facet_wrap(~WCZ1, ncol = 4)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=15),
        axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15),
        panel.border = element_rect(fill=NA,color="black", linewidth=2, linetype="solid"))+
  theme(strip.text.x = element_text(size = 15))+
  guides(color = guide_legend(override.aes = list(size = 3))) +
  ggtitle("Phytoplankton community similarity seasonal pattern")


# Anosim
gp %>% 
  filter(WCZ == "Port Shelter") -> gp1

gp %>% 
  filter(WCZ == "Port Shelter") %>% 
  select(!c(Year, Month, WCZ, Station, Season)) %>% 
  column_to_rownames(., var = "Sample") %>% data.frame() -> com1

com1 = com1[,which(colSums(com1)>0)]

anosim(vegdist(com1), gp1$Season)

# Size trend for each species ---------------------------------------------

## add group information (diatom or dino or others) to each species
al = read.csv("ZM EPD longterm data.csv")
#check 
al<-
  al %>% as_tibble() %>% 
  select(Species.group,Species) %>% 
  dplyr::rename(Phyto_group = Species.group) %>% 
  unique()


al1 = read.csv("EPD-20Y-identified-species-HAB.csv")

al1 %>% as_tibble()

al = al %>% left_join(al1[,-1], by = "Species")

# read species size table
df = read.csv("epd 20y size nbss.csv")

df<-
  df %>% as_tibble() %>% 
  left_join(al,by = "Species") %>% 
  filter(Species != "Mesodinium rubrum") # heterotrophic zooplankton

# select top 10 most abudnant diatom species
df %>% as_tibble() %>% 
  select(Species, Density,Phyto_group) %>%
  group_by(Species,Phyto_group) %>% 
  dplyr::summarize(Species_density_sum = sum(Density)) %>% 
  arrange(desc(Species_density_sum)) %>% 
  filter(Species  %>% str_detect("spp.", negate = T)) %>% 
  filter(Phyto_group == "Diatom") %>% .[1:10,1] %>% pull(Species) -> dia10

# select top 10 most abudnant dino species
df %>% as_tibble() %>% 
  filter(Species %>% str_detect("Heterosigma|Takayama|vestifici", negate = T)) %>%  ### few data
  select(Species, Density,Phyto_group) %>% 
  group_by(Species,Phyto_group) %>% 
  summarize(Species_density_sum = sum(Density)) %>% 
  arrange(desc(Species_density_sum)) %>% 
  filter(Species  %>% str_detect("spp.", negate = T)) %>% 
  filter(Phyto_group == "Dinoflagellate") %>% .[1:10,1] %>% pull(Species) -> dino10

df %>% as_tibble() %>% 
  filter(Species %>% str_detect("Heterosigma|Takayama|vestifici", negate = T)) %>% ### few data
  select(Species, Density,Phyto_group) %>% 
  group_by(Species,Phyto_group) %>% 
  summarize(Species_density_sum = sum(Density)) %>% 
  arrange(desc(Species_density_sum)) %>% 
  filter(Species  %>% str_detect("spp.", negate = T)) %>% 
  filter(Phyto_group == "Other") %>% .[1:5,1] %>% pull(Species) -> other5   #. the 5 the one is very low abundance


spp25 = c(dia10, dino10, other5)

## temporal trend of species biov (all stations together)
spp25_new = spp25 %>%
  str_replace("Pseudo-nitzschia delicatissima", "P. delicatissima") %>% ## shorten name for virtulizaiton
  str_replace("Pseudo-nitzschia pungens", "P. pungens")

df %>% as_tibble() %>% 
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs())  %>%
  filter(Species %in% spp25) %>%
  # filter(Species %>% str_detect("Coscinodiscus spp.")) %>% 
  mutate(Species = Species %>% 
           str_replace("Pseudo-nitzschia delicatissima", "P. delicatissima") %>% 
           str_replace("Pseudo-nitzschia pungens", "P. pungens")) %>% 
  filter(Average.biovolume>0) %>% 
  ggplot(aes(x = Lag_Months, y =  Average.biovolume))+
  geom_point(aes(color = Phyto_group), size = 1,shape = 21, alpha = 0.5)+
  facet_wrap(~Species %>% factor(levels = spp25_new), ncol = 5)+
  theme_classic()+ 
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 35, hjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  labs(y = expression(Species~biovolume~(mu*m^3)))+
  xlab("")+
  geom_smooth(se=T, method = "lm",color = "black",linewidth = 1)+
  # geom_smooth(se=T, method = "lm",aes(color = Phyto_group),linewidth = 2)+
  # ggpubr::stat_cor(method = "spearman",size = 4.8, label.y =1.1)+
  # ggpubr::stat_cor(method = "spearman", size = 5,aes(label = ..r.label..))+
  # ggpubr::stat_cor(method = "spearman", size = 5,aes(label = ..p.label..), label.y = 4.75)+
  scale_x_continuous(breaks= c(0, 120,240),
                     labels = c("2001_01", "2010_01","2020_01"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  theme(strip.text.x = element_text(size = 11,face = "bold.italic"))+
  # theme(legend.position = "none")+
  scale_color_manual(values = c("coral1", "dodgerblue", "grey66"))+
  ## size of legend label
  guides(color = guide_legend(override.aes = list(size = 4) ) )

## calculate slope
library(broom)
df %>% as_tibble() %>% 
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs())  %>%
  filter(Species %in% spp25) %>% 
  filter(Average.biovolume>0) %>% 
  mutate(Species = Species %>% 
           str_replace("Pseudo-nitzschia delicatissima", "P. delicatissima") %>% 
           str_replace("Pseudo-nitzschia pungens", "P. pungens")) %>% 
  select(Species, Average.biovolume,Lag_Months) %>% 
  group_by(Species) %>%
  do(tidy(lm(Average.biovolume ~ Lag_Months, data = .))) %>%
  filter(term == "Lag_Months") %>%
  select(Species, estimate,p.value) %>% print(n = 25)


# Community mean size trend -----------------------------------------------

# Station
df %>% as_tibble() %>% 
  mutate(WCZ = Waterzone %>% str_replace("Tolo Harbour and Channel", "Tolo Harbour")) %>% 
  # filter(Species %in% c(spp25)) %>% 
  # filter(Species != "small flagellates") %>% 
  filter(Station != "JM4") %>% ## no data
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs())  %>%  
  select(Station, Year, Month, WCZ, Lag_Months,Density, BIO.volume) %>% 
  group_by(Station, Year, Month, WCZ,Lag_Months) %>% 
  summarise(BioV_mean = sum(BIO.volume)/sum(Density)) %>%
  ggplot(aes(x = Lag_Months, y = log(BioV_mean,10)))+
  geom_point(aes(color = WCZ), size = 1,shape = 21, alpha = 0.4)+
  facet_wrap(~Station, ncol = 5)+
  theme_classic()+ 
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 35, hjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  labs(y = expression(Community~mean~biovolum~(mu*m^3)))+
  xlab("")+
  geom_smooth(se=F, method = "lm", aes(color = WCZ),size = 1.5)+
  scale_x_continuous(breaks= c(0, 120,240),
                     labels = c("2001_01", "2010_01","2020_01"))+
  # scale_y_continuous(trans = log10_trans(),
  #                    breaks = trans_breaks("log10", function(x) 10^x),
  #                    labels = trans_format("log10", math_format(10^.x)))+
  # ggpubr::stat_cor(method = "spearman", size = 5,aes(label = ..r.label..))+
  # ggpubr::stat_cor(method = "spearman", size = 5,aes(label = ..p.label..), label.y = 4.3)+### not normal distribution, by shapiro-walk test
  theme(strip.text.x = element_text(size = 15))+
  theme(aspect.ratio=4/4)

# slope of community mean size trend
df %>% as_tibble() %>% 
  mutate(WCZ = Waterzone %>% str_replace("Tolo Harbour and Channel", "Tolo Harbour")) %>% 
  # filter(Species %in% c(spp25)) %>% 
  # filter(Species != "small flagellates") %>% 
  filter(Station != "JM4") %>% ## no data
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs())  %>%  
  select(Station, Year, Month, WCZ, Lag_Months,Density, BIO.volume) %>% 
  group_by(Station, Year, Month, WCZ,Lag_Months) %>% 
  summarise(BioV_mean = sum(BIO.volume)/sum(Density)) %>% 
  select(Station,Lag_Months,BioV_mean) %>% 
  group_by(Station) %>% 
  do(tidy(lm(BioV_mean %>% log(10) ~ Lag_Months, data = .))) %>%
  filter(term == "Lag_Months") %>%
  select(Station,estimate,p.value) %>% print(n=25)

#WCZ
df %>% as_tibble() %>% 
  mutate(Waterzone = Waterzone %>% str_replace("Tolo Harbour and Channel","Tolo Harbour")) %>% 
  # filter(Species != "small flagellates") %>% 
  filter(Station != "JM4") %>% ## no data
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs())  %>%  
  select(Station, Year, Month, Waterzone, Lag_Months,Density, BIO.volume) %>% 
  group_by(Year, Month, Waterzone,Lag_Months) %>% 
  summarise(BioV_mean = sum(BIO.volume)/sum(Density)) %>% 
  ungroup %>% 
  select(Waterzone, Lag_Months, BioV_mean) %>% 
  group_by(Waterzone, Lag_Months) %>% 
  summarise(BioV_mean_wcz = mean(BioV_mean)) %>% 
  ggplot(aes(x = Lag_Months, y = BioV_mean_wcz %>% log(10)))+
  geom_point(aes(color = Waterzone), size = 1,shape = 21)+
  facet_wrap(~Waterzone, ncol = 3)+
  theme_classic()+ 
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=15, angle = 35, hjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  # labs(y = expression(WCZ~community~mean~biovolume~(mu*m^3)~(25~species)))+
  ylab("Community mean biovolume (WCZ level, log10)")+
  xlab("")+
  geom_smooth(se=F, method = "lm", aes(color = Waterzone),size = 2)+
  scale_x_continuous(breaks= c(0, 120,240),
                     labels = c("2001_01", "2010_01","2020_01"))+
  # ggpubr::stat_cor(method = "spearman",size = 6)+
  theme(strip.text.x = element_text(size = 20))+
  theme(aspect.ratio=4/4)

## calcu slope
df %>% as_tibble() %>% 
  mutate(Waterzone = Waterzone %>% str_replace("Tolo Harbour and Channel","Tolo Harbour")) %>% 
  # filter(Species != "small flagellates") %>% 
  filter(Station != "JM4") %>% ## no data
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs())  %>%  
  select(Station, Year, Month, Waterzone, Lag_Months,Density, BIO.volume) %>% 
  group_by(Year, Month, Waterzone,Lag_Months) %>% 
  summarise(BioV_mean = sum(BIO.volume)/sum(Density)) %>% 
  ungroup %>% 
  select(Waterzone, Lag_Months, BioV_mean) %>% 
  group_by(Waterzone, Lag_Months) %>% 
  summarise(BioV_mean_wcz = mean(BioV_mean)) %>% 
  ungroup() %>% 
  group_by(Waterzone) %>% 
  do(tidy(lm(BioV_mean_wcz %>% log(10) ~ Lag_Months, data = .))) %>%
  filter(term == "Lag_Months") %>%
  select(Waterzone,estimate,p.value) %>% print(n=9)

# Small and large species (definition, size distribution and temporal trends) ------------------------------------------------------

### unify
df %>% 
  mutate(Species = Species %>% 
           str_replace("Pseudo-nitzschia delicatissima", "Pseudo-nitzschia spp.") %>% 
           str_replace("Pseudo-nitzschia pungens", "Pseudo-nitzschia spp.")) -> df

df$Species %>% unique() %>% length()#167

# define small species
df %>% 
  select(Species, Average.biovolume) %>% 
  group_by(Species) %>% 
  summarise(av_biov = mean(Average.biovolume)) %>% 
  filter(av_biov>0) %>% 
  arrange(av_biov) %>% .[1:41,1] -> smallsp #### lowest 1/4

# define large species
df %>% 
  # filter(Species != "small flagellates") %>% 
  select(Species, Average.biovolume) %>% 
  group_by(Species) %>% 
  summarise(av_biov = mean(Average.biovolume)) %>% 
  filter(av_biov>0) %>% 
  arrange(av_biov %>% desc()) %>% .[1:41,1] -> largesp #### largest 1/4

smallsp$Species  

# calcution of %small
df %>% 
  select(-c(Density, BIO.volume,  Phyto_group)) %>% 
  group_by(Station,Year,Month,Waterzone,Species) %>% 
  summarise(Average.biovolume1 = mean(Average.biovolume)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "Species", values_from = "Average.biovolume1",values_fill = 0) %>% 
  mutate(Sample = paste(Station, Year, Month, Waterzone, sep = "_")) %>% 
  select(-c(Station, Year, Month, Waterzone)) %>% 
  column_to_rownames(., var = "Sample") %>% as.matrix() %>% 
  prop.table(1) %>% data.frame(check.names = F) %>% 
  rownames_to_column(., var = "Sample") %>% as_tibble() %>% 
  pivot_longer(!Sample, names_to = "Species", values_to = "Relative abundance") %>% 
  mutate(Station = Sample %>% str_split("_") %>% sapply('[', 1)) %>% 
  mutate(Year = Sample %>% str_split("_") %>% sapply('[', 2) %>% as.numeric()) %>% 
  mutate(Month = Sample %>% str_split("_") %>% sapply('[', 3) %>% as.numeric()) %>% 
  mutate(WCZ = Sample %>% str_split("_") %>% sapply('[', 4)) %>% 
  mutate(Size_class = ifelse(Species %in% c(smallsp$Species), "small", "large")) -> dff 

# temporal trend of %small (station level)
dff %>% 
  select(!Species) %>% 
  filter(Station != "JM4") %>% 
  group_by(Sample, Station, Year, Month, WCZ, Size_class) %>% 
  summarise(sum_ra = sum(`Relative abundance`)) %>% 
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs()) %>% 
  filter(Size_class == "small") %>% 
  # filter(sum_ra>0) %>% 
  # filter(sum_ra<1) %>% 
  ggplot(aes(x = Lag_Months, y = sum_ra , group = Station))+
  geom_point(aes(color = WCZ), size = 1.5,shape = 21, alpha = 0.4)+
  facet_wrap(~Station, ncol = 5)+
  theme_classic()+ 
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 35, hjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab("Proportion of small species (1/4)")+
  xlab("")+
  geom_smooth(se=F, method = "lm", aes(color = WCZ),size = 1)+
  # geom_smooth(method = "lm", se = FALSE, aes(linetype = Sig1, color = Phyto_group)) +
  scale_x_continuous(breaks= c(0, 120,240),
                     labels = c("2001_01", "2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  # ggpubr::stat_cor(method = "spearman", size = 5,aes(label = ..r.label..),label.y = 0.9)+
  # ggpubr::stat_cor(method = "spearman", size = 5,aes(label = ..p.label..), label.y = 0.7)+
  theme(aspect.ratio=4/4)


## temporal trend of %small at WCZ level
df %>%
  filter(Waterzone != "Junk Bay") %>%
  select(-c(Density, BIO.volume,  Phyto_group, Station)) %>%
  group_by(Year,Month,Waterzone,Species) %>%
  summarise(Average.biovolume1 = mean(Average.biovolume)) %>%
  ungroup() %>%
  pivot_wider(names_from = "Species", values_from = "Average.biovolume1",values_fill = 0) %>%
  mutate(Sample = paste(Year, Month, Waterzone, sep = "_")) %>%
  select(-c(Year, Month, Waterzone)) %>%
  column_to_rownames(., var = "Sample") %>% as.matrix() %>%
  prop.table(1) %>% data.frame(check.names = F) %>%
  rownames_to_column(., var = "Sample") %>% as_tibble() %>%
  pivot_longer(!Sample, names_to = "Species", values_to = "Relative abundance") %>%
  mutate(Year = Sample %>% str_split("_") %>% sapply('[', 1) %>% as.numeric()) %>%
  mutate(Month = Sample %>% str_split("_") %>% sapply('[', 2) %>% as.numeric()) %>%
  mutate(WCZ = Sample %>% str_split("_") %>% sapply('[', 3)) %>%
  mutate(Size_class = ifelse(Species %in% c(smallsp$Species), "small", "large")) %>%
  select(!Species) %>%
  mutate(WCZ=WCZ %>% str_remove("and Channel")) %>%
  group_by(Year, Month, WCZ, Size_class) %>%
  summarise(sum_ra = sum(`Relative abundance`)) %>%
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs()) %>%
  filter(Size_class == "small") -> dff1

dff1 %>%
  ggplot(aes(x = Lag_Months, y = sum_ra %>% log(10), group = WCZ))+
  geom_point(aes(color = WCZ), size = 1.5,shape = 21, alpha = 0.4)+
  facet_wrap(~WCZ, ncol =3)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(axis.text.x = element_text(color="black", size=12, angle = 35, hjust = 1),
        axis.title=element_text(size=20))+
  theme(legend.title=element_text(size=20), legend.text=element_text(size=20),
        axis.title=element_text(size=20))+
  ylab("Proportion of small species (log10)")+
  xlab("")+
  geom_smooth(se=F, method = "lm", aes(color = WCZ),size = 1)+
  # geom_smooth(method = "lm", se = FALSE, aes(linetype = Sig1, color = Phyto_group)) +
  scale_x_continuous(breaks= c(0, 120,240),
                     labels = c("2001_01", "2010_01","2020_01"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(aspect.ratio=4/4)

# slope and p value
# wcz
dff1 %>% 
  group_by(Year, Month, WCZ, Size_class,Lag_Months) %>% 
  summarise(sum_ra1 = sum(sum_ra)) %>% 
  filter(Size_class == "small") %>% 
  ungroup() %>% 
  select(WCZ, Lag_Months, sum_ra1) %>% 
  group_by(WCZ) %>% 
  do(tidy(lm(sum_ra1 %>% log(10) ~ Lag_Months, data = .))) %>%
  filter(term == "Lag_Months") %>%
  select(WCZ,estimate,p.value) %>% print(n=9)
# station
dff %>% 
  select(!Species) %>% 
  group_by(Sample, Station, Year, Month, WCZ, Size_class) %>% 
  summarise(sum_ra = sum(`Relative abundance`)) %>% 
  mutate(Lag_Months = ((Year-2000)*12+(Month-1)) %>% abs()) %>% 
  filter(Size_class == "small") %>% 
  filter(sum_ra>0) %>% 
  ungroup() %>% 
  select(Station, Lag_Months, sum_ra) %>% 
  group_by(Station) %>% 
  do(tidy(lm(sum_ra ~ Lag_Months, data = .))) %>%
  filter(term == "Lag_Months") %>%
  select(Station,estimate,p.value) %>% print(n=25)

# Size distribution of small and large species

df %>%  
  mutate(ESD = (Average.biovolume*3/4/pi)^(1/3)) %>% 
  filter(Species %in% smallsp$Species) %>% 
  ggplot(aes(x = Species, y = ESD, group = Species))+
  # geom_boxplot()+
  # geom_violin(trim=TRUE)+
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange", linewidth=1.5,size=1,
               aes(color = Phyto_group))+
  coord_flip()+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15,face = "italic"),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15))+
  labs(y = expression(ESD~mu*m))+
  xlab("") +
  theme(legend.position = 'none')+
  scale_color_manual(values = c("coral1", "dodgerblue", "grey66")) -> p1

p1

df %>%  
  mutate(ESD = (Average.biovolume*3/4/pi)^(1/3)) %>% 
  filter(Species %in% largesp$Species) %>% 
  ggplot(aes(x = Species, y = ESD, group = Species))+
  # geom_boxplot()+
  # geom_violin(trim=TRUE)+
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange", linewidth=1.5,size=1,
               aes(color = Phyto_group))+
  coord_flip()+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15,face = "italic"),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=15))+
  labs(y = expression(ESD~mu*m))+
  xlab("") +
  theme(legend.position = 'none')+
  scale_color_manual(values = c("coral1", "dodgerblue", "grey66"))+
  scale_y_continuous(
    trans = "log2",
    labels = scales::math_format(2^.x, format = log2)) -> p2

p2

ggarrange(p1,p2, nrow = 1, ncol =2,  align = "v")

# Relation between %small and resident time -------------------------------
daf = data.frame(WCZ = c("Deep Bay", "Eastern Buffer", "Mirs Bay", "North Western",
                         "Port Shelter", "Southern", "Tolo Harbour",
                         "Victoria Harbour", "Western Buffer"),
                 Small_change_rate = c(-0.00083, -0.00058, -0.00028,-0.00041,
                                       -0.00029, -0.00082, -0.000041,-0.00043, 
                                       -0.00056),
                 Resident_time = c(1.39, 4.97,23.7,2.95,19.8,3.44,
                                   11.4,2.38,2.47))

daf %>%
  mutate(Small_change_rate1 = Small_change_rate*(-1)*(12)*(100),
         Resident_time1 = Resident_time %>% log(2)) %>% 
  ggplot(aes(x = Resident_time1, y = Small_change_rate1))+
  geom_point(aes(color = WCZ), size = 6)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size = 15))+ 
  theme(axis.text.x = element_text(color="black", size=15),
        axis.text.y = element_text(color="black", size=15))+
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15),
        axis.title=element_text(size=20))+
  geom_smooth(method = "lm", color = "black", size = 2)+   
  ylab("Annual decline rate of %small")+
  xlab("Residence time of water (day, log2-transformed)")+
  ggpubr::stat_cor(method = "pearson", size = 10)+
  ylim(0,1)






