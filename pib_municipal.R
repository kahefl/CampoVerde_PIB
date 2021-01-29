library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)

setwd("C:\\Users\\kahel\\OneDrive\\Documents\\Coisas do R\\scripts\\Campo_Verde\\pib_municipal")

pib <- read_xlsx("C:/Users/kahel/OneDrive/campo_verde/tab_25.xlsx")

p1 <- pib %>%
  select(Ano,pib) %>% 
  mutate(setor = rep("pib",16)) %>% 
  rename(valor = pib) %>%
  rename(anos = Ano) %>%
  mutate(anos=as.numeric(anos))

p2 <- pib %>%
  select(Ano,imposto) %>% 
  mutate(setor = rep("Arrecadação de Impostos",16)) %>% 
  rename(valor = imposto) %>% 
  rename(anos = Ano) %>%
  mutate(anos=as.numeric(anos))

p3 <- pib %>%
  select(Ano,agro) %>% 
  mutate(setor = rep("Setor da Agropecuária",16)) %>% 
  rename(valor = agro) %>%
  rename(anos = Ano) %>%
  mutate(anos=as.numeric(anos))

p4 <- pib %>%
  select(Ano,indus) %>% 
  mutate(setor = rep("Setor da Indústria",16)) %>% 
  rename(valor = indus) %>% 
  rename(anos = Ano) %>%
  mutate(anos=as.numeric(anos))

p5 <- pib %>%
  select(Ano,servicos) %>% 
  mutate(setor = rep("Setor de Serviços",16)) %>% 
  rename(valor = servicos) %>%
  rename(anos = Ano) %>%
  mutate(anos=as.numeric(anos))

p6 <- pib %>%
  select(Ano,`admin publico`) %>% 
  mutate(setor = rep("Administração Pública",16)) %>% 
  rename(valor = `admin publico`) %>%
  rename(anos = Ano) %>%
  mutate(anos=as.numeric(anos))


p7 <- rbind(p2,p3,p4,p5,p6)

ggplot(p7, aes(y=valor, anos, fill=setor, sprintf("%0.2f", round(valor, digits = 2))))+
  geom_col(position = "fill",size = 0.1, width = 0.85)+
  scale_y_continuous(labels = label_percent(suffix = "%"))+
  labs(x=" ", y=" ", title = " ",subtitle  = " ") +
  theme_excel_new()+
  scale_fill_manual(values = c("#CD853F","#FDE910","#228B22", "#007FFF", "#FF8C00"))+
  theme(axis.text.x.bottom = element_text(colour = "black", angle = 45),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major.y = element_line(colour = "grey80", size=0.10),
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "bottom",
        plot.title = element_text(colour = "black", size = 20, vjust = 0.5),
        plot.subtitle = element_text(size =15, colour = "black"))+
  geom_text(size=2.98, fontface="bold",aes(label = ifelse(valor == 0,"", paste0("R$ ", 1.0*valor))),
            position = position_fill(vjust = 0.5))

ggsave("CV_pib.jpg", width = 11, height = 7)
