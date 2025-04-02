library(readxl)
library(openxlsx)
library(tidyverse)
library(here)

film_cnae <- rbind(
  read_xlsx("atv_distribuicao_cinema.xlsx") %>% mutate(CNAE = "Distribuição"),
  read_xlsx("atv_exibicao_cinema.xlsx") %>% mutate(CNAE = "Exibição"),
  read_xlsx("atv_pos_cinema.xlsx") %>% mutate(CNAE = "Pós-Produção"),
  read_xlsx("atv_producao_cinema.xlsx") %>% mutate(CNAE = "Produção"),
  read_xlsx("estudios_cinema.xlsx") %>% mutate(CNAE = "Estudios"),
  read_xlsx("producao_filmes_publicidade.xlsx") %>% mutate(CNAE = "Filmes Publicidade"),
  read_xlsx("operadora_tv_ass_cabo.xlsx") %>% mutate(CNAE = "TV Cabo"),
  read_xlsx("operadora_tv_ass_satelite.xlsx") %>% mutate(CNAE = "TV Satelite"),
  read_xlsx("atv_tv_aberta.xlsx") %>% mutate(CNAE = "TV Aberta")
)

colnames(film_cnae) <- c("municipio", "qtd_empresas", "cnae")

film_cnae %>% group_by() %>% summarise(n = sum(qtd_empresas))

film_cnae <- film_cnae %>% left_join(read_xlsx("RD_Municipio_CNAE.xlsx"))

macrorregiao2 <- film_cnae %>% group_by(macrorreg_new) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
         INTERIOR = ifelse(macrorreg_new == "RMR", "Metropolitana", "Interior")) %>% 
  group_by(INTERIOR) %>% summarise(n = sum(n))


macrorregiao1 <- film_cnae %>%
  group_by(macrorreg_new) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(INTERIOR = ifelse(macrorreg_new == "RMR", "Metropolitana", "Interior"))


g101 <- ggplot(macrorregiao1) +
  geom_bar(stat = "identity", width = .6, aes(x = INTERIOR, y = n, fill = reorder(macrorreg_new, n))) +
  geom_text(aes(x = INTERIOR, y = n, fill = reorder(macrorreg_new, n),
                label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.1L), ")")), 
            position = position_stack(vjust = 0.5), size = 4) +
  geom_text(aes(x = INTERIOR, y = n,
                label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.1L), ")")), 
            size = 4, fontface = "bold", vjust = -.5,
            data = macrorregiao2) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 12)) +
  labs(x = "Região do Estado",
       y = "Empresas",
       fill = "Macrorregião") +
  scale_fill_manual(values = c("Agreste" = "#F35E23",
                               "Mata" = "#90C842", 
                               "Sertão" = "goldenrod3", 
                               "RMR" = "lightslateblue")) +
  expand_limits(y = c(0, 2500))

plot(g101)

ggsave("grafico 001.png")


mac <- unique(film_cnae %>% select(rd, macrorreg_new))

rd <- film_cnae %>% group_by(rd) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
         label = paste0(n, " (", prop, ")")) %>% 
  left_join(mac)

ggplot(rd, aes(x = reorder(rd, n), y = n)) +
  geom_point(size = 4, aes(color = macrorreg_new)) +
  coord_flip() +
  geom_text(aes(label = label), size = 3, hjust = -.2) +
  theme_bw() +
  theme(axis.text = element_text(size = 11, face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 12)) +
  expand_limits(y = c(0, 2700)) +
  labs(x = "RD", y = "Empresas", color = "Macrorregião")


ggsave("grafico 002.png")


cnae <- film_cnae %>% group_by(cnae) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
         label = paste0(n, " (", prop, ")"))


ggplot(cnae, aes(x = reorder(cnae, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightslateblue", width = .6, color = "black") +
  coord_flip() +
  geom_text(aes(label = label), size = 3, hjust = -.2) +
  theme_bw() +
  theme(axis.text = element_text(size = 11, face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 12)) +
  expand_limits(y = c(0, 3300)) +
  labs(x = "CNAE", y = "Empresas")

ggsave("grafico 003.png")

tabela_cnae <- film_cnae %>% spread(cnae, qtd_empresas)

tabela_cnae[is.na(tabela_cnae)] <- 0

tabela_cnae$xTotal <- tabela_cnae$Distribuição + tabela_cnae$Estudios + tabela_cnae$Exibição + 
  tabela_cnae$`Filmes Publicidade` + tabela_cnae$`Pós-Produção` + tabela_cnae$Produção

tabela_cnae <- tabela_cnae %>% left_join(read_xlsx("RD_Municipio_CNAE.xlsx"))

a <- tabela_cnae %>% arrange(-xTotal) %>% head(10)

write.xlsx(a, "tabela 001.xlsx")


tabela_cnae_long <- tabela_cnae %>% gather(key = cnae, value = qtd_empresas, Distribuição:xTotal) 

g1 <- ggplot(tabela_cnae_long %>% group_by(macrorreg_new, cnae) %>% 
               summarise(qtd_empresas = sum(qtd_empresas)),
             aes(x = macrorreg_new, y = qtd_empresas)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~cnae)

g1  


a <- film_cnae %>% group_by(cnae, macrorreg_new) %>% 
  summarise(qtd_empresas = sum(qtd_empresas)) %>% 
  group_by(macrorreg_new) %>% 
  mutate(prop = scales::percent(qtd_empresas/sum(qtd_empresas), accuracy = 0.1))  
  
g2 <- ggplot(a,
             aes(x = reorder(cnae, qtd_empresas), y = qtd_empresas)) + 
  geom_bar(stat = "identity", aes(fill = macrorreg_new), color = "black") +
  geom_text(aes(label = paste0(qtd_empresas, " (", prop, ")")), size = 3, hjust = -.1) +
  facet_wrap(~macrorreg_new, scale = "free_x") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "none") +
  expand_limits(y = 2700) +
  scale_fill_manual(values = c("Agreste" = "#F35E23",
                               "Mata" = "#90C842", 
                               "Sertão" = "goldenrod3", 
                               "RMR" = "lightslateblue")) +
  labs(x = "CNAE", y = "Empresas")

g2  

ggsave("grafico 004.png")

g3 <- ggplot(tabela_cnae_long %>% group_by(rd, cnae) %>% 
               summarise(qtd_empresas = sum(qtd_empresas)),
             aes(x = cnae, y = qtd_empresas)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~rd) +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid = element_blank())


g3  

# CEMPRE

cempre <- read_xlsx("dados_audiovisual_sidra2.xlsx") %>% 
  group_by(ANO) %>% 
  summarise(Empresas = sum(Empresas),
            Pessoal = sum(`Pessoal Ocupado`),
            Assalariado_masc = sum(`Assalariado Masculino`),
            Assalariado_fem = sum(`Assalariado Feminino`))


library(ggrepel)

ggplot(cempre, aes(x = ANO, y = Empresas)) +
  geom_smooth(method = "lm", linewidth =.1, lty = "dashed", color = "black", fill = "grey") +
  geom_line() + 
  geom_label_repel(aes(label = Empresas), size = 3) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("grafico 005.png")

ggplot(cempre, aes(x = ANO, y = Pessoal)) +
  geom_smooth(method = "lm", linewidth =.1, lty = "dashed", color = "black", fill = "grey") +
  geom_line() + 
  geom_label_repel(aes(label = Pessoal), size = 3) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("grafico 006.png")


ggplot(cempre %>% drop_na(), aes(x = ANO)) +
  geom_line(aes(y = Assalariado_masc), color = "lightslateblue") +
  geom_line(aes(y = Assalariado_fem), color = "#F35E23") +
  geom_label_repel(aes(label = Assalariado_masc, y = Assalariado_masc), size = 3, color = "lightslateblue") +
  geom_label_repel(aes(label = Assalariado_fem, y = Assalariado_fem), size = 3, color = "#F35E23") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("grafico 007.png")



# DADOS SEM MEI

setwd(here("DADOS SEM MEI"))


film_cnae <- rbind(
  read_xlsx("distribuicao_cinema.xlsx") %>% mutate(CNAE = "Distribuição"),
  read_xlsx("atv_exibicao.xlsx") %>% mutate(CNAE = "Exibição"),
  read_xlsx("atv_pos_cinema.xlsx") %>% mutate(CNAE = "Pós-Produção"),
  read_xlsx("prod_cinema.xlsx") %>% mutate(CNAE = "Produção"),
  read_xlsx("estudio_cinema.xlsx") %>% mutate(CNAE = "Estudios"),
  read_xlsx("prod_filmes_publicidade.xlsx") %>% mutate(CNAE = "Filmes Publicidade"),
  read_xlsx("tv_cabo.xlsx") %>% mutate(CNAE = "TV Cabo"),
  read_xlsx("tv_satelite.xlsx") %>% mutate(CNAE = "TV Satelite"),
  read_xlsx("tv_aberta.xlsx") %>% mutate(CNAE = "TV Aberta")
)

colnames(film_cnae) <- c("municipio", "qtd_empresas", "cnae")

film_cnae %>% group_by() %>% summarise(n = sum(qtd_empresas))

film_cnae <- film_cnae %>% left_join(read_xlsx("RD_Municipio_CNAE.xlsx"))

macrorregiao2 <- film_cnae %>% group_by(macrorreg_new) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
         INTERIOR = ifelse(macrorreg_new == "RMR", "Metropolitana", "Interior")) %>% 
  group_by(INTERIOR) %>% summarise(n = sum(n))


macrorregiao1 <- film_cnae %>%
  group_by(macrorreg_new) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(INTERIOR = ifelse(macrorreg_new == "RMR", "Metropolitana", "Interior"))


g101 <- ggplot(macrorregiao1) +
  geom_bar(stat = "identity", width = .6, aes(x = INTERIOR, y = n, fill = reorder(macrorreg_new, n))) +
  geom_text(aes(x = INTERIOR, y = n, fill = reorder(macrorreg_new, n),
                label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.1L), ")")), 
            position = position_stack(vjust = 0.5), size = 4) +
  geom_text(aes(x = INTERIOR, y = n,
                label = paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.1L), ")")), 
            size = 4, fontface = "bold", vjust = -.5,
            data = macrorregiao2) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 12)) +
  labs(x = "Região do Estado",
       y = "Empresas",
       fill = "Macrorregião") +
  scale_fill_manual(values = c("Agreste" = "#F35E23",
                               "Mata" = "#90C842", 
                               "Sertão" = "goldenrod3", 
                               "RMR" = "lightslateblue")) +
  expand_limits(y = c(0, 650))

plot(g101)

ggsave("grafico 001.png")


mac <- unique(film_cnae %>% select(rd, macrorreg_new))

rd <- film_cnae %>% group_by(rd) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
         label = paste0(n, " (", prop, ")")) %>% 
  left_join(mac)

ggplot(rd, aes(x = reorder(rd, n), y = n)) +
  geom_point(size = 4, aes(color = macrorreg_new)) +
  coord_flip() +
  geom_text(aes(label = label), size = 3, hjust = -.2) +
  theme_bw() +
  theme(axis.text = element_text(size = 11, face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 12)) +
  expand_limits(y = c(0, 700)) +
  labs(x = "RD", y = "Empresas", color = "Macrorregião")


ggsave("grafico 002.png")


cnae <- film_cnae %>% group_by(cnae) %>% summarise(n = sum(qtd_empresas)) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
         label = paste0(n, " (", prop, ")"))


ggplot(cnae, aes(x = reorder(cnae, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightslateblue", width = .6, color = "black") +
  coord_flip() +
  geom_text(aes(label = label), size = 3, hjust = -.2) +
  theme_bw() +
  theme(axis.text = element_text(size = 11, face = "bold"),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 12)) +
  expand_limits(y = c(0, 400)) +
  labs(x = "CNAE", y = "Empresas")

ggsave("grafico 003.png")

tabela_cnae <- film_cnae %>% spread(cnae, qtd_empresas)

tabela_cnae[is.na(tabela_cnae)] <- 0

tabela_cnae$xTotal <- tabela_cnae$Distribuição + tabela_cnae$Estudios + tabela_cnae$Exibição + 
  tabela_cnae$`Filmes Publicidade` + tabela_cnae$`Pós-Produção` + tabela_cnae$Produção

tabela_cnae <- tabela_cnae %>% left_join(read_xlsx("RD_Municipio_CNAE.xlsx"))

a <- tabela_cnae %>% arrange(-xTotal) %>% head(10)

write.xlsx(a, "tabela 001.xlsx")


tabela_cnae_long <- tabela_cnae %>% gather(key = cnae, value = qtd_empresas, Distribuição:xTotal) 

g1 <- ggplot(tabela_cnae_long %>% group_by(macrorreg_new, cnae) %>% 
               summarise(qtd_empresas = sum(qtd_empresas)),
             aes(x = macrorreg_new, y = qtd_empresas)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~cnae)

g1  


a <- film_cnae %>% group_by(cnae, macrorreg_new) %>% 
  summarise(qtd_empresas = sum(qtd_empresas)) %>% 
  group_by(macrorreg_new) %>% 
  mutate(prop = scales::percent(qtd_empresas/sum(qtd_empresas), accuracy = 0.1))  

g2 <- ggplot(a,
             aes(x = reorder(cnae, qtd_empresas), y = qtd_empresas)) + 
  geom_bar(stat = "identity", aes(fill = macrorreg_new), color = "black") +
  geom_text(aes(label = paste0(qtd_empresas, " (", prop, ")")), size = 3, hjust = -.1) +
  facet_wrap(~macrorreg_new, scale = "free_x") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "none") +
  expand_limits(y = 350) +
  scale_fill_manual(values = c("Agreste" = "#F35E23",
                               "Mata" = "#90C842", 
                               "Sertão" = "goldenrod3", 
                               "RMR" = "lightslateblue")) +
  labs(x = "CNAE", y = "Empresas")

g2  

ggsave("grafico 004.png")

g3 <- ggplot(tabela_cnae_long %>% group_by(rd, cnae) %>% 
               summarise(qtd_empresas = sum(qtd_empresas)),
             aes(x = cnae, y = qtd_empresas)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~rd) +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid = element_blank())


g3  
