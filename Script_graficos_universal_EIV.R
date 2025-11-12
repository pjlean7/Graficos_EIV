#Limpando o ambiente
rm(list = ls())

#Ativando bibliotecas 
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("tidyr")
install.packages("rmarkdown")
install.packages("gridExtra")
install.packages("writexl")
install.packages("gt")
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(readxl)
library(rmarkdown)
library(gridExtra)
library(writexl)
library(gt)
####################################################################################################
##### ATENÇÃO, SEMPRE QUE FOR UTILIZAR MUDAR O BANCO DE DADOS, A LEGENDA ########################
####                E O LOCAL ONDE SERÁ SALVO OS GRÁFICOS              ########################
###############################################################################################

###Leitura de dados
dados <- read_excel("C:/Users/Graça/Documents/BD_documentos_EIV_Valence.xlsx")
legenda <- read_excel("C:/Users/Graça/Documents/BD_documentos_EIV_Valence.xlsx", sheet = 2)
setwd <- ("C:/Users/Graça/Documents/GRAFICOS_TESTE_eiv_pedro/")

head(legenda)
dados$start<-NULL
dados$end<-NULL

## Ver os daNULL# Ver os dados carregados
head(dados)
class(dados$today)
table(dados$today)
dados$today <- as.Date(dados$today, origin = "1899-12-30")
colnames(dados)[colnames(dados) == "today"] <- "data da entrevista"

novos_nomes <- sapply(colnames(dados), function(col) {
  match_index <- match(col, legenda$Nº)
  if (!is.na(match_index)) {
    return(legenda$legenda[match_index])
  } else {
    return(col)  # Mantém o nome original se não encontrar correspondência
  }
})

## Aplicar os novos nomes às colunas de 'dados'
colnames(dados) <- novos_nomes

####Caminho para as tabelas
caminho_tab <- "C:/Users/Graça/Praxis Projetos e Consultoria Ltda/Praxis Projetos e Consultoria Ltda - Projetos/2024_EIV_Valence/Percepcao/Graficos_Tabelas/Tabelas"

##Função salvar gráfico - ****LEMBRAR DE MODIFICAR O DIRETÓRIO!!!****  
salvar_grafico_dir <- function(grafico, nome_arquivo, 
                               diretorio = "C:/Users/Graça/Praxis Projetos e Consultoria Ltda/Praxis Projetos e Consultoria Ltda - Projetos/2024_EIV_Valence/Percepcao/Graficos_Tabelas/Gráficos_R",
                               formato = "png", largura = 8, altura = 6, dpi = 300, ...) {
  
  # Verificar e criar o diretório se não existir
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
    message("Diretório criado: ", diretorio)
  }
  
  # Construir o caminho completo
  caminho_completo <- file.path(diretorio, paste0(nome_arquivo, ".", formato))
  
  # Salvar o gráfico
  ggplot2::ggsave(
    filename = caminho_completo,
    plot = grafico,
    device = formato,
    width = largura,
    height = altura,
    dpi = dpi,
  )
  
  message("Gráfico salvo em: ", caminho_completo)
  return(invisible(caminho_completo))
}

#################################### Criação dos gráficos ##############################################


###Criação do gráfico genero
contagem_sexo <- dados %>%
  group_by(sex) %>%
  summarise(n = n())

cor_sexo <- c("Feminino" ="#c24641", "feminino" = "#c24641", "Masculino" = "#4F81BD", "masculino" = "#4F81BD" )

genero <- ggplot(contagem_sexo, aes(x = sex, y = n, fill = sex))+
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = n), vjust = -0.3, color = "black") + 
  labs(x = "Sexo", y = "Quantidade", title = "")+
  scale_fill_manual(values = cor_sexo) +
  guides(fill = guide_legend(title = "Sexo"))+
  theme_minimal()+
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 12, color = "#222"),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12, margin = margin( r = 2, l = 2)),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank()
  ) +
  coord_cartesian(ylim = c(0, max(contagem_sexo$n) + 5))

salvar_grafico_dir(grafico = genero,
                   nome_arquivo = "Genero")

######Criação do gráfico Faixa de idade 
contagem_faixa_idade <- dados %>%
  group_by(Faixa_idade) %>%
  summarise(n = n())

cores_idade <- c('#76923c', '#ffd04b', '#F69200', '#FCAABE', '#c24641')

idade <- ggplot(contagem_faixa_idade, aes(x = Faixa_idade, y = n, fill = Faixa_idade)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.3, color = "black") +  # Adiciona os números sobre as barras
  scale_fill_manual(values = cores_idade) + 
  labs(title = "",
       x = "Faixa Etária",
       y = "Número de Pessoas") +
  guides(fill = guide_legend(title = "Faixa Etária"))+
  theme_minimal()+
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 12, color = "#222"),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank()
  )

salvar_grafico_dir(grafico = idade,
                   nome_arquivo = "Idade")

##### Criação do gráfico Escolaridade
contagem_escolaridade <- dados %>%
  group_by(escol) %>%
  summarise(n = n())

verdes <- colorRampPalette(c("#76923c", "#8edb6f"))(7)

escolaridade <- ggplot(contagem_escolaridade, aes(x = escol, y = n, fill = escol)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.3, color = "black") +  # Adiciona os números sobre as barras
  scale_fill_manual(values = verdes) + 
  labs(title = "",
       x = "Escolaridade",
       y = "Número de Pessoas") +
  guides(fill = guide_legend(title = "Escolaridade"))+
  theme_minimal()+
  theme( panel.background = element_rect(fill = "gray90"),
       plot.background = element_rect (fill ="gray90"),
       axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
       axis.title.x = element_blank(),
       axis.text.x = element_blank(),
       axis.text.y = element_text(size = 8, color = "#222"),
       legend.title = element_text(face = "bold", size = 10),
       legend.text = element_text(size = 8),     # Diminui o tamanho do texto da legenda
       legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
       plot.margin = unit(c(0, 0, 0, 0), "cm"),
       panel.spacing = unit(0, "lines"),
       axis.ticks.y = element_blank(),
       panel.border = element_blank()
)
salvar_grafico_dir(grafico = escolaridade,
                   nome_arquivo = "Escolaridade")

######### Criação do gráfico Situação ocupacional e setor de atividade

dados$set_atv <- factor(
  dados$set_atv,
  levels = c("nao_se_aplica", "industria", "const_civil", "comercio", 
             "ser_pub", "outro", "outros_servicos"),
  labels = c("Não se aplica (aposentado, estudante, dona de casa...)", 
             "Indústria", "Construção civil", "Comércio",
             "Serviços Públicos", "Outro", "Outros serviços")
)

ordem_atv <- c("Comércio","Indústria", "Serviços Públicos", "Construção civil", "Outros serviços", "Outro", "Não se aplica (aposentado, estudante, dona de casa...)")

dados$set_atv <-factor(dados$set_atv, levels = ordem_atv)

contagem_ocupacional_atividade <- dados %>%
  group_by(sit_ocup, set_atv)%>%
  summarise(n = n())

contagem_ocupacional_atividade <- contagem_ocupacional_atividade %>%
  group_by(set_atv) %>%
  mutate(posicao_texto = cumsum(n) - 0.5 * n)


cor_sit <- c("#808000","#6A5ACD", "#B22222", '#DAA520', "#D2691E", "#8B4513", "#696969")

sit_ocup <- ggplot(contagem_ocupacional_atividade, aes(x = sit_ocup, y = n, fill = set_atv)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cor_sit,
                    labels = c("Não se aplica (aposentado, estudante, dona de casa...)" = "Não se aplica")) + 
  labs(title = "",
       x = "Situação ocupacional",
       y = "Setor de atividade") +
  guides(fill = guide_legend(title = "Setor de atividade"))+
  theme_minimal()+
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 12, color = "#222"),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank()
)

salvar_grafico_dir(grafico = sit_ocup,
                   nome_arquivo = "Situação ocupacional por setor de atividade")

###Salvando tabela 
contagem_ocupacional_atividade %>% gt()

write_xlsx(contagem_ocupacional_atividade, path = file.path(caminho_tab, "Situação ocupacional e setor de atividade.xlsx"))

########## Criação do gráfico de Renda
dados$renda <- factor(
  dados$renda,
  levels = c("sem_renda", "ate_1", "mais_de_1_a_3", "mais_de_3_a_5", "mais_de_5_a_10", 
             "mais_de_10"),
  labels = c("Sem renda", "Até 1", "Mais de 1 a 3", "Mais de 3 a 5", "Mais de 5 a 10", 
             "Mais de 10")
)

ordem_renda <- c("Sem renda", "Até 1", "Mais de 1 a 3", "Mais de 3 a 5", "Mais de 5 a 10", "Mais de 10")

dados$renda <-factor(dados$renda, levels = ordem_renda)

contagem_renda <- dados %>%
  group_by(renda)%>%
  summarise(n = n())

cor_renda <- c('#ffd04b', '#F69200','#c24641', '#C40505', '#604A7B', '#76923c')

renda <- ggplot(contagem_renda, aes(x = renda, y = n, fill = renda)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.3, color = "black") +  # Adiciona os números sobre as barras
  scale_fill_manual(values = cor_renda) + 
  labs(title = "",
       x = "Renda",
       y = "Número de Pessoas", 
  y = "Setor de atividade") +
  guides(fill = guide_legend(title = "Renda")) +
  theme_minimal()+
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 12, color = "#222"),,
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 12),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank()
  )

salvar_grafico_dir(grafico = renda,
                   nome_arquivo = "Renda")



################################################ Avaliação ####################################################
##### Criação do gráfico de AVALIAÇÃO GERAL DO BAIRRO

########### Ajeitando colunas
dados$geral_av <- factor(
  dados$geral_av,
  levels = c("otimo", "bom", "razoavel", "ruim"),
  labels = c("Ótimo", "Bom","Razoável","Ruim")
)



# Lista das colunas
colunas_para_recodificar <- c("av_agua", "av_esgoto","av_lixo","av_drena","av_educ", 
                              "av_saud","av_lazer", "av_verde","av_comerc","av_segur","av_imovel",
                              "av_vizinho","av_ar","av_som","av_transp","av_ponto","av_transt",
                              "av_vaga","av_sinal","av_circu","av_calçada","av_ciclo","av_arbor",
                              "av_mob")

# Recodificar todas
dados[colunas_para_recodificar] <- lapply(
  dados[colunas_para_recodificar],
  function(x) {
    factor(
      x,
      levels = c("otimo", "bom", "regular", "ruim", 
                 "pessimo", "nao_sabe", "inexistente"),
      labels = c("Ótimo", "Bom", "Regular", "Ruim",
                 "Péssimo", "Não sabe", "Inexistente")
    )
  }
)

###########
ordem_av <- c("Ótimo", "Bom", "Razoável", "Ruim")

dados$geral_av <-factor(dados$geral_av, levels = ordem_av)

contagem_avaliacao_bairro <- dados %>%
  group_by(geral_av) %>%
  summarise(n = n())

cor_av_bair <- c('#00b050', '#d3e171', '#FFDC6D', '#F69200')

av_geral <- ggplot(contagem_avaliacao_bairro, aes(x = geral_av, y = n, fill = geral_av)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.3, color = "black") +  # Adiciona os números sobre as barras
  scale_fill_manual(values = cor_av_bair) + 
  labs(title = "",
       x = "Avaliação do bairro",
       y = "Número de Pessoas") +
  guides(fill = guide_legend(title = "Valor Avaliação")) + 
  theme_minimal()+
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 12, color = "#222"),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank()
  )

salvar_grafico_dir(grafico = av_geral,
                   nome_arquivo = "Avaliação geral do bairro")

############ Criação do gráfico de avaliação dos temas

#Ordenando as variaveis
ordem_situacoes <- c("Não sabe", "Inexistente","Péssimo","Ruim", "Regular","Bom","Ótimo")

#Escolhendo as cores para os gráficos
cores_tema <- c("Não sabe" = "#BFBFBF",   
                "Inexistente" = "#595959",
                "Péssimo" = "#B22222",
                "Ruim" = "#F69200",
                "Regular" = "#FFDC6D",
                "Bom" = "#d3e171",
                "Ótimo" = "#9BBB59")

titulos_legiveis_sep <- c("av_agua" = "Fornecimento de água",
                      "av_esgoto" = "Rede de esgoto",
                      "av_lixo" = "Coleta de lixo",
                      "av_drena" = "Drenagem de água da chuva",
                      "av_educ" = "Serviço público de educação",
                      "av_saud" = "Serviço público de saúde",
                      "av_lazer"= "Espaços públicos de lazer",
                      "av_verde" = "Áreas verdes",
                      "av_comerc" = "Comércio e serviços",
                      "av_segur" = "Segurança pública",
                      "av_imovel" = "Valorização dos imóveis",
                      "av_vizinho" = "Relações de vizinhança",
                      "av_ar" = "Poluição do ar",
                      "av_som" = "Poluição sonora",
                      "av_transp" = "Transporte público",
                      "av_ponto" = "Pontos de ônibus ",
                      "av_transt" = "Circulação viária",
                      "av_vaga" = "Vagas estacionamento",
                      "av_sinal" = "Sinalização",
                      "av_circu" = "Circulação pedestres",
                      "av_calçada" = "Qualidade de calçadas",
                      "av_ciclo" = "Ciclovias",
                      "av_arbor" = "Arborização das vias",
                      "av_mob" = "Mobiliário urbano")

dados_long <- pivot_longer(dados, 
                           cols = starts_with("av"), 
                           names_to = "Tipo_Avaliacao", 
                           values_to = "Valor_Avaliacao")

dados_contagem <- dados_long %>%
  group_by(Tipo_Avaliacao, Valor_Avaliacao) %>%
  summarise(frequencia = n()) %>%
  ungroup()

dados_contagem$Valor_Avaliacao <- factor(dados_contagem$Valor_Avaliacao, levels = ordem_situacoes )

dev.new() ## Abre uma janel externa para o gráfico

av_temas <- ggplot(dados_contagem, aes(x = Tipo_Avaliacao, y = frequencia, fill = Valor_Avaliacao)) +
  geom_bar(stat = "identity") +   # Usar os valores diretamente para as barras
  coord_flip() +                  # Inverter as barras para o formato horizontal
  geom_text(aes(label = ifelse(frequencia > 10, as.character(frequencia), "")), position = position_stack(vjust = 0.5), color = "black", size = 3) +
  scale_fill_manual(values = cores_tema)+
  scale_x_discrete(labels = titulos_legiveis_sep) +
  labs(title = "", x = "Tema", y = "Valor da Avaliação") +
  guides(fill = guide_legend(title = "Valor Avaliação", reverse = TRUE)) +
  theme(axis.text.x = "titulos_legiveis",       # Remove os rótulos das avaliações no eixo y
        axis.ticks.y = element_blank()) +
  theme_minimal()+
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 11, color = "#222"),
         axis.text.y = element_text(size = 11, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 8),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank()
  )

salvar_grafico_dir(grafico = av_temas,
                   nome_arquivo = "Avaliação de temas")

###################################################Prioridade##############################################
##### Gráfico temas prioridade Geral (Necessário criar nomenclatura para os temas, evitando repetição)

############RECODIFICANDO
colunas_para_recodificar <- c("preocup_a","preocup_b","preocup_c")

# Recodificar todas
dados[colunas_para_recodificar] <- lapply(
  dados[colunas_para_recodificar],
  function(x) {
    factor(
      x,
      levels = c("arborizacao_vias", "areas_verdes", "ciclovias", 
                             "circulacao_pedestres", "circulacao_viaria", 
                             "coleta_lixo", "comercio_serviços", "dren_agua_chuva",
                             "esp_pub_lazer", "espacos_vagas_estacio",
                             "fornecimento_agua", "mob_urb", "poluicao_ar",
                             "poluicao_sonora", "pontos_onibus", "qualidade_calcadas",
                             "rede_esgoto", "rel_vizinhanca", "seguranca_publica",
                             "serviço_pub_educ", "serviço_pub_saude", "sinalizacao",
                             "transp_publico", "valorizacao_imoveis"),
      labels = c("Arborização das vias", "Áreas verdes", "Ciclovias", "Circulação pedestres (acessos, faixas, etc.)",
                 "Circulação viária - trânsito", "Coleta de lixo", "Comércio e serviços", "Drenagem de água chuva", "Espaços públicos de lazer",
                 "Espaços/vagas estacionamento", "Fornecimento de água", "Mobiliário urbano (lixeira, banco, etc.)", "Poluição do ar",
                 "Poluição sonora (ruído)", "Pontos de ônibus", "Qualidade de calçadas", "Rede de esgoto", "Relações de vizinhança",
                 "Segurança pública", "Serviço público de educação", "Serviço público de saúde", "Sinalização (pedestres, veículos, ônibus, etc)",
                 "Transporte público", "Valorização dos imóveis")
    )
  }
)
#############

cores_problemas <- c("Arborização das vias" = "#006400",
                     "Áreas verdes" = "#6B8E23",
                     "Calçadas" = "#D3D3D3",
                     "Ciclovias" = "#FFA500",
                     "Circulação pedestres (acessos, faixas, etc.)" = "#ADD8E6",
                     "Circulação viária - trânsito" = "#FF0000",
                     "Coleta de lixo" = "#A52A2A",
                     "Comércio e serviços" = "#800080",
                     "Drenagem de água chuva" = "#00008B",
                     "Espaços públicos de lazer" = "#FFFF00",
                     "Espaços/ vagas estacionamento" = "#A9A9A9",
                     "Fornecimento de água" = "#87CEEB",
                     "Mobiliário urbano (lixeira, banco, etc.)" = "#F5F5DC",
                     "Poluição do ar" = "#000000",
                     "Poluição sonora (ruído)" = "#8B0000",
                     "Pontos de ônibus" = "#32CD32",
                     "Qualidade de calçadas" = "#808080",
                     "Rede de esgoto" = "#654321",
                     "Relações de vizinhança" = "#FFC0CB",
                     "Segurança pública" = "#4169E1",
                     "Serviço público de educação" = "#90EE90",
                     "Serviço público de saúde" = "#FF6347",
                     "Sinalização (pedestres, veículos, ônibus, etc)" = "#FFFF00",
                     "Transporte público" = "#0000CD",
                     "Valorização dos imóveis" = "#FFD700",
                     "Áreas verdes" = "#98FB98")
  
colunas_preocup <- c("preocup_a", "preocup_b", "preocup_c") 

titulos_preocup <- c("preocup_a" = "Primeiro tema",
                     "preocup_b" = "Segundo tema",
                     "preocup_c" = "Terceiro tema")

dados_preocup <- pivot_longer(dados, 
                           cols = starts_with("preocup"), 
                           names_to = "preocupacao", 
                           values_to = "Valor_preocup")

dados_preocup_1 <- dados_preocup %>%
  group_by(preocupacao, Valor_preocup) %>%
  summarise(frequencia = n()) %>%
  ungroup()

dados_preocup_a <- dados_preocup_1 %>%
  filter(preocupacao == "preocup_a") %>%      # Filtra os dados para 'preocup_a'
  group_by(Valor_preocup) %>%                      # Agrupa pelos temas
  summarise(frequencia_total = sum(frequencia)) %>%  # Soma as frequências de cada tema
  arrange(desc(frequencia_total))

dados_preocup_a_limpo <- na.omit(dados_preocup_a)

top_3_temas_a <- dados_preocup_a_limpo %>%
  group_by(Valor_preocup) %>%                    # Agrupar por tema
  summarise(frequencia_total = sum(frequencia_total)) %>%  # Somar a frequencia de cada tema
  arrange(desc(frequencia_total)) %>%    # Ordenar pela frequencia total (decrescente)
  slice_head(n = 3)

grafico_a <-ggplot(top_3_temas_a, aes(x = Valor_preocup, y = frequencia_total, fill = Valor_preocup)) +
  geom_bar(stat = "identity", width = 0.7) +              # Barras para mostrar as frequências
  geom_text(aes(label = frequencia_total),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Tema", y = "Frequência") +
  scale_fill_manual(values = cores_problemas)+
  guides(fill = guide_legend(title = "Temas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(top_3_temas_a$frequencia_total) + 10))

### Preocupação B
dados_preocup_b <- dados_preocup_1 %>%
  filter(preocupacao == "preocup_b") %>%      # Filtra os dados para 'preocup_a'
  group_by(Valor_preocup) %>%                      # Agrupa pelos temas
  summarise(frequencia_total = sum(frequencia)) %>%  # Soma as frequências de cada tema
  arrange(desc(frequencia_total))

dados_preocup_b_limpo <- na.omit(dados_preocup_b) 

top_3_temas_b <- dados_preocup_b_limpo %>%
  group_by(Valor_preocup) %>%                    # Agrupar por tema
  summarise(frequencia_total = sum(frequencia_total)) %>%  # Somar a frequencia de cada tema
  arrange(desc(frequencia_total)) %>%    # Ordenar pela frequencia total (decrescente)
  slice_head(n = 3)

grafico_b <- ggplot(top_3_temas_b, aes(x = Valor_preocup, y = frequencia_total, fill = Valor_preocup)) +
  geom_bar(stat = "identity", width = 0.7) +              # Barras para mostrar as frequências
  geom_text(aes(label = frequencia_total),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Tema", y = "Frequência") +
  scale_fill_manual(values = cores_problemas) +
  guides(fill = guide_legend(title = "Temas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(top_3_temas_a$frequencia_total) + 10))

dados_preocup_cores <- dados_preocup_1 %>%
  group_by(Valor_preocup)%>%
  summarise(preocupacao = n())

### Preocupação C
dados_preocup_c <- dados_preocup_1 %>%
  filter(preocupacao == "preocup_c") %>%      # Filtra os dados para 'preocup_a'
  group_by(Valor_preocup) %>%                      # Agrupa pelos temas
  summarise(frequencia_total = sum(frequencia)) %>%  # Soma as frequências de cada tema
  arrange(desc(frequencia_total))

dados_preocup_c_limpo <- na.omit(dados_preocup_c)

top_3_temas_c <- dados_preocup_c_limpo %>%
  group_by(Valor_preocup) %>%                    # Agrupar por tema
  summarise(frequencia_total = sum(frequencia_total)) %>%  # Somar a frequencia de cada tema
  arrange(desc(frequencia_total)) %>%    # Ordenar pela frequencia total (decrescente)
  slice_head(n = 3)

grafico_c <- ggplot(top_3_temas_c, aes(x = Valor_preocup, y = frequencia_total, fill = Valor_preocup)) +
  geom_bar(stat = "identity", width = 0.7) +              # Barras para mostrar as frequências
  geom_text(aes(label = frequencia_total),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Tema", y = "Frequência") +
  scale_fill_manual(values = cores_problemas) + 
  guides(fill = guide_legend(title = "Temas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(top_3_temas_a$frequencia_total) + 10))

#### Modificar diretório!!
ggsave("C:/Users/Graça/Documents/GRAFICOS_TESTE_eiv_pedro/temas.png",
  plot = grid.arrange(grafico_a, grafico_b, grafico_c, ncol = 1),
  width = 10, height = 5, dpi = 300) 


########################################### Deposição lixo ##################################################
### Grafico de deposição irregular de lixo
cores_5_problemas <- c("depo_irregu" = "#6B4C3B", 
                       "polu_visual" = "#A9A9A9" ,	
                       "polu_geral" = "#4682B4",	
                       "prob_drenag" = "#5F9EA0" ,	
                       "prob_imovel" = "#FFA500")

colunas_s_n_ns <- c("depo_irregu", "polu_visual",	"polu_geral",	"prob_drenag",	"prob_imovel")

titulos_ns <- c("depo_irregu" = "Problemas com Deposito irregular de lixo",
                "polu_visual" = "Problemas com Poluição visual",
                "polu_geral" = "Problemas com Poluição sonora, atmosférica e das águas",
                "prob_drenag" = "Problemas com Drenagem e indundações nos períodos de chuva",
                "prob_imovel" = "Problemas com Presença de imóveis vazios")

dados_problemas <- dados %>%
  select(depo_irregu, polu_visual, polu_geral, prob_drenag, prob_imovel)
 
dados_problemas_sim <- colSums(dados_problemas == "Sim")
  

contagem_sim <- dados_problemas %>%
  summarise(
    depo_irregu = sum(depo_irregu == "sim"),
    polu_visual = sum(polu_visual == "sim"),
    polu_geral = sum(polu_geral == "sim"),
    prob_drenag = sum(prob_drenag == "sim"),
    prob_imovel = sum(prob_imovel == "sim")
    )

contagem_sim <- contagem_sim %>%
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")



problemas <- ggplot(contagem_sim, aes(x = variavel, y = valor, fill = variavel)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = valor),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "", y = "Contagem") +
  scale_fill_manual(values = cores_5_problemas,
                    labels = c("depo_irregu" = "Deposito irregular de lixo",
                               "polu_visual" = "Poluição visual",
                               "polu_geral" = "Poluição sonora, atmosférica e das águas",
                               "prob_drenag" = "Drenagem e indundações nos períodos de chuva",
                               "prob_imovel" = "Presença de imóveis vazios")) +
  guides(fill = guide_legend(title = "Temas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 12, color = "#222"),
         axis.text.y = element_text(size = 12, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())

salvar_grafico_dir(grafico = problemas,
                   nome_arquivo = "Problemas")

##### Grafico problemas separado por setor
dados[] <- lapply(dados, function(x) {
  if(is.character(x)) {
    x[tolower(x) == "sim"] <- "Sim"
    x[tolower(x) == "nao"] <- "Não"
    x[tolower(x) == "nao_sabe_nao_respondeu"] <- "Não sabe / Não respondeu"
  }
  x
})

cor_sim_nao <- c("Sim" = '#006a8e',
                 "Não" = '#b1283a',
                 "NR" = '#a8a6a7',
                 "Não sabe / Não respondeu" = '#a8a6a7')

dados_problemas_sep <- dados %>%
  select(depo_irregu, polu_visual, polu_geral, prob_drenag, prob_imovel, tip_quest)

ordem_sim_nao <- c("Sim", "Não", "Não sabe / Não respondeu")

for (coluna in colunas_s_n_ns){
dados[[coluna]] <-factor(dados[[coluna]], levels = ordem_sim_nao)
}

gerar_graficos_ns <- function(dados){
  for (coluna in colunas_s_n_ns) {
    # Criar gráfico de barras para a coluna atual
    p <- ggplot(dados, aes_string(x = coluna, fill = coluna)) +
      geom_bar(stat = "count") + 
      scale_fill_manual (values = cor_sim_nao)+
      labs(title = "",
           x = titulos_ns[coluna],
           y = "Contagem") +
      guides(fill = guide_legend(title = "Legenda")) +
      theme_minimal() +
      theme( panel.background = element_rect(fill = "gray90"),
             plot.background = element_rect (fill ="gray90"),
             axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
             axis.title.x = element_blank(),
             axis.text.x = element_text(size = 12, color = "#222"),
             axis.text.y = element_text(size = 12, color = "#222"),
             legend.title = element_text(face = "bold", size = 10),
             legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
             legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
             plot.margin = unit(c(0, 0, 0, 0), "cm"),
             panel.spacing = unit(0, "lines"),
             axis.ticks.y = element_blank(),
             panel.border = element_blank())
    # Exibir o gráfico
    print(p)
    
    # Definir o caminho completo para salvar o gráfico
    caminho_completo <- paste0(
      "C:/Users/Graça/Documents/GRAFICOS_TESTE_eiv_pedro/",
      coluna, ".png"  # Nome do arquivo baseado na coluna
    )
    
    # Salvar o gráfico
    ggsave(caminho_completo, plot = p, width = 10, height = 6, dpi = 300)
  }

}
gerar_graficos_ns(dados)

########### Grafico local de problemas
colunas_onde <- c("onde_depo",	"onde_polu_v",	"onde_polu",	"onde_drenag",	"onde_imovel")

titulos_onde <- c("onde_depo" = "Local de deposito de lixo irregular",
                  "onde_polu_v" = "Local de poluição visual",
                  "onde_polu" = "Local de poluição sonora",
                  "onde_drenag" = "Local sem drenagem de água",
                  "onde_imovel" = "Local com imoveis abandonados")

cor_locais <- c('#1a365d', '#2d543d','#b35900')

gerar_graficos_problemas <- function(dados){
  for (coluna in colunas_onde){
    
    dados_filtrados <- dados %>%
      filter(!!sym(coluna) != "NSA") %>%  # Filtrar valores "NSA"
      group_by(!!sym(coluna)) %>%         # Agrupar pela coluna atual
      summarise(n = n()) %>%              # Contar a frequência
      arrange(desc(n)) %>%                # Ordenar em ordem decrescente
      slice_head(n = 3)                   # Manter apenas os 3 primeiros
    
    
    p <- ggplot(dados_filtrados, aes(x = !!sym(coluna), y = n, fill = !!sym(coluna))) +
      geom_bar(stat = "identity") +  # Barras para mostrar as frequências
      scale_fill_manual(values = cor_locais)+
      geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +  # Rótulos das barras
      labs(
        title = "",  # Título do gráfico
        x = "Categoria",               # Rótulo do eixo x
        y = "Frequência"               # Rótulo do eixo y
      ) +
      guides(fill = guide_legend(title = "Categorias")) +  # Legenda
      theme_minimal() +
      theme( panel.background = element_rect(fill = "gray90"),
             plot.background = element_rect (fill ="gray90"),
             axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
             axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_text(size = 12, color = "#222"),
             legend.title = element_text(face = "bold", size = 10),
             legend.text = element_text(size = 12),     # Diminui o tamanho do texto da legenda
             legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
             plot.margin = unit(c(0, 0, 0, 0), "cm"),
             panel.spacing = unit(0, "lines"),
             axis.ticks.y = element_blank(),
             panel.border = element_blank())+
      coord_cartesian(ylim = c(0, max(dados_filtrados$n) + 3))
    # Exibir o gráfico
    print(p)
    
    # Definir o caminho completo para salvar o gráfico
    caminho_completo <- paste0(
      "C:/Users/Graça/Documents/GRAFICOS_TESTE_eiv_pedro/",
      coluna, ".png"  # Nome do arquivo baseado na coluna
    )
    
    # Salvar o gráfico
    ggsave(caminho_completo, plot = p, width = 10, height = 6, dpi = 300)
  }
}


gerar_graficos_problemas(dados)


###Grafico conhece o local
conhece_local <- dados%>%
  group_by(conhc_loc)%>%
  summarise(n = n())
  
conhece_local$conhc_loc <-factor(conhece_local$conhc_loc, levels = ordem_sim_nao)

conhece <- ggplot(conhece_local, aes(x = conhc_loc, y = n, fill = conhc_loc)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Conhece o local ?", y = "Frequência") +
  guides(fill = guide_legend(title = "Conhece o local")) +
  scale_fill_manual(values = cor_sim_nao)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(conhece_local$n) + 10))

salvar_grafico_dir(grafico = conhece,
                   nome_arquivo = "Conhece o lugar")

###### Grafico avaliação do uso da area

dados[] <- lapply(dados, function(x) {
  if(is.character(x)) {
    x[tolower(x) == "positiva"] <- "Positiva"
    x[tolower(x) == "ambos"] <- "Ambos"
    x[tolower(x) == "indiferente"] <- "Indiferente"
    x[tolower(x) == "negativa"] <- "Negativa"
  }
  x
})


cores_avaliacao_area <- c("Positiva" = '#5773CC',
                          "Ambos" = '#FFB900',
                          "Negativa" = '#8B0000',
                          "Indiferente" = '#9370DB')

ordem_avaliações <- c("Positiva","Ambos", "Negativa", "Indiferente")

aval_area <- dados%>%
  group_by(uso_area)%>%
  summarise(n = n())

aval_area$uso_area <- factor(aval_area$uso_area, levels = ordem_avaliações )

aval_area <- na.omit(aval_area)

av_area <- ggplot(aval_area, aes(x = uso_area, y = n, fill = uso_area)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Temas", y = "Frequência") +
  guides(fill = guide_legend(title = "Temas")) +
  scale_fill_manual(values = cores_avaliacao_area)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(aval_area$n) + 5))

salvar_grafico_dir(grafico = av_area,
                   nome_arquivo = "Avaliação da área")

##### Graficos aspectos positivos
cores_aspect_pos <- c("#008B8B", "#2E8B57", "#FF7F50")

Aspectos_positivos <- dados %>%
  filter(aspec_pos != "NR") %>%  # Filtrar valores "NSA"
  group_by(aspec_pos) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) ##%>%                # Ordenar em ordem decrescente
  ## slice_head(n = 3) 

### Salvando tabela
write_xlsx(Aspectos_positivos, path = file.path(caminho_tab, "Aspectos Positivos.xlsx"))

positivos <- ggplot(Aspectos_positivos, aes(x = aspec_pos, y = n, fill = aspec_pos)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Tema", y = "Frequência") +
  scale_fill_manual( values = cores_aspect_pos )+
  guides(fill = guide_legend(title = "Temas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(Aspectos_positivos$n) + 2))

salvar_grafico_dir(grafico = positivos,
                   nome_arquivo = "Aspectos positivos")

##### Graficos aspectos negativos
cores_aspec_neg <- c("#800000",  "#A0522D",  "#778899")

Aspectos_negativos <- dados %>%
  filter(apes_neg != "NR") %>%  # Filtrar valores "NSA"
  group_by(apes_neg) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) ##%>%                # Ordenar em ordem decrescente
  ##slice_head(n = 3) 

### Salvando tabela
write_xlsx(Aspectos_negativos, path = file.path(caminho_tab, "Aspectos Negativos.xlsx"))


negativos <- ggplot(Aspectos_negativos, aes(x = apes_neg, y = n, fill = apes_neg)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Tema", y = "Frequência") +
  scale_fill_manual(values = cores_aspec_neg)+
  guides(fill = guide_legend(title = "Temas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(Aspectos_negativos$n) + 2))

salvar_grafico_dir(grafico = negativos,
                   nome_arquivo = "Aspectos negativos")

###### Gráfico de uso do local
  
ordem_sim_nao_NR <- c("Sim", "Não", "NR")

Algm_uso_local <- dados %>%
  group_by(uso_loc)%>%
  summarise(n = n())

Algm_uso_local$uso_loc<- factor(Algm_uso_local$uso_loc, levels = ordem_sim_nao_NR)

usa_local <- ggplot(Algm_uso_local, aes(x = uso_loc, y = n, fill = uso_loc)) +
  geom_bar(stat = "identity", width = 0.7) +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "", y = "Frequência") +
  scale_fill_manual(values = cor_sim_nao)+
  guides(fill = guide_legend(title = "Respostas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(Algm_uso_local$n) + 5))

salvar_grafico_dir(grafico = usa_local,
                   nome_arquivo = "Alguem usa o local")

####### Gráfico de qual uso do local (Escolher cores)

cores_uso <- c("#E41A1C", "#FF7F00","#4DAF4A") 

Qual_uso <- dados %>%
  filter(qual_uso != "NSA" & qual_uso != "NR") %>%  # Filtrar valores "NSA"
  group_by(qual_uso) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) %>%                # Ordenar em ordem decrescente
  slice_head(n = 3) 

qual <- ggplot(Qual_uso, aes(x = qual_uso, y = n, fill = qual_uso)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Usos", y = "Frequência") +
  scale_fill_manual(values = cores_uso)+
  guides(fill = guide_legend(title = "Tipos de uso")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(Qual_uso$n) + 2))

salvar_grafico_dir(grafico = qual,
                   nome_arquivo = "Qual uso")

###Grafico ouviu falar do empreendimento

proposta <- dados%>%
  group_by(prop_empr)%>%
  summarise(n = n())

proposta$prop_empr<- factor(proposta$prop_empr, levels = ordem_sim_nao )

propostas_empreendimento <- ggplot(proposta, aes(x = prop_empr, y = n, fill = prop_empr)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Conhece o local ?", y = "Frequência") +
  guides(fill = guide_legend(title = "Conhece a proposta?")) +
  scale_fill_manual(values = cor_sim_nao)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(conhece_local$n) + 10))

salvar_grafico_dir(grafico = propostas_empreendimento,
                   nome_arquivo = "Proposta para o empreendimento")

######Grafico mudanças com o empreendimento (Escolher cores)

mudanca <- dados%>%
  group_by(mud_empre)%>%
  summarise(n = n())

mudanca$mud_empre<- factor(mudanca$mud_empre, levels = ordem_sim_nao )

mudancas_empreendimento <- ggplot(mudanca, aes(x = mud_empre, y = n, fill = mud_empre)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Mudanças na região?", y = "Frequência") +
  guides(fill = guide_legend(title = "Mudanças na região")) +
  scale_fill_manual(values = cor_sim_nao)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(conhece_local$n) + 10))

salvar_grafico_dir(grafico = mudancas_empreendimento,
                   nome_arquivo = "Mudancas Empreendimento")

#######Grafico mudanças positivos empreendimento (Escolher cores)

cores_mud_pos <- c("Atração de novas unidades de comércio e serviço" = "#FF8C00",
           "Valorização dos imóveis" = "#FFD700",
           "Embelezamento" = "#33A02C",
           "Melhoria da circulação viária" = "#5F9EA0",
           "Aumento da acessibilidade para pedestres" = "#BDB76B",
           "Eliminação das áreas de riscos existentes" = "#B22222",
           "Aumento da segurança da população" = "#D8BFD8",
           "Aumento do controle sobre atividades ilicitas" = "#F0E68C",
           "Melhoria da condição de saneamento" = "#377EB8")

dados_empilhados_pos <- dados %>%
  separate_rows(mud_pos, sep = ",\\s*")

##Ajeitando a ortografia
dados_empilhados_pos[] <- lapply(dados_empilhados_pos, function(x) {
  if(is.character(x)) {
    x[tolower(x) == "atracao_novas_unis_comer_ser"] <- "Atração de novas unidades de comércio e serviço"
    x[tolower(x) == "valorizacao_imo"] <- "Valorização dos imóveis"
    x[tolower(x) == "embelezamento"] <- "Embelezamento"
    x[tolower(x) == "melhoria_circu_viaria"] <- "Melhoria da circulação viária"
    x[tolower(x) == "aumento_acess_pedestres"] <- "Aumento da acessibilidade para pedestres"
    x[tolower(x) == "eliminacao_areas_risco_exis"] <- "Eliminação das áreas de riscos existentes"
    x[tolower(x) == "aumento_segu_pop"] <- "Aumento da segurança da população"
    x[tolower(x) == "aumento_control_ativi_ilicitas"] <- "Aumento do controle sobre atividades ilicitas"
    x[tolower(x) == "melhoria_cond_sanea"] <- "Melhoria da condição de saneamento"
  }
  x
})

mudancas_pos<- dados_empilhados_pos  %>%
  filter(mud_pos != "NSA" & mud_pos != "NR" & mud_pos != "Outro") %>%  # Filtrar valores "NSA"
  group_by(mud_pos) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) %>%                # Ordenar em ordem decrescente
  slice_head(n = 3) 

mudancas_positivas <- ggplot(mudancas_pos, aes(x = mud_pos, y = n, fill = mud_pos)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Usos", y = "Frequência") +
  scale_fill_manual(values = cores_mud_pos)+
  guides(fill = guide_legend(title = "Mudanças positivas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(mudancas_pos$n) + 2))

salvar_grafico_dir(grafico = mudancas_positivas,
                   nome_arquivo = "Mudanças positivas")

############## Potencializador de medidas positivas(EScolher cores)

cores_potenc_pos <- c("#4DAF4A","#377EB8","#FFFACD" )

potencial_pos<- dados_empilhados_pos %>%
  filter(potenc_pos != "NSA" & potenc_pos != "NR" & potenc_pos != "Outro") %>%  # Filtrar valores "NSA"
  group_by(potenc_pos) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) %>%                # Ordenar em ordem decrescente
  slice_head(n = 3) 

potencializador_positivo <- ggplot(potencial_pos, aes(x = potenc_pos, y = n, fill = potenc_pos)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "", y = "Frequência") +
  scale_fill_manual(values = cores_potenc_pos, labels = c("A prefeitura deve usar o dinheiro\npara melhorar as condições do bairro",
   "Aumentar policiamento","Maior divulgação do espaço,\nnem todo mundo sabeque não é particular"))+
  guides(fill = guide_legend(title = "Potencializadores")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(potencial_pos$n) + 2))

salvar_grafico_dir(grafico = potencializador_positivo,
                   nome_arquivo = "Potencializador de medidas positivas")

#############Grafico mudanças negativas empreendimento (EScolher cores)

cores_mud_neg <- c("Aumento do trânsito de veículos dentro da Vila para cortar caminho" = "#2F4F4F",
                   "Aumento da poluição sonora – mais carros" = '#9932CC',
                   "Insegurança quanto ao futuro" = '#FF0000',
                   "Transtornos durante o período de obras" = '#FF8C00',
                   "Aumento da poluição do ar - carros" = "#9370DB",
                   "Especulação imobiliária na Vila" = "#8B4513",
                   "Alteração da paisagem existente" = "#808000",
                   "Não se aplica")

dados_empilhados_neg <- dados %>%
  separate_rows(mud_neg, sep = ",\\s*")

##Ajeitando a ortografia 
dados_empilhados_neg[] <- lapply(dados_empilhados_neg, function(x) {
  if(is.character(x)) {
    x[tolower(x) == "inseguranca_futuro"] <- "Insegurança quanto ao futuro"
    x[tolower(x) == "transt_periodo_obras"] <- "Transtornos durante o período de obras"
    x[tolower(x) == "aumento_veicu_vila"] <- "Aumento do trânsito de veículos dentro da Vila para cortar caminho"
    x[tolower(x) == "aumento_polui_sonora"] <- "Aumento da poluição sonora – mais carros"
    x[tolower(x) == "aumento_da_poluiçao_do_ar "] <- "Aumento da poluição do ar - carros"
    x[tolower(x) == "especulacao_imob_vila"] <- "Especulação imobiliária na Vila"
    x[tolower(x) == "alteracao_paisag_existente"] <- "Alteração da paisagem existente"
    x[tolower(x) == "nao_se_aplica"] <- "Não se aplica"
  }
  x
})


mudancas_neg<- dados_empilhados_neg %>%
  filter(mud_neg != "NSA" & mud_neg != "NR" & mud_neg != "Outro") %>%  # Filtrar valores "NSA"
  group_by(mud_neg) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) %>%                # Ordenar em ordem decrescente
  slice_head(n = 3) 

mudancas_negativas <- ggplot(mudancas_neg, aes(x = mud_neg, y = n, fill = mud_neg)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Usos", y = "Frequência") +
  scale_fill_manual(values = cores_mud_neg, labels = c("Aumento do trânsito de veículos dentro\nda Vila para cortar caminho",
   "Aumento da poluição sonora\n– mais carros","Transtorno durante o o período de obras" ))+
  guides(fill = guide_legend(title = "Mudanças negativas")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(mudancas_neg$n) + 2))

salvar_grafico_dir(grafico = mudancas_negativas,
                   nome_arquivo = "Mudanças negativas")

################ Mitigador de medidas negativas

cores_mitg_neg <- c('#003366','#D3D3D3','#228B22')

mitigacao_neg<- dados_empilhados_neg %>%
  filter(mitg_neg != "NSA" & mitg_neg != "NR" & mitg_neg != "Outro") %>%  # Filtrar valores "NSA"
  group_by(mitg_neg) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) %>%                # Ordenar em ordem decrescente
  slice_head(n = 3) 

mitigacao_negativa <- ggplot(mitigacao_neg, aes(x = mitg_neg, y = n, fill = mitg_neg)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Usos", y = "Frequência") +
  scale_fill_manual(values = cores_mitg_neg, labels = c("Ampliação das vias, atendimento a saúde\n,escolas",
                      "Construção de áreas verdes, abertura de vias\npúblicas e fiscalização dos competentes", "Não sabe"))+
  guides(fill = guide_legend(title = "Mitigadores")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(mitigacao_neg$n) + 2))

salvar_grafico_dir(grafico = mitigacao_negativa,
                   nome_arquivo = "Mitigação de medidas negativas")

################ Avaliação positiva ou negativa de temas

colunas_prob <- c("impl_seg",	"impl_emp",	"impl_saud",	"impl_educ",	"impl_som",	"impl_ar",	"impl_comerc",	"impl_transp",	"impl_transt",	"impl_ambie",	"impl_vizinho",	"impl_imovel")

###Ajeitando a ortografia
dados[] <- lapply(dados, function(x) {
  if(is.character(x)) {
    x[tolower(x) == "positivos"] <- "Positivos"
    x[tolower(x) == "negativos"] <- "Negativos"
    x[tolower(x) == "nao_altera"] <- "Não altera"
    x[tolower(x) == "nao_se_aplica"] <- "Não se aplica"
  }
  x
})

ordem_pos_neg <- c("Positivos", "Negativos", "Não altera", "Não se aplica")

for (coluna in colunas_prob){
  dados[[coluna]] <-factor(dados[[coluna]], levels = ordem_pos_neg)
}

cor_pos_neg <- c("Positivos" = '#006a8e',
                 "Negativos" = '#b1283a',
                 "Não altera" = '#a8a6a7',
                 "Não se aplica" = '#1C1C1C')

gerar_graficos_pos_neg <- function(dados){
  for (coluna in colunas_prob) {
    
    dados_filtrados <- dados %>% 
      group_by(!!sym(coluna)) %>%         # Agrupar pela coluna atual
      summarise(n = n())              # Contar a frequência
      
    p <- ggplot(dados_filtrados, aes(x = !!sym(coluna), y = n, fill = !!sym(coluna)))+
                  geom_bar(stat = "identity", na.rm = FALSE) +  # Barras para mostrar as frequências
                  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +  # Rótulos das barras
                  labs(
                    title = "", 
                    x = "Categoria",               # Rótulo do eixo x
                    y = "Frequência"               # Rótulo do eixo y
                  ) +
                  guides(fill = guide_legend(title = "Categorias")) +  # Legenda
                  scale_fill_manual(values = cor_pos_neg)+
                  theme_minimal() +
                  theme( panel.background = element_rect(fill = "gray90"),
                         plot.background = element_rect (fill ="gray90"),
                         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
                         axis.title.x = element_blank(),
                         axis.text.x = element_text(size = 8, color = "#222"),
                         axis.text.y = element_text(size = 8, color = "#222"),
                         legend.title = element_text(face = "bold", size = 10),
                         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
                         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
                         plot.margin = unit(c(0, 0, 0, 0), "cm"),
                         panel.spacing = unit(0, "lines"),
                         axis.ticks.y = element_blank(),
                         panel.border = element_blank())
    # Exibir o gráfico
    print(p)
    
    # Definir o caminho completo para salvar o gráfico
    caminho_completo <- paste0(
      "C:/Users/Graça/Documents/GRAFICOS_TESTE_eiv_pedro/",
      coluna, ".png"  # Nome do arquivo baseado na coluna
    )
    
    # Salvar o gráfico
    ggsave(caminho_completo, plot = p, width = 10, height = 6, dpi = 300)
  }
}
gerar_graficos_pos_neg (dados)

########### Grafico valorização imovel

###Ajeitando a ortografia
dados[] <- lapply(dados, function(x) {
  if(is.character(x)) {
    x[tolower(x) == "valorizacao"] <- "Valorização"
    x[tolower(x) == "desvalorizacao"] <- "Desvalorização"
    x[tolower(x) == "nao_altera"] <- "Não altera"
    x[tolower(x) == "nao_se_aplica"] <- "Não se aplica"
  }
  x
})

ordem_val <- c("Valorização", "Não altera", "Desvalorização", "Não se aplica")

valorização_imoveis <- dados %>% 
  group_by(val_des_imovel) %>%         # Agrupar pela coluna atual
  summarise(n = n())  

valorização_imoveis$val_des_imovel <-factor(valorização_imoveis$val_des_imovel, levels = ordem_val)

cor_val_imvl <- c("Valorização" = '#006a8e',
                 "Desvalorização" = '#b1283a',
                 "Não altera" = '#a8a6a7',
                 "Não se aplica" = '#1C1C1C')

valorizacao_imovel <- ggplot(valorização_imoveis, aes(x = val_des_imovel, y = n, fill = val_des_imovel))+
  geom_bar(stat = "identity") +  # Barras para mostrar as frequências
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +  # Rótulos das barras
  labs(
    title = "", 
    x = "Categoria",               # Rótulo do eixo x
    y = "Frequência"               # Rótulo do eixo y
  ) +
  guides(fill = guide_legend(title = "Categorias")) +  # Legenda
  scale_fill_manual(values = cor_val_imvl)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())

salvar_grafico_dir(grafico = valorizacao_imovel,
                   nome_arquivo = "Valorização dos imóveis")

############# Grafico do por que valoriza o imovel (Duvidas: retirar o não sabe? Escolher cores)

###Ajeitando ortografia
dados[] <- lapply(dados, function(x) {
  if(is.character(x)) {
    x[tolower(x) == "atracao_resid_reg"] <- "Atração de uso residencial para a região"
    x[tolower(x) == "ponto_de_referencia"] <- "Ponto de referência"
    x[tolower(x) == "aum_oferta_com_ser"] <- "Aumento da oferta de comércio/ serviços"
    x[tolower(x) == "maior_visib_regiao"] <- "Maior visibilidade/valorização da região"
    x[tolower(x) == "maior_movi_pes_ruas"] <- "Maior movimentação de pessoas nas ruas"
    x[tolower(x) == "aumento_do_transito"] <- "Aumento do trânsito"
    x[tolower(x) == "aumento_poluiçao_sonora_ou_ar"] <- "Aumento da poluição sonora/ ar"
    x[tolower(x) == "maior_insegurança"] <- "Maior insegurança"
    x[tolower(x) == "maior_segurança"] <- "Maior segurança"
    x[tolower(x) == "projeto_planejado_para_funcao"] <- "Projeto planejado para a função"
    x[tolower(x) == "projeto_arquitetonico_de_qualidade"] <- "Projeto arquitetônico de qualidade (bonito)"
    x[tolower(x) == "empreendedor_experiente"] <- "Empreendedor experiente"
    x[tolower(x) == "outro"] <- "Outro"
    x[tolower(x) == "nao_se_aplica"] <- "Não se aplica"
  }
  x
})

motivo_val<- dados %>%
  filter(motiv_imovel != "NSA" & motiv_imovel != "NR" & motiv_imovel != "Outro") %>%  # Filtrar valores "NSA"
  group_by(motiv_imovel) %>%         # Agrupar pela coluna atual
  summarise(n = n()) %>%              # Contar a frequência
  arrange(desc(n)) %>%                # Ordenar em ordem decrescente
  slice_head(n = 3)

cor_valorizacao <- c("Atração de uso residencial para a região" = "#DAA520",
                     "Maior visibilidade/valorização da região" = "#FFD700",
                     "Ponto de referência" = '#6495ED',
                     "Aumento da oferta de comércio/ serviços" = "#FFFF00",
                     "Maior movimentação de pessoas nas ruas" = '#FF7F50',
                     "Aumento do trânsito" = '#708090',
                     "Aumento da poluição sonora/ ar" = '#9370DB',
                     "Maior insegurança" = '#FFE4B5',
                     "Maior segurança" = '#808000',
                     "Projeto planejado para a função" = '#836FFF',
                     "Projeto arquitetônico de qualidade (bonito)" = '#00BFFF',
                     "Empreendedor experiente" = '#00FA9A',
                     "Outro" = '#A9A9A9',
                     "Não sabe" = "#4F4F4F")

motivos_valorizacao <- ggplot(motivo_val, aes(x = motiv_imovel, y = n, fill = motiv_imovel)) +
  geom_bar(stat = "identity") +              # Barras para mostrar as frequências
  geom_text(aes(label = n),vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "Usos", y = "Frequência") +
  scale_fill_manual(values = cor_valorizacao)+
  guides(fill = guide_legend(title = "Motivos")) +
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(motivo_val$n) + 2))

salvar_grafico_dir(grafico = motivos_valorizacao,
                   nome_arquivo = "Motivos para valorização dos imóveis")

################ Gráfico interferencia na paisagem urbana

interferencia_paisagem <- dados %>%
  group_by(intef_paisg)%>%
  summarise(n = n())

cor_sim_nao_nsa_nr <- c("Sim" = '#006a8e',
                        "Não" = '#b1283a',
                        "Não sabe / Não respondeu" = '#a8a6a7',
                        "Não se aplica" = '#1C1C1C' )

ordem_sim_nao_NR_NSA <- c("Sim", "Não", "Não sabe / Não respondeu", "Não se aplica")

interferencia_paisagem$intef_paisg <-factor(interferencia_paisagem$intef_paisg, levels = ordem_sim_nao_NR_NSA )

paisagem <- ggplot(interferencia_paisagem, aes(x = intef_paisg, y = n, fill = intef_paisg))+
  geom_bar(stat = "identity",width = 0.5 ) +  # Barras para mostrar as frequências
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +  # Rótulos das barras
  guides(fill = guide_legend(title = "Categorias")) +  # Legenda
  labs(
    title = "", 
    x = "Categoria",               # Rótulo do eixo x
    y = "Frequência"               # Rótulo do eixo y
  ) +
  scale_fill_manual(values = cor_sim_nao_nsa_nr)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(interferencia_paisagem$n) + 5))

salvar_grafico_dir(grafico = paisagem,
                   nome_arquivo = "Inteferencia paisagem")

################ Grafico vai fazer uso do equipamento

uso_equipamento <- dados %>%
  group_by(uso_emprend)%>%
  summarise(n = n())

uso_equipamento$uso_emprend <-factor(uso_equipamento$uso_emprend, levels = ordem_sim_nao )

equipamento <- ggplot(uso_equipamento, aes(x = uso_emprend, y = n, fill = uso_emprend))+
  geom_bar(stat = "identity", width = 0.5) +  # Barras para mostrar as frequências
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +  # Rótulos das barras
  guides(fill = guide_legend(title = "Categorias")) +  # Legenda
  labs(
    title = "", 
    x = "Categoria",               # Rótulo do eixo x
    y = "Frequência"               # Rótulo do eixo y
  ) +
  scale_fill_manual(values = cor_sim_nao)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(interferencia_paisagem$n) + 5))

salvar_grafico_dir(grafico = equipamento,
                   nome_arquivo = "Uso do equipamento")

############### AValiação geral empreendimento

av_emprend <- dados %>%
  filter(emprend != "NA")%>%
  group_by(emprend)%>%
  summarise(n = n())

ordem_av_emrend <- c("Positiva", "Negativa", "Ambos", "Indiferente", "Não se aplica") 

cores_emprend <- c("Positiva" = '#006400',
                   "Negativa" = '#B22222',
                   "Ambos" = '#FF8C00',
                   "Indiferente" = '#4169E1',
                   "Não se aplica" = '#a8a6a7')

av_emprend$emprend <-factor(av_emprend$emprend, levels = ordem_av_emrend )

avaliacao_empreendimento <- ggplot(av_emprend, aes(x = emprend, y = n, fill = emprend))+
  geom_bar(stat = "identity") +  # Barras para mostrar as frequências
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +  # Rótulos das barras
  labs(
    title = "", 
    x = "Categoria",               # Rótulo do eixo x
    y = "Frequência"               # Rótulo do eixo y
  ) +
  guides(fill = guide_legend(title = "Categorias")) +  # Legenda
  scale_fill_manual(values = cores_emprend)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())

salvar_grafico_dir(grafico = avaliacao_empreendimento,
                   nome_arquivo = "Avaliação do empreendimento")

################ Tem interesse em participar das reuniões

interesse_reunioes <- dados %>%
  group_by(x)%>%
  summarise(n = n())

interesse_reunioes$x <-factor(interesse_reunioes$x, levels = ordem_sim_nao )

interesse <- ggplot(interesse_reunioes, aes(x = x, y = n, fill = x))+
  geom_bar(stat = "identity", width = 0.5) +  # Barras para mostrar as frequências
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +  # Rótulos das barras
  guides(fill = guide_legend(title = "Categorias")) +  # Legenda
  labs(
    title = "", 
    x = "Categoria",               # Rótulo do eixo x
    y = "Frequência"               # Rótulo do eixo y
  ) +
  scale_fill_manual(values = cor_sim_nao)+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "gray90"),
         plot.background = element_rect (fill ="gray90"),
         axis.title.y = element_text(face = "bold", size = 10, margin = margin(l = 5, r = 2)),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 8, color = "#222"),
         axis.text.y = element_text(size = 8, color = "#222"),
         legend.title = element_text(face = "bold", size = 10),
         legend.text = element_text(size = 7),     # Diminui o tamanho do texto da legenda
         legend.key.size = unit(0.5, "cm"),         # Diminui o tamanho das caixas de cores da legenda
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.spacing = unit(0, "lines"),
         axis.ticks.y = element_blank(),
         panel.border = element_blank())+
  coord_cartesian(ylim = c(0, max(interesse_reunioes$n) + 5))

salvar_grafico_dir(grafico = interesse,
                   nome_arquivo = "Interesse em reuniões")
