library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(tibble)
library(tidyr)
library(readxl)

# Carregar planilhas de diferentes sheets em um único dataframe
c1 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Athina")
c2 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "London")
c3 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Rio de Janeiro")
c4 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Sydney")
c5 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Beijing")

# Renomear as colunas manualmente para garantir que sejam iguais
colnames(c2) <- colnames(c1)  # Renomear as colunas de c2 para corresponder às de c1
colnames(c3) <- colnames(c1)  # Renomear as colunas de c3 para corresponder às de c1
colnames(c4) <- colnames(c1)  # Renomear as colunas de c4 para corresponder às de c1
colnames(c5) <- colnames(c1)  # Renomear as colunas de c5 para corresponder às de c1


banco= rbind(c1,c2,c3,c4,c5)

# PADRONIZAÇÃO 
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}
# Analise 1 

banco1= filter(banco, Gender == "F")
banco1= filter(banco1, !is.na(Medal))

resultado1 <- banco1 %>%
  group_by(Team) %>%             
  summarise(total_medalhistas = n()) %>%   
  arrange(desc(total_medalhistas)) %>%    
  head(5)                                 
resultado1$Team <- gsub("Germany", "Alemanha", resultado1$Team)
resultado1$Team <- gsub("United States", "Estados Unidos", resultado1$Team)



ggplot(resultado1, aes(x = reorder(Team, -total_medalhistas), y = total_medalhistas)) +
  geom_bar(stat = "identity", fill = "#A11D21") +
  labs(x = "Países", y = "Número de Medalhistas") +
  theme_estat()
ggsave("grafico1.pdf", width = 158, height = 93, units = "mm")

