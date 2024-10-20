library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(tibble)
library(tidyr)
library(readxl)

c1 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Athina")
c2 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "London")
c3 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Rio de Janeiro")
c4 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Sydney")
c5 <- read_excel("~/Olimpiadas 2000 - 2016.xlsx", sheet = "Beijing")


colnames(c2) <- colnames(c1)  
colnames(c3) <- colnames(c1)  
colnames(c4) <- colnames(c1)  
colnames(c5) <- colnames(c1)  


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
ggsave("grafico1.jpg", width = 158, height = 93, units = "mm")


# análise 2
banco2=banco
colnames(banco2)[5] <- "peso"

colnames(banco2)[4] <- "altura"

banco2= filter(banco2, !is.na(peso))
banco2= filter(banco2, !is.na(altura))

banco2$peso_kg <- banco2$peso * 0.453592
banco2$altura_m <- banco2$altura/100

# Calcular o IMC
banco2$IMC <- banco2$peso_kg / (banco2$altura_m^2)

resultado2 <- banco2[banco2$Sport %in% c("Gymnastics", "Soccer", "Judo", "Athletics", "Badminton"), ]


resultado2$Sport <- gsub("Gymnastics", "Ginástica", resultado2$Sport)
resultado2$Sport <- gsub("Goccer", "Futebol", resultado2$Sport)
resultado2$Sport <- gsub("Judo", "Judô", resultado2$Sport)
resultado2$Sport <- gsub("Athletics", "Atletismo", resultado2$Sport)


estatisticas_imc <- resultado2 %>%
  group_by(Sport) %>%
  summarise(media_IMC = mean(IMC, na.rm = TRUE),
            desvio_padrao_IMC = sd(IMC, na.rm = TRUE))



ggplot(resultado2) +
  aes(x = reorder(Sport, IMC, FUN = median), y = IMC) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Esportes", y = "IMC") +
  theme_estat()


ggsave("grafico2.jpg", width = 158, height = 93, units = "mm")

print_quadro_resumo <- function(data, var_name, title = "Medidas resumo da(o) [nome da variável]", label = "quad:quadro_resumo1") {
  var_name <- substitute(var_name)  # Captura o nome da variável
  data <- data %>%
    summarize(
      `Média` = round(mean(!!sym(var_name), na.rm = TRUE), 2),
      `Desvio Padrão` = round(sd(!!sym(var_name), na.rm = TRUE), 2),
      `Variância` = round(var(!!sym(var_name), na.rm = TRUE), 2),
      `Mínimo` = round(min(!!sym(var_name), na.rm = TRUE), 2),
      `1º Quartil` = round(quantile(!!sym(var_name), probs = 0.25, na.rm = TRUE), 2),
      `Mediana` = round(quantile(!!sym(var_name), probs = 0.5, na.rm = TRUE), 2),
      `3º Quartil` = round(quantile(!!sym(var_name), probs = 0.75, na.rm = TRUE), 2),
      `Máximo` = round(max(!!sym(var_name), na.rm = TRUE), 2)
    ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]\n",
                 "\\caption{", title, "}\n",
                 "\\centering\n",
                 "\\begin{adjustbox}{max width=\\textwidth}\n",
                 "\\begin{tabular}{", sep = "")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  
  latex <- str_c(latex, "| l |\n", sep = "")
  
  for (i in seq(2, col_count)) {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      { floor(log10(.)) + 1 } %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount, ".2]\n", sep = "")
  }
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep = "")
  
  if (col_count > 2) {
    for (i in seq(1, col_count)) {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep = "")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep = "")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep = "")
      else
        latex <- str_c(latex, "\\\\\n", sep = "")
    }
  } else {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep = "")
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep = "")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count)) {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i, ]), collapse = " & "), " \\\\\n")
  }
  
  latex <- str_c(latex, "\t\\bottomrule\n\n",
                 "\t\\end{tabular}\n",
                 "\t\\label{", label, "}\n",
                 "\t\\end{adjustbox}\n",
                 "\\end{quadro}", sep = "")
  
  writeLines(latex)  # Escreve o código LaTeX na saída
}

resultado2 %>%
  group_by(Sport) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = IMC)

```{r, results='asis'}

tabela_latex <- "
\\begin{quadro}[H]
\\caption{Medidas resumo da(o) [nome da variável]}
\\centering
\\begin{adjustbox}{max width=\\textwidth}
\\begin{tabular}{| l |
			S[table-format = 2.2]
			S[table-format = 1.2]
			S[table-format = 2.2]
			S[table-format = 2.2]
			|}
	\\toprule
		\\textbf{Estatística}& \\textbf{Atletismo}& \\textbf{Badminton}& \\textbf{Ginástica}& \\textbf{Judô}\\\\
		\\midrule
		Média & 22.03 & 22.39 & 21.15 & 25.55 \\\\
		Desvio Padrão & 3.77 & 1.78 & 2.30 & 5.12 \\\\
		Variância & 14.23 &  3.18 &  5.29 & 26.20 \\\\
		Mínimo & 14.98 & 16.90 & 14.08 & 17.58 \\\\
		1º Quartil & 19.61 & 21.22 & 19.57 & 22.28 \\\\
		Mediana & 21.20 & 22.30 & 21.36 & 24.41 \\\\
		3º Quartil & 23.34 & 23.31 & 22.77 & 27.41 \\\\
		Máximo & 44.38 & 31.14 & 30.82 & 63.90 \\\\
	\\bottomrule
	\\end{tabular}
	\\label{quad:quadro_resumo1}
	\\end{adjustbox}
\\end{quadro}"

cat(tabela_latex)
```

```{r}
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
library(grid)

# Criando a tabela com os dados (substitua os dados reais)
quadro_resumo1 <- data.frame(
  Estatística = c("Média", "Desvio Padrão", "Variância", "Mínimo", "1º Quartil", "Mediana", "3º Quartil", "Máximo"),
  Atletismo = c(22.03, 3.77, 14.23, 14.98, 19.61, 21.20, 23.34, 44.38),
  Badminton = c(22.39, 1.78, 3.18, 16.90, 21.22, 22.30, 23.31, 31.14),
  Ginástica = c(21.15, 2.30, 5.29, 14.08, 19.57, 21.36, 22.77, 30.82),
  Judô = c(25.55, 5.12, 26.20, 17.58, 22.28, 24.41, 27.41, 63.90)
)

# Criando a tabela com gridExtra
tabela <- tableGrob(quadro_resumo1)

# Salvando a tabela como uma imagem
ggsave("tabela_resumo.png", tabela, width = 8, height = 6)

```
