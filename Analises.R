library(tidyverse)
library(readxl)


#Incluir base e dicionário
base = read_excel("Base_trabalho.xlsx")
dicionario = read_excel("dicionario_Base_trabalho.xlsx")

#Transformar qualitativas em fatores 
base <- base |> 
  mutate(
    sexo = factor(sexo, levels = c(0,1), labels = c("Feminino", "Masculino")),
    reincidente = factor(reincidente, levels = c(0,1), labels = c("Nao", "Sim")),
    filhos = factor(filhos, levels = c(0,1), labels = c("Nao", "Sim")),
    casado = factor(casado, levels = c(0,1), labels = c("Nao", "Sim")),
    escolaridade = factor(escolaridade,
                          levels = c(1,2,3),
                          labels = c("Fundamental","Medio","Superior"))
  )
summary(base)


#(b) Análise sobre dados faltantes
#Checar linhas
linhas_com_na <- base |>  filter(if_any(everything(), is.na))
nrow(linhas_com_na)

#(c) Histograma da variável idade
p_hist_idade <- ggplot(base, aes(x = idade)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue") +
  labs(title = "Histograma: Idade dos detentos",
       x = "Idade (anos)", y = "Frequência") +
  theme_minimal()
print(p_hist_idade)


#(d) Boxplot da variável tempo_preso
p_box_tempo <- ggplot(base, aes(y = tempo_preso)) +
  geom_boxplot(outlier.shape = 21, fill = "lightgreen") +
  labs(title = "Boxplot: Tempo preso (meses)",
       y = "Tempo preso (meses)") +
  theme_minimal()
print(p_box_tempo)


#(e) Boxplot do score_periculosidade por escolaridade
p_box_score_escola <- ggplot(base, aes(x = escolaridade, y =
                                         score_periculosidade)) +
  geom_boxplot() +
  labs(title = "Score de Periculosidade por Escolaridade",
       x = "Escolaridade", y = "Score de Periculosidade") +
  theme_minimal()
print(p_box_score_escola)


#(f) Gráfico de barras para reincidente
p_bar_reinc <- ggplot(base, aes(x = reincidente)) +
  geom_bar() +
  labs(title = "Contagem de Reincidentes",
       x = "Reincidente", y = "Contagem") +
  theme_minimal()  
print(p_bar_reinc)

#(g) Salvar figuras
dir.create("figuras", showWarnings = FALSE)

ggsave("figuras/hist_idade.png", p_hist_idade, width = 7, height = 5, dpi = 300)
ggsave("figuras/box_tempo_preso.png", p_box_tempo, width = 6, height = 5, dpi =
         300)
ggsave("figuras/box_score_escolaridade.png", p_box_score_escola, width = 7,
       height = 5, dpi = 300)
ggsave("figuras/bar_reincidente.png", p_bar_reinc, width = 5, height = 4, dpi = 300)

