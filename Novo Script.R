#novo script
library(tidyverse)
library(readxl)


#Incluir base 
base = read_excel("Base_trabalho.xlsx")


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


#b) Medidas de posição 
resumo_quartis <- base %>%
  summarise(
    media_idade = mean(idade),
    q1_idade = quantile(idade, 0.25),
    mediana_idade = median(idade),
    q3_idade = quantile(idade, 0.75),
    media_tempo = mean(tempo_preso),
    q1_tempo = quantile(tempo_preso, 0.25),
    mediana_tempo = median(tempo_preso),
    q3_tempo = quantile(tempo_preso, 0.75),
    media_score = mean(score_periculosidade),
    q1_score = quantile(score_periculosidade, 0.25),
    mediana_score = median(score_periculosidade),
    q3_score = quantile(score_periculosidade, 0.75))
print(resumo_quartis)


#(c) Gráfico de dispersão entre tempo_preso e score_periculosidade
p_disp <- ggplot(base, aes(x = tempo_preso, y = score_periculosidade)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Dispersão: Tempo Preso x Score de Periculosidade",
    x = "Tempo preso (meses)",
    y = "Score de periculosidade"
  ) +
  theme_minimal()
print(p_disp)


#d) Correlação entre tempo_preso e score_periculosidade
cor_tempo_score <- cor(base$tempo_preso, base$score_periculosidade, use =
                         "complete.obs")
cor_tempo_score


#e) Medidas de dispersão 
resumo_disp <- base |> 
  summarise(
    var_idade = var(idade),
    dp_idade = sd(idade),
    amp_idade = max(idade) - min(idade),
    var_tempo = var(tempo_preso),
    dp_tempo = sd(tempo_preso),
    amp_tempo = max(tempo_preso) - min(tempo_preso),
    var_score = var(score_periculosidade),
    dp_score = sd(score_periculosidade),
    amp_score = max(score_periculosidade) - min(score_periculosidade)
  )
print(resumo_disp)


