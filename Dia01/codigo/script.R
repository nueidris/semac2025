## Carregando pacotes =========================================



pacotes <- c("janitor", "tidyverse", "tidymodels", "vroom", "corrr", "GGally", "rpart.plot")
instalados <- installed.packages()
for (pacote in pacotes) {
  if (!(pacote %in% instalados)) install.packages(pacote)
}

library(tidyverse)
library(tidymodels)
library(janitor)
library(vroom)
library(corrr)
library(GGally)
library(rpart.plot)



## Carregando os dados ========================================



# Leitura inicial
dados <- vroom::vroom(
  "./dados/dados_sujos.csv"
)

# Visualizar a estrutura bruta
dados %>% pillar::glimpse()



## Diagnosticando erros de estrutura ==========================

# Investigando falhas de leitura
dados %>% vroom::problems()


## Intervenção cirúrgica (Strings) ============================



# Lendo como texto puro
linhas <- readr::read_lines("./dados/dados_sujos.csv")
linhas[3]

# Identificando tokens problemáticos com RegEx
tokens <- stringr::str_split(linhas[3], ",")
tokens %>%
  purrr::map(grep, pattern = '(?:(?!").(,|$))+', perl = TRUE) %>%
  purrr::map(~ tokens[[1]][.])

# Corrigindo aspas desbalanceadas na linha 03
linhas[3] <- linhas[3] %>%
  stringr::str_replace('("\\d+\\.\\d+),(\\d+\\.\\d+)', '\\1",\\2')

# Salvando a versão corrigida
linhas %>% readr::write_lines("./dados/dados_corrigidos_01.csv")



## Reimportação e nomes de colunas ============================



# Importando com locale correto
dados <- vroom::vroom(
  "./dados/dados_corrigidos_01.csv",
  # na = "N/A",                       # NÃO USAR!
  locale = locale(decimal_mark = ".") # usa . como separador decimal
)

dados %>% pillar::glimpse()

# Verificando nomes originais (ruins)
dados %>% names()

# Limpando nomes automaticamente (snake_case)
dados <- janitor::clean_names(dados)
dados %>% names()



## Tratando strings "N/A" =====================================



# Substituindo ”N/A” por NA real em todas as colunas de texto
dados <- dados %>%
  mutate(across(where(is_character), ~ na_if(., "N/A")))

# Verificando a mudança
dados %>% pillar::glimpse()



## Convertendo texto para números =============================



# Aplicando parse_number em colunas de texto (exceto id e diagnosis)
dados <- dados %>%
  dplyr::mutate(
    across(
      where(is.character) & !c(id, diagnosis),
      ~ parse_number(
        str_replace_all(.x, ",", "."), # troca "," por "."
      )
    )
  )

dados %>% pillar::glimpse()



## Removendo duplicatas =======================================



# Verificando duplicatas
dados %>% janitor::get_dupes()

# Removendo duplicatas (mantendo apenas linhas distintas)
dados <- dados %>% dplyr::distinct()

# Confirmando a limpeza
dados %>% janitor::get_dupes()



## Padronizando categorias ====================================



# Verificando valores únicos
dados$diagnosis %>% unique()

# Padronizando com case_when
dados <- dados %>%
  dplyr::mutate(
    diagnosis = case_when(
      diagnosis %in% c("M", "m") ~ "M",
      diagnosis %in% c("B", "b", "Benign") ~ "B"
    ),
    diagnosis = forcats::as_factor(diagnosis)
  )

dados$diagnosis %>% unique()



## Imputação de dados faltantes ===============================



# Imputação por mediana por grupo
dados <- dados %>%
  dplyr::group_by(diagnosis) %>%
  dplyr::mutate(across(
    where(is.numeric),
    ~ replace_na(.x, median(.x, na.rm = TRUE))
  )) %>%
  dplyr::ungroup()

dados %>% pillar::glimpse()



## Sumarizando dados ==========================================



dados %>%
  dplyr::select(ends_with("_mean")) %>%
  summary()



## O desafio da visualização em massa =========================



dados %>%
  ggplot(aes(x = smoothness_mean)) +
  geom_histogram(bins = nclass.Sturges(dados$radius_mean)) +
  labs(
    x = "Raio médio",
    y = "Frequência"
  ) +
  theme_classic()



## Tranformação: Wide -> Longo ================================



# Criando um dataset long
dados_longos <- dados %>%
  tidyr::pivot_longer(
    cols = c(matches("_(mean|se|worst)")), # Regra para selecionar colunas
    cols_vary = "slowest", # Coluna com o nome das variaveis
    names_to = "variavel", values_to = "valor" # Coluna com as medidas
  )
dados_longos



## Visualização em massa ======================================



dados_longos %>%
  dplyr::filter(variavel %>% grepl("_mean", x = .)) %>%
  ggplot() +
  geom_histogram(
    aes(x = valor),
    bins = nclass.Sturges(dados$radius_mean), # regra de Sturges
    color = "black", fill = "white"
  ) +
  facet_wrap(~variavel) +
  theme_classic()



## Ajustando as escalas =======================================



dados_longos %>%
  dplyr::filter(grepl("_mean", variavel)) %>%
  ggplot() +
  geom_histogram(
    aes(x = valor),
    bins = nclass.Sturges(dados$radius_mean),
    color = "black", fill = "white"
  ) +
  # A correcao magica:
  facet_wrap(~variavel, scales = "free") +
  theme_classic()



## Distribuição e outliers ====================================



dados_longos %>%
  dplyr::filter(variavel %>% grepl("_mean", x = .)) %>%
  ggplot() +
  geom_boxplot(aes(y = valor)) +
  facet_wrap(~variavel, scales = "free") +
  theme_classic()



## Separação por grupo ========================================



dados_longos %>%
  dplyr::filter(variavel %>% grepl("_mean", x = .)) %>%
  ggplot() +
  # Adicionamos cor baseado no diagnostico
  geom_boxplot(aes(y = valor, color = diagnosis)) +
  facet_wrap(~variavel, scales = "free") +
  labs(color = "Diagnóstico") +
  theme_classic()



## Visualização multivariada ==================================



dados %>%
  # Selecionando variaveis de interesse
  dplyr::select(diagnosis, radius_mean, texture_mean, fractal_dim_mean) %>%
  # ggpairs cria uma matriz de gráficos
  GGally::ggpairs(
    aes(color = diagnosis, alpha = 0.5), # Cores por grupo
  ) +
  theme_classic()



## Correlação não-paramétrica



# Calculando a correlacao de Spearman
dados %>%
  dplyr::select(radius_mean, texture_mean) %>%
  corrr::correlate(method = "spearman", quiet = TRUE)


# ggpairs com correlação não paramétrica
dados %>%
  dplyr::select(diagnosis, radius_mean, texture_mean, fractal_dim_mean) %>%
  GGally::ggpairs(
    aes(color = diagnosis, alpha = 0.5),
    upper = list(continuous = wrap(ggally_cor, method = "spearman")) # para correlação não paramétrica de Spearman
  ) +
  theme_classic()



## Modelagem ==================================================


# Semente para reprodutibilidade
set.seed(20251125)

# Divisão 80/20
split <- rsample::initial_split(dados, prop = 0.8)
treino <- rsample::training(split)
teste <- rsample::testing(split)



# Especificando o modelo
tree_spec <- parsnip::decision_tree(
  mode = "classification",
  engine = "rpart",
  tree_depth = 4 # profundidade máxima
)

# Ajuste do modelo
tree_fit <- tree_spec %>%
  fit(diagnosis ~ ., data = treino)



# Plot da árvore
rpart.plot::rpart.plot(tree_fit$fit, type = 4, roundint = FALSE)



# Gerar Predicoes no conjunto de Teste
predicoes <- tree_fit %>%
  predict(teste) %>%
  dplyr::pull(.pred_class)

# Gerar Matriz de Confusao
teste %>%
  dplyr::mutate(predicoes = predicoes) %>%
  conf_mat(truth = diagnosis, estimate = predicoes)

# Calcular Metricas (Acuracia e Kappa)
metricas <- metric_set(accuracy, kap) # acuracia e \kappa
teste %>%
  dplyr::mutate(predicoes = predicoes) %>%
  metricas(truth = diagnosis, estimate = predicoes)
