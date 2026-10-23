# Arquivo gerado a partir dos chunks R de relatorio2.qmd
# Objetivo: carregar os objetos, funções, análises e modelos em um ambiente R.
# Execute este arquivo a partir da raiz do projeto, onde existe data/base_unificada.rds.
# Observação: os chunks são executados na ordem original; os modelos são ajustados novamente.


# -----------------------------------------------------------------------------
# Chunk 1: sem rótulo
# -----------------------------------------------------------------------------
#| label: setup
#| include: false
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.width = 10,
  fig.height = 6,
  fig.align = "center"
)


# -----------------------------------------------------------------------------
# Chunk 2: carregar pacotes
# -----------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(gt)
library(dplyr)
library(purrr)
library(stringr)
library(knitr)
library(rstatix)
library(ordinal)
library(VGAM)
library(gtsummary)
library(broom)
library(car)


# -----------------------------------------------------------------------------
# Chunk 3: funcoes-tabelas
# -----------------------------------------------------------------------------
# ---- Padrão de tabelas do relatório ----
# Tema do gtsummary (tabelas dos modelos): português, vírgula decimal
theme_gtsummary_language("pt", decimal.mark = ",", big.mark = ".")

# Formatação de números no padrão brasileiro
fmt_br <- function(x, d = 2) {
  formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
}
fmt_pval <- function(p) ifelse(p < 0.001, "<0,001", fmt_br(p, 3))
fmt_p <- function(p) if (p < 0.001) "p < 0,001" else paste0("p = ", fmt_br(p, 3))

# Tabela padrão: booktabs, legenda numerada, fonte 9, nota no rodapé
tabela_padrao <- function(dados,
                          titulo,
                          nomes_colunas = names(dados),
                          nota = NULL,
                          longa = FALSE,
                          col_p = NULL,
                          p_valores = NULL,
                          alinhamento = c("l", rep("c", ncol(dados) - 1))) {
  tab <- kbl(
    dados,
    format = "latex",
    booktabs = TRUE,
    longtable = longa,
    linesep = "",
    caption = titulo,
    col.names = nomes_colunas,
    align = alinhamento
  ) %>%
    kable_styling(
      latex_options = if (longa) "repeat_header" else "HOLD_position",
      repeat_header_text = "\\textit{(continuação)}",
      font_size = 9
    )
  
  # p-valores menores que 0,05 em negrito (mesmo padrão das tabelas dos modelos)
  if (!is.null(col_p)) {
    tab <- column_spec(tab, col_p, bold = !is.na(p_valores) & p_valores < 0.05)
  }
  
  if (!is.null(nota)) {
    tab <- footnote(
      tab,
      general = nota,
      general_title = "Nota:",
      threeparttable = TRUE,
      footnote_as_chunk = TRUE
    )
  }
  tab
}

# Tabela de frequências com linha de total
tabela_frequencia <- function(dados, var, rotulo, titulo, na_rotulo = NULL) {
  tab <- dados %>%
    count(categoria = .data[[var]]) %>%
    mutate(
      categoria = as.character(categoria),
      freq_relativa = n / sum(n) * 100
    )
  
  if (!is.null(na_rotulo)) {
    tab <- tab %>% mutate(categoria = coalesce(categoria, na_rotulo))
  }
  
  tab <- bind_rows(
    tab,
    tibble(categoria = "Total", n = sum(tab$n), freq_relativa = 100)
  ) %>%
    mutate(
      n = fmt_br(n, 0),
      freq_relativa = fmt_br(freq_relativa, 2)
    )
  
  tabela_padrao(
    tab,
    titulo,
    nomes_colunas = c(rotulo, "Frequência", "Frequência relativa (%)")
  ) %>%
    row_spec(nrow(tab) - 1, extra_latex_after = "\\midrule") %>%
    row_spec(nrow(tab), bold = TRUE)
}


# -----------------------------------------------------------------------------
# Chunk 4: carregar dados
# -----------------------------------------------------------------------------
df <- readRDS("data/base_unificada.rds")

#filtros bases etnia/cor
etnia_cor_colombia <- df %>%
  filter(sistema_etnia_cor == "Pertencimento étnico – Colômbia")

etnia_cor_brasil <- df %>%
  filter(sistema_etnia_cor == "Cor/raça – Brasil")

df_trabalho <- df %>%
  filter(trabalho_detalhado == "Sim")


df_acessos_javeriana <- df %>%
  filter(instituicao_origem == "Javeriana") %>%
  select(
    instituicao_origem,
    starts_with("acesso_puj")
  ) %>%
  rename_with(
    ~ str_replace(.x, "^acesso_puj", "acesso"),
    starts_with("acesso_puj")
  )

df_acessos_ufmg <- df %>%
  filter(instituicao_origem != "Javeriana") %>%
  select(
    instituicao_origem,
    starts_with("acesso_ufmg")
  ) %>%
  rename_with(
    ~ str_replace(.x, "^acesso_ufmg", "acesso"),
    starts_with("acesso_ufmg")
  ) %>%
  rename(
    "acesso_cinema" = "acesso_cinemas",
    "acesso_teatro" = "acesso_teatros"
  )


# -----------------------------------------------------------------------------
# Chunk 5: instituicao de origem
# -----------------------------------------------------------------------------
tabela_frequencia(
  df,
  "contexto_institucional",
  "Contexto institucional",
  "Distribuição dos participantes por instituição de origem (Universidade - País)"
)


# -----------------------------------------------------------------------------
# Chunk 6: idade
# -----------------------------------------------------------------------------
df %>%
  summarise(
    Media = mean(idade, na.rm = TRUE),
    DP = sd(idade, na.rm = TRUE),
    Minimo = min(idade, na.rm = TRUE),
    Maximo = max(idade, na.rm = TRUE),
    N = n()
  ) %>%
  mutate(
    across(c(Media, DP, Minimo, Maximo), ~ fmt_br(.x, 2)),
    N = fmt_br(N, 0)
  ) %>%
  tabela_padrao(
    "Medidas descritivas da idade dos participantes (anos)",
    nomes_colunas = c("Média", "Desvio padrão", "Mínimo", "Máximo", "N"),
    alinhamento = rep("c", 5)
  )


# -----------------------------------------------------------------------------
# Chunk 7: genero respondente
# -----------------------------------------------------------------------------
tabela_frequencia(
  df,
  "genero_modelo",
  "Gênero",
  "Distribuição dos participantes por gênero",
  na_rotulo = "Prefiro não responder"
)


# -----------------------------------------------------------------------------
# Chunk 8: estado civil
# -----------------------------------------------------------------------------
tabela_frequencia(
  df,
  "estado_civil_detalhado",
  "Estado civil",
  "Distribuição dos participantes por estado civil"
)


# -----------------------------------------------------------------------------
# Chunk 9: etinia cor
# -----------------------------------------------------------------------------
plot_etnia_cor_colombia<- ggplot(etnia_cor_colombia, aes(x = fct_inorder(etnia_cor_detalhada), y = after_stat(count / sum(count) * 100))) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Distribuição de Etnia/cor",
    x = "Etnia/Cor",
    y = "Porcentagem (%)",
    subtitle = "Pertencimento étnico – Colômbia"
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

plot_etnia_cor_brasil<- ggplot(etnia_cor_brasil, aes(x = fct_inorder(etnia_cor_detalhada), y = after_stat(count / sum(count) * 100))) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Distribuição de Etnia/cor",
    x = "Etnia/Cor",
    y = "Porcentagem (%)",
    subtitle = "Cor/raça – Brasil"
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

plot_etnia_cor_colombia + plot_etnia_cor_brasil


# -----------------------------------------------------------------------------
# Chunk 10: nivel academico
# -----------------------------------------------------------------------------
tabela_frequencia(
  df,
  "nivel_academico_detalhado",
  "Nível acadêmico",
  "Distribuição dos participantes por nível acadêmico"
)


# -----------------------------------------------------------------------------
# Chunk 11: deficiencia
# -----------------------------------------------------------------------------
tabela_frequencia(
  df,
  "deficiencia_detalhada",
  "Pessoa com deficiência",
  "Distribuição dos participantes por condição de deficiência"
)


# -----------------------------------------------------------------------------
# Chunk 12: moradia
# -----------------------------------------------------------------------------
ggplot(df, aes(x = fct_inorder(moradia_modelo))) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Distribuição do modelo de moradia",
    x = "modelo de moradia",
    y = "Frequência",
    subtitle = "Contagem modelo de moradia"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )


# -----------------------------------------------------------------------------
# Chunk 13: trabalho
# -----------------------------------------------------------------------------
trabalho <- ggplot(df, aes(x = fct_inorder(trabalho_detalhado))) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Distribuição do estudantes que trabalham",
    x = "Trabalho",
    y = "Frequência",
    subtitle = "Contagem trabalhadores entre os respondentes"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

horas_trabalho <- ggplot(df_trabalho, aes(x = horas_trabalho_semana)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Distribuição horas trabalhadas",
    x = "Tempo em horas",
    subtitle = "Horas/Semana"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

trabalho + horas_trabalho


# -----------------------------------------------------------------------------
# Chunk 14: teste
# -----------------------------------------------------------------------------
#| fig-width: 12
#| fig-height: 20
grupo <- "instituicao_origem"

vars <- names(
  df %>% 
    select(
      starts_with("sm"),
      starts_with("rec"),
      -rec_dormir_puj,
      -rec_ler_ufmg,
      -rec_aulas_puj,
      -rec_festas_ufmg
    )
)

df_long <- df %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variavel",
    values_to = "valor"
  )

df_prop <- df_long %>%
  count(variavel, instituicao_origem, valor) %>%
  group_by(variavel, instituicao_origem) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

df_prop_sm <- df_prop %>%
  filter(str_starts(variavel, "sm"))

df_prop_rec <- df_prop %>%
  filter(str_starts(variavel, "rec"))

ggplot(
  df_prop_sm,
  aes(
    x = factor(valor),
    y = prop,
    fill = instituicao_origem
  )
) +
  geom_col(position = "dodge") +
  facet_wrap(
    ~ variavel,
    scales = "free_x",
    ncol = 2
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(
    title = "Atividades Significativas (SM)",
    x = NULL,
    y = "Proporção",
    fill = "Instituição"
  )


# -----------------------------------------------------------------------------
# Chunk 15: sem rótulo
# -----------------------------------------------------------------------------
#| fig-width: 12
#| fig-height: 20
ggplot(
  df_prop_rec,
  aes(
    x = factor(valor),
    y = prop,
    fill = instituicao_origem
  )
) +
  geom_col(position = "dodge") +
  facet_wrap(
    ~ variavel,
    scales = "free_x",
    ncol = 2
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(
    title = "Recursos de Lazer (REC)",
    x = NULL,
    y = "Proporção",
    fill = "Instituição"
  )


# -----------------------------------------------------------------------------
# Chunk 16: sem rótulo
# -----------------------------------------------------------------------------
resultado <- map_dfr(
  vars,
  function(var) {
    
    dados <- df %>%
      select(all_of(grupo), all_of(var)) %>%
      filter(
        !is.na(.data[[grupo]]),
        !is.na(.data[[var]])
      )
    
    grupos <- unique(dados[[grupo]])
    
    if (length(grupos) != 2) {
      return(NULL)
    }
    
    teste <- wilcox.test(
      dados[[var]] ~ dados[[grupo]],
      exact = FALSE
    )
    
    tibble(
      variavel = var,
      mediana_g1 = median(
        dados[[var]][dados[[grupo]] == grupos[1]],
        na.rm = TRUE
      ),
      mediana_g2 = median(
        dados[[var]][dados[[grupo]] == grupos[2]],
        na.rm = TRUE
      ),
      W = unname(teste$statistic),
      p_valor = teste$p.value
    )
  }
)

resultado %>%
  mutate(
    mediana_g1 = fmt_br(mediana_g1, 1),
    mediana_g2 = fmt_br(mediana_g2, 1),
    W = fmt_br(W, 1),
    p_valor_txt = fmt_pval(p_valor)
  ) %>%
  select(variavel, mediana_g1, mediana_g2, W, p_valor_txt) %>%
  tabela_padrao(
    "Teste de Mann-Whitney: comparação entre instituições",
    nomes_colunas = c("Variável", "Mediana G1", "Mediana G2", "W", "p-valor"),
    nota = "Mediana G1 e Mediana G2 são as medianas da Javeriana e da UFMG, respectivamente. p-valores menores que 0,05 em negrito.",
    longa = TRUE,
    col_p = 5,
    p_valores = resultado$p_valor
  )


# -----------------------------------------------------------------------------
# Chunk 17: sem rótulo
# -----------------------------------------------------------------------------
hist_horas <- ggplot(
  df,
  aes(
    x = horas_universidade_semana,
    fill = .data[[grupo]],
    color = .data[[grupo]]
  )
) +
  geom_density(
    alpha = 0.25,
    linewidth = 1
  ) +
  geom_rug(
    alpha = 0.15
  ) +
  theme_classic() +
  labs(
    title = "Distribuição de horas na Universidade",
    x = "Horas",
    subtitle = "Horas/semana"
  )

bx_horas <- ggplot(
  df,
  aes(
    y = horas_universidade_semana,
    x = instituicao_origem,
    fill = .data[[grupo]],
    color = .data[[grupo]]
  )
) +
  geom_boxplot() +
  geom_rug(
    alpha = 0.15
  ) +
  theme_classic() +
  labs(
    title = "Distribuição de horas na Universidade",
    x = "Instituição",
    subtitle = "Horas/semana"
  )

hist_horas + bx_horas


# -----------------------------------------------------------------------------
# Chunk 18: sem rótulo
# -----------------------------------------------------------------------------
var <- "horas_universidade_semana"

resultado_shapiro <- df %>%
  filter(
    !is.na(.data[[var]]),
    !is.na(.data[[grupo]])
  ) %>%
  group_by(.data[[grupo]]) %>%
  summarise(
    n = n(),
    W = unname(shapiro.test(.data[[var]])$statistic),
    p_valor = shapiro.test(.data[[var]])$p.value,
    .groups = "drop"
  ) %>%
  rename(grupo_resultado = all_of(grupo))

resultado_total <- df %>%
  filter(
    !is.na(.data[[var]]),
    !is.na(.data[[grupo]])
  ) %>%
  summarise(
    grupo_resultado = "Total",
    n = n(),
    W = unname(shapiro.test(.data[[var]])$statistic),
    p_valor = shapiro.test(.data[[var]])$p.value
  )

resultado_shapiro <- bind_rows(
  resultado_shapiro,
  resultado_total
)

resultado_shapiro %>%
  mutate(
    n = fmt_br(n, 0),
    W = fmt_br(W, 4),
    p_valor_txt = fmt_pval(p_valor)
  ) %>%
  select(grupo_resultado, n, W, p_valor_txt) %>%
  tabela_padrao(
    "Teste de Shapiro-Wilk para normalidade das horas semanais na universidade",
    nomes_colunas = c("Grupo", "n", "W", "p-valor"),
    nota = "Hipótese nula: os dados seguem distribuição normal. p-valores menores que 0,05 em negrito.",
    col_p = 4,
    p_valores = resultado_shapiro$p_valor
  )


# -----------------------------------------------------------------------------
# Chunk 19: sem rótulo
# -----------------------------------------------------------------------------
var <- "horas_universidade_semana"
grupo <- "instituicao_origem"

teste <- wilcox.test(
  df[[var]] ~ df[[grupo]]
)

grupos <- unique(df[[grupo]])

resultado <- tibble(
  Variável = var,
  `Grupo 1` = grupos[1],
  `Mediana G1` = median(
    df[[var]][df[[grupo]] == grupos[1]],
    na.rm = TRUE
  ),
  `Grupo 2` = grupos[2],
  `Mediana G2` = median(
    df[[var]][df[[grupo]] == grupos[2]],
    na.rm = TRUE
  ),
  `W` = unname(teste$statistic),
  `p-valor` = teste$p.value
)

resultado %>%
  mutate(
    `Mediana G1` = fmt_br(`Mediana G1`, 1),
    `Mediana G2` = fmt_br(`Mediana G2`, 1),
    W = fmt_br(W, 1),
    p_txt = fmt_pval(`p-valor`)
  ) %>%
  select(Variável, `Grupo 1`, `Mediana G1`, `Grupo 2`, `Mediana G2`, W, p_txt) %>%
  tabela_padrao(
    "Teste de Mann-Whitney: horas semanais na universidade entre as instituições",
    nomes_colunas = c("Variável", "Grupo 1", "Mediana G1", "Grupo 2", "Mediana G2", "W", "p-valor"),
    nota = "Teste de Wilcoxon rank-sum com correção de continuidade. p-valores menores que 0,05 em negrito.",
    col_p = 7,
    p_valores = resultado$`p-valor`
  )


# -----------------------------------------------------------------------------
# Chunk 20: sem rótulo
# -----------------------------------------------------------------------------
ggplot(
  df,
  aes(
    x = factor(
      mhc_classificacao,
      levels = c(
        "Languishing",
        "Moderate",
        "Flourishing"
      )
    )
  )
) +
  geom_bar() +
  theme_classic() +
  labs(
    title = "Distribuição da classificação MHC",
    x = "Classificação",
    y = "Frequência",
    subtitle = "Critérios de Keyes"
  )


# -----------------------------------------------------------------------------
# Chunk 21: preparo-modelos
# -----------------------------------------------------------------------------
# ---- Variável resposta ordenada ----
df$mhc_classificacao <- factor(
  df$mhc_classificacao,
  levels = c("Languishing", "Moderate", "Flourishing"),
  ordered = TRUE
)

# ---- Práticas de lazer incluídas nos modelos ----
vars_rec_modelo <- c(
  "rec_caminhar", "rec_casal", "rec_contemplar", "rec_treinamento",
  "rec_competicoes", "rec_comer_social", "rec_redes_sociais",
  "rec_jogos_eletronicos", "rec_filmes", "rec_academia",
  "rec_assistir_treinos", "rec_danca"
)

# Modelo 1: cada valor da escala (1 a 10) é uma categoria
df <- df %>%
  mutate(
    across(
      starts_with("rec"),
      ~ factor(.x, levels = as.character(1:10), ordered = F)
    ),
    horas_universidade_semana = case_when(
      horas_universidade_semana <= 20 ~ "Até 20h",
      horas_universidade_semana > 20 & horas_universidade_semana <= 40 ~ "20 - 40h",
      TRUE ~ "+40h"
    ),
    etnia_cor_detalhada = recode(
      etnia_cor_detalhada,
      "Negro(a)" = "Preta"
    ),
    etnia_cor_detalhada = factor(etnia_cor_detalhada),
    etnia_cor_detalhada = relevel(etnia_cor_detalhada, ref = "Preta")
  )

# Modelo 2: escala agrupada em cinco faixas
df2 <- df %>%
  mutate(
    across(
      starts_with("rec"),
      ~ factor(
        case_when(
          .x %in% c("1", "2")  ~ "1-2",
          .x %in% c("3", "4")  ~ "3-4",
          .x %in% c("5", "6")  ~ "5-6",
          .x %in% c("7", "8")  ~ "7-8",
          .x %in% c("9", "10") ~ "9-10",
          TRUE ~ NA_character_
        ),
        levels = c("1-2", "3-4", "5-6", "7-8", "9-10")
      )
    )
  )

# Modelo 3: escala tratada como número (1 a 10)
df3 <- df %>%
  mutate(
    across(
      starts_with("rec"),
      ~ as.numeric(as.character(.x))
    )
  )

# ---- Ajuste dos três modelos ----
modelo_ordinal <- clm(
  mhc_classificacao ~
    idade +
    genero_modelo +
    trabalho_sim +
    horas_universidade_semana +
    rec_caminhar +
    rec_casal +
    rec_contemplar +
    rec_treinamento +
    rec_competicoes +
    rec_comer_social +
    rec_redes_sociais +
    rec_jogos_eletronicos +
    rec_filmes +
    rec_academia +
    rec_assistir_treinos +
    rec_danca +
    rec_amigos +
    rec_desenhar +
    rec_musica +
    rec_jogos_mesa +
    rec_religioso +
    moradia_modelo +
    etnia_cor_detalhada +
    anos_no_curso,
  data = df
)

modelo_ordinal2 <- clm(
  mhc_classificacao ~
    idade +
    genero_modelo +
    trabalho_sim +
    horas_universidade_semana +
    rec_caminhar +
    rec_casal +
    rec_contemplar +
    rec_treinamento +
    rec_competicoes +
    rec_comer_social +
    rec_redes_sociais +
    rec_jogos_eletronicos +
    rec_filmes +
    rec_academia +
    rec_assistir_treinos +
    rec_danca +
    rec_amigos +
    rec_desenhar +
    rec_musica +
    rec_jogos_mesa +
    rec_religioso +
    moradia_modelo +
    etnia_cor_detalhada +
    anos_no_curso,
  data = df2
)

modelo_ordinal3 <- clm(
  mhc_classificacao ~
    idade +
    genero_modelo +
    trabalho_sim +
    horas_universidade_semana +
    rec_caminhar +
    rec_casal +
    rec_contemplar +
    rec_treinamento +
    rec_competicoes +
    rec_comer_social +
    rec_redes_sociais +
    rec_jogos_eletronicos +
    rec_filmes +
    rec_academia +
    rec_assistir_treinos +
    rec_danca +
    rec_amigos +
    rec_desenhar +
    rec_musica +
    rec_jogos_mesa +
    rec_religioso +
    moradia_modelo +
    etnia_cor_detalhada +
    anos_no_curso,
  data = df3
)


# -----------------------------------------------------------------------------
# Chunk 22: funcoes-auxiliares
# -----------------------------------------------------------------------------
# Mantém só os coeficientes das variáveis (remove os limiares/interceptos)
tidy_clm <- function(x, ..., conf.type = "profile") {
  broom::tidy(x, ..., conf.type = conf.type) %>%
    dplyr::filter(coef.type == "location") %>%
    dplyr::select(-coef.type)
}

# Tabela padronizada, com quebra de página (longtable) para o PDF
tabela_modelo <- function(modelo, titulo, conf_type = "profile") {
  tbl_regression(
    modelo,
    exponentiate = TRUE,
    tidy_fun = purrr::partial(tidy_clm, conf.type = conf_type)
  ) %>%
    bold_p(t = 0.05) %>%
    bold_labels() %>%
    italicize_levels() %>%
    modify_header(label = "**Variável**") %>%
    modify_caption(titulo) %>%
    as_kable_extra(booktabs = TRUE, longtable = TRUE, linesep = "") %>%
    kableExtra::kable_styling(
      latex_options = c("repeat_header"),
      repeat_header_text = "\\textit{(continuação)}",
      font_size = 9
    )
}

# OR, IC 95% e p-valor de um termo específico (usado no texto)
or_info <- function(m, termo) {
  ic <- tryCatch(
    exp(suppressMessages(confint(m, parm = termo))),
    error = function(e) exp(confint(m, parm = termo, type = "Wald"))
  )
  list(
    or = unname(exp(coef(m)[termo])),
    lo = ic[1],
    hi = ic[2],
    p  = summary(m)$coefficients[termo, "Pr(>|z|)"]
  )
}
or_txt <- function(m, termo) {
  i <- or_info(m, termo)
  paste0("OR = ", fmt_br(i$or), "; IC 95%: ", fmt_br(i$lo), " a ", fmt_br(i$hi), "; ", fmt_p(i$p))
}

# Diagnóstico de estimação
diag_info <- function(m) {
  tibble(
    parametros = length(coef(m)),
    n = m$nobs,
    aic = AIC(m)
  )
}

# Contagem de "células" atividade x nível x classificação
contar_celulas <- function(dados) {
  map_dfr(vars_rec_modelo, function(v) {
    dados %>%
      filter(!is.na(mhc_classificacao), !is.na(.data[[v]])) %>%
      mutate(nivel = droplevels(.data[[v]])) %>%
      count(mhc_classificacao, nivel, .drop = FALSE) %>%
      mutate(variavel = v)
  })
}


# -----------------------------------------------------------------------------
# Chunk 23: numeros-texto
# -----------------------------------------------------------------------------
n_modelo   <- modelo_ordinal3$nobs
n_lang     <- sum(df$mhc_classificacao == "Languishing", na.rm = TRUE)
n_excluidos <- nrow(df) - n_modelo

# Modelo 1
beta1        <- modelo_ordinal$beta
n_beta1      <- length(beta1)
or1          <- exp(beta1)
n_extremos1  <- sum(or1 > 100 | or1 < 0.01, na.rm = TRUE)
cel1         <- contar_celulas(df)
pct_vazias1  <- mean(cel1$n == 0) * 100
pct_poucas1  <- mean(cel1$n < 5) * 100

# Modelo 2
beta2        <- modelo_ordinal2$beta
n_beta2      <- length(beta2)
cel2         <- contar_celulas(df2)
pct_vazias2  <- mean(cel2$n == 0) * 100
pct_poucas2  <- mean(cel2$n < 5) * 100
coefs2       <- summary(modelo_ordinal2)$coefficients
termos_rec2  <- names(beta2)[str_starts(names(beta2), "rec_")]
n_comp2      <- length(termos_rec2)
n_sig2       <- sum(coefs2[termos_rec2, "Pr(>|z|)"] < 0.05, na.rm = TRUE)

# Modelo 3
n_beta3      <- length(modelo_ordinal3$beta)
jogos        <- or_info(modelo_ordinal3, "rec_jogos_eletronicos")
filmes       <- or_info(modelo_ordinal3, "rec_filmes")
contemplar   <- or_info(modelo_ordinal3, "rec_contemplar")


# -----------------------------------------------------------------------------
# Chunk 24: tabela-tres-modelos
# -----------------------------------------------------------------------------
tibble(
  aspecto = c(
    "Como a atividade entra",
    "O que cada OR compara",
    "Valores estimados por atividade",
    "Total de efeitos estimados"
  ),
  m1 = c("10 categorias separadas (1, 2, ..., 10)", "Cada nível contra o nível 1", "9", as.character(n_beta1)),
  m2 = c("5 faixas (1-2, 3-4, 5-6, 7-8, 9-10)", "Cada faixa contra a faixa 1-2", "4", as.character(n_beta2)),
  m3 = c("Um número de 1 a 10", "O efeito de subir 1 ponto na escala", "1", as.character(n_beta3))
) %>%
  tabela_padrao(
    "Diferenças na forma de incluir as práticas de lazer nos três modelos",
    nomes_colunas = c("", "Modelo 1 (original)", "Modelo 2 (faixas)", "Modelo 3 (numérico)")
  ) %>%
  column_spec(1, bold = TRUE, width = "3.2cm") %>%
  column_spec(2:4, width = "3.8cm")


# -----------------------------------------------------------------------------
# Chunk 25: tabela-danca
# -----------------------------------------------------------------------------
tab_danca <- df %>%
  filter(!is.na(mhc_classificacao), !is.na(rec_danca)) %>%
  count(mhc_classificacao, rec_danca, .drop = FALSE) %>%
  pivot_wider(names_from = rec_danca, values_from = n) %>%
  mutate(mhc_classificacao = as.character(mhc_classificacao))

tab_danca %>%
  tabela_padrao(
    "Número de estudantes por classificação MHC e frequência de dança",
    nomes_colunas = c("Classificação MHC", names(tab_danca)[-1])
  ) %>%
  add_header_above(
    setNames(c(1, ncol(tab_danca) - 1), c(" ", "Frequência de dança (escala de 1 a 10)"))
  ) %>%
  footnote(
    general = "A categoria 9 tem 0 observações para duas das três categorias da variavel responta, isso impede a estimação estável no Modelo 1.",
    general_title = "Nota:",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )


# -----------------------------------------------------------------------------
# Chunk 26: sem rótulo
# -----------------------------------------------------------------------------
tab_uni <- df %>%
  filter(!is.na(mhc_classificacao), !is.na(contexto_institucional)) %>%
  count(mhc_classificacao, contexto_institucional, .drop = FALSE) %>%
  pivot_wider(names_from = contexto_institucional, values_from = n) %>%
  mutate(mhc_classificacao = as.character(mhc_classificacao))

tab_uni %>%
  tabela_padrao(
    "Número de estudantes por classificação MHC e Universidade",
    nomes_colunas = c("Classificação MHC", names(tab_uni)[-1])
  )


# -----------------------------------------------------------------------------
# Chunk 27: tabela-modelo2
# -----------------------------------------------------------------------------
tabela_modelo(
  modelo_ordinal2,
  "Modelo 2 — Regressão ordinal com práticas de lazer agrupadas em cinco faixas (referência: faixa 1-2)"
)


# -----------------------------------------------------------------------------
# Chunk 28: tabela-modelo3
# -----------------------------------------------------------------------------
tabela_modelo(
  modelo_ordinal3,
  "Modelo 3 — Regressão ordinal com práticas de lazer em escala numérica (OR por ponto a mais na escala)"
)


# -----------------------------------------------------------------------------
# Chunk 29: tabela-diagnostico
# -----------------------------------------------------------------------------
bind_rows(
  diag_info(modelo_ordinal)  %>% mutate(modelo = "1 - Original (10 níveis)"),
  diag_info(modelo_ordinal2) %>% mutate(modelo = "2 - Faixas (5 níveis)"),
  diag_info(modelo_ordinal3) %>% mutate(modelo = "3 - Escala numérica")
) %>%
  select(modelo, parametros, n, aic) %>%
  mutate(
    aic = fmt_br(aic, 1)
  ) %>%
  tabela_padrao(
    "Diagnóstico de estimação dos três modelos",
    nomes_colunas = c(
      "Modelo", "Parâmetros estimados", "N", "AIC"
    ),
    nota = "Parâmetros estimados incluem os 2 limiares (interceptos) do modelo ordinal"
  )


# -----------------------------------------------------------------------------
# Chunk 30: tabela-modelo1
# -----------------------------------------------------------------------------
# tabela_modelo(
#   modelo_ordinal,
#   "Modelo 1 (original) — Regressão ordinal com cada nível da escala de lazer como categoria (referência: nível 1)",
#   conf_type = "Wald"
# )
