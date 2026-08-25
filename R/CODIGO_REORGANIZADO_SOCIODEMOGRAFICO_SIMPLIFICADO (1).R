# =============================================================================
# 0. CONFIGURAÇÃO
# =============================================================================

rm(list = ls())
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1,
  error = NULL
)

pasta_dados <- "C:/Users/namuetchasilvar/Downloads"

nome_arquivo_javeriana <-
  "AsociacinEntreLasPrc_DATA_2026-06-01_1046.csv"

nome_arquivo_ufmg <- paste0(
  "PRÁTICAS DE LAZER POR JOVENS UNIVERSITÁRIOS ",
  "(respostas) - Respostas ao formulário 1 (2).csv"
)

pasta_saida <- file.path(
  pasta_dados,
  "resultados_tese_ordinal_CORRIGIDOS"
)

data_min_javeriana <- as.Date("2026-03-09")
data_min_ufmg <- as.Date("2026-04-28")
ano_analise <- 2026L
alpha <- 0.05
set.seed(20260803)

# TRUE somente enquanto forem usados os arquivos com as dimensões conhecidas.
validar_contagens_dos_arquivos_atuais <- TRUE

# FALSE por padrão. Os microdados desidentificados ainda são restritos
# e não devem ser compartilhados como se fossem anônimos.
exportar_microdados_restritos <- FALSE

dir.create(
  pasta_saida,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(pasta_saida)) {
  stop(
    paste0(
      "Não foi possível criar a pasta de saída:\n",
      pasta_saida
    ),
    call. = FALSE
  )
}

arquivo_log <- file.path(
  pasta_saida,
  "00_LOG_EXECUCAO.txt"
)

if (file.exists(arquivo_log)) {
  file.remove(arquivo_log)
}

registrar <- function(...) {
  texto <- paste0(...)
  linha <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    texto
  )

  cat(linha, "\n")

  try(
    cat(
      linha,
      "\n",
      file = arquivo_log,
      append = TRUE
    ),
    silent = TRUE
  )

  invisible(linha)
}

registrar("INÍCIO DA EXECUÇÃO")

# =============================================================================
# 1. PACOTES
# =============================================================================

pacotes <- c(
  "readr", "dplyr", "stringr", "lubridate",
  "tidyr", "purrr", "tibble", "openxlsx", "ordinal"
)

pacotes_ausentes <- pacotes[
  !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(pacotes_ausentes) > 0L) {
  stop(
    paste0(
      "Instale os pacotes ausentes antes de executar:\n",
      "install.packages(c(",
      paste(sprintf('\"%s\"', pacotes_ausentes), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(openxlsx)
  library(ordinal)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

# =============================================================================
# 2. FUNÇÕES GERAIS
# =============================================================================

achar_coluna <- function(df, trecho, exata = FALSE) {
  nomes <- names(df)

  indice <- if (isTRUE(exata)) {
    which(nomes == trecho)
  } else {
    which(str_detect(nomes, fixed(trecho, ignore_case = TRUE)))
  }

  if (length(indice) != 1L) {
    stop(
      paste0(
        "A busca por coluna não retornou exatamente uma coluna.\n",
        "Trecho: ", trecho, "\n",
        "Quantidade encontrada: ", length(indice), "\n",
        "Correspondências: ",
        paste(nomes[indice], collapse = " | ")
      ),
      call. = FALSE
    )
  }

  nomes[indice]
}

normalizar_texto <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_lower(str_squish(coalesce(x, "")))
  x
}

dummy_contem <- function(x, padroes) {
  texto <- normalizar_texto(x)
  padroes <- normalizar_texto(padroes)
  ausente <- texto == ""

  resultado <- vapply(
    texto,
    function(valor) {
      if (valor == "") return(NA_integer_)
      as.integer(any(vapply(
        padroes,
        function(p) str_detect(valor, fixed(p)),
        logical(1)
      )))
    },
    integer(1)
  )

  resultado[ausente] <- NA_integer_
  resultado
}

dummy_exato <- function(x, categorias) {
  texto <- normalizar_texto(x)
  categorias <- normalizar_texto(categorias)
  ifelse(
    texto == "",
    NA_integer_,
    as.integer(texto %in% categorias)
  )
}

checkbox_01 <- function(x) {
  valor <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(valor) ~ NA_integer_,
    valor == 1 ~ 1L,
    valor == 0 ~ 0L,
    TRUE ~ NA_integer_
  )
}

sim_nao_num <- function(x) {
  texto <- normalizar_texto(x)
  case_when(
    texto == "" ~ NA_integer_,
    texto == "sim" ~ 1L,
    texto == "nao" ~ 0L,
    TRUE ~ NA_integer_
  )
}

juntar_rotulos <- function(valores, rotulos) {
  valores <- as.integer(valores)
  selecionados <- rotulos[which(valores == 1L)]
  if (length(selecionados) == 0L) return(NA_character_)
  paste(selecionados, collapse = "; ")
}

validar_intervalo <- function(x, minimo, maximo, nome) {
  x <- suppressWarnings(as.numeric(x))
  invalidos <- unique(x[!is.na(x) & (x < minimo | x > maximo)])

  if (length(invalidos) > 0L) {
    stop(
      paste0(
        "Valores inválidos em ", nome, ": ",
        paste(invalidos, collapse = ", "),
        ". Intervalo esperado: ", minimo, " a ", maximo, "."
      ),
      call. = FALSE
    )
  }
}

validar_dummy <- function(df, variaveis) {
  problemas <- map_dfr(
    variaveis,
    function(nome) {
      valores <- unique(df[[nome]])
      invalidos <- valores[
        !is.na(valores) & !(valores %in% c(0L, 1L, 0, 1))
      ]

      tibble(
        variavel = nome,
        valida = length(invalidos) == 0L,
        valores_invalidos = paste(invalidos, collapse = "; ")
      )
    }
  )

  if (any(!problemas$valida)) {
    stop(
      paste0(
        "Foram encontradas dummies inválidas:\n",
        paste(
          problemas$variavel[!problemas$valida],
          problemas$valores_invalidos[!problemas$valida],
          sep = " = ",
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  problemas
}

verificar_duplicatas <- function(df, excluir = character(0), origem) {
  dados <- df %>%
    select(-any_of(excluir)) %>%
    mutate(across(everything(), as.character))

  chave <- do.call(
    paste,
    c(dados, sep = "\u241F")
  )
  frequencias <- table(chave)
  tibble(
    origem = origem,
    linhas = nrow(df),
    duplicatas_exatas_sem_identificadores = sum(frequencias[frequencias > 1L] - 1L),
    grupos_duplicados = sum(frequencias > 1L)
  )
}

calcular_alpha <- function(df_itens) {
  dados <- as.data.frame(df_itens)

  dados <- dados[
    stats::complete.cases(dados),
    ,
    drop = FALSE
  ]

  k <- ncol(dados)

  if (nrow(dados) < 2L || k < 2L) {
    return(NA_real_)
  }

  variancias_itens <- vapply(
    dados,
    stats::var,
    numeric(1)
  )

  variancia_total <- stats::var(
    rowSums(dados)
  )

  if (
    !is.finite(variancia_total) ||
    variancia_total <= 0
  ) {
    return(NA_real_)
  }

  (k / (k - 1)) * (
    1 -
      sum(
        variancias_itens,
        na.rm = TRUE
      ) / variancia_total
  )
}

obter_contagem_tabela <- function(
  tabela,
  categoria
) {
  if (
    categoria %in% names(tabela)
  ) {
    return(
      as.integer(
        unname(
          tabela[[categoria]]
        )
      )
    )
  }

  0L
}

calcular_mhc <- function(df) {
  itens <- c(
  "sm_alegria",
  "sm_interesse",
  "sm_satisfacao",
  "sm_contribuicao",
  "sm_comunidade",
  "sm_sociedade_melhor",
  "sm_pessoas_boas",
  "sm_sociedade_sentido",
  "sm_personalidade",
  "sm_responsabilidades",
  "sm_relacoes",
  "sm_crescimento",
  "sm_confianca",
  "sm_vida_sentido"
)
  emocionais <- c("sm_alegria", "sm_interesse", "sm_satisfacao")
  funcionais <- setdiff(itens, emocionais)

  df %>%
    rowwise() %>%
    mutate(
      mhc_itens_validos = sum(!is.na(c_across(all_of(itens)))),
      mhc_total_14_84 = if_else(
        mhc_itens_validos == 14L,
        sum(c_across(all_of(itens))),
        NA_real_
      ),
      mhc_total_0_70 = if_else(
        mhc_itens_validos == 14L,
        sum(c_across(all_of(itens)) - 1),
        NA_real_
      ),
      .alto_emocional = sum(
        c_across(all_of(emocionais)) >= 5,
        na.rm = TRUE
      ),
      .alto_funcional = sum(
        c_across(all_of(funcionais)) >= 5,
        na.rm = TRUE
      ),
      .baixo_emocional = sum(
        c_across(all_of(emocionais)) <= 2,
        na.rm = TRUE
      ),
      .baixo_funcional = sum(
        c_across(all_of(funcionais)) <= 2,
        na.rm = TRUE
      ),
      mhc_codigo = case_when(
        mhc_itens_validos < 14L ~ NA_integer_,
        .alto_emocional >= 1L & .alto_funcional >= 6L ~ 3L,
        .baixo_emocional >= 1L & .baixo_funcional >= 6L ~ 1L,
        TRUE ~ 2L
      ),
      mhc_classificacao = case_when(
        mhc_codigo == 1L ~ "Languishing",
        mhc_codigo == 2L ~ "Moderate",
        mhc_codigo == 3L ~ "Flourishing",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    select(
      -.alto_emocional,
      -.alto_funcional,
      -.baixo_emocional,
      -.baixo_funcional
    )
}

adicionar_coluna_tipificada <- function(df, nome, tipo) {
  if (nome %in% names(df)) return(df)

  df[[nome]] <- switch(
    tipo,
    character = NA_character_,
    integer = NA_integer_,
    numeric = NA_real_,
    stop("Tipo de schema desconhecido: ", tipo, call. = FALSE)
  )

  df
}

alinhar_schema <- function(df, schema_tipos) {
  for (nome in names(schema_tipos)) {
    df <- adicionar_coluna_tipificada(df, nome, schema_tipos[[nome]])
  }

  df <- df[, names(schema_tipos), drop = FALSE]

  for (nome in names(schema_tipos)) {
    df[[nome]] <- switch(
      schema_tipos[[nome]],
      character = as.character(df[[nome]]),
      integer = as.integer(df[[nome]]),
      numeric = as.numeric(df[[nome]])
    )
  }

  as_tibble(df)
}

comparar_tipos <- function(df1, df2) {
  tibble(
    coluna = names(df1),
    tipo_javeriana = vapply(df1, function(x) class(x)[1], character(1)),
    tipo_ufmg = vapply(df2, function(x) class(x)[1], character(1)),
    mesmo_tipo = tipo_javeriana == tipo_ufmg
  )
}

resumo_continuo <- function(df, variaveis) {
  grupos <- c("Total", sort(unique(df$instituicao_origem)))

  map_dfr(
    grupos,
    function(grupo) {
      dados <- if (grupo == "Total") df else filter(df, instituicao_origem == grupo)

      map_dfr(
        variaveis,
        function(nome) {
          x <- suppressWarnings(as.numeric(dados[[nome]]))
          validos <- x[!is.na(x)]

          tibble(
            grupo = grupo,
            variavel = nome,
            n_total = length(x),
            n_valido = length(validos),
            n_ausente = sum(is.na(x)),
            media = if (length(validos) > 0L) mean(validos) else NA_real_,
            desvio_padrao = if (length(validos) > 1L) sd(validos) else NA_real_,
            mediana = if (length(validos) > 0L) median(validos) else NA_real_,
            q1 = if (length(validos) > 0L) quantile(validos, 0.25, names = FALSE) else NA_real_,
            q3 = if (length(validos) > 0L) quantile(validos, 0.75, names = FALSE) else NA_real_,
            minimo = if (length(validos) > 0L) min(validos) else NA_real_,
            maximo = if (length(validos) > 0L) max(validos) else NA_real_
          )
        }
      )
    }
  )
}

resumo_categorico <- function(df, variaveis) {
  grupos <- c("Total", sort(unique(df$instituicao_origem)))

  map_dfr(
    grupos,
    function(grupo) {
      dados <- if (grupo == "Total") df else filter(df, instituicao_origem == grupo)

      map_dfr(
        variaveis,
        function(nome) {
          x <- as.character(dados[[nome]])
          ausentes <- is.na(x) | str_squish(x) == ""
          n_validos <- sum(!ausentes)

          categorias <- sort(unique(x[!ausentes]))

          if (length(categorias) == 0L) {
            return(tibble(
              grupo = grupo,
              variavel = nome,
              categoria = NA_character_,
              n = 0L,
              percentual_valido = NA_real_,
              percentual_total = 0,
              n_ausente = sum(ausentes)
            ))
          }

          map_dfr(
            categorias,
            function(categoria) {
              n_categoria <- sum(x == categoria, na.rm = TRUE)
              tibble(
                grupo = grupo,
                variavel = nome,
                categoria = categoria,
                n = n_categoria,
                percentual_valido = 100 * n_categoria / n_validos,
                percentual_total = 100 * n_categoria / length(x),
                n_ausente = sum(ausentes)
              )
            }
          )
        }
      )
    }
  )
}

auditar_dummies <- function(df, variaveis) {
  grupos <- c("Total", sort(unique(df$instituicao_origem)))

  map_dfr(
    grupos,
    function(grupo) {
      dados <- if (grupo == "Total") df else filter(df, instituicao_origem == grupo)

      map_dfr(
        variaveis,
        function(nome) {
          x <- dados[[nome]]
          tibble(
            grupo = grupo,
            dummy = nome,
            n_0 = sum(x == 0, na.rm = TRUE),
            n_1 = sum(x == 1, na.rm = TRUE),
            n_ausente = sum(is.na(x)),
            valores_validos = all(is.na(x) | x %in% c(0, 1))
          )
        }
      )
    }
  )
}

relatorio_ausencias <- function(df) {
  map_dfr(
    c("Total", sort(unique(df$instituicao_origem))),
    function(grupo) {
      dados <- if (grupo == "Total") df else filter(df, instituicao_origem == grupo)

      tibble(
        grupo = grupo,
        variavel = names(dados),
        n = nrow(dados),
        n_ausente = vapply(dados, function(x) sum(is.na(x)), integer(1)),
        percentual_ausente = 100 * n_ausente / n
      )
    }
  )
}

extrair_celulas_raras <- function(tabela_categorica, limite = 5L) {
  tabela_categorica %>%
    filter(!is.na(categoria), n > 0L, n < limite) %>%
    mutate(
      alerta = paste0("Célula com n < ", limite)
    )
}

# =============================================================================
# 3. LOCALIZAÇÃO E LEITURA DOS ARQUIVOS ORIGINAIS
# =============================================================================

arquivo_javeriana <- file.path(
  pasta_dados,
  nome_arquivo_javeriana
)

arquivo_ufmg <- file.path(
  pasta_dados,
  nome_arquivo_ufmg
)

if (!file.exists(arquivo_javeriana)) {
  stop(
    paste0(
      "Arquivo da Javeriana não encontrado:\n",
      arquivo_javeriana
    ),
    call. = FALSE
  )
}

if (!file.exists(arquivo_ufmg)) {
  stop(
    paste0(
      "Arquivo da UFMG não encontrado:\n",
      arquivo_ufmg
    ),
    call. = FALSE
  )
}

arquivo_javeriana <- normalizePath(
  arquivo_javeriana,
  winslash = "/",
  mustWork = TRUE
)

arquivo_ufmg <- normalizePath(
  arquivo_ufmg,
  winslash = "/",
  mustWork = TRUE
)

registrar("Arquivo Javeriana: ", arquivo_javeriana)
registrar("Arquivo UFMG: ", arquivo_ufmg)

base_javeriana_original <- readr::read_csv(
  arquivo_javeriana,
  show_col_types = FALSE,
  name_repair = "minimal"
)

base_ufmg_original <- readr::read_csv(
  arquivo_ufmg,
  show_col_types = FALSE,
  name_repair = "minimal"
)

if (anyDuplicated(names(base_javeriana_original)) > 0L) {
  stop(
    "Existem nomes de colunas duplicados na base Javeriana.",
    call. = FALSE
  )
}

if (anyDuplicated(names(base_ufmg_original)) > 0L) {
  stop(
    "Existem nomes de colunas duplicados na base UFMG.",
    call. = FALSE
  )
}

dimensoes_originais <- tibble::tibble(
  base = c(
    "Javeriana original",
    "UFMG original"
  ),
  linhas = c(
    nrow(base_javeriana_original),
    nrow(base_ufmg_original)
  ),
  colunas = c(
    ncol(base_javeriana_original),
    ncol(base_ufmg_original)
  )
)

if (isTRUE(validar_contagens_dos_arquivos_atuais)) {
  if (!identical(
    dim(base_javeriana_original),
    c(163L, 118L)
  )) {
    stop(
      paste0(
        "Dimensão inesperada na Javeriana: ",
        paste(
          dim(base_javeriana_original),
          collapse = " x "
        ),
        ". Esperado: 163 x 118."
      ),
      call. = FALSE
    )
  }

  if (!identical(
    dim(base_ufmg_original),
    c(262L, 61L)
  )) {
    stop(
      paste0(
        "Dimensão inesperada na UFMG: ",
        paste(
          dim(base_ufmg_original),
          collapse = " x "
        ),
        ". Esperado: 262 x 61."
      ),
      call. = FALSE
    )
  }
}

# =============================================================================
# 4. IDENTIFICAÇÃO ROBUSTA DAS COLUNAS DA UFMG
# =============================================================================

col_u <- list(
  data = achar_coluna(base_ufmg_original, "Carimbo de data/hora", TRUE),
  elegibilidade = achar_coluna(
    base_ufmg_original,
    "Nosso questionário é destinado a pessoas com idade entre 18 e 29 anos"
  ),
  consentimento = achar_coluna(
    base_ufmg_original,
    "TERMO DE CONSENTIMENTO LIVRE E ESCLARECIDO"
  ),

  rec_caminhar = achar_coluna(base_ufmg_original, "Caminhada pelo campus"),
  rec_casal = achar_coluna(base_ufmg_original, "Passar tempo com o(a) parceiro(a)"),
  rec_amigos = achar_coluna(base_ufmg_original, "Momentos de convivência com amigos"),
  rec_contemplar = achar_coluna(base_ufmg_original, "Sentar-me em áreas abertas"),
  rec_desenhar = achar_coluna(base_ufmg_original, "Desenhar por prazer"),
  rec_musica = achar_coluna(base_ufmg_original, "Participar de aulas de música"),
  rec_danca = achar_coluna(base_ufmg_original, "Participar de aulas de dança"),
  rec_treinamento = achar_coluna(base_ufmg_original, "Participar de treinos esportivos"),
  rec_assistir_treinos = achar_coluna(base_ufmg_original, "Assistir a treinos esportivos"),
  rec_jogos_mesa = achar_coluna(base_ufmg_original, "Jogar jogos de cartas ou tabuleiro"),
  rec_comer_social = achar_coluna(base_ufmg_original, "Comer em contexto social"),
  rec_religioso = achar_coluna(base_ufmg_original, "Participar de encontros religiosos"),
  rec_redes_sociais = achar_coluna(base_ufmg_original, "Utilizar redes sociais por lazer"),
  rec_jogos_eletronicos = achar_coluna(base_ufmg_original, "Jogar jogos eletrônicos"),
  rec_competicoes = achar_coluna(base_ufmg_original, "Participar de competições esportivas"),
  rec_filmes = achar_coluna(base_ufmg_original, "Assistir filmes online"),
  rec_bebida = achar_coluna(base_ufmg_original, "Consumir bebidas alcoólicas"),
  rec_fumar = achar_coluna(base_ufmg_original, "Fumar dentro da universidade"),
  rec_academia = achar_coluna(base_ufmg_original, "Frequentar a academia do campus"),
  rec_interacoes_afetivo_sexuais = achar_coluna(base_ufmg_original, "Vivenciar interações afetivo-sexuais"),

  sm_interesse = achar_coluna(base_ufmg_original, "Interessada(o) pela vida"),
  sm_satisfacao = achar_coluna(base_ufmg_original, "Satisfeito (a)", TRUE),
  sm_alegria = achar_coluna(base_ufmg_original, "Feliz", TRUE),
  sm_contribuicao = achar_coluna(base_ufmg_original, "algo importante para contribuir"),
  sm_comunidade = achar_coluna(base_ufmg_original, "pertencia a uma comunidade"),
  sm_sociedade_melhor = achar_coluna(base_ufmg_original, "sociedade está se tornando"),
  sm_pessoas_boas = achar_coluna(base_ufmg_original, "pessoas, em geral, são boas"),
  sm_sociedade_sentido = achar_coluna(base_ufmg_original, "forma como a nossa sociedade funciona"),
  sm_personalidade = achar_coluna(base_ufmg_original, "características de personalidade"),
  sm_responsabilidades = achar_coluna(base_ufmg_original, "administrou bem as responsabilidades"),
  sm_relacoes = achar_coluna(base_ufmg_original, "relacionamentos afetuosos"),
  sm_crescimento = achar_coluna(base_ufmg_original, "experiências que o desafiaram"),
  sm_confianca = achar_coluna(base_ufmg_original, "confiante para pensar"),
  sm_vida_sentido = achar_coluna(base_ufmg_original, "vida tem um propósito"),

  genero = achar_coluna(base_ufmg_original, "Como você se identifica em relação ao seu gênero"),
  idade = achar_coluna(base_ufmg_original, "Qual a sua idade"),
  estado_civil = achar_coluna(base_ufmg_original, "Qual o seu estado civil"),
  cor = achar_coluna(base_ufmg_original, "Qual sua cor"),
  deficiencia = achar_coluna(base_ufmg_original, "Você possui alguma deficiência"),
  moradia = achar_coluna(base_ufmg_original, "Quem reside com você"),
  ano_ingresso = achar_coluna(base_ufmg_original, "Em que ano você iniciou"),
  horas_universidade = achar_coluna(base_ufmg_original, "quantas horas semanais você permanece"),
  nivel = achar_coluna(base_ufmg_original, "Você é estudante de"),
  interesse_grupo = achar_coluna(base_ufmg_original, "interesse em para participar do grupo focal"),
  email = achar_coluna(base_ufmg_original, "E-mail", TRUE),
  whatsapp = achar_coluna(base_ufmg_original, "WhatsApp", TRUE)
)

# =============================================================================
# 5. DUPLICIDADES E FILTROS
# =============================================================================

auditoria_duplicatas <- bind_rows(
  verificar_duplicatas(
    base_javeriana_original,
    excluir = c(
      "salud_mental_ins", "redcap_survey_identifier",
      "form_1_timestamp", "grupo_focal_permision",
      "grupo_focal_puj", "dados_grupo_focal_puj"
    ),
    origem = "Javeriana"
  ),
  verificar_duplicatas(
    base_ufmg_original,
    excluir = c(
      col_u$data, col_u$interesse_grupo,
      col_u$email, col_u$whatsapp
    ),
    origem = "UFMG"
  )
)

base_javeriana_filtrada <- base_javeriana_original %>%
  mutate(
    .data_hora_filtro = parse_date_time(
      form_1_timestamp,
      orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"),
      quiet = TRUE
    )
  ) %>%
  filter(
    !is.na(.data_hora_filtro),
    as.Date(.data_hora_filtro) >= data_min_javeriana,
    rango_edad == 1,
    consent == 1,
    form_1_complete == 2
  )

base_ufmg_filtrada <- base_ufmg_original %>%
  mutate(
    .data_hora_filtro = parse_date_time(
      .data[[col_u$data]],
      orders = c("dmy HMS", "dmy HM", "ymd HMS", "ymd HM"),
      quiet = TRUE
    ),
    .elegibilidade = normalizar_texto(.data[[col_u$elegibilidade]]),
    .consentimento = normalizar_texto(
      str_remove_all(.data[[col_u$consentimento]], '^\"|\"$')
    )
  ) %>%
  filter(
    !is.na(.data_hora_filtro),
    as.Date(.data_hora_filtro) >= data_min_ufmg,
    .elegibilidade == "sim",
    str_detect(.consentimento, fixed("concordo em participar"))
  )

fluxo_amostral <- tibble(
  etapa = c(
    "Base original",
    "Após data mínima, elegibilidade, consentimento e completude"
  ),
  javeriana = c(
    nrow(base_javeriana_original),
    nrow(base_javeriana_filtrada)
  ),
  ufmg = c(
    nrow(base_ufmg_original),
    nrow(base_ufmg_filtrada)
  ),
  total = javeriana + ufmg
)

if (isTRUE(validar_contagens_dos_arquivos_atuais)) {
  if (nrow(base_javeriana_filtrada) != 102L) {
    stop(
      paste0(
        "A Javeriana deveria ter 102 casos elegíveis, mas tem ",
        nrow(base_javeriana_filtrada), "."
      ),
      call. = FALSE
    )
  }

  if (nrow(base_ufmg_filtrada) != 125L) {
    stop(
      paste0(
        "A UFMG deveria ter 125 casos elegíveis, mas tem ",
        nrow(base_ufmg_filtrada), "."
      ),
      call. = FALSE
    )
  }
}

registrar(
  "CASOS ELEGÍVEIS | Javeriana: ", nrow(base_javeriana_filtrada),
  " | UFMG: ", nrow(base_ufmg_filtrada)
)

# =============================================================================

# =============================================================================
# 6. SCHEMA HARMONIZADO SIMPLIFICADO
# =============================================================================
#
# PRINCÍPIO:
# - manter apenas as categorias sociodemográficas necessárias;
# - não carregar tipos detalhados de deficiência;
# - não carregar trabalho/renda/carga de trabalho;
# - não carregar cursos específicos;
# - não carregar estrato socioeconômico ou condição de atleta;
# - manter instituição somente na base interna restrita de auditoria;
# - criar indicador institucional apenas na base de modelagem.
#
# IMPORTANTE SOBRE GÊNERO:
# Os instrumentos originais não possuem variável de orientação sexual e não
# coletam "bissexual". Bissexualidade é orientação sexual, não categoria de
# gênero. Portanto, o terceiro grupo que pode ser derivado validamente dos
# dados existentes é "Diversidade de gênero". O código NÃO fabrica uma
# categoria "Bissexual" sem informação observada.

itens_mhc <- c(
  "sm_alegria",
  "sm_interesse",
  "sm_satisfacao",
  "sm_contribuicao",
  "sm_comunidade",
  "sm_sociedade_melhor",
  "sm_pessoas_boas",
  "sm_sociedade_sentido",
  "sm_personalidade",
  "sm_responsabilidades",
  "sm_relacoes",
  "sm_crescimento",
  "sm_confianca",
  "sm_vida_sentido"
)

praticas_comuns_20 <- c(
  "rec_caminhar",
  "rec_casal",
  "rec_amigos",
  "rec_contemplar",
  "rec_desenhar",
  "rec_musica",
  "rec_danca",
  "rec_treinamento",
  "rec_assistir_treinos",
  "rec_jogos_mesa",
  "rec_comer_social",
  "rec_religioso",
  "rec_redes_sociais",
  "rec_jogos_eletronicos",
  "rec_competicoes",
  "rec_filmes",
  "rec_bebida",
  "rec_fumar",
  "rec_academia",
  "rec_interacoes_afetivo_sexuais"
)

praticas_teoricas_13 <- c(
  "rec_caminhar",
  "rec_casal",
  "rec_amigos",
  "rec_contemplar",
  "rec_treinamento",
  "rec_competicoes",
  "rec_comer_social",
  "rec_redes_sociais",
  "rec_jogos_eletronicos",
  "rec_filmes",
  "rec_academia",
  "rec_assistir_treinos",
  "rec_danca"
)

colunas_schema <- c(
  # Informação institucional restrita
  "instituicao_origem",

  # MHC-SF
  "mhc_itens_validos",
  "mhc_total_14_84",
  "mhc_total_0_70",
  "mhc_codigo",
  "mhc_classificacao",
  itens_mhc,

  # Práticas comuns
  praticas_comuns_20,

  # Gênero: referência = Mulher
  "genero_modelo",
  "genero_homem",
  "genero_diversidade",

  # Estado civil: referência = Solteiro(a)
  "estado_civil_modelo",
  "estado_civil_uniao_estavel",
  "estado_civil_casado",

  # Deficiência: referência = Não
  "deficiencia_modelo",
  "deficiencia_sim",

  # Moradia: referência = Com família
  "moradia_modelo",
  "moradia_sozinha",
  "moradia_compartilha_casa",

  # Etnia/cor: referência = Branco
  # Uso principal: descrição/sensibilidade, devido à não equivalência completa
  # entre pertencimento étnico colombiano e cor/raça brasileira.
  "etnia_modelo",
  "etnia_negro",
  "etnia_indigena",

  # Trajetória acadêmica
  "idade",
  "ano_ingresso",
  "anos_no_curso",
  "horas_universidade_semana",
  "idade_centralizada",
  "anos_curso_centralizados",
  "horas_universidade_10",

  # Nível acadêmico: referência = Graduação
  "nivel_academico_modelo",
  "pos_graduacao"
)

colunas_character <- c(
  "instituicao_origem",
  "mhc_classificacao",
  "genero_modelo",
  "estado_civil_modelo",
  "deficiencia_modelo",
  "moradia_modelo",
  "etnia_modelo",
  "nivel_academico_modelo"
)

colunas_numeric <- c(
  itens_mhc,
  praticas_comuns_20,
  "mhc_total_14_84",
  "mhc_total_0_70",
  "idade",
  "ano_ingresso",
  "anos_no_curso",
  "horas_universidade_semana",
  "idade_centralizada",
  "anos_curso_centralizados",
  "horas_universidade_10"
)

colunas_integer <- c(
  "mhc_itens_validos",
  "mhc_codigo",
  "genero_homem",
  "genero_diversidade",
  "estado_civil_uniao_estavel",
  "estado_civil_casado",
  "deficiencia_sim",
  "moradia_sozinha",
  "moradia_compartilha_casa",
  "etnia_negro",
  "etnia_indigena",
  "pos_graduacao"
)

schema_tipos <- setNames(
  rep("integer", length(colunas_schema)),
  colunas_schema
)
schema_tipos[colunas_character] <- "character"
schema_tipos[colunas_numeric] <- "numeric"
schema_tipos[colunas_integer] <- "integer"

if (anyDuplicated(names(schema_tipos)) > 0L) {
  stop(
    "O schema harmonizado contém nomes de colunas duplicados.",
    call. = FALSE
  )
}

registrar(
  "SCHEMA HARMONIZADO SIMPLIFICADO | ",
  length(colunas_schema),
  " colunas"
)

# =============================================================================
# 7. HARMONIZAÇÃO DA JAVERIANA
# =============================================================================

base_javeriana_harmonizada <- base_javeriana_filtrada %>%
  mutate(
    instituicao_origem = "Javeriana",

    # -------------------------------------------------------------------------
    # Práticas de lazer comuns às duas universidades
    # -------------------------------------------------------------------------
    rec_caminhar = as.numeric(rec_caminar),
    rec_casal = as.numeric(rec_casal),
    rec_amigos = as.numeric(rec_amigos),
    rec_contemplar = as.numeric(rec_contemplar),
    rec_desenhar = as.numeric(rec_desenhar),
    rec_musica = as.numeric(rec_musica),
    rec_danca = as.numeric(rec_danca),
    rec_treinamento = as.numeric(rec_treino),
    rec_assistir_treinos = as.numeric(rec_assistir),
    rec_jogos_mesa = as.numeric(rec_jogos),
    rec_comer_social = as.numeric(rec_comer),
    rec_religioso = as.numeric(rec_religioso),
    rec_redes_sociais = as.numeric(rec_redes),
    rec_jogos_eletronicos = as.numeric(rec_games),
    rec_competicoes = as.numeric(rec_competicao),
    rec_filmes = as.numeric(rec_filmes),
    rec_bebida = as.numeric(rec_bebida_puj),
    rec_fumar = as.numeric(rec_fumar_puj),
    rec_academia = as.numeric(rec_academia),
    rec_interacoes_afetivo_sexuais = as.numeric(rec_sex_puj),

    # -------------------------------------------------------------------------
    # MHC-SF
    # -------------------------------------------------------------------------
    sm_alegria = as.numeric(sm_alegria),
    sm_interesse = as.numeric(sm_interesvida),
    sm_satisfacao = as.numeric(sm_satisfaccionvida),
    sm_contribuicao = as.numeric(sm_importanciasociedad),
    sm_comunidade = as.numeric(sm_partecomunidad),
    sm_sociedade_melhor = as.numeric(sm_sociedadebien),
    sm_pessoas_boas = as.numeric(sm_personasbuenas),
    sm_sociedade_sentido = as.numeric(sm_funcionamentosociedad),
    sm_personalidade = as.numeric(sm_mipersonalidad),
    sm_responsabilidades = as.numeric(sm_responsabilidades),
    sm_relacoes = as.numeric(sm_relaciones),
    sm_crescimento = as.numeric(sm_experiencias),
    sm_confianca = as.numeric(sm_confianza),
    sm_vida_sentido = as.numeric(sm_vidasentido),

    # -------------------------------------------------------------------------
    # Gênero
    # Mulher cis/trans -> Mulher
    # Homem cis/trans  -> Homem
    # Não binário, fluido ou agênero -> Diversidade de gênero
    # Prefiro não responder -> NA
    # -------------------------------------------------------------------------
    .genero_mulher = pmax(
      coalesce(checkbox_01(sexo___1), 0L),
      coalesce(checkbox_01(sexo___3), 0L)
    ),
    .genero_homem = pmax(
      coalesce(checkbox_01(sexo___2), 0L),
      coalesce(checkbox_01(sexo___4), 0L)
    ),
    .genero_diversidade = pmax(
      coalesce(checkbox_01(sexo___5), 0L),
      coalesce(checkbox_01(sexo___6), 0L),
      coalesce(checkbox_01(sexo___7), 0L)
    ),
    .genero_nao_respondeu = coalesce(
      checkbox_01(sexo___8),
      0L
    ),
    .genero_n_grupos = (
      .genero_mulher +
        .genero_homem +
        .genero_diversidade
    ),
    genero_modelo = case_when(
      .genero_n_grupos == 0L ~ NA_character_,
      .genero_diversidade == 1L ~ "Diversidade de gênero",
      .genero_n_grupos > 1L ~ "Diversidade de gênero",
      .genero_mulher == 1L ~ "Mulher",
      .genero_homem == 1L ~ "Homem",
      TRUE ~ NA_character_
    ),
    genero_homem = case_when(
      genero_modelo == "Mulher" ~ 0L,
      genero_modelo == "Homem" ~ 1L,
      genero_modelo == "Diversidade de gênero" ~ 0L,
      TRUE ~ NA_integer_
    ),
    genero_diversidade = case_when(
      genero_modelo == "Mulher" ~ 0L,
      genero_modelo == "Homem" ~ 0L,
      genero_modelo == "Diversidade de gênero" ~ 1L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Estado civil
    # Apenas: Solteiro(a), União estável, Casado(a)
    # Viúvo(a), divorciado(a) e não resposta -> NA
    # -------------------------------------------------------------------------
    .civil_solteiro = checkbox_01(estado_civil___1),
    .civil_casado = checkbox_01(estado_civil___2),
    .civil_uniao_estavel = checkbox_01(estado_civil___5),
    estado_civil_modelo = case_when(
      .civil_casado == 1L ~ "Casado(a)",
      .civil_uniao_estavel == 1L ~ "União estável",
      .civil_solteiro == 1L ~ "Solteiro(a)",
      TRUE ~ NA_character_
    ),
    estado_civil_uniao_estavel = case_when(
      estado_civil_modelo == "Solteiro(a)" ~ 0L,
      estado_civil_modelo == "União estável" ~ 1L,
      estado_civil_modelo == "Casado(a)" ~ 0L,
      TRUE ~ NA_integer_
    ),
    estado_civil_casado = case_when(
      estado_civil_modelo == "Solteiro(a)" ~ 0L,
      estado_civil_modelo == "União estável" ~ 0L,
      estado_civil_modelo == "Casado(a)" ~ 1L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Deficiência
    # Somente Sim/Não. Tipos de deficiência são descartados.
    # -------------------------------------------------------------------------
    deficiencia_sim = case_when(
      suppressWarnings(as.numeric(deficiencia)) == 1 ~ 1L,
      suppressWarnings(as.numeric(deficiencia)) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    deficiencia_modelo = case_when(
      deficiencia_sim == 1L ~ "Sim",
      deficiencia_sim == 0L ~ "Não",
      TRUE ~ NA_character_
    ),

    # -------------------------------------------------------------------------
    # Moradia
    # Apenas:
    # 1) Mora sozinho(a)
    # 2) Com família
    # 3) Compartilha casa
    #
    # Parceiro(a)/cônjuge é agrupado com "Com família".
    # Em seleção múltipla, presença de família/parceiro tem prioridade.
    # -------------------------------------------------------------------------
    .moradia_sozinha = checkbox_01(membros_familias___1),
    .moradia_familia = checkbox_01(membros_familias___2),
    .moradia_pares = checkbox_01(membros_familias___3),
    .moradia_parceiro = checkbox_01(membros_familias___4),
    .moradia_conjuge = checkbox_01(membros_familias___5),

    moradia_modelo = case_when(
      .moradia_familia == 1L |
        .moradia_parceiro == 1L |
        .moradia_conjuge == 1L ~ "Com família",
      .moradia_pares == 1L ~ "Compartilha casa",
      .moradia_sozinha == 1L ~ "Mora sozinho(a)",
      TRUE ~ NA_character_
    ),
    moradia_sozinha = case_when(
      moradia_modelo == "Com família" ~ 0L,
      moradia_modelo == "Compartilha casa" ~ 0L,
      moradia_modelo == "Mora sozinho(a)" ~ 1L,
      TRUE ~ NA_integer_
    ),
    moradia_compartilha_casa = case_when(
      moradia_modelo == "Com família" ~ 0L,
      moradia_modelo == "Compartilha casa" ~ 1L,
      moradia_modelo == "Mora sozinho(a)" ~ 0L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Etnia/cor
    # Javeriana mede pertencimento étnico, não cor/raça.
    # - Indígena -> Indígena
    # - Negro(a), Raizal ou Palenquero(a) -> Negro
    # - Nenhuma filiação étnica e Rom NÃO são convertidos artificialmente em
    #   "Branco"; ficam NA na variável harmonizada de três categorias.
    # -------------------------------------------------------------------------
    .etnia_indigena = checkbox_01(cultura___1),
    .etnia_rom = checkbox_01(cultura___2),
    .etnia_raizal = checkbox_01(cultura___3),
    .etnia_palenquera = checkbox_01(cultura___4),
    .etnia_negra = checkbox_01(cultura___5),
    .etnia_sem_filiacao = checkbox_01(cultura___6),

    etnia_modelo = case_when(
      .etnia_indigena == 1L ~ "Indígena",
      .etnia_negra == 1L |
        .etnia_raizal == 1L |
        .etnia_palenquera == 1L ~ "Negro",
      TRUE ~ NA_character_
    ),
    etnia_negro = case_when(
      etnia_modelo == "Branco" ~ 0L,
      etnia_modelo == "Negro" ~ 1L,
      etnia_modelo == "Indígena" ~ 0L,
      TRUE ~ NA_integer_
    ),
    etnia_indigena = case_when(
      etnia_modelo == "Branco" ~ 0L,
      etnia_modelo == "Negro" ~ 0L,
      etnia_modelo == "Indígena" ~ 1L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Idade e trajetória acadêmica
    # -------------------------------------------------------------------------
    idade = suppressWarnings(as.numeric(idade)),
    ano_ingresso = suppressWarnings(as.numeric(ano_de_ingresso)),
    anos_no_curso = ano_analise - ano_ingresso,
    horas_universidade_semana = suppressWarnings(
      as.numeric(tempo_universidade)
    ),

    # -------------------------------------------------------------------------
    # Nível acadêmico
    # Apenas Graduação/Pós-graduação.
    # Referência = Graduação.
    # -------------------------------------------------------------------------
    .nivel_graduacao = checkbox_01(nivel_pregrado),
    .nivel_pos = as.integer(
      rowSums(
        cbind(
          coalesce(checkbox_01(nivel_especi), 0L),
          coalesce(checkbox_01(nivel_maestria), 0L),
          coalesce(checkbox_01(nivel_doctorado), 0L)
        ),
        na.rm = TRUE
      ) > 0L
    ),
    nivel_academico_modelo = case_when(
      .nivel_pos == 1L ~ "Pós-graduação",
      .nivel_graduacao == 1L ~ "Graduação",
      TRUE ~ NA_character_
    ),
    pos_graduacao = case_when(
      nivel_academico_modelo == "Graduação" ~ 0L,
      nivel_academico_modelo == "Pós-graduação" ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  calcular_mhc() %>%
  select(-starts_with("."))

# =============================================================================
# 8. HARMONIZAÇÃO DA UFMG
# =============================================================================

base_ufmg_harmonizada <- base_ufmg_filtrada %>%
  mutate(
    instituicao_origem = "UFMG",

    # -------------------------------------------------------------------------
    # Práticas de lazer comuns
    # -------------------------------------------------------------------------
    rec_caminhar = as.numeric(.data[[col_u$rec_caminhar]]),
    rec_casal = as.numeric(.data[[col_u$rec_casal]]),
    rec_amigos = as.numeric(.data[[col_u$rec_amigos]]),
    rec_contemplar = as.numeric(.data[[col_u$rec_contemplar]]),
    rec_desenhar = as.numeric(.data[[col_u$rec_desenhar]]),
    rec_musica = as.numeric(.data[[col_u$rec_musica]]),
    rec_danca = as.numeric(.data[[col_u$rec_danca]]),
    rec_treinamento = as.numeric(.data[[col_u$rec_treinamento]]),
    rec_assistir_treinos = as.numeric(.data[[col_u$rec_assistir_treinos]]),
    rec_jogos_mesa = as.numeric(.data[[col_u$rec_jogos_mesa]]),
    rec_comer_social = as.numeric(.data[[col_u$rec_comer_social]]),
    rec_religioso = as.numeric(.data[[col_u$rec_religioso]]),
    rec_redes_sociais = as.numeric(.data[[col_u$rec_redes_sociais]]),
    rec_jogos_eletronicos = as.numeric(.data[[col_u$rec_jogos_eletronicos]]),
    rec_competicoes = as.numeric(.data[[col_u$rec_competicoes]]),
    rec_filmes = as.numeric(.data[[col_u$rec_filmes]]),
    rec_bebida = as.numeric(.data[[col_u$rec_bebida]]),
    rec_fumar = as.numeric(.data[[col_u$rec_fumar]]),
    rec_academia = as.numeric(.data[[col_u$rec_academia]]),
    rec_interacoes_afetivo_sexuais = as.numeric(
      .data[[col_u$rec_interacoes_afetivo_sexuais]]
    ),

    # -------------------------------------------------------------------------
    # MHC-SF
    # -------------------------------------------------------------------------
    sm_alegria = as.numeric(.data[[col_u$sm_alegria]]),
    sm_interesse = as.numeric(.data[[col_u$sm_interesse]]),
    sm_satisfacao = as.numeric(.data[[col_u$sm_satisfacao]]),
    sm_contribuicao = as.numeric(.data[[col_u$sm_contribuicao]]),
    sm_comunidade = as.numeric(.data[[col_u$sm_comunidade]]),
    sm_sociedade_melhor = as.numeric(.data[[col_u$sm_sociedade_melhor]]),
    sm_pessoas_boas = as.numeric(.data[[col_u$sm_pessoas_boas]]),
    sm_sociedade_sentido = as.numeric(.data[[col_u$sm_sociedade_sentido]]),
    sm_personalidade = as.numeric(.data[[col_u$sm_personalidade]]),
    sm_responsabilidades = as.numeric(.data[[col_u$sm_responsabilidades]]),
    sm_relacoes = as.numeric(.data[[col_u$sm_relacoes]]),
    sm_crescimento = as.numeric(.data[[col_u$sm_crescimento]]),
    sm_confianca = as.numeric(.data[[col_u$sm_confianca]]),
    sm_vida_sentido = as.numeric(.data[[col_u$sm_vida_sentido]]),

    # -------------------------------------------------------------------------
    # Gênero
    # -------------------------------------------------------------------------
    .genero_texto = as.character(.data[[col_u$genero]]),
    .genero_mulher = pmax(
      coalesce(dummy_contem(.genero_texto, "Mulher cisgênero"), 0L),
      coalesce(dummy_contem(.genero_texto, "Mulher transgênero"), 0L)
    ),
    .genero_homem = pmax(
      coalesce(dummy_contem(.genero_texto, "Homem cisgênero"), 0L),
      coalesce(dummy_contem(.genero_texto, "Homem transgênero"), 0L)
    ),
    .genero_diversidade = pmax(
      coalesce(dummy_contem(.genero_texto, "Pessoa não-binária"), 0L),
      coalesce(dummy_contem(.genero_texto, "Gênero fluido"), 0L),
      coalesce(dummy_contem(.genero_texto, "Agênero"), 0L)
    ),
    .genero_n_grupos = (
      .genero_mulher +
        .genero_homem +
        .genero_diversidade
    ),
    genero_modelo = case_when(
      normalizar_texto(.genero_texto) == "" ~ NA_character_,
      str_detect(
        normalizar_texto(.genero_texto),
        fixed("prefiro nao responder")
      ) & .genero_n_grupos == 0L ~ NA_character_,
      .genero_diversidade == 1L ~ "Diversidade de gênero",
      .genero_n_grupos > 1L ~ "Diversidade de gênero",
      .genero_mulher == 1L ~ "Mulher",
      .genero_homem == 1L ~ "Homem",
      TRUE ~ NA_character_
    ),
    genero_homem = case_when(
      genero_modelo == "Mulher" ~ 0L,
      genero_modelo == "Homem" ~ 1L,
      genero_modelo == "Diversidade de gênero" ~ 0L,
      TRUE ~ NA_integer_
    ),
    genero_diversidade = case_when(
      genero_modelo == "Mulher" ~ 0L,
      genero_modelo == "Homem" ~ 0L,
      genero_modelo == "Diversidade de gênero" ~ 1L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Estado civil
    # -------------------------------------------------------------------------
    .civil_texto = as.character(.data[[col_u$estado_civil]]),
    estado_civil_modelo = case_when(
      dummy_exato(.civil_texto, "Solteiro(a)") == 1L ~ "Solteiro(a)",
      dummy_exato(.civil_texto, "União estável") == 1L ~ "União estável",
      dummy_exato(.civil_texto, "Casado(a)") == 1L ~ "Casado(a)",
      TRUE ~ NA_character_
    ),
    estado_civil_uniao_estavel = case_when(
      estado_civil_modelo == "Solteiro(a)" ~ 0L,
      estado_civil_modelo == "União estável" ~ 1L,
      estado_civil_modelo == "Casado(a)" ~ 0L,
      TRUE ~ NA_integer_
    ),
    estado_civil_casado = case_when(
      estado_civil_modelo == "Solteiro(a)" ~ 0L,
      estado_civil_modelo == "União estável" ~ 0L,
      estado_civil_modelo == "Casado(a)" ~ 1L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Deficiência
    # -------------------------------------------------------------------------
    deficiencia_sim = sim_nao_num(
      .data[[col_u$deficiencia]]
    ),
    deficiencia_modelo = case_when(
      deficiencia_sim == 1L ~ "Sim",
      deficiencia_sim == 0L ~ "Não",
      TRUE ~ NA_character_
    ),

    # -------------------------------------------------------------------------
    # Moradia
    # -------------------------------------------------------------------------
    .moradia_texto = as.character(.data[[col_u$moradia]]),
    .moradia_sozinha = dummy_contem(
      .moradia_texto,
      c("Somente você", "Moro sozinho")
    ),
    .moradia_familia = dummy_contem(
      .moradia_texto,
      c(
        "Familiares",
        "Irmã", "Irmão", "Avô", "Avó", "Pai", "Mãe",
        "Namorado", "Namorada",
        "Companheiro", "Companheira",
        "Esposo", "Esposa", "Cônjuge"
      )
    ),
    .moradia_compartilha = dummy_contem(
      .moradia_texto,
      c(
        "Amigos", "República", "Republica",
        "Colegas", "universitárias", "estudantes",
        "divido apartamento", "outras pessoas",
        "Colegas de casa"
      )
    ),
    moradia_modelo = case_when(
      .moradia_familia == 1L ~ "Com família",
      .moradia_compartilha == 1L ~ "Compartilha casa",
      .moradia_sozinha == 1L ~ "Mora sozinho(a)",
      TRUE ~ NA_character_
    ),
    moradia_sozinha = case_when(
      moradia_modelo == "Com família" ~ 0L,
      moradia_modelo == "Compartilha casa" ~ 0L,
      moradia_modelo == "Mora sozinho(a)" ~ 1L,
      TRUE ~ NA_integer_
    ),
    moradia_compartilha_casa = case_when(
      moradia_modelo == "Com família" ~ 0L,
      moradia_modelo == "Compartilha casa" ~ 1L,
      moradia_modelo == "Mora sozinho(a)" ~ 0L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Etnia/cor
    # UFMG:
    # Branca -> Branco
    # Preta + Parda -> Negro
    # Indígena -> Indígena
    # Amarela e "Prefiro não declarar" -> NA
    # -------------------------------------------------------------------------
    .cor_texto = as.character(.data[[col_u$cor]]),
    etnia_modelo = case_when(
      dummy_exato(.cor_texto, "Branca") == 1L ~ "Branco",
      dummy_exato(.cor_texto, c("Preta", "Parda")) == 1L ~ "Negro",
      dummy_exato(.cor_texto, "Indígena") == 1L ~ "Indígena",
      TRUE ~ NA_character_
    ),
    etnia_negro = case_when(
      etnia_modelo == "Branco" ~ 0L,
      etnia_modelo == "Negro" ~ 1L,
      etnia_modelo == "Indígena" ~ 0L,
      TRUE ~ NA_integer_
    ),
    etnia_indigena = case_when(
      etnia_modelo == "Branco" ~ 0L,
      etnia_modelo == "Negro" ~ 0L,
      etnia_modelo == "Indígena" ~ 1L,
      TRUE ~ NA_integer_
    ),

    # -------------------------------------------------------------------------
    # Idade e trajetória acadêmica
    # -------------------------------------------------------------------------
    idade = suppressWarnings(
      as.numeric(.data[[col_u$idade]])
    ),
    ano_ingresso = suppressWarnings(
      as.numeric(.data[[col_u$ano_ingresso]])
    ),
    anos_no_curso = ano_analise - ano_ingresso,
    horas_universidade_semana = suppressWarnings(
      as.numeric(.data[[col_u$horas_universidade]])
    ),

    # -------------------------------------------------------------------------
    # Nível acadêmico
    # -------------------------------------------------------------------------
    .nivel_texto = as.character(.data[[col_u$nivel]]),
    nivel_academico_modelo = case_when(
      dummy_exato(.nivel_texto, "Graduação") == 1L ~ "Graduação",
      dummy_exato(.nivel_texto, "Pós-graduação") == 1L ~ "Pós-graduação",
      TRUE ~ NA_character_
    ),
    pos_graduacao = case_when(
      nivel_academico_modelo == "Graduação" ~ 0L,
      nivel_academico_modelo == "Pós-graduação" ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  calcular_mhc() %>%
  select(-starts_with("."))

# =============================================================================
# 9. ALINHAMENTO, UNIÃO RESTRITA E CENTRALIZAÇÃO
# =============================================================================

base_javeriana_harmonizada <- alinhar_schema(
  base_javeriana_harmonizada,
  schema_tipos
)

base_ufmg_harmonizada <- alinhar_schema(
  base_ufmg_harmonizada,
  schema_tipos
)

if (!identical(
  names(base_javeriana_harmonizada),
  names(base_ufmg_harmonizada)
)) {
  stop(
    "As bases harmonizadas não possuem os mesmos nomes e ordem de colunas.",
    call. = FALSE
  )
}

comparacao_tipos <- comparar_tipos(
  base_javeriana_harmonizada,
  base_ufmg_harmonizada
)

if (any(!comparacao_tipos$mesmo_tipo)) {
  stop(
    paste0(
      "As bases harmonizadas possuem tipos incompatíveis em: ",
      paste(
        comparacao_tipos$coluna[
          !comparacao_tipos$mesmo_tipo
        ],
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

base_harmonizada_restrita <- bind_rows(
  base_javeriana_harmonizada,
  base_ufmg_harmonizada
)

media_idade <- mean(
  base_harmonizada_restrita$idade,
  na.rm = TRUE
)

media_anos_curso <- mean(
  base_harmonizada_restrita$anos_no_curso,
  na.rm = TRUE
)

media_horas_universidade <- mean(
  base_harmonizada_restrita$horas_universidade_semana,
  na.rm = TRUE
)

base_harmonizada_restrita <- base_harmonizada_restrita %>%
  mutate(
    idade_centralizada = idade - media_idade,
    anos_curso_centralizados = anos_no_curso - media_anos_curso,
    horas_universidade_10 = (
      horas_universidade_semana -
        media_horas_universidade
    ) / 10
  ) %>%
  alinhar_schema(schema_tipos)

base_javeriana_harmonizada <- base_harmonizada_restrita %>%
  filter(instituicao_origem == "Javeriana")

base_ufmg_harmonizada <- base_harmonizada_restrita %>%
  filter(instituicao_origem == "UFMG")

dimensoes_harmonizadas <- tibble(
  base = c(
    "Javeriana harmonizada simplificada",
    "UFMG harmonizada simplificada",
    "Base harmonizada restrita"
  ),
  linhas = c(
    nrow(base_javeriana_harmonizada),
    nrow(base_ufmg_harmonizada),
    nrow(base_harmonizada_restrita)
  ),
  colunas = c(
    ncol(base_javeriana_harmonizada),
    ncol(base_ufmg_harmonizada),
    ncol(base_harmonizada_restrita)
  )
)

registrar(
  "BASE HARMONIZADA RESTRITA | ",
  nrow(base_harmonizada_restrita),
  " linhas | ",
  ncol(base_harmonizada_restrita),
  " colunas"
)

# =============================================================================
# 10. VALIDAÇÕES DA BASE HARMONIZADA RESTRITA
# =============================================================================

walk(
  praticas_comuns_20,
  ~ validar_intervalo(
    base_harmonizada_restrita[[.x]],
    1,
    10,
    .x
  )
)

walk(
  itens_mhc,
  ~ validar_intervalo(
    base_harmonizada_restrita[[.x]],
    1,
    6,
    .x
  )
)

validar_intervalo(
  base_harmonizada_restrita$idade,
  18,
  29,
  "idade"
)

validar_intervalo(
  base_harmonizada_restrita$mhc_codigo,
  1,
  3,
  "mhc_codigo"
)

validar_intervalo(
  base_harmonizada_restrita$horas_universidade_semana,
  0,
  168,
  "horas_universidade_semana"
)

validar_intervalo(
  base_harmonizada_restrita$ano_ingresso,
  1990,
  ano_analise,
  "ano_ingresso"
)

validar_intervalo(
  base_harmonizada_restrita$anos_no_curso,
  0,
  36,
  "anos_no_curso"
)

validar_intervalo(
  base_harmonizada_restrita$mhc_itens_validos,
  14,
  14,
  "mhc_itens_validos"
)

categorias_permitidas <- list(
  genero_modelo = c(
    "Mulher",
    "Homem",
    "Diversidade de gênero"
  ),
  estado_civil_modelo = c(
    "Solteiro(a)",
    "União estável",
    "Casado(a)"
  ),
  deficiencia_modelo = c(
    "Não",
    "Sim"
  ),
  moradia_modelo = c(
    "Mora sozinho(a)",
    "Com família",
    "Compartilha casa"
  ),
  etnia_modelo = c(
    "Branco",
    "Negro",
    "Indígena"
  ),
  nivel_academico_modelo = c(
    "Graduação",
    "Pós-graduação"
  )
)

walk2(
  names(categorias_permitidas),
  categorias_permitidas,
  function(nome, categorias) {
    observadas <- unique(
      na.omit(
        as.character(
          base_harmonizada_restrita[[nome]]
        )
      )
    )

    invalidas <- setdiff(
      observadas,
      categorias
    )

    if (length(invalidas) > 0L) {
      stop(
        paste0(
          "Categorias não permitidas em ",
          nome,
          ": ",
          paste(invalidas, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
)

colunas_dummy <- c(
  "genero_homem",
  "genero_diversidade",
  "estado_civil_uniao_estavel",
  "estado_civil_casado",
  "deficiencia_sim",
  "moradia_sozinha",
  "moradia_compartilha_casa",
  "etnia_negro",
  "etnia_indigena",
  "pos_graduacao"
)

auditoria_validade_dummies <- validar_dummy(
  base_harmonizada_restrita,
  colunas_dummy
)

# Garantias estruturais das referências:
# Mulher; Solteiro(a); Não deficiência; Com família; Branco; Graduação.
if (any(
  rowSums(
    base_harmonizada_restrita[
      ,
      c(
        "genero_homem",
        "genero_diversidade"
      ),
      drop = FALSE
    ],
    na.rm = TRUE
  ) > 1L
)) {
  stop(
    "Combinação impossível nas dummies de gênero.",
    call. = FALSE
  )
}

if (any(
  rowSums(
    base_harmonizada_restrita[
      ,
      c(
        "estado_civil_uniao_estavel",
        "estado_civil_casado"
      ),
      drop = FALSE
    ],
    na.rm = TRUE
  ) > 1L
)) {
  stop(
    "Combinação impossível nas dummies de estado civil.",
    call. = FALSE
  )
}

if (any(
  rowSums(
    base_harmonizada_restrita[
      ,
      c(
        "moradia_sozinha",
        "moradia_compartilha_casa"
      ),
      drop = FALSE
    ],
    na.rm = TRUE
  ) > 1L
)) {
  stop(
    "Combinação impossível nas dummies de moradia.",
    call. = FALSE
  )
}

if (any(
  rowSums(
    base_harmonizada_restrita[
      ,
      c(
        "etnia_negro",
        "etnia_indigena"
      ),
      drop = FALSE
    ],
    na.rm = TRUE
  ) > 1L
)) {
  stop(
    "Combinação impossível nas dummies de etnia/cor.",
    call. = FALSE
  )
}

# O schema é curado explicitamente. Mesmo assim, verifica nomes de campos
# identificadores diretos antes de prosseguir.
nomes_normalizados <- names(
  base_harmonizada_restrita
) %>%
  normalizar_texto() %>%
  str_replace_all("[^a-z0-9]+", "_") %>%
  str_replace_all("^_+|_+$", "")

termos_identificadores_diretos <- c(
  "nome",
  "email",
  "telefone",
  "celular",
  "whatsapp",
  "cpf",
  "documento",
  "matricula",
  "endereco",
  "timestamp",
  "data_hora",
  "redcap",
  "identificador"
)

padrao_identificadores <- paste0(
  "(^|_)(",
  paste(
    termos_identificadores_diretos,
    collapse = "|"
  ),
  ")($|_)"
)

colunas_identificadoras <- names(
  base_harmonizada_restrita
)[
  str_detect(
    nomes_normalizados,
    regex(
      padrao_identificadores,
      ignore_case = TRUE
    )
  )
]

if (length(colunas_identificadoras) > 0L) {
  stop(
    paste0(
      "A base harmonizada contém possíveis identificadores diretos: ",
      paste(
        colunas_identificadoras,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (anyNA(base_harmonizada_restrita$mhc_codigo)) {
  stop(
    "Há participantes sem classificação válida do MHC-SF.",
    call. = FALSE
  )
}

if (isTRUE(validar_contagens_dos_arquivos_atuais)) {
  if (
    nrow(base_javeriana_harmonizada) != 102L ||
      nrow(base_ufmg_harmonizada) != 125L
  ) {
    stop(
      paste0(
        "Contagem elegível inesperada. Javeriana=",
        nrow(base_javeriana_harmonizada),
        "; UFMG=",
        nrow(base_ufmg_harmonizada),
        ". Esperado: 102 e 125."
      ),
      call. = FALSE
    )
  }

  distribuicao_esperada <- c(
    `1` = 13L,
    `2` = 120L,
    `3` = 94L
  )

  distribuicao_observada <- table(
    base_harmonizada_restrita$mhc_codigo
  )

  if (!identical(
    as.integer(
      distribuicao_observada[
        names(distribuicao_esperada)
      ]
    ),
    as.integer(distribuicao_esperada)
  )) {
    stop(
      paste0(
        "Distribuição inesperada do MHC-SF: ",
        paste(
          names(distribuicao_observada),
          distribuicao_observada,
          sep = "=",
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }
}

# Remove objetos brutos e filtrados da memória após harmonização validada.
rm(
  base_javeriana_original,
  base_ufmg_original,
  base_javeriana_filtrada,
  base_ufmg_filtrada
)
invisible(gc())

# =============================================================================
# 11. BASES RESTRITAS DE ANÁLISE E MODELAGEM
# =============================================================================

# Base analítica interna: não contém identificadores diretos, mas permanece
# restrita por conter microdados individuais e instituição.
base_analitica_restrita <- base_harmonizada_restrita

# Referências explícitas para os modelos:
# - Universidade: Javeriana
# - Gênero: Mulher
# - Estado civil: Solteiro(a)
# - Deficiência: Não
# - Moradia: Com família
# - Nível acadêmico: Graduação
#
# Etnia:
# - referência nominal = Branco
# - NÃO entra no modelo principal combinado por incompatibilidade de mensuração
#   entre os dois instrumentos; permanece em descrição e sensibilidade.

covariaveis_ajuste_candidatas <- c(
  "indicador_ufmg",
  "idade_centralizada",
  "genero_homem",
  "genero_diversidade",
  "estado_civil_uniao_estavel",
  "estado_civil_casado",
  "deficiencia_sim",
  "moradia_sozinha",
  "moradia_compartilha_casa",
  "pos_graduacao",
  "anos_curso_centralizados",
  "horas_universidade_10"
)

covariaveis_etnia_sensibilidade <- c(
  "etnia_negro",
  "etnia_indigena"
)

base_modelagem_restrita <- base_harmonizada_restrita %>%
  mutate(
    indicador_ufmg = case_when(
      instituicao_origem == "Javeriana" ~ 0L,
      instituicao_origem == "UFMG" ~ 1L,
      TRUE ~ NA_integer_
    ),
    mhc_ordinal = ordered(
      mhc_codigo,
      levels = c(
        1L,
        2L,
        3L
      ),
      labels = c(
        "Languishing",
        "Moderate",
        "Flourishing"
      )
    )
  ) %>%
  select(
    mhc_codigo,
    mhc_classificacao,
    mhc_ordinal,
    all_of(praticas_comuns_20),
    all_of(covariaveis_ajuste_candidatas),
    all_of(covariaveis_etnia_sensibilidade)
  )

# Algumas categorias solicitadas podem existir no dicionário, mas não possuir
# nenhum caso na amostra elegível atual. Ex.: se "Casado(a)" tiver n=0, a dummy
# estado_civil_casado não pode entrar no modelo porque não tem variação.
#
# A categoria continua preservada no schema e na tabela de referências; apenas
# a dummy sem variação é retirada automaticamente das fórmulas desta execução.

tabela_variacao_covariaveis <- tibble(
  variavel = covariaveis_ajuste_candidatas,
  n_nao_ausente = map_int(
    covariaveis_ajuste_candidatas,
    ~ sum(
      !is.na(
        base_modelagem_restrita[[.x]]
      )
    )
  ),
  n_valores_distintos = map_int(
    covariaveis_ajuste_candidatas,
    ~ n_distinct(
      base_modelagem_restrita[[.x]],
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    entra_modelo = (
      n_nao_ausente > 0L &
        n_valores_distintos >= 2L
    )
  )

covariaveis_ajuste <- tabela_variacao_covariaveis %>%
  filter(
    entra_modelo
  ) %>%
  pull(
    variavel
  )

covariaveis_excluidas_sem_variacao <-
  tabela_variacao_covariaveis %>%
  filter(
    !entra_modelo
  ) %>%
  pull(
    variavel
  )

if (
  length(
    covariaveis_excluidas_sem_variacao
  ) > 0L
) {
  registrar(
    "COVARIÁVEIS EXCLUÍDAS DAS FÓRMULAS POR AUSÊNCIA DE VARIAÇÃO: ",
    paste(
      covariaveis_excluidas_sem_variacao,
      collapse = ", "
    )
  )
}

if (!identical(
  levels(base_modelagem_restrita$mhc_ordinal),
  c(
    "Languishing",
    "Moderate",
    "Flourishing"
  )
)) {
  stop(
    "A ordem do desfecho ordinal está incorreta.",
    call. = FALSE
  )
}

# Garantia de retirada completa de trabalho.
variaveis_trabalho_proibidas <- names(
  base_harmonizada_restrita
)[
  str_detect(
    names(base_harmonizada_restrita),
    regex(
      "trabalho|renda|salario|salário",
      ignore_case = TRUE
    )
  )
]

if (length(variaveis_trabalho_proibidas) > 0L) {
  stop(
    paste0(
      "Variáveis de trabalho/renda ainda estão na base harmonizada: ",
      paste(
        variaveis_trabalho_proibidas,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

# =============================================================================
# 12. REFERÊNCIAS, DUMMIES E DECISÕES DE HARMONIZAÇÃO
# =============================================================================

tabela_referencias <- tribble(
  ~variavel, ~categoria_referencia, ~dummy, ~categoria_com_codigo_1, ~uso,
  "Universidade/país", "Javeriana – Colômbia",
  "indicador_ufmg", "UFMG – Brasil", "Modelo principal",

  "Gênero", "Mulher",
  "genero_homem", "Homem", "Modelo principal",
  "Gênero", "Mulher",
  "genero_diversidade", "Diversidade de gênero", "Modelo principal",

  "Estado civil", "Solteiro(a)",
  "estado_civil_uniao_estavel", "União estável", "Modelo principal",
  "Estado civil", "Solteiro(a)",
  "estado_civil_casado", "Casado(a)", "Modelo principal",

  "Deficiência", "Não",
  "deficiencia_sim", "Sim", "Modelo principal",

  "Moradia", "Com família",
  "moradia_sozinha", "Mora sozinho(a)", "Modelo principal",
  "Moradia", "Com família",
  "moradia_compartilha_casa", "Compartilha casa", "Modelo principal",

  "Etnia/cor", "Branco",
  "etnia_negro", "Negro", "Descrição/sensibilidade",
  "Etnia/cor", "Branco",
  "etnia_indigena", "Indígena", "Descrição/sensibilidade",

  "Nível acadêmico", "Graduação",
  "pos_graduacao", "Pós-graduação", "Modelo principal"
) %>%
  mutate(
    codigo_referencia = paste0(
      dummy,
      "=0"
    )
  )

decisoes_harmonizacao <- tribble(
  ~tema, ~decisao, ~justificativa,

  "Deficiência",
  "Manter somente Não/Sim.",
  "Tipos de deficiência foram removidos do schema e não entram em descritivas, modelos ou exportações.",

  "Estado civil",
  "Manter Solteiro(a), União estável e Casado(a).",
  "Viúvo(a), divorciado(a) e não resposta ficam como NA; referência dos modelos é Solteiro(a).",

  "Gênero",
  "Manter Mulher, Homem e Diversidade de gênero.",
  "Os instrumentos não coletam orientação sexual; por isso não é metodologicamente válido criar 'Bissexual' a partir da variável de gênero. Referência = Mulher.",

  "Moradia",
  "Manter Mora sozinho(a), Com família e Compartilha casa.",
  "Parceiro/cônjuge é agrupado com família; referência = Com família.",

  "Etnia/cor",
  "Manter Branco, Negro e Indígena quando a informação é derivável.",
  "Na UFMG, Preta e Parda formam Negro. Na Javeriana, Negra/Raizal/Palenquera formam Negro e Indígena permanece Indígena. Nenhuma filiação étnica não é convertida em Branco. Etnia fica fora do modelo principal combinado.",

  "Trabalho",
  "Retirar completamente.",
  "Nenhuma variável de trabalho, renda ou carga horária de trabalho integra o schema ou os modelos.",

  "Nível acadêmico",
  "Manter apenas Graduação e Pós-graduação.",
  "Especialização, mestrado e doutorado são agrupados como Pós-graduação; referência = Graduação.",

  "Instituição",
  "Manter o nome apenas na base analítica restrita e criar indicador somente na base de modelagem.",
  "A informação institucional é necessária ao ajuste, mas não é exportada em microdados desidentificados.",

  "Microdados",
  "Não declarar anonimização plena.",
  "A versão individual é apenas desidentificada e restrita; a saída compartilhável é agregada com supressão de células pequenas."
)

# Dicionário compacto das variáveis sociodemográficas finais.
dicionario_sociodemografico <- tribble(
  ~variavel, ~tipo, ~categorias, ~referencia, ~modelo_principal,

  "genero_modelo", "Categórica",
  "Mulher | Homem | Diversidade de gênero",
  "Mulher", TRUE,

  "estado_civil_modelo", "Categórica",
  "Solteiro(a) | União estável | Casado(a)",
  "Solteiro(a)", TRUE,

  "deficiencia_modelo", "Categórica",
  "Não | Sim",
  "Não", TRUE,

  "moradia_modelo", "Categórica",
  "Mora sozinho(a) | Com família | Compartilha casa",
  "Com família", TRUE,

  "etnia_modelo", "Categórica",
  "Branco | Negro | Indígena",
  "Branco", FALSE,

  "nivel_academico_modelo", "Categórica",
  "Graduação | Pós-graduação",
  "Graduação", TRUE
)

# =============================================================================
# 13. DESCRITIVAS, AUSÊNCIAS, CONSISTÊNCIA E SUPRESSÃO
# =============================================================================

variaveis_continuas_descritivas <- unique(c(
  "mhc_total_14_84",
  "mhc_total_0_70",
  "idade",
  "anos_no_curso",
  "horas_universidade_semana",
  praticas_comuns_20
))

variaveis_categoricas_descritivas <- c(
  "mhc_classificacao",
  "genero_modelo",
  "estado_civil_modelo",
  "deficiencia_modelo",
  "moradia_modelo",
  "etnia_modelo",
  "nivel_academico_modelo"
)

auditoria_dummies <- auditar_dummies(
  base_harmonizada_restrita,
  colunas_dummy
)

tabela_descritiva_continua <- resumo_continuo(
  base_harmonizada_restrita,
  variaveis_continuas_descritivas
)

tabela_descritiva_categorica <- resumo_categorico(
  base_harmonizada_restrita,
  variaveis_categoricas_descritivas
)

tabela_ausencias <- relatorio_ausencias(
  base_harmonizada_restrita
)

tabela_celulas_raras <- extrair_celulas_raras(
  tabela_descritiva_categorica,
  limite = 5L
)

alpha_mhc <- map_dfr(
  c(
    "Total",
    sort(
      unique(
        base_harmonizada_restrita$instituicao_origem
      )
    )
  ),
  function(grupo) {
    dados_grupo <- if (grupo == "Total") {
      base_harmonizada_restrita
    } else {
      filter(
        base_harmonizada_restrita,
        instituicao_origem == grupo
      )
    }

    tibble(
      grupo = grupo,
      n_total = nrow(dados_grupo),
      n_casos_completos = sum(
        complete.cases(
          dados_grupo[
            ,
            itens_mhc,
            drop = FALSE
          ]
        )
      ),
      numero_itens = length(itens_mhc),
      alfa_cronbach = calcular_alpha(
        dados_grupo[
          ,
          itens_mhc,
          drop = FALSE
        ]
      )
    )
  }
)

# Perfil das novas recodificações, útil para conferir perdas por categorias
# que passaram a NA após a simplificação.
auditoria_recodificacoes <- bind_rows(
  map_dfr(
    variaveis_categoricas_descritivas[
      variaveis_categoricas_descritivas != "mhc_classificacao"
    ],
    function(v) {
      base_harmonizada_restrita %>%
        group_by(instituicao_origem) %>%
        summarise(
          variavel = v,
          n_total = n(),
          n_valido = sum(!is.na(.data[[v]])),
          n_ausente = sum(is.na(.data[[v]])),
          percentual_ausente = 100 * n_ausente / n_total,
          .groups = "drop"
        )
    }
  )
)

suprimir_celulas_categoricas <- function(
  tabela,
  limite = 5L
) {
  tabela %>%
    group_by(
      grupo,
      variavel
    ) %>%
    group_modify(
      ~ {
        dados <- .x

        primaria <- (
          !is.na(dados$n) &
            dados$n > 0L &
            dados$n < limite
        )

        secundaria <- rep(
          FALSE,
          nrow(dados)
        )

        if (
          sum(primaria, na.rm = TRUE) == 1L &&
            any(
              !primaria &
                !is.na(dados$n) &
                dados$n > 0L
            )
        ) {
          candidatas <- which(
            !primaria &
              !is.na(dados$n) &
              dados$n > 0L
          )

          escolhida <- candidatas[
            which.min(
              dados$n[candidatas]
            )
          ]

          secundaria[escolhida] <- TRUE
        }

        dados$.supressao_primaria <- primaria
        dados$.supressao_secundaria <- secundaria
        dados
      }
    ) %>%
    ungroup() %>%
    mutate(
      status_supressao = case_when(
        .supressao_primaria ~
          paste0("Primária: n<", limite),
        .supressao_secundaria ~
          "Complementar",
        TRUE ~
          "Não suprimida"
      ),
      n_exibido = case_when(
        .supressao_primaria ~
          paste0("<", limite),
        .supressao_secundaria ~
          "Suprimido",
        TRUE ~
          as.character(n)
      ),
      percentual_valido = if_else(
        .supressao_primaria |
          .supressao_secundaria,
        NA_real_,
        percentual_valido
      ),
      percentual_total = if_else(
        .supressao_primaria |
          .supressao_secundaria,
        NA_real_,
        percentual_total
      )
    ) %>%
    select(
      grupo,
      variavel,
      categoria,
      n_exibido,
      percentual_valido,
      percentual_total,
      status_supressao
    )
}

suprimir_celulas_continuas <- function(
  tabela,
  limite = 5L
) {
  tabela %>%
    mutate(
      suprimir = (
        is.na(n_valido) |
          n_valido < limite
      ),
      n_valido_exibido = if_else(
        suprimir,
        paste0("<", limite),
        as.character(n_valido)
      ),
      across(
        c(
          media,
          desvio_padrao,
          mediana,
          q1,
          q3,
          minimo,
          maximo
        ),
        ~ if_else(
          suprimir,
          NA_real_,
          as.numeric(.x)
        )
      ),
      status_supressao = if_else(
        suprimir,
        paste0("Suprimida: n<", limite),
        "Não suprimida"
      )
    ) %>%
    select(
      grupo,
      variavel,
      n_valido_exibido,
      media,
      desvio_padrao,
      mediana,
      q1,
      q3,
      minimo,
      maximo,
      status_supressao
    )
}

# 14. FUNÇÕES DA REGRESSÃO ORDINAL
# =============================================================================

formula_de_termos <- function(desfecho, termos) {
  if (length(termos) == 0L) {
    as.formula(paste(desfecho, "~ 1"))
  } else {
    reformulate(termos, response = desfecho)
  }
}

formula_unilateral <- function(termos) {
  if (length(termos) == 0L) return(NULL)
  reformulate(termos)
}

avaliar_hessiana <- function(ajuste) {
  hessiana <- ajuste$Hessian %||% NULL

  if (is.null(hessiana)) {
    return(list(
      positiva = NA,
      menor_autovalor = NA_real_,
      condicao = ajuste$cond.H %||% NA_real_
    ))
  }

  autovalores <- tryCatch(
    eigen(hessiana, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) rep(NA_real_, nrow(hessiana))
  )

  list(
    positiva = if (all(is.na(autovalores))) NA else all(autovalores > 0),
    menor_autovalor = if (all(is.na(autovalores))) NA_real_ else min(autovalores),
    condicao = ajuste$cond.H %||% tryCatch(kappa(hessiana), error = function(e) NA_real_)
  )
}

extrair_codigo_convergencia <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_integer_)

  if (is.list(x)) {
    for (nome in c("code", "codigo", "convergence", "status")) {
      if (!is.null(x[[nome]])) {
        return(extrair_codigo_convergencia(x[[nome]]))
      }
    }
    x <- unlist(x, use.names = FALSE)
  }

  if (is.data.frame(x) || is.matrix(x)) {
    x <- unlist(x, use.names = FALSE)
  }

  if (length(x) == 0L) return(NA_integer_)
  valor <- x[[1]]

  if (is.logical(valor)) {
    return(if (isTRUE(valor)) 0L else -1L)
  }
  if (is.numeric(valor)) {
    return(suppressWarnings(as.integer(valor)))
  }

  texto <- trimws(as.character(valor))
  texto_minusculo <- tolower(texto)

  if (grepl("fail|falha|error|erro|false", texto_minusculo)) return(-1L)

  numero <- regmatches(texto, regexpr("-?[0-9]+", texto))
  if (length(numero) > 0L && nzchar(numero)) {
    return(suppressWarnings(as.integer(numero)))
  }

  if (grepl("success|sucesso|converg|true", texto_minusculo)) return(0L)
  NA_integer_
}

diagnosticar_convergencia_clm <- function(ajuste) {
  codigo <- extrair_codigo_convergencia(ajuste$convergence)

  max_gradiente <- suppressWarnings(as.numeric(ajuste$maxGradient))
  if (length(max_gradiente) == 0L || !is.finite(max_gradiente[1])) {
    max_gradiente <- NA_real_
    if (!is.null(ajuste$gradient)) {
      gradiente_calculado <- suppressWarnings(
        max(abs(as.numeric(ajuste$gradient)), na.rm = TRUE)
      )
      if (is.finite(gradiente_calculado)) {
        max_gradiente <- gradiente_calculado
      }
    }
  } else {
    max_gradiente <- max_gradiente[1]
  }

  mensagem <- if (is.null(ajuste$message) || length(ajuste$message) == 0L) {
    NA_character_
  } else {
    paste(as.character(unlist(ajuste$message)), collapse = " | ")
  }

  tolerancia <- suppressWarnings(as.numeric(ajuste$control$gradTol))
  if (length(tolerancia) == 0L || !is.finite(tolerancia[1])) {
    tolerancia <- 1e-6
  } else {
    tolerancia <- tolerancia[1]
  }

  convergiu <- if (!is.na(codigo)) {
    codigo %in% c(0L, 1L)
  } else {
    is.finite(max_gradiente) && max_gradiente <= tolerancia
  }

  list(
    codigo = codigo,
    max_gradiente = max_gradiente,
    mensagem = mensagem,
    tolerancia = tolerancia,
    convergiu = convergiu
  )
}

ajustar_clm_seguro <- function(
  dados,
  termos,
  nome,
  nominal_termos = character(0),
  scale_termos = character(0)
) {
  necessarias <- unique(c(
    "mhc_ordinal", "mhc_codigo",
    termos, nominal_termos, scale_termos
  ))

  ausentes <- setdiff(necessarias, names(dados))
  if (length(ausentes) > 0L) {
    return(list(
      ok = FALSE,
      nome = nome,
      erro = paste0("Variáveis ausentes: ", paste(ausentes, collapse = ", "))
    ))
  }

  base <- dados %>%
    dplyr::select(dplyr::all_of(necessarias)) %>%
    dplyr::filter(stats::complete.cases(.))

  if (nrow(base) == 0L) {
    return(list(ok = FALSE, nome = nome, erro = "Nenhum caso completo."))
  }

  if (dplyr::n_distinct(base$mhc_ordinal) < 3L) {
    return(list(
      ok = FALSE,
      nome = nome,
      erro = "O desfecho não possui os três níveis na amostra do modelo."
    ))
  }

  preditores_verificados <- unique(c(termos, nominal_termos, scale_termos))
  sem_variacao <- preditores_verificados[
    vapply(
      base[preditores_verificados],
      function(x) dplyr::n_distinct(x, na.rm = TRUE) < 2L,
      logical(1)
    )
  ]

  if (length(sem_variacao) > 0L) {
    return(list(
      ok = FALSE,
      nome = nome,
      erro = paste0("Preditor sem variação: ", paste(sem_variacao, collapse = ", "))
    ))
  }

  formula_principal <- formula_de_termos("mhc_ordinal", termos)
  formula_nominal <- formula_unilateral(nominal_termos)
  formula_scale <- formula_unilateral(scale_termos)
  avisos <- character(0)

  argumentos_clm <- list(
    formula = formula_principal,
    data = base,
    link = "logit",
    Hess = TRUE,
    control = ordinal::clm.control(maxIter = 1000, gradTol = 1e-6)
  )
  if (!is.null(formula_nominal)) argumentos_clm$nominal <- formula_nominal
  if (!is.null(formula_scale)) argumentos_clm$scale <- formula_scale

  ajuste <- tryCatch(
    withCallingHandlers(
      do.call(ordinal::clm, argumentos_clm),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(ajuste, "error")) {
    return(list(
      ok = FALSE,
      nome = nome,
      erro = conditionMessage(ajuste),
      avisos = paste(unique(avisos), collapse = " | ")
    ))
  }

  matriz_var <- tryCatch(stats::vcov(ajuste), error = function(e) NULL)
  coeficientes <- tryCatch(stats::coef(ajuste), error = function(e) NULL)
  hess <- avaliar_hessiana(ajuste)
  diagnostico <- diagnosticar_convergencia_clm(ajuste)

  codigo_convergencia <- diagnostico$codigo
  max_gradiente <- diagnostico$max_gradiente
  mensagem_convergencia <- diagnostico$mensagem
  convergiu <- isTRUE(diagnostico$convergiu)

  otimo_unico <- if (!is.na(codigo_convergencia)) {
    codigo_convergencia == 0L
  } else {
    convergiu && !isFALSE(hess$positiva)
  }

  coef_finitos <- !is.null(coeficientes) && all(is.finite(coeficientes))
  vcov_finita <- !is.null(matriz_var) && all(is.finite(matriz_var))

  status <- dplyr::case_when(
    !convergiu ~ "Falha de convergência",
    convergiu && !otimo_unico ~ "Convergiu para ótimo não único",
    !coef_finitos ~ "Coeficientes não finitos",
    !vcov_finita ~ "Matriz de variância inválida",
    isFALSE(hess$positiva) ~ "Hessiana não positiva",
    is.finite(hess$condicao) && hess$condicao > 1e6 ~
      "Convergiu, mas Hessiana muito mal condicionada",
    TRUE ~ "Válido"
  )

  contagens <- table(base$mhc_codigo)
  modelo_valido <- otimo_unico && coef_finitos && vcov_finita &&
    !isFALSE(hess$positiva) &&
    !(is.finite(hess$condicao) && hess$condicao > 1e6)

  list(
    ok = modelo_valido,
    nome = nome,
    ajuste = ajuste,
    dados = base,
    formula = deparse(formula_principal),
    nominal = paste(nominal_termos, collapse = " + "),
    scale = paste(scale_termos, collapse = " + "),
    n = nrow(base),
    n_definhamento = obter_contagem_tabela(contagens, "1"),
    n_moderado = obter_contagem_tabela(contagens, "2"),
    n_florescimento = obter_contagem_tabela(contagens, "3"),
    n_preditores = length(termos),
    n_parametros = length(stats::coef(ajuste)),
    codigo_convergencia = codigo_convergencia,
    mensagem_convergencia = mensagem_convergencia,
    convergiu = convergiu,
    otimo_unico = otimo_unico,
    coef_finitos = coef_finitos,
    vcov_finita = vcov_finita,
    hessiana_positiva = hess$positiva,
    menor_autovalor = hess$menor_autovalor,
    condicao_hessiana = hess$condicao,
    max_gradiente = max_gradiente,
    status = status,
    avisos = paste(unique(avisos), collapse = " | "),
    erro = NA_character_
  )
}

resumo_modelo <- function(objeto) {
  if (!isTRUE(objeto$ok) && is.null(objeto$ajuste)) {
    return(tibble(
      modelo = objeto$nome,
      n = NA_integer_,
      n_definhamento = NA_integer_,
      n_moderado = NA_integer_,
      n_florescimento = NA_integer_,
      n_preditores = NA_integer_,
      n_parametros = NA_integer_,
      logLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      convergiu = FALSE,
      hessiana_positiva = NA,
      condicao_hessiana = NA_real_,
      max_gradiente = NA_real_,
      status = "Erro",
      formula = NA_character_,
      nominal = NA_character_,
      scale = NA_character_,
      alerta_sobreparametrizacao = NA_character_,
      avisos = objeto$avisos %||% NA_character_,
      erro = objeto$erro %||% NA_character_
    ))
  }

  ajuste <- objeto$ajuste
  parametros_localizacao <- length(ajuste$beta %||% numeric(0))
  menor_grupo <- min(
    objeto$n_definhamento,
    objeto$n_moderado,
    objeto$n_florescimento
  )

  razao <- if (parametros_localizacao > 0L) {
    menor_grupo / parametros_localizacao
  } else {
    NA_real_
  }

  tibble(
    modelo = objeto$nome,
    n = objeto$n,
    n_definhamento = objeto$n_definhamento,
    n_moderado = objeto$n_moderado,
    n_florescimento = objeto$n_florescimento,
    n_preditores = objeto$n_preditores,
    n_parametros = objeto$n_parametros,
    logLik = as.numeric(logLik(ajuste)),
    AIC = AIC(ajuste),
    BIC = BIC(ajuste),
    convergiu = objeto$convergiu,
    hessiana_positiva = objeto$hessiana_positiva,
    condicao_hessiana = objeto$condicao_hessiana,
    max_gradiente = objeto$max_gradiente,
    status = objeto$status,
    formula = objeto$formula,
    nominal = objeto$nominal,
    scale = objeto$scale,
    alerta_sobreparametrizacao = case_when(
      is.na(razao) ~ NA_character_,
      razao < 1 ~ "Crítico: menos de 1 caso na menor categoria por parâmetro de localização.",
      razao < 5 ~ "Atenção: menos de 5 casos na menor categoria por parâmetro de localização.",
      TRUE ~ "Sem alerta por esta regra descritiva."
    ),
    avisos = objeto$avisos,
    erro = objeto$erro
  )
}

extrair_coeficientes <- function(objeto) {
  if (is.null(objeto$ajuste)) {
    return(tibble(
      modelo = objeto$nome,
      termo = NA_character_,
      tipo_parametro = NA_character_,
      estimativa = NA_real_,
      erro_padrao = NA_real_,
      z = NA_real_,
      p = NA_real_,
      OR = NA_real_,
      IC95_inf = NA_real_,
      IC95_sup = NA_real_
    ))
  }

  ajuste <- objeto$ajuste
  matriz <- coef(summary(ajuste))
  tabela <- as.data.frame(matriz, check.names = FALSE)
  tabela$termo <- rownames(tabela)

  nomes_colunas <- names(tabela)
  col_est <- nomes_colunas[str_detect(nomes_colunas, "^Estimate$")][1]
  col_se <- nomes_colunas[str_detect(nomes_colunas, "Std\\. Error")][1]
  col_z <- nomes_colunas[str_detect(nomes_colunas, "z value")][1]
  col_p <- nomes_colunas[str_detect(nomes_colunas, "Pr\\(")][1]

  thresholds <- names(ajuste$alpha %||% numeric(0))
  termos_localizacao <- names(ajuste$beta %||% numeric(0))

  tabela %>%
    transmute(
      modelo = objeto$nome,
      termo = termo,
      tipo_parametro = case_when(
        termo %in% thresholds ~ "Limiar",
        termo %in% termos_localizacao ~ "Localização/OR proporcional",
        TRUE ~ "Nominal ou escala – coeficiente bruto"
      ),
      estimativa = .data[[col_est]],
      erro_padrao = .data[[col_se]],
      z = .data[[col_z]],
      p = .data[[col_p]],
      OR = if_else(
        tipo_parametro == "Localização/OR proporcional",
        exp(estimativa),
        NA_real_
      ),
      IC95_inf = if_else(
        tipo_parametro == "Localização/OR proporcional",
        exp(estimativa - qnorm(0.975) * erro_padrao),
        NA_real_
      ),
      IC95_sup = if_else(
        tipo_parametro == "Localização/OR proporcional",
        exp(estimativa + qnorm(0.975) * erro_padrao),
        NA_real_
      )
    )
}

executar_teste_ordinal <- function(objeto, tipo = c("nominal", "scale")) {
  tipo <- match.arg(tipo)

  if (is.null(objeto$ajuste)) {
    return(tibble(
      modelo = objeto$nome,
      teste = tipo,
      termo = NA_character_,
      Df = NA_real_,
      logLik = NA_real_,
      AIC = NA_real_,
      LRT = NA_real_,
      p = NA_real_,
      status = "Modelo indisponível",
      erro = objeto$erro %||% NA_character_
    ))
  }

  resultado <- tryCatch(
    {
      x <- if (tipo == "nominal") {
        ordinal::nominal_test(objeto$ajuste)
      } else {
        ordinal::scale_test(objeto$ajuste)
      }

      df <- as.data.frame(x, check.names = FALSE)
      df$termo <- rownames(df)

      col_p <- names(df)[str_detect(names(df), "Pr\\(")][1]

      tibble(
        modelo = objeto$nome,
        teste = tipo,
        termo = df$termo,
        Df = if ("Df" %in% names(df)) df$Df else NA_real_,
        logLik = if ("logLik" %in% names(df)) df$logLik else NA_real_,
        AIC = if ("AIC" %in% names(df)) df$AIC else NA_real_,
        LRT = if ("LRT" %in% names(df)) df$LRT else NA_real_,
        p = if (!is.na(col_p)) df[[col_p]] else NA_real_,
        status = "Executado",
        erro = NA_character_
      )
    },
    error = function(e) {
      tibble(
        modelo = objeto$nome,
        teste = tipo,
        termo = NA_character_,
        Df = NA_real_,
        logLik = NA_real_,
        AIC = NA_real_,
        LRT = NA_real_,
        p = NA_real_,
        status = "Erro",
        erro = conditionMessage(e)
      )
    }
  )

  resultado
}

comparar_modelos_lrt <- function(modelo_reduzido, modelo_completo, nome) {
  if (
    is.null(modelo_reduzido$ajuste) ||
      is.null(modelo_completo$ajuste) ||
      !isTRUE(modelo_reduzido$ok) ||
      !isTRUE(modelo_completo$ok)
  ) {
    return(tibble(
      comparacao = nome,
      Df = NA_real_,
      AIC_reduzido = if (!is.null(modelo_reduzido$ajuste)) AIC(modelo_reduzido$ajuste) else NA_real_,
      AIC_completo = if (!is.null(modelo_completo$ajuste)) AIC(modelo_completo$ajuste) else NA_real_,
      logLik_reduzido = if (!is.null(modelo_reduzido$ajuste)) as.numeric(logLik(modelo_reduzido$ajuste)) else NA_real_,
      logLik_completo = if (!is.null(modelo_completo$ajuste)) as.numeric(logLik(modelo_completo$ajuste)) else NA_real_,
      LR_stat = NA_real_,
      p = NA_real_,
      status = "Modelo indisponível ou inválido; LRT não calculado"
    ))
  }

  if (modelo_reduzido$n != modelo_completo$n) {
    return(tibble(
      comparacao = nome,
      Df = NA_real_,
      AIC_reduzido = AIC(modelo_reduzido$ajuste),
      AIC_completo = AIC(modelo_completo$ajuste),
      logLik_reduzido = as.numeric(logLik(modelo_reduzido$ajuste)),
      logLik_completo = as.numeric(logLik(modelo_completo$ajuste)),
      LR_stat = NA_real_,
      p = NA_real_,
      status = "Amostras diferentes; LRT não calculado"
    ))
  }

  lr <- 2 * (
    as.numeric(logLik(modelo_completo$ajuste)) -
      as.numeric(logLik(modelo_reduzido$ajuste))
  )
  df <- attr(logLik(modelo_completo$ajuste), "df") -
    attr(logLik(modelo_reduzido$ajuste), "df")
  p <- pchisq(lr, df = df, lower.tail = FALSE)

  tibble(
    comparacao = nome,
    Df = df,
    AIC_reduzido = AIC(modelo_reduzido$ajuste),
    AIC_completo = AIC(modelo_completo$ajuste),
    logLik_reduzido = as.numeric(logLik(modelo_reduzido$ajuste)),
    logLik_completo = as.numeric(logLik(modelo_completo$ajuste)),
    LR_stat = lr,
    p = p,
    status = "Calculado"
  )
}

calcular_vif <- function(dados, termos) {
  dados <- dados %>%
    select(all_of(termos)) %>%
    filter(complete.cases(.))

  matriz <- model.matrix(
    reformulate(termos),
    data = dados
  )

  if ("(Intercept)" %in% colnames(matriz)) {
    matriz <- matriz[, colnames(matriz) != "(Intercept)", drop = FALSE]
  }

  if (ncol(matriz) == 0L) return(tibble())

  if (ncol(matriz) == 1L) {
    return(tibble(
      termo_matriz = colnames(matriz),
      VIF = 1,
      tolerancia = 1
    ))
  }

  map_dfr(
    seq_len(ncol(matriz)),
    function(i) {
      y <- matriz[, i]
      x <- matriz[, -i, drop = FALSE]

      ajuste <- lm.fit(cbind(1, x), y)
      ss_res <- sum(ajuste$residuals^2)
      ss_tot <- sum((y - mean(y))^2)
      r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
      vif <- if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf

      tibble(
        termo_matriz = colnames(matriz)[i],
        VIF = vif,
        tolerancia = if (is.finite(vif) && vif > 0) 1 / vif else 0
      )
    }
  )
}

numero_condicao <- function(dados, termos) {
  dados_completos <- dados %>%
    dplyr::select(dplyr::all_of(termos)) %>%
    dplyr::filter(stats::complete.cases(.))

  matriz <- stats::model.matrix(
    stats::reformulate(termos),
    data = dados_completos
  )

  if ("(Intercept)" %in% colnames(matriz)) {
    matriz <- matriz[
      ,
      colnames(matriz) != "(Intercept)",
      drop = FALSE
    ]
  }

  if (ncol(matriz) < 2L) {
    return(1)
  }

  desvios <- apply(
    matriz,
    2,
    stats::sd,
    na.rm = TRUE
  )

  manter <- is.finite(desvios) & desvios > 0

  matriz <- matriz[
    ,
    manter,
    drop = FALSE
  ]

  if (ncol(matriz) < 2L) {
    return(1)
  }

  matriz_padronizada <- scale(matriz)

  tryCatch(
    kappa(
      matriz_padronizada,
      exact = TRUE
    ),
    error = function(e) NA_real_
  )
}

avaliar_classificacao <- function(objeto) {
  
  # ----------------------------------------------------------
  # 1. Verifica se o modelo está disponível
  # ----------------------------------------------------------
  
  if (
    is.null(objeto$ajuste) ||
    is.null(objeto$dados)
  ) {
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome %||% NA_character_,
        n = objeto$n %||% NA_integer_,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = "Modelo indisponível"
      ),
      confusao = tibble::tibble()
    ))
  }
  
  # ----------------------------------------------------------
  # 2. Identifica a variável resposta do modelo
  # ----------------------------------------------------------
  
  nome_resposta <- tryCatch(
    all.vars(
      stats::formula(objeto$ajuste)
    )[1],
    error = function(e) NA_character_
  )
  
  if (
    is.na(nome_resposta) ||
    !nome_resposta %in% names(objeto$dados)
  ) {
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome,
        n = objeto$n,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = "Variável resposta não localizada"
      ),
      confusao = tibble::tibble()
    ))
  }
  
  # ----------------------------------------------------------
  # 3. Retira a resposta antes da predição
  # ----------------------------------------------------------
  # Isso obriga predict.clm() a devolver uma coluna de
  # probabilidade para cada categoria do MHC.
  
  dados_predicao <- objeto$dados[
    ,
    setdiff(
      names(objeto$dados),
      nome_resposta
    ),
    drop = FALSE
  ]
  
  resultado_predicao <- tryCatch(
    stats::predict(
      objeto$ajuste,
      newdata = dados_predicao,
      type = "prob"
    ),
    error = function(e) e
  )
  
  if (inherits(resultado_predicao, "error")) {
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome,
        n = objeto$n,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = paste0(
          "Falha na predição: ",
          conditionMessage(resultado_predicao)
        )
      ),
      confusao = tibble::tibble()
    ))
  }
  
  probabilidades <- resultado_predicao$fit
  
  # ----------------------------------------------------------
  # 4. Converte explicitamente para matriz numérica
  # ----------------------------------------------------------
  
  if (is.data.frame(probabilidades)) {
    probabilidades <- as.matrix(probabilidades)
  }
  
  if (
    is.null(dim(probabilidades)) ||
    length(dim(probabilidades)) != 2L
  ) {
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome,
        n = objeto$n,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = paste0(
          "Predição não retornou matriz de probabilidades. ",
          "Classe recebida: ",
          paste(class(probabilidades), collapse = ", ")
        )
      ),
      confusao = tibble::tibble()
    ))
  }
  
  nomes_classes <- colnames(probabilidades)
  
  probabilidades <- matrix(
    suppressWarnings(
      as.numeric(probabilidades)
    ),
    nrow = nrow(probabilidades),
    ncol = ncol(probabilidades),
    dimnames = list(
      rownames(probabilidades),
      nomes_classes
    )
  )
  
  if (any(!is.finite(probabilidades))) {
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome,
        n = objeto$n,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = "Probabilidades não numéricas ou não finitas"
      ),
      confusao = tibble::tibble()
    ))
  }
  
  # ----------------------------------------------------------
  # 5. Recupera os nomes das categorias
  # ----------------------------------------------------------
  
  niveis_resposta <- levels(
    objeto$dados[[nome_resposta]]
  )
  
  if (
    is.null(colnames(probabilidades)) &&
    length(niveis_resposta) == ncol(probabilidades)
  ) {
    colnames(probabilidades) <- niveis_resposta
  }
  
  classes <- colnames(probabilidades)
  
  if (
    is.null(classes) ||
    length(classes) != ncol(probabilidades)
  ) {
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome,
        n = objeto$n,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = "Categorias das probabilidades não identificadas"
      ),
      confusao = tibble::tibble()
    ))
  }
  
  # ----------------------------------------------------------
  # 6. Confere número de linhas
  # ----------------------------------------------------------
  
  observados <- as.character(
    objeto$dados[[nome_resposta]]
  )
  
  if (nrow(probabilidades) != length(observados)) {
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome,
        n = objeto$n,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = paste0(
          "Número de predições diferente do número de observações: ",
          nrow(probabilidades),
          " versus ",
          length(observados)
        )
      ),
      confusao = tibble::tibble()
    ))
  }
  
  if (!all(observados %in% classes)) {
    categorias_incompativeis <- setdiff(
      unique(observados),
      classes
    )
    
    return(list(
      resumo = tibble::tibble(
        modelo = objeto$nome,
        n = objeto$n,
        acuracia_aparente = NA_real_,
        brier_multiclasse = NA_real_,
        status = paste0(
          "Categorias observadas não coincidem com as preditas: ",
          paste(
            categorias_incompativeis,
            collapse = ", "
          )
        )
      ),
      confusao = tibble::tibble()
    ))
  }
  
  # ----------------------------------------------------------
  # 7. Classes preditas
  # ----------------------------------------------------------
  
  classes_preditas <- classes[
    max.col(
      probabilidades,
      ties.method = "first"
    )
  ]
  
  # ----------------------------------------------------------
  # 8. Matriz one-hot numérica
  # ----------------------------------------------------------
  
  one_hot <- matrix(
    0,
    nrow = length(observados),
    ncol = length(classes),
    dimnames = list(
      NULL,
      classes
    )
  )
  
  posicao_observada <- match(
    observados,
    classes
  )
  
  one_hot[
    cbind(
      seq_along(observados),
      posicao_observada
    )
  ] <- 1
  
  # ----------------------------------------------------------
  # 9. Métricas aparentes
  # ----------------------------------------------------------
  
  brier <- mean(
    rowSums(
      (probabilidades - one_hot)^2
    )
  )
  
  acuracia <- mean(
    classes_preditas == observados
  )
  
  # ----------------------------------------------------------
  # 10. Matriz de confusão
  # ----------------------------------------------------------
  
  confusao <- as.data.frame.matrix(
    table(
      observado = factor(
        observados,
        levels = classes
      ),
      predito = factor(
        classes_preditas,
        levels = classes
      )
    )
  ) %>%
    tibble::rownames_to_column(
      "observado"
    ) %>%
    tibble::as_tibble()
  
  list(
    resumo = tibble::tibble(
      modelo = objeto$nome,
      n = length(observados),
      acuracia_aparente = acuracia,
      brier_multiclasse = brier,
      status = "Calculado"
    ),
    confusao = confusao
  )
}

ajustar_limiares_binarios <- function(
  dados,
  termos,
  termos_interesse,
  nome_modelo
) {
  base <- dados %>%
    select(mhc_codigo, all_of(termos)) %>%
    filter(complete.cases(.))

  limiares <- list(
    `Moderado/Florescimento versus Definhamento` =
      as.integer(base$mhc_codigo >= 2L),
    `Florescimento versus Definhamento/Moderado` =
      as.integer(base$mhc_codigo >= 3L)
  )

  map_dfr(
    names(limiares),
    function(limiar) {
      base$.y_bin <- limiares[[limiar]]
      formula <- formula_de_termos(".y_bin", termos)

      ajuste <- tryCatch(
        glm(
          formula,
          data = base,
          family = binomial(link = "logit")
        ),
        error = function(e) e
      )

      if (inherits(ajuste, "error")) {
        return(tibble(
          modelo = nome_modelo,
          limiar = limiar,
          termo = termos_interesse,
          estimativa = NA_real_,
          erro_padrao = NA_real_,
          p = NA_real_,
          OR = NA_real_,
          IC95_inf = NA_real_,
          IC95_sup = NA_real_,
          status = paste0("Erro: ", conditionMessage(ajuste))
        ))
      }

      coef_tab <- coef(summary(ajuste))

      map_dfr(
        termos_interesse,
        function(termo) {
          if (!termo %in% rownames(coef_tab)) {
            return(tibble(
              modelo = nome_modelo,
              limiar = limiar,
              termo = termo,
              estimativa = NA_real_,
              erro_padrao = NA_real_,
              p = NA_real_,
              OR = NA_real_,
              IC95_inf = NA_real_,
              IC95_sup = NA_real_,
              status = "Termo ausente"
            ))
          }

          estimativa <- coef_tab[termo, "Estimate"]
          ep <- coef_tab[termo, "Std. Error"]

          tibble(
            modelo = nome_modelo,
            limiar = limiar,
            termo = termo,
            estimativa = estimativa,
            erro_padrao = ep,
            p = coef_tab[termo, "Pr(>|z|)"],
            OR = exp(estimativa),
            IC95_inf = exp(estimativa - qnorm(0.975) * ep),
            IC95_sup = exp(estimativa + qnorm(0.975) * ep),
            status = if_else(
              all(is.finite(c(estimativa, ep))),
              "Calculado",
              "Estimativa não finita"
            )
          )
        }
      )
    }
  )
}

# =============================================================================

# =============================================================================
# 15. MODELOS
# =============================================================================

# Amostra comum dos modelos principais.
# Etnia não entra aqui devido à diferença de mensuração entre os instrumentos.
base_ajuste_comum <- base_modelagem_restrita %>%
  select(
    mhc_codigo,
    mhc_classificacao,
    mhc_ordinal,
    all_of(covariaveis_ajuste),
    all_of(praticas_comuns_20)
  ) %>%
  filter(
    complete.cases(.)
  )

tamanho_amostras_modelo <- tibble(
  base = c(
    "Base de modelagem total",
    "Amostra comum dos modelos principais"
  ),
  n = c(
    nrow(base_modelagem_restrita),
    nrow(base_ajuste_comum)
  ),
  languishing = c(
    sum(
      base_modelagem_restrita$mhc_codigo == 1L,
      na.rm = TRUE
    ),
    sum(
      base_ajuste_comum$mhc_codigo == 1L,
      na.rm = TRUE
    )
  ),
  moderate = c(
    sum(
      base_modelagem_restrita$mhc_codigo == 2L,
      na.rm = TRUE
    ),
    sum(
      base_ajuste_comum$mhc_codigo == 2L,
      na.rm = TRUE
    )
  ),
  flourishing = c(
    sum(
      base_modelagem_restrita$mhc_codigo == 3L,
      na.rm = TRUE
    ),
    sum(
      base_ajuste_comum$mhc_codigo == 3L,
      na.rm = TRUE
    )
  )
)

modelo_nulo <- ajustar_clm_seguro(
  base_ajuste_comum,
  character(0),
  "M0 – Nulo"
)

blocos_sociodemograficos <- list(
  `Instituição/país` = "indicador_ufmg",
  Idade = "idade_centralizada",
  Gênero = c(
    "genero_homem",
    "genero_diversidade"
  ),
  `Estado civil` = c(
    "estado_civil_uniao_estavel",
    "estado_civil_casado"
  ),
  Deficiência = "deficiencia_sim",
  Moradia = c(
    "moradia_sozinha",
    "moradia_compartilha_casa"
  ),
  `Nível acadêmico` = "pos_graduacao",
  `Tempo no curso` = "anos_curso_centralizados",
  `Horas na universidade` = "horas_universidade_10"
)

modelos_sociodemograficos <- imap(
  blocos_sociodemograficos,
  ~ ajustar_clm_seguro(
    base_ajuste_comum,
    .x,
    paste0(
      "S – ",
      .y
    )
  )
)

modelos_praticas_brutos <- setNames(
  map(
    praticas_comuns_20,
    ~ ajustar_clm_seguro(
      base_modelagem_restrita,
      .x,
      paste0(
        "Bruto – ",
        .x
      )
    )
  ),
  praticas_comuns_20
)

modelos_praticas_ajustados <- setNames(
  map(
    praticas_comuns_20,
    ~ ajustar_clm_seguro(
      base_ajuste_comum,
      c(
        .x,
        covariaveis_ajuste
      ),
      paste0(
        "Ajustado – ",
        .x
      )
    )
  ),
  praticas_comuns_20
)

# Não interrompe o script por uma falha isolada.
# Todas as falhas ficam registradas no relatório final.
falhas_modelos_principais <- names(
  modelos_praticas_ajustados
)[
  !vapply(
    modelos_praticas_ajustados,
    function(x) isTRUE(x$ok),
    logical(1)
  )
]

tabela_falhas_modelos_principais <- if (
  length(falhas_modelos_principais) > 0L
) {
  map_dfr(
    falhas_modelos_principais,
    function(nome) {
      x <- modelos_praticas_ajustados[[nome]]

      tibble(
        pratica = nome,
        status = x$status %||% "Erro",
        erro = x$erro %||% NA_character_,
        codigo_convergencia = x$codigo_convergencia %||% NA_integer_,
        max_gradiente = x$max_gradiente %||% NA_real_,
        condicao_hessiana = x$condicao_hessiana %||% NA_real_
      )
    }
  )
} else {
  tibble(
    pratica = character(0),
    status = character(0),
    erro = character(0),
    codigo_convergencia = integer(0),
    max_gradiente = numeric(0),
    condicao_hessiana = numeric(0)
  )
}

termos_B1 <- c(
  "indicador_ufmg",
  "idade_centralizada"
)

termos_B2 <- unique(c(
  termos_B1,
  intersect(
    c(
      "genero_homem",
      "genero_diversidade"
    ),
    covariaveis_ajuste
  )
))

termos_B3 <- covariaveis_ajuste

termos_B4 <- c(
  termos_B3,
  praticas_teoricas_13
)

termos_B5 <- c(
  termos_B3,
  praticas_comuns_20
)

modelos_blocos <- list(
  B1 = ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B1,
    "B1 – Instituição/país + idade"
  ),
  B2 = ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B2,
    "B2 – + gênero"
  ),
  B3 = ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B3,
    "B3 – Ajuste sociodemográfico simplificado"
  ),
  B4 = ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B4,
    "B4 – Total teórico com 13 práticas"
  ),
  B5 = ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B5,
    "B5 – Exploratório com 20 práticas"
  )
)

todos_modelos_po <- c(
  list(Nulo = modelo_nulo),
  modelos_sociodemograficos,
  modelos_praticas_brutos,
  modelos_praticas_ajustados,
  modelos_blocos
)

resumo_modelos_po <- map_dfr(
  todos_modelos_po,
  resumo_modelo
)

coeficientes_po <- map_dfr(
  todos_modelos_po,
  extrair_coeficientes
)

coef_praticas_brutas <- map_dfr(
  modelos_praticas_brutos,
  extrair_coeficientes
) %>%
  filter(
    tipo_parametro ==
      "Localização/OR proporcional",
    termo %in% praticas_comuns_20
  ) %>%
  mutate(
    p_FDR_BH = p.adjust(
      p,
      method = "BH"
    )
  )

coef_praticas_ajustadas <- map_dfr(
  modelos_praticas_ajustados,
  extrair_coeficientes
) %>%
  filter(
    tipo_parametro ==
      "Localização/OR proporcional",
    termo %in% praticas_comuns_20
  ) %>%
  mutate(
    p_FDR_BH = p.adjust(
      p,
      method = "BH"
    )
  )

comparacoes_blocos <- bind_rows(
  comparar_modelos_lrt(
    modelos_blocos$B1,
    modelos_blocos$B2,
    "B1 versus B2"
  ),
  comparar_modelos_lrt(
    modelos_blocos$B2,
    modelos_blocos$B3,
    "B2 versus B3"
  ),
  comparar_modelos_lrt(
    modelos_blocos$B3,
    modelos_blocos$B4,
    "B3 versus B4"
  ),
  comparar_modelos_lrt(
    modelos_blocos$B4,
    modelos_blocos$B5,
    "B4 versus B5"
  )
)

teste_nominal_B4 <- executar_teste_ordinal(
  modelos_blocos$B4,
  "nominal"
)

teste_scale_B4 <- executar_teste_ordinal(
  modelos_blocos$B4,
  "scale"
)

teste_nominal_B5 <- executar_teste_ordinal(
  modelos_blocos$B5,
  "nominal"
)

teste_scale_B5 <- executar_teste_ordinal(
  modelos_blocos$B5,
  "scale"
)

extrair_termos_significativos <- function(
  tabela,
  termos_validos
) {
  tabela %>%
    filter(
      status == "Executado",
      !is.na(p),
      p < alpha,
      termo %in% termos_validos
    ) %>%
    arrange(
      p,
      termo
    ) %>%
    distinct(
      termo,
      .keep_all = TRUE
    ) %>%
    pull(
      termo
    )
}

termos_nominais_B4 <- extrair_termos_significativos(
  teste_nominal_B4,
  termos_B4
)

termos_scale_B4 <- extrair_termos_significativos(
  teste_scale_B4,
  termos_B4
)

# Limite operacional de até três termos, agora escolhidos pelo menor p.
termos_nominais_ajuste <- head(
  termos_nominais_B4,
  3L
)

termos_scale_ajuste <- head(
  termos_scale_B4,
  3L
)

modelo_ppo_B4 <- if (
  length(termos_nominais_ajuste) > 0L
) {
  ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B4,
    "B4-PPO – chances proporcionais parciais",
    nominal_termos = termos_nominais_ajuste
  )
} else {
  list(
    ok = FALSE,
    nome =
      "B4-PPO – chances proporcionais parciais",
    erro =
      "Nenhum termo significativo no nominal_test() do B4."
  )
}

modelo_scale_B4 <- if (
  length(termos_scale_ajuste) > 0L
) {
  ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B4,
    "B4-SCALE – efeito de escala",
    scale_termos = termos_scale_ajuste
  )
} else {
  list(
    ok = FALSE,
    nome =
      "B4-SCALE – efeito de escala",
    erro =
      "Nenhum termo significativo no scale_test() do B4."
  )
}

comparacao_PO_PPO <- comparar_modelos_lrt(
  modelos_blocos$B4,
  modelo_ppo_B4,
  "B4-PO versus B4-PPO"
)

comparacao_PO_SCALE <- comparar_modelos_lrt(
  modelos_blocos$B4,
  modelo_scale_B4,
  "B4-PO versus B4-SCALE"
)

coeficientes_PPO <- extrair_coeficientes(
  modelo_ppo_B4
)

coeficientes_SCALE <- extrair_coeficientes(
  modelo_scale_B4
)

or_limiares <- if (
  length(termos_nominais_ajuste) > 0L
) {
  ajustar_limiares_binarios(
    base_ajuste_comum,
    termos_B4,
    termos_nominais_ajuste,
    "Regressões binárias complementares por ponto de corte – B4"
  )
} else {
  tibble(
    modelo = character(0),
    limiar = character(0),
    termo = character(0),
    estimativa = numeric(0),
    erro_padrao = numeric(0),
    p = numeric(0),
    OR = numeric(0),
    IC95_inf = numeric(0),
    IC95_sup = numeric(0),
    status = character(0)
  )
}

vif_B4 <- calcular_vif(
  base_ajuste_comum,
  termos_B4
)

vif_B5 <- calcular_vif(
  base_ajuste_comum,
  termos_B5
)

condicao_modelos <- tibble(
  modelo = c(
    "B4 – 13 práticas",
    "B5 – 20 práticas"
  ),
  numero_condicao = c(
    numero_condicao(
      base_ajuste_comum,
      termos_B4
    ),
    numero_condicao(
      base_ajuste_comum,
      termos_B5
    )
  )
)

avaliacao_B4 <- avaliar_classificacao(
  modelos_blocos$B4
)

avaliacao_B5 <- avaliar_classificacao(
  modelos_blocos$B5
)

avaliacao_PPO <- avaliar_classificacao(
  modelo_ppo_B4
)

avaliacao_SCALE <- avaliar_classificacao(
  modelo_scale_B4
)

classificacao_resumo <- bind_rows(
  avaliacao_B4$resumo,
  avaliacao_B5$resumo,
  avaliacao_PPO$resumo,
  avaliacao_SCALE$resumo
)

# ---------------------------------------------------------------------------
# Sensibilidade de etnia/cor
# ---------------------------------------------------------------------------
#
# Não é modelo principal, pois Javeriana e UFMG não medem exatamente o mesmo
# construto. A referência é Branco sempre que a categoria está observada.

variacao_etnia_sensibilidade <- tibble(
  variavel = covariaveis_etnia_sensibilidade,
  n_valores_distintos = map_int(
    covariaveis_etnia_sensibilidade,
    ~ n_distinct(
      base_modelagem_restrita[[.x]],
      na.rm = TRUE
    )
  )
)

covariaveis_etnia_sensibilidade_validas <-
  variacao_etnia_sensibilidade %>%
  filter(
    n_valores_distintos >= 2L
  ) %>%
  pull(
    variavel
  )

termos_modelo_etnia_sensibilidade <- unique(c(
  covariaveis_ajuste,
  covariaveis_etnia_sensibilidade_validas
))

modelo_etnia_sensibilidade <- if (
  length(
    covariaveis_etnia_sensibilidade_validas
  ) > 0L
) {
  ajustar_clm_seguro(
    base_modelagem_restrita,
    termos_modelo_etnia_sensibilidade,
    "Sensibilidade – sociodemográfico + etnia/cor"
  )
} else {
  list(
    ok = FALSE,
    nome =
      "Sensibilidade – sociodemográfico + etnia/cor",
    erro =
      "As dummies de etnia/cor não possuem variação suficiente nesta amostra."
  )
}

coeficientes_etnia_sensibilidade <- extrair_coeficientes(
  modelo_etnia_sensibilidade
)

# ---------------------------------------------------------------------------
# Seleção do modelo total entre PO, PPO e SCALE
# ---------------------------------------------------------------------------

ppo_supera_po <- (
  isTRUE(modelo_ppo_B4$ok) &&
    nrow(comparacao_PO_PPO) == 1L &&
    is.finite(comparacao_PO_PPO$p) &&
    comparacao_PO_PPO$p < alpha
)

scale_supera_po <- (
  isTRUE(modelo_scale_B4$ok) &&
    nrow(comparacao_PO_SCALE) == 1L &&
    is.finite(comparacao_PO_SCALE$p) &&
    comparacao_PO_SCALE$p < alpha
)

candidatos_total <- list(
  PO = modelos_blocos$B4
)

if (ppo_supera_po) {
  candidatos_total$PPO <- modelo_ppo_B4
}

if (scale_supera_po) {
  candidatos_total$SCALE <- modelo_scale_B4
}

candidatos_validos <- candidatos_total[
  vapply(
    candidatos_total,
    function(x) isTRUE(x$ok) && !is.null(x$ajuste),
    logical(1)
  )
]

if (length(candidatos_validos) > 0L) {
  aics_candidatos <- vapply(
    candidatos_validos,
    function(x) AIC(x$ajuste),
    numeric(1)
  )

  nome_chave_modelo_total <- names(
    aics_candidatos
  )[
    which.min(
      aics_candidatos
    )
  ]

  modelo_total_selecionado <-
    candidatos_validos[
      [nome_chave_modelo_total]
    ]

  modelo_total_selecionado_nome <-
    modelo_total_selecionado$nome
} else {
  modelo_total_selecionado <- NULL

  modelo_total_selecionado_nome <- paste0(
    "Nenhum modelo total plenamente válido. B4: ",
    modelos_blocos$B4$status %||%
      modelos_blocos$B4$erro %||%
      "sem diagnóstico"
  )
}

modelo_principal_nome <- paste0(
  "Modelos ajustados prática por prática, com amostra comum e FDR-BH; ",
  "modelo total de sensibilidade selecionado: ",
  modelo_total_selecionado_nome
)

decisao_modelo <- tibble(
  item = c(
    "Estratégia inferencial principal",
    "Referências sociodemográficas",
    "Etnia/cor",
    "Modelo total PO",
    "Modelo PPO",
    "Modelo SCALE",
    "Modelo total selecionado",
    "Modelo B5"
  ),
  decisao = c(
    "Modelos ajustados prática por prática, mesma amostra e correção FDR-BH.",
    "Javeriana; Mulher; Solteiro(a); Não deficiência; Com família; Graduação.",
    "Branco é a referência, mas etnia/cor é analisada apenas como sensibilidade por não equivalência completa entre instrumentos.",
    modelos_blocos$B4$status %||%
      modelos_blocos$B4$erro %||%
      "Sem informação.",
    modelo_ppo_B4$status %||%
      modelo_ppo_B4$erro %||%
      "Sem informação.",
    modelo_scale_B4$status %||%
      modelo_scale_B4$erro %||%
      "Sem informação.",
    modelo_total_selecionado_nome,
    "Exploratório com 20 práticas; interpretar com cautela devido à sobreparametrização."
  )
)

# =============================================================================
# 16. DESIDENTIFICAÇÃO, EXPORTAÇÃO E VALIDAÇÃO FINAL
# =============================================================================

# -----------------------------------------------------------------------------
# 16.1 Microdados desidentificados de uso restrito
# -----------------------------------------------------------------------------
#
# Esta base NÃO é declarada anônima. Ela continua contendo respostas individuais.
# Instituição e medidas contínuas exatas são removidas/substituídas por faixas.

base_microdados_desidentificada_restrita <-
  base_harmonizada_restrita %>%
  mutate(
    faixa_etaria = cut(
      idade,
      breaks = c(
        17,
        20,
        23,
        26,
        29
      ),
      labels = c(
        "18–20",
        "21–23",
        "24–26",
        "27–29"
      ),
      include.lowest = TRUE
    ),
    faixa_tempo_curso = cut(
      anos_no_curso,
      breaks = c(
        -Inf,
        1,
        3,
        5,
        Inf
      ),
      labels = c(
        "Até 1 ano",
        "2–3 anos",
        "4–5 anos",
        "6 anos ou mais"
      )
    ),
    faixa_horas_universidade = cut(
      horas_universidade_semana,
      breaks = c(
        -Inf,
        20,
        40,
        60,
        Inf
      ),
      labels = c(
        "Até 20",
        "21–40",
        "41–60",
        "Mais de 60"
      )
    )
  ) %>%
  select(
    -instituicao_origem,
    -idade,
    -idade_centralizada,
    -ano_ingresso,
    -anos_no_curso,
    -anos_curso_centralizados,
    -horas_universidade_semana,
    -horas_universidade_10
  )

# Versão reduzida de microdados; também permanece restrita.
base_analitica_desidentificada_restrita <-
  base_microdados_desidentificada_restrita %>%
  select(
    mhc_codigo,
    mhc_classificacao,
    mhc_total_14_84,
    all_of(praticas_comuns_20),
    genero_modelo,
    estado_civil_modelo,
    deficiencia_modelo,
    moradia_modelo,
    etnia_modelo,
    nivel_academico_modelo,
    faixa_etaria,
    faixa_tempo_curso,
    faixa_horas_universidade
  )

# -----------------------------------------------------------------------------
# 16.2 Verificações de segurança
# -----------------------------------------------------------------------------

colunas_proibidas_exportacao <- c(
  "instituicao_origem",
  "indicador_ufmg",
  "contexto_institucional",
  "codigo_analitico",
  "id_analitico",
  "atleta",
  "atleta_sim",
  "estrato_social",
  "estrato_socioeconomico",
  "estrato_socioeconomico_detalhado",
  "trabalho",
  "trabalho_sim",
  "horas_trabalho_semana",
  "renda",
  "salario",
  "salário",
  "nome",
  "email",
  "telefone",
  "whatsapp",
  "matricula",
  "cpf"
)

problemas_exportacao <- unique(
  c(
    intersect(
      names(
        base_microdados_desidentificada_restrita
      ),
      colunas_proibidas_exportacao
    ),
    intersect(
      names(
        base_analitica_desidentificada_restrita
      ),
      colunas_proibidas_exportacao
    )
  )
)

if (length(problemas_exportacao) > 0L) {
  stop(
    paste0(
      "A base desidentificada ainda contém campos proibidos: ",
      paste(
        problemas_exportacao,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# 16.3 Arquivos de saída
# -----------------------------------------------------------------------------

arquivo_excel_restrito <- file.path(
  pasta_saida,
  "RESULTADOS_RESTRITOS_COMPLETOS.xlsx"
)

arquivo_excel_compartilhavel <- file.path(
  pasta_saida,
  "TABELAS_AGREGADAS_COMPARTILHAVEIS.xlsx"
)

arquivo_microdados <- file.path(
  pasta_saida,
  "BASE_MICRODADOS_DESIDENTIFICADA_RESTRITA.csv"
)

arquivo_analitica_desidentificada <- file.path(
  pasta_saida,
  "BASE_ANALITICA_DESIDENTIFICADA_RESTRITA.csv"
)

if (isTRUE(exportar_microdados_restritos)) {
  write_csv(
    base_microdados_desidentificada_restrita,
    arquivo_microdados,
    na = ""
  )

  write_csv(
    base_analitica_desidentificada_restrita,
    arquivo_analitica_desidentificada,
    na = ""
  )

  registrar(
    "MICRODADOS RESTRITOS EXPORTADOS: TRUE"
  )
} else {
  registrar(
    "MICRODADOS RESTRITOS EXPORTADOS: FALSE"
  )
}

# -----------------------------------------------------------------------------
# 16.4 Funções para planilhas
# -----------------------------------------------------------------------------

garantir_planilha_nao_vazia <- function(x) {
  if (
    is.data.frame(x) &&
      nrow(x) == 0L
  ) {
    return(
      tibble(
        mensagem =
          "Sem resultados para esta etapa."
      )
    )
  }

  x
}

escrever_lista_excel <- function(
  lista,
  arquivo
) {
  wb <- openxlsx::createWorkbook()

  nomes_usados <- character(0)

  for (nome_original in names(lista)) {
    nome_seguro <- substr(
      nome_original,
      1L,
      31L
    )

    # Evita nomes de abas duplicados após truncamento.
    if (nome_seguro %in% nomes_usados) {
      sufixo <- 2L
      nome_base <- substr(
        nome_seguro,
        1L,
        27L
      )

      while (
        paste0(
          nome_base,
          "_",
          sufixo
        ) %in% nomes_usados
      ) {
        sufixo <- sufixo + 1L
      }

      nome_seguro <- paste0(
        nome_base,
        "_",
        sufixo
      )
    }

    nomes_usados <- c(
      nomes_usados,
      nome_seguro
    )

    openxlsx::addWorksheet(
      wb,
      nome_seguro
    )

    dados_aba <- garantir_planilha_nao_vazia(
      lista[[nome_original]]
    )

    dados_aba <- as.data.frame(
      dados_aba,
      stringsAsFactors = FALSE
    )

    openxlsx::writeDataTable(
      wb,
      sheet = nome_seguro,
      x = dados_aba,
      withFilter = TRUE,
      tableStyle = "TableStyleMedium2"
    )

    openxlsx::freezePane(
      wb,
      sheet = nome_seguro,
      firstRow = TRUE
    )

    if (ncol(dados_aba) > 0L) {
      openxlsx::setColWidths(
        wb,
        sheet = nome_seguro,
        cols = seq_len(
          ncol(dados_aba)
        ),
        widths = "auto"
      )
    }
  }

  openxlsx::saveWorkbook(
    wb,
    arquivo,
    overwrite = TRUE
  )

  invisible(
    arquivo
  )
}

# -----------------------------------------------------------------------------
# 16.5 Relatório completo restrito
# -----------------------------------------------------------------------------

lista_excel_restrita <- list(
  `Leia-me` = tibble(
    item = c(
      "Status",
      "Referências",
      "Deficiência",
      "Estado civil",
      "Gênero",
      "Moradia",
      "Etnia/cor",
      "Trabalho",
      "Nível acadêmico",
      "Microdados",
      "Modelo principal"
    ),
    descricao = c(
      "Resultados analíticos de uso restrito.",
      "Javeriana; Mulher; Solteiro(a); Não deficiência; Com família; Branco (sensibilidade); Graduação.",
      "Somente Não/Sim.",
      "Somente Solteiro(a), União estável e Casado(a).",
      "Mulher, Homem e Diversidade de gênero. Os instrumentos não coletam orientação sexual; não foi criada categoria Bissexual.",
      "Mora sozinho(a), Com família ou Compartilha casa.",
      "Branco, Negro ou Indígena quando derivável. Não equivalência entre os instrumentos impede uso principal combinado.",
      "Todas as variáveis de trabalho/renda foram retiradas.",
      "Somente Graduação/Pós-graduação.",
      "Microdados individuais não são exportados por padrão.",
      modelo_principal_nome
    )
  ),
  `Fluxo amostral` = fluxo_amostral,
  `Dimensões originais` = dimensoes_originais,
  `Dimensões harmonizadas` = dimensoes_harmonizadas,
  `Comparação tipos` = comparacao_tipos,
  `Referências` = tabela_referencias,
  `Variação covariáveis` = tabela_variacao_covariaveis,
  `Dicionário sociodemo` = dicionario_sociodemografico,
  `Decisões harmonização` = decisoes_harmonizacao,
  `Auditoria recodificação` = auditoria_recodificacoes,
  `Validação dummies` = auditoria_validade_dummies,
  `Auditoria dummies` = auditoria_dummies,
  `Duplicidades` = auditoria_duplicatas,
  `Descritiva categórica` = tabela_descritiva_categorica,
  `Descritiva contínua` = tabela_descritiva_continua,
  `Dados ausentes` = tabela_ausencias,
  `Células raras` = tabela_celulas_raras,
  `Alfa MHC-SF` = alpha_mhc,
  `Tamanho amostras modelo` = tamanho_amostras_modelo,
  `Falhas modelos principais` = tabela_falhas_modelos_principais,
  `Resumo modelos` = resumo_modelos_po,
  `Coeficientes PO` = coeficientes_po,
  `Práticas brutas FDR` = coef_praticas_brutas,
  `Práticas ajustadas FDR` = coef_praticas_ajustadas,
  `Comparações blocos` = comparacoes_blocos,
  `Teste nominal B4` = teste_nominal_B4,
  `Teste escala B4` = teste_scale_B4,
  `Teste nominal B5` = teste_nominal_B5,
  `Teste escala B5` = teste_scale_B5,
  `Comparação PO-PPO` = comparacao_PO_PPO,
  `Comparação PO-SCALE` = comparacao_PO_SCALE,
  `Coeficientes PPO` = coeficientes_PPO,
  `Coeficientes SCALE` = coeficientes_SCALE,
  `Regressões por ponto corte` = or_limiares,
  `VIF B4` = vif_B4,
  `VIF B5` = vif_B5,
  `Número condição` = condicao_modelos,
  `Classificação resumo` = classificacao_resumo,
  `Confusão B4` = avaliacao_B4$confusao,
  `Confusão B5` = avaliacao_B5$confusao,
  `Confusão PPO` = avaliacao_PPO$confusao,
  `Confusão SCALE` = avaliacao_SCALE$confusao,
  `Sensibilidade etnia` = coeficientes_etnia_sensibilidade,
  `Decisão modelo` = decisao_modelo
)

lista_excel_restrita <- map(
  lista_excel_restrita,
  garantir_planilha_nao_vazia
)

escrever_lista_excel(
  lista_excel_restrita,
  arquivo_excel_restrito
)

# -----------------------------------------------------------------------------
# 16.6 Tabelas agregadas compartilháveis
# -----------------------------------------------------------------------------

tabela_categorica_compartilhavel <-
  suprimir_celulas_categoricas(
    tabela_descritiva_categorica,
    limite = 5L
  )

tabela_continua_compartilhavel <-
  suprimir_celulas_continuas(
    tabela_descritiva_continua,
    limite = 5L
  )

coef_modelo_total_compartilhavel <- if (
  !is.null(modelo_total_selecionado)
) {
  extrair_coeficientes(
    modelo_total_selecionado
  )
} else {
  tibble(
    mensagem =
      "Nenhum modelo total válido foi selecionado."
  )
}

lista_agregada <- list(
  `Leia-me` = tibble(
    item = c(
      "Proteção de células pequenas",
      "Microdados",
      "Referências",
      "Interpretação principal",
      "Etnia/cor"
    ),
    descricao = c(
      "Células com n<5 recebem supressão primária e, quando necessário, supressão complementar.",
      "Este arquivo não contém microdados individuais.",
      "Mulher; Solteiro(a); Não deficiência; Com família; Graduação. Branco é referência apenas na análise de sensibilidade de etnia/cor.",
      "Modelos ajustados prática por prática, amostra comum e FDR-BH.",
      "Interpretar somente como sensibilidade por diferenças entre os instrumentos colombiano e brasileiro."
    )
  ),
  `Fluxo amostral` = fluxo_amostral,
  `Referências` = tabela_referencias,
  `Descritiva categórica` =
    tabela_categorica_compartilhavel,
  `Descritiva contínua` =
    tabela_continua_compartilhavel,
  `Práticas ajustadas principais` =
    coef_praticas_ajustadas,
  `Modelo total selecionado` =
    coef_modelo_total_compartilhavel,
  `Decisão analítica` =
    decisao_modelo
)

lista_agregada <- map(
  lista_agregada,
  garantir_planilha_nao_vazia
)

escrever_lista_excel(
  lista_agregada,
  arquivo_excel_compartilhavel
)

# -----------------------------------------------------------------------------
# 16.7 Reprodutibilidade
# -----------------------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(
    pasta_saida,
    "sessionInfo.txt"
  )
)

versoes_pacotes <- tibble(
  pacote = pacotes,
  versao = map_chr(
    pacotes,
    ~ as.character(
      packageVersion(.x)
    )
  )
)

write_csv(
  versoes_pacotes,
  file.path(
    pasta_saida,
    "versoes_pacotes.csv"
  )
)

md5_bases <- tools::md5sum(
  c(
    arquivo_javeriana,
    arquivo_ufmg
  )
)

write_csv(
  tibble(
    arquivo = names(md5_bases),
    md5 = unname(md5_bases)
  ),
  file.path(
    pasta_saida,
    "md5_bases_originais.csv"
  )
)

# -----------------------------------------------------------------------------
# 16.8 Validação final
# -----------------------------------------------------------------------------

validacoes_finais <- tibble(
  verificacao = c(
    "Javeriana elegível = 102",
    "UFMG elegível = 125",
    "Total = 227",
    "MHC Languishing = 13",
    "MHC Moderate = 120",
    "MHC Flourishing = 94",
    "Sem variáveis de trabalho",
    "Deficiência somente Sim/Não",
    "Estado civil somente 3 categorias",
    "Gênero sem categoria inventada de orientação sexual",
    "Moradia somente 3 categorias",
    "Nível acadêmico somente 2 categorias",
    "Microdados não exportados por padrão"
  ),
  resultado = c(
    nrow(base_javeriana_harmonizada) == 102L,
    nrow(base_ufmg_harmonizada) == 125L,
    nrow(base_harmonizada_restrita) == 227L,
    sum(
      base_harmonizada_restrita$mhc_codigo == 1L,
      na.rm = TRUE
    ) == 13L,
    sum(
      base_harmonizada_restrita$mhc_codigo == 2L,
      na.rm = TRUE
    ) == 120L,
    sum(
      base_harmonizada_restrita$mhc_codigo == 3L,
      na.rm = TRUE
    ) == 94L,
    length(
      variaveis_trabalho_proibidas
    ) == 0L,
    all(
      na.omit(
        unique(
          base_harmonizada_restrita$deficiencia_modelo
        )
      ) %in% c(
        "Não",
        "Sim"
      )
    ),
    all(
      na.omit(
        unique(
          base_harmonizada_restrita$estado_civil_modelo
        )
      ) %in% c(
        "Solteiro(a)",
        "União estável",
        "Casado(a)"
      )
    ),
    !any(
      str_detect(
        na.omit(
          base_harmonizada_restrita$genero_modelo
        ),
        regex(
          "bissex|bisex",
          ignore_case = TRUE
        )
      )
    ),
    all(
      na.omit(
        unique(
          base_harmonizada_restrita$moradia_modelo
        )
      ) %in% c(
        "Mora sozinho(a)",
        "Com família",
        "Compartilha casa"
      )
    ),
    all(
      na.omit(
        unique(
          base_harmonizada_restrita$nivel_academico_modelo
        )
      ) %in% c(
        "Graduação",
        "Pós-graduação"
      )
    ),
    !isTRUE(
      exportar_microdados_restritos
    )
  )
)

falhas_criticas <- validacoes_finais %>%
  filter(
    !resultado
  )

write_csv(
  validacoes_finais,
  file.path(
    pasta_saida,
    "00_VALIDACOES_FINAIS.csv"
  )
)

resumo_execucao <- c(
  "EXECUÇÃO CONCLUÍDA",
  paste0(
    "Javeriana elegível: ",
    nrow(base_javeriana_harmonizada)
  ),
  paste0(
    "UFMG elegível: ",
    nrow(base_ufmg_harmonizada)
  ),
  paste0(
    "Total: ",
    nrow(base_harmonizada_restrita)
  ),
  paste0(
    "Colunas harmonizadas simplificadas: ",
    ncol(base_harmonizada_restrita)
  ),
  paste0(
    "Amostra comum dos modelos: ",
    nrow(base_ajuste_comum)
  ),
  paste0(
    "Modelos principais com falha: ",
    length(falhas_modelos_principais)
  ),
  paste0(
    "Modelo total selecionado: ",
    modelo_total_selecionado_nome
  ),
  paste0(
    "Microdados exportados: ",
    exportar_microdados_restritos
  ),
  paste0(
    "Validações finais com falha: ",
    nrow(falhas_criticas)
  )
)

writeLines(
  resumo_execucao,
  file.path(
    pasta_saida,
    "00_RESUMO_EXECUCAO.txt"
  )
)

walk(
  resumo_execucao,
  registrar
)

if (nrow(falhas_criticas) > 0L) {
  registrar(
    "ATENÇÃO: existem falhas nas validações finais. Consulte 00_VALIDACOES_FINAIS.csv."
  )
} else {
  registrar(
    "VALIDAÇÕES FINAIS: todas aprovadas."
  )
}

registrar(
  "FIM DA EXECUÇÃO"
)
