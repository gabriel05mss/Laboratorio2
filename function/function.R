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
  dados <- dados[complete.cases(dados), drop = FALSE]
  k <- ncol(dados)
  
  if (nrow(dados) < 2L || k < 2L) return(NA_real_)
  
  variancias_itens <- vapply(dados, var, numeric(1))
  variancia_total <- var(rowSums(dados))
  
  if (!is.finite(variancia_total) || variancia_total <= 0) return(NA_real_)
  
  (k / (k - 1)) * (
    1 - sum(variancias_itens, na.rm = TRUE) / variancia_total
  )
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