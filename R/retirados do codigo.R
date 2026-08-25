data_min_javeriana <- as.Date("2026-03-09")
data_min_ufmg <- as.Date("2026-04-28")
ano_analise <- 2026L
alpha <- 0.05
set.seed(20260803)


arquivo_log <- file.path(pasta_saida, "00_LOG_EXECUCAO.txt")
if (file.exists(arquivo_log)) file.remove(arquivo_log)

registrar <- function(...) {
  texto <- paste0(...)
  linha <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    texto
  )
  cat(linha, "\n")
  cat(linha, "\n", file = arquivo_log, append = TRUE)
}

registrar("INÍCIO DA EXECUÇÃO")

options(
  warn = 1,
  error = function() {
    mensagem <- geterrmessage()
    linha <- paste0(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " | ERRO NÃO TRATADO | ",
      mensagem
    )
    cat(linha, "\n", file = arquivo_log, append = TRUE)
    captura <- capture.output(traceback(2))
    if (length(captura) > 0L) {
      cat(
        paste(captura, collapse = "\n"),
        "\n",
        file = arquivo_log,
        append = TRUE
      )
    }
  }
)

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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

localizar_arquivo <- function(pasta, nome_exato, padrao_alternativo) {
  caminho_exato <- file.path(pasta, nome_exato)
  
  if (file.exists(caminho_exato)) {
    return(normalizePath(caminho_exato, winslash = "/", mustWork = TRUE))
  }
  
  candidatos <- list.files(
    pasta,
    pattern = padrao_alternativo,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(candidatos) == 0L) {
    stop(
      paste0(
        "Arquivo não encontrado.\nNome esperado: ", nome_exato,
        "\nPasta: ", pasta
      ),
      call. = FALSE
    )
  }
  
  if (length(candidatos) > 1L) {
    stop(
      paste0(
        "Mais de um arquivo candidato foi encontrado:\n",
        paste(candidatos, collapse = "\n"),
        "\nMantenha somente o arquivo correto ou informe o nome exato."
      ),
      call. = FALSE
    )
  }
  
  normalizePath(candidatos, winslash = "/", mustWork = TRUE)
}

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


sim_nao_num <- function(x) {
  texto <- normalizar_texto(x)
  case_when(
    texto == "" ~ NA_integer_,
    texto == "sim" ~ 1L,
    texto == "nao" ~ 0L,
    TRUE ~ NA_integer_
  )
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

if (!file.exists(arquivo_javeriana)) {
  stop(
    paste0("Arquivo da Javeriana não encontrado:\n", arquivo_javeriana),
    call. = FALSE
  )
}

if (!file.exists(arquivo_ufmg)) {
  stop(
    paste0("Arquivo da UFMG não encontrado:\n", arquivo_ufmg),
    call. = FALSE
  )
}


#DUPLICIDADES E FILTROS ----

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

base_javeriana_harmonizada <- base_javeriana_filtrada %>%
  mutate(
    instituicao_origem = "Javeriana",
    contexto_institucional = "Javeriana – Colômbia",
    indicador_ufmg = 0L,
    .estrato_1 = checkbox_01(estrato_economico___1),
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
    
    rec_aulas_puj = as.numeric(rec_aulas),
    rec_dormir_puj = as.numeric(rec_dormir),
    
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
    
    .gender_woman_cis_ref = checkbox_01(sexo___1),
    genero_homem_cis = checkbox_01(sexo___2),
    genero_mulher_trans = checkbox_01(sexo___3),
    genero_homem_trans = checkbox_01(sexo___4),
    genero_nao_binario = checkbox_01(sexo___5),
    genero_fluido = checkbox_01(sexo___6),
    genero_agenero = checkbox_01(sexo___7),
    genero_nao_respondeu = checkbox_01(sexo___8),
    
    .marital_single_ref = checkbox_01(estado_civil___1),
    estado_civil_casado = checkbox_01(estado_civil___2),
    estado_civil_viuvo = checkbox_01(estado_civil___3),
    estado_civil_divorciado = checkbox_01(estado_civil___4),
    estado_civil_uniao_estavel = checkbox_01(estado_civil___5),
    
    .eth_puj_none_ref = checkbox_01(cultura___6),
    etnia_puj_indigena = checkbox_01(cultura___1),
    etnia_puj_rom = checkbox_01(cultura___2),
    etnia_puj_raizal = checkbox_01(cultura___3),
    etnia_puj_palenquera = checkbox_01(cultura___4),
    etnia_puj_negra = checkbox_01(cultura___5),
    
    deficiencia_sim = case_when(
      as.numeric(deficiencia) == 1 ~ 1L,
      as.numeric(deficiencia) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    deficiencia_detalhada = case_when(
      deficiencia_sim == 1L ~ "Sim",
      deficiencia_sim == 0L ~ "Não",
      TRUE ~ NA_character_
    ),
    
    deficiencia_fisica = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___1), 0L)
    ),
    deficiencia_tetraplegia = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___tetraplejia), 0L)
    ),
    deficiencia_auditiva = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___2), 0L)
    ),
    deficiencia_visual = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___3), 0L)
    ),
    deficiencia_surdocegueira = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___4), 0L)
    ),
    deficiencia_multipla = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___5), 0L)
    ),
    deficiencia_intelectual = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___6), 0L)
    ),
    deficiencia_tea = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___7), 0L)
    ),
    deficiencia_psicossocial = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___8), 0L)
    ),
    deficiencia_nao_respondeu = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(tipo_deficiencia___9), 0L)
    ),
    
    .res_family_ref = checkbox_01(membros_familias___2),
    moradia_sozinho_detalhe = checkbox_01(membros_familias___1),
    moradia_pares_detalhe = checkbox_01(membros_familias___3),
    moradia_parceiro_detalhe = checkbox_01(membros_familias___4),
    moradia_conjuge_detalhe = checkbox_01(membros_familias___5),
    
    trabalho_sim = case_when(
      as.numeric(trabalho_remunerado) == 1 ~ 1L,
      as.numeric(trabalho_remunerado) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    trabalho_detalhado = case_when(
      trabalho_sim == 1L ~ "Sim",
      trabalho_sim == 0L ~ "Não",
      TRUE ~ NA_character_
    ),
    horas_trabalho_semana = case_when(
      trabalho_sim == 1L ~ as.numeric(tempo_trabalho),
      trabalho_sim == 0L ~ 0,
      TRUE ~ NA_real_
    ),
    trabalho_puj_formal = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(trabalho___1), 0L)
    ),
    trabalho_puj_informal = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(trabalho___2), 0L)
    ),
    trabalho_puj_formal_informal = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(checkbox_01(trabalho___3), 0L)
    ),
    
    idade = as.numeric(idade),
    ano_ingresso = as.numeric(ano_de_ingresso),
    anos_no_curso = ano_analise - ano_ingresso,
    horas_universidade_semana = as.numeric(tempo_universidade),
    
    .level_grad_ref = checkbox_01(nivel_pregrado),
    nivel_especializacao = checkbox_01(nivel_especi),
    nivel_mestrado = checkbox_01(nivel_maestria),
    nivel_doutorado = checkbox_01(nivel_doctorado),
    nivel_pos_nao_especificado = 0L,
    
    .access_puj_1 = case_when(recex1 == 1 ~ 1L, recex1 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_2 = case_when(recex2 == 1 ~ 1L, recex2 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_3 = case_when(recex3 == 1 ~ 1L, recex3 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_4 = case_when(recex4 == 1 ~ 1L, recex4 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_5 = case_when(recex5 == 1 ~ 1L, recex5 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_6 = case_when(recex6 == 1 ~ 1L, recex6 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_7 = case_when(recex7 == 1 ~ 1L, recex7 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_8 = case_when(recex8 == 1 ~ 1L, recex8 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_9 = case_when(recex9 == 1 ~ 1L, recex9 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_10 = case_when(recex10 == 1 ~ 1L, recex10 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_11 = case_when(recex11 == 1 ~ 1L, recex11 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_12 = case_when(recex12 == 1 ~ 1L, recex12 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_13 = case_when(recex13 == 1 ~ 1L, recex13 == 2 ~ 0L, TRUE ~ NA_integer_),
    .access_puj_14 = case_when(recex14 == 1 ~ 1L, recex14 == 2 ~ 0L, TRUE ~ NA_integer_),
    
    acesso_puj_clubes = .access_puj_1,
    acesso_puj_quadras_privadas = .access_puj_2,
    acesso_puj_academias_privadas = .access_puj_3,
    acesso_puj_cinema = .access_puj_4,
    acesso_puj_parques = .access_puj_5,
    acesso_puj_teatro = .access_puj_6,
    acesso_puj_centros_comerciais = .access_puj_7,
    acesso_puj_piscinas = .access_puj_8,
    acesso_puj_escolas_danca = .access_puj_9,
    acesso_puj_escolas_musica = .access_puj_10,
    acesso_puj_centros_culturais = .access_puj_11,
    acesso_puj_jogos_mesa = .access_puj_12,
    acesso_puj_discotecas = .access_puj_13,
    acesso_puj_centros_esportivos = .access_puj_14
  ) %>%
  rowwise() %>%
  mutate(
    .gender_all_missing = all(is.na(c_across(c(
      .gender_woman_cis_ref, genero_homem_cis,
      genero_mulher_trans, genero_homem_trans,
      genero_nao_binario, genero_fluido,
      genero_agenero, genero_nao_respondeu
    )))),
    genero_num_opcoes = if_else(
      .gender_all_missing,
      NA_integer_,
      as.integer(sum(
        c_across(c(
          .gender_woman_cis_ref, genero_homem_cis,
          genero_mulher_trans, genero_homem_trans,
          genero_nao_binario, genero_fluido,
          genero_agenero, genero_nao_respondeu
        )),
        na.rm = TRUE
      ))
    ),
    genero_detalhado = juntar_rotulos(
      c_across(c(
        .gender_woman_cis_ref, genero_homem_cis,
        genero_mulher_trans, genero_homem_trans,
        genero_nao_binario, genero_fluido,
        genero_agenero, genero_nao_respondeu
      )),
      rotulos_genero
    ),
    genero_multiplas_identidades = case_when(
      is.na(genero_num_opcoes) ~ NA_integer_,
      TRUE ~ as.integer(genero_num_opcoes > 1L)
    ),
    genero_modelo = case_when(
      is.na(genero_num_opcoes) ~ NA_character_,
      genero_num_opcoes == 1L & .gender_woman_cis_ref == 1L ~
        "Mulher cisgênero",
      genero_num_opcoes == 1L & genero_homem_cis == 1L ~
        "Homem cisgênero",
      genero_nao_respondeu == 1L | genero_num_opcoes == 0L ~
        NA_character_,
      TRUE ~ "Diversidade/múltiplas identidades"
    ),
    genero_homem = case_when(
      genero_modelo == "Homem cisgênero" ~ 1L,
      genero_modelo %in% c(
        "Mulher cisgênero",
        "Diversidade/múltiplas identidades"
      ) ~ 0L,
      TRUE ~ NA_integer_
    ),
    genero_diverso = case_when(
      genero_modelo == "Diversidade/múltiplas identidades" ~ 1L,
      genero_modelo %in% c(
        "Mulher cisgênero",
        "Homem cisgênero"
      ) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    estado_civil_detalhado = juntar_rotulos(
      c_across(c(
        .marital_single_ref, estado_civil_casado,
        estado_civil_viuvo, estado_civil_divorciado,
        estado_civil_uniao_estavel
      )),
      rotulos_civil_puj
    ),
    estado_civil_com_parceiro = case_when(
      estado_civil_casado == 1L | estado_civil_uniao_estavel == 1L ~ 1L,
      .marital_single_ref == 1L |
        estado_civil_viuvo == 1L |
        estado_civil_divorciado == 1L ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    sistema_etnia_cor = "Pertencimento étnico – Colômbia",
    etnia_cor_detalhada = juntar_rotulos(
      c_across(c(
        etnia_puj_indigena, etnia_puj_rom,
        etnia_puj_raizal, etnia_puj_palenquera,
        etnia_puj_negra, .eth_puj_none_ref
      )),
      rotulos_etnia_puj
    ),
    minoria_etnico_racial_sensibilidade = case_when(
      .eth_puj_none_ref == 1L ~ 0L,
      sum(c_across(c(
        etnia_puj_indigena, etnia_puj_rom,
        etnia_puj_raizal, etnia_puj_palenquera,
        etnia_puj_negra
      )), na.rm = TRUE) > 0L ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    deficiencia_outra_nao_especificada = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      deficiencia_sim == 1L &
        sum(c_across(c(
          deficiencia_fisica, deficiencia_tetraplegia,
          deficiencia_auditiva, deficiencia_visual,
          deficiencia_surdocegueira, deficiencia_multipla,
          deficiencia_intelectual, deficiencia_tea,
          deficiencia_psicossocial, deficiencia_nao_respondeu
        )), na.rm = TRUE) == 0L ~ 1L,
      TRUE ~ 0L
    ),
    
    .residence_all_missing = all(is.na(c_across(c(
      moradia_sozinho_detalhe, .res_family_ref,
      moradia_pares_detalhe, moradia_parceiro_detalhe,
      moradia_conjuge_detalhe
    )))),
    moradia_num_opcoes = if_else(
      .residence_all_missing,
      NA_integer_,
      as.integer(sum(
        c_across(c(
          moradia_sozinho_detalhe, .res_family_ref,
          moradia_pares_detalhe, moradia_parceiro_detalhe,
          moradia_conjuge_detalhe
        )),
        na.rm = TRUE
      ))
    ),
    moradia_detalhada = juntar_rotulos(
      c_across(c(
        moradia_sozinho_detalhe, .res_family_ref,
        moradia_pares_detalhe, moradia_parceiro_detalhe,
        moradia_conjuge_detalhe
      )),
      rotulos_moradia_puj
    ),
    moradia_mista_detalhe = case_when(
      is.na(moradia_num_opcoes) ~ NA_integer_,
      TRUE ~ as.integer(moradia_num_opcoes > 1L)
    ),
    moradia_outra_detalhe = case_when(
      is.na(moradia_num_opcoes) ~ NA_integer_,
      TRUE ~ as.integer(moradia_num_opcoes == 0L)
    ),
    moradia_modelo = case_when(
      .res_family_ref == 1L |
        moradia_parceiro_detalhe == 1L |
        moradia_conjuge_detalhe == 1L ~ "Família/parceiro(a)",
      moradia_sozinho_detalhe == 1L & moradia_num_opcoes == 1L ~ "Sozinho(a)",
      moradia_pares_detalhe == 1L ~ "Pares/coletivo",
      TRUE ~ NA_character_
    ),
    moradia_sozinho = case_when(
      moradia_modelo == "Sozinho(a)" ~ 1L,
      moradia_modelo %in% c("Família/parceiro(a)", "Pares/coletivo") ~ 0L,
      TRUE ~ NA_integer_
    ),
    moradia_pares = case_when(
      moradia_modelo == "Pares/coletivo" ~ 1L,
      moradia_modelo %in% c("Família/parceiro(a)", "Sozinho(a)") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    nivel_academico_detalhado = juntar_rotulos(
      c_across(c(
        .level_grad_ref, nivel_especializacao,
        nivel_mestrado, nivel_doutorado,
        nivel_pos_nao_especificado
      )),
      c(
        "Graduação", "Especialização",
        "Mestrado", "Doutorado",
        "Pós-graduação não especificada"
      )
    ),
    pos_graduacao = case_when(
      .level_grad_ref == 1L ~ 0L,
      sum(c_across(c(
        nivel_especializacao, nivel_mestrado,
        nivel_doutorado, nivel_pos_nao_especificado
      )), na.rm = TRUE) > 0L ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    .access_n_validos = sum(
      !is.na(c_across(starts_with(".access_puj_")))
    ),
    acesso_externo_algum = case_when(
      .access_n_validos == 0L ~ NA_integer_,
      any(c_across(starts_with(".access_puj_")) == 1L, na.rm = TRUE) ~ 1L,
      all(c_across(starts_with(".access_puj_")) == 0L, na.rm = TRUE) ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup() %>%
  calcular_mhc() %>%
  select(-starts_with("."))

if (!identical(
  names(base_javeriana_harmonizada),
  names(base_ufmg_harmonizada)
)) {
  stop(
    "As bases harmonizadas não têm os mesmos nomes e ordem de colunas.",
    call. = FALSE
  )
}

if (any(!comparacao_tipos$mesmo_tipo)) {
  stop(
    paste0(
      "As bases harmonizadas têm tipos diferentes em: ",
      paste(
        comparacao_tipos$coluna[!comparacao_tipos$mesmo_tipo],
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

