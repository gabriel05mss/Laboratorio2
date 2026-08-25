library(readxl)
library(dplyr)
library(stringdist)
library(stringr)
library(lubridate)
library(ordinal)
library(openxlsx)
library(tibble)
library(purrr)
library(tidyr)

rm(list = ls())

df <- read_excel("data/data_raw.xlsx")

source("function/function.R")

col_u <- list(
  data = achar_coluna(df, "Carimbo de data/hora", TRUE),
  elegibilidade = achar_coluna(
    df,
    "Nosso questionário é destinado a pessoas com idade entre 18 e 29 anos"
  ),
  consentimento = achar_coluna(
    df,
    "TERMO DE CONSENTIMENTO LIVRE E ESCLARECIDO"
  ),
  
  rec_caminhar = achar_coluna(df, "Caminhada pelo campus"),
  rec_casal = achar_coluna(df, "Passar tempo com o(a) parceiro(a)"),
  rec_amigos = achar_coluna(df, "Momentos de convivência com amigos"),
  rec_contemplar = achar_coluna(df, "Sentar-me em áreas abertas"),
  rec_desenhar = achar_coluna(df, "Desenhar por prazer"),
  rec_musica = achar_coluna(df, "Participar de aulas de música"),
  rec_danca = achar_coluna(df, "Participar de aulas de dança"),
  rec_treinamento = achar_coluna(df, "Participar de treinos esportivos"),
  rec_assistir_treinos = achar_coluna(df, "Assistir a treinos esportivos"),
  rec_jogos_mesa = achar_coluna(df, "Jogar jogos de cartas ou tabuleiro"),
  rec_comer_social = achar_coluna(df, "Comer em contexto social"),
  rec_ler_ufmg = achar_coluna(df, "Ler assuntos que não fazem parte"),
  rec_religioso = achar_coluna(df, "Participar de encontros religiosos"),
  rec_festas_ufmg = achar_coluna(df, "Ir a festas realizadas no campus"),
  rec_redes_sociais = achar_coluna(df, "Utilizar redes sociais por lazer"),
  rec_jogos_eletronicos = achar_coluna(df, "Jogar jogos eletrônicos"),
  rec_competicoes = achar_coluna(df, "Participar de competições esportivas"),
  rec_filmes = achar_coluna(df, "Assistir filmes online"),
  rec_bebida = achar_coluna(df, "Consumir bebidas alcoólicas"),
  rec_fumar = achar_coluna(df, "Fumar dentro da universidade"),
  rec_academia = achar_coluna(df, "Frequentar a academia do campus"),
  rec_interacoes_afetivo_sexuais = achar_coluna(df, "Vivenciar interações afetivo-sexuais"),
  
  acesso_lista = achar_coluna(
    df,
    "Marque as opções de lazer a que você tem acesso fora da universidade"
  ),
  acesso_geral = achar_coluna(
    df,
    "Tem acesso a equipamentos de lazer fora da universidade"
  ),
  
  sm_interesse = achar_coluna(df, "Interessada(o) pela vida"),
  sm_satisfacao = achar_coluna(df, "Satisfeito (a)", TRUE),
  sm_alegria = achar_coluna(df, "Feliz", TRUE),
  sm_contribuicao = achar_coluna(df, "algo importante para contribuir"),
  sm_comunidade = achar_coluna(df, "pertencia a uma comunidade"),
  sm_sociedade_melhor = achar_coluna(df, "sociedade está se tornando"),
  sm_pessoas_boas = achar_coluna(df, "pessoas, em geral, são boas"),
  sm_sociedade_sentido = achar_coluna(df, "forma como a nossa sociedade funciona"),
  sm_personalidade = achar_coluna(df, "características de personalidade"),
  sm_responsabilidades = achar_coluna(df, "administrou bem as responsabilidades"),
  sm_relacoes = achar_coluna(df, "relacionamentos afetuosos"),
  sm_crescimento = achar_coluna(df, "experiências que o desafiaram"),
  sm_confianca = achar_coluna(df, "confiante para pensar"),
  sm_vida_sentido = achar_coluna(df, "vida tem um propósito"),
  
  genero = achar_coluna(df, "Como você se identifica em relação ao seu gênero"),
  idade = achar_coluna(df, "Qual a sua idade"),
  estado_civil = achar_coluna(df, "Qual o seu estado civil"),
  cor = achar_coluna(df, "Qual sua cor"),
  deficiencia = achar_coluna(df, "Você possui alguma deficiência"),
  tipo_deficiencia = achar_coluna(df, "Caso tenha respondido sim"),
  moradia = achar_coluna(df, "Quem reside com você"),
  trabalho = achar_coluna(df, "Você possui alguma renda/salário"),
  natureza_trabalho = achar_coluna(df, "qual a natureza da sua renda/salário"),
  horas_trabalho = achar_coluna(df, "carga horária de trabalho semanal"),
  ano_ingresso = achar_coluna(df, "Em que ano você iniciou"),
  horas_universidade = achar_coluna(df, "quantas horas semanais você permanece"),
  nivel = achar_coluna(df, "Você é estudante de"),
  curso_graduacao = achar_coluna(df, "Qual o seu curso de graduação"),
  curso_mestrado = achar_coluna(df, "Mestrado", TRUE),
  curso_doutorado = achar_coluna(df, "Doutorado", TRUE),
  curso_especializacao = achar_coluna(df, "Especialização", TRUE),
  interesse_grupo = achar_coluna(df, "interesse em para participar do grupo focal"),
  email = achar_coluna(df, "E-mail", TRUE),
  whatsapp = achar_coluna(df, "WhatsApp", TRUE)
)

names(df) <- sapply(
  names(df),
  function(x) {
    if (x %in% names(col_u)) {
      depara[[x]]
    } else {
      x
    }
  }
)

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
    df,
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

base_ufmg_filtrada <- df %>%
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
    nrow(df),
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
# 6. SCHEMA ÚNICO DAS BASES HARMONIZADAS
# =============================================================================

colunas_schema <- c(
  "instituicao_origem",
  "contexto_institucional",
  "indicador_ufmg",
  "mhc_itens_validos",
  "mhc_total_14_84",
  "mhc_total_0_70",
  "mhc_codigo",
  "mhc_classificacao",
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
  "sm_vida_sentido",
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
  "rec_interacoes_afetivo_sexuais",
  "rec_aulas_puj",
  "rec_dormir_puj",
  "rec_ler_ufmg",
  "rec_festas_ufmg",
  "genero_detalhado",
  "genero_num_opcoes",
  "genero_homem_cis",
  "genero_mulher_trans",
  "genero_homem_trans",
  "genero_nao_binario",
  "genero_fluido",
  "genero_agenero",
  "genero_multiplas_identidades",
  "genero_nao_respondeu",
  "genero_modelo",
  "genero_homem",
  "genero_diverso",
  "estado_civil_detalhado",
  "estado_civil_casado",
  "estado_civil_viuvo",
  "estado_civil_divorciado",
  "estado_civil_uniao_estavel",
  "estado_civil_nao_respondeu",
  "estado_civil_com_parceiro",
  "sistema_etnia_cor",
  "etnia_cor_detalhada",
  "etnia_puj_indigena",
  "etnia_puj_rom",
  "etnia_puj_raizal",
  "etnia_puj_palenquera",
  "etnia_puj_negra",
  "cor_raca_ufmg_preta",
  "cor_raca_ufmg_parda",
  "cor_raca_ufmg_amarela",
  "cor_raca_ufmg_indigena",
  "cor_raca_ufmg_nao_respondeu",
  "minoria_etnico_racial_sensibilidade",
  "deficiencia_detalhada",
  "deficiencia_sim",
  "deficiencia_fisica",
  "deficiencia_tetraplegia",
  "deficiencia_auditiva",
  "deficiencia_visual",
  "deficiencia_surdocegueira",
  "deficiencia_multipla",
  "deficiencia_intelectual",
  "deficiencia_tea",
  "deficiencia_psicossocial",
  "deficiencia_nao_respondeu",
  "deficiencia_outra_nao_especificada",
  "moradia_detalhada",
  "moradia_num_opcoes",
  "moradia_sozinho_detalhe",
  "moradia_pares_detalhe",
  "moradia_parceiro_detalhe",
  "moradia_conjuge_detalhe",
  "moradia_mista_detalhe",
  "moradia_outra_detalhe",
  "moradia_modelo",
  "moradia_sozinho",
  "moradia_pares",
  "trabalho_detalhado",
  "trabalho_sim",
  "horas_trabalho_semana",
  "trabalho_puj_formal",
  "trabalho_puj_informal",
  "trabalho_puj_formal_informal",
  "trabalho_ufmg_estagio_bolsa",
  "trabalho_ufmg_clt",
  "trabalho_ufmg_autonomo",
  "trabalho_ufmg_pj",
  "trabalho_ufmg_servidor_publico",
  "trabalho_ufmg_assistencia_estudantil",
  "trabalho_ufmg_empreendedor",
  "idade",
  "ano_ingresso",
  "anos_no_curso",
  "horas_universidade_semana",
  "idade_centralizada",
  "anos_curso_centralizados",
  "horas_universidade_10",
  "nivel_academico_detalhado",
  "nivel_especializacao",
  "nivel_mestrado",
  "nivel_doutorado",
  "nivel_pos_nao_especificado",
  "pos_graduacao",
  "acesso_externo_algum",
  "acesso_puj_clubes",
  "acesso_puj_quadras_privadas",
  "acesso_puj_academias_privadas",
  "acesso_puj_cinema",
  "acesso_puj_parques",
  "acesso_puj_teatro",
  "acesso_puj_centros_comerciais",
  "acesso_puj_piscinas",
  "acesso_puj_escolas_danca",
  "acesso_puj_escolas_musica",
  "acesso_puj_centros_culturais",
  "acesso_puj_jogos_mesa",
  "acesso_puj_discotecas",
  "acesso_puj_centros_esportivos",
  "acesso_ufmg_cinemas",
  "acesso_ufmg_bares",
  "acesso_ufmg_clubes",
  "acesso_ufmg_centros_comerciais",
  "acesso_ufmg_teatros",
  "acesso_ufmg_museus",
  "acesso_ufmg_aulas_coletivas",
  "acesso_ufmg_restaurantes",
  "acesso_ufmg_casas_shows",
  "acesso_ufmg_parques_diversoes",
  "acesso_ufmg_outro"
)

colunas_character <- c(
  "instituicao_origem",
  "contexto_institucional",
  "mhc_classificacao",
  "genero_detalhado",
  "genero_modelo",
  "estado_civil_detalhado",
  "sistema_etnia_cor",
  "etnia_cor_detalhada",
  "deficiencia_detalhada",
  "moradia_detalhada",
  "moradia_modelo",
  "trabalho_detalhado",
  "nivel_academico_detalhado"
)
colunas_numeric <- c(
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
  "sm_vida_sentido",
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
  "rec_interacoes_afetivo_sexuais",
  "rec_aulas_puj",
  "rec_dormir_puj",
  "rec_ler_ufmg",
  "rec_festas_ufmg",
  "mhc_total_14_84",
  "mhc_total_0_70",
  "horas_trabalho_semana",
  "idade",
  "ano_ingresso",
  "anos_no_curso",
  "horas_universidade_semana",
  "idade_centralizada",
  "anos_curso_centralizados",
  "horas_universidade_10"
)
colunas_integer <- c(
  "indicador_ufmg",
  "mhc_itens_validos",
  "mhc_codigo",
  "genero_num_opcoes",
  "genero_homem_cis",
  "genero_mulher_trans",
  "genero_homem_trans",
  "genero_nao_binario",
  "genero_fluido",
  "genero_agenero",
  "genero_multiplas_identidades",
  "genero_nao_respondeu",
  "genero_homem",
  "genero_diverso",
  "estado_civil_casado",
  "estado_civil_viuvo",
  "estado_civil_divorciado",
  "estado_civil_uniao_estavel",
  "estado_civil_nao_respondeu",
  "estado_civil_com_parceiro",
  "etnia_puj_indigena",
  "etnia_puj_rom",
  "etnia_puj_raizal",
  "etnia_puj_palenquera",
  "etnia_puj_negra",
  "cor_raca_ufmg_preta",
  "cor_raca_ufmg_parda",
  "cor_raca_ufmg_amarela",
  "cor_raca_ufmg_indigena",
  "cor_raca_ufmg_nao_respondeu",
  "minoria_etnico_racial_sensibilidade",
  "deficiencia_sim",
  "deficiencia_fisica",
  "deficiencia_tetraplegia",
  "deficiencia_auditiva",
  "deficiencia_visual",
  "deficiencia_surdocegueira",
  "deficiencia_multipla",
  "deficiencia_intelectual",
  "deficiencia_tea",
  "deficiencia_psicossocial",
  "deficiencia_nao_respondeu",
  "deficiencia_outra_nao_especificada",
  "moradia_num_opcoes",
  "moradia_sozinho_detalhe",
  "moradia_pares_detalhe",
  "moradia_parceiro_detalhe",
  "moradia_conjuge_detalhe",
  "moradia_mista_detalhe",
  "moradia_outra_detalhe",
  "moradia_sozinho",
  "moradia_pares",
  "trabalho_sim",
  "trabalho_puj_formal",
  "trabalho_puj_informal",
  "trabalho_puj_formal_informal",
  "trabalho_ufmg_estagio_bolsa",
  "trabalho_ufmg_clt",
  "trabalho_ufmg_autonomo",
  "trabalho_ufmg_pj",
  "trabalho_ufmg_servidor_publico",
  "trabalho_ufmg_assistencia_estudantil",
  "trabalho_ufmg_empreendedor",
  "nivel_especializacao",
  "nivel_mestrado",
  "nivel_doutorado",
  "nivel_pos_nao_especificado",
  "pos_graduacao",
  "acesso_externo_algum",
  "acesso_puj_clubes",
  "acesso_puj_quadras_privadas",
  "acesso_puj_academias_privadas",
  "acesso_puj_cinema",
  "acesso_puj_parques",
  "acesso_puj_teatro",
  "acesso_puj_centros_comerciais",
  "acesso_puj_piscinas",
  "acesso_puj_escolas_danca",
  "acesso_puj_escolas_musica",
  "acesso_puj_centros_culturais",
  "acesso_puj_jogos_mesa",
  "acesso_puj_discotecas",
  "acesso_puj_centros_esportivos",
  "acesso_ufmg_cinemas",
  "acesso_ufmg_bares",
  "acesso_ufmg_clubes",
  "acesso_ufmg_centros_comerciais",
  "acesso_ufmg_teatros",
  "acesso_ufmg_museus",
  "acesso_ufmg_aulas_coletivas",
  "acesso_ufmg_restaurantes",
  "acesso_ufmg_casas_shows",
  "acesso_ufmg_parques_diversoes",
  "acesso_ufmg_outro"
)

schema_tipos <- setNames(
  rep("integer", length(colunas_schema)),
  colunas_schema
)
schema_tipos[colunas_character] <- "character"
schema_tipos[colunas_numeric] <- "numeric"
schema_tipos[colunas_integer] <- "integer"

if (
  length(schema_tipos) != 155L ||
  anyDuplicated(names(schema_tipos)) > 0L
) {
  stop(
    "O schema harmonizado deveria conter 155 colunas únicas.",
    call. = FALSE
  )
}

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
praticas_exclusivas_4 <- c(
  "rec_aulas_puj",
  "rec_dormir_puj",
  "rec_ler_ufmg",
  "rec_festas_ufmg"
)
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

# =============================================================================
# 7. HARMONIZAÇÃO DA JAVERIANA
# =============================================================================

rotulos_genero <- c(
  "Mulher cisgênero", "Homem cisgênero",
  "Mulher transgênero", "Homem transgênero",
  "Pessoa não binária", "Gênero fluido",
  "Agênero", "Prefiro não responder"
)

rotulos_civil_puj <- c(
  "Solteiro(a)", "Casado(a)", "Viúvo(a)",
  "Divorciado(a)", "União estável"
)

rotulos_etnia_puj <- c(
  "Indígena", "Rom", "Raizal",
  "Palenquero", "Negro(a)",
  "Nenhuma filiação étnica"
)

rotulos_moradia_puj <- c(
  "Sozinho(a)", "Família",
  "Amigos/colegas/república",
  "Namorado(a)/companheiro(a)", "Cônjuge"
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

# =============================================================================
# 8. HARMONIZAÇÃO DA UFMG
# =============================================================================

base_ufmg_harmonizada <- base_ufmg_filtrada %>%
  mutate(
    instituicao_origem = "UFMG",
    contexto_institucional = "UFMG – Brasil",
    indicador_ufmg = 1L,
    
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
    rec_ler_ufmg = as.numeric(.data[[col_u$rec_ler_ufmg]]),
    rec_religioso = as.numeric(.data[[col_u$rec_religioso]]),
    rec_festas_ufmg = as.numeric(.data[[col_u$rec_festas_ufmg]]),
    rec_redes_sociais = as.numeric(.data[[col_u$rec_redes_sociais]]),
    rec_jogos_eletronicos = as.numeric(.data[[col_u$rec_jogos_eletronicos]]),
    rec_competicoes = as.numeric(.data[[col_u$rec_competicoes]]),
    rec_filmes = as.numeric(.data[[col_u$rec_filmes]]),
    rec_bebida = as.numeric(.data[[col_u$rec_bebida]]),
    rec_fumar = as.numeric(.data[[col_u$rec_fumar]]),
    rec_academia = as.numeric(.data[[col_u$rec_academia]]),
    rec_interacoes_afetivo_sexuais = as.numeric(.data[[col_u$rec_interacoes_afetivo_sexuais]]),
    
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
    
    .gender_text = as.character(.data[[col_u$genero]]),
    .gender_woman_cis_ref = dummy_contem(
      .gender_text,
      "Mulher cisgênero"
    ),
    genero_homem_cis = dummy_contem(
      .gender_text,
      "Homem cisgênero"
    ),
    genero_mulher_trans = dummy_contem(
      .gender_text,
      "Mulher transgênero"
    ),
    genero_homem_trans = dummy_contem(
      .gender_text,
      "Homem transgênero"
    ),
    genero_nao_binario = dummy_contem(
      .gender_text,
      "Pessoa não-binária"
    ),
    genero_fluido = dummy_contem(
      .gender_text,
      "Gênero fluido"
    ),
    genero_agenero = dummy_contem(
      .gender_text,
      "Agênero"
    ),
    genero_nao_respondeu = dummy_contem(
      .gender_text,
      "Prefiro não responder"
    ),
    
    .civil_text = as.character(.data[[col_u$estado_civil]]),
    .marital_single_ref = dummy_exato(.civil_text, "Solteiro(a)"),
    estado_civil_casado = dummy_exato(.civil_text, "Casado(a)"),
    estado_civil_viuvo = NA_integer_,
    estado_civil_divorciado = dummy_exato(.civil_text, "Divorciado(a)"),
    estado_civil_uniao_estavel = dummy_exato(.civil_text, "União estável"),
    estado_civil_nao_respondeu = dummy_exato(.civil_text, "Prefiro não responder"),
    
    .race_text = as.character(.data[[col_u$cor]]),
    .race_ufmg_white_ref = dummy_exato(.race_text, "Branca"),
    cor_raca_ufmg_preta = dummy_exato(.race_text, "Preta"),
    cor_raca_ufmg_parda = dummy_exato(.race_text, "Parda"),
    cor_raca_ufmg_amarela = dummy_exato(.race_text, "Amarela"),
    cor_raca_ufmg_indigena = dummy_exato(.race_text, "Indígena"),
    cor_raca_ufmg_nao_respondeu = dummy_exato(
      .race_text,
      "Prefiro não declarar"
    ),
    
    deficiencia_detalhada = as.character(.data[[col_u$deficiencia]]),
    deficiencia_sim = sim_nao_num(deficiencia_detalhada),
    .disability_type_text = as.character(
      .data[[col_u$tipo_deficiencia]]
    ),
    
    deficiencia_fisica = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.disability_type_text, "Deficiência física"),
        0L
      )
    ),
    deficiencia_tetraplegia = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.disability_type_text, "Tetraplegia"),
        0L
      )
    ),
    deficiencia_auditiva = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.disability_type_text, "Deficiência auditiva"),
        0L
      )
    ),
    deficiencia_visual = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.disability_type_text, "Deficiência visual"),
        0L
      )
    ),
    deficiencia_surdocegueira = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.disability_type_text, "Surdocegueira"),
        0L
      )
    ),
    deficiencia_multipla = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.disability_type_text, "Deficiência múltipla"),
        0L
      )
    ),
    deficiencia_intelectual = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.disability_type_text, "Deficiência intelectual"),
        0L
      )
    ),
    deficiencia_tea = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ as.integer(
        normalizar_texto(.disability_type_text) != "" &
          str_detect(
            normalizar_texto(.disability_type_text),
            "espectro autista|\\btea\\b"
          )
      )
    ),
    deficiencia_psicossocial = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(
          .disability_type_text,
          "Deficiência psicossocial"
        ),
        0L
      )
    ),
    deficiencia_nao_respondeu = case_when(
      is.na(deficiencia_sim) ~ NA_integer_,
      deficiencia_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(
          .disability_type_text,
          "Prefiro não responder"
        ),
        0L
      )
    ),
    
    .residence_text = as.character(.data[[col_u$moradia]]),
    moradia_sozinho_detalhe = dummy_contem(
      .residence_text,
      c("Somente você", "Moro sozinho")
    ),
    .res_family_ref = dummy_contem(
      .residence_text,
      c(
        "Familiares", "Irmã", "Irmão",
        "Avô", "Avó", "Pai", "Mãe"
      )
    ),
    moradia_pares_detalhe = dummy_contem(
      .residence_text,
      c(
        "Amigos", "República", "Colegas",
        "universitárias", "estudantes",
        "divido apartamento", "outras pessoas"
      )
    ),
    moradia_parceiro_detalhe = dummy_contem(
      .residence_text,
      c(
        "Namorado", "Namorada",
        "Companheiro", "Companheira"
      )
    ),
    moradia_conjuge_detalhe = dummy_contem(
      .residence_text,
      c("Esposo", "Esposa", "Cônjuge")
    ),
    
    trabalho_detalhado = as.character(.data[[col_u$trabalho]]),
    trabalho_sim = sim_nao_num(trabalho_detalhado),
    horas_trabalho_semana = case_when(
      trabalho_sim == 1L ~ as.numeric(.data[[col_u$horas_trabalho]]),
      trabalho_sim == 0L ~ 0,
      TRUE ~ NA_real_
    ),
    .work_nature_text = as.character(
      .data[[col_u$natureza_trabalho]]
    ),
    trabalho_ufmg_estagio_bolsa = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.work_nature_text, "Estágio/Bolsista"),
        0L
      )
    ),
    trabalho_ufmg_clt = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(dummy_exato(.work_nature_text, "CLT"), 0L)
    ),
    trabalho_ufmg_autonomo = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.work_nature_text, "Freelancer"),
        0L
      )
    ),
    trabalho_ufmg_pj = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(
          .work_nature_text,
          c("Pessoa Jurídica", "PJ")
        ),
        0L
      )
    ),
    trabalho_ufmg_servidor_publico = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(.work_nature_text, "Servidor público"),
        0L
      )
    ),
    trabalho_ufmg_assistencia_estudantil = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(
          .work_nature_text,
          "Bolsa de assistência estudantil"
        ),
        0L
      )
    ),
    trabalho_ufmg_empreendedor = case_when(
      is.na(trabalho_sim) ~ NA_integer_,
      trabalho_sim == 0L ~ 0L,
      TRUE ~ coalesce(
        dummy_contem(
          .work_nature_text,
          c("Empreendedor", "proprietário de negócio")
        ),
        0L
      )
    ),
    
    idade = as.numeric(.data[[col_u$idade]]),
    ano_ingresso = as.numeric(.data[[col_u$ano_ingresso]]),
    anos_no_curso = ano_analise - ano_ingresso,
    horas_universidade_semana = as.numeric(
      .data[[col_u$horas_universidade]]
    ),
    
    .level_text = as.character(.data[[col_u$nivel]]),
    .level_grad_ref = dummy_exato(.level_text, "Graduação"),
    .level_post = dummy_exato(.level_text, "Pós-graduação"),
    nivel_especializacao = case_when(
      is.na(.level_post) ~ NA_integer_,
      .level_post == 0L ~ 0L,
      TRUE ~ as.integer(
        !is.na(.data[[col_u$curso_especializacao]]) &
          str_squish(.data[[col_u$curso_especializacao]]) != ""
      )
    ),
    nivel_mestrado = case_when(
      is.na(.level_post) ~ NA_integer_,
      .level_post == 0L ~ 0L,
      TRUE ~ as.integer(
        !is.na(.data[[col_u$curso_mestrado]]) &
          str_squish(.data[[col_u$curso_mestrado]]) != ""
      )
    ),
    nivel_doutorado = case_when(
      is.na(.level_post) ~ NA_integer_,
      .level_post == 0L ~ 0L,
      TRUE ~ as.integer(
        !is.na(.data[[col_u$curso_doutorado]]) &
          str_squish(.data[[col_u$curso_doutorado]]) != ""
      )
    ),
    nivel_pos_nao_especificado = case_when(
      is.na(.level_post) ~ NA_integer_,
      .level_post == 0L ~ 0L,
      TRUE ~ as.integer(
        nivel_especializacao == 0L &
          nivel_mestrado == 0L &
          nivel_doutorado == 0L
      )
    ),
    
    .access_general_text = as.character(.data[[col_u$acesso_geral]]),
    .access_list_text = as.character(.data[[col_u$acesso_lista]]),
    acesso_externo_algum = case_when(
      sim_nao_num(.access_general_text) == 0L ~ 0L,
      sim_nao_num(.access_general_text) == 1L ~ 1L,
      normalizar_texto(.access_list_text) != "" ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    acesso_ufmg_cinemas = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Cinemas")
    ),
    acesso_ufmg_bares = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Bares")
    ),
    acesso_ufmg_clubes = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Clubes sociais")
    ),
    acesso_ufmg_centros_comerciais = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Shopping centers")
    ),
    acesso_ufmg_teatros = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Teatros")
    ),
    acesso_ufmg_museus = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Museus")
    ),
    acesso_ufmg_aulas_coletivas = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Aulas coletivas")
    ),
    acesso_ufmg_restaurantes = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Restaurantes")
    ),
    acesso_ufmg_casas_shows = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Casas de shows")
    ),
    acesso_ufmg_parques_diversoes = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Parques de diversões")
    ),
    acesso_ufmg_outro = case_when(
      acesso_externo_algum == 0L ~ 0L,
      TRUE ~ dummy_contem(.access_list_text, "Outros")
    )
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
    
    estado_civil_detalhado = case_when(
      .marital_single_ref == 1L ~ "Solteiro(a)",
      estado_civil_casado == 1L ~ "Casado(a)",
      estado_civil_viuvo == 1L ~ "Viúvo(a)",
      estado_civil_divorciado == 1L ~ "Divorciado(a)",
      estado_civil_uniao_estavel == 1L ~ "União estável",
      estado_civil_nao_respondeu == 1L ~ "Prefiro não responder",
      TRUE ~ NA_character_
    ),
    estado_civil_com_parceiro = case_when(
      estado_civil_casado == 1L | estado_civil_uniao_estavel == 1L ~ 1L,
      .marital_single_ref == 1L |
        estado_civil_viuvo == 1L |
        estado_civil_divorciado == 1L ~ 0L,
      estado_civil_nao_respondeu == 1L ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    
    sistema_etnia_cor = "Cor/raça – Brasil",
    etnia_cor_detalhada = case_when(
      .race_ufmg_white_ref == 1L ~ "Branca",
      cor_raca_ufmg_preta == 1L ~ "Preta",
      cor_raca_ufmg_parda == 1L ~ "Parda",
      cor_raca_ufmg_amarela == 1L ~ "Amarela",
      cor_raca_ufmg_indigena == 1L ~ "Indígena",
      cor_raca_ufmg_nao_respondeu == 1L ~ "Prefiro não declarar",
      TRUE ~ NA_character_
    ),
    minoria_etnico_racial_sensibilidade = case_when(
      .race_ufmg_white_ref == 1L ~ 0L,
      cor_raca_ufmg_preta == 1L |
        cor_raca_ufmg_parda == 1L |
        cor_raca_ufmg_amarela == 1L |
        cor_raca_ufmg_indigena == 1L ~ 1L,
      cor_raca_ufmg_nao_respondeu == 1L ~ NA_integer_,
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
      moradia_modelo %in% c(
        "Família/parceiro(a)", "Pares/coletivo"
      ) ~ 0L,
      TRUE ~ NA_integer_
    ),
    moradia_pares = case_when(
      moradia_modelo == "Pares/coletivo" ~ 1L,
      moradia_modelo %in% c(
        "Família/parceiro(a)", "Sozinho(a)"
      ) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    nivel_academico_detalhado = case_when(
      .level_grad_ref == 1L ~ "Graduação",
      nivel_especializacao == 1L ~ "Especialização",
      nivel_mestrado == 1L ~ "Mestrado",
      nivel_doutorado == 1L ~ "Doutorado",
      nivel_pos_nao_especificado == 1L ~
        "Pós-graduação não especificada",
      TRUE ~ NA_character_
    ),
    pos_graduacao = case_when(
      .level_grad_ref == 1L ~ 0L,
      .level_post == 1L ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup() %>%
  calcular_mhc() %>%
  select(-starts_with("."))

# =============================================================================
# 9. ALINHAMENTO E UNIÃO
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
    "As bases harmonizadas não têm os mesmos nomes e ordem de colunas.",
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
      "As bases harmonizadas têm tipos diferentes em: ",
      paste(
        comparacao_tipos$coluna[!comparacao_tipos$mesmo_tipo],
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

base_unificada <- bind_rows(
  base_javeriana_harmonizada,
  base_ufmg_harmonizada
)

media_idade <- mean(base_unificada$idade, na.rm = TRUE)
media_anos_curso <- mean(base_unificada$anos_no_curso, na.rm = TRUE)
media_horas_univ <- mean(base_unificada$horas_universidade_semana, na.rm = TRUE)

base_unificada <- base_unificada %>%
  mutate(
    idade_centralizada = idade - media_idade,
    anos_curso_centralizados = anos_no_curso - media_anos_curso,
    horas_universidade_10 = (horas_universidade_semana - media_horas_univ) / 10
  ) %>%
  alinhar_schema(schema_tipos)

# Divide novamente apenas para exportação e conferência.
base_javeriana_harmonizada <- base_unificada %>%
  filter(instituicao_origem == "Javeriana")
base_ufmg_harmonizada <- base_unificada %>%
  filter(instituicao_origem == "UFMG")

dimensoes_harmonizadas <- tibble(
  base = c(
    "Javeriana harmonizada",
    "UFMG harmonizada",
    "Base unificada"
  ),
  linhas = c(
    nrow(base_javeriana_harmonizada),
    nrow(base_ufmg_harmonizada),
    nrow(base_unificada)
  ),
  colunas = c(
    ncol(base_javeriana_harmonizada),
    ncol(base_ufmg_harmonizada),
    ncol(base_unificada)
  )
)

# =============================================================================
# 10. VALIDAÇÕES DE INTEGRIDADE E ANONIMIZAÇÃO
# =============================================================================

walk(
  praticas_comuns_20,
  ~ validar_intervalo(base_unificada[[.x]], 1, 10, .x)
)

walk(
  praticas_exclusivas_4,
  ~ validar_intervalo(base_unificada[[.x]], 1, 10, .x)
)

walk(
  itens_mhc,
  ~ validar_intervalo(base_unificada[[.x]], 1, 6, .x)
)

validar_intervalo(base_unificada$idade, 18, 29, "idade")
validar_intervalo(base_unificada$mhc_codigo, 1, 3, "mhc_codigo")
validar_intervalo(
  base_unificada$horas_universidade_semana,
  0,
  168,
  "horas_universidade_semana"
)
validar_intervalo(
  base_unificada$horas_trabalho_semana,
  0,
  168,
  "horas_trabalho_semana"
)
validar_intervalo(
  base_unificada$ano_ingresso,
  1990,
  ano_analise,
  "ano_ingresso"
)
validar_intervalo(
  base_unificada$anos_no_curso,
  0,
  36,
  "anos_no_curso"
)
validar_intervalo(
  base_unificada$mhc_itens_validos,
  14,
  14,
  "mhc_itens_validos"
)

if (any(
  rowSums(
    base_unificada[, c("genero_homem", "genero_diverso")],
    na.rm = TRUE
  ) > 1L
)) {
  stop("Combinação impossível nas dummies do gênero.", call. = FALSE)
}

if (any(
  rowSums(
    base_unificada[, c("moradia_sozinho", "moradia_pares")],
    na.rm = TRUE
  ) > 1L
)) {
  stop("Combinação impossível nas dummies de moradia.", call. = FALSE)
}

colunas_dummy <- names(schema_tipos)[
  schema_tipos == "integer" &
    !names(schema_tipos) %in% c(
      "mhc_itens_validos", "mhc_codigo",
      "genero_num_opcoes", "moradia_num_opcoes"
    )
]

auditoria_validade_dummies <- validar_dummy(
  base_unificada,
  colunas_dummy
)

nomes_normalizados <- names(base_unificada) %>%
  normalizar_texto() %>%
  stringr::str_replace_all("[^a-z0-9]+", "_") %>%
  stringr::str_replace_all("^_+|_+$", "")

# =============================================================================
# VERIFICAÇÃO DE IDENTIFICADORES DIRETOS E VARIÁVEIS DE CURSO DETALHADO
# =============================================================================

nomes_normalizados <- names(base_unificada) %>%
  normalizar_texto() %>%
  stringr::str_replace_all("[^a-z0-9]+", "_") %>%
  stringr::str_replace_all("^_+|_+$", "")

# Identificadores diretos.
# "curso" não entra aqui, pois também aparece em variáveis analíticas
# legítimas, como anos_no_curso.
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
  "ip",
  "redcap",
  "identificador"
)

padrao_identificadores_diretos <- paste0(
  "(^|_)(",
  paste(
    termos_identificadores_diretos,
    collapse = "|"
  ),
  ")($|_)"
)

colunas_identificadores_diretos <- names(base_unificada)[
  stringr::str_detect(
    nomes_normalizados,
    stringr::regex(
      padrao_identificadores_diretos,
      ignore_case = TRUE
    )
  )
]

# Verificação separada para nomes detalhados de cursos.
# O padrão exige que o nome da coluna comece com "curso",
# evitando sinalizar anos_no_curso.
padrao_curso_detalhado <- paste0(
  "^(",
  paste(
    c(
      "curso",
      "curso_graduacao",
      "curso_especializacao",
      "curso_mestrado",
      "curso_doutorado",
      "nome_curso",
      "nome_do_curso"
    ),
    collapse = "|"
  ),
  ")($|_)"
)

colunas_curso_detalhado <- names(base_unificada)[
  stringr::str_detect(
    nomes_normalizados,
    stringr::regex(
      padrao_curso_detalhado,
      ignore_case = TRUE
    )
  )
]

colunas_suspeitas <- unique(
  c(
    colunas_identificadores_diretos,
    colunas_curso_detalhado
  )
)

if (length(colunas_suspeitas) > 0L) {
  stop(
    paste0(
      "A base interna contém possíveis identificadores ",
      "ou informações acadêmicas detalhadas: ",
      paste(
        colunas_suspeitas,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}
if (anyNA(base_unificada$mhc_codigo)) {
  stop(
    "Há participantes sem classificação válida do MHC-SF.",
    call. = FALSE
  )
}

if (isTRUE(validar_contagens_dos_arquivos_atuais)) {
  distribuicao_esperada <- c(
    `1` = 13L,
    `2` = 120L,
    `3` = 94L
  )
  distribuicao_observada <- table(base_unificada$mhc_codigo)
  
  if (!identical(
    as.integer(distribuicao_observada[names(distribuicao_esperada)]),
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
  
  validacoes_categorias <- tibble(
    verificacao = c(
      "Gênero – mulher cisgênero",
      "Gênero – homem cisgênero",
      "Gênero – diversidade/múltiplas",
      "Gênero – não resposta",
      "Estado civil – sem parceiro",
      "Estado civil – com parceiro",
      "Estado civil – não resposta",
      "Moradia – família/parceiro",
      "Moradia – sozinho",
      "Moradia – pares/coletivo"
    ),
    observado = c(
      sum(base_unificada$genero_modelo == "Mulher cisgênero", na.rm = TRUE),
      sum(base_unificada$genero_modelo == "Homem cisgênero", na.rm = TRUE),
      sum(base_unificada$genero_modelo == "Diversidade/múltiplas identidades", na.rm = TRUE),
      sum(is.na(base_unificada$genero_modelo)),
      sum(base_unificada$estado_civil_com_parceiro == 0, na.rm = TRUE),
      sum(base_unificada$estado_civil_com_parceiro == 1, na.rm = TRUE),
      sum(is.na(base_unificada$estado_civil_com_parceiro)),
      sum(base_unificada$moradia_modelo == "Família/parceiro(a)", na.rm = TRUE),
      sum(base_unificada$moradia_modelo == "Sozinho(a)", na.rm = TRUE),
      sum(base_unificada$moradia_modelo == "Pares/coletivo", na.rm = TRUE)
    ),
    esperado = c(
      111L, 102L, 10L, 4L,
      211L, 14L, 2L,
      186L, 14L, 27L
    )
  ) %>%
    mutate(confere = observado == esperado)
  
  if (any(!validacoes_categorias$confere)) {
    stop(
      paste0(
        "Falha nas frequências categóricas esperadas:\n",
        paste(
          validacoes_categorias$verificacao[!validacoes_categorias$confere],
          validacoes_categorias$observado[!validacoes_categorias$confere],
          validacoes_categorias$esperado[!validacoes_categorias$confere],
          sep = " | observado=",
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }
} else {
  validacoes_categorias <- tibble()
}

# Remove as bases originais da memória após a harmonização validada.
rm(
  base_javeriana_original,
  df,
  base_javeriana_filtrada,
  base_ufmg_filtrada
)
invisible(gc())

# =============================================================================
# 11. BASE ANALÍTICA E BASE DE MODELAGEM
# =============================================================================

colunas_base_analitica <- c(
  "instituicao_origem", "contexto_institucional", "indicador_ufmg",
  "mhc_itens_validos", "mhc_total_14_84", "mhc_total_0_70",
  "mhc_codigo", "mhc_classificacao",
  itens_mhc,
  praticas_comuns_20,
  
  "genero_detalhado", "genero_num_opcoes",
  "genero_homem_cis", "genero_mulher_trans",
  "genero_homem_trans", "genero_nao_binario",
  "genero_fluido", "genero_agenero",
  "genero_multiplas_identidades", "genero_nao_respondeu",
  "genero_modelo", "genero_homem", "genero_diverso",
  
  "estado_civil_detalhado", "estado_civil_casado",
  "estado_civil_viuvo", "estado_civil_divorciado",
  "estado_civil_uniao_estavel", "estado_civil_nao_respondeu",
  "estado_civil_com_parceiro",
  
  "sistema_etnia_cor", "etnia_cor_detalhada",
  "etnia_puj_indigena", "etnia_puj_rom",
  "etnia_puj_raizal", "etnia_puj_palenquera",
  "etnia_puj_negra",
  "cor_raca_ufmg_preta", "cor_raca_ufmg_parda",
  "cor_raca_ufmg_amarela", "cor_raca_ufmg_indigena",
  "cor_raca_ufmg_nao_respondeu", "minoria_etnico_racial_sensibilidade",
  
  "deficiencia_detalhada", "deficiencia_sim",
  "deficiencia_fisica", "deficiencia_tetraplegia",
  "deficiencia_auditiva", "deficiencia_visual",
  "deficiencia_surdocegueira", "deficiencia_multipla",
  "deficiencia_intelectual", "deficiencia_tea",
  "deficiencia_psicossocial", "deficiencia_nao_respondeu",
  "deficiencia_outra_nao_especificada",
  
  "moradia_detalhada", "moradia_num_opcoes",
  "moradia_sozinho_detalhe", "moradia_pares_detalhe",
  "moradia_parceiro_detalhe", "moradia_conjuge_detalhe",
  "moradia_mista_detalhe", "moradia_outra_detalhe",
  "moradia_modelo", "moradia_sozinho", "moradia_pares",
  
  "trabalho_detalhado", "trabalho_sim", "horas_trabalho_semana",
  
  "idade", "ano_ingresso", "anos_no_curso",
  "horas_universidade_semana", "idade_centralizada",
  "anos_curso_centralizados", "horas_universidade_10",
  
  "nivel_academico_detalhado", "nivel_especializacao",
  "nivel_mestrado", "nivel_doutorado",
  "nivel_pos_nao_especificado", "pos_graduacao"
)

base_analitica_restrita <- base_unificada %>%
  select(all_of(colunas_base_analitica))

covariaveis_ajuste <- c(
  "indicador_ufmg",
  "idade_centralizada",
  "genero_homem",
  "genero_diverso",
  "estado_civil_com_parceiro",
  "deficiencia_sim",
  "moradia_sozinho",
  "moradia_pares",
  "pos_graduacao",
  "anos_curso_centralizados",
  "trabalho_sim",
  "horas_universidade_10"
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

colunas_base_modelagem <- unique(c(
  "instituicao_origem", "contexto_institucional",
  "mhc_codigo", "mhc_classificacao",
  praticas_comuns_20,
  covariaveis_ajuste
))

base_modelagem <- base_unificada %>%
  select(all_of(colunas_base_modelagem)) %>%
  mutate(
    mhc_ordinal = ordered(
      mhc_codigo,
      levels = c(1L, 2L, 3L),
      labels = c("Languishing", "Moderate", "Flourishing")
    )
  )

if (!identical(
  levels(base_modelagem$mhc_ordinal),
  c("Languishing", "Moderate", "Flourishing")
)) {
  stop("A ordem do desfecho ordinal está incorreta.", call. = FALSE)
}

if (any(c(
  "horas_trabalho_semana",
  "horas_trabalho_original",
  "intensidade_trabalho",
  "tempo_trabalho"
) %in% c(covariaveis_ajuste, praticas_teoricas_13, praticas_comuns_20))) {
  stop(
    "A carga/intensidade de trabalho entrou indevidamente na modelagem.",
    call. = FALSE
  )
}

# =============================================================================
# 12. DICIONÁRIO DE DUMMIES, REFERÊNCIAS E HARMONIZAÇÃO
# =============================================================================

tabela_referencias <- tribble(
  ~variavel_teorica, ~categoria_referencia, ~dummy, ~categoria_1, ~uso,
  "Universidade/país", "Javeriana – Colômbia", "indicador_ufmg", "UFMG – Brasil", "Modelo principal",
  
  "Gênero detalhado", "Mulher cisgênero", "genero_homem_cis", "Homem cisgênero", "Descritiva",
  "Gênero detalhado", "Mulher cisgênero", "genero_mulher_trans", "Mulher transgênero", "Descritiva",
  "Gênero detalhado", "Mulher cisgênero", "genero_homem_trans", "Homem transgênero", "Descritiva",
  "Gênero detalhado", "Mulher cisgênero", "genero_nao_binario", "Pessoa não binária", "Descritiva",
  "Gênero detalhado", "Mulher cisgênero", "genero_fluido", "Gênero fluido", "Descritiva",
  "Gênero detalhado", "Mulher cisgênero", "genero_agenero", "Agênero", "Descritiva",
  "Gênero detalhado", "Mulher cisgênero", "genero_multiplas_identidades", "Múltiplas identidades", "Descritiva",
  "Gênero detalhado", "Mulher cisgênero", "genero_nao_respondeu", "Prefiro não responder", "Descritiva; excluída do modelo",
  "Gênero – modelo", "Mulher cisgênero", "genero_homem", "Homem cisgênero", "Modelo principal",
  "Gênero – modelo", "Mulher cisgênero", "genero_diverso", "Diversidade/múltiplas identidades", "Modelo principal",
  
  "Estado civil detalhado", "Solteiro(a)", "estado_civil_casado", "Casado(a)", "Descritiva",
  "Estado civil detalhado", "Solteiro(a)", "estado_civil_viuvo", "Viúvo(a)", "Descritiva; opção não oferecida na UFMG",
  "Estado civil detalhado", "Solteiro(a)", "estado_civil_divorciado", "Divorciado(a)", "Descritiva",
  "Estado civil detalhado", "Solteiro(a)", "estado_civil_uniao_estavel", "União estável", "Descritiva",
  "Estado civil detalhado", "Solteiro(a)", "estado_civil_nao_respondeu", "Prefiro não responder", "Descritiva; opção não oferecida na Javeriana",
  "Estado civil – modelo", "Sem parceiro(a)", "estado_civil_com_parceiro", "Com parceiro(a)", "Modelo principal",
  
  "Etnia – Javeriana", "Nenhuma filiação étnica", "etnia_puj_indigena", "Indígena", "Descritiva Javeriana",
  "Etnia – Javeriana", "Nenhuma filiação étnica", "etnia_puj_rom", "Rom", "Descritiva Javeriana",
  "Etnia – Javeriana", "Nenhuma filiação étnica", "etnia_puj_raizal", "Raizal", "Descritiva Javeriana",
  "Etnia – Javeriana", "Nenhuma filiação étnica", "etnia_puj_palenquera", "Palenquero", "Descritiva Javeriana",
  "Etnia – Javeriana", "Nenhuma filiação étnica", "etnia_puj_negra", "Negro(a)", "Descritiva Javeriana",
  
  "Cor/raça – UFMG", "Branca", "cor_raca_ufmg_preta", "Preta", "Descritiva UFMG",
  "Cor/raça – UFMG", "Branca", "cor_raca_ufmg_parda", "Parda", "Descritiva UFMG",
  "Cor/raça – UFMG", "Branca", "cor_raca_ufmg_amarela", "Amarela", "Descritiva UFMG",
  "Cor/raça – UFMG", "Branca", "cor_raca_ufmg_indigena", "Indígena", "Descritiva UFMG",
  "Cor/raça – UFMG", "Branca", "cor_raca_ufmg_nao_respondeu", "Prefiro não declarar", "Descritiva UFMG",
  "Etnia/cor ampla", "Branca/sem filiação étnica", "minoria_etnico_racial_sensibilidade", "Grupo minoritizado", "Somente sensibilidade; não entra no modelo principal",
  
  "Deficiência", "Não", "deficiencia_sim", "Sim", "Modelo principal",
  "Moradia detalhada", "Família", "moradia_sozinho_detalhe", "Sozinho(a)", "Descritiva",
  "Moradia detalhada", "Família", "moradia_pares_detalhe", "Amigos/colegas/república", "Descritiva",
  "Moradia detalhada", "Família", "moradia_parceiro_detalhe", "Namorado(a)/companheiro(a)", "Descritiva",
  "Moradia detalhada", "Família", "moradia_conjuge_detalhe", "Cônjuge", "Descritiva",
  "Moradia detalhada", "Família", "moradia_mista_detalhe", "Arranjo múltiplo", "Descritiva",
  "Moradia detalhada", "Família", "moradia_outra_detalhe", "Outro/não classificado", "Descritiva",
  "Moradia – modelo", "Família/parceiro(a)", "moradia_sozinho", "Sozinho(a)", "Modelo principal",
  "Moradia – modelo", "Família/parceiro(a)", "moradia_pares", "Pares/coletivo", "Modelo principal",
  
  "Trabalho remunerado", "Não", "trabalho_sim", "Sim", "Modelo principal",
  "Nível acadêmico", "Graduação", "nivel_especializacao", "Especialização", "Descritiva",
  "Nível acadêmico", "Graduação", "nivel_mestrado", "Mestrado", "Descritiva",
  "Nível acadêmico", "Graduação", "nivel_doutorado", "Doutorado", "Descritiva",
  "Nível acadêmico", "Graduação", "nivel_pos_nao_especificado", "Pós-graduação não especificada", "Descritiva",
  "Nível acadêmico – modelo", "Graduação", "pos_graduacao", "Pós-graduação", "Modelo principal",
  "Acesso externo", "Não", "acesso_externo_algum", "Sim", "Descritiva"
) %>%
  mutate(
    codigo_referencia = if_else(
      str_detect(variavel_teorica, "Gênero – modelo"),
      "gender_man=0 e gender_diverse=0",
      if_else(
        str_detect(variavel_teorica, "Moradia – modelo"),
        "res_alone=0 e res_peers=0",
        paste0(dummy, "=0")
      )
    ),
    observacao = case_when(
      dummy == "genero_nao_respondeu" ~
        "Não resposta não é combinada com diversidade de gênero.",
      dummy == "minoria_etnico_racial_sensibilidade" ~
        "Pertencimento étnico colombiano e cor/raça brasileira não são medidas idênticas.",
      TRUE ~
        "A categoria de referência não possui coluna dummy própria."
    )
  )

decisoes_harmonizacao <- tribble(
  ~tema, ~decisao, ~justificativa,
  "União das bases",
  "Harmonizar primeiro e unir depois.",
  "bind_rows() somente é executado após igualdade de nomes, ordem e tipos.",
  "Valores não coletados",
  "Usar NA, nunca zero.",
  "Zero significa opção disponível e não selecionada; NA significa variável/opção não coletada.",
  "Gênero",
  "Preservar todas as opções; não resposta fica fora do modelo.",
  "Não resposta não é identidade de gênero e não deve ser agrupada com diversidade.",
  "Estado civil",
  "Dummies detalhadas com solteiro(a) como referência; modelo binário usa sem parceiro(a) como referência.",
  "Viúvo/divorciado não são recodificados como solteiro; compõem apenas a categoria analítica sem parceiro.",
  "Moradia",
  "Preservar opções; no modelo, presença de família/parceiro tem prioridade.",
  "A regra evita criar uma categoria rara por um único caso misto e fica documentada.",
  "Etnia/cor",
  "Manter blocos nacionais separados.",
  "Pertencimento étnico colombiano e cor/raça brasileira não são construtos idênticos.",
  "Deficiência",
  "Tipos são lidos somente quando disability_yes=1.",
  "Tipos são considerados somente entre participantes que responderam sim; textos de quem respondeu não não geram falso positivo.",
  "Acesso externo UFMG",
  "Derivar acesso geral da lista quando a pergunta geral estiver vazia.",
  "Nos arquivos atuais a lista foi respondida, mas a pergunta geral está vazia nos casos elegíveis.",
  "Anonimização",
  "Exportar microdados apenas como pseudonimizados e restritos.",
  "Datas, contatos, identificadores, cursos e respostas abertas são removidos; compartilhamento externo é somente agregado.",
  "Estratégia inferencial",
  "Modelos ajustados prática por prática são principais; B4 é o modelo total de sensibilidade.",
  "A amostra comum e a correção FDR-BH melhoram a comparabilidade; B5 permanece exploratório."
)

# Mapa de origem das variáveis comuns.
mapa_javeriana <- c(
  rec_caminhar = "rec_caminar",
  rec_casal = "rec_casal",
  rec_amigos = "rec_amigos",
  rec_contemplar = "rec_contemplar",
  rec_desenhar = "rec_desenhar",
  rec_musica = "rec_musica",
  rec_danca = "rec_danca",
  rec_treinamento = "rec_treino",
  rec_assistir_treinos = "rec_assistir",
  rec_jogos_mesa = "rec_jogos",
  rec_comer_social = "rec_comer",
  rec_religioso = "rec_religioso",
  rec_redes_sociais = "rec_redes",
  rec_jogos_eletronicos = "rec_games",
  rec_competicoes = "rec_competicao",
  rec_filmes = "rec_filmes",
  rec_bebida = "rec_bebida_puj",
  rec_fumar = "rec_fumar_puj",
  rec_academia = "rec_academia",
  rec_interacoes_afetivo_sexuais = "rec_sex_puj",
  rec_aulas_puj = "rec_aulas",
  rec_dormir_puj = "rec_dormir",
  sm_alegria = "sm_alegria",
  sm_interesse = "sm_interesvida",
  sm_satisfacao = "sm_satisfaccionvida",
  sm_contribuicao = "sm_importanciasociedad",
  sm_comunidade = "sm_partecomunidad",
  sm_sociedade_melhor = "sm_sociedadebien",
  sm_pessoas_boas = "sm_personasbuenas",
  sm_sociedade_sentido = "sm_funcionamentosociedad",
  sm_personalidade = "sm_mipersonalidad",
  sm_responsabilidades = "sm_responsabilidades",
  sm_relacoes = "sm_relaciones",
  sm_crescimento = "sm_experiencias",
  sm_confianca = "sm_confianza",
  sm_vida_sentido = "sm_vidasentido",
  idade = "idade",
  ano_ingresso = "ano_de_ingresso",
  horas_universidade_semana = "tempo_universidade"
)

mapa_ufmg <- c(
  rec_caminhar = col_u$rec_caminhar,
  rec_casal = col_u$rec_casal,
  rec_amigos = col_u$rec_amigos,
  rec_contemplar = col_u$rec_contemplar,
  rec_desenhar = col_u$rec_desenhar,
  rec_musica = col_u$rec_musica,
  rec_danca = col_u$rec_danca,
  rec_treinamento = col_u$rec_treinamento,
  rec_assistir_treinos = col_u$rec_assistir_treinos,
  rec_jogos_mesa = col_u$rec_jogos_mesa,
  rec_comer_social = col_u$rec_comer_social,
  rec_religioso = col_u$rec_religioso,
  rec_redes_sociais = col_u$rec_redes_sociais,
  rec_jogos_eletronicos = col_u$rec_jogos_eletronicos,
  rec_competicoes = col_u$rec_competicoes,
  rec_filmes = col_u$rec_filmes,
  rec_bebida = col_u$rec_bebida,
  rec_fumar = col_u$rec_fumar,
  rec_academia = col_u$rec_academia,
  rec_interacoes_afetivo_sexuais = col_u$rec_interacoes_afetivo_sexuais,
  rec_ler_ufmg = col_u$rec_ler_ufmg,
  rec_festas_ufmg = col_u$rec_festas_ufmg,
  sm_alegria = col_u$sm_alegria,
  sm_interesse = col_u$sm_interesse,
  sm_satisfacao = col_u$sm_satisfacao,
  sm_contribuicao = col_u$sm_contribuicao,
  sm_comunidade = col_u$sm_comunidade,
  sm_sociedade_melhor = col_u$sm_sociedade_melhor,
  sm_pessoas_boas = col_u$sm_pessoas_boas,
  sm_sociedade_sentido = col_u$sm_sociedade_sentido,
  sm_personalidade = col_u$sm_personalidade,
  sm_responsabilidades = col_u$sm_responsabilidades,
  sm_relacoes = col_u$sm_relacoes,
  sm_crescimento = col_u$sm_crescimento,
  sm_confianca = col_u$sm_confianca,
  sm_vida_sentido = col_u$sm_vida_sentido,
  idade = col_u$idade,
  ano_ingresso = col_u$ano_ingresso,
  horas_universidade_semana = col_u$horas_universidade
)

dominio_coluna <- function(nome) {
  case_when(
    nome %in% c("instituicao_origem", "contexto_institucional", "indicador_ufmg") ~
      "Identificação analítica",
    nome %in% c(
      "mhc_itens_validos", "mhc_total_14_84",
      "mhc_total_0_70", "mhc_codigo",
      "mhc_classificacao", itens_mhc
    ) ~ "MHC-SF",
    nome %in% c(praticas_comuns_20, praticas_exclusivas_4) ~
      "Práticas de lazer",
    str_detect(nome, "^gender") ~ "Gênero",
    str_detect(nome, "^marital") ~ "Estado civil",
    str_detect(nome, "^eth|^race") ~ "Etnia/cor",
    str_detect(nome, "^disability") ~ "Deficiência",
    str_detect(nome, "^res") ~ "Moradia",
    str_detect(nome, "^work") ~ "Trabalho/renda",
    nome %in% c(
      "idade", "ano_ingresso", "anos_no_curso",
      "horas_universidade_semana", "idade_centralizada",
      "anos_curso_centralizados", "horas_universidade_10",
      "nivel_academico_detalhado", "nivel_especializacao",
      "nivel_mestrado", "nivel_doutorado",
      "nivel_pos_nao_especificado", "pos_graduacao"
    ) ~ "Trajetória acadêmica",
    str_detect(nome, "^access") ~ "Acesso externo",
    str_detect(nome, "^puj_stratum|^athlete") ~
      "Específica da Javeriana",
    TRUE ~ "Outro"
  )
}

equivalencia_coluna <- function(nome) {
  case_when(
    nome %in% praticas_comuns_20 |
      nome %in% itens_mhc |
      nome %in% c(
        "idade", "ano_ingresso", "anos_no_curso",
        "horas_universidade_semana", "deficiencia_sim",
        "trabalho_sim", "pos_graduacao"
      ) ~ "Direta ou harmonizada entre as duas bases",
    nome %in% praticas_exclusivas_4 ~
      "Prática exclusiva de uma universidade",
    str_detect(nome, "^eth_puj|^puj_|^athlete|^access_puj|^work_puj") ~
      "Específica da Javeriana; NA na UFMG",
    str_detect(nome, "^race_ufmg|^access_ufmg|^work_ufmg") ~
      "Específica da UFMG; NA na Javeriana",
    nome == "minoria_etnico_racial_sensibilidade" ~
      "Harmonização ampla apenas para sensibilidade",
    nome %in% c(
      "genero_modelo", "genero_homem", "genero_diverso",
      "estado_civil_com_parceiro", "moradia_modelo",
      "moradia_sozinho", "moradia_pares",
      "idade_centralizada", "anos_curso_centralizados", "horas_universidade_10"
    ) ~ "Derivada analítica comum",
    TRUE ~ "Estrutural, descritiva ou derivada"
  )
}

tabela_harmonizacao <- tibble(
  ordem = seq_along(colunas_schema),
  coluna_harmonizada = colunas_schema,
  tipo = unname(schema_tipos[colunas_schema]),
  dominio = map_chr(colunas_schema, dominio_coluna),
  origem_javeriana = if_else(
    colunas_schema %in% names(mapa_javeriana),
    unname(mapa_javeriana[colunas_schema]),
    case_when(
      str_detect(colunas_schema, "^gender") ~ "sexo___1 a sexo___8",
      str_detect(colunas_schema, "^marital") ~ "estado_civil___1 a ___5",
      str_detect(colunas_schema, "^eth_puj|ethnicity") ~ "cultura___1 a ___6",
      str_detect(colunas_schema, "^disability") ~ "deficiencia + tipo_deficiencia___*",
      str_detect(colunas_schema, "^res") ~ "membros_familias___1 a ___5",
      str_detect(colunas_schema, "^work") ~ "trabalho_remunerado + trabalho___* + tempo_trabalho",
      str_detect(colunas_schema, "^level|postgrad") ~ "nivel_pregrado, nivel_especi, nivel_maestria, nivel_doctorado",
      str_detect(colunas_schema, "^access_puj|external_access") ~ "recex1 a recex14",
      str_detect(colunas_schema, "^puj_stratum") ~ "estrato_economico___1 a ___7",
      TRUE ~ "Derivada ou não coletada"
    )
  ),
  origem_ufmg = if_else(
    colunas_schema %in% names(mapa_ufmg),
    unname(mapa_ufmg[colunas_schema]),
    case_when(
      str_detect(colunas_schema, "^gender") ~ col_u$genero,
      str_detect(colunas_schema, "^marital") ~ col_u$estado_civil,
      str_detect(colunas_schema, "^race_ufmg|ethnicity") ~ col_u$cor,
      str_detect(colunas_schema, "^disability") ~ paste(col_u$deficiencia, col_u$tipo_deficiencia, sep = " | "),
      str_detect(colunas_schema, "^res") ~ col_u$moradia,
      str_detect(colunas_schema, "^work") ~ paste(col_u$trabalho, col_u$natureza_trabalho, col_u$horas_trabalho, sep = " | "),
      str_detect(colunas_schema, "^level|postgrad") ~ paste(col_u$nivel, col_u$curso_mestrado, col_u$curso_doutorado, col_u$curso_especializacao, sep = " | "),
      str_detect(colunas_schema, "^access_ufmg|external_access") ~ paste(col_u$acesso_geral, col_u$acesso_lista, sep = " | "),
      TRUE ~ "Derivada ou não coletada"
    )
  ),
  equivalencia = map_chr(colunas_schema, equivalencia_coluna),
  entra_base_analitica = colunas_schema %in% colunas_base_analitica,
  entra_modelo = colunas_schema %in% c(
    "mhc_codigo",
    covariaveis_ajuste,
    praticas_comuns_20
  )
)

tabela_harmonizacao <- tabela_harmonizacao %>%
  mutate(
    n_nao_ausente_javeriana = map_int(
      coluna_harmonizada,
      ~ sum(!is.na(base_javeriana_harmonizada[[.x]]))
    ),
    n_nao_ausente_ufmg = map_int(
      coluna_harmonizada,
      ~ sum(!is.na(base_ufmg_harmonizada[[.x]]))
    ),
    disponibilidade = case_when(
      n_nao_ausente_javeriana > 0L & n_nao_ausente_ufmg > 0L ~
        "Dados nas duas bases",
      n_nao_ausente_javeriana > 0L & n_nao_ausente_ufmg == 0L ~
        "Dados somente na Javeriana",
      n_nao_ausente_javeriana == 0L & n_nao_ausente_ufmg > 0L ~
        "Dados somente na UFMG",
      TRUE ~ "Sem dados observados nos arquivos elegíveis"
    ),
    motivo_coluna_adicional = case_when(
      disponibilidade == "Dados nas duas bases" ~
        "Variável comum, derivada comum ou dummy de opção disponível nas duas estruturas.",
      disponibilidade == "Dados somente na Javeriana" ~
        "Pergunta/opção exclusiva do instrumento colombiano; mantida para auditoria e descrição, com NA na UFMG.",
      disponibilidade == "Dados somente na UFMG" ~
        "Pergunta/opção exclusiva do instrumento brasileiro; mantida para auditoria e descrição, com NA na Javeriana.",
      TRUE ~
        "Coluna estrutural preservada para equivalência do schema, embora sem observações nos casos elegíveis."
    )
  )

resumo_disponibilidade_colunas <- tabela_harmonizacao %>%
  count(disponibilidade, name = "n_colunas") %>%
  arrange(desc(n_colunas))


contagem_colunas <- bind_rows(
  dimensoes_originais %>%
    transmute(
      etapa = base,
      linhas,
      colunas,
      explicacao = "Colunas conforme cada instrumento original."
    ),
  dimensoes_harmonizadas %>%
    transmute(
      etapa = base,
      linhas,
      colunas,
      explicacao = case_when(
        str_detect(base, "harmonizada") ~
          "Mesmo schema de 155 colunas; variáveis não coletadas recebem NA.",
        TRUE ~
          "A união soma linhas, não colunas."
      )
    ),
  tibble(
    etapa = c(
      "Base analítica restrita",
      "Base de modelagem"
    ),
    linhas = c(
      nrow(base_analitica_restrita),
      nrow(base_modelagem)
    ),
    colunas = c(
      ncol(base_analitica_restrita),
      ncol(base_modelagem)
    ),
    explicacao = c(
      "Dummies detalhadas visíveis; sem datas, contatos, cursos ou respostas abertas.",
      "Somente desfecho, práticas comuns e covariáveis analíticas."
    )
  )
)

###13

calcular_alpha <- function(df_itens) {
  
  dados <- as.data.frame(df_itens)
  
  # Mantém somente as linhas completas.
  # A segunda vírgula indica que todas as colunas serão preservadas.
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
# =============================================================================
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
    n_definhamento = unname(contagens["1"] %||% 0L),
    n_moderado = unname(contagens["2"] %||% 0L),
    n_florescimento = unname(contagens["3"] %||% 0L),
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
    select(all_of(termos)) %>%
    filter(complete.cases(.))
  
  matriz <- model.matrix(
    reformulate(termos),
    data = dados_completos
  )
  
  if ("(Intercept)" %in% colnames(matriz)) {
    matriz <- matriz[, colnames(matriz) != "(Intercept)", drop = FALSE]
  }
  
  if (ncol(matriz) < 2L) return(1)
  
  matriz <- scale(matriz)
  matriz <- matriz[, apply(matriz, 2, sd, na.rm = TRUE) > 0, drop = FALSE]
  kappa(matriz, exact = TRUE)
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
probabilidades <- tryCatch(
  predict(
    objeto$ajuste,
    newdata = objeto$dados,
    type = "prob"
  )$fit,
  error = function(e) NULL
)

if (is.null(probabilidades)) {
  return(list(
    resumo = tibble(
      modelo = objeto$nome,
      n = objeto$n,
      acuracia_aparente = NA_real_,
      brier_multiclasse = NA_real_,
      status = "Falha na predição"
    ),
    confusao = tibble()
  ))
}

classes <- colnames(probabilidades)
pred <- classes[max.col(probabilidades, ties.method = "first")]
obs <- as.character(objeto$dados$mhc_ordinal)

one_hot <- sapply(
  classes,
  function(classe) as.integer(obs == classe)
)
brier <- mean(rowSums((probabilidades - one_hot)^2))

conf <- as.data.frame.matrix(table(
  observado = obs,
  predito = pred
)) %>%
  rownames_to_column("observado") %>%
  as_tibble()

list(
  resumo = tibble(
    modelo = objeto$nome,
    n = objeto$n,
    acuracia_aparente = mean(pred == obs),
    brier_multiclasse = brier,
    status = "Calculado"
  ),
  confusao = conf
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
    `Moderate/Flourishing versus Languishing` =
      as.integer(base$mhc_codigo >= 2L),
    `Flourishing versus Languishing/Moderate` =
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
# 15. MODELOS
# =============================================================================

# Amostra comum para os modelos ajustados e cumulativos.
base_ajuste_comum <- base_modelagem %>%
  select(
    instituicao_origem, contexto_institucional,
    mhc_codigo, mhc_classificacao, mhc_ordinal,
    all_of(covariaveis_ajuste),
    all_of(praticas_comuns_20)
  ) %>%
  filter(complete.cases(.))

tamanho_amostras_modelo <- tibble(
  base = c(
    "Base modelagem total",
    "Amostra comum ajustada"
  ),
  n = c(
    nrow(base_modelagem),
    nrow(base_ajuste_comum)
  ),
  languishing = c(
    sum(base_modelagem$mhc_codigo == 1L, na.rm = TRUE),
    sum(base_ajuste_comum$mhc_codigo == 1L)
  ),
  moderate = c(
    sum(base_modelagem$mhc_codigo == 2L, na.rm = TRUE),
    sum(base_ajuste_comum$mhc_codigo == 2L)
  ),
  flourishing = c(
    sum(base_modelagem$mhc_codigo == 3L, na.rm = TRUE),
    sum(base_ajuste_comum$mhc_codigo == 3L)
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
  Gênero = c("genero_homem", "genero_diverso"),
  `Estado civil` = "estado_civil_com_parceiro",
  Deficiência = "deficiencia_sim",
  Moradia = c("moradia_sozinho", "moradia_pares"),
  `Nível acadêmico` = "pos_graduacao",
  `Tempo no curso` = "anos_curso_centralizados",
  `Trabalho remunerado` = "trabalho_sim",
  `Horas na universidade` = "horas_universidade_10"
)

modelos_sociodemograficos <- imap(
  blocos_sociodemograficos,
  ~ ajustar_clm_seguro(
    base_ajuste_comum,
    .x,
    paste0("S – ", .y)
  )
)

modelos_praticas_brutos <- setNames(
  map(
    praticas_comuns_20,
    ~ ajustar_clm_seguro(
      base_modelagem,
      .x,
      paste0("Bruto – ", .x)
    )
  ),
  praticas_comuns_20
)

modelos_praticas_ajustados <- setNames(
  map(
    praticas_comuns_20,
    ~ ajustar_clm_seguro(
      base_ajuste_comum,
      c(.x, covariaveis_ajuste),
      paste0("Ajustado – ", .x)
    )
  ),
  praticas_comuns_20
)


falhas_modelos_principais <- names(modelos_praticas_ajustados)[
  !vapply(
    modelos_praticas_ajustados,
    function(x) isTRUE(x$ok),
    logical(1)
  )
]

if (length(falhas_modelos_principais) > 0L) {
  detalhes_falhas <- map_chr(
    modelos_praticas_ajustados[falhas_modelos_principais],
    ~ paste0(.x$status %||% "Erro", ": ", .x$erro %||% "sem mensagem")
  )
  stop(
    paste0(
      "Falha em modelos ajustados da estratégia principal:\n",
      paste(
        falhas_modelos_principais,
        detalhes_falhas,
        sep = " | ",
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

termos_B1 <- c("indicador_ufmg", "idade_centralizada")
termos_B2 <- c(
  termos_B1,
  "genero_homem", "genero_diverso"
)
termos_B3 <- covariaveis_ajuste
termos_B4 <- c(termos_B3, praticas_teoricas_13)
termos_B5 <- c(termos_B3, praticas_comuns_20)

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
    "B3 – Ajuste sociodemográfico completo"
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

# FDR apenas dentro de cada família dos 20 modelos prática por vez.
coef_praticas_brutas <- map_dfr(
  modelos_praticas_brutos,
  extrair_coeficientes
) %>%
  filter(
    tipo_parametro == "Localização/OR proporcional",
    termo %in% praticas_comuns_20
  ) %>%
  mutate(p_FDR_BH = p.adjust(p, method = "BH"))

coef_praticas_ajustadas <- map_dfr(
  modelos_praticas_ajustados,
  extrair_coeficientes
) %>%
  filter(
    tipo_parametro == "Localização/OR proporcional",
    termo %in% praticas_comuns_20
  ) %>%
  mutate(p_FDR_BH = p.adjust(p, method = "BH"))

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

extrair_termos_significativos <- function(tabela, termos_validos) {
  tabela %>%
    filter(
      status == "Executado",
      !is.na(p),
      p < alpha,
      termo %in% termos_validos
    ) %>%
    pull(termo) %>%
    unique()
}

termos_nominais_B4 <- extrair_termos_significativos(
  teste_nominal_B4,
  termos_B4
)
termos_scale_B4 <- extrair_termos_significativos(
  teste_scale_B4,
  termos_B4
)

# Limita modelos de sensibilidade a três termos para evitar parametrização
# descontrolada. Todos os termos sinalizados continuam registrados nas tabelas.
termos_nominais_ajuste <- head(termos_nominais_B4, 3L)
termos_scale_ajuste <- head(termos_scale_B4, 3L)

modelo_ppo_B4 <- if (length(termos_nominais_ajuste) > 0L) {
  ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B4,
    "B4-PPO – chances proporcionais parciais",
    nominal_termos = termos_nominais_ajuste
  )
} else {
  list(
    ok = FALSE,
    nome = "B4-PPO – chances proporcionais parciais",
    erro = "Nenhum termo significativo no nominal_test() do B4."
  )
}

modelo_scale_B4 <- if (length(termos_scale_ajuste) > 0L) {
  ajustar_clm_seguro(
    base_ajuste_comum,
    termos_B4,
    "B4-SCALE – sensibilidade com efeito de escala",
    scale_termos = termos_scale_ajuste
  )
} else {
  list(
    ok = FALSE,
    nome = "B4-SCALE – sensibilidade com efeito de escala",
    erro = "Nenhum termo significativo no scale_test() do B4."
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

coeficientes_PPO <- extrair_coeficientes(modelo_ppo_B4)
coeficientes_SCALE <- extrair_coeficientes(modelo_scale_B4)

or_limiares <- if (length(termos_nominais_ajuste) > 0L) {
  ajustar_limiares_binarios(
    base_ajuste_comum,
    termos_B4,
    termos_nominais_ajuste,
    "Diagnóstico por limiar – B4"
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

vif_B4 <- calcular_vif(base_ajuste_comum, termos_B4)
vif_B5 <- calcular_vif(base_ajuste_comum, termos_B5)

condicao_modelos <- tibble(
  modelo = c("B4 – 13 práticas", "B5 – 20 práticas"),
  numero_condicao = c(
    numero_condicao(base_ajuste_comum, termos_B4),
    numero_condicao(base_ajuste_comum, termos_B5)
  )
)

avaliacao_B4 <- avaliar_classificacao(modelos_blocos$B4)
avaliacao_B5 <- avaliar_classificacao(modelos_blocos$B5)
avaliacao_PPO <- avaliar_classificacao(modelo_ppo_B4)

classificacao_resumo <- bind_rows(
  avaliacao_B4$resumo,
  avaliacao_B5$resumo,
  avaliacao_PPO$resumo
)

# Modelo principal:
# - PPO somente se foi ajustado e a comparação LRT favorece PPO;
# - caso contrário, B4-PO.
ppo_favorecido_B4 <- (
  isTRUE(modelo_ppo_B4$ok) &&
    nrow(comparacao_PO_PPO) == 1L &&
    is.finite(comparacao_PO_PPO$p) &&
    comparacao_PO_PPO$p < alpha
)

modelo_total_selecionado_nome <- if (ppo_favorecido_B4) {
  modelo_ppo_B4$nome
} else if (isTRUE(modelos_blocos$B4$ok)) {
  modelos_blocos$B4$nome
} else {
  paste0(
    "Nenhum modelo total plenamente válido; B4 status: ",
    modelos_blocos$B4$status %||% modelos_blocos$B4$erro
  )
}

modelo_principal_nome <- paste0(
  "Modelos ajustados prática por prática (20 práticas, amostra comum, ",
  "FDR-BH); modelo total selecionado: ",
  modelo_total_selecionado_nome
)

decisao_modelo <- tibble(
  item = c(
    "Estratégia inferencial principal",
    "Modelo total PO de referência",
    "Modelo total selecionado",
    "Modelo PPO do B4",
    "Modelo de escala do B4",
    "Modelo com 20 práticas"
  ),
  decisao = c(
    paste0(
      "Modelos ajustados prática por prática, sobre a mesma amostra, ",
      "com correção FDR-BH."
    ),
    "B4 – Total teórico com 13 práticas; análise multivariável de sensibilidade.",
    modelo_total_selecionado_nome,
    if (is.null(modelo_ppo_B4$ajuste)) {
      modelo_ppo_B4$erro
    } else {
      paste0(
        "Ajustado com: ",
        paste(termos_nominais_ajuste, collapse = ", "),
        ". Só substitui o PO se o modelo for válido e o LRT for significativo."
      )
    },
    if (is.null(modelo_scale_B4$ajuste)) {
      modelo_scale_B4$erro
    } else {
      paste0(
        "Sensibilidade com: ",
        paste(termos_scale_ajuste, collapse = ", ")
      )
    },
    paste0(
      "Exploratório; não interpretar como modelo principal devido à ",
      "sobreparametrização e à baixa frequência de Languishing."
    )
  )
)

# =============================================================================
# 16. EXPORTAÇÃO SEGURA
# =============================================================================

# A origem institucional é necessária apenas durante as análises comparativas.
# Ela, o indicador institucional e todas as colunas específicas de uma
# universidade são retirados dos microdados exportados.

colunas_institucionais_ou_quase_identificadoras <- unique(c(
  "instituicao_origem",
  "contexto_institucional",
  "indicador_ufmg",
  "genero_detalhado",
  "genero_num_opcoes",
  "estado_civil_detalhado",
  "sistema_etnia_cor",
  "etnia_cor_detalhada",
  "minoria_etnico_racial_sensibilidade",
  "deficiencia_detalhada",
  "moradia_detalhada",
  "moradia_num_opcoes",
  "trabalho_detalhado",
  "horas_trabalho_semana",
  "ano_ingresso",
  "nivel_academico_detalhado",
  praticas_exclusivas_4,
  grep(
    "(^|_)(puj|ufmg)($|_)",
    names(base_unificada),
    value = TRUE
  )
))

# Faixas substituem medidas sociodemográficas exatas nos microdados exportados.
base_unificada_sem_identificadores <- base_unificada %>%
  mutate(
    faixa_etaria = cut(
      idade,
      breaks = c(17, 20, 23, 26, 29),
      labels = c("18–20", "21–23", "24–26", "27–29"),
      include.lowest = TRUE
    ),
    faixa_tempo_curso = cut(
      anos_no_curso,
      breaks = c(-Inf, 1, 3, 5, Inf),
      labels = c("Até 1 ano", "2–3 anos", "4–5 anos", "6 anos ou mais")
    ),
    faixa_horas_universidade = cut(
      horas_universidade_semana,
      breaks = c(-Inf, 20, 40, 60, Inf),
      labels = c("Até 20", "21–40", "41–60", "Mais de 60")
    )
  ) %>%
  select(
    -any_of(colunas_institucionais_ou_quase_identificadoras),
    -any_of(c(
      "idade", "idade_centralizada",
      "anos_no_curso", "anos_curso_centralizados",
      "horas_universidade_semana", "horas_universidade_10"
    ))
  )

# Base analítica reduzida: somente variáveis comparáveis e categorias agrupadas.
colunas_base_analitica_sem_identificadores <- unique(c(
  "mhc_itens_validos", "mhc_total_14_84", "mhc_total_0_70",
  "mhc_codigo", "mhc_classificacao",
  itens_mhc,
  praticas_comuns_20,
  "genero_modelo", "genero_homem", "genero_diverso",
  "estado_civil_com_parceiro",
  "deficiencia_sim",
  "moradia_modelo", "moradia_sozinho", "moradia_pares",
  "trabalho_sim", "pos_graduacao",
  "faixa_etaria", "faixa_tempo_curso", "faixa_horas_universidade"
))

base_analitica_sem_identificadores <- base_unificada_sem_identificadores %>%
  select(all_of(colunas_base_analitica_sem_identificadores))

colunas_proibidas_exportacao <- c(
  "codigo_analitico", "instituicao_origem", "contexto_institucional",
  "indicador_ufmg", "atleta_sim",
  "estrato_socioeconomico_detalhado",
  paste0("estrato_socioeconomico_", 2:6),
  "estrato_socioeconomico_nao_respondeu"
)

problemas_exportacao <- union(
  intersect(names(base_unificada_sem_identificadores), colunas_proibidas_exportacao),
  intersect(names(base_analitica_sem_identificadores), colunas_proibidas_exportacao)
)

if (length(problemas_exportacao) > 0L) {
  stop(
    paste0(
      "A exportação ainda contém campos proibidos: ",
      paste(problemas_exportacao, collapse = ", ")
    ),
    call. = FALSE
  )
}

# Todos os nomes das colunas exportadas devem estar em português.
termos_ingles_proibidos <- c(
  "gender", "marital", "ethnicity", "race", "disability",
  "residence", "work", "level", "access", "athlete", "stratum"
)

nomes_ingles <- names(base_unificada_sem_identificadores)[
  stringr::str_detect(
    names(base_unificada_sem_identificadores),
    stringr::regex(
      paste(termos_ingles_proibidos, collapse = "|"),
      ignore_case = TRUE
    )
  )
]

if (length(nomes_ingles) > 0L) {
  stop(
    paste0(
      "Ainda existem nomes de colunas em inglês: ",
      paste(nomes_ingles, collapse = ", ")
    ),
    call. = FALSE
  )
}

arquivo_excel <- file.path(
  pasta_saida,
  "RESULTADOS_FINAIS_CORRIGIDOS.xlsx"
)

arquivo_base_unificada <- file.path(
  pasta_saida,
  "BASE_UNIFICADA_SEM_IDENTIFICADORES.csv"
)

arquivo_base_analitica <- file.path(
  pasta_saida,
  "BASE_ANALITICA_SEM_IDENTIFICADORES.csv"
)

readr::write_csv(
  base_unificada_sem_identificadores,
  arquivo_base_unificada,
  na = ""
)

readr::write_csv(
  base_analitica_sem_identificadores,
  arquivo_base_analitica,
  na = ""
)

lista_excel <- list(
  `Leia-me` = tibble(
    item = c(
      "Status deste arquivo",
      "Colunas originais",
      "Colunas harmonizadas",
      "União",
      "Referências",
      "Etnia/cor",
      "Carga de trabalho",
      "Modelo principal"
    ),
    descricao = c(
      "Arquivo de resultados. Os microdados exportados não contêm instituição, código individual, condição de atleta nem estrato socioeconômico; tabelas compartilháveis suprimem n<5.",
      "Javeriana: 118; UFMG: 61.",
      "As duas bases internas têm 155 colunas idênticas em nomes, ordem e tipos.",
      "A união interna soma linhas: 102 + 125 = 227. A versão exportada remove campos institucionais e quase identificadores.",
      "Não existe dummy própria para a categoria de referência.",
      "Blocos nacionais separados; eth_minority_sens não entra no modelo principal.",
      "Mantida apenas para descrição. Não entra nas fórmulas.",
      modelo_principal_nome
    )
  ),
  `Fluxo amostral` = fluxo_amostral,
  `Dimensões e colunas` = contagem_colunas,
  `Comparação tipos` = comparacao_tipos,
  `Harmonização completa` = tabela_harmonizacao,
  `Resumo disponibilidade` = resumo_disponibilidade_colunas,
  `Dummies e referências` = tabela_referencias,
  `Decisões harmonização` = decisoes_harmonizacao,
  `Validação dummies` = auditoria_validade_dummies,
  `Auditoria dummies` = auditoria_dummies,
  `Duplicidades` = auditoria_duplicatas,
  `Frequências esperadas` = validacoes_categorias,
  `Tamanho amostras modelo` = tamanho_amostras_modelo,
  `Descritiva categórica` = tabela_descritiva_categorica,
  `Descritiva contínua` = tabela_descritiva_continua,
  `Dados ausentes` = tabela_ausencias,
  `Células raras` = tabela_celulas_raras,
  `Alfa MHC-SF` = alpha_mhc,
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
  `OR por limiar` = or_limiares,
  `VIF B4` = vif_B4,
  `VIF B5` = vif_B5,
  `Número condição` = condicao_modelos,
  `Classificação resumo` = classificacao_resumo,
  `Confusão B4` = avaliacao_B4$confusao,
  `Confusão B5` = avaliacao_B5$confusao,
  `Confusão PPO` = avaliacao_PPO$confusao,
  `Decisão modelo` = decisao_modelo
)

garantir_planilha_nao_vazia <- function(x) {
  if (is.data.frame(x) && nrow(x) == 0L) {
    return(tibble(mensagem = "Sem resultados para esta etapa."))
  }
  x
}


escrever_lista_excel <- function(lista, arquivo) {
  wb <- openxlsx::createWorkbook()
  
  for (nome_aba in names(lista)) {
    nome_seguro <- substr(nome_aba, 1L, 31L)
    openxlsx::addWorksheet(wb, nome_seguro)
    dados_aba <- garantir_planilha_nao_vazia(lista[[nome_aba]])
    dados_aba <- as.data.frame(dados_aba, stringsAsFactors = FALSE)
    
    openxlsx::writeDataTable(
      wb,
      sheet = nome_seguro,
      x = dados_aba,
      withFilter = TRUE,
      tableStyle = "TableStyleMedium2"
    )
    openxlsx::freezePane(wb, sheet = nome_seguro, firstRow = TRUE)
    openxlsx::setColWidths(
      wb,
      sheet = nome_seguro,
      cols = seq_len(max(1L, ncol(dados_aba))),
      widths = "auto"
    )
  }
  
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
  invisible(arquivo)
}

lista_excel <- map(
  lista_excel,
  garantir_planilha_nao_vazia
)

escrever_lista_excel(lista_excel, arquivo_excel)

# Tabelas agregadas destinadas a revisão/compartilhamento.
lista_agregada <- list(
  `Leia-me` = tibble(
    item = c(
      "Proteção de células pequenas",
      "Microdados",
      "Interpretação principal",
      "Modelo total"
    ),
    descricao = c(
      "Categorias com n<5 são exibidas como <5 e têm percentuais suprimidos.",
      "Este arquivo não contém microdados individuais.",
      "Modelos ajustados prática por prática, com amostra comum e FDR-BH.",
      "B4 com 13 práticas é análise multivariável de sensibilidade; B5 é exploratório."
    )
  ),
  `Fluxo amostral` = fluxo_amostral,
  `Descritiva categórica` = suprimir_celulas_categoricas(
    tabela_descritiva_categorica,
    limite = 5L
  ),
  `Descritiva contínua` = suprimir_celulas_continuas(
    tabela_descritiva_continua,
    limite = 5L
  ),
  `MHC-SF` = suprimir_celulas_categoricas(
    tabela_descritiva_categorica %>%
      filter(variavel == "mhc_classificacao"),
    limite = 5L
  ),
  `Práticas ajustadas principais` = coef_praticas_ajustadas,
  `Modelo total selecionado` = if (ppo_favorecido_B4) {
    coeficientes_PPO
  } else {
    extrair_coeficientes(modelos_blocos$B4)
  },
  `Decisão analítica` = decisao_modelo
)

lista_agregada <- map(
  lista_agregada,
  garantir_planilha_nao_vazia
)

escrever_lista_excel(
  lista_agregada,
  file.path(
    pasta_saida,
    "TABELAS_AGREGADAS_COMPARTILHAVEIS.xlsx"
  )
)

# Os objetos de modelos não são exportados em RDS, pois podem conter o model frame individual.
capture.output(
  sessionInfo(),
  file = file.path(pasta_saida, "sessionInfo.txt")
)

versoes_pacotes <- tibble(
  pacote = pacotes,
  versao = map_chr(
    pacotes,
    ~ as.character(packageVersion(.x))
  )
)
write_csv(
  versoes_pacotes,
  file.path(pasta_saida, "versoes_pacotes.csv")
)

md5_bases <- tools::md5sum(c(
  arquivo_javeriana,
  arquivo_ufmg
))
write_csv(
  tibble(
    arquivo = names(md5_bases),
    md5 = unname(md5_bases)
  ),
  file.path(pasta_saida, "md5_bases_originais.csv")
)

resumo_execucao <- c(
  "EXECUÇÃO CONCLUÍDA",
  paste0("Javeriana elegível: ", nrow(base_javeriana_harmonizada)),
  paste0("UFMG elegível: ", nrow(base_ufmg_harmonizada)),
  paste0("Total: ", nrow(base_unificada)),
  paste0("Colunas harmonizadas: ", ncol(base_unificada)),
  paste0("Colunas base analítica sem identificadores: ", ncol(base_analitica_sem_identificadores)),
  paste0("Colunas base unificada sem identificadores: ", ncol(base_unificada_sem_identificadores)),
  paste0(
    "MHC-SF: ",
    paste(
      names(table(base_unificada$mhc_codigo)),
      as.integer(table(base_unificada$mhc_codigo)),
      sep = "=",
      collapse = "; "
    )
  ),
  paste0("Amostra comum ajustada: ", nrow(base_ajuste_comum)),
  paste0("Estratégia/modelo: ", modelo_principal_nome),
  paste0("B4 status: ", modelos_blocos$B4$status),
  paste0("B5 status: ", modelos_blocos$B5$status)
)

writeLines(
  resumo_execucao,
  file.path(pasta_saida, "00_RESUMO_EXECUCAO.txt")
)

walk(resumo_execucao, registrar)
registrar("FIM DA EXECUÇÃO")
