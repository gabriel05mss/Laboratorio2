# VOU MANTER PARA FACILITAR MINHA VIDA, MAS SÃO COISAS MAU FORMATADAS ----
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


checkbox_01 <- function(x) {
  valor <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(valor) ~ NA_integer_,
    valor == 1 ~ 1L,
    valor == 0 ~ 0L,
    TRUE ~ NA_integer_
  )
}

juntar_rotulos <- function(valores, rotulos) {
  valores <- as.integer(valores)
  selecionados <- rotulos[which(valores == 1L)]
  if (length(selecionados) == 0L) return(NA_character_)
  paste(selecionados, collapse = "; ")
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

normalizar_texto <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_lower(str_squish(coalesce(x, "")))
  x
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

sim_nao_num <- function(x) {
  texto <- normalizar_texto(x)
  case_when(
    texto == "" ~ NA_integer_,
    texto == "sim" ~ 1L,
    texto == "nao" ~ 0L,
    TRUE ~ NA_integer_
  )
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

comparar_tipos <- function(df1, df2) {
  tibble(
    coluna = names(df1),
    tipo_javeriana = vapply(df1, function(x) class(x)[1], character(1)),
    tipo_ufmg = vapply(df2, function(x) class(x)[1], character(1)),
    mesmo_tipo = tipo_javeriana == tipo_ufmg
  )
}

# CAMINHO DOS ARQUIVOS ----
pasta_dados <- "data/"

nome_arquivo_javeriana <- "javeriana.csv"
nome_arquivo_ufmg <- "ufmg.xlsx"

pasta_saida <- file.path(
  pasta_dados,
  "resultados_tese_ordinal_CORRIGIDOS"
)

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

# NÃO SEI PORQUE MAS ESTA NO CODIGO DA CLIENTE ----
data_min_javeriana <- as.Date("2026-03-09")
data_min_ufmg <- as.Date("2026-04-28")
ano_analise <- 2026L

# PACOTES ----

pacotes <- c(
  "readr", "dplyr", "stringr", "lubridate",
  "tidyr", "purrr", "tibble", "openxlsx", "ordinal"
)

pacotes_ausentes <- pacotes[
  !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
]

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

# LOCALIZAÇÃO E LEITURA DOS ARQUIVOS ORIGINAIS ----

arquivo_javeriana <- file.path(
  pasta_dados,
  "javeriana.csv"
)

arquivo_ufmg <- file.path(
  pasta_dados,
  "ufmg.xlsx"
)

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

base_javeriana_original <- readr::read_csv(
  arquivo_javeriana,
  show_col_types = FALSE,
  name_repair = "minimal"
)

base_ufmg_original <- read.xlsx(arquivo_ufmg)

# RENOMEAR COLUNAS ----

base_ufmg <- base_ufmg_original %>%
  rename(
    data = `Carimbo.de.data/hora`,
    elegibilidade = `Nosso.questionário.é.destinado.a.pessoas.com.idade.entre.18.e.29.anos..Você.se.encontra.dentro.desta.faixa.etária?`,
    consentimento = `TERMO.DE.CONSENTIMENTO.LIVRE.E.ESCLARECIDO.O(a).Sr.(a).está.sendo.convidado(a).a.participar.como.voluntário(a).da.pesquisa."LAZER.E.SAÚDE.MENTAL.POSITIVA:.Associações.das.práticas.de.lazer.com.a.percepção.saúde.mental.positiva.dos.jovens.universitários.da.Universidade.Federal.de.Minas.Gerais.(Brasil).e.Universidade.Pontifícia.Javeriana.de.Bogotá.(Colômbia).",.que.tem.como.objetivo.estabelecer.a.associação.entre.as.práticas.de.lazer.vivenciadas.na.universidade.e.a.percepção.de.saúde.mental.positiva.dos.jovens.estudantes.da.Universidade.Federal.de.Minas.Gerais.e.da.Pontifícia.Universidad.Javeriana.de.Bogotá..Esta.pesquisa.corresponde.a.uma.tese.de.doutorado.no.programa.de.Pós-graduação.Interdisciplinar.em.Estudos.do.Lazer.(PPGIEL).da.Escola.de.Educação.Física,.Fisioterapia.e.Terapia.Ocupacional.da.Universidade.Federal.de.Minas.Gerais.(UFMG),.sob.a.responsabilidade.da.doutoranda.Namuetcha.Silva.Ricardo,.orientação.da.professora.Drª..Ana.Cláudia.Porfírio.Couto.e.co-orientação.do.Profº..Dr..Argemiro.Alberto.Floréz.Pregonero..Será.aplicado.um.questionário.com.três.seções..A.primeira.seção,."Práticas.de.lazer.por.jovens.universitários",.foi.validada.em.português.por.especialistas.na.área.do.lazer..A.segunda.seção.consiste.no.questionário.sobre.saúde.mental.-.Mental.Health.Continuum.-.Short.Form.(MHC-SF),.validado.em.português.por.Machado.e.Oliveira.(2015)..A.terceira.seção.refere-se.a.dados.sociodemográficos,.específicos.para.o.público-alvo.desta.pesquisa..Os.riscos.da.pesquisa.incluem.possível.desconforto.ou.constrangimento.ao.responder.às.perguntas..Caso.isso.ocorra,.você.poderá.interromper.sua.participação.a.qualquer.momento..Sua.participação.é.voluntária,.não.haverá.qualquer.custo.ou.compensação.financeira,.e.você.pode.recusar-se.a.participar.ou.retirar.seu.consentimento.a.qualquer.momento..Todos.os.dados.serão.mantidos.em.sigilo.e.usados.apenas.para.fins.acadêmicos.e.científicos..A.pesquisa.foi.aprovada.pelo.Comitê.de.Ética.da.UFMG,.conforme.parecer.CAAE:.79712924.8.0000.5149..Em.caso.de.dúvidas,.entre.em.contato.com:.Pesquisadora.responsável:.Ana.Cláudia.Porfírio.Couto.(anacouto@ufmg.br).Co-orientador:.Argemiro.Alberto.Floréz.Pregonero.(floreza@javeriana.edu.co).Doutoranda:.Namuetcha.Silva.Ricardo.(namuetcha.bh@gmail.com).COEP-UFMG:.coep@prpq.ufmg.br.-.(31).3409-4592`,
    rec_caminhar = Caminhada.pelo.campus.para.relaxamento.e.lazer.,
    rec_casal = `Passar.tempo.com.o(a).parceiro(a).em.momentos.de.lazer.no.campus.`,
    rec_amigos = `Momentos.de.convivência.com.amigos.no.campus.(Ex:.sentar.para.conversar.assuntos.diversos).`,
    rec_contemplar = `Sentar-me.em.áreas.abertas.para.relaxar.`,
    rec_desenhar = `Desenhar.por.prazer,.como.forma.de.relaxar.`,
    rec_musica = Participar.de.aulas.de.música.como.atividade.de.lazer.,
    rec_danca = Participar.de.aulas.de.dança.como.atividade.de.lazer.,
    rec_treinamento = Participar.de.treinos.esportivos.como.prática.de.lazer.,
    rec_assistir_treinos = Assistir.a.treinos.esportivos.como.forma.de.lazer.,
    rec_jogos_mesa = Jogar.jogos.de.cartas.ou.tabuleiro.com.amigos.por.lazer.,
    rec_comer_social = Comer.em.contexto.social.,
    rec_ler_ufmg = `Ler.assuntos.que.não.fazem.parte.das.disciplinas,.apenas.por.interesse.pessoal.`,
    rec_religioso = Participar.de.encontros.religiosos.como.forma.de.lazer.espiritual.e.convivência.,
    rec_festas_ufmg = Ir.a.festas.realizadas.no.campus.para.socializar.e.se.divertir.,
    rec_redes_sociais = Utilizar.redes.sociais.por.lazer.,
    rec_jogos_eletronicos = `Jogar.jogos.eletrônicos.(online.ou.offline).por.lazer.`,
    rec_competicoes = Participar.de.competições.esportivas.por.lazer.,
    rec_filmes = `Assistir.filmes.online.(não.relacionados.aos.estudos)`,
    rec_bebida = Consumir.bebidas.alcoólicas.dentro.da.universidade.como.prática.de.lazer.,
    rec_fumar = Fumar.dentro.da.universidade.como.prática.de.lazer.,
    rec_academia = Frequentar.a.academia.do.campus.como.forma.de.lazer,
    rec_interacoes_afetivo_sexuais = `Vivenciar.interações.afetivo-sexuais.no.contexto.universitário.`,
    acesso_lista = Marque.as.opções.de.lazer.a.que.você.tem.acesso.fora.da.universidade,
    acesso_geral = `Tem.acesso.a.equipamentos.de.lazer.fora.da.universidade?.(Ex:.clube,.quadras.privadas,.academias.privadas,.cinema,.teatro)`,
    sm_interesse = `Interessada(o).pela.vida.`,
    sm_satisfacao = `Satisfeito.(a)`,
    sm_alegria = Feliz,
    sm_contribuicao = Que.você.teve.algo.importante.para.contribuir.para.a.sociedade.,
    sm_comunidade = `Que.você.pertencia.a.uma.comunidade.(como.um.grupo.social.ou.sua.vizinhança).`,
    sm_sociedade_melhor = Que.nossa.sociedade.está.se.tornando.um.lugar.melhor.para.pessoas.como.você.,
    sm_pessoas_boas = `Que.as.pessoas,.em.geral,.são.boas.`,
    sm_sociedade_sentido = Que.a.forma.como.a.nossa.sociedade.funciona.faz.sentido.para.você.,
    sm_personalidade = Que.você.gostava.da.maior.parte.de.suas.características.de.personalidade.,
    sm_responsabilidades = Que.você.administrou.bem.as.responsabilidades.do.seu.dia.a.dia.,
    sm_relacoes = Que.você.tinha.relacionamentos.afetuosos.e.de.confiança.com.outras.pessoas.,
    sm_crescimento = `Que.você.teve.experiências.que.o.desafiaram.a.crescer.e.tornar-se.uma.pessoa.melhor.`,
    sm_confianca = Que.você.foi.confiante.para.pensar.ou.expressar.suas.ideias.e.opiniões.próprias.,
    sm_vida_sentido = Que.sua.vida.tem.um.propósito.ou.um.sentido.,
    genero = `Como.você.se.identifica.em.relação.ao.seu.gênero?`,
    idade = `Qual.a.sua.idade?.(Em.anos.completos)`,
    estado_civil = `Qual.o.seu.estado.civil?`,
    cor = `Qual.sua.cor?`,
    deficiencia = `Você.possui.alguma.deficiência?`,
    tipo_deficiencia = `Caso.tenha.respondido.sim,.selecione.a.opção.ou.as.opções.abaixo.`,
    moradia = `Quem.reside.com.você.em.sua.casa?`,
    trabalho = `Você.possui.alguma.renda/salário.atualmente?`,
    natureza_trabalho = `Se.sim,.qual.a.natureza.da.sua.renda/salário?.(Marque.apenas.uma.opção)`,
    horas_trabalho = `Qual.sua.carga.horária.de.trabalho.semanal.?`,
    ano_ingresso = `Em.que.ano.você.iniciou.seu.curso.atual.na.universidade?`,
    horas_universidade = `Em.média,.quantas.horas.semanais.você.permanece.na.universidade.?`,
    nivel = `Você.é.estudante.de.?`,
    curso_graduacao = `Qual.o.seu.curso.de.graduação?`,
    curso_mestrado = Mestrado,
    curso_doutorado = Doutorado,
    curso_especializacao = Especialização,
    interesse_grupo = `Você.possui.interesse.em.para.participar.do.grupo.focal?`,
    email = `E-mail`,
    whatsapp = WhatsApp
  )

# FILTROS ----

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

base_ufmg_filtrada <- base_ufmg %>%
  mutate(
    .data_hora_filtro = convertToDateTime(data)
  ) %>%
  filter(
    !is.na(.data_hora_filtro),
    as.Date(.data_hora_filtro) >= data_min_ufmg,
    elegibilidade == "Sim",
    str_detect(consentimento, fixed("Concordo em participar desta pesquisa"))
  )

# SCHEMA ÚNICO DAS BASES HARMONIZADAS ----

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

# HARMONIZAÇÃO DA JAVERIANA ----

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

# FUNÇÃO MAL FORMATADA E MUITO REDUNDANTE ----

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


# HARMONIZAÇÃO DA UFMG ----

base_ufmg_harmonizada <- base_ufmg_filtrada %>%
  mutate(
    instituicao_origem = "UFMG",
    contexto_institucional = "UFMG – Brasil",
    indicador_ufmg = 1L,
    across(
      any_of(colunas_numeric),
      as.numeric
    ),
    .gender_text = as.character(genero),
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
    .civil_text = as.character(estado_civil),
    .marital_single_ref = dummy_exato(.civil_text, "Solteiro(a)"),
    estado_civil_casado = dummy_exato(.civil_text, "Casado(a)"),
    estado_civil_viuvo = NA_integer_,
    estado_civil_divorciado = dummy_exato(.civil_text, "Divorciado(a)"),
    estado_civil_uniao_estavel = dummy_exato(.civil_text, "União estável"),
    estado_civil_nao_respondeu = dummy_exato(.civil_text, "Prefiro não responder"),
    
    .race_text = as.character(cor),
    .race_ufmg_white_ref = dummy_exato(.race_text, "Branca"),
    cor_raca_ufmg_preta = dummy_exato(.race_text, "Preta"),
    cor_raca_ufmg_parda = dummy_exato(.race_text, "Parda"),
    cor_raca_ufmg_amarela = dummy_exato(.race_text, "Amarela"),
    cor_raca_ufmg_indigena = dummy_exato(.race_text, "Indígena"),
    cor_raca_ufmg_nao_respondeu = dummy_exato(
      .race_text,
      "Prefiro não declarar"
    ),
    
    deficiencia_sim = sim_nao_num(deficiencia),
    
    deficiencia_detalhada = case_when(
      deficiencia_sim == 1L ~ "Sim",
      deficiencia_sim == 0L ~ "Não",
      TRUE ~ NA_character_
    ),
    
    .disability_type_text = as.character(
      tipo_deficiencia
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
    
    .residence_text = as.character(moradia),
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
    
    trabalho_detalhado = as.character(trabalho),
    trabalho_sim = sim_nao_num(trabalho_detalhado),
    horas_trabalho_semana = case_when(
      trabalho_sim == 1L ~ as.numeric(horas_trabalho),
      trabalho_sim == 0L ~ 0,
      TRUE ~ NA_real_
    ),
    .work_nature_text = as.character(
      natureza_trabalho
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
    
    idade = as.numeric(idade),
    ano_ingresso = as.numeric(ano_ingresso),
    anos_no_curso = ano_analise - ano_ingresso,
    horas_universidade_semana = as.numeric(
      horas_universidade
    ),
    
    .level_text = as.character(nivel),
    .level_grad_ref = dummy_exato(.level_text, "Graduação"),
    .level_post = dummy_exato(.level_text, "Pós-graduação"),
    nivel_especializacao = case_when(
      is.na(.level_post) ~ NA_integer_,
      .level_post == 0L ~ 0L,
      TRUE ~ as.integer(
        !is.na(curso_especializacao) &
          str_squish(curso_especializacao) != ""
      )
    ),
    nivel_mestrado = case_when(
      is.na(.level_post) ~ NA_integer_,
      .level_post == 0L ~ 0L,
      TRUE ~ as.integer(
        !is.na(curso_mestrado) &
          str_squish(curso_mestrado) != ""
      )
    ),
    nivel_doutorado = case_when(
      is.na(.level_post) ~ NA_integer_,
      .level_post == 0L ~ 0L,
      TRUE ~ as.integer(
        !is.na(curso_doutorado) &
          str_squish(curso_doutorado) != ""
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
    
    .access_general_text = as.character(acesso_geral),
    .access_list_text = as.character(acesso_lista),
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

# ALINHAMENTO E UNIÃO ----

base_javeriana_harmonizada <- alinhar_schema(
  base_javeriana_harmonizada,
  schema_tipos
)

base_ufmg_harmonizada <- alinhar_schema(
  base_ufmg_harmonizada,
  schema_tipos
)

comparacao_tipos <- comparar_tipos(
  base_javeriana_harmonizada,
  base_ufmg_harmonizada
)

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

# SALVAR BASE ----
saveRDS(base_unificada,"data/base_unificada.rds")
