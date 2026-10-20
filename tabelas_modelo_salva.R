library(xml2)
library(webshot2)
library(magick)

# ============================================================
# 1. GERAR A TABELA ORIGINAL
# ============================================================

# Utiliza a função existente, sem modificá-la
tab <- tabela_modelo(
  modelo_ordinal3,
  "Modelo 3 — Regressão ordinal com práticas de lazer em escala numérica (OR por ponto a mais na escala)"
)

# Ler o HTML produzido pela tabela
doc <- read_html(as.character(tab))

tabela <- xml_find_first(doc, ".//table")
caption <- xml_find_first(tabela, "./caption")
cabecalho <- xml_find_first(tabela, "./thead")
corpo <- xml_find_first(tabela, "./tbody")
rodape <- xml_find_first(tabela, "./tfoot")

# Guardar as linhas do corpo
linhas <- xml_find_all(corpo, "./tr")

# ============================================================
# 2. IDENTIFICAR AS VARIÁVEIS EXPLICATIVAS
# ============================================================

# Nomes dos termos da fórmula do modelo
variaveis <- attr(
  terms(modelo_ordinal3),
  "term.labels"
)

# Identificar as linhas que representam o início de cada variável.
# Somente serão consideradas as linhas cujo texto em negrito
# corresponde exatamente a um termo da fórmula.

eh_variavel <- vapply(
  linhas,
  function(linha) {
    
    span <- xml_find_first(
      linha,
      "./td[1]//span[contains(@style, 'font-weight: bold')]"
    )
    
    if (inherits(span, "xml_missing")) {
      return(FALSE)
    }
    
    nome <- trimws(xml_text(span))
    
    nome %in% variaveis
  },
  logical(1)
)

indices_variaveis <- which(eh_variavel)

if (length(indices_variaveis) == 0) {
  stop("Nenhuma variável explicativa foi identificada.")
}

# Conferir as variáveis encontradas
nomes_encontrados <- vapply(
  indices_variaveis,
  function(i) {
    span <- xml_find_first(
      linhas[[i]],
      "./td[1]//span[contains(@style, 'font-weight: bold')]"
    )
    
    trimws(xml_text(span))
  },
  character(1)
)

print(nomes_encontrados)

# Conferir se todas as variáveis foram encontradas
faltantes <- setdiff(variaveis, nomes_encontrados)

if (length(faltantes) > 0) {
  warning(
    "Variáveis não identificadas na tabela: ",
    paste(faltantes, collapse = ", ")
  )
}

# ============================================================
# 3. SEPARAR EM BLOCOS: PRIMEIRO COM 4, DEPOIS COM 5 VARIÁVEIS
# ============================================================

n_variaveis <- length(indices_variaveis)

# Definir os tamanhos dos blocos
tamanhos_blocos <- c(
  4,
  rep(5, ceiling((n_variaveis - 4) / 5))
)

# Remover tamanhos que ultrapassam o total de variáveis
tamanhos_blocos <- tamanhos_blocos[
  cumsum(tamanhos_blocos) - tamanhos_blocos < n_variaveis
]

# Criar os grupos de índices das variáveis
fins <- cumsum(tamanhos_blocos)
inicios <- c(1, head(fins, -1) + 1)

grupos <- Map(
  function(inicio, fim) {
    seq(inicio, min(fim, n_variaveis))
  },
  inicios,
  fins
)

# Para cada bloco, incluir todas as linhas das variáveis
# correspondentes, incluindo seus níveis categóricos.
blocos <- lapply(
  grupos,
  function(grupo) {
    
    primeira_linha <- indices_variaveis[min(grupo)]
    
    if (max(grupo) < length(indices_variaveis)) {
      
      proxima_variavel <- indices_variaveis[max(grupo) + 1]
      ultima_linha <- proxima_variavel - 1
      
    } else {
      
      ultima_linha <- length(linhas)
      
    }
    
    seq(primeira_linha, ultima_linha)
  }
)

# ============================================================
# 4. EXTRAIR TÍTULO, CABEÇALHO E NOTA DE RODAPÉ
# ============================================================

# O título será preservado exatamente como aparece na tabela
html_titulo <- as.character(caption)

# Cabeçalho das quatro colunas
html_cabecalho <- as.character(cabecalho)

# Nota de rodapé com as abreviações
html_rodape <- if (!inherits(rodape, "xml_missing")) {
  as.character(rodape)
} else {
  ""
}

# ============================================================
# 5. ESTILO DA TABELA
# ============================================================

css <- "
  html, body {
    margin: 0;
    padding: 0;
    background: white;
    color: black;
    font-family: Arial, Helvetica, sans-serif;
  }

  body {
    padding: 24px;
    box-sizing: border-box;
    width: 1200px;
  }

  table {
    width: 100%;
    border-collapse: collapse;
    table-layout: auto;
    font-size: 17px;
    margin: 0 auto;
    color: black;
  }

  caption {
    caption-side: top;
    font-size: 21px !important;
    font-weight: bold;
    text-align: center;
    padding: 0 0 20px 0;
    line-height: 1.4;
  }

  thead {
    display: table-header-group;
  }

  th {
    padding: 10px 12px;
    border-bottom: 2px solid black;
    text-align: left;
    white-space: nowrap;
  }

  td {
    padding: 8px 12px;
    vertical-align: top;
  }

  tbody tr {
    border-bottom: 1px solid #dddddd;
  }

  tfoot td {
    padding-top: 16px;
    border-top: 1px solid black;
    font-size: 13px;
  }

  sup {
    font-size: 10px;
  }
"

# ============================================================
# 6. EXPORTAR CADA BLOCO PARA PNG
# ============================================================

pasta_saida <- "tabelas_modelo3"

dir.create(
  pasta_saida,
  showWarnings = FALSE,
  recursive = TRUE
)

arquivos_png <- character(length(blocos))

for (i in seq_along(blocos)) {
  
  # Linhas correspondentes às duas variáveis do bloco
  linhas_bloco <- linhas[blocos[[i]]]
  
  html_linhas <- paste(
    vapply(
      linhas_bloco,
      as.character,
      character(1)
    ),
    collapse = "\n"
  )
  
  # Montar uma tabela HTML independente.
  # O título, cabeçalho e rodapé são repetidos em cada bloco.
  
  html_bloco <- paste0(
    "<!DOCTYPE html>",
    "<html lang='pt-BR'>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<style>", css, "</style>",
    "</head>",
    "<body>",
    
    "<table>",
    
    html_titulo,
    
    html_cabecalho,
    
    "<tbody>",
    html_linhas,
    "</tbody>",
    
    html_rodape,
    
    "</table>",
    
    "</body>",
    "</html>"
  )
  
  # Salvar o HTML temporário
  arquivo_html <- file.path(
    pasta_saida,
    sprintf("modelo3_bloco_%02d.html", i)
  )
  
  writeLines(
    html_bloco,
    arquivo_html,
    useBytes = TRUE
  )
  
  # Nome do PNG
  arquivo_png <- file.path(
    pasta_saida,
    sprintf("modelo3_bloco_%02d.png", i)
  )
  
  # Capturar o conteúdo completo da página
  webshot2::webshot(
    url = normalizePath(
      arquivo_html,
      winslash = "/"
    ),
    file = arquivo_png,
    selector = "body",
    vwidth = 1250,
    vheight = 1800,
    zoom = 1.5,
    expand = 10
  )
  
  # Garantir fundo branco
  imagem <- magick::image_read(arquivo_png)
  
  imagem <- magick::image_background(
    imagem,
    "white",
    flatten = TRUE
  )
  
  # Cortar margens excedentes sem remover conteúdo
  imagem <- magick::image_trim(imagem)
  
  magick::image_write(
    imagem,
    path = arquivo_png,
    format = "png"
  )
  
  arquivos_png[i] <- arquivo_png
  
  message(
    "Bloco ", i, " exportado: ", arquivo_png
  )
}

# ============================================================
# 7. LISTAR OS ARQUIVOS GERADOS
# ============================================================

print(arquivos_png)