library(kableExtra)
library(webshot2)

tab <- resultado %>%
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

kableExtra::save_kable(
  tab,
  "tabela_wilcoxon.png"
)

library(magick)

image_read("tabela_wilcoxon.png") |>
  image_background("white", flatten = TRUE) |>
  image_write("tabela_wilcoxon.png")
              
# magick::image_read_pdf("tabela_genero.pdf", density = 300) |>
#   magick::image_write("tabela_genero.png")
