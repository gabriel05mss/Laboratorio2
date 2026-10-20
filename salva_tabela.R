library(kableExtra)
library(webshot2)

tab <- bind_rows(
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


kableExtra::save_kable(
  tab,
  "diag_model.png")

library(magick)


image_read("diag_model.png") |>
  image_background("white", flatten = TRUE) |>
  image_write("diag_model.png")
# magick::image_read_pdf("tabela_genero.pdf", density = 300) |>
#   magick::image_write("tabela_genero.png")




