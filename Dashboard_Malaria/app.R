library(shiny)

# ============================================================
# PAINEL EPIDEMIOLÓGICO DA MALÁRIA — EIRUNEPÉ/AM
# Base visual: main_graficos_v6_LAMINAS_POSITIVAS_ATIVA_PASSIVA.R
# ============================================================

# Pasta pública do Shiny onde os PNGs serão servidos.
pasta_graficos <- file.path("www", "graficos")

# Pasta onde o v6 realmente grava os gráficos.
pasta_graficos_v6 <- file.path("..", "Resultados", "Graficos")

# Cria a pasta pública se ela ainda não existir.
if (!dir.exists(pasta_graficos)) {
  dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
}

# Copia automaticamente os gráficos produzidos pelo v6 para o www.
# Assim, não é necessário copiar os 37 PNGs manualmente.
if (dir.exists(pasta_graficos_v6)) {

  arquivos_v6 <- list.files(
    pasta_graficos_v6,
    pattern = "\\.png$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(arquivos_v6) > 0) {

    file.copy(
      from = arquivos_v6,
      to = file.path(
        pasta_graficos,
        basename(arquivos_v6)
      ),
      overwrite = TRUE,
      copy.date = TRUE
    )

  } else {

    warning(
      "A pasta Resultados/Graficos existe, mas nenhum arquivo PNG foi encontrado."
    )
  }

} else {

  warning(
    paste0(
      "A pasta dos gráficos do v6 não foi encontrada: ",
      normalizePath(
        pasta_graficos_v6,
        winslash = "/",
        mustWork = FALSE
      )
    )
  )
}

graficos <- list(
  list(id="01", arquivo="01_LPI_mensal.png", titulo="Casos positivos de malária por mês – LPI"),
list(id="01b", arquivo="01_LPI_mensal_2_anos.png", titulo="Casos positivos de malária por mês – LPI — 2 anos"),
list(id="02", arquivo="02_serie_historica_casos.png", titulo="Série histórica do total de casos de malária"),
list(id="03", arquivo="03_falciparum_mensal.png", titulo="P. falciparum – comparação mensal"),
list(id="05", arquivo="05_oportunidade_tratamento.png", titulo="Intervalo entre início dos sintomas e tratamento"),
list(id="06", arquivo="06_oportunidade_historica.png", titulo="Série histórica da oportunidade de tratamento"),
list(id="07", arquivo="07_LVC_mensal.png", titulo="Quantidade de exames de LVC realizados, mês a mês"),
list(id="08", arquivo="08_gestantes_historico.png", titulo="Quantidade de exames de malária realizados em gestantes"),
list(id="09", arquivo="09_gestantes_especie_pie.png", titulo="Proporção da espécie de malária em gestantes positivas"),
list(id="10", arquivo="10_gestantes_unidade.png", titulo="Exames para malária em gestantes por unidade notificante"),
list(id="11", arquivo="11_tafenoquina.png", titulo="P. vivax/mistas, uso de tafenoquina e proporção tratada"),
list(id="12", arquivo="12_recaida.png", titulo="Casos de P. vivax/mistas e recaídas"),
list(id="13", arquivo="13_top10_localidades.png", titulo="As 10 localidades com mais casos"),
list(id="14", arquivo="14_faixa_etaria_historica.png", titulo="Série histórica da proporção por faixa etária"),
list(id="15", arquivo="15_exames_positividade_historica.png", titulo="Série histórica da quantidade de exames e positivos"),
list(id="16", arquivo="16_falciparum_vivax.png", titulo="Distribuição mensal de P. falciparum x P. vivax/mistas"),
list(id="16b", arquivo="16_falciparum_vivax_linhas.png", titulo="P. falciparum x P. vivax/mistas – série mensal"),
list(id="17", arquivo="17_areas_especiais_doughnut.png", titulo="Proporção dos casos de malária por tipo de área"),
list(id="18", arquivo="18_autoctone_importado.png", titulo="Casos importados por mês"),
list(id="19", arquivo="19_sexo.png", titulo="Perfil dos casos por sexo"),
list(id="20", arquivo="20_raca_cor.png", titulo="Perfil dos casos por raça/cor"),
list(id="21", arquivo="21_escolaridade.png", titulo="Perfil dos casos por escolaridade"),
list(id="22", arquivo="22_ocupacao.png", titulo="Principais ocupações entre os casos"),
list(id="23", arquivo="23_semana_epidemiologica.png", titulo="Distribuição dos casos por semana epidemiológica"),
list(id="24", arquivo="24_ranking_nacional_top10.png", titulo="Ranking nacional – Top 10 municípios por casos"),
list(id="25", arquivo="25_ranking_nacional_top20.png", titulo="Ranking nacional – Top 20 municípios por casos"),
list(id="26", arquivo="26_ranking_nacional_top50.png", titulo="Ranking nacional – Top 50 municípios por casos"),
list(id="27", arquivo="27_tempo_sintomas_tratamento_localidade.png", titulo="Oportunidade de tratamento por localidade"),
list(id="28", arquivo="28_especies_historicas.png", titulo="Proporção das espécies de Plasmodium por ano"),
list(id="29", arquivo="29_tendencia_temporal.png", titulo="Tendência temporal dos casos"),
list(id="30", arquivo="30_diagrama_controle_malaria.png", titulo="Diagrama de controle mensal da malária"),
list(id="31", arquivo="31_diagrama_controle_falciparum.png", titulo="Diagrama de controle mensal – P. falciparum"),
list(id="32", arquivo="32_diagrama_controle_vivax.png", titulo="Diagrama de controle mensal – P. vivax"),
list(id="33", arquivo="33_casos_positivos_por_zona.png", titulo="Casos positivos por zona da localidade de infecção"),
list(id="34", arquivo="34_casos_zona_mensal.png", titulo="Casos positivos por zona – comparação mensal"),
list(id="35", arquivo="35_casos_importados_mensal.png", titulo="Casos importados de outros municípios – comparação mensal"),
list(id="36", arquivo="36_laminas_positivas_busca_ativa_passiva.png", titulo="Proporção de lâminas positivas por busca – 2 anos")
)

mostrar_grafico <- function(g) {
  caminho <- file.path(pasta_graficos, g$arquivo)
  if (file.exists(caminho)) {
    div(
      class = "grafico-card",
      h3(g$titulo),
      img(
        src = file.path("graficos", g$arquivo),
        class = "grafico-img"
      )
    )
  } else {
    div(
      class = "grafico-card",
      h3(g$titulo),
      div(
        class = "alert alert-warning",
        paste0("Arquivo não encontrado: ", g$arquivo)
      )
    )
  }
}

ui <- fluidPage(

  tags$head(
    tags$title("Painel Epidemiológico da Malária — Eirunepé/AM"),
    tags$style(HTML("
      body {
        background-color: #f4f6f8;
        font-family: Arial, sans-serif;
      }
      .cabecalho {
        background: white;
        padding: 25px 30px;
        margin: 15px 0 20px 0;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .cabecalho h1 { margin-top: 0; font-weight: 700; }
      .grafico-card {
        background: white;
        padding: 20px;
        margin-bottom: 25px;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .grafico-card h3 {
        margin-top: 0;
        margin-bottom: 20px;
        font-weight: 600;
      }
      .grafico-img {
        width: 100%;
        height: auto;
        display: block;
        margin: auto;
      }
    "))
  ),

  div(
    class = "cabecalho",
    h1("Painel Epidemiológico da Malária"),
    h3("Eirunepé — Amazonas"),
    p("Painel baseado nos gráficos produzidos pela versão v6 da análise automatizada do SIVEP-Malária.")
  ),

  tabsetPanel(
    
    tabPanel(
      "Casos e diagnóstico",
      br(),
      mostrar_grafico(graficos[[1]]),
      mostrar_grafico(graficos[[2]]),
      mostrar_grafico(graficos[[3]]),
      mostrar_grafico(graficos[[4]]),
      mostrar_grafico(graficos[[5]]),
      mostrar_grafico(graficos[[6]]),
      mostrar_grafico(graficos[[7]]),
      mostrar_grafico(graficos[[15]])
    ),

    tabPanel(
      "Gestantes, LVC e tratamento",
      br(),
      mostrar_grafico(graficos[[8]]),
      mostrar_grafico(graficos[[9]]),
      mostrar_grafico(graficos[[10]]),
      mostrar_grafico(graficos[[11]]),
      mostrar_grafico(graficos[[12]])
    ),

    tabPanel(
      "Perfil epidemiológico",
      br(),
      mostrar_grafico(graficos[[14]]),
      mostrar_grafico(graficos[[19]]),
      mostrar_grafico(graficos[[20]]),
      mostrar_grafico(graficos[[21]]),
      mostrar_grafico(graficos[[22]]),
      mostrar_grafico(graficos[[23]])
    ),

    tabPanel(
      "Localidades e rankings",
      br(),
      mostrar_grafico(graficos[[13]]),
      mostrar_grafico(graficos[[24]]),
      mostrar_grafico(graficos[[25]]),
      mostrar_grafico(graficos[[26]]),
      mostrar_grafico(graficos[[27]])
    ),

    tabPanel(
      "Espécies e origem",
      br(),
      mostrar_grafico(graficos[[16]]),
      mostrar_grafico(graficos[[17]]),
      mostrar_grafico(graficos[[18]]),
      mostrar_grafico(graficos[[28]]),
      mostrar_grafico(graficos[[29]])
    ),

    tabPanel(
      "Controle epidemiológico",
      br(),
      mostrar_grafico(graficos[[30]]),
      mostrar_grafico(graficos[[31]]),
      mostrar_grafico(graficos[[32]]),
      mostrar_grafico(graficos[[33]]),
      mostrar_grafico(graficos[[34]]),
      mostrar_grafico(graficos[[35]]),
      mostrar_grafico(graficos[[36]])
    ),

    tabPanel(
      "Todos os gráficos",
      br(),
      lapply(graficos, mostrar_grafico)
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
