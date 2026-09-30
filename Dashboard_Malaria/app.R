library(shiny)

# ============================================================
# PAINEL EPIDEMIOLÓGICO DA MALÁRIA — EIRUNEPÉ/AM
# ============================================================

pasta_graficos <- file.path("www", "graficos")
pasta_dados <- file.path("dados", "dados_dashboard.rds")

if (!file.exists(pasta_dados)) {
  stop("O arquivo dados/dados_dashboard.rds não foi encontrado no pacote publicado.")
}
dados_dashboard <- readRDS(pasta_dados)

if (!dir.exists(pasta_graficos)) {
  stop("A pasta www/graficos não foi encontrada no pacote publicado.")
}

# ------------------------------------------------------------
# Catálogo público de gráficos
# ------------------------------------------------------------
graficos <- list(
  list(id="01", arquivo="01_LPI_mensal.png", titulo="Casos positivos de malária por mês — LPI"),
  list(id="01b", arquivo="01_LPI_mensal_2_anos.png", titulo="Casos positivos de malária por mês — comparação de 2 anos"),
  list(id="02", arquivo="02_serie_historica_casos.png", titulo="Série histórica do total de casos de malária"),
  list(id="03", arquivo="03_falciparum_mensal.png", titulo="P. falciparum — comparação mensal"),
  list(id="04b", arquivo="04b_uso_testes_rapidos_historico.png", titulo="Uso de Testes Rápidos — série histórica"),
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
  list(id="16b", arquivo="16_falciparum_vivax_linhas.png", titulo="P. falciparum x P. vivax/mistas — série mensal"),
  list(id="18", arquivo="18_autoctone_importado.png", titulo="Casos importados por mês"),
  list(id="19", arquivo="19_sexo.png", titulo="Perfil dos casos por sexo"),
  list(id="20", arquivo="20_raca_cor.png", titulo="Perfil dos casos por raça/cor"),
  list(id="21", arquivo="21_escolaridade.png", titulo="Perfil dos casos por escolaridade"),
  list(id="22", arquivo="22_ocupacao.png", titulo="Principais ocupações entre os casos"),
  list(id="23", arquivo="23_semana_epidemiologica.png", titulo="Distribuição dos casos por semana epidemiológica"),
  list(id="24", arquivo="24_ranking_nacional_top10.png", titulo="Ranking nacional — Top 10 municípios por casos"),
  list(id="25", arquivo="25_ranking_nacional_top20.png", titulo="Ranking nacional — Top 20 municípios por casos"),
  list(id="26", arquivo="26_ranking_nacional_top50.png", titulo="Ranking nacional — Top 50 municípios por casos"),
  list(id="27", arquivo="27_tempo_sintomas_tratamento_localidade.png", titulo="Oportunidade de tratamento por localidade"),
  list(id="28", arquivo="28_especies_historicas.png", titulo="Proporção das espécies de Plasmodium por ano"),
  list(id="30", arquivo="30_diagrama_controle_malaria.png", titulo="Diagrama de controle mensal da malária"),
  list(id="31", arquivo="31_diagrama_controle_falciparum.png", titulo="Diagrama de controle mensal — P. falciparum"),
  list(id="32", arquivo="32_diagrama_controle_vivax.png", titulo="Diagrama de controle mensal — P. vivax"),
  list(id="33", arquivo="33_casos_positivos_por_zona.png", titulo="Casos positivos por zona da localidade de infecção"),
  list(id="34", arquivo="34_casos_zona_mensal.png", titulo="Casos positivos por zona — comparação mensal"),
  list(id="35", arquivo="35_casos_importados_mensal.png", titulo="Casos importados de outros municípios — comparação mensal"),
  list(id="36", arquivo="36_laminas_positivas_busca_ativa_passiva_v2.png", titulo="Proporção de lâminas positivas por busca — 2 anos"),
  list(id="37", arquivo="37_municipios_importadores_especie_mensal.png", titulo="Municípios importadores por mês e espécie"),
  list(id="38", arquivo="38_positividade_falciparum_area_especial.png", titulo="Casos positivos de P. falciparum por área especial")
)

grafico_por_id <- function(id) {
  g <- graficos[vapply(graficos, function(x) identical(x$id, id), logical(1))]
  if (length(g)) g[[1]] else NULL
}

mostrar_grafico <- function(g) {
  if (is.null(g)) return(NULL)
  caminho <- file.path(pasta_graficos, g$arquivo)

  if (file.exists(caminho)) {
    div(
      class = "grafico-card",
      h3(g$titulo),
      img(
        src = paste0(
          file.path("graficos", g$arquivo),
          "?v=", as.numeric(file.info(caminho)$mtime)
        ),
        class = "grafico-img"
      )
    )
  } else {
    div(
      class = "grafico-card",
      h3(g$titulo),
      div(class = "alerta-arquivo",
          paste0("Arquivo ainda não disponível: ", g$arquivo))
    )
  }
}

# ------------------------------------------------------------
# Funções auxiliares para os cards e tabela
# ------------------------------------------------------------
valor_seguro <- function(x, padrao = 0) {
  if (length(x) == 0 || is.null(x) || is.na(x[1])) padrao else x[1]
}

formatar_numero <- function(x) {
  format(round(valor_seguro(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

formatar_pct <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x[1])) return("n/c")
  paste0(ifelse(x[1] >= 0, "+", ""), sprintf("%.1f", x[1]), "%")
}

resumo_atual <- dados_dashboard$resumo_ano
ano_atual <- valor_seguro(dados_dashboard$ano_recente, max(resumo_atual$ANO, na.rm = TRUE))
mes_atual <- valor_seguro(dados_dashboard$mes_max_ano_recente, 12)

linha_atual <- resumo_atual[resumo_atual$ANO == ano_atual, , drop = FALSE]
casos_atual <- valor_seguro(linha_atual$CASOS)
pf_atual <- valor_seguro(linha_atual$FALCIPARUM)
pv_atual <- valor_seguro(linha_atual$VIVAX)

importados_atual <- 0

if (!is.null(dados_dashboard$importados_mensal_comp)) {
  imp <- dados_dashboard$importados_mensal_comp

  importados_atual <- sum(
    imp$ANO_RECENTE,
    na.rm = TRUE
  )
}

meses_rotulo <- c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")

# ------------------------------------------------------------
# Interface
# ------------------------------------------------------------
ui <- fluidPage(

  tags$head(
    tags$title("Painel Epidemiológico da Malária — Eirunepé/AM"),

    tags$style(HTML("
      * { box-sizing: border-box; }

      body {
        margin: 0;
        background: #f3f6f9;
        color: #172033;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;
      }

      .container-fluid {
        max-width: 1500px;
        margin: 0 auto;
        padding: 0 22px 40px 22px;
      }

      .hero {
        position: relative;
        overflow: hidden;
        background: linear-gradient(135deg, #123b5d 0%, #176b87 55%, #1c8a7a 100%);
        color: white;
        padding: 34px 38px;
        margin: 18px 0 18px 0;
        border-radius: 18px;
        box-shadow: 0 10px 30px rgba(18,59,93,0.18);
      }

      .hero:after {
        content: '';
        position: absolute;
        width: 280px;
        height: 280px;
        right: -90px;
        top: -120px;
        border-radius: 50%;
        background: rgba(255,255,255,0.08);
      }

      .hero h1 {
        margin: 0 0 5px 0;
        font-size: 32px;
        font-weight: 750;
        letter-spacing: -0.5px;
      }

      .hero h3 {
        margin: 0 0 12px 0;
        font-size: 19px;
        font-weight: 500;
        opacity: 0.92;
      }

      .hero p {
        margin: 0;
        max-width: 850px;
        font-size: 14px;
        opacity: 0.86;
      }

      .periodo-badge {
        display: inline-block;
        margin-top: 18px;
        padding: 7px 13px;
        border-radius: 999px;
        background: rgba(255,255,255,0.14);
        border: 1px solid rgba(255,255,255,0.22);
        font-size: 13px;
      }

      .cards {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 15px;
        margin: 0 0 18px 0;
      }

      .metric-card {
        background: white;
        border-radius: 14px;
        padding: 18px 20px;
        box-shadow: 0 4px 16px rgba(15,23,42,0.07);
        border: 1px solid #e7edf3;
      }

      .metric-label {
        color: #667085;
        font-size: 12px;
        font-weight: 650;
        text-transform: uppercase;
        letter-spacing: .6px;
      }

      .metric-value {
        margin-top: 6px;
        font-size: 28px;
        font-weight: 750;
        color: #123b5d;
      }

      .metric-sub {
        margin-top: 3px;
        color: #7a8699;
        font-size: 12px;
      }

      .comparacao-card {
        background: white;
        border-radius: 16px;
        padding: 24px 26px;
        margin-bottom: 20px;
        box-shadow: 0 5px 20px rgba(15,23,42,0.07);
        border: 1px solid #e7edf3;
      }

      .comparacao-card h2 {
        margin: 0 0 5px 0;
        color: #123b5d;
        font-size: 21px;
        font-weight: 750;
      }

      .comparacao-card .subtitulo {
        color: #667085;
        font-size: 13px;
        margin-bottom: 15px;
      }

      .tabela-comparacao {
        width: 100%;
        border-collapse: separate;
        border-spacing: 0;
        overflow: hidden;
        border: 1px solid #dfe6ed;
        border-radius: 10px;
        font-size: 15px;
      }

      .tabela-comparacao th {
        background: #dfe7ef;
        color: #142033;
        padding: 13px 15px;
        text-align: center;
        font-weight: 750;
      }

      .tabela-comparacao th:first-child { text-align: left; }

      .tabela-comparacao td {
        padding: 13px 15px;
        border-top: 1px solid #e8edf2;
        text-align: center;
        font-size: 16px;
      }

      .tabela-comparacao td:first-child {
        text-align: left;
        font-weight: 650;
      }

      .variacao-negativa {
        color: #087443;
        background: #e7f7ee;
        font-weight: 750;
      }

      .variacao-positiva {
        color: #a32222;
        background: #fdeaea;
        font-weight: 750;
      }

      .variacao-neutra {
        color: #475467;
        background: #f3f4f6;
        font-weight: 750;
      }

      .nota-tabela {
        margin: 12px 0 0 0;
        color: #667085;
        font-size: 12px;
      }

      .grafico-card {
        background: white;
        padding: 22px 24px 24px 24px;
        margin-bottom: 22px;
        border-radius: 16px;
        box-shadow: 0 5px 20px rgba(15,23,42,0.06);
        border: 1px solid #e7edf3;
      }

      .grafico-card h3 {
        margin: 0 0 17px 0;
        color: #172033;
        font-size: 19px;
        font-weight: 700;
      }

      .grafico-img {
        width: 100%;
        height: auto;
        display: block;
        margin: auto;
        border-radius: 8px;
      }

      .alerta-arquivo {
        padding: 14px;
        border-radius: 9px;
        background: #fff7e6;
        border: 1px solid #f2d28b;
        color: #805b10;
      }

      .nav-tabs {
        border-bottom: 1px solid #d9e1e8;
        margin-bottom: 18px;
      }

      .nav-tabs > li > a {
        color: #31536d;
        font-weight: 650;
        border: 0 !important;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: #123b5d;
        background: white;
        border-bottom: 3px solid #1c8a7a !important;
      }

      .footer {
        margin-top: 28px;
        padding: 18px 5px;
        text-align: center;
        color: #8490a0;
        font-size: 12px;
      }

      @media (max-width: 900px) {
        .cards { grid-template-columns: repeat(2, 1fr); }
      }

      @media (max-width: 600px) {
        .container-fluid { padding: 0 10px 25px 10px; }
        .hero { padding: 25px 22px; }
        .hero h1 { font-size: 25px; }
        .cards { grid-template-columns: 1fr 1fr; gap: 9px; }
        .metric-card { padding: 14px; }
        .metric-value { font-size: 23px; }
        .comparacao-card { padding: 17px 13px; }
      }
    "))
  ),

  div(
    class = "hero",
    h1("Painel Epidemiológico da Malária"),
    h3("Eirunepé — Amazonas"),
    p("Monitoramento epidemiológico municipal com indicadores derivados do SIVEP-Malária e atualização automatizada."),
    span(
      class = "periodo-badge",
      paste0("Dados processados até ", meses_rotulo[mes_atual], "/", ano_atual)
    )
  ),

  div(
    class = "cards",
    div(class = "metric-card",
        div(class = "metric-label", "Casos positivos"),
        div(class = "metric-value", formatar_numero(casos_atual)),
        div(class = "metric-sub", paste0("até ", meses_rotulo[mes_atual], "/", ano_atual))),
    div(class = "metric-card",
        div(class = "metric-label", "P. falciparum"),
        div(class = "metric-value", formatar_numero(pf_atual)),
        div(class = "metric-sub", "casos positivos")),
    div(class = "metric-card",
        div(class = "metric-label", "P. vivax"),
        div(class = "metric-value", formatar_numero(pv_atual)),
        div(class = "metric-sub", "casos positivos")),
    div(class = "metric-card",
        div(class = "metric-label", "Importados"),
        div(class = "metric-value", formatar_numero(importados_atual)),
        div(class = "metric-sub", "provável infecção fora do município"))
  ),

  div(
    class = "comparacao-card",
    h2("Comparação do período epidemiológico"),
    div(
      class = "subtitulo",
      paste0(
        "Casos positivos: janeiro a ", meses_rotulo[mes_atual], " de ",
        ano_atual, " comparados com o mesmo período de ", ano_atual - 1, "."
      )
    ),
    uiOutput("tabela_comparacao"),
    p(
      class = "nota-tabela",
      "P. falciparum e P. vivax incluem infecções mistas nas respectivas linhas; o Total conta casos positivos únicos. A coluna percentual mostra a diferença na quantidade em relação ao ano anterior."
    )
  ),

  tabsetPanel(
    tabPanel(
      "Casos e diagnóstico",
      br(),
      mostrar_grafico(grafico_por_id("01")),
      mostrar_grafico(grafico_por_id("01b")),
      mostrar_grafico(grafico_por_id("02")),
      mostrar_grafico(grafico_por_id("03")),
      mostrar_grafico(grafico_por_id("04b")),
      mostrar_grafico(grafico_por_id("15"))
    ),

    tabPanel(
      "Gestantes, LVC e tratamento",
      br(),
      mostrar_grafico(grafico_por_id("06")),
      mostrar_grafico(grafico_por_id("07")),
      mostrar_grafico(grafico_por_id("08")),
      mostrar_grafico(grafico_por_id("09")),
      mostrar_grafico(grafico_por_id("10")),
      mostrar_grafico(grafico_por_id("11")),
      mostrar_grafico(grafico_por_id("12"))
    ),

    tabPanel(
      "Perfil epidemiológico",
      br(),
      mostrar_grafico(grafico_por_id("14")),
      mostrar_grafico(grafico_por_id("19")),
      mostrar_grafico(grafico_por_id("20")),
      mostrar_grafico(grafico_por_id("21")),
      mostrar_grafico(grafico_por_id("22")),
      mostrar_grafico(grafico_por_id("23"))
    ),

    tabPanel(
      "Localidades e rankings",
      br(),
      mostrar_grafico(grafico_por_id("13")),
      mostrar_grafico(grafico_por_id("24")),
      mostrar_grafico(grafico_por_id("25")),
      mostrar_grafico(grafico_por_id("26")),
      mostrar_grafico(grafico_por_id("27"))
    ),

    tabPanel(
      "Espécies e origem",
      br(),
      mostrar_grafico(grafico_por_id("16")),
      mostrar_grafico(grafico_por_id("16b")),
      mostrar_grafico(grafico_por_id("18")),
      mostrar_grafico(grafico_por_id("28")),
      mostrar_grafico(grafico_por_id("35")),
      mostrar_grafico(grafico_por_id("37"))
    ),

    tabPanel(
      "Controle epidemiológico",
      br(),
      mostrar_grafico(grafico_por_id("30")),
      mostrar_grafico(grafico_por_id("31")),
      mostrar_grafico(grafico_por_id("32")),
      mostrar_grafico(grafico_por_id("33")),
      mostrar_grafico(grafico_por_id("34")),
      mostrar_grafico(grafico_por_id("36")),
      mostrar_grafico(grafico_por_id("38"))
    ),

    tabPanel(
      "Todos os gráficos",
      br(),
      lapply(graficos, mostrar_grafico)
    )
  ),

  div(
    class = "footer",
    paste0("Painel Epidemiológico da Malária • Eirunepé/AM • Atualização automática • ", ano_atual)
  )
)

server <- function(input, output, session) {

  output$tabela_comparacao <- renderUI({
    tab <- dados_dashboard$comparacao_especies_periodo

    if (is.null(tab) || !nrow(tab)) {
      return(div(class = "alerta-arquivo",
                 "Tabela comparativa não disponível nos dados publicados. Execute novamente o script de análise."))
    }

    ano_recente <- dados_dashboard$ano_recente
    ano_anterior <- ano_recente - 1

    celula_variacao <- function(v) {
      if (is.na(v)) {
        tags$td(class = "variacao-neutra", "n/c")
      } else if (v < 0) {
        tags$td(class = "variacao-negativa",
                paste0(sprintf("%.1f", v), "%"))
      } else if (v > 0) {
        tags$td(class = "variacao-positiva",
                paste0("+", sprintf("%.1f", v), "%"))
      } else {
        tags$td(class = "variacao-neutra", "0,0%")
      }
    }

    linhas <- lapply(seq_len(nrow(tab)), function(i) {
      tags$tr(
        tags$td(tab$INDICADOR[i]),
        tags$td(format(tab$ANO_RECENTE[i], big.mark = ".", scientific = FALSE)),
        tags$td(format(tab$ANO_ANTERIOR[i], big.mark = ".", scientific = FALSE)),
        celula_variacao(tab$VARIACAO_PCT[i])
      )
    })

    tags$table(
      class = "tabela-comparacao",
      tags$thead(
        tags$tr(
          tags$th(""),
          tags$th(as.character(ano_recente)),
          tags$th(as.character(ano_anterior)),
          tags$th(paste0("% ", ano_recente, "-", ano_anterior))
        )
      ),
      tags$tbody(linhas)
    )
  })
}

shinyApp(ui = ui, server = server)
