#' 対応分析（CA）アプリ — モジュール版統合
#'
#' df が指定されていればそれを使用、未指定なら DataSourceModule 経由で .rda/.RData/.rds をアップロードして選択。
#' 以降の本体処理は src$data() を唯一のデータソースとして参照します。
#'
#' ver 1.3 (module integration)
#' @import shiny
#' @import FactoMineR
#' @import ggplot2
#' @import vcd
#' @import showtext
#' @importFrom DT datatable
#' @export
Shiny_ca <- function(df = NULL) {
  showtext::showtext_auto(TRUE)
  has_df <- !is.null(df)

  ui <- shiny::fluidPage(
    shiny::titlePanel("対応分析アプリ（CA）"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # ▼ 共通データ入力モジュール（df 未指定時のみアップロード UI を表示）
        DataSourceUI("src", show_upload = !has_df, accept = c('.rda','.RData','.rds')),
        # 変数選択はデータがセットされてから動的生成
        shiny::uiOutput("row_var_ui"),
        shiny::uiOutput("col_var_ui"),
        shiny::actionButton("run_analysis", "分析を実行")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("クロス表", DT::DTOutput("cross_table")),
          shiny::tabPanel("カイ二乗検定", shiny::verbatimTextOutput("chi_result")),
          shiny::tabPanel("モザイクプロット", shiny::plotOutput("mosaic_plot", width = 600, height = 600)),
          shiny::tabPanel("CAマップ",
            shiny::plotOutput("ca_map_symmetric", width = 600, height = 600),
            shiny::plotOutput("ca_map_row",       width = 600, height = 600),
            shiny::plotOutput("ca_map_col",       width = 600, height = 600)
          ),
          shiny::tabPanel("データ確認", DT::DTOutput("original_data"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    showtext::showtext_auto(TRUE)

    # ▼ 共通データ入力モジュール
    src <- DataSourceServer("src", df = df, allow_replace = FALSE)

    # データがセットされた後に行/列の選択 UI を生成
    output$row_var_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("row_var", "行変数の選択", choices = names(src$data()))
    })
    output$col_var_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("col_var", "列変数の選択", choices = names(src$data()))
    })

    # 以降の処理は唯一のデータソースとして src$data() を参照
    data_for_plot <- shiny::reactive({ shiny::req(src$data()); src$data() })

    # クロス表（イベントで作成）
    cross_tab <- shiny::eventReactive(input$run_analysis, {
      shiny::req(input$row_var, input$col_var, data_for_plot())
      df_filtered <- data_for_plot()[!is.na(data_for_plot()[[input$row_var]]) & !is.na(data_for_plot()[[input$col_var]]), ]
      table(df_filtered[[input$row_var]], df_filtered[[input$col_var]])
    })

    # カイ二乗検定
    chi_result <- shiny::eventReactive(input$run_analysis, {
      stats::chisq.test(cross_tab())
    })

    # CA 実行
    ca_result <- shiny::eventReactive(input$run_analysis, {
      FactoMineR::CA(cross_tab(), graph = FALSE)
    })

    # 出力表示
    output$cross_table <- DT::renderDT({
      shiny::req(cross_tab())
      DT::datatable(as.data.frame.matrix(cross_tab()), options = list(pageLength = 10))
    })

    output$chi_result <- shiny::renderPrint({
      shiny::req(chi_result())
      chi_result()
    })

    # mosaic plot（セル比率の計算は参考に保持）
    output$mosaic_plot <- shiny::renderPlot({
      shiny::req(input$row_var, input$col_var, data_for_plot())
      .tbl   <- table(data_for_plot()[[input$row_var]], data_for_plot()[[input$col_var]])
      .tbl.p <- round(100 * prop.table(.tbl, margin = 1), 1)
      tab    <- ifelse(.tbl.p < 1, NA, .tbl.p)
      vcd::mosaic(vcd::structable(.tbl), shade = TRUE, las = 2, pop = FALSE)
      # 注: vcd::labeling_cells などでセル値の描画も可能
    })

    # CA マップ（FactoMineR の標準描画）
    output$ca_map_symmetric <- shiny::renderPlot({
      shiny::req(ca_result())
      FactoMineR::plot.CA(ca_result(), title = "CA 対称マップ")
    })
    output$ca_map_row <- shiny::renderPlot({
      shiny::req(ca_result())
      plot(ca_result(), invisible = "col", title = "行カテゴリのCAマップ")
    })
    output$ca_map_col <- shiny::renderPlot({
      shiny::req(ca_result())
      plot(ca_result(), invisible = "row", title = "列カテゴリのCAマップ")
    })

    output$original_data <- DT::renderDT({
      shiny::req(data_for_plot())
      DT::datatable(data_for_plot(), options = list(pageLength = 5))
    })
  }

  shiny::shinyApp(ui, server)
}
