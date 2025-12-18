#' ENQview_lite（モジュール版統合）— df 指定/アップロード入口を共通モジュールに置換
#'
#' @import shiny
#' @import showtext
#' @import DT
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import tibble
#' @import gtsummary
#' @import gt
#' @import vcd
#' @import GGally
#' @import FactoMineR
#' @import rlang
#' @import scales
#' @export
ENQview_lite <- function(df = NULL) {
  pkgs <- c("shiny","showtext","DT","ggplot2","dplyr","tidyr","purrr","tibble","gtsummary","gt","vcd","GGally","FactoMineR","rlang","scales")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0) stop(sprintf("必要パッケージが見つかりません: %s", paste(miss, collapse=", ")))

  showtext::showtext_auto(TRUE)
  has_df <- !is.null(df)

  ui <- shiny::fluidPage(
    shiny::titlePanel("ENQview_lite RDAファイルからデータフレーム選択版"),
    shiny::tabPanel(
      "基本集計",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # ▼ 共通モジュールUI
          DataSourceUI("src", show_upload = !has_df, accept = c('.rda','.RData','.rds')),
          shiny::uiOutput("variables_ui"),
          shiny::tags$hr(),
          shiny::uiOutput("cross_var_ui"),
          shiny::uiOutput("layer_var_ui"),
          shiny::uiOutput("variables2_ui"),
          shiny::uiOutput("hist_var_ui")
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(type = "tabs",
            shiny::tabPanel("単変数集計",
              shiny::h2("棒グラフと度数分布"),
              shiny::plotOutput("barchart"),
              DT::dataTableOutput("simple_table")
            ),
            shiny::tabPanel("2変数分析",
              shiny::h2("クロス集計（gtsummary::tbl_cross )"),
              gt::gt_output(outputId = "my_gt_table2"),
              shiny::plotOutput("crosschart", width = 600, height = 600),
              shiny::h3("χ2乗検定"),
              shiny::verbatimTextOutput("chisq_test2")
            ),
            shiny::tabPanel("pairs",
              shiny::h2("GGally::pairs"),
              shiny::plotOutput("pairs", width = 600, height = 600)
            ),
            shiny::tabPanel("pairs_multi",
              shiny::h2("GGally::pairs 多変数"),
              shiny::plotOutput("pairs_multi", width = 900, height = 900)
            ),
            shiny::tabPanel("2変数分析（層化）",
              shiny::h2("クロス集計（gtsummary::tbl_cross )"),
              gt::gt_output(outputId = "my_gt_table"),
              shiny::plotOutput("crosschart2", width = 900, height = 600),
              shiny::h3("χ2乗検定"),
              shiny::verbatimTextOutput("chisq_test")
            ),
            shiny::tabPanel("MA plot(Bar)", shiny::plotOutput("MAplot", width = 600, height = 600)),
            shiny::tabPanel("MA plot(Dot)", shiny::plotOutput("MAplot_Dot", width = 600, height = 600)),
            shiny::tabPanel("層化 MA plot",
              shiny::plotOutput("MAplot_lineDot", width = 600, height = 400),
              shiny::plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
            ),
            shiny::tabPanel("層化 MA plot2",
              shiny::plotOutput("MAplot_lineDot2", width = 600, height = 400),
              shiny::plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
            ),
            shiny::tabPanel("Grid回答 General mosaic表示",
              shiny::plotOutput("GridAnswerG_mosaic", width = 600, height = 600),
              shiny::plotOutput("GridAnswerG_CA", width = 700, height = 700)
            ),
            shiny::tabPanel("単変数check",
              shiny::plotOutput("barchart2"),
              DT::dataTableOutput("simple_table2")
            ),
            shiny::tabPanel("選択変数のデータ一覧", DT::dataTableOutput("table_for_plot"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    showtext::showtext_auto(TRUE)

    # ▼ 共通データ入力モジュール
    src <- DataSourceServer("src", df = df, allow_replace = FALSE)

    # data が来たら UI を更新
    shiny::observeEvent(src$data(), {
      shiny::req(src$data())
      data <- src$data()
      shiny::updateSelectInput(session, "select_input_data_for_hist", choices = colnames(data))
      shiny::updateSelectInput(session, "select_input_data_for_cross", choices = c(" ", colnames(data)))
      shiny::updateSelectInput(session, "select_input_data_for_layer", choices = c(" ", colnames(data)))
      shiny::updateSelectInput(session, "variables", choices = colnames(data), selected = colnames(data)[3:4])
    })

    data_for_plot <- shiny::reactive({ shiny::req(src$data()); src$data() })

    # --- 動的入力UI ---
    output$hist_var_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("select_input_data_for_hist", "確認したい単変数", choices = names(src$data()))
    })
    output$cross_var_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("select_input_data_for_cross", "クロス集計変数を選択", choices = c(" ", names(src$data())))
    })
    output$layer_var_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("select_input_data_for_layer", "層変数を選択", choices = c(" ", names(src$data())))
    })
    output$variables_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("variables", "変数を選択", choices = names(src$data()), selected = names(src$data())[1], multiple = TRUE, selectize = FALSE, size = 7)
    })

    # --- 以下、元の出力群（src$data() に置換） ---
    output$table_preview <- shiny::renderTable({
      shiny::req(src$data(), input$variables)
      src$data()[, input$variables, drop = FALSE]
    })

    output$barchart <- shiny::renderPlot({
      data_for_plot() %>% dplyr::count(!!!rlang::syms(input$variables[1])) %>%
        dplyr::rename(V1=1) %>% dplyr::filter(V1 != "非該当") %>%
        dplyr::mutate(rate=100 * .data[["n"]]/sum(.data[["n"]])) %>%
        ggplot2::ggplot(ggplot2::aes(x=V1,y=rate)) + ggplot2::geom_col(ggplot2::aes(fill=V1)) + ggplot2::ggtitle(input$variables[1])
    })

    output$barchart2 <- shiny::renderPlot({
      data_for_plot() %>% dplyr::count(!!!rlang::syms(input$select_input_data_for_hist)) %>% dplyr::rename(V1=1) %>% dplyr::filter(V1 != "非該当") %>%
        dplyr::mutate(rate=100 * .data[["n"]]/sum(.data[["n"]])) %>%
        ggplot2::ggplot(ggplot2::aes(x=V1,y=rate)) + ggplot2::geom_col(ggplot2::aes(fill=V1)) + ggplot2::ggtitle(input$select_input_data_for_hist)
    })

    output$pairs <- shiny::renderPlot({
      data_for_plot()[,c(input$variables[1],input$select_input_data_for_cross)] %>%
        GGally::ggpairs(mapping = ggplot2::aes(color = !!as.name(input$variables[1]))) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45,hjust = 1)) + ggplot2::ggtitle(input$variables[1]) -> p
      p
    })

    output$pairs_multi <- shiny::renderPlot({
      data_for_plot()[,input$variables] %>% GGally::ggpairs(mapping = ggplot2::aes(color = !!as.name(input$variables))) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45,hjust = 1)) + ggplot2::ggtitle(input$variables) -> p
      p
    })

    output$crosschart <- shiny::renderPlot({
      .tbl <- table(data_for_plot()[[input$select_input_data_for_cross]], data_for_plot()[[input$variables[1]]])
      .tbl.p <- round(100 * prop.table(.tbl, margin = 1),1); tab <- ifelse(.tbl.p < 1, NA, .tbl.p)
      data_for_plot()[,c(input$select_input_data_for_cross,input$variables[1])] %>% vcd::structable() %>%
        vcd::mosaic(shade=TRUE,las=2, labeling=labeling_values)
    })

    output$crosschart2 <- shiny::renderPlot({
      .tbl <- table(data_for_plot()[[input$select_input_data_for_cross]], data_for_plot()[[input$variables[1]]])
      .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1); tab <- ifelse(.tbl.p < 1, NA, .tbl.p)
      data_for_plot()[,c(input$variables[1], input$select_input_data_for_cross, input$select_input_data_for_layer)] %>% vcd::structable() %>%
        vcd::mosaic(condvars = 3, split_vertical = TRUE, shade=TRUE, las=2, labeling=labeling_values)
    })

    output$my_gt_table <- gt::render_gt({
      data_for_plot() %>% gtsummary::tbl_cross(col = input$select_input_data_for_cross, row = input$variables[1], percent = "row") %>%
        gtsummary::add_p(test="chisq.test") %>% gtsummary::bold_labels() %>% gtsummary::as_gt()
    })
    output$my_gt_table2 <- gt::render_gt({
      data_for_plot() %>% gtsummary::tbl_cross(row = input$select_input_data_for_cross, col = input$variables[1], percent = "row") %>%
        gtsummary::add_p(test="chisq.test") %>% gtsummary::bold_labels() %>% gtsummary::as_gt()
    })

    output$chisq_test <- shiny::renderPrint({
      res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]], data_for_plot()[[input$variables[1]]]),correct = FALSE)
      print(res.chisq)
    })
    output$chisq_test2 <- shiny::renderPrint({
      res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]], data_for_plot()[[input$variables[1]]]),correct = FALSE)
      print(res.chisq)
    })

    output$MA_gt_table <- gt::render_gt({
      data_for_plot() %>% gtsummary::tbl_cross(row = input$select_input_data_for_cross, col = input$variables[1], percent = "row") %>%
        gtsummary::add_p(test="chisq.test") %>% gtsummary::bold_labels() %>% gtsummary::as_gt()
    })

    output$MAplot <- shiny::renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
        selected_data %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(. == 1,na.rm =TRUE))) %>%
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Question", values_to = "Ratio") -> ratio_df
        ggplot2::ggplot(ratio_df, ggplot2::aes(x = Question, y = Ratio)) + ggplot2::geom_bar(stat = "identity", fill = "skyblue") +
          ggplot2::scale_y_continuous(labels = scales::percent_format()) + ggplot2::labs(title = selected_vars, x = "質問項目", y = "割合（% )") + ggplot2::theme_minimal()
      }
    })

    output$MAplot_Dot <- shiny::renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
        selected_data %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(. == 1,na.rm =TRUE))) %>%
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Question", values_to = "Ratio") -> ratio_df
        ratio_df %>% ggplot2::ggplot(ggplot2::aes(x=Ratio, y=reorder(Question,Ratio))) + ggplot2::geom_point(size=3) + ggplot2::theme_bw() +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_line(colour="grey60",linetype="dashed")) +
          ggplot2::labs(title = selected_vars, x = "割合（% )", y = "質問項目")
      }
    })

    output$MAplot_lineDot <- shiny::renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        gp_vari <- input$select_input_data_for_layer
        data_for_plot() %>% dplyr::group_by(!!!rlang::syms(gp_vari)) %>%
          dplyr::summarise(度数=dplyr::n(), dplyr::across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/dplyr::n(), .names="ratio_{col}")) -> MA_group_tbl
        MA_group_tbl %>% dplyr::select(-度数) %>% tidyr::pivot_longer(cols = tidyselect::starts_with("ratio_"), names_to = "variable", values_to = "value") -> df_long
        ggplot2::ggplot(df_long, ggplot2::aes(x = !!as.name(gp_vari), y = value, shape =variable, group = variable)) +
          ggplot2::geom_line(ggplot2::aes(color = variable)) + ggplot2::geom_point(ggplot2::aes(color = variable),size=4) + ggplot2::labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") + ggplot2::theme_minimal() + ggplot2::scale_color_discrete() + ggplot2::scale_shape_manual(values = 1:length(selected_vars))
      }
    })

    output$MAplot_lineDot2 <- shiny::renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        gp_vari <- input$select_input_data_for_layer
        data_for_plot() %>% dplyr::group_by(!!!rlang::syms(gp_vari)) %>%
          dplyr::summarise(度数=dplyr::n(), dplyr::across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/dplyr::n(), .names="ratio_{col}")) -> MA_group_tbl
        MA_group_tbl %>% dplyr::select(-度数) %>% tidyr::pivot_longer(cols = tidyselect::starts_with("ratio_"), names_to = "variable", values_to = "value") -> df_long
        ggplot2::ggplot(df_long, ggplot2::aes(x = !!as.name(gp_vari), y = value, shape =variable, group = variable)) +
          ggplot2::geom_line(ggplot2::aes(color = variable)) + ggplot2::geom_point(ggplot2::aes(color = variable),size=4) + ggplot2::labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") + ggplot2::theme_minimal() + ggplot2::scale_color_discrete() + ggplot2::scale_shape_manual(values = 1:length(selected_vars)) + ggplot2::theme(legend.position = 'none')
      }
    })

    output$MAplot_lineDotwarp <- shiny::renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        gp_vari <- input$select_input_data_for_layer
        data_for_plot() %>% dplyr::group_by(!!!rlang::syms(gp_vari)) %>%
          dplyr::summarise(度数=dplyr::n(), dplyr::across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/dplyr::n(), .names="ratio_{col}")) -> MA_group_tbl
        MA_group_tbl %>% dplyr::select(-度数) %>% tidyr::pivot_longer(cols = tidyselect::starts_with("ratio_"), names_to = "variable", values_to = "value") -> df_long
        ggplot2::ggplot(df_long, ggplot2::aes(x = !!as.name(gp_vari), y = value, color = variable, group = variable)) + ggplot2::geom_line() + ggplot2::geom_point() +
          ggplot2::facet_wrap(~ variable, ncol=3) + ggplot2::labs(x = "Group", y = "Value") + ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none",axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
      }
    })

    output$GridAnswerG_mosaic <- shiny::renderPlot({
      selected_vars <- input$variables
      vectors <- purrr::map(selected_vars, ~ {
        data_for_plot() %>% dplyr::select(selected_vars) %>% dplyr::count(rlang::sym(.x)) %>% dplyr::pull(1)
      })
      union_all <- Reduce(union, vectors); union_all <- ifelse(is.na(union_all), "NA", union_all)
      count_categories <- function(x) { table(factor(x, levels = union_all, exclude =NULL)) }
      category_count_tbl <- data_for_plot() %>% dplyr::summarise(dplyr::across(selected_vars, ~ count_categories(.)))
      cat_tbl <- as.matrix(category_count_tbl); rownames(cat_tbl) <- union_all
      names(cat_tbl) <- ifelse(is.na(names(cat_tbl)), "NA", names(cat_tbl))
      rnames <- rownames(t(cat_tbl))
      t(cat_tbl) %>% tibble::as_tibble() %>% dplyr::mutate(ID=rnames,IDn=1:length(rnames)) %>% dplyr::arrange(dplyr::desc(union_all[1])) %>% dplyr::select(IDn) %>% unlist() %>% setNames(NULL) -> order_vec
      t(cat_tbl)[order_vec,] %>% vcd::mosaic(shade = TRUE,rot_labels = c(0, 0), margins=c(left=12,top=5),just_labels=c(left="right",top="left"))
    })

    output$GridAnswerG_CA <- shiny::renderPlot({
      selected_vars <- input$variables
      vectors <- purrr::map(selected_vars, ~ {
        data_for_plot() %>% dplyr::select(selected_vars) %>% dplyr::count(rlang::sym(.x)) %>% dplyr::pull(1)
      })
      union_all <- Reduce(union, vectors)
      count_categories <- function(x) { table(factor(x, levels = union_all, exclude = NULL)) }
      category_count_tbl <- data_for_plot() %>% dplyr::summarise(dplyr::across(selected_vars, ~ count_categories(.)))
      cat_tbl <- as.matrix(category_count_tbl); rownames(cat_tbl) <- union_all
      FactoMineR::CA(t(cat_tbl))
    })

    output$simple_table <- DT::renderDataTable({
      tmp <- table(data_for_plot()[[input$variables[1]]]); tmp2 <- round(100*prop.table(tmp),1)
      data.frame(tmp,rate=tmp2)[,c(1,2,4)]
    })

    output$simple_table2 <- DT::renderDataTable({
      tmp <- table(data_for_plot()[[input$select_input_data_for_hist]]); tmp2 <- round(100*prop.table(tmp),1)
      data.frame(tmp,rate=tmp2)[,c(1,2,4)]
    })

    output$table_for_plot <- DT::renderDataTable({ data_for_plot() %>% dplyr::select(input$variables) })
  }

  shiny::shinyApp(ui = ui, server = server)
}
