#' speMCA用の変数選択app（モジュール版統合）
#'
#' df が指定されていればそれを使用、未指定なら DataSourceModule 経由で .rda/.RData/.rds をアップロードして選択。
#' 以降の本体処理は src$data() を唯一のデータソースとして参照します。
#'
#' @import shiny
#' @import GDAtools
#' @import ggplot2
#' @import dplyr
#' @importFrom DT datatable
#' @export
Shiny_speMCA <- function(df = NULL) {
  has_df <- !is.null(df)

  ui <- shiny::fluidPage(
    shiny::titlePanel("speMCA 分析アプリ"),
    shiny::tags$p(
      paste0("ENQview Version: ", as.character(utils::packageVersion("ENQview"))),
      style = "color: gray; margin-left: 15px;"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # ▼ 共通モジュール（df 未指定時のみアップロードUIを表示）
        DataSourceUI("src", show_upload = !has_df, accept = c('.rda','.RData','.rds')),

        # --- データがセットされてから出す動的UI（入口を src$data() に統一） ---
        shiny::uiOutput("variables_ui"),        # Active変数
        shiny::uiOutput("junk_selector"),       # junkカテゴリ選択 + 実行ボタン
        shiny::uiOutput("supvars_ui"),          # 追加変数
        shiny::uiOutput("inter_v1_ui"),         # 交互作用 v1
        shiny::uiOutput("inter_v2_ui"),         # 交互作用 v2
        shiny::downloadButton("download_mca", "speMCA結果をダウンロード"),
        shiny::uiOutput("var_ellipses_ui"),     # 楕円対象変数
        shiny::uiOutput("kellipses_cat_selector") # 楕円カテゴリ選択
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("選択変数", shiny::uiOutput("selected_info")),
          shiny::tabPanel("修正慣性率",
                   shiny::tableOutput("eig_table"),
                   shiny::plotOutput("eig_plot")
          ),
          shiny::tabPanel("変数マップ",
                   shiny::plotOutput("var_map", height = "600px"),
                   shiny::plotOutput("var_map_32", height = "600px"),
                   shiny::plotOutput("var_map_13", height = "600px")
          ),
          shiny::tabPanel("個体マップ",
                   shiny::plotOutput("ind_map", height = "600px"),
                   shiny::plotOutput("ind_map_32", height = "600px"),
                   shiny::plotOutput("ind_map_13", height = "600px")
          ),
          shiny::tabPanel("データ表示", DT::DTOutput("data_table")),
          shiny::tabPanel("supvarsの情報", shiny::verbatimTextOutput("supvars_out")),
          shiny::tabPanel("変数マップ＋supvars", shiny::plotOutput("supvars_map")),
          shiny::tabPanel("交互作用plot", shiny::plotOutput("interaction_map")),
          shiny::tabPanel("集中楕円", shiny::plotOutput("kellipses_map"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    showtext::showtext_auto(TRUE)

    # ▼ 共通データ入力モジュール（df 指定/未指定を吸収）
    src <- DataSourceServer("src", df = df, allow_replace = FALSE)

    # ---- データがセットされた後に各入力UIを生成（names(src$data()) を使用） ----
    output$variables_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("variables", "Active変数を選んでください",
                  choices = names(src$data()), multiple = TRUE,
                  selectize = FALSE, size = 7)
    })

    output$supvars_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("supvars", "追加変数を選んでください",
                  choices = names(src$data()), multiple = TRUE,
                  selectize = FALSE, size = 7)
    })

    output$inter_v1_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("inter_v1", "交互作用v1を選んでください",
                  choices = names(src$data()), multiple = FALSE)
    })

    output$inter_v2_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("inter_v2", "交互作用v2を選んでください",
                  choices = names(src$data()), multiple = FALSE)
    })

    output$var_ellipses_ui <- shiny::renderUI({
      shiny::req(src$data())
      shiny::selectInput("var_ellipses", "集中楕円表示変数を選んでください",
                  choices = names(src$data()), multiple = FALSE)
    })

    # ---- junkカテゴリ取得 ----
    junk_cat <- shiny::reactive({
      shiny::req(input$variables, src$data())
      if (length(input$variables) < 2) return(NULL)
      df_sub <- src$data()[, input$variables, drop = FALSE]
      jc <- getindexcat(df_sub)
      if (is.null(jc) || length(jc) == 0) return(NULL)
      jc
    })

    output$junk_selector <- shiny::renderUI({
      jc <- junk_cat()
      if (is.null(jc)) return(NULL)
      shiny::tagList(
        shiny::selectInput(
          inputId  = "excluded_cats",
          label    = "juck指定するカテゴリを選択してください",
          choices  = jc,
          multiple = TRUE,
          selectize = FALSE,
          size     = min(10, length(jc))
        ),
        shiny::actionButton("run_mca", "speMCAを実行する")
      )
    })

    # ---- MCA 実行（specificMCA） ----
    mca_result <- shiny::eventReactive(input$run_mca, {
      shiny::req(input$variables, src$data())
      if (length(input$variables) < 2) return(NULL)

      df_sub <- src$data()[, input$variables, drop = FALSE]
      jc <- junk_cat()
      selected_labels <- input$excluded_cats
      excl_indices <- if (is.null(selected_labels) || length(selected_labels) == 0) NULL else {
        idx <- match(selected_labels, jc); idx[!is.na(idx)]
      }

      tryCatch({
        GDAtools::speMCA(df_sub, excl = excl_indices)
      }, error = function(e) {
        message("MCAエラー: ", e$message)
        NULL
      })
    })

    # ---- speMCAのresult ダウンローダー ----
    output$download_mca <- shiny::downloadHandler(
      filename = function() paste0("mca_result_", Sys.Date(), ".rds"),
      content = function(file) {
        result <- mca_result()
        if (is.null(result)) {
          shiny::showNotification("MCA結果がまだありません。", type = "error"); return(NULL)
        }
        saveRDS(result, file)
      }
    )

    # ---- 修正寄与率の表示 ----
    output$eig_table <- shiny::renderTable({
      res <- mca_result(); if (is.null(res)) return(NULL)
      data.frame(
        軸 = seq_along(res$eig$mrate),
        修正慣性率 = res$eig$mrate,
        累積慣性率 = res$eig$cum.mrate
      )
    })

    output$eig_plot <- shiny::renderPlot({
      res <- mca_result(); if (is.null(res)) return(NULL)
      df_eig <- data.frame(dim = seq_along(res$eig$mrate), mrate = res$eig$mrate, cum_mrate = res$eig$cum.mrate)
      ggplot2::ggplot(df_eig, ggplot2::aes(x = dim)) +
        ggplot2::geom_bar(ggplot2::aes(y = mrate), stat = "identity", fill = "skyblue") +
        ggplot2::geom_line(ggplot2::aes(y = cum_mrate), color = "red", size = 1) +
        ggplot2::geom_point(ggplot2::aes(y = cum_mrate), color = "red", size = 2) +
        ggplot2::labs(x = "次元", y = "修正慣性率", title = "修正慣性率と累積慣性率") +
        ggplot2::theme_minimal()
    })

    # ---- 変数マップ ----
    output$var_map <- shiny::renderPlot({
      showtext::showtext_auto(TRUE)
      res <- mca_result(); if (is.null(res)) return(NULL)
      GDAtools::ggcloud_variables(res) + ggplot2::theme(aspect.ratio = 1) + ggplot2::ggtitle("変数マップ 1−2軸")
    }, width = "auto", height = "auto", res = 120)

    output$var_map_32 <- shiny::renderPlot({
      res <- mca_result(); if (is.null(res)) return(NULL)
      GDAtools::ggcloud_variables(res, axes = c(3, 2)) + ggplot2::theme(aspect.ratio = 1) + ggplot2::ggtitle("変数マップ 3−2軸")
    }, width = "auto", height = "auto", res = 120)

    output$var_map_13 <- shiny::renderPlot({
      res <- mca_result(); if (is.null(res)) return(NULL)
      GDAtools::ggcloud_variables(res, axes = c(1, 3)) + ggplot2::theme(aspect.ratio = 1) + ggplot2::ggtitle("変数マップ 1−3軸")
    }, width = "auto", height = "auto", res = 120)

    # ---- 個体マップ ----
    output$ind_map <- shiny::renderPlot({
      res <- mca_result(); if (is.null(res)) return(NULL)
      GDAtools::ggcloud_indiv(res) + ggplot2::theme(aspect.ratio = 1)
    }, width = "auto", height = 600, res = 120)

    output$ind_map_32 <- shiny::renderPlot({
      res <- mca_result(); if (is.null(res)) return(NULL)
      GDAtools::ggcloud_indiv(res, axes = c(3, 2)) + ggplot2::theme(aspect.ratio = 1)
    }, width = "auto", height = 600, res = 120)

    output$ind_map_13 <- shiny::renderPlot({
      res <- mca_result(); if (is.null(res)) return(NULL)
      GDAtools::ggcloud_indiv(res, axes = c(1, 3)) + ggplot2::theme(aspect.ratio = 1)
    }, width = "auto", height = 600, res = 120)

    # ---- データ表 ----
    output$data_table <- DT::renderDT({
      shiny::req(src$data())
      DT::datatable(src$data(), options = list(pageLength = 10))
    })

    # ---- Active/Junk/楕円カテゴリの確認UI ----
    output$selected_info <- shiny::renderUI({
      shiny::req(input$variables)
      vars <- input$variables
      junk <- input$excluded_cats
      kellipse_cat <- input$category_selector
      shiny::tagList(
        shiny::h4("Active 変数"),
        if (length(vars) > 0) { shiny::HTML(paste("<ul>", paste(paste0("<li>", vars, "</li>"), collapse = ""), "</ul>")) } else { shiny::em("なし") },
        shiny::h4("Junk カテゴリ"),
        if (!is.null(junk) && length(junk) > 0) { shiny::HTML(paste("<ul>", paste(paste0("<li>", junk, "</li>"), collapse = ""), "</ul>")) } else { shiny::em("なし") },
        shiny::h4("集中楕円描画カテゴリ"),
        if (!is.null(kellipse_cat) && length(kellipse_cat) > 0) { shiny::HTML(paste("<ul>", paste(paste0("<li>", kellipse_cat, "</li>"), collapse = ""), "</ul>")) } else { shiny::em("なし") }
      )
    })

    # ---- 集中楕円：変数変更時にカテゴリ候補を更新 ----
    shiny::observeEvent(input$var_ellipses, {
      shiny::req(input$var_ellipses, src$data())
      var <- src$data()[[input$var_ellipses]]
      lv <- levels(as.factor(var))
      shiny::updateCheckboxGroupInput(session, "selected_categories", choices = lv, selected = lv)
    })

    # 楕円カテゴリ選択UI
    output$kellipses_cat_selector <- shiny::renderUI({
      shiny::req(input$var_ellipses, src$data())
      var <- src$data()[[input$var_ellipses]]
      lv <- levels(as.factor(var))
      shiny::checkboxGroupInput("selected_categories", "表示するカテゴリ", choices = lv, selected = lv)
    })

    # 楕円付き個体マップ
    output$kellipses_map <- shiny::renderPlot({
      shiny::req(input$var_ellipses, input$selected_categories)
      resmca <- mca_result(); if (is.null(resmca)) return(NULL)
      var <- src$data()[[input$var_ellipses]]
      base_map_ind <- GDAtools::ggcloud_indiv(resmca, col = "lightgrey")
      var_factor <- as.factor(var)
      sel_index <- which(levels(var_factor) %in% input$selected_categories)
      GDAtools::ggadd_kellipses(base_map_ind, resmca, var = var_factor, sel = sel_index) + ggplot2::coord_fixed(ratio = 1)
    }, width = "auto", height = 600, res = 120)
  }

  shiny::shinyApp(ui, server)
}
