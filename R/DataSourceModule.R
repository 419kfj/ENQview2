#' DataSourceModule — 共通データ入力モジュール（df指定 or RDA/RDSアップロード）
#'
#' 複数の Shiny アプリで共通の「データ入力フロー」を再利用するためのモジュールです。
#' - df が渡されていればそれを利用（検証が通れば）
#' - df が無ければ .rda/.RData/.rds のアップロード→オブジェクト選択
#' - .rds は単一オブジェクト、.rda/.RData は複数オブジェクトを環境にロードします
#'
#' @section 使い方（親アプリ側）:
#'
#' #### UI 側
#' ```r
#' sidebarPanel(
#'   dataSourceUI("src", show_upload = is.null(df)),
#'   # 以降はアプリ固有の UI ...
#' )
#' ```
#'
#' #### server 側
#' ```r
#' src <- dataSourceServer("src", df = df, allow_replace = FALSE,
#'                         validate = function(x) TRUE)  # 必要なら検証関数
#' # 以降は src$data() を唯一のデータソースとして利用
#' observeEvent(src$data(), {
#'   req(src$data())
#'   dat <- src$data()
#'   # updateSelectInput(..., choices = names(dat)) など
#' })
#' ```
#'
#' @param id モジュールID
#' @param show_upload logical df が未指定のとき TRUE を推奨（UI 表示制御）
#' @param accept 受け付ける拡張子（既定: c('.rda','.RData','.rds')）
#' @return UI タグ（`tagList`）
#' @export
#' @import shiny
#' @importFrom tools file_ext
#'
DataSourceUI <- function(id, show_upload = TRUE, accept = c('.rda','.RData','.rds')) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (isTRUE(show_upload)) {
      shiny::tagList(
        shiny::fileInput(ns('file'), 'RDA/RData/RDS ファイルを選択', accept = accept),
        shiny::uiOutput(ns('objects_ui')),
        shiny::helpText('※ .rds は単一オブジェクトのため選択は不要な場合があります')
      )
    } else {
      shiny::helpText('データフレームが指定されています。アップロードは不要です。')
    }
  )
}

#' @title DataSourceServer — 共通データ入力モジュールのサーバー側
#' @description df 指定／アップロードの両フローを吸収して `data()` を返します。
#' @param id モジュールID
#' @param df optional data.frame（指定されればそれを利用）
#' @param allow_replace logical df 指定済みでもアップロードで差替えを許すか（既定: FALSE）
#' @param validate function(df) TRUE/FALSE or 文字列（検証に失敗したときのメッセージ）。NULLなら検証しない。
#' @return list(data = reactive({df}), ready = reactive({logical}))
#' @export
#' @import shiny
#' @importFrom tools file_ext
DataSourceServer <- function(id, df = NULL, allow_replace = FALSE, validate = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data.rv <- shiny::reactiveVal(NULL)

    has_df <- !is.null(df)

    # --- 起動直後：df が渡されていればセット＆検証 ---
    if (has_df) {
      ok <- TRUE
      if (!is.null(validate)) {
        res <- try(validate(df), silent = TRUE)
        if (is.logical(res)) ok <- isTRUE(res) else if (is.character(res)) { ok <- FALSE; shiny::showNotification(res, type = 'error') }
      }
      if (ok) data.rv(df) else data.rv(NULL)
    }

    # --- アップロード：df 無 or 差替え許可時のみ反応 ---
    rda_env <- shiny::reactive({
      if (has_df && !allow_replace) return(NULL)
      shiny::req(input$file)
      env <- new.env()
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == 'rds') {
        obj <- readRDS(input$file$datapath)
        assign('.__rds__', obj, envir = env)
      } else {
        load(input$file$datapath, envir = env)
      }
      env
    })

    output$objects_ui <- shiny::renderUI({
      if (has_df && !allow_replace) return(NULL)
      shiny::req(rda_env())
      objs <- ls(rda_env())
      if (identical(objs, '.__rds__')) {
        shiny::helpText('RDS が読み込まれました。単一オブジェクトを利用します。')
      } else {
        shiny::selectInput(ns('object'), 'RDA/RData 内のオブジェクトを選択', choices = objs, selected = objs[1])
      }
    })

    shiny::observe({
      if (has_df && !allow_replace) return(NULL)
      shiny::req(rda_env())
      objs <- ls(rda_env())

      set_df <- function(x) {
        ok <- TRUE
        if (!is.null(validate)) {
          res <- try(validate(x), silent = TRUE)
          if (is.logical(res)) ok <- isTRUE(res) else if (is.character(res)) { ok <- FALSE; shiny::showNotification(res, type = 'error') }
        }
        if (ok) {
          data.rv(x)
          shiny::showNotification('データフレームを読み込みました。')
        } else {
          data.rv(NULL)
        }
      }

      if (identical(objs, '.__rds__')) {
        obj <- get('.__rds__', envir = rda_env())
        if (is.data.frame(obj)) set_df(obj) else {
          shiny::showNotification('RDS のオブジェクトはデータフレームではありません。', type = 'error')
        }
        return(invisible(NULL))
      }

      shiny::req(input$object)
      obj <- get(input$object, envir = rda_env())
      if (is.data.frame(obj)) set_df(obj) else {
        shiny::showNotification('選択したオブジェクトはデータフレームではありません。', type = 'error')
        data.rv(NULL)
      }
    })

    list(
      data  = shiny::reactive({ data.rv() }),
      ready = shiny::reactive({ !is.null(data.rv()) })
    )
  })
}
