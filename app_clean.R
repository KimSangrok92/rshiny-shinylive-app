options(shiny.maxRequestSize = 100 * 1024^2)

options(encoding = "UTF-8")

try({
    if (!grepl("UTF-8|65001", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
        suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE))
        suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
    }
}, silent = TRUE)

library(shiny)

library(shinydashboardPlus)

library(shinyBS)

library(rintrojs)

library(readxl)

library(dplyr)

library(ggplot2)

library(DT)

library(lubridate)

library(scales)

library(stringr)

library(tidyr)

library(bslib)

suppressWarnings(try(library(showtext), silent = TRUE))
suppressWarnings(try(library(showtextdb), silent = TRUE))

options(asp_plot_family = "sans")
try({
    if (requireNamespace("showtext", quietly = TRUE) && requireNamespace("showtextdb", quietly = TRUE)) {
        showtextdb::load_showtext_fonts()
        showtext::showtext_auto(enable = TRUE)
        options(asp_plot_family = "wqy-microhei")
    }
}, silent = TRUE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

empty_diag_df <- function() {
    tibble::tibble(DIAG_DATE = as.Date(character()), ICD_NAME = character(), C1 = character(), C2 = character(), MDRP_NO = character(), PTNO = character())
}

norm_key <- function(x) {
    stringr::str_to_lower(stringr::str_replace_all(stringr::str_trim(x), "\\s+", "_"))
}

compress_key <- function(x) {
    stringr::str_replace_all(norm_key(x), "[_\\-]", "")
}

as_num <- function(x) {
    suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(x))))
}

pick_col <- function(df, candidates) {
    if (is.null(df) || ncol(df) == 0) 
        return(NA_character_)
    raw <- names(df)
    nk <- norm_key(raw)
    ck <- compress_key(raw)
    for (cand in candidates) {
        c_nk <- norm_key(cand)
        c_ck <- compress_key(cand)
        hit <- which(nk == c_nk)
        if (length(hit) >= 1) 
            return(raw[hit[1]])
        hit <- which(ck == c_ck)
        if (length(hit) >= 1) 
            return(raw[hit[1]])
        hit <- which(stringr::str_detect(nk, stringr::fixed(c_nk)))
        if (length(hit) == 1) 
            return(raw[hit])
    }
    NA_character_
}

keep_only_prev <- function(prev, choices) {
    intersect(prev %||% character(0), choices)
}

to_month_key <- function(x) {
    if (inherits(x, "Date")) 
        return(format(x, "%Y-%m"))
    if (inherits(x, "POSIXct")) 
        return(format(as.Date(x), "%Y-%m"))
    v <- as.character(x)
    v <- trimws(v)
    v[v == ""] <- NA_character_
    v <- ifelse(!is.na(v) & grepl("^\\d{6}$", v), paste0(substr(v, 1, 4), "-", substr(v, 5, 6)), v)
    v <- gsub("[./]", "-", v)
    v <- sub("^(\\d{4})-(\\d{1})$", "\\1-0\\2", v)
    dt <- suppressWarnings(as.Date(ifelse(grepl("^\\d{4}-\\d{2}$", v), paste0(v, "-01"), v)))
    ifelse(is.na(dt), v, format(dt, "%Y-%m"))
}

parse_any_datetime <- function(x) {
    if (inherits(x, "POSIXct")) 
        return(x)
    if (inherits(x, "Date")) 
        return(lubridate::as_datetime(x))
    if (is.numeric(x)) 
        return(lubridate::as_datetime(x, origin = "1899-12-30"))
    x <- as.character(x)
    x[!nzchar(x)] <- NA_character_
    out <- suppressWarnings(lubridate::ymd_hms(x))
    idx <- is.na(out)
    if (any(idx)) 
        out[idx] <- suppressWarnings(lubridate::ymd(x[idx]))
    idx <- is.na(out)
    if (any(idx)) 
        out[idx] <- suppressWarnings(lubridate::mdy_hms(x[idx]))
    idx <- is.na(out)
    if (any(idx)) 
        out[idx] <- suppressWarnings(lubridate::mdy(x[idx]))
    idx <- is.na(out)
    if (any(idx)) 
        out[idx] <- suppressWarnings(lubridate::ymd_hms(gsub("/", "-", x[idx])))
    out
}

coerce_ddd_to_grams <- function(ddd_val, ddd_unit) {
    v <- as_num(ddd_val)
    u <- toupper(trimws(as.character(ddd_unit)))
    if (is.na(v)) 
        return(NA_real_)
    if (u %in% c("MG", "밀리그램")) 
        return(v/1000)
    if (u %in% c("G", "그램", "")) 
        return(v)
    v
}

dose_to_grams <- function(amount, unit, unit_per_form_g) {
    unit_up <- toupper(as.character(unit))
    if (!is.na(unit_up) && unit_up %in% c("CAP", "TAB", "VIA", "VIAL", "AMP", "BOTTLE", "EA", "UNIT", "개", "정", "캡슐", "병", "바이알", "앰플")) 
        return(as_num(amount) * as_num(unit_per_form_g))
    if (!is.na(unit_up) && unit_up %in% c("MG", "밀리그램", "MG.")) 
        return(as_num(amount)/1000)
    if (!is.na(unit_up) && unit_up %in% c("G", "그램", "G.")) 
        return(as_num(amount))
    if (!is.na(unit_per_form_g) && !is.na(amount)) 
        return(as_num(amount) * as_num(unit_per_form_g))
    NA_real_
}

resolve_sheet_name_from_list <- function(sheets, targets) {
    sn <- norm_key(sheets)
    for (t in targets) {
        idx <- which(sn == norm_key(t))
        if (length(idx) == 1) 
            return(sheets[idx])
    }
    for (t in targets) {
        hit <- which(stringr::str_detect(sn, stringr::fixed(norm_key(t))))
        if (length(hit) == 1) 
            return(sheets[hit])
    }
    NA_character_
}

resolve_abx_sheet <- function(sheets) {
    resolve_sheet_name_from_list(sheets, c("Sheet4", "antibiotics", "antibiotic", "abx", "항생제_사용리스트", "항생제", "항생제리스트"))
}

resolve_pd_sheet <- function(sheets) {
    resolve_sheet_name_from_list(sheets, c("Sheet5", "inpatient day", "inpatient days", "inpatient_days", "patient days", "patient_days", "입원일수", "재원일수", "환자일수", "입원일", "연월_입원일수"))
}

has_dotddd_sheets <- function(sheets) {
    !is.na(resolve_abx_sheet(sheets)) && !is.na(resolve_pd_sheet(sheets))
}

standardize_abx <- function(df) {
    col_patient <- pick_col(df, c("patient_id", "환자구분번호", "환자번호", "ptno", "mdrp_no"))
    col_mdrp <- pick_col(df, c("MDRP_NO", "mdrp_no", "환자구분번호"))
    col_ptno <- pick_col(df, c("PTNO", "ptno", "환자번호", "patient_id"))
    col_atc <- pick_col(df, c("ATC_Code", "atc_code", "ATC", "atc", "ATC 코드", "atccode"))
    col_ingr <- pick_col(df, c("성분명", "ingredient", "INGREDIENT", "generic_name", "Generic name", "GENERIC NAME"))
    col_form <- pick_col(df, c("제형", "form", "FORM", "dosage_form", "Dosage form"))
    col_route <- pick_col(df, c("경로", "route", "ROUTE", "adm_route", "Adm.R", "ADM.R", "Administration route", "투여경로"))
    col_amt <- pick_col(df, c("일회량", "dose", "일회 용량", "투여량", "용량"))
    col_unit <- pick_col(df, c("단위", "unit", "UNIT", "Adm_unit", "adm_unit"))
    col_unit_g <- pick_col(df, c("단위환산함량", "unit_to_grams", "규격(g)", "함량_g", "그램환산", "규격단위당_함량", "규격단위당환산함량"))
    col_ddd_val <- pick_col(df, c("DDD", "ddd"))
    col_ddd_unit <- pick_col(df, c("DDD단위", "ddd_unit", "DDD_unit"))
    col_dt <- pick_col(df, c("투여일시", "dose_datetime", "administration_time", "administration_datetime"))
    col_date <- pick_col(df, c("투여일", "dose_date", "date", "투여일자"))
    col_month <- pick_col(df, c("처방월", "presc_month", "yyyymm", "연월"))
    col_class <- pick_col(df, c("class", "계열", "계열분류", "drug_class", "계열코드"))
    col_age <- pick_col(df, c("나이", "age", "연령"))
    col_seq <- pick_col(df, c("처방일투여순번", "투여순번", "adm_seq", "sequence"))
    col_data_id <- pick_col(df, c("데이터번호", "data_id", "record_id"))
    col_key <- pick_col(df, c("상품명_OR_병원코드_OR_EDI코드", "drug_key", "product_code", "drug_name", "상품명", "병원코드", "약품코드", "EDI", "EDI코드", "제품코드"))
    col_spec <- pick_col(df, c("규격단위", "spec_unit", "규격단위(규격)", "규격"))
    col_aud <- pick_col(df, c("AUD", "aud"))
    need_core <- c(col_patient %||% col_ptno %||% col_mdrp, col_atc, col_ingr, col_amt, col_unit, col_unit_g, col_dt %||% col_date)
    if (any(is.na(need_core))) {
        stop(sprintf("antibiotics 시트 필수 컬럼 누락: %s", paste(c("환자구분번호/환자번호", "ATC_Code", "성분명", "일회량(투여량)", "단위", "단위환산함량", "투여일시/투여일")[is.na(need_core)], collapse = ", ")))
    }
    dt_vec <- if (!is.na(col_dt)) 
        parse_any_datetime(df[[col_dt]])
    else as.POSIXct(NA)
    date_vec <- if (!is.na(col_date)) 
        as.Date(parse_any_datetime(df[[col_date]]))
    else as.Date(dt_vec)
    month_vec_from_date <- to_month_key(date_vec)
    month_vec <- if (!is.na(col_month)) 
        to_month_key(df[[col_month]])
    else month_vec_from_date
    ddd_g_vec <- NA_real_
    if (!is.na(col_ddd_val) && !is.na(col_ddd_unit)) {
        ddd_g_vec <- coerce_ddd_to_grams(df[[col_ddd_val]], df[[col_ddd_unit]])
    }
    else if (!is.na(col_ddd_val)) {
        ddd_g_vec <- as_num(df[[col_ddd_val]])
    }
    form_vec <- if (!is.na(col_form)) 
        as.character(df[[col_form]])
    else "UNK"
    route_vec <- if (!is.na(col_route)) 
        as.character(df[[col_route]])
    else "UNK"
    route_vec <- dplyr::recode(route_vec, O = "Oral", o = "Oral", P = "Parenteral", p = "Parenteral", .default = route_vec)
    patient_id_vec <- if (!is.na(col_patient)) 
        as.character(df[[col_patient]])
    else if (!is.na(col_ptno)) 
        as.character(df[[col_ptno]])
    else as.character(df[[col_mdrp]])
    out <- dplyr::tibble(patient_id = patient_id_vec, atc_code = as.character(df[[col_atc]]), ingredient = as.character(df[[col_ingr]]), form = form_vec, route = route_vec, dose = as_num(df[[col_amt]]), unit = as.character(df[[col_unit]]), unit_g = as_num(df[[col_unit_g]]), ddd_g = ddd_g_vec, dose_dt = dt_vec, date = date_vec, month = month_vec)
    if (!is.na(col_class)) 
        out$class <- as.character(df[[col_class]])
    if (!is.na(col_mdrp)) 
        out$MDRP_NO <- as.character(df[[col_mdrp]])
    if (!is.na(col_ptno)) 
        out$PTNO <- as.character(df[[col_ptno]])
    if (!is.na(col_key)) 
        out$drug_key <- as.character(df[[col_key]])
    if (!is.na(col_seq)) 
        out$adm_seq <- as.character(df[[col_seq]])
    if (!is.na(col_data_id)) 
        out$data_id <- as.character(df[[col_data_id]])
    if (!is.na(col_month)) 
        out$presc_month <- month_vec
    if (!is.na(col_spec)) 
        out$spec_unit <- as.character(df[[col_spec]])
    if (!is.na(col_age)) 
        out$age <- as_num(df[[col_age]])
    if (!is.na(col_aud)) 
        out$aud <- as_num(df[[col_aud]])
    out
}

standardize_pd <- function(df) {
    col_month <- pick_col(df, c("month", "연월", "처방월", "월", "yyyymm", "ym"))
    col_pd <- pick_col(df, c("patient_days", "입원일수", "재원일수", "환자일수", "입원일", "pd"))
    if (is.na(col_month) || is.na(col_pd)) 
        stop("inpatient day(s) 시트 필수 컬럼 누락: 연월(month), 입원일수(patient_days)")
    dplyr::filter(tibble::tibble(month = to_month_key(df[[col_month]]), patient_days = as_num(df[[col_pd]])), !is.na(month), !is.na(patient_days))
}

if (!exists("LIGHT_BLUE")) LIGHT_BLUE <- "#1EA0FF"

if (!exists("BAR_BLUE")) BAR_BLUE <- "#0066CC"

GRID_GREY <- "#E6E6E6"

theme_sheet3_blue <- function() {
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(text = ggplot2::element_text(family = getOption("asp_plot_family", "sans")), plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0, color = "black"), plot.subtitle = ggplot2::element_text(color = "black"), plot.caption = ggplot2::element_text(size = 9, color = "black"), axis.title = ggplot2::element_text(size = 12, color = "black"), axis.text = ggplot2::element_text(size = 10, color = "black"), legend.title = ggplot2::element_text(color = "black"), legend.text = ggplot2::element_text(color = "black"), 
        axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.6), axis.line.y = ggplot2::element_line(color = "black", linewidth = 0.6), axis.ticks = ggplot2::element_line(color = "#444444", linewidth = 0.4), axis.ticks.length = grid::unit(3, "pt"), panel.grid.major.x = ggplot2::element_line(color = "#F0F0F0", linewidth = 0.4), panel.grid.major.y = ggplot2::element_line(color = GRID_GREY, linewidth = 0.4), panel.grid.minor = ggplot2::element_blank(), plot.margin = ggplot2::margin(6, 
            6, 6, 6), legend.position = "none")
}

fold_panel <- function(title, ..., right = NULL, open = TRUE) {
    open_attr <- if (isTRUE(open)) 
        NA
    else NULL
    shiny::tags$details(class = "fold", open = open_attr, shiny::tags$summary(shiny::tags$div(class = "head-row", shiny::tags$span(class = "head-left", title), if (!is.null(right)) 
        shiny::tags$div(class = "head-right no-toggle", right)), shiny::tags$i(class = "fa fa-chevron-down fold-caret")), shiny::div(class = "fold-body", ...))
}

sidebar_css <- "\n  /* 사이드바 루트: 파란 배경 + 내용에 따라 높이 증가 + 최소 높이는 뷰포트 기준 */\n  #s2_sidebar{\n    display:flex; flex-direction:column;\n    background:#082846;                 /* ← 파란 배경(전체) */\n    border-radius:8px;\n    padding:10px;\n    height:auto;                        /* ← 고정 높이 제거 */\n    min-height:calc(100vh - 120px);     /* ← 화면이 작아도 최소 높이 보장 */\n    width: 280px;       /* ← 추가 */\n    min-width: 280px;   /* ← 추가 */\n  }\n\n  /* 스크롤 영역: 파란 배경 유지, 내부 스크롤 허용(높이는 뷰포트 기준으로 계산) */\n  #s2_sidebar .s2-scroll{\n    flex:1 1 auto;\n    overflow-y:auto;\n    padding-right:4px;\n    max-height:calc(100vh - 120px - 20px); /* 사이드바 패딩(약 20px) 감안 */\n    background:transparent;                /* 배경은 상위(#s2_sidebar)가 담당 */\n  }\n\n  /* 섹션 간격 */\n  #s2_sidebar .control-card{ margin-bottom:10px; background:transparent; }\n\n  /* '분석기간/진단일자' 라벨, 체크박스 라벨 가독성 */\n  #s2_sidebar .control-label,\n  #s2_sidebar label,\n  #s2_sidebar .checkbox label { color:#FFFFFF; }\n\n  /* 기존에 있던 항목 유지 */\n  #s2_run{width:100%;font-weight:700;margin-top:8px;}\n  #sheet_selector{display:none!important;}\n"

fold_css <- "\n.fold { background:#0B3156; border-radius:6px; margin-bottom:10px; }\n.fold > summary { cursor:pointer; padding:8px 10px; color:#fff; font-weight:600;\n                  display:flex; align-items:center; }\n.fold > summary::-webkit-details-marker { display:none; }\n\n/* 헤더 왼쪽(제목) + 오른쪽(전체선택) 배치 컨테이너 */\n.fold > summary .head-row{ display:flex; align-items:center; justify-content:space-between; gap:12px; width:100%; }\n\n/* 오른쪽의 전체선택 체크박스 컨테이너 */\n.fold > summary .head-right{ display:flex; align-items:center; gap:8px; }\n.fold > summary .head-right .shiny-input-container{ margin:0; }\n.fold > summary .head-right label{ margin:0; color:#fff; font-weight:600; font-size:12px; }\n.fold > summary .head-right input[type='checkbox']{ transform:scale(.95); }\n\n/* 본문 */\n.fold .fold-body { padding:8px 10px; background:#082846; border-top:1px solid rgba(255,255,255,.08); }\n.fold .shiny-input-checkboxgroup { max-height:260px; overflow-y:auto; margin:0; }\n\n/* 접힘 화살표 */\n.fold .fold-caret { margin-left:8px; transition:transform .15s ease; }\n.fold[open] .fold-caret { transform:rotate(180deg); }\n"

head_tools_js_css <- "\n<script>\n  // summary 내부에서 .no-toggle 영역 클릭 시 접힘 방지\n  document.addEventListener('click', function(e){\n    var blocker = e.target.closest('.no-toggle');\n    if(blocker && blocker.closest('summary')){ e.stopPropagation(); }\n  }, true);\n</script>\n<style>\n  .fold summary .small-link { font-size:12px; font-weight:600; margin-left:8px; color:#fff; opacity:.95; }\n  .fold summary .small-link:hover { text-decoration: underline; }\n  .fold summary .sep { margin:0 6px; opacity:.7; color:#fff; }\n</style>\n"

ui <- fluidPage(theme = bslib::bs_theme(version = 3), tags$head(tags$link(rel = 'preconnect', href = 'https://fonts.googleapis.com')), tags$head(tags$link(rel = 'preconnect', href = 'https://fonts.gstatic.com', crossorigin = 'anonymous')), tags$head(tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css2?family=Noto+Sans+KR:wght@400;500;700&display=swap')), tags$head(tags$style(HTML("\n      :root {\n        --hanyang-blue: #003876;\n        --hanyang-light-blue: #0066CC;\n        --hanyang-medium-blue: #004B9F;\n        --hanyang-dark-blue: #002a5c;\n        --hanyang-accent-blue: #1ea0ff;\n        --light-gray: #F8F9FA;\n        --header-h: 70px;\n      }\n\n      /* 헤더 */\n      .hospital-header {\n        background: linear-gradient(90deg, var(--hanyang-blue), var(--hanyang-light-blue));\n        color: #fff;\n        position: fixed;\n        top: 0; left: 0; right: 0;\n        height: var(--header-h);\n        display: flex; align-items: center; justify-content: space-between;\n        padding: 0 20px;\n        box-shadow: 0 2px 10px rgba(0,0,0,.1);\n        z-index: 2000;\n      }\n      .hospital-title { margin: 0; font-size: 30px; font-weight: 700; }\n\n      /* 레이아웃 */\n      html, body { min-height: 100%; font-family: 'Noto Sans KR', 'Malgun Gothic', 'Apple SD Gothic Neo', sans-serif; }\n      .layout-container{\n        display: flex; align-items: stretch;\n        min-height: calc(100vh - var(--header-h));\n        margin-top: var(--header-h);\n      }\n\n      /* 좌측 사이드바 */\n      .sidebar-container{\n        position: relative;\n        width: 300px;\n        transition: width .25s ease;\n        overflow: visible;                 /* 내부 스크롤은 하위 요소에서 */\n        background: linear-gradient(180deg, var(--hanyang-blue), var(--hanyang-dark-blue));\n        border-right: 3px solid var(--hanyang-light-blue);\n        display: flex; flex-direction: column;\n      }\n      .sidebar-container.collapsed{\n        width: 0 !important;\n        overflow: hidden; pointer-events: none;\n      }\n      .well{\n        background: transparent; color: #fff; border: 0; box-shadow: none;\n        padding: 18px; min-height: 100%;\n        display: flex; flex-direction: column;\n      }\n\n      /* 컨트롤 카드 */\n      .control-card{\n        background: rgba(255,255,255,.06);\n        border: 1px solid rgba(255,255,255,.12);\n        border-radius: 12px;\n        padding: 12px; margin-bottom: 14px;\n      }\n\n      /* 우측 메인 */\n      .main-panel{ flex: 1; background: var(--light-gray); min-width: 0; overflow: auto; }\n      .main-content{\n        margin: 20px; padding: 24px; background: #fff;\n        border-radius: 14px; box-shadow: 0 4px 20px rgba(0,0,0,.08);\n        border-top: 4px solid var(--hanyang-light-blue);\n      }\n\n      /* 시트 버튼 래퍼 */\n      .sheet-btn-wrap{ display:flex; justify-content:center; gap:40px; flex-wrap:wrap; margin-bottom:10px; }\n      .sheet-btn-wrap .btn{\n        border-radius:28px; padding:16px 40px; font-weight:700; font-size:20px; min-width:280px;\n        box-shadow:0 2px 6px rgba(0,0,0,.12);\n      }\n      .sheet-btn-wrap .btn i{ margin-right:6px; }\n      .sheet-btn-wrap .btn:hover{ transform:translateY(-1px); box-shadow:0 4px 10px rgba(0,0,0,.18); }\n      @media (max-width: 992px){\n        .sheet-btn-wrap .btn{ min-width:170px; padding:10px 22px; font-size:15px; }\n      }\n\n      /* 버튼 */\n      .btn-info{\n        background: linear-gradient(45deg, var(--hanyang-blue), var(--hanyang-light-blue)) !important;\n        border: none !important; color: white !important; border-radius: 25px !important;\n        font-weight: 600 !important; padding: 12px 25px !important; text-transform: uppercase !important;\n        box-shadow: 0 3px 10px rgba(0,56,118,0.3) !important; transition: all .3s ease !important;\n        margin-right: 10px !important;\n      }\n      .btn-info:hover{\n        background: linear-gradient(45deg, var(--hanyang-medium-blue), var(--hanyang-accent-blue)) !important;\n        transform: translateY(-2px) !important;\n      }\n\n      /* Bootstrap 기본 좌우 패딩 제거 (풀폭) */\n      .container-fluid{ padding-left:0 !important; padding-right:0 !important; \n      }\n      /* Sheet2(DOT/DDD)용 컨텐츠 폭 제한 래퍼 */\n      .s2-narrow {\n        max-width: 1400px;     /* 필요시 1000~1400px 사이에서 조절 */\n        width: 100%;\n        margin-left: auto;     /* 가운데 정렬 */\n        margin-right: auto;\n      }\n      /* 초대형 화면에서도 과도한 확장 방지 */\n      @media (min-width: 1800px) {\n      .s2-narrow { max-width: 1400px; }\n      }\n      /* 중간 화면에서는 여백을 조금 더 활용 */\n      @media (max-width: 1400px) {\n      .s2-narrow { max-width: 96%; }\n      }\n      /* 날짜 달력(dropdown) 레이어를 헤더(z-index:2000)보다 위에 배치 */\n      .datepicker-dropdown { z-index: 4000 !important; }\n    "))), 
    tags$head(tags$script(HTML("\n      (function () {\n        // 사이드바 토글\n        $(function(){\n          $('#sidebarToggle').on('click', function(){\n            $('.sidebar-container').toggleClass('collapsed');\n          });\n        });\n\n        var lockedTitle = null;\n        function applyTitle(){\n          if(!lockedTitle) return;\n          if(document.title !== lockedTitle) document.title = lockedTitle;\n          var h = document.querySelector('.hospital-title');\n          if(h && h.textContent !== lockedTitle) h.textContent = lockedTitle;\n        }\n\n        // Shiny 핸들러 등록 (지연 로딩 대비)\n        function register(){\n          if(!(window.Shiny && Shiny.addCustomMessageHandler)) return false;\n          Shiny.addCustomMessageHandler('setTitle', function(x){\n            lockedTitle = (x && x.title) ? String(x.title) : null;\n            applyTitle();\n          });\n          return true;\n        }\n        if(!register()){ document.addEventListener('shiny:connected', register); }\n\n        // DOM 준비 시 1회 적용 + 변경 감시 + 폴백\n        document.addEventListener('DOMContentLoaded', applyTitle);\n        function obs(sel, opt){\n          var el = (sel === 'title') ? document.querySelector('head > title') : document.querySelector(sel);\n          if(!el) return;\n          new MutationObserver(applyTitle).observe(el, opt || {childList:true, characterData:true, subtree:true});\n        }\n        obs('title'); obs('.hospital-title');\n        setInterval(applyTitle, 1000);\n      })();\n      \n    "))), 
    introjsUI(), div(class = "hospital-header", h1(class = "hospital-title", "ASP Toolkit Program"), actionButton("sidebarToggle", "☰", class = "btn btn-info sidebar-toggle", width = "auto")), div(class = "layout-container", div(class = "sidebar-container", div(class = "well", fileInput("file", "📊 Data Upload", accept = ".xlsx"), uiOutput("sheet_selector"), uiOutput("filter_controls"))), div(class = "main-panel", div(class = "main-content", uiOutput("upload_status"), uiOutput("main_content")))))

server <- function(input, output, session) {
    dt_center_opts <- list(pageLength = 10, scrollX = TRUE, dom = "tip", columnDefs = list(list(className = "dt-center", targets = "_all")))
    rv_choices <- reactiveValues(sheet1_diagname = character(0))
    file_uploaded <- reactiveVal(FALSE)
    available_sheets <- reactiveVal(NULL)
    current_data <- reactiveVal(NULL)
    sheet1_c2_initialized <- reactiveVal(FALSE)
    diag_all_programmatic <- reactiveVal(FALSE)
    sheet1_diag_initialized <- reactiveVal(FALSE)
    prog_sheet1_c1_all <- reactiveVal(FALSE)
    prog_sheet1_c2_all <- reactiveVal(FALSE)
    prog_sheet1_diag_all <- reactiveVal(FALSE)
    prog_s2_class_all <- reactiveVal(FALSE)
    prog_s2_ingr_all <- reactiveVal(FALSE)
    prog_s2_route_all <- reactiveVal(FALSE)
    s2_updating <- reactiveVal(FALSE)
    prog_s2_dept_all <- reactiveVal(FALSE)
    prog_s2_c1_all <- reactiveVal(FALSE)
    prog_s2_c2_all <- reactiveVal(FALSE)
    observeEvent(list(input$file, input$sheet), {
        sheet1_c2_initialized(FALSE)
        sheet1_diag_initialized(FALSE)
    }, ignoreInit = TRUE)
    has_valid_age <- function(df) {
        "age" %in% names(df) && any(is.finite(df$age), na.rm = TRUE)
    }
    get_visible_sheets <- function() {
        sl <- available_sheets() %||% character(0)
        abx <- resolve_abx_sheet(sl)
        pd <- resolve_pd_sheet(sl)
        setdiff(sl, c(abx, pd))
    }
    observeEvent(input$file, {
        req(input$file)
        tryCatch({
            file_uploaded(TRUE)
            available_sheets(readxl::excel_sheets(input$file$datapath))
        }, error = function(e) {
            showNotification(paste("파일 처리 오류:", e$message), type = "error")
            file_uploaded(FALSE)
        })
    })
    output$upload_status <- renderUI({
        if (!file_uploaded()) {
            return(div(style = "text-align:center;padding:50px;", h3("📂 Excel 데이터를 업로드해주세요")))
        }
        vis <- get_visible_sheets()
        fluidRow(column(12, div(class = "sheet-btn-wrap", if ("Sheet3" %in% vis) 
            bsButton("patient_info_btn", "👥 Patients", style = "info", size = "large"), if (length(vis) >= 1) 
            bsButton("patients_btn", "🏥 Diagnosis", style = "info", size = "large"), if (length(vis) >= 2) 
            bsButton("antimicrobials_btn", "💊 DOT/DDD", style = "info", size = "large"))))
    })
    output$sheet_selector <- renderUI({
        if (!file_uploaded()) 
            return(NULL)
        vis <- get_visible_sheets()
        default_sheet <- if ("Sheet3" %in% vis) 
            "Sheet3"
        else if (length(vis) > 0) 
            vis[1]
        else NULL
        tags$div(style = "display:none;", selectInput("sheet", label = NULL, choices = vis, selected = default_sheet))
    })
    observeEvent(input$patients_btn, {
        vis <- get_visible_sheets()
        if (length(vis) >= 1) 
            updateSelectInput(session, "sheet", selected = vis[1])
    })
    observeEvent(input$antimicrobials_btn, {
        vis <- get_visible_sheets()
        if (length(vis) >= 2) 
            updateSelectInput(session, "sheet", selected = vis[2])
    })
    observeEvent(input$patient_info_btn, {
        vis <- get_visible_sheets()
        if ("Sheet3" %in% vis) 
            updateSelectInput(session, "sheet", selected = "Sheet3")
    })
    observe({
        if (!file_uploaded() || is.null(input$file) || is.null(input$sheet)) 
            return()
        tryCatch({
            data <- readxl::read_excel(input$file$datapath, sheet = input$sheet)
            current_data(data)
        }, error = function(e) {
            showNotification(paste("데이터 읽기 오류:", e$message), type = "error")
            current_data(NULL)
        })
    })
    same_set <- function(a, b) {
        a <- sort(unique(a %||% character(0)))
        b <- sort(unique(b %||% character(0)))
        identical(a, b)
    }
    safe_update_group <- function(inputId, rv_slot, choices, selected) {
        old_choices <- isolate(rv_choices[[rv_slot]] %||% character(0))
        old_selected <- isolate(input[[inputId]] %||% character(0))
        choices <- choices %||% character(0)
        selected <- selected %||% character(0)
        if (same_set(old_choices, choices) && same_set(old_selected, selected)) {
            return(invisible(FALSE))
        }
        if (!is.null(input[[inputId]])) 
            freezeReactiveValue(input, inputId)
        updateCheckboxGroupInput(session, inputId, choices = choices, selected = selected)
        rv_choices[[rv_slot]] <- choices
        invisible(TRUE)
    }
    begin_batch_update <- function(flag_reactiveVal) {
        if (isTRUE(flag_reactiveVal())) 
            return(FALSE)
        flag_reactiveVal(TRUE)
        session$onFlushed(function() flag_reactiveVal(FALSE), once = TRUE)
        TRUE
    }
    sheet3_raw <- reactive({
        if (!file_uploaded()) 
            return(NULL)
        sl <- available_sheets() %||% character(0)
        if (!("Sheet3" %in% sl)) 
            return(NULL)
        tryCatch(read_excel(input$file$datapath, sheet = "Sheet3"), error = function(e) {
            showNotification(paste("Sheet3 읽기 오류:", e$message), "error")
            NULL
        })
    })
    sheet3_data <- reactive({
        df <- sheet3_raw()
        if (is.null(df) || nrow(df) == 0) 
            return(tibble())
        col_adm <- pick_col(df, c("ADMTIME", "admtime", "adm_time", "admission_time", "입원일시", "입원일자", "입원일", "admissiondate", "admission date"))
        col_dis <- pick_col(df, c("DISCHTIME", "dischtime", "dis_time", "discharge_time", "퇴원일시", "퇴원일자", "퇴원일", "dischargedate", "discharge date"))
        col_sex <- pick_col(df, c("sex", "SEX", "gender", "성별"))
        col_dept <- pick_col(df, c("ADM_DEPARTMENT", "adm_department", "department", "진료과", "입원부서"))
        col_mdrp <- pick_col(df, c("MDRP_NO", "mdrp_no"))
        col_ptno <- pick_col(df, c("PTNO", "ptno", "환자번호", "patient_id"))
        col_admtyp <- pick_col(df, c("adm_type", "admission_type", "입원경로"))
        col_expire <- pick_col(df, c("inhospital_expire", "원내사망", "사망여부"))
        col_age <- pick_col(df, c("age", "연령"))
        validate(need(all(!is.na(c(col_adm, col_dis, col_sex))), "Sheet3에는 ADMTIME, DISCHTIME, sex 컬럼이 있어야 합니다."))
        out <- tibble(ADMTIME = parse_any_datetime(df[[col_adm]]), DISCHTIME = parse_any_datetime(df[[col_dis]]), sex = as.character(df[[col_sex]]))
        if (!is.na(col_dept)) 
            out$ADM_DEPARTMENT <- as.character(df[[col_dept]])
        if (!is.na(col_mdrp)) 
            out$MDRP_NO <- df[[col_mdrp]]
        if (!is.na(col_ptno)) 
            out$PTNO <- df[[col_ptno]]
        if (!is.na(col_admtyp)) 
            out$adm_type <- as.character(df[[col_admtyp]])
        if (!is.na(col_expire)) 
            out$inhospital_expire <- df[[col_expire]]
        if (!is.na(col_age)) 
            out$age <- as_num(df[[col_age]])
        out %>% mutate(length_of_stay = as.numeric(difftime(DISCHTIME, ADMTIME, units = "days")), length_of_stay = ifelse(length_of_stay < 0, NA, length_of_stay), admit_year = year(ADMTIME))
    })
    output$plot_yearly <- renderPlot({
        d <- sheet3_data()
        if (is.null(d) || nrow(d) == 0 || all(is.na(d$ADMTIME))) {
            return(ggplot() + geom_text(aes(0.5, 0.5, label = "유효한 ADMTIME 데이터가 없습니다"), color = "grey50", size = 5) + theme_void())
        }
        mon <- d %>% filter(!is.na(ADMTIME)) %>% mutate(month = as.Date(lubridate::floor_date(ADMTIME, "month"))) %>% count(month, name = "n") %>% arrange(month) %>% mutate(month_lab = factor(format(month, "%Y-%m"), levels = format(sort(unique(month)), "%Y-%m")))
        ggplot(mon, aes(x = month_lab, y = n)) + geom_col(fill = BAR_BLUE, alpha = 0.88, width = 0.7) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_comma(), expand = expansion(mult = c(0, 0.05))) + labs(title = "Monthly Admission", x = "Month (YYYY-MM)", y = "Admmission Count") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    output$plot_los <- renderPlot({
        d <- sheet3_data()
        if (is.null(d) || nrow(d) == 0 || all(is.na(d$length_of_stay))) {
            return(ggplot() + geom_text(aes(0.5, 0.5, label = "유효한 입원기간 데이터가 없습니다"), color = "grey50", size = 5) + theme_void())
        }
        los <- d$length_of_stay
        ok <- is.finite(los) & !is.na(los) & (los >= 0)
        los <- los[ok]
        if (!length(los)) {
            return(ggplot() + geom_text(aes(0.5, 0.5, label = "입원기간 데이터가 없습니다"), color = "grey50", size = 5) + theme_void())
        }
        brks <- c(-Inf, 10, 20, 30, 40, 50, Inf)
        labs <- c("0~10", "11~20", "21~30", "31~40", "41~50", "50+")
        grp <- cut(los, breaks = brks, right = TRUE, include.lowest = TRUE, labels = labs)
        df <- as.data.frame(table(grp), stringsAsFactors = FALSE)
        names(df) <- c("band", "n")
        df$band <- factor(df$band, levels = labs)
        ggplot(df, aes(x = band, y = n)) + geom_col(width = 0.85, fill = BAR_BLUE, alpha = 0.88, color = "white") + labs(title = "Days of Hospitalization", x = "Days", y = "Count") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    output$plot_gender <- renderPlot({
        d <- sheet3_data()
        if (is.null(d) || nrow(d) == 0 || !("sex" %in% names(d))) {
            return(ggplot() + geom_text(aes(0.5, 0.5, label = "'sex' 컬럼이 없습니다"), color = "grey50", size = 5) + theme_void())
        }
        g <- d %>% filter(!is.na(sex)) %>% mutate(sex_chr = toupper(trimws(as.character(sex)))) %>% mutate(label = dplyr::case_when(sex_chr %in% c("M", "MALE", "남", "남자", "1") ~ "Male", sex_chr %in% c("F", "FEMALE", "여", "여자", "2") ~ "Female", TRUE ~ "기타")) %>% count(label, name = "n") %>% mutate(pct = n/sum(n), pct_lab = scales::percent(pct, accuracy = 0.1), display = paste0(label, " ", scales::comma(n), " (", pct_lab, ")"))
        fill_map <- c(Male = BAR_BLUE, Female = LIGHT_BLUE, 기타 = "#B0B0B0")
        ggplot(g, aes(x = "", y = n, fill = label)) + geom_col(width = 1, color = "white") + coord_polar(theta = "y") + geom_text(aes(label = display), position = position_stack(vjust = 0.5), color = "black", size = 4) + scale_fill_manual(values = fill_map, guide = "none") + labs(title = "Gender Distribution", x = NULL, y = NULL) + theme_void(base_size = 12) + theme(plot.title = element_text(face = "bold", size = 15, color = "black"))
    })
    output$tbl_patient_info <- renderDT({
        data <- sheet3_data()
        if (nrow(data) == 0) {
            return(datatable(data.frame(메시지 = "Sheet3 데이터가 없습니다"), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip", columnDefs = list(list(className = "dt-center", targets = "_all"))), class = "display nowrap"))
        }
        data <- data %>% dplyr::select(-admit_year)
        if ("adm_type" %in% names(data)) {
            data <- data %>% mutate(adm_type = dplyr::recode(as.character(adm_type), I = "Inpatient", E = "Emergency", .default = adm_type))
        }
        data <- data %>% mutate(ADMTIME = as.Date(ADMTIME), DISCHTIME = as.Date(DISCHTIME), length_of_stay = as.integer(round(length_of_stay, 0)))
        front <- intersect(c("MDRP_NO", "PTNO"), names(data))
        order <- c(front, setdiff(names(data), front))
        data2 <- data[, order, drop = FALSE]
        pretty_names <- c(MDRP_NO = "Uniqe Number", PTNO = "Patient", ADMTIME = "Admission Date", DISCHTIME = "Discharge Date", sex = "Gender", ADM_DEPARTMENT = "Department", adm_type = "Admission Route", inhospital_expire = "Death", age = "Age", length_of_stay = "Days of Hospitalization", admit_year = "Admission Year")
        names(data2) <- ifelse(names(data2) %in% names(pretty_names), pretty_names[names(data2)], names(data2))
        datatable(data2, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip", columnDefs = list(list(className = "dt-center", targets = "_all"))), class = "display nowrap")
    })
    patients_main_content_ui <- function() {
        tagList(fluidRow(column(4, div(class = "card-box", h3("📅 Monthly Admission"), plotOutput("plot_yearly", height = "300px"))), column(4, div(class = "card-box", h3("📊 Days of Hospitalization"), plotOutput("plot_los", height = "300px"))), column(4, div(class = "card-box", h3("👥 Gender"), plotOutput("plot_gender", height = "300px")))), fluidRow(column(12, div(class = "card-box", h3("📋 Patient Data"), DTOutput("tbl_patient_info")))))
    }
    sheet1_applied <- reactiveVal(FALSE)
    sheet1_date_applied <- reactiveVal(NULL)
    sheet1_c1_applied <- reactiveVal(NULL)
    sheet1_c2_applied <- reactiveVal(NULL)
    sheet1_diagname_applied <- reactiveVal(NULL)
    observeEvent(list(input$sheet, input$file), {
        if (identical(input$sheet, "Sheet1")) {
            sheet1_applied(FALSE)
            sheet1_date_applied(NULL)
            sheet1_c1_applied(NULL)
            sheet1_c2_applied(NULL)
            sheet1_diagname_applied(NULL)
        }
    }, ignoreInit = TRUE)
    diagnosis_controls_ui <- function() {
        tagList(singleton(tags$head(tags$style(HTML(paste0(sidebar_css, fold_css))))), singleton(HTML(head_tools_js_css)), div(id = "s2_sidebar", div(class = "s2-scroll", div(class = "control-card", dateRangeInput("date", "📅 Date", start = NULL, end = NULL)), fold_panel(title = "🏥 Classification1", right = checkboxInput("sheet1_c1_all", "ALL", value = TRUE, width = "auto"), checkboxGroupInput("sheet1_c1", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), fold_panel(title = "🏥 Classification2", 
            right = checkboxInput("sheet1_c2_all", "ALL", value = TRUE, width = "auto"), checkboxGroupInput("sheet1_c2", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), fold_panel(title = "🏥️ Diagnosis", right = checkboxInput("sheet1_diag_all", "ALL", value = TRUE, width = "auto"), checkboxGroupInput("sheet1_diagname", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), div(class = "control-card", actionButton("sheet1_apply", "Calculation", 
            class = "btn btn-primary", style = "width:100%;font-weight:600;")))))
    }
    sheet1_base_df <- reactive({
        if (is.null(input$sheet) || !identical(input$sheet, "Sheet1")) 
            return(empty_diag_df())
        raw <- current_data()
        if (is.null(raw) || nrow(raw) == 0) 
            return(empty_diag_df())
        col_date <- pick_col(raw, c("DIAG_DATE", "diag_date", "진단일자", "진단일", "date"))
        col_name <- pick_col(raw, c("ICD_NAME", "ICD_Name", "icd_name", "진단명", "diag_name", "diagnosis_name"))
        col_c1 <- pick_col(raw, c("Classification1", "Classfication1", "classification1", "classfication1", "분류1"))
        col_c2 <- pick_col(raw, c("Classification2", "Classfication2", "classification2", "classfication2", "분류2"))
        col_mdrp <- pick_col(raw, c("MDRP_NO", "mdrp_no", "환자구분번호"))
        col_ptno <- pick_col(raw, c("PTNO", "ptno", "환자번호", "patient_id"))
        if (is.na(col_date) || is.na(col_name)) 
            return(empty_diag_df())
        tibble(DIAG_DATE = as.Date(parse_any_datetime(raw[[col_date]])), ICD_NAME = as.character(raw[[col_name]]), C1 = if (!is.na(col_c1)) 
            as.character(raw[[col_c1]])
        else NA_character_, C2 = if (!is.na(col_c2)) 
            as.character(raw[[col_c2]])
        else NA_character_, MDRP_NO = if (!is.na(col_mdrp)) 
            as.character(raw[[col_mdrp]])
        else NA_character_, PTNO = if (!is.na(col_ptno)) 
            as.character(raw[[col_ptno]])
        else NA_character_) %>% filter(!is.na(DIAG_DATE), nzchar(ICD_NAME))
    })
    observeEvent(list(input$sheet, current_data()), {
        req(identical(input$sheet, "Sheet1"))
        df <- current_data()
        req(!is.null(df), nrow(df) > 0)
        col_date <- pick_col(df, c("DIAG_DATE", "diag_date", "진단일자", "진단일", "date"))
        col_name <- pick_col(df, c("ICD_NAME", "ICD_Name", "icd_name", "진단명", "diag_name", "diagnosis_name"))
        if (is.na(col_date) || is.na(col_name)) 
            return()
        dt <- parse_any_datetime(df[[col_date]])
        rng <- range(as.Date(dt), na.rm = TRUE)
        updateDateRangeInput(session, "date", start = rng[1], end = rng[2])
        diag_choices <- sort(unique(na.omit(as.character(df[[col_name]]))))
        rv_choices$sheet1_diagname <- diag_choices
        updateCheckboxGroupInput(session, "sheet1_diagname", choices = diag_choices, selected = diag_choices)
        updateCheckboxInput(session, "sheet1_diag_all", value = length(diag_choices) > 0, label = "ALL")
    }, ignoreInit = TRUE)
    observeEvent(input$sheet, {
        if (identical(input$sheet, "Sheet1")) {
            diag_all_programmatic(TRUE)
            updateCheckboxInput(session, "sheet1_diag_all", TRUE)
        }
    }, ignoreInit = TRUE)
    observeEvent(input$sheet, {
        if (identical(input$sheet, "Sheet1")) {
            sheet1_diag_initialized(FALSE)
            updateCheckboxInput(session, "sheet1_diag_all", value = TRUE, label = "ALL")
        }
    }, ignoreInit = TRUE)
    observeEvent(input$date, {
        req(identical(input$sheet, "Sheet1"))
        df <- current_data()
        req(!is.null(df), nrow(df) > 0)
        col_date <- pick_col(df, c("DIAG_DATE", "diag_date", "진단일자", "진단일", "date"))
        col_c1 <- pick_col(df, c("Classification1", "Classfication1", "classification1", "classfication1", "분류1"))
        if (is.na(col_date)) {
            updateCheckboxGroupInput(session, "sheet1_c1", choices = character(0), selected = character(0))
            return()
        }
        dt <- parse_any_datetime(df[[col_date]])
        view <- tibble(DIAG_DATE = as.Date(dt), C1 = if (!is.na(col_c1)) 
            as.character(df[[col_c1]])
        else NA_character_) %>% filter(!is.na(DIAG_DATE))
        if (!is.null(input$date) && length(input$date) == 2 && all(!is.na(input$date))) {
            view <- filter(view, between(DIAG_DATE, as.Date(input$date[1]), as.Date(input$date[2])))
        }
        c1_choices <- if (!is.na(col_c1)) 
            sort(unique(na.omit(view$C1)))
        else character(0)
        keep <- function(prev, choices) {
            sel <- intersect(prev %||% character(0), choices)
            if (length(sel) == 0) 
                choices
            else sel
        }
        sel_c1 <- keep(input$sheet1_c1, c1_choices)
        updateCheckboxGroupInput(session, "sheet1_c1", choices = c1_choices, selected = keep(input$sheet1_c1, c1_choices))
        rv_choices$sheet1_c1 <- c1_choices
        prog_sheet1_c1_all(TRUE)
        updateCheckboxInput(session, "sheet1_c1_all", value = (length(c1_choices) > 0 && length(sel_c1) == length(c1_choices)))
    }, ignoreInit = TRUE)
    observeEvent(list(input$sheet1_c1, input$date), {
        req(identical(input$sheet, "Sheet1"))
        df <- current_data()
        req(!is.null(df), nrow(df) > 0)
        col_date <- pick_col(df, c("DIAG_DATE", "diag_date", "진단일자", "진단일", "date"))
        col_c1 <- pick_col(df, c("Classification1", "Classfication1", "classification1", "classfication1", "분류1"))
        col_c2 <- pick_col(df, c("Classification2", "Classfication2", "classification2", "classfication2", "분류2"))
        if (is.na(col_date)) 
            return()
        dt <- parse_any_datetime(df[[col_date]])
        view <- tibble::tibble(DIAG_DATE = as.Date(dt), C1 = if (!is.na(col_c1)) 
            as.character(df[[col_c1]])
        else NA_character_, C2 = if (!is.na(col_c2)) 
            as.character(df[[col_c2]])
        else NA_character_) %>% dplyr::filter(!is.na(DIAG_DATE))
        if (!is.null(input$date) && length(input$date) == 2 && all(!is.na(input$date))) {
            view <- dplyr::filter(view, dplyr::between(DIAG_DATE, as.Date(input$date[1]), as.Date(input$date[2])))
        }
        if (!is.na(col_c1) && length(input$sheet1_c1 %||% character(0)) > 0) {
            view <- dplyr::filter(view, C1 %in% input$sheet1_c1)
        }
        c2_choices <- if (!is.na(col_c2)) 
            sort(unique(stats::na.omit(view$C2)))
        else character(0)
        sel_c2 <- intersect(isolate(input$sheet1_c2) %||% character(0), c2_choices)
        if (!isTRUE(sheet1_c2_initialized())) {
            if (isTRUE(isolate(input$sheet1_c2_all))) {
                sel_c2 <- c2_choices
            }
            sheet1_c2_initialized(TRUE)
        }
        prog_sheet1_c2_all(TRUE)
        updateCheckboxGroupInput(session, "sheet1_c2", choices = c2_choices, selected = sel_c2)
        rv_choices$sheet1_c2 <- c2_choices
        updateCheckboxInput(session, "sheet1_c2_all", value = (length(c2_choices) > 0 && length(sel_c2) == length(c2_choices)))
    }, ignoreInit = TRUE)
    observeEvent(list(input$sheet, input$date, input$sheet1_c1, input$sheet1_c2), {
        req(identical(input$sheet, "Sheet1"))
        df <- current_data()
        req(!is.null(df), nrow(df) > 0)
        col_date <- pick_col(df, c("DIAG_DATE", "diag_date", "진단일자", "진단일", "date"))
        col_c1 <- pick_col(df, c("Classification1", "Classfication1", "classification1", "classfication1", "분류1"))
        col_c2 <- pick_col(df, c("Classification2", "Classfication2", "classification2", "classfication2", "분류2"))
        col_name <- pick_col(df, c("ICD_NAME", "ICD_Name", "icd_name", "진단명", "diag_name", "diagnosis_name"))
        if (is.na(col_date) || is.na(col_name)) {
            updateCheckboxGroupInput(session, "sheet1_diagname", choices = character(0), selected = character(0))
            updateCheckboxInput(session, "sheet1_diag_all", value = FALSE, label = "ALL")
            return()
        }
        dt <- parse_any_datetime(df[[col_date]])
        view <- dplyr::filter(tibble::tibble(DIAG_DATE = as.Date(dt), C1 = if (!is.na(col_c1)) 
            as.character(df[[col_c1]])
        else NA_character_, C2 = if (!is.na(col_c2)) 
            as.character(df[[col_c2]])
        else NA_character_, NAME = as.character(df[[col_name]])), !is.na(DIAG_DATE), nzchar(NAME))
        if (!is.null(input$date) && length(input$date) == 2 && all(!is.na(input$date))) {
            view <- dplyr::filter(view, dplyr::between(DIAG_DATE, as.Date(input$date[1]), as.Date(input$date[2])))
        }
        if (!is.na(col_c1) && length(input$sheet1_c1 %||% character(0)) > 0) 
            view <- dplyr::filter(view, C1 %in% input$sheet1_c1)
        if (!is.na(col_c2) && length(input$sheet1_c2 %||% character(0)) > 0) 
            view <- dplyr::filter(view, C2 %in% input$sheet1_c2)
        diag_choices <- sort(unique(stats::na.omit(view$NAME)))
        rv_choices$sheet1_diagname <- diag_choices
        prev_sel <- isolate(input$sheet1_diagname) %||% character(0)
        sel_diag <- intersect(prev_sel, diag_choices)
        if (!length(sel_diag) && length(diag_choices)) 
            sel_diag <- diag_choices
        freezeReactiveValue(input, "sheet1_diagname")
        updateCheckboxGroupInput(session, "sheet1_diagname", choices = diag_choices, selected = sel_diag)
        prog_sheet1_diag_all(TRUE)
        updateCheckboxInput(session, "sheet1_diag_all", value = length(diag_choices) > 0 && setequal(sel_diag, diag_choices), label = "ALL")
    }, ignoreInit = TRUE)
    observeEvent(input$sheet, {
        if (identical(input$sheet, "Sheet1")) {
            cur_choices <- isolate(rv_choices$sheet1_diagname %||% character(0))
            cur_sel <- isolate(input$sheet1_diagname %||% character(0))
            if (length(cur_choices) > 0 && (!length(cur_sel) || !all(cur_sel %in% cur_choices))) {
                updateCheckboxGroupInput(session, "sheet1_diagname", choices = cur_choices, selected = cur_choices)
                updateCheckboxInput(session, "sheet1_diag_all", value = TRUE, label = "ALL")
            }
            else {
                updateCheckboxInput(session, "sheet1_diag_all", value = length(cur_choices) > 0 && setequal(cur_sel, cur_choices), label = "ALL")
            }
        }
    }, ignoreInit = TRUE)
    observeEvent(input$sheet1_c1_all, ignoreInit = TRUE, {
        if (isTRUE(prog_sheet1_c1_all())) {
            prog_sheet1_c1_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$sheet1_c1_all)) 
            isolate(rv_choices$sheet1_c1 %||% character(0))
        else character(0)
        updateCheckboxGroupInput(session, "sheet1_c1", selected = sel)
    })
    observeEvent(input$sheet1_c2_all, ignoreInit = TRUE, {
        if (isTRUE(prog_sheet1_c2_all())) {
            prog_sheet1_c2_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$sheet1_c2_all)) 
            isolate(rv_choices$sheet1_c2 %||% character(0))
        else character(0)
        updateCheckboxGroupInput(session, "sheet1_c2", selected = sel)
    })
    observeEvent(input$sheet1_diag_all, ignoreInit = TRUE, {
        if (isTRUE(prog_sheet1_diag_all())) {
            prog_sheet1_diag_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$sheet1_diag_all)) 
            isolate(rv_choices$sheet1_diagname %||% character(0))
        else character(0)
        updateCheckboxGroupInput(session, "sheet1_diagname", selected = sel)
    })
    observeEvent(input$sheet1_c1, ignoreInit = TRUE, {
        choices <- isolate(rv_choices$sheet1_c1 %||% character(0))
        sel <- input$sheet1_c1 %||% character(0)
        all_on <- length(choices) > 0 && setequal(sel, choices)
        if (!identical(isTRUE(input$sheet1_c1_all), all_on)) {
            prog_sheet1_c1_all(TRUE)
            updateCheckboxInput(session, "sheet1_c1_all", value = all_on)
        }
    })
    observeEvent(input$sheet1_c2, ignoreInit = TRUE, {
        choices <- isolate(rv_choices$sheet1_c2 %||% character(0))
        sel <- input$sheet1_c2 %||% character(0)
        all_on <- length(choices) > 0 && setequal(sel, choices)
        if (!identical(isTRUE(input$sheet1_c2_all), all_on)) {
            prog_sheet1_c2_all(TRUE)
            updateCheckboxInput(session, "sheet1_c2_all", value = all_on)
        }
    })
    observeEvent(input$sheet1_diagname, ignoreInit = TRUE, {
        choices <- isolate(rv_choices$sheet1_diagname %||% character(0))
        sel <- input$sheet1_diagname %||% character(0)
        all_on <- length(choices) > 0 && setequal(sel, choices)
        if (!identical(isTRUE(input$sheet1_diag_all), all_on)) {
            prog_sheet1_diag_all(TRUE)
            updateCheckboxInput(session, "sheet1_diag_all", value = all_on)
        }
    })
    observeEvent(input$sheet1_apply, ignoreInit = TRUE, {
        sheet1_date_applied(as.Date(input$date))
        sheet1_c1_applied(input$sheet1_c1)
        sheet1_c2_applied(input$sheet1_c2)
        sheet1_diagname_applied(input$sheet1_diagname)
        sheet1_applied(TRUE)
    })
    sheet1_df_applied <- reactive({
        if (!isTRUE(sheet1_applied())) 
            return(empty_diag_df())
        d <- sheet1_base_df()
        dr <- sheet1_date_applied()
        if (!is.null(dr) && length(dr) == 2 && all(!is.na(dr))) 
            d <- d %>% filter(between(DIAG_DATE, dr[1], dr[2]))
        sel_c1 <- sheet1_c1_applied()
        sel_c2 <- sheet1_c2_applied()
        sel_dn <- sheet1_diagname_applied()
        if (!is.null(sel_c1) && length(sel_c1) > 0 && "C1" %in% names(d)) 
            d <- d %>% filter(C1 %in% sel_c1)
        if (!is.null(sel_c2) && length(sel_c2) > 0 && "C2" %in% names(d)) 
            d <- d %>% filter(C2 %in% sel_c2)
        if (!is.null(sel_dn) && length(sel_dn) > 0) 
            d <- d %>% filter(ICD_NAME %in% sel_dn)
        d
    })
    sheet1_df_applied_nodiag <- reactive({
        if (!isTRUE(sheet1_applied())) 
            return(empty_diag_df())
        d <- sheet1_base_df()
        dr <- sheet1_date_applied()
        if (!is.null(dr) && length(dr) == 2 && all(!is.na(dr))) 
            d <- d %>% filter(between(DIAG_DATE, dr[1], dr[2]))
        sel_c1 <- sheet1_c1_applied()
        sel_c2 <- sheet1_c2_applied()
        if (!is.null(sel_c1) && length(sel_c1) > 0 && "C1" %in% names(d)) 
            d <- d %>% filter(C1 %in% sel_c1)
        if (!is.null(sel_c2) && length(sel_c2) > 0 && "C2" %in% names(d)) 
            d <- d %>% filter(C2 %in% sel_c2)
        d
    })
    sheet1_diag_data <- reactive({
        sheet1_df_applied() %>% select(DIAG_DATE, ICD_NAME)
    })
    sheet1_diag_date_only <- reactive({
        sheet1_df_applied_nodiag() %>% select(DIAG_DATE, ICD_NAME)
    })
    sheet1_filter_keys <- reactive({
        d <- sheet1_df_applied()
        if (nrow(d) == 0) 
            return(NULL)
        list(mdrp = unique(na.omit(d$MDRP_NO)), ptno = unique(na.omit(d$PTNO)))
    })
    sheet1_wait_plot <- function(msg = "") {
        ggplot() + geom_text(aes(0.5, 0.5, label = msg), colour = "grey50", size = 5) + theme_void()
    }
    output$sheet1_plot_diag_date <- renderPlot({
        if (!isTRUE(sheet1_applied())) 
            return(sheet1_wait_plot())
        d <- sheet1_diag_data()
        if (nrow(d) == 0) 
            return(sheet1_wait_plot("표시할 진단 데이터가 없습니다"))
        ts <- d %>% mutate(month = floor_date(DIAG_DATE, "month")) %>% count(month, name = "n") %>% arrange(month) %>% mutate(month_lab = factor(format(month, "%Y-%m"), levels = format(sort(unique(month)), "%Y-%m")))
        ggplot(ts, aes(x = month_lab, y = n)) + geom_col(fill = BAR_BLUE, alpha = 0.88, width = 0.7) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_comma(), expand = expansion(mult = c(0, 0.05))) + labs(title = "", x = "Month (YYYY-MM)", y = "Count") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    sheet1_top20_icdname <- reactive({
        if (!isTRUE(sheet1_applied())) 
            return(tibble())
        d <- sheet1_df_applied_nodiag()
        if (nrow(d) == 0) 
            return(tibble())
        d %>% mutate(label = str_squish(ICD_NAME)) %>% count(label, sort = TRUE, name = "n") %>% slice_head(n = 20) %>% arrange(n) %>% mutate(wrapped = str_wrap(label, 28))
    })
    output$sheet1_plot_icd_code <- renderPlot({
        if (!isTRUE(sheet1_applied())) 
            return(sheet1_wait_plot())
        topn <- sheet1_top20_icdname()
        if (nrow(topn) == 0) 
            return(sheet1_wait_plot("표시할 진단 데이터가 없습니다"))
        ggplot(topn, aes(x = reorder(label, n), y = n)) + geom_col(fill = BAR_BLUE, alpha = 0.9, width = 0.7) + coord_flip() + scale_y_continuous(breaks = breaks_pretty(6), labels = label_comma(), expand = expansion(mult = c(0, 0.05))) + labs(title = "Top 20 Diagnosis", x = "Diagnosis", y = "Count") + theme_sheet3_blue()
    }, height = function() 520)
    output$sheet1_plot_dept <- renderPlot({
        if (!isTRUE(sheet1_applied())) 
            return(sheet1_wait_plot())
        s3 <- sheet3_data()
        if (is.null(s3) || nrow(s3) == 0) 
            return(sheet1_wait_plot("Sheet3 데이터가 없습니다"))
        if (!("ADM_DEPARTMENT" %in% names(s3))) 
            return(sheet1_wait_plot("Sheet3에 ADM_DEPARTMENT 컬럼이 없습니다"))
        df <- s3 %>% mutate(ADM_DEPARTMENT = str_squish(as.character(ADM_DEPARTMENT)), ADM_DEPARTMENT = ifelse(!nzchar(ADM_DEPARTMENT), "(미기록)", ADM_DEPARTMENT))
        keys <- sheet1_filter_keys()
        if (!is.null(keys)) {
            use_mdrp <- ("MDRP_NO" %in% names(df)) && length(keys$mdrp) > 0
            use_ptno <- ("PTNO" %in% names(df)) && length(keys$ptno) > 0
            if (use_mdrp || use_ptno) 
                df <- df %>% filter((use_mdrp & MDRP_NO %in% keys$mdrp) | (use_ptno & PTNO %in% keys$ptno))
        }
        if (nrow(df) == 0) 
            return(sheet1_wait_plot("선택 조건에 해당하는 환자 행이 없습니다"))
        top <- df %>% count(ADM_DEPARTMENT, name = "n", sort = TRUE) %>% arrange(n)
        ggplot(top, aes(x = reorder(ADM_DEPARTMENT, n), y = n)) + geom_col(fill = BAR_BLUE, alpha = 0.9, width = 0.7) + coord_flip() + scale_y_continuous(breaks = breaks_pretty(6), labels = label_comma(), expand = expansion(mult = c(0, 0.05))) + labs(title = "", x = "Department", y = "Count") + theme_sheet3_blue()
    })
    diagnosis_main_content_ui <- function() {
        tagList(fluidRow(column(6, div(class = "card-box", h3("📅 Diagnosis Date (Monthly)"), plotOutput("sheet1_plot_diag_date", height = 320))), column(6, div(class = "card-box", h3("🏥 Department Distribution"), plotOutput("sheet1_plot_dept", height = 320)))), fluidRow(column(12, div(class = "card-box", h3("🏷️ Top 20 Diagnosis"), plotOutput("sheet1_plot_icd_code", height = 520)))))
    }
    s2_initial_load <- reactiveVal(TRUE)
    s2_select_all_on_entry <- reactiveVal(FALSE)
    s2_locked_group_key <- reactiveVal("total")
    s2_locked_picked <- reactiveVal(NULL)
    s2_distinct_palette <- function(n) {
        base <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#0568AE", "#D55E00", "#CC79A7", "#000000")
        if (n <= length(base)) 
            return(base[seq_len(n)])
        extra_n <- n - length(base)
        extra <- grDevices::hcl(h = seq(15, 375, length.out = extra_n + 1)[1:extra_n], c = 100, l = 60)
        c(base, extra)
    }
    s2_group_colors <- function(groups, group_key = "class") {
        g <- as.character(groups)
        cols <- s2_distinct_palette(length(g))
        names(cols) <- g
        if (identical(group_key, "class")) {
            if ("2" %in% g) 
                cols["2"] <- "#E41A1C"
            if ("3" %in% g) 
                cols["3"] <- "#FF7F00"
        }
        cols
    }
    s2_data <- reactive({
        if (!file_uploaded() || is.null(input$file)) 
            return(NULL)
        sheets_list <- tryCatch(excel_sheets(input$file$datapath), error = function(e) character(0))
        sh_abx <- resolve_abx_sheet(sheets_list)
        sh_pd <- resolve_pd_sheet(sheets_list)
        if (is.na(sh_abx) || is.na(sh_pd)) {
            showNotification("Sheet2: 항생제/입원일수 시트를 찾을 수 없습니다.", type = "error")
            return(NULL)
        }
        abx_raw <- tryCatch(read_excel(input$file$datapath, sheet = sh_abx), error = function(e) {
            showNotification(paste("항생제 시트 읽기 오류:", e$message), "error")
            NULL
        })
        pd_raw <- tryCatch(read_excel(input$file$datapath, sheet = sh_pd), error = function(e) {
            showNotification(paste("입원일수 시트 읽기 오류:", e$message), "error")
            NULL
        })
        if (is.null(abx_raw) || is.null(pd_raw)) 
            return(NULL)
        abx_std <- tryCatch(standardize_abx(abx_raw), error = function(e) {
            showNotification(paste("항생제 표준화 오류:", e$message), "error")
            NULL
        })
        col_ingr <- pick_col(abx_raw, c("ingredient", "INGREDIENT", "Generic name", "generic_name", "GENERIC NAME", "성분명", "성분"))
        col_route <- pick_col(abx_raw, c("route", "ROUTE", "adm_route", "Adm.R", "ADM.R", "Administration route", "투여경로", "경로"))
        col_form <- pick_col(abx_raw, c("form", "FORM", "제형", "dosage_form", "Dosage form"))
        col_date <- pick_col(abx_raw, c("date", "DATE", "투여일", "투여일자", "투여일시", "administration_date", "AdministrationDate"))
        col_pid <- pick_col(abx_raw, c("patient_id", "PATIENT_ID", "PTNO", "ptno", "환자번호"))
        col_atc <- pick_col(abx_raw, c("atc_code", "ATC_CODE", "ATC", "ATCCODE", "ATC 코드"))
        if (!"ingredient" %in% names(abx_std)) 
            abx_std$ingredient <- if (!is.na(col_ingr)) 
                as.character(abx_raw[[col_ingr]])
            else NA_character_
        if (!"route" %in% names(abx_std)) 
            abx_std$route <- if (!is.na(col_route)) 
                as.character(abx_raw[[col_route]])
            else NA_character_
        if (!"form" %in% names(abx_std)) 
            abx_std$form <- if (!is.na(col_form)) 
                as.character(abx_raw[[col_form]])
            else NA_character_
        if (!"date" %in% names(abx_std) && !is.na(col_date)) 
            abx_std$date <- as.Date(parse_any_datetime(abx_raw[[col_date]]))
        if (!"patient_id" %in% names(abx_std) && !is.na(col_pid)) 
            abx_std$patient_id <- as.character(abx_raw[[col_pid]])
        if (!"atc_code" %in% names(abx_std) && !is.na(col_atc)) 
            abx_std$atc_code <- as.character(abx_raw[[col_atc]])
        if ("route" %in% names(abx_std)) {
            abx_std$route <- dplyr::recode(as.character(abx_std$route), O = "Oral", o = "Oral", P = "Parenteral", p = "Parenteral", .default = as.character(abx_std$route))
        }
        pd_std <- tryCatch(standardize_pd(pd_raw), error = function(e) {
            showNotification(paste("입원일수 표준화 오류:", e$message), "error")
            NULL
        })
        if (is.null(abx_std) || is.null(pd_std)) 
            return(NULL)
        abx_std <- if (inherits(abx_std, "data.frame")) {
            tibble::as_tibble(abx_std)
        }
        else if (is.list(abx_std) && length(abx_std) == 1 && inherits(abx_std[[1]], "data.frame")) {
            tibble::as_tibble(abx_std[[1]])
        }
        else {
            tibble::as_tibble(abx_std)
        }
        pd_std <- if (inherits(pd_std, "data.frame")) {
            tibble::as_tibble(pd_std)
        }
        else if (is.list(pd_std) && length(pd_std) == 1 && inherits(pd_std[[1]], "data.frame")) {
            tibble::as_tibble(pd_std[[1]])
        }
        else {
            tibble::as_tibble(pd_std)
        }
        if (!"month" %in% names(pd_std)) 
            pd_std$month <- character()
        if (!"patient_days" %in% names(pd_std)) 
            pd_std$patient_days <- numeric()
        pd_std$month <- as.character(pd_std$month)
        pd_std$patient_days <- suppressWarnings(as.numeric(pd_std$patient_days))
        col_class <- pick_col(abx_raw, c("class", "계열", "계열분류", "drug_class", "계열코드"))
        col_age <- pick_col(abx_raw, c("age", "연령"))
        col_mdrp <- pick_col(abx_raw, c("MDRP_NO", "mdrp_no", "환자구분번호"))
        col_ptno <- pick_col(abx_raw, c("PTNO", "ptno", "환자번호", "patient_id"))
        abx_std$class <- if (!is.na(col_class)) 
            as.character(abx_raw[[col_class]])
        else NA_character_
        abx_std$age <- if (!is.na(col_age)) 
            as.numeric(abx_raw[[col_age]])
        else NA_real_
        abx_std$MDRP_NO <- if (!is.na(col_mdrp)) 
            as.character(abx_raw[[col_mdrp]])
        else NA_character_
        abx_std$PTNO <- if (!is.na(col_ptno)) 
            as.character(abx_raw[[col_ptno]])
        else NA_character_
        sh_diag <- if ("Sheet1" %in% sheets_list) 
            "Sheet1"
        else NA_character_
        diag_map <- NULL
        if (!is.na(sh_diag)) {
            diag_raw <- tryCatch(read_excel(input$file$datapath, sheet = sh_diag), error = function(e) {
                showNotification(paste("Sheet1 읽기 오류:", e$message), "error")
                NULL
            })
            if (!is.null(diag_raw)) {
                d_mdrp <- pick_col(diag_raw, c("MDRP_NO", "mdrp_no", "환자구분번호"))
                d_ptno <- pick_col(diag_raw, c("PTNO", "ptno", "환자번호", "patient_id"))
                d_c1 <- pick_col(diag_raw, c("Classification1", "Classfication1", "classification1", "classfication1", "분류1"))
                d_c2 <- pick_col(diag_raw, c("Classification2", "Classfication2", "classification2", "classfication2", "분류2"))
                if (!is.na(d_mdrp) && !is.na(d_ptno)) {
                  diag_map <- dplyr::filter(tibble::tibble(MDRP_NO = as.character(diag_raw[[d_mdrp]]), PTNO = as.character(diag_raw[[d_ptno]]), C1 = if (!is.na(d_c1)) 
                    as.character(diag_raw[[d_c1]])
                  else NA_character_, C2 = if (!is.na(d_c2)) 
                    as.character(diag_raw[[d_c2]])
                  else NA_character_), !is.na(MDRP_NO), !is.na(PTNO))
                }
            }
        }
        sh_s3 <- if ("Sheet3" %in% sheets_list) 
            "Sheet3"
        else NA_character_
        dept_map <- NULL
        if (!is.na(sh_s3)) {
            s3_raw <- tryCatch(read_excel(input$file$datapath, sheet = sh_s3), error = function(e) {
                showNotification(paste("Sheet3 읽기 오류:", e$message), "error")
                NULL
            })
            if (!is.null(s3_raw)) {
                s_mdrp <- pick_col(s3_raw, c("MDRP_NO", "mdrp_no", "환자구분번호"))
                s_ptno <- pick_col(s3_raw, c("PTNO", "ptno", "환자번호", "patient_id"))
                s_dept <- pick_col(s3_raw, c("ADM_DEPARTMENT", "adm_department", "department", "진료과", "입원부서"))
                if (!is.na(s_mdrp) && !is.na(s_ptno) && !is.na(s_dept)) {
                  dept_map <- dplyr::filter(tibble::tibble(MDRP_NO = as.character(s3_raw[[s_mdrp]]), PTNO = as.character(s3_raw[[s_ptno]]), DEPT = as.character(s3_raw[[s_dept]])), !is.na(MDRP_NO), !is.na(PTNO), nzchar(DEPT))
                }
            }
        }
        age_map <- NULL
        if (!is.na(sh_s3) && !is.null(s3_raw)) {
            s_age <- pick_col(s3_raw, c("age", "연령"))
            if (!is.na(s_mdrp) && !is.na(s_ptno) && !is.na(s_age)) {
                age_map <- dplyr::filter(tibble::tibble(MDRP_NO = as.character(s3_raw[[s_mdrp]]), PTNO = as.character(s3_raw[[s_ptno]]), age_s3 = as.numeric(s3_raw[[s_age]])), !is.na(MDRP_NO), !is.na(PTNO))
            }
        }
        if (!is.null(age_map)) {
            age_map <- tibble::as_tibble(age_map)
            abx_std <- abx_std %>% dplyr::left_join(age_map, by = c("MDRP_NO", "PTNO")) %>% dplyr::mutate(age = dplyr::coalesce(as.numeric(.data$age), as.numeric(.data$age_s3))) %>% dplyr::select(-age_s3)
        }
        meta <- list(classes = sort(unique(stats::na.omit(abx_std$class))), ingredients = sort(unique(stats::na.omit(abx_std$ingredient))), routes = sort(unique(stats::na.omit(abx_std$route))), age_range = {
            r <- range(abx_std$age, na.rm = TRUE)
            if (any(!is.finite(r))) NULL else r
        }, date_range = {
            r <- range(abx_std$date, na.rm = TRUE)
            if (length(r) != 2 || any(!is.finite(r))) c(NA, NA) else r
        })
        list(abx = abx_std, pd = pd_std, meta = meta, diag_map = diag_map, dept_map = dept_map)
    })
    s2_date_initialized <- reactiveVal(FALSE)
    observeEvent(input$file, {
        s2_date_initialized(FALSE)
    }, ignoreInit = TRUE)
    observeEvent(input$sheet, {
        if (identical(input$sheet, "Sheet2")) 
            s2_date_initialized(FALSE)
    }, ignoreInit = TRUE)
    observeEvent(list(s2_data(), input$sheet), {
        if (!identical(input$sheet, "Sheet2")) 
            return()
        d <- s2_data()
        req(!is.null(d))
        dr <- d$meta$date_range
        if (length(dr) == 2 && all(!is.na(dr)) && !isTRUE(s2_date_initialized())) {
            freezeReactiveValue(input, "s2_date")
            updateDateRangeInput(session, "s2_date", start = dr[1], end = dr[2], min = dr[1], max = dr[2])
            s2_date_initialized(TRUE)
        }
    }, ignoreInit = FALSE)
    observeEvent(input$s2_date, ignoreInit = TRUE, {
        s2_update_choices()
    })
    s2_controls_ui <- function() {
        tagList(singleton(tags$head(tags$style(HTML(paste0(sidebar_css, fold_css))))), singleton(HTML(head_tools_js_css)), div(id = "s2_sidebar", div(class = "s2-scroll", div(class = "control-card", dateRangeInput("s2_date", "📅 Date", start = NULL, end = NULL)), fold_panel(title = "🏥 Department", right = checkboxInput("s2_dept_all", "ALL", value = TRUE, width = "auto"), checkboxGroupInput("s2_pick_dept", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), conditionalPanel(condition = "input.s2_metric_tab == 'DOT'", 
            fold_panel(title = "🧒 Age", radioButtons("s2_age_group", label = NULL, choices = c(전체 = "All", `Adult(≥15)` = "Adult", `Pediatric(<15)` = "Pediatric"), selected = "All", inline = FALSE), open = FALSE)), conditionalPanel(condition = "input.s2_metric_tab == 'DDD'", fold_panel(title = "🧒 Age", radioButtons("s2_age_group_ddd", label = NULL, choices = c(`Adult(≥15)` = "Adult15+"), selected = "Adult15+", inline = FALSE), open = FALSE)), fold_panel(title = "🏷️ Classification1", 
            right = checkboxInput("s2_c1_all", "ALL", value = TRUE, width = "auto"), checkboxGroupInput("s2_pick_c1", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), fold_panel(title = "🏷️ Classification2", right = checkboxInput("s2_c2_all", "ALL", value = TRUE, width = "auto"), checkboxGroupInput("s2_pick_c2", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), fold_panel(title = "🧪 Classification", right = checkboxInput("s2_class_all", 
            "ALL", value = TRUE, width = "auto"), checkboxGroupInput("s2_pick_class", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), fold_panel(title = "💊 Generic name", right = checkboxInput("s2_ingr_all", "ALL", value = TRUE, width = "auto"), checkboxGroupInput("s2_pick_ingr", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), fold_panel(title = "💉 Administration route", right = checkboxInput("s2_route_all", "ALL", value = TRUE, 
            width = "auto"), checkboxGroupInput("s2_pick_route", label = NULL, choices = NULL, selected = NULL, inline = FALSE), open = FALSE), div(class = "control-card", actionButton("s2_run", "🔁 Calculation", class = "btn btn-primary", style = "width:100%;font-weight:600;")))))
    }
    s2_main_content_ui <- function() {
        div(class = "s2-narrow", tabsetPanel(id = "s2_metric_tab", tabPanel("DOT", fluidRow(column(12, div(class = "card-box card-pad", h4("Overall DOT Trend"), plotOutput("s2_plot_overall_dot", height = 320)))), fluidRow(column(12, div(class = "card-box card-pad", h4("Overall DOT Trend by Selected Antimicrobial Class"), plotOutput("s2_plot_filtered_dot", height = 320)))), fluidRow(column(12, div(class = "card-box card-pad", h4("Overall DOT Trend by Selected Generic Name"), plotOutput("s2_plot_filtered_ingr_dot", 
            height = 500)))), fluidRow(column(12, div(class = "card-box card-pad", h4("Summary Table of DOT (Month)"), DTOutput("s2_tbl_dot")))), fluidRow(column(12, div(class = "card-box card-pad", h4("Summary Table of DOT (Year)"), DTOutput("s2_tbl_agg_dot"))))), tabPanel("DDD", fluidRow(column(12, div(class = "card-box card-pad", h4("Overall DDD Trend"), plotOutput("s2_plot_overall_ddd", height = 320)))), fluidRow(column(12, div(class = "card-box card-pad", h4("Overall DDD Trend by Selected Antimicrobial Class"), 
            plotOutput("s2_plot_filtered_ddd", height = 320)))), fluidRow(column(12, div(class = "card-box card-pad", h4("Overall DDD Trend by Selected Generic Name"), plotOutput("s2_plot_filtered_ingr_ddd", height = 500)))), fluidRow(column(12, div(class = "card-box card-pad", h4("Summary Table of DDD (Month)"), DTOutput("s2_tbl_ddd")))), fluidRow(column(12, div(class = "card-box card-pad", h4("Summary Table of DDD (Year)"), DTOutput("s2_tbl_agg_ddd")))))))
    }
    s2_current_group_key <- reactive({
        if (length(input$s2_pick_class %||% character(0)) > 0) 
            "class"
        else if (length(input$s2_pick_ingr %||% character(0)) > 0) 
            "ingr"
        else if (length(input$s2_pick_route %||% character(0)) > 0) 
            "route"
        else "total"
    })
    s2_picked_by_group <- reactive({
        switch(s2_current_group_key(), class = input$s2_pick_class, ingr = input$s2_pick_ingr, route = input$s2_pick_route, NULL)
    })
    s2_base_abx <- reactive({
        d <- s2_data()
        if (is.null(d)) 
            return(tibble())
        abx <- d$abx
        if (!is.null(input$s2_date) && length(input$s2_date) == 2 && all(!is.na(input$s2_date))) {
            abx <- abx %>% dplyr::filter(date >= as.Date(input$s2_date[1]), date <= as.Date(input$s2_date[2]))
        }
        abx
    })
    s2_update_choices <- function() {
        if (!begin_batch_update(s2_updating)) 
            return(invisible(NULL))
        d <- s2_data()
        if (is.null(d)) 
            return(invisible(NULL))
        abx_date <- s2_base_abx()
        if (!nrow(abx_date)) {
            safe_update_group("s2_pick_dept", "s2_dept", character(0), character(0))
            safe_update_group("s2_pick_c1", "s2_c1", character(0), character(0))
            safe_update_group("s2_pick_c2", "s2_c2", character(0), character(0))
            safe_update_group("s2_pick_class", "s2_class", character(0), character(0))
            safe_update_group("s2_pick_ingr", "s2_ingr", character(0), character(0))
            safe_update_group("s2_pick_route", "s2_route", character(0), character(0))
            s2_initial_load(FALSE)
            return(invisible(NULL))
        }
        getv <- function(df, nm) if (nm %in% names(df)) 
            df[[nm]]
        else NULL
        keys_abx <- abx_date %>% dplyr::transmute(MDRP_NO = as.character(MDRP_NO), PTNO = as.character(PTNO)) %>% dplyr::filter(!is.na(MDRP_NO), !is.na(PTNO)) %>% dplyr::distinct()
        dept_map <- d$dept_map
        dept_choices <- if (!is.null(dept_map)) {
            dept_map %>% dplyr::semi_join(keys_abx, by = c("MDRP_NO", "PTNO")) %>% dplyr::distinct(DEPT) %>% dplyr::pull(DEPT) %>% stats::na.omit() %>% unique() %>% sort()
        }
        else character(0)
        sel_dept <- if (isTRUE(s2_initial_load())) 
            dept_choices
        else intersect(input$s2_pick_dept %||% character(0), dept_choices)
        safe_update_group("s2_pick_dept", "s2_dept", dept_choices, sel_dept)
        keys_after_dept <- keys_abx
        if (length(sel_dept) > 0 && !is.null(dept_map)) {
            keys_after_dept <- keys_abx %>% dplyr::semi_join(dept_map %>% dplyr::filter(DEPT %in% sel_dept), by = c("MDRP_NO", "PTNO"))
        }
        metric <- input$s2_metric_tab %||% "DOT"
        abx_after_dept <- abx_date %>% dplyr::semi_join(keys_after_dept, by = c("MDRP_NO", "PTNO"))
        if (identical(metric, "DOT")) {
            age_pick <- input$s2_age_group %||% "All"
            if (has_valid_age(abx_after_dept)) {
                if (identical(age_pick, "Adult")) 
                  abx_after_dept <- dplyr::filter(abx_after_dept, age >= 15)
                else if (identical(age_pick, "Pediatric")) 
                  abx_after_dept <- dplyr::filter(abx_after_dept, age < 15)
            }
        }
        else {
            if (has_valid_age(abx_after_dept)) {
                abx_after_dept <- dplyr::filter(abx_after_dept, age >= 15)
            }
        }
        abx_for_choices <- abx_after_dept
        if (!nrow(abx_for_choices)) {
            abx_for_choices <- abx_date %>% dplyr::semi_join(keys_after_dept, by = c("MDRP_NO", "PTNO"))
        }
        keys_after_age <- abx_for_choices %>% dplyr::transmute(MDRP_NO = as.character(MDRP_NO), PTNO = as.character(PTNO)) %>% dplyr::filter(!is.na(MDRP_NO), !is.na(PTNO)) %>% dplyr::distinct()
        diag_map <- d$diag_map
        c1_choices <- if (!is.null(diag_map)) {
            diag_map %>% dplyr::semi_join(keys_after_age, by = c("MDRP_NO", "PTNO")) %>% dplyr::distinct(C1) %>% dplyr::pull(C1) %>% stats::na.omit() %>% unique() %>% sort()
        }
        else character(0)
        sel_c1 <- if (isTRUE(s2_initial_load())) 
            c1_choices
        else intersect(input$s2_pick_c1 %||% character(0), c1_choices)
        safe_update_group("s2_pick_c1", "s2_c1", c1_choices, sel_c1)
        keys_after_c1 <- keys_after_age
        if (length(sel_c1) > 0 && !is.null(diag_map)) {
            keys_after_c1 <- keys_after_age %>% dplyr::semi_join(diag_map %>% dplyr::filter(C1 %in% sel_c1), by = c("MDRP_NO", "PTNO"))
        }
        c2_choices <- if (!is.null(diag_map)) {
            diag_map %>% dplyr::semi_join(keys_after_c1, by = c("MDRP_NO", "PTNO")) %>% dplyr::distinct(C2) %>% dplyr::pull(C2) %>% stats::na.omit() %>% unique() %>% sort()
        }
        else character(0)
        sel_c2 <- if (isTRUE(s2_initial_load())) 
            c2_choices
        else intersect(input$s2_pick_c2 %||% character(0), c2_choices)
        safe_update_group("s2_pick_c2", "s2_c2", c2_choices, sel_c2)
        keys_after_c2 <- keys_after_c1
        if (length(sel_c2) > 0 && !is.null(diag_map)) {
            keys_after_c2 <- keys_after_c1 %>% dplyr::semi_join(diag_map %>% dplyr::filter(C2 %in% sel_c2), by = c("MDRP_NO", "PTNO"))
        }
        abx_k <- abx_for_choices %>% dplyr::semi_join(keys_after_c2, by = c("MDRP_NO", "PTNO"))
        classes <- sort(unique(stats::na.omit(getv(abx_k, "class")))) %||% character(0)
        sel_cls <- if (isTRUE(s2_initial_load())) 
            classes
        else intersect(input$s2_pick_class %||% character(0), classes)
        safe_update_group("s2_pick_class", "s2_class", classes, sel_cls)
        abx_ingr <- if ("class" %in% names(abx_k) && length(sel_cls) > 0) 
            dplyr::filter(abx_k, class %in% sel_cls)
        else abx_k
        ingrs <- sort(unique(stats::na.omit(getv(abx_ingr, "ingredient")))) %||% character(0)
        sel_ingr <- if (isTRUE(s2_initial_load())) 
            ingrs
        else intersect(input$s2_pick_ingr %||% character(0), ingrs)
        safe_update_group("s2_pick_ingr", "s2_ingr", ingrs, sel_ingr)
        abx_route <- if ("ingredient" %in% names(abx_ingr) && length(sel_ingr) > 0) 
            dplyr::filter(abx_ingr, ingredient %in% sel_ingr)
        else abx_ingr
        routes <- sort(unique(stats::na.omit(getv(abx_route, "route")))) %||% character(0)
        sel_route <- if (isTRUE(s2_initial_load())) 
            routes
        else intersect(input$s2_pick_route %||% character(0), routes)
        safe_update_group("s2_pick_route", "s2_route", routes, sel_route)
        s2_initial_load(FALSE)
    }
    s2_all_range <- reactiveVal(NULL)
    observeEvent(s2_data(), {
        d <- s2_data()
        if (is.null(d)) 
            return()
        dr <- d$meta$date_range
        if (length(dr) == 2 && all(!is.na(dr))) 
            s2_all_range(dr)
        if (identical(input$sheet, "Sheet2")) 
            s2_initial_load(TRUE)
    }, ignoreInit = TRUE)
    observeEvent(input$s2_date, {
        dr <- s2_all_range()
        req(!is.null(dr))
        cur <- as.Date(input$s2_date)
        req(length(cur) == 2)
        s_new <- if (is.na(cur[1])) 
            dr[1]
        else cur[1]
        e_new <- if (is.na(cur[2])) 
            dr[2]
        else cur[2]
        s_new <- max(min(s_new, dr[2]), dr[1])
        e_new <- max(min(e_new, dr[2]), dr[1])
        if (s_new > e_new) 
            e_new <- s_new
        cur_now <- as.Date(input$s2_date)
        if (!identical(cur_now[1], s_new) || !identical(cur_now[2], e_new)) {
            updateDateRangeInput(session, "s2_date", start = s_new, end = e_new)
        }
    }, ignoreInit = TRUE)
    observeEvent(list(s2_data(), input$s2_date), ignoreInit = TRUE, {
        if (!isTRUE(s2_updating())) 
            s2_update_choices()
    })
    observeEvent(input$s2_pick_class, ignoreInit = TRUE, {
        if (!isTRUE(s2_updating())) 
            s2_update_choices()
    })
    observeEvent(input$s2_pick_ingr, ignoreInit = TRUE, {
        if (!isTRUE(s2_updating())) 
            s2_update_choices()
    })
    observeEvent(input$s2_age_group, {
        if (identical(input$s2_metric_tab, "DOT")) {
            s2_initial_load(TRUE)
            s2_update_choices()
        }
    }, ignoreInit = TRUE)
    observeEvent(input$s2_metric_tab, {
        s2_initial_load(TRUE)
        s2_update_choices()
    }, ignoreInit = TRUE)
    observeEvent(input$s2_pick_dept, ignoreInit = TRUE, {
        if (!isTRUE(s2_updating())) 
            s2_update_choices()
    })
    observeEvent(input$s2_pick_c1, ignoreInit = TRUE, {
        if (!isTRUE(s2_updating())) 
            s2_update_choices()
    })
    observeEvent(input$s2_pick_c2, ignoreInit = TRUE, {
        if (!isTRUE(s2_updating())) 
            s2_update_choices()
    })
    observeEvent(input$s2_pick_route, ignoreInit = TRUE, {
        if (!isTRUE(s2_updating())) 
            s2_update_choices()
    })
    observeEvent(input$s2_age_group_ddd, ignoreInit = TRUE, {
        if (identical(input$s2_metric_tab, "DDD")) 
            s2_update_choices()
    })
    observeEvent(input$sheet, {
        if (identical(input$sheet, "Sheet2")) {
            s2_select_all_on_entry(TRUE)
            s2_initial_load(TRUE)
            s2_update_choices()
        }
    }, ignoreInit = TRUE)
    observeEvent(input$s2_class_all, ignoreInit = TRUE, {
        if (isTRUE(prog_s2_class_all())) {
            prog_s2_class_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$s2_class_all)) 
            isolate(rv_choices$s2_class %||% character(0))
        else character(0)
        freezeReactiveValue(input, "s2_pick_class")
        updateCheckboxGroupInput(session, "s2_pick_class", selected = sel)
    })
    observeEvent(input$s2_ingr_all, ignoreInit = TRUE, {
        if (isTRUE(prog_s2_ingr_all())) {
            prog_s2_ingr_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$s2_ingr_all)) 
            isolate(rv_choices$s2_ingr %||% character(0))
        else character(0)
        freezeReactiveValue(input, "s2_pick_ingr")
        updateCheckboxGroupInput(session, "s2_pick_ingr", selected = sel)
    })
    observeEvent(input$s2_route_all, ignoreInit = TRUE, {
        if (isTRUE(prog_s2_route_all())) {
            prog_s2_route_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$s2_route_all)) 
            isolate(rv_choices$s2_route %||% character(0))
        else character(0)
        freezeReactiveValue(input, "s2_pick_route")
        updateCheckboxGroupInput(session, "s2_pick_route", selected = sel)
    })
    observeEvent(input$s2_pick_class, ignoreInit = TRUE, {
        choices <- isolate(rv_choices$s2_class %||% character(0))
        sel <- input$s2_pick_class %||% character(0)
        all_on <- length(choices) > 0 && setequal(sel, choices)
        if (!identical(isTRUE(input$s2_class_all), all_on)) {
            prog_s2_class_all(TRUE)
            updateCheckboxInput(session, "s2_class_all", value = all_on)
        }
    })
    observeEvent(input$s2_pick_ingr, ignoreInit = TRUE, {
        choices <- isolate(rv_choices$s2_ingr %||% character(0))
        sel <- input$s2_pick_ingr %||% character(0)
        all_on <- length(choices) > 0 && setequal(sel, choices)
        if (!identical(isTRUE(input$s2_ingr_all), all_on)) {
            prog_s2_ingr_all(TRUE)
            updateCheckboxInput(session, "s2_ingr_all", value = all_on)
        }
    })
    observeEvent(input$s2_pick_route, ignoreInit = TRUE, {
        choices <- isolate(rv_choices$s2_route %||% character(0))
        sel <- input$s2_pick_route %||% character(0)
        all_on <- length(choices) > 0 && setequal(sel, choices)
        if (!identical(isTRUE(input$s2_route_all), all_on)) {
            prog_s2_route_all(TRUE)
            updateCheckboxInput(session, "s2_route_all", value = all_on)
        }
    })
    observeEvent(input$s2_pick_dept, ignoreInit = TRUE, {
        ch <- isolate(rv_choices$s2_dept) %||% character(0)
        sel <- input$s2_pick_dept %||% character(0)
        all_on <- length(ch) > 0 && setequal(sel, ch)
        if (!identical(isTRUE(input$s2_dept_all), all_on)) {
            prog_s2_dept_all(TRUE)
            freezeReactiveValue(input, "s2_dept_all")
            updateCheckboxInput(session, "s2_dept_all", value = all_on)
        }
    })
    observeEvent(input$s2_dept_all, ignoreInit = TRUE, {
        if (isTRUE(prog_s2_dept_all())) {
            prog_s2_dept_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$s2_dept_all)) 
            isolate(rv_choices$s2_dept %||% character(0))
        else character(0)
        freezeReactiveValue(input, "s2_pick_dept")
        updateCheckboxGroupInput(session, "s2_pick_dept", selected = sel)
    })
    observeEvent(input$s2_pick_c1, ignoreInit = TRUE, {
        choices <- isolate(rv_choices$s2_c1 %||% character(0))
        sel <- input$s2_pick_c1 %||% character(0)
        all_on <- length(choices) > 0 && setequal(sel, choices)
        if (!identical(isTRUE(input$s2_c1_all), all_on)) {
            prog_s2_c1_all(TRUE)
            updateCheckboxInput(session, "s2_c1_all", value = all_on)
        }
    })
    observeEvent(input$s2_c1_all, ignoreInit = TRUE, {
        if (isTRUE(prog_s2_c1_all())) {
            prog_s2_c1_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$s2_c1_all)) 
            isolate(rv_choices$s2_c1 %||% character(0))
        else character(0)
        freezeReactiveValue(input, "s2_pick_c1")
        updateCheckboxGroupInput(session, "s2_pick_c1", selected = sel)
    })
    observeEvent(input$s2_pick_c2, ignoreInit = TRUE, {
        ch <- isolate(rv_choices$s2_c2) %||% character(0)
        sel <- input$s2_pick_c2 %||% character(0)
        all_on <- length(ch) > 0 && setequal(sel, ch)
        if (!identical(isTRUE(input$s2_c2_all), all_on)) {
            prog_s2_c2_all(TRUE)
            freezeReactiveValue(input, "s2_c2_all")
            updateCheckboxInput(session, "s2_c2_all", value = all_on)
        }
    })
    observeEvent(input$s2_c2_all, ignoreInit = TRUE, {
        if (isTRUE(prog_s2_c2_all())) {
            prog_s2_c2_all(FALSE)
            return(invisible())
        }
        sel <- if (isTRUE(input$s2_c2_all)) 
            isolate(rv_choices$s2_c2 %||% character(0))
        else character(0)
        freezeReactiveValue(input, "s2_pick_c2")
        updateCheckboxGroupInput(session, "s2_pick_c2", selected = sel)
    })
    observeEvent(input$s2_run, {
        s2_locked_group_key(isolate(s2_current_group_key()))
        s2_locked_picked(isolate(s2_picked_by_group()))
    })
    s2_abx_filtered <- reactive({
        d <- s2_data()
        if (is.null(d)) 
            return(tibble())
        abx <- s2_base_abx()
        if (!nrow(abx)) 
            return(abx)
        keys <- abx %>% dplyr::transmute(MDRP_NO = as.character(MDRP_NO), PTNO = as.character(PTNO)) %>% dplyr::filter(!is.na(MDRP_NO), !is.na(PTNO)) %>% dplyr::distinct()
        if (!is.null(d$dept_map) && length(input$s2_pick_dept %||% character(0)) > 0) {
            keys <- keys %>% dplyr::semi_join(d$dept_map %>% dplyr::filter(DEPT %in% input$s2_pick_dept), by = c("MDRP_NO", "PTNO"))
        }
        abx <- abx %>% dplyr::semi_join(keys, by = c("MDRP_NO", "PTNO"))
        metric <- input$s2_metric_tab %||% "DOT"
        if (identical(metric, "DOT")) {
            pick <- input$s2_age_group %||% "All"
            if (has_valid_age(abx)) {
                if (identical(pick, "Adult")) 
                  abx <- dplyr::filter(abx, age >= 15)
                else if (identical(pick, "Pediatric")) 
                  abx <- dplyr::filter(abx, age < 15)
            }
        }
        else {
            if (has_valid_age(abx)) 
                abx <- dplyr::filter(abx, age >= 15)
        }
        keys2 <- abx %>% dplyr::transmute(MDRP_NO = as.character(MDRP_NO), PTNO = as.character(PTNO)) %>% dplyr::filter(!is.na(MDRP_NO), !is.na(PTNO)) %>% dplyr::distinct()
        if (!is.null(d$diag_map) && length(input$s2_pick_c1 %||% character(0)) > 0) {
            keys2 <- keys2 %>% dplyr::semi_join(d$diag_map %>% dplyr::filter(C1 %in% input$s2_pick_c1), by = c("MDRP_NO", "PTNO"))
        }
        if (!is.null(d$diag_map) && length(input$s2_pick_c2 %||% character(0)) > 0) {
            keys2 <- keys2 %>% dplyr::semi_join(d$diag_map %>% dplyr::filter(C2 %in% input$s2_pick_c2), by = c("MDRP_NO", "PTNO"))
        }
        abx <- abx %>% dplyr::semi_join(keys2, by = c("MDRP_NO", "PTNO"))
        if (!is.null(input$s2_pick_class) && length(input$s2_pick_class) > 0) 
            abx <- abx %>% dplyr::filter(class %in% input$s2_pick_class)
        if (!is.null(input$s2_pick_ingr) && length(input$s2_pick_ingr) > 0) 
            abx <- abx %>% dplyr::filter(ingredient %in% input$s2_pick_ingr)
        if (!is.null(input$s2_pick_route) && length(input$s2_pick_route) > 0) 
            abx <- abx %>% dplyr::filter(route %in% input$s2_pick_route)
        abx
    })
    s2_abx_only_date <- reactive({
        s2_base_abx()
    })
    s2_calc_std <- function(abx_df, pd_df) {
        empty_std <- tibble(month = character(), class = character(), atc_code = character(), ingredient = character(), form = character(), route = character(), DOT_cnt = numeric(), DDD_sum = numeric(), patient_days = numeric(), DOT_1000PD = numeric(), DDD_1000PD = numeric())
        if (is.null(abx_df)) 
            return(empty_std)
        df <- if (inherits(abx_df, "data.frame")) {
            tibble::as_tibble(abx_df)
        }
        else if (is.list(abx_df) && length(abx_df) == 1 && inherits(abx_df[[1]], "data.frame")) {
            tibble::as_tibble(abx_df[[1]])
        }
        else {
            tibble::tibble()
        }
        if (!nrow(df)) 
            return(empty_std)
        pd_tbl <- if (inherits(pd_df, "data.frame")) {
            tibble::as_tibble(pd_df)
        }
        else if (is.list(pd_df) && length(pd_df) == 1 && inherits(pd_df[[1]], "data.frame")) {
            tibble::as_tibble(pd_df[[1]])
        }
        else {
            tibble::tibble()
        }
        if (!"month" %in% names(pd_tbl)) 
            pd_tbl$month <- character()
        if (!"patient_days" %in% names(pd_tbl)) 
            pd_tbl$patient_days <- numeric()
        pd_tbl <- pd_tbl %>% dplyr::mutate(month = as.character(month), patient_days = suppressWarnings(as.numeric(patient_days)))
        if (!"date" %in% names(df)) 
            df$date <- as.Date(NA)
        if (!"patient_id" %in% names(df)) 
            df$patient_id <- NA_character_
        if (!"atc_code" %in% names(df)) 
            df$atc_code <- NA_character_
        if (!"ingredient" %in% names(df)) 
            df$ingredient <- NA_character_
        if (!"form" %in% names(df)) 
            df$form <- NA_character_
        if (!"route" %in% names(df)) 
            df$route <- NA_character_
        if (!"class" %in% names(df)) 
            df$class <- NA_character_
        df$month <- to_month_key(df$date)
        dot_daily <- df %>% dplyr::filter(!is.na(date)) %>% dplyr::distinct(patient_id, atc_code, ingredient, form, route, class, date, month, .keep_all = FALSE) %>% dplyr::count(class, atc_code, ingredient, form, route, month, name = "DOT_cnt")
        if (all(c("dose", "unit", "unit_g", "ddd_g") %in% names(df))) {
            df <- df %>% dplyr::mutate(dose_g = dose_to_grams(dose, unit, unit_g), ddd_cnt = dplyr::if_else(!is.na(ddd_g) & ddd_g > 0, dose_g/ddd_g, 0))
        }
        else {
            df$ddd_cnt <- 0
        }
        ddd_mon <- df %>% dplyr::group_by(class, atc_code, ingredient, form, route, month) %>% dplyr::summarise(DDD_sum = sum(ddd_cnt, na.rm = TRUE), .groups = "drop")
        mon <- dplyr::full_join(ddd_mon, dot_daily, by = c("class", "atc_code", "ingredient", "form", "route", "month")) %>% dplyr::mutate(DDD_sum = dplyr::coalesce(DDD_sum, 0), DOT_cnt = dplyr::coalesce(DOT_cnt, 0))
        mon %>% dplyr::left_join(pd_tbl, by = "month") %>% dplyr::mutate(patient_days = dplyr::coalesce(patient_days, NA_real_), DDD_1000PD = dplyr::if_else(!is.na(patient_days) & patient_days > 0, 1000 * DDD_sum/patient_days, NA_real_), DOT_1000PD = dplyr::if_else(!is.na(patient_days) & patient_days > 0, 1000 * DOT_cnt/patient_days, NA_real_))
    }
    s2_std_overall <- eventReactive(input$s2_run, {
        d <- isolate(s2_data())
        if (is.null(d)) 
            return(tibble())
        s2_calc_std(isolate(s2_abx_only_date()), d$pd)
    }, ignoreInit = TRUE)
    s2_std_filtered <- eventReactive(input$s2_run, {
        d <- isolate(s2_data())
        if (is.null(d)) 
            return(tibble())
        s2_calc_std(isolate(s2_abx_filtered()), d$pd)
    }, ignoreInit = TRUE)
    s2_month_total <- function(std_df) {
        if (is.null(std_df) || nrow(std_df) == 0) 
            return(tibble())
        std_df %>% group_by(month) %>% summarise(DOT_cnt = sum(DOT_cnt, na.rm = TRUE), DDD_sum = sum(DDD_sum, na.rm = TRUE), patient_days = suppressWarnings(first(na.omit(patient_days))), .groups = "drop") %>% mutate(DOT_1000PD = if_else(is.finite(patient_days) & patient_days > 0, 1000 * DOT_cnt/patient_days, NA_real_), DDD_1000PD = if_else(is.finite(patient_days) & patient_days > 0, 1000 * DDD_sum/patient_days, NA_real_), month_date = as.Date(paste0(month, "-01")))
    }
    s2_group_ts <- function(std_df, group_key, picked = NULL) {
        if (nrow(std_df) == 0) 
            return(tibble())
        if (identical(group_key, "total")) {
            tot <- s2_month_total(std_df) %>% mutate(group = "TOTAL")
            return(tot %>% select(month, month_date, group, patient_days, DOT_cnt, DOT_1000PD, DDD_sum, DDD_1000PD))
        }
        grp_col <- switch(group_key, class = "class", ingr = "ingredient", route = "route")
        out <- std_df %>% group_by(month, .data[[grp_col]]) %>% summarise(DOT_cnt = sum(DOT_cnt, na.rm = TRUE), DDD_sum = sum(DDD_sum, na.rm = TRUE), patient_days = suppressWarnings(first(na.omit(patient_days))), .groups = "drop") %>% mutate(DOT_1000PD = if_else(is.finite(patient_days) & patient_days > 0, 1000 * DOT_cnt/patient_days, NA_real_), DDD_1000PD = if_else(is.finite(patient_days) & patient_days > 0, 1000 * DDD_sum/patient_days, NA_real_), month_date = as.Date(paste0(month, "-01"))) %>% rename(group = !!rlang::sym(grp_col))
        if (!is.null(picked) && length(picked) > 0) 
            out <- out %>% filter(group %in% picked)
        out
    }
    s2_add_total_and_cumulate <- function(ts_df, metric = c("DOT_1000PD", "DDD_1000PD")) {
        metric <- match.arg(metric)
        if (is.null(ts_df) || nrow(ts_df) == 0) 
            return(ts_df)
        if (all(ts_df$group == "TOTAL")) {
            out <- ts_df %>% dplyr::mutate(value = .data[[metric]]) %>% dplyr::group_by(group) %>% dplyr::arrange(month_date, .by_group = TRUE) %>% dplyr::mutate(value_cum = cumsum(replace(value, is.na(value), 0))) %>% dplyr::ungroup()
            return(out)
        }
        no_total <- ts_df %>% dplyr::filter(.data$group != "TOTAL")
        total_by_m <- no_total %>% dplyr::group_by(month, month_date) %>% dplyr::summarise(value = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") %>% dplyr::mutate(group = "TOTAL")
        combined <- dplyr::bind_rows(no_total %>% dplyr::mutate(value = .data[[metric]]), total_by_m) %>% dplyr::group_by(group) %>% dplyr::arrange(month_date, .by_group = TRUE) %>% dplyr::mutate(value_cum = cumsum(replace(value, is.na(value), 0))) %>% dplyr::ungroup()
        combined
    }
    s2_ts_cum_with_total <- function(std_df, group_key = c("class", "ingr", "route"), metric = c("DOT", "DDD"), picked = NULL) {
        group_key <- match.arg(group_key)
        metric <- match.arg(metric)
        if (is.null(std_df) || !nrow(std_df)) {
            return(list(groups = tibble(), total = tibble(), metric_col = NULL))
        }
        ts <- s2_group_ts(std_df, group_key, picked)
        metric_col <- if (metric == "DOT") 
            "DOT_1000PD"
        else "DDD_1000PD"
        if (!nrow(ts)) {
            return(list(groups = tibble(), total = tibble(), metric_col = metric_col))
        }
        ts <- ts %>% dplyr::arrange(group, month_date) %>% dplyr::mutate(val = dplyr::coalesce(.data[[metric_col]], 0)) %>% dplyr::group_by(group) %>% dplyr::mutate(val_cum = cumsum(val)) %>% dplyr::ungroup()
        tot <- s2_month_total(std_df) %>% dplyr::arrange(month_date) %>% dplyr::mutate(val = dplyr::coalesce(.data[[metric_col]], 0), val_cum = cumsum(val))
        list(groups = ts, total = tot, metric_col = metric_col)
    }
    s2_group_table <- function(std_df, group_key, picked = NULL) {
        if (is.null(std_df) || nrow(std_df) == 0) 
            return(tibble())
        if (identical(group_key, "total")) {
            out <- std_df %>% dplyr::group_by(month, ingredient) %>% dplyr::summarise(DOT_cnt = sum(DOT_cnt, na.rm = TRUE), DDD_sum = sum(DDD_sum, na.rm = TRUE), patient_days = suppressWarnings(dplyr::first(na.omit(patient_days))), .groups = "drop") %>% dplyr::mutate(DOT_1000PD = dplyr::if_else(is.finite(patient_days) & patient_days > 0, 1000 * DOT_cnt/patient_days, NA_real_), DDD_1000PD = dplyr::if_else(is.finite(patient_days) & patient_days > 0, 1000 * DDD_sum/patient_days, NA_real_), group = "TOTAL") %>% 
                dplyr::relocate(group, .after = month)
            return(out)
        }
        grp_col <- switch(group_key, class = "class", ingr = "ingredient", route = "route")
        df <- std_df
        if (!is.null(picked) && length(picked) > 0) {
            df <- dplyr::filter(df, .data[[grp_col]] %in% picked)
        }
        if (identical(grp_col, "ingredient")) {
            out <- df %>% dplyr::group_by(month, ingredient) %>% dplyr::summarise(DOT_cnt = sum(DOT_cnt, na.rm = TRUE), DDD_sum = sum(DDD_sum, na.rm = TRUE), patient_days = suppressWarnings(dplyr::first(na.omit(patient_days))), .groups = "drop") %>% dplyr::mutate(DOT_1000PD = dplyr::if_else(is.finite(patient_days) & patient_days > 0, 1000 * DOT_cnt/patient_days, NA_real_), DDD_1000PD = dplyr::if_else(is.finite(patient_days) & patient_days > 0, 1000 * DDD_sum/patient_days, NA_real_), group = ingredient) %>% 
                dplyr::relocate(group, .before = ingredient)
            return(out)
        }
        else {
            out <- df %>% dplyr::group_by(month, .data[[grp_col]], ingredient) %>% dplyr::summarise(DOT_cnt = sum(DOT_cnt, na.rm = TRUE), DDD_sum = sum(DDD_sum, na.rm = TRUE), patient_days = suppressWarnings(dplyr::first(na.omit(patient_days))), .groups = "drop") %>% dplyr::mutate(DOT_1000PD = dplyr::if_else(is.finite(patient_days) & patient_days > 0, 1000 * DOT_cnt/patient_days, NA_real_), DDD_1000PD = dplyr::if_else(is.finite(patient_days) & patient_days > 0, 1000 * DDD_sum/patient_days, NA_real_)) %>% 
                dplyr::rename(group = !!rlang::sym(grp_col))
            return(out)
        }
    }
    s2_table_aggregated <- function(std_df, group_key, picked = NULL, metric = c("DOT", "DDD")) {
        metric <- match.arg(metric)
        if (is.null(std_df) || !nrow(std_df)) 
            return(tibble())
        metric_col <- if (metric == "DOT") 
            "DOT_1000PD"
        else "DDD_1000PD"
        if (identical(group_key, "total")) {
            out <- std_df %>% dplyr::group_by(ingredient) %>% dplyr::summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop") %>% dplyr::mutate(group = "TOTAL") %>% dplyr::select(group, ingredient, value)
            return(out %>% dplyr::arrange(dplyr::desc(value)))
        }
        grp_col <- switch(group_key, class = "class", ingr = "ingredient", route = "route")
        df <- std_df
        if (!is.null(picked) && length(picked) > 0) {
            df <- df %>% dplyr::filter(.data[[grp_col]] %in% picked)
        }
        out <- df %>% dplyr::group_by(.data[[grp_col]], ingredient) %>% dplyr::summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop") %>% dplyr::rename(group = !!rlang::sym(grp_col))
        out %>% dplyr::arrange(dplyr::desc(value))
    }
    s2_wait_plot <- function(msg = "좌측 ‘🔁 Calculation’을 누르면 갱신됩니다.") {
        ggplot() + geom_text(aes(0.5, 0.5, label = msg), color = "grey50", size = 5) + theme_void()
    }
    output$s2_plot_overall_dot <- renderPlot({
        std <- s2_std_overall()
        if (is.null(std) || nrow(std) == 0) 
            return(s2_wait_plot())
        tot <- s2_month_total(std)
        if (nrow(tot) == 0) 
            return(s2_wait_plot("표시할 데이터가 없습니다"))
        ggplot(tot, aes(x = month_date, y = DOT_1000PD)) + geom_line(linewidth = 1.1, color = BAR_BLUE) + geom_point(size = 2.2, color = BAR_BLUE) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DOT") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    output$s2_plot_overall_ddd <- renderPlot({
        std <- s2_std_overall()
        if (is.null(std) || nrow(std) == 0) 
            return(s2_wait_plot())
        tot <- s2_month_total(std)
        if (nrow(tot) == 0) 
            return(s2_wait_plot("표시할 데이터가 없습니다"))
        ggplot(tot, aes(x = month_date, y = DDD_1000PD)) + geom_line(linewidth = 1.1, color = BAR_BLUE) + geom_point(size = 2.2, color = BAR_BLUE) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DDD") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    output$s2_plot_filtered_dot <- renderPlot({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(s2_wait_plot())
        pick <- if (identical(s2_locked_group_key(), "class")) 
            s2_locked_picked()
        else NULL
        ts <- s2_group_ts(std, group_key = "class", picked = pick)
        if (nrow(ts) == 0) 
            return(s2_wait_plot("선택 조건에 해당하는 데이터가 없습니다"))
        groups <- unique(ts$group)
        cols_map <- s2_group_colors(groups, "class")
        if (length(groups) == 1) {
            col1 <- unname(cols_map[[as.character(groups[1])]])
            ggplot(ts, aes(x = month_date, y = DOT_1000PD)) + geom_line(linewidth = 1.1, color = col1) + geom_point(size = 2, color = col1) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DOT") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
        }
        else {
            ggplot(ts, aes(x = month_date, y = DOT_1000PD, color = group)) + geom_line(linewidth = 1) + geom_point(size = 1.8) + scale_color_manual(values = cols_map, breaks = groups) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DOT", color = "계열") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
        }
    })
    output$s2_plot_filtered_ddd <- renderPlot({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(s2_wait_plot())
        pick <- if (identical(s2_locked_group_key(), "class")) 
            s2_locked_picked()
        else NULL
        ts <- s2_group_ts(std, group_key = "class", picked = pick)
        if (nrow(ts) == 0) 
            return(s2_wait_plot("선택 조건에 해당하는 데이터가 없습니다"))
        groups <- unique(ts$group)
        cols_map <- s2_group_colors(groups, "class")
        if (length(groups) == 1) {
            col1 <- unname(cols_map[[as.character(groups[1])]])
            ggplot(ts, aes(x = month_date, y = DDD_1000PD)) + geom_line(linewidth = 1.1, color = col1) + geom_point(size = 2, color = col1) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DDD") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
        }
        else {
            ggplot(ts, aes(x = month_date, y = DDD_1000PD, color = group)) + geom_line(linewidth = 1) + geom_point(size = 1.8) + scale_color_manual(values = cols_map, breaks = groups) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DDD", color = "계열") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
        }
    })
    output$s2_plot_filtered_ingr_dot <- renderPlot({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(s2_wait_plot())
        pick <- if (identical(s2_locked_group_key(), "ingr")) 
            s2_locked_picked()
        else NULL
        ts <- s2_group_ts(std, group_key = "ingr", picked = pick)
        if (nrow(ts) == 0) 
            return(s2_wait_plot("선택 조건에 해당하는 데이터가 없습니다"))
        groups <- unique(ts$group)
        cols_map <- s2_group_colors(groups, "ingr")
        if (length(groups) == 1) {
            col1 <- unname(cols_map[[as.character(groups[1])]])
            ggplot(ts, aes(x = month_date, y = DOT_1000PD)) + geom_line(linewidth = 1.1, color = col1) + geom_point(size = 2, color = col1) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DOT") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
        }
        else {
            ggplot(ts, aes(x = month_date, y = DOT_1000PD, color = group)) + geom_line(linewidth = 1) + geom_point(size = 1.8) + scale_color_manual(values = cols_map, breaks = groups) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DOT", color = "성분명") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
        }
    })
    output$s2_plot_filtered_ingr_ddd <- renderPlot({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(s2_wait_plot())
        pick <- if (identical(s2_locked_group_key(), "ingr")) 
            s2_locked_picked()
        else NULL
        ts <- s2_group_ts(std, group_key = "ingr", picked = pick)
        if (nrow(ts) == 0) 
            return(s2_wait_plot("선택 조건에 해당하는 데이터가 없습니다"))
        groups <- unique(ts$group)
        cols_map <- s2_group_colors(groups, "ingr")
        if (length(groups) == 1) {
            col1 <- unname(cols_map[[as.character(groups[1])]])
            ggplot(ts, aes(x = month_date, y = DDD_1000PD)) + geom_line(linewidth = 1.1, color = col1) + geom_point(size = 2, color = col1) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DDD") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
        }
        else {
            ggplot(ts, aes(x = month_date, y = DDD_1000PD, color = group)) + geom_line(linewidth = 1) + geom_point(size = 1.8) + scale_color_manual(values = cols_map, breaks = groups) + scale_x_date(breaks = breaks_pretty(6), labels = label_date("%Y-%m")) + scale_y_continuous(breaks = breaks_pretty(6), labels = label_number(accuracy = 0.1)) + labs(x = "Month (YYYY-MM)", y = "DDD", color = "성분명") + theme_sheet3_blue() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
        }
    })
    output$s2_tbl_dot <- renderDT({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(datatable(data.frame(안내 = "좌측 ‘🔁 Calculation’을 누르면 갱신됩니다."), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        gk <- s2_locked_group_key()
        pick <- s2_locked_picked()
        ts <- s2_group_table(std, gk, pick)
        if (nrow(ts) == 0) 
            return(datatable(data.frame(메시지 = "표시할 데이터가 없습니다"), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        out <- ts %>% dplyr::arrange(month, group, ingredient) %>% dplyr::select(month, group, ingredient, DOT_1000PD)
        datatable(out, rownames = FALSE, colnames = c("month", "group", "ingredient", "DOT"), options = dt_center_opts, class = "display nowrap") %>% formatRound("DOT_1000PD", 2)
    })
    output$s2_tbl_agg_dot <- renderDT({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(datatable(data.frame(안내 = "좌측 ‘🔁 Calculation’을 누르면 갱신됩니다."), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        gk <- s2_locked_group_key()
        pick <- s2_locked_picked()
        base <- s2_group_table(std, gk, pick)
        if (!nrow(base)) 
            return(datatable(data.frame(메시지 = "표시할 데이터가 없습니다"), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        out <- base %>% dplyr::mutate(year = as.integer(substr(month, 1, 4))) %>% dplyr::group_by(year, group, ingredient) %>% dplyr::summarise(DOT = sum(DOT_1000PD, na.rm = TRUE), .groups = "drop") %>% dplyr::arrange(year, group, ingredient)
        datatable(out, rownames = FALSE, colnames = c("YEAR", "GROUP", "INGREDIENT", "DOT"), options = dt_center_opts, class = "display nowrap") %>% formatRound("DOT", 2)
    })
    output$s2_tbl_ddd <- renderDT({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(datatable(data.frame(안내 = "좌측 ‘🔁 Calculation’을 누르면 갱신됩니다."), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        gk <- s2_locked_group_key()
        pick <- s2_locked_picked()
        ts <- s2_group_table(std, gk, pick)
        if (nrow(ts) == 0) 
            return(datatable(data.frame(메시지 = "표시할 데이터가 없습니다"), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        out <- ts %>% dplyr::arrange(month, group, ingredient) %>% dplyr::select(month, group, ingredient, DDD_1000PD)
        datatable(out, rownames = FALSE, colnames = c("Month", "Group", "Ingredient", "DDD"), options = dt_center_opts, class = "display nowrap") %>% formatRound("DDD_1000PD", 2)
    })
    output$s2_tbl_agg_ddd <- renderDT({
        std <- s2_std_filtered()
        if (is.null(std) || nrow(std) == 0) 
            return(datatable(data.frame(안내 = "좌측 ‘🔁 Calculation’을 누르면 갱신됩니다."), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        gk <- s2_locked_group_key()
        pick <- s2_locked_picked()
        base <- s2_group_table(std, gk, pick)
        if (!nrow(base)) 
            return(datatable(data.frame(메시지 = "표시할 데이터가 없습니다"), rownames = FALSE, options = dt_center_opts, class = "display nowrap"))
        out <- base %>% dplyr::mutate(year = as.integer(substr(month, 1, 4))) %>% dplyr::group_by(year, group, ingredient) %>% dplyr::summarise(DDD = sum(DDD_1000PD, na.rm = TRUE), .groups = "drop") %>% dplyr::arrange(year, group, ingredient)
        datatable(out, rownames = FALSE, colnames = c("Year", "Group", "Ingrdient", "DDD"), options = dt_center_opts, class = "display nowrap") %>% formatRound("DDD", 2)
    })
    output$filter_controls <- renderUI({
        if (!file_uploaded() || is.null(input$sheet)) 
            return(NULL)
        if (input$sheet == "Sheet1") 
            return(diagnosis_controls_ui())
        else if (input$sheet == "Sheet2") 
            return(s2_controls_ui())
        else if (input$sheet == "Sheet3") 
            return(NULL)
        else return(NULL)
    })
    output$main_content <- renderUI({
        if (!file_uploaded() || is.null(input$sheet)) 
            return(NULL)
        if (input$sheet == "Sheet3") 
            return(patients_main_content_ui())
        else if (input$sheet == "Sheet1") 
            return(diagnosis_main_content_ui())
        else if (input$sheet == "Sheet2") 
            return(s2_main_content_ui())
        tagList(fluidRow(column(12, div(class = "card-box", h3("📄 선택된 시트는 사전 정의된 대시보드가 없습니다.")))))
    })
}

shinyApp(ui, server)

