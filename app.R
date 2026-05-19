# ============================================================
# APP SHINY - GESTIÓN DE PROYECTOS I+D  
# ============================================================

library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(stringr)
library(scales)
library(bslib)
library(highcharter)
library(reactable)
library(openxlsx)
library(purrr)
library(shinyWidgets)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------
get_con <- function() {
  dbConnect(
    Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT")),
    dbname   = Sys.getenv("SUPABASE_DB"),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    sslmode  = "require"
  )
}

fmt_clp <- function(x) {
  if (length(x)==0||is.null(x)||is.na(x)||x==0) return("—")
  paste0("$", format(round(x), big.mark=".", scientific=FALSE))
}

safe <- function(x, default="—") {
  if (length(x)==0||is.null(x)) return(default)
  x <- as.character(x[[1]])
  if (is.na(x)||trimws(x)==""||trimws(x)=="NA") return(default)
  trimws(x)
}

safe_title <- function(x) {
  v <- safe(x); if(v=="—") return("—"); str_to_title(v)
}

badge_color <- function(estado) {
  e <- toupper(safe(estado,""))
  if (e %in% c("ACTIVO","VIGENTE","EN EJECUCIÓN","ADJUDICADO")) return("#15803D")
  if (e=="CERRADO") return("#64748B")
  if (e=="") return("#94A3B8")
  "#B45309"
}

lbl <- function(txt) tags$div(
  style="font-size:.64rem;color:#94A3B8;font-weight:700;letter-spacing:.6px;
         text-transform:uppercase;margin-bottom:2px;", txt)

# Normalizar nombres de facultad: quitar tildes, limpiar duplicados
normalizar_facultad <- function(x) {
  if (is.na(x)||trimws(x)=="") return("Sin información")
  # Quitar tildes con iconv (más robusto que chartr)
  x <- iconv(trimws(x), from="UTF-8", to="ASCII//TRANSLIT")
  x <- toupper(x)
  # Quitar "FACULTAD DE"
  x <- gsub("^FACULTAD DE\\s+", "", x)
  # Limpiar caracteres residuales de transliteración
  x <- gsub("[^A-Z0-9 ,./]", "", x)
  x <- trimws(x)
  # Normalizar variantes conocidas
  x <- gsub("CIENCIAS JURIDICAS,\\s*ECONOMICAS\\s*Y\\s*ADMINISTRATIVAS.*",
             "CIENCIAS JURIDICAS, ECONOMICAS Y ADMINISTRATIVAS", x)
  x <- gsub("CIENCIAS JURIDICAS,\\s*ADMINISTRATIVAS\\s*Y\\s*ECONOMICAS.*",
             "CIENCIAS JURIDICAS, ECONOMICAS Y ADMINISTRATIVAS", x)
  x <- gsub("VICERRECTORIA DE VINCULACION.*",
             "VICERRECTORIA DE VINCULACION Y COMPROMISO PUBLICO", x)
  x <- gsub("VICERRECTORIA DE INVESTIGACION.*",
             "VICERRECTORIA DE INVESTIGACION Y POSGRADO", x)
  str_to_title(x)
}

# ------------------------------------------------------------
# TEMA
# ------------------------------------------------------------
tema_app <- bs_theme(
  version=5, bg="#F1F5F9", fg="#0F172A",
  primary="#1E3A5F", secondary="#2563EB",
  success="#15803D", info="#2563EB",
  warning="#B45309", danger="#B91C1C",
  base_font=font_google("Inter"),
  heading_font=font_google("Inter", wght="700"),
  font_scale=0.93
)

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = tags$span(
    tags$span(style="color:#93C5FD;font-weight:800;letter-spacing:-.5px;","I+D"),
    tags$span(style="color:white;font-weight:300;"," Proyectos")
  ),
  theme=tema_app, bg="#1E3A5F", inverse=TRUE,

  header = tags$head(tags$style(HTML("
    .navbar { box-shadow:0 2px 10px rgba(0,0,0,.35); }
    .card   { border-radius:10px!important; border:none!important;
              box-shadow:0 1px 6px rgba(0,0,0,.07)!important; }
    .card-header { font-weight:700; font-size:.82rem; letter-spacing:.2px;
                   border-bottom:1px solid #E2E8F0!important;
                   background:#FAFBFC!important; color:#1E3A5F!important;
                   padding:.55rem 1rem!important; }
    /* Banda de filtros */
    .filtros-band { background:#F8FAFC; border-radius:10px;
                    padding:.7rem 1rem; margin-bottom:.6rem;
                    border:1px solid #E2E8F0; }
    .filter-label { font-size:.68rem; font-weight:700; color:#64748B;
                    letter-spacing:.5px; text-transform:uppercase;
                    margin-bottom:3px; display:block; }
    /* Picker dropdown: fondo blanco nítido */
    .bootstrap-select .dropdown-menu {
      background:white!important;
      border:1px solid #CBD5E1!important;
      border-radius:8px!important;
      box-shadow:0 8px 24px rgba(0,0,0,.12)!important;
      z-index:9999!important;
      font-size:.82rem!important;
    }
    .bootstrap-select .dropdown-menu li a {
      padding:5px 12px!important;
      color:#1E293B!important;
    }
    .bootstrap-select .dropdown-menu li.active a,
    .bootstrap-select .dropdown-menu li a:hover {
      background:#EFF6FF!important; color:#1E3A5F!important;
    }
    .bootstrap-select .dropdown-menu .bs-searchbox input {
      border-radius:6px!important; font-size:.82rem!important;
      border:1px solid #CBD5E1!important;
    }
    /* Badges */
    .badge-estado { padding:2px 9px; border-radius:20px;
                    font-size:.7rem; font-weight:700;
                    display:inline-block; white-space:nowrap; }
    /* Botones descarga */
    .btn-dl { border-radius:7px!important; font-size:.75rem!important;
              font-weight:600!important; padding:3px 9px!important; }
    /* Ficha: tarjetas */
    .proy-card   { margin-bottom:.45rem!important; }
    .proy-activo  { border-left:3px solid #15803D!important; }
    .proy-cerrado { border-left:4px solid #94A3B8!important; }
    .proy-otro    { border-left:4px solid #B45309!important; }
    .proy-card:hover { box-shadow:0 4px 16px rgba(0,0,0,.11)!important; }
    /* Grid metadatos ficha */
    .meta-row { display:grid; grid-template-columns:repeat(5,1fr);
                gap:0; margin-bottom:0; }
    .meta-cell { padding:5px 10px; border-right:1px solid #F1F5F9; }
    .meta-cell:first-child { padding-left:0; }
    .meta-cell:last-child  { border-right:none; }
    .meta-lbl { font-size:.63rem; color:#94A3B8; font-weight:700;
                letter-spacing:.6px; text-transform:uppercase; margin-bottom:2px; }
    .meta-val { font-weight:600; color:#1E293B; font-size:.8rem; }
    /* Franja financiera al fondo de la tarjeta */
    .fin-band { background:#F8FAFC; border-radius:0 0 8px 8px;
                margin:8px -1rem -1rem -1rem; padding:8px 14px;
                display:grid; grid-template-columns:auto auto auto 1fr;
                gap:16px; align-items:center;
                border-top:1px solid #E2E8F0; }
    .monto-ppal { font-size:1.05rem; font-weight:800; color:#15803D; }
    .monto-sec  { font-size:.79rem; font-weight:600; color:#475569; }
    .fac-tag { font-size:.74rem; color:#64748B; text-align:right; }
    .obj-box { background:#EFF6FF; border-radius:6px; padding:7px 10px;
               font-size:.76rem; color:#1E40AF; line-height:1.5;
               border-left:3px solid #BFDBFE; margin-top:8px; }
    /* Panel lateral investigador */
    .inv-panel { background:#EFF6FF; border-radius:10px; padding:14px; }
    /* Selectize investigador */
    .selectize-input { border-radius:7px!important; font-size:.83rem!important;
                       border:1px solid #CBD5E1!important; }
    .selectize-dropdown { border-radius:7px!important; font-size:.82rem!important;
                          box-shadow:0 8px 24px rgba(0,0,0,.12)!important;
                          border:1px solid #CBD5E1!important; }
    .selectize-dropdown-content .option:hover,
    .selectize-dropdown-content .option.active {
      background:#EFF6FF!important; color:#1E3A5F!important; }
  "))),

  nav_spacer(),

  # ── TAB 1: RESUMEN ────────────────────────────────────────
  nav_panel("Resumen", icon=icon("chart-pie"), padding="1rem",

    div(class="filtros-band",
      layout_columns(col_widths=c(2,10), gap=".75rem",
        div(tags$span(class="filter-label","Ano"),
            pickerInput("anio_resumen", NULL,
                        choices=c("Todos"="ALL"), selected="ALL",
                        options=pickerOptions(liveSearch=TRUE, size=10), width="100%")),
        div()
      )
    ),

    layout_columns(col_widths=c(3,3,3,3), gap=".6rem",
      value_box("Total Proyectos",   textOutput("kpi_total"),   showcase=icon("folder-open"),   theme="primary"),
      value_box("Proyectos Activos", textOutput("kpi_activos"), showcase=icon("circle-check"),  theme="success"),
      value_box("Monto Total",       textOutput("kpi_monto"),   showcase=icon("peso-sign"),     theme="info"),
      value_box("Investigadores",    textOutput("kpi_inv"),     showcase=icon("user-graduate"), theme="warning")
    ),

    layout_columns(col_widths=c(6,6), gap=".6rem",
      card(card_header("Proyectos y Monto por Año"), highchartOutput("hc_anio",     height="260px")),
      card(card_header("Distribución por Fondo"),    highchartOutput("hc_fondo",    height="260px"))
    ),
    layout_columns(col_widths=c(5,4,3), gap=".6rem",
      card(card_header("Proyectos por Facultad"),    highchartOutput("hc_facultad", height="290px")),
      card(card_header("Género del Director/a"),     highchartOutput("hc_genero",   height="290px")),
      card(card_header("Top Investigadores (N° proyectos)"),
           reactableOutput("top_inv", height="255px"))
    )
  ),

  # ── TAB 2: EXPLORADOR ─────────────────────────────────────
  nav_panel("Explorador", icon=icon("table"), padding="1rem",

    div(class="filtros-band",
      layout_columns(col_widths=c(3,3,3,3), gap=".75rem",
        div(tags$span(class="filter-label","Estado"),
            pickerInput("f_estado",   NULL, choices=c("Todos"="ALL"), selected="ALL",
                        options=pickerOptions(liveSearch=TRUE,size=10), width="100%")),
        div(tags$span(class="filter-label","Fondo"),
            pickerInput("f_fondo",    NULL, choices=c("Todos"="ALL"), selected="ALL",
                        options=pickerOptions(liveSearch=TRUE,liveSearchPlaceholder="Buscar...",size=10), width="100%")),
        div(tags$span(class="filter-label","Facultad"),
            pickerInput("f_facultad", NULL, choices=c("Todas"="ALL"), selected="ALL",
                        options=pickerOptions(liveSearch=TRUE,liveSearchPlaceholder="Buscar...",size=10), width="100%")),
        div(tags$span(class="filter-label","Año"),
            pickerInput("f_anio",     NULL, choices=c("Todos"="ALL"), selected="ALL",
                        options=pickerOptions(liveSearch=TRUE,size=10), width="100%"))
      )
    ),

    card(
      card_header(
        div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:6px;",
          textOutput("n_filtrados", inline=TRUE),
          div(style="display:flex;gap:5px;flex-wrap:wrap;",
            downloadButton("dl_excel_filtro","Excel (filtro)", icon=icon("file-excel"),
                           class="btn btn-success btn-sm btn-dl"),
            downloadButton("dl_csv_filtro",  "CSV (filtro)",  icon=icon("file-csv"),
                           class="btn btn-outline-secondary btn-sm btn-dl"),
            downloadButton("dl_excel_todo",  "Excel completo",icon=icon("database"),
                           class="btn btn-outline-primary btn-sm btn-dl")
          )
        )
      ),
      reactableOutput("tabla_react", height="530px")
    )
  ),

  # ── TAB 3: FICHA ──────────────────────────────────────────
  nav_panel("Ficha", icon=icon("user-tie"), padding="1rem",

    layout_columns(col_widths=c(3,9), gap=".75rem",

      card(
        card_header("Investigador/a"),
        card_body(
          # Selectize con búsqueda mientras escribes
          tags$span(class="filter-label","Nombre — escribe para buscar"),
          selectizeInput("sel_inv", NULL, choices=NULL,
                         options=list(
                           placeholder="Ej: Fern...",
                           maxOptions=500
                         ), width="100%"),

          tags$span(class="filter-label", style="margin-top:10px;display:block;","Categoría"),
          pickerInput("sel_tipo", NULL, choices=c("Todas"="ALL"), selected="ALL",
                      options=pickerOptions(liveSearch=TRUE,size=8), width="100%"),

          tags$span(class="filter-label", style="margin-top:10px;display:block;","Estado"),
          pickerInput("sel_estado_f", NULL, choices=c("Todos"="ALL"), selected="ALL",
                      options=pickerOptions(liveSearch=TRUE,size=8), width="100%"),

          tags$hr(style="margin:12px 0;border-color:#E2E8F0;"),
          uiOutput("resumen_inv")
        )
      ),

      div(style="overflow-y:auto;max-height:83vh;padding-right:4px;",
          uiOutput("fichas_proy"))
    )
  ),

  nav_item(tags$small(class="text-white-50 me-3",
                      textOutput("ts_update", inline=TRUE)))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ── DATOS ─────────────────────────────────────────────────
  datos <- reactivePoll(
    intervalMillis=300000, session,
    checkFunc=function() {
      con <- get_con(); on.exit(dbDisconnect(con))
      dbGetQuery(con,"SELECT MAX(updated_at) FROM lista")[[1]]
    },
    valueFunc=function() {
      con <- get_con(); on.exit(dbDisconnect(con))
      df <- dbReadTable(con,"lista")

      df$ano <- as.integer(df$ano)
      df$monto_total_del_proyecto <- as.numeric(df$monto_total_del_proyecto)
      df$financiamiento_fuente    <- as.numeric(df$financiamiento_fuente)
      df$aporte_uct_pecunario     <- as.numeric(df$aporte_uct_pecunario)
      df$inicio  <- as.Date(substr(df$inicio, 1,10))
      df$termino <- as.Date(substr(df$termino,1,10))

      # Facultad NORMALIZADA (quita tildes, unifica variantes)
      df$facultad_corta <- vapply(df$facultad, normalizar_facultad, character(1))

      df$genero_label <- case_when(
        !is.na(df$genero)&df$genero=="M"~"Masculino",
        !is.na(df$genero)&df$genero=="F"~"Femenino",
        TRUE~"No informado")

      df$estado_limpio <- ifelse(
        is.na(df$estado_4)|trimws(df$estado_4)=="",
        "Sin estado", trimws(df$estado_4))

      df$director_limpio <- ifelse(
        is.na(df$director_del_proyecto)|trimws(df$director_del_proyecto)=="",
        "Sin nombre", str_to_title(trimws(df$director_del_proyecto)))

      df
    }
  )

  output$ts_update <- renderText({ datos(); paste("↻",format(Sys.time(),"%H:%M")) })

  # ── POBLAR FILTROS ────────────────────────────────────────
  observe({
    df    <- datos()
    anios <- as.character(sort(unique(na.omit(df$ano)), decreasing=TRUE))

    updatePickerInput(session,"anio_resumen",
      choices=c("Todos"="ALL", anios), selected="ALL")

    updatePickerInput(session,"f_estado",
      choices=c("Todos"="ALL", sort(unique(df$estado_limpio))), selected="ALL")
    updatePickerInput(session,"f_fondo",
      choices=c("Todos"="ALL", sort(unique(na.omit(df$fondo)))), selected="ALL")
    updatePickerInput(session,"f_facultad",
      choices=c("Todas"="ALL", sort(unique(df$facultad_corta))), selected="ALL")
    updatePickerInput(session,"f_anio",
      choices=c("Todos"="ALL", anios), selected="ALL")

    invs <- sort(unique(df$director_limpio[df$director_limpio!="Sin nombre"]))
    updateSelectizeInput(session,"sel_inv",
      choices=c("Seleccione..."="", invs), server=TRUE)
  })

  # ── DATOS FILTRADOS POR PERÍODO ───────────────────────────
  # Año exacto tiene prioridad; si no, aplica desde/hasta
  datos_rango <- reactive({
    df <- datos()
    anio <- input$anio_resumen
    if (!is.null(anio) && anio != "ALL")
      df <- df[!is.na(df$ano) & df$ano == as.integer(anio), ]
    df
  })

  # ── KPIs ─────────────────────────────────────── ──────────
  output$kpi_total   <- renderText(format(nrow(datos_rango()), big.mark="."))
  output$kpi_activos <- renderText(
    format(sum(toupper(datos_rango()$estado_limpio) %in%
               c("ACTIVO","EN EJECUCIÓN","VIGENTE","ADJUDICADO")), big.mark="."))
  output$kpi_monto <- renderText({
    t <- sum(datos_rango()$monto_total_del_proyecto, na.rm=TRUE)
    paste0("$",format(round(t/1e9,1),big.mark="."),"B")
  })
  output$kpi_inv <- renderText(
    format(length(unique(
      datos_rango()$director_limpio[datos_rango()$director_limpio!="Sin nombre"]
    )), big.mark="."))

  # ── HIGHCHARTS ────────────────────────────────────────────
  hc_col <- c("#2563EB","#15803D","#B45309","#7C3AED","#0F766E","#B91C1C","#D97706","#1D4ED8")

  output$hc_anio <- renderHighchart({
    # Usa TODOS los datos del período (incluyendo año NA excluido del eje x)
    df <- datos_rango() %>%
      filter(!is.na(ano)) %>%
      group_by(ano) %>%
      summarise(n=n(), monto=sum(monto_total_del_proyecto,na.rm=TRUE)/1e6,.groups="drop") %>%
      arrange(ano)
    highchart() %>%
      hc_chart(style=list(fontFamily="Inter")) %>%
      hc_xAxis(categories=as.character(df$ano)) %>%
      hc_yAxis_multiples(
        list(title=list(text="N° Proyectos"),min=0),
        list(title=list(text="Monto (MM$)"),opposite=TRUE,min=0)
      ) %>%
      hc_add_series(name="Proyectos",type="column",data=df$n,
                    color="#2563EB",yAxis=0,
                    dataLabels=list(enabled=TRUE,style=list(fontSize="8px",fontWeight="700"))) %>%
      hc_add_series(name="Monto (MM$)",type="spline",data=round(df$monto,1),
                    color="#B45309",yAxis=1,lineWidth=2.5,marker=list(radius=3)) %>%
      hc_plotOptions(column=list(borderRadius=4,borderWidth=0)) %>%
      hc_tooltip(shared=TRUE,backgroundColor="#1E3A5F",style=list(color="white")) %>%
      hc_legend(enabled=TRUE,align="center",verticalAlign="bottom",
                itemStyle=list(fontSize="11px",fontWeight="600")) %>%
      hc_credits(enabled=FALSE) %>% hc_exporting(enabled=TRUE)
  })

  output$hc_fondo <- renderHighchart({
    df <- datos_rango() %>% filter(!is.na(fondo)) %>%
      count(fondo) %>% arrange(n) %>% tail(12)
    highchart() %>%
      hc_chart(type="bar",style=list(fontFamily="Inter")) %>%
      hc_xAxis(categories=df$fondo,labels=list(style=list(fontSize="10px"))) %>%
      hc_yAxis(title=list(text="N° Proyectos")) %>%
      hc_add_series(name="Proyectos",data=df$n,colorByPoint=TRUE,colors=hc_col,
                    dataLabels=list(enabled=TRUE,style=list(fontSize="9px",fontWeight="700"))) %>%
      hc_plotOptions(bar=list(borderRadius=4,borderWidth=0)) %>%
      hc_tooltip(pointFormat="<b>{point.y}</b> proyectos",
                 backgroundColor="#1E3A5F",style=list(color="white")) %>%
      hc_legend(enabled=FALSE) %>% hc_credits(enabled=FALSE) %>% hc_exporting(enabled=TRUE)
  })

  output$hc_facultad <- renderHighchart({
    # Solo proyectos con facultad informada
    df <- datos_rango() %>%
      filter(facultad_corta!="Sin información") %>%
      count(facultad_corta) %>% arrange(n)
    highchart() %>%
      hc_chart(type="bar",style=list(fontFamily="Inter")) %>%
      hc_xAxis(categories=df$facultad_corta,labels=list(style=list(fontSize="9px"))) %>%
      hc_yAxis(title=list(text="N° Proyectos")) %>%
      hc_add_series(name="Proyectos",data=df$n,color="#B45309",
                    dataLabels=list(enabled=TRUE,style=list(fontSize="9px",fontWeight="700"))) %>%
      hc_plotOptions(bar=list(borderRadius=4,borderWidth=0)) %>%
      hc_tooltip(pointFormat="<b>{point.y}</b> proyectos",
                 backgroundColor="#1E3A5F",style=list(color="white")) %>%
      hc_legend(enabled=FALSE) %>% hc_credits(enabled=FALSE) %>% hc_exporting(enabled=TRUE)
  })

  output$hc_genero <- renderHighchart({
    df <- datos_rango() %>% count(genero_label)
    hchart(df,"pie",hcaes(name=genero_label,y=n),name="Proyectos",
           dataLabels=list(enabled=TRUE,
                           format="<b>{point.name}</b><br>{point.y} ({point.percentage:.1f}%)")) %>%
      hc_colors(c("#2563EB","#B45309","#94A3B8")) %>%
      hc_plotOptions(pie=list(innerSize="42%",borderWidth=2,borderColor="#F1F5F9")) %>%
      hc_tooltip(pointFormat="<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)",
                 backgroundColor="#1E3A5F",style=list(color="white")) %>%
      hc_credits(enabled=FALSE) %>% hc_exporting(enabled=TRUE)
  })

  # Top investigadores — ordenado por N° proyectos, mismo período
  output$top_inv <- renderReactable({
    df <- datos_rango() %>%
      filter(director_limpio!="Sin nombre") %>%
      group_by(Investigador=director_limpio) %>%
      summarise(N=n(), Monto=sum(monto_total_del_proyecto,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(N), desc(Monto)) %>% head(10)

    reactable(df, compact=TRUE, striped=TRUE, bordered=FALSE,
              defaultColDef=colDef(
                headerStyle=list(background="#EFF6FF",color="#1E3A5F",
                                 fontWeight="700",fontSize=".74rem"),
                style=list(fontSize=".78rem")),
              columns=list(
                Investigador=colDef(minWidth=130),
                N=colDef(name="Proy.",width=55,align="center",
                         style=list(fontWeight="800",color="#1E3A5F",fontSize=".9rem")),
                Monto=colDef(width=120,align="right",
                             format=colFormat(currency="CLP",separators=TRUE,locales="es-CL"))
              ))
  })

  # ── EXPLORADOR ────────────────────────────────────────────
  datos_filtrados <- reactive({
    df <- datos()
    if (!is.null(input$f_estado)   && input$f_estado   !="ALL")
      df <- df[df$estado_limpio==input$f_estado, ]
    if (!is.null(input$f_fondo)    && input$f_fondo    !="ALL")
      df <- df[!is.na(df$fondo) & df$fondo==input$f_fondo, ]
    if (!is.null(input$f_facultad) && input$f_facultad !="ALL")
      df <- df[df$facultad_corta==input$f_facultad, ]
    if (!is.null(input$f_anio)     && input$f_anio     !="ALL")
      df <- df[!is.na(df$ano) & as.character(df$ano)==input$f_anio, ]
    df
  })

  output$n_filtrados <- renderText(
    paste(format(nrow(datos_filtrados()),big.mark="."),"proyectos"))

  output$tabla_react <- renderReactable({
    df <- datos_filtrados()
    tbl <- data.frame(
      Código   = vapply(df$codigo_del_proyecto, safe, character(1)),
      Año      = df$ano,
      Nombre   = vapply(df$nombre_del_proyecto, safe, character(1)),
      Director = df$director_limpio,
      Facultad = df$facultad_corta,
      Fondo    = vapply(df$fondo, safe, character(1)),
      Estado   = df$estado_limpio,
      Monto    = df$monto_total_del_proyecto,
      stringsAsFactors=FALSE,
      row.names=NULL
    )
    reactable(tbl,
      searchable=TRUE, sortable=TRUE, resizable=TRUE,
      striped=TRUE, highlight=TRUE, compact=TRUE,
      defaultPageSize=15, showPageSizeOptions=TRUE,
      pageSizeOptions=c(10,15,25,50),
      language=reactableLang(
        searchPlaceholder="Buscar en toda la tabla...",
        noData="Sin resultados",
        pageInfo="{rowStart}–{rowEnd} de {rows} proyectos",
        pagePrevious="←", pageNext="→"),
      defaultColDef=colDef(
        headerStyle=list(background="#EFF6FF",color="#1E3A5F",
                         fontWeight="700",fontSize=".77rem"),
        style=list(fontSize=".8rem"), na="—"),
      columns=list(
        Código  =colDef(minWidth=130,sticky="left",
                        style=list(fontWeight="700",color="#2563EB")),
        Año     =colDef(width=58),
        Nombre  =colDef(minWidth=270,
                        cell=function(v) div(title=v,
                          style="white-space:nowrap;overflow:hidden;
                                 text-overflow:ellipsis;max-width:270px;",v)),
        Director=colDef(minWidth=160),
        Facultad=colDef(minWidth=150),
        Fondo   =colDef(width=110),
        Estado  =colDef(width=105,
                        cell=function(v){
                          v <- if(is.null(v)||is.na(v)||v=="") "Sin estado" else v
                          tags$span(class="badge-estado",
                            style=paste0("background:",badge_color(v),";color:white;"),v)
                        }),
        Monto   =colDef(width=135,align="right",
                        format=colFormat(currency="CLP",separators=TRUE,locales="es-CL"),
                        na="—")
      )
    )
  })

  # Descargas — captura datos_filtrados() en el momento exacto de descarga
  cols_exp <- c("codigo_del_proyecto","ano","nombre_del_proyecto",
                "director_del_proyecto","genero","correo","facultad","departamento",
                "fondo","fuente_de_financiamiento","categoria_de_proyecto","subcategoria",
                "estado_4","inicio","termino","duracion_meses",
                "monto_total_del_proyecto","financiamiento_fuente",
                "aporte_uct_pecunario","aporte_asociados_pecunario")
  prep <- function(df) {
    df <- df %>% select(any_of(cols_exp))
    df
  }
  output$dl_excel_filtro <- downloadHandler(
    filename = function() paste0("proyectos_filtro_", Sys.Date(), ".xlsx"),
    content  = function(f) write.xlsx(prep(datos_filtrados()), f, overwrite=TRUE)
  )
  output$dl_csv_filtro <- downloadHandler(
    filename = function() paste0("proyectos_filtro_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(prep(datos_filtrados()), f, row.names=FALSE, fileEncoding="UTF-8")
  )
  output$dl_excel_todo <- downloadHandler(
    filename = function() paste0("proyectos_completo_", Sys.Date(), ".xlsx"),
    content  = function(f) write.xlsx(prep(datos()), f, overwrite=TRUE)
  )

  # ── FICHA INVESTIGADOR ────────────────────────────────────
  proy_inv <- reactive({
    req(input$sel_inv, input$sel_inv!="")
    datos() %>% filter(director_limpio==input$sel_inv)
  })

  observe({
    req(input$sel_inv, input$sel_inv!="")
    df <- proy_inv()
    updatePickerInput(session,"sel_tipo",
      choices=c("Todas"="ALL",sort(unique(na.omit(df$categoria_de_proyecto)))),
      selected="ALL")
    updatePickerInput(session,"sel_estado_f",
      choices=c("Todos"="ALL",sort(unique(df$estado_limpio))),
      selected="ALL")
  })

  proy_inv_f <- reactive({
    req(input$sel_inv, input$sel_inv!="")
    df <- proy_inv()
    if (!is.null(input$sel_tipo)     && input$sel_tipo    !="ALL")
      df <- df[!is.na(df$categoria_de_proyecto)&df$categoria_de_proyecto==input$sel_tipo,]
    if (!is.null(input$sel_estado_f) && input$sel_estado_f!="ALL")
      df <- df[df$estado_limpio==input$sel_estado_f,]
    df
  })

  # Resumen lateral — reactivo al filtro activo
  output$resumen_inv <- renderUI({
    req(input$sel_inv, input$sel_inv!="")
    df_f  <- proy_inv_f()
    df_all <- proy_inv()
    inv   <- df_all[1,]
    monto <- sum(df_f$monto_total_del_proyecto,na.rm=TRUE)
    correo <- safe(inv$correo)
    n_f <- nrow(df_f); n_t <- nrow(df_all)

    div(class="inv-panel",
      div(style="font-size:.67rem;color:#64748B;font-weight:700;letter-spacing:.5px;","INVESTIGADOR/A"),
      div(style="font-size:.9rem;font-weight:700;color:#1E3A5F;margin-top:3px;line-height:1.3;",
          inv$director_limpio),
      if(correo!="—")
        div(style="font-size:.72rem;color:#2563EB;margin-top:2px;",tolower(correo)),
      tags$hr(style="margin:10px 0;border-color:#BFDBFE;"),
      div(style="display:grid;grid-template-columns:1fr 1fr;gap:8px;",
        div(
          div(style="font-size:.65rem;color:#64748B;font-weight:700;","PROYECTOS"),
          div(style="font-size:1.6rem;font-weight:800;color:#1E3A5F;line-height:1;",n_f),
          if(n_f<n_t)
            div(style="font-size:.66rem;color:#94A3B8;margin-top:2px;",
                paste0("de ",n_t," en total"))
        ),
        div(
          div(style="font-size:.65rem;color:#64748B;font-weight:700;","MONTO FILTRADO"),
          div(style="font-size:.84rem;font-weight:700;color:#15803D;line-height:1.3;",
              fmt_clp(monto))
        )
      )
    )
  })

  # Tarjetas de proyectos — diseño mejorado
  output$fichas_proy <- renderUI({
    req(input$sel_inv, input$sel_inv!="")
    df <- proy_inv_f()

    if(nrow(df)==0) return(card(card_body(
      tags$p(class="text-muted text-center mt-3 mb-3",
             icon("circle-info")," Sin proyectos con los filtros aplicados."))))

    tarjetas <- lapply(seq_len(nrow(df)), function(i) {
      p   <- df[i,]
      est <- safe(p$estado_limpio,"Sin estado")
      col <- badge_color(est)
      borde <- if(toupper(est)%in%c("ACTIVO","VIGENTE","ADJUDICADO")) "#15803D"
               else if(toupper(est)=="CERRADO") "#94A3B8" else "#B45309"

      cod    <- safe(p$codigo_del_proyecto)
      anio   <- safe(p$ano)
      nombre <- safe_title(p$nombre_del_proyecto)
      fondo  <- safe(p$fondo)
      categ  <- safe_title(p$categoria_de_proyecto)
      fac    <- safe(p$facultad_corta)
      ini    <- if(!is.na(p$inicio))  format(p$inicio, "%b %Y") else "—"
      ter    <- if(!is.na(p$termino)) format(p$termino,"%b %Y") else "—"
      dur    <- if(!is.na(p$duracion_meses)) paste0(p$duracion_meses," meses") else "—"
      monto  <- fmt_clp(p$monto_total_del_proyecto)
      fin_e  <- fmt_clp(p$financiamiento_fuente)
      ap_uct <- fmt_clp(p$aporte_uct_pecunario)
      obj    <- safe(p$obj_genreal)

      div(
        style = paste0(
          "background:white; border-radius:10px; margin-bottom:.5rem;",
          "box-shadow:0 1px 5px rgba(0,0,0,.07);",
          "border-left:4px solid ", borde, ";"
        ),

        # ── Cabecera ──
        div(style="display:flex;justify-content:space-between;align-items:center;
                   padding:.6rem 1rem .4rem;border-bottom:1px solid #F1F5F9;",
          div(
            tags$span(style="font-size:.8rem;font-weight:700;color:#2563EB;letter-spacing:-.2px;", cod),
            tags$span(style="font-size:.72rem;color:#CBD5E1;margin:0 6px;", "·"),
            tags$span(style="font-size:.72rem;color:#94A3B8;", anio),
            tags$span(style="font-size:.72rem;color:#CBD5E1;margin:0 6px;", "·"),
            tags$span(style="font-size:.72rem;color:#94A3B8;", fondo)
          ),
          tags$span(
            style=paste0("background:",col,";color:white;padding:2px 10px;",
                         "border-radius:20px;font-size:.7rem;font-weight:700;"),
            est
          )
        ),

        # ── Nombre ──
        div(style="padding:.5rem 1rem .4rem;",
          tags$p(style="font-weight:700;font-size:.9rem;color:#0F172A;margin:0;line-height:1.35;",
                 nombre)
        ),

        # ── Metadatos en 2 filas ──
        div(style="display:grid;grid-template-columns:repeat(4,1fr);gap:0;
                   border-top:1px solid #F8FAFC;border-bottom:1px solid #F8FAFC;
                   background:#FAFBFC;",

          div(style="padding:.4rem 1rem;border-right:1px solid #F1F5F9;",
            div(style="font-size:.62rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Categoría"),
            div(style="font-size:.8rem;font-weight:600;color:#334155;",categ)
          ),
          div(style="padding:.4rem 1rem;border-right:1px solid #F1F5F9;",
            div(style="font-size:.62rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Inicio"),
            div(style="font-size:.8rem;font-weight:600;color:#334155;",ini)
          ),
          div(style="padding:.4rem 1rem;border-right:1px solid #F1F5F9;",
            div(style="font-size:.62rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Término"),
            div(style="font-size:.8rem;font-weight:600;color:#334155;",ter)
          ),
          div(style="padding:.4rem 1rem;",
            div(style="font-size:.62rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Duración"),
            div(style="font-size:.8rem;font-weight:600;color:#334155;",dur)
          )
        ),

        # ── Financiero ──
        div(style="display:flex;gap:0;align-items:stretch;",

          div(style="padding:.5rem 1rem;flex:0 0 auto;border-right:1px solid #F1F5F9;
                     background:#F0FDF4;border-radius:0 0 0 6px;",
            div(style="font-size:.61rem;color:#15803D;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Monto Total"),
            div(style="font-size:1rem;font-weight:800;color:#15803D;", monto)
          ),
          div(style="padding:.5rem 1rem;border-right:1px solid #F1F5F9;",
            div(style="font-size:.61rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Financ. Externo"),
            div(style="font-size:.82rem;font-weight:600;color:#475569;", fin_e)
          ),
          div(style="padding:.5rem 1rem;border-right:1px solid #F1F5F9;",
            div(style="font-size:.61rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Aporte UCT"),
            div(style="font-size:.82rem;font-weight:600;color:#475569;", ap_uct)
          ),
          div(style="padding:.5rem 1rem;flex:1;",
            div(style="font-size:.61rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:2px;","Facultad"),
            div(style="font-size:.8rem;color:#64748B;", fac)
          )
        ),

        # ── Objetivo ──
        if(obj!="—")
          div(style="padding:.4rem 1rem .5rem;border-top:1px solid #F1F5F9;",
            div(style="font-size:.61rem;color:#94A3B8;font-weight:700;letter-spacing:.5px;
                       text-transform:uppercase;margin-bottom:3px;","Objetivo"),
            div(style="font-size:.78rem;color:#64748B;line-height:1.5;
                       background:#F8FAFC;border-radius:6px;padding:6px 9px;
                       border-left:2px solid #BFDBFE;",
                obj)
          )
      )
    })

    do.call(tagList, tarjetas)
  })
}

# ============================================================
# install.packages(c("shiny","DBI","RPostgres","dplyr","stringr",
#   "scales","bslib","highcharter","reactable","openxlsx",
#   "purrr","shinyWidgets"))
# ============================================================
shinyApp(ui=ui, server=server)
