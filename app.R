# ============================
# app.R — Dashboard Elecciones 2025
# Versión con barras HTML, tamaño proporcional a electores,
# filtros región + comuna y mapa avanzado.
# ============================


library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(leaflet)
library(stringi)

# ---- Normaliza nombres de comuna ----
normalizar_comuna <- function(x) {
  x |>
    toupper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    trimws()
}

# ---- Cargar centroides ----
centroides <- read.csv("centroides_comunas_chile_wgs84.csv",
                       encoding = "UTF-8")

# ---- Leer votos ----
leer_datos <- function() {
  read_excel("votos_2025.xlsx")
}

# ---- FUNCIÓN ARREGLADA PARA BARRAS HTML ----
barra_html <- function(valor, label, color = "#d9534f") {
  
  # Vector seguro y acotado
  v <- suppressWarnings(as.numeric(valor))
  if (length(v) != 1) v <- v[1]
  if (is.na(v)) v <- 0
  v <- pmin(pmax(v, 0), 100)
  
  paste0(
    "<div style='margin-top:5px;'>",
    "<div style='font-size:12px;'>", label, " (", round(v,1), "%)</div>",
    "<div style='background:#eee; height:10px; width:100%; border-radius:3px;'>",
    "<div style='height:10px; width:", v, "%; background:", color, 
    "; border-radius:3px;'></div>",
    "</div>",
    "</div>"
  )
}

# ================= UI =================
ui <- fluidPage(
  titlePanel("Dashboard Elecciones 2025"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("region_ui"),
      uiOutput("comuna_ui"),
      selectInput(
        inputId = "candidato",
        label   = "Candidato:",
        choices = c("Jara", "Kast", "Parisi", "Kaiser"),
        selected = "Jara"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa",
                 h3("Mapa georreferenciado"),
                 leafletOutput("mapa", height = "600px")
        ),
        tabPanel("Tabla",
                 h3("Tabla resumen"),
                 tableOutput("tabla_resumen")
        ),
        tabPanel("Gráfico",
                 h3("Votación por comuna"),
                 plotOutput("plot_candidato", height = "500px")
        )
      )
    )
  )
)

# ================= SERVER =================
server <- function(input, output, session) {
  
  # DATA UNIFICADA
  datos <- reactive({
    votos <- leer_datos() %>%
      mutate(comuna_key = normalizar_comuna(COMUNA))
    
    cent <- centroides %>%
      mutate(comuna_key = normalizar_comuna(Comuna))
    
    votos %>% left_join(
      cent %>% select(comuna_key, lon, lat),
      by = "comuna_key"
    )
  })
  
  # UI dinámico Región
  output$region_ui <- renderUI({
    df <- datos()
    selectInput(
      "region", "Región:",
      choices = c("Todas", sort(unique(df$Región))),
      selected = "Todas"
    )
  })
  
  # UI dinámico Comuna
  output$comuna_ui <- renderUI({
    df <- datos()
    if (input$region != "Todas")
      df <- df %>% filter(Región == input$region)
    
    selectInput(
      "comuna", "Comuna:",
      choices = c("Todas", sort(unique(df$COMUNA))),
      selected = "Todas"
    )
  })
  
  # Filtrado Región
  datos_filtrados_region <- reactive({
    df <- datos()
    if (input$region != "Todas")
      df <- df %>% filter(Región == input$region)
    df
  })
  
  # Filtrado Comuna
  datos_filtrados <- reactive({
    df <- datos_filtrados_region()
    if (input$comuna != "Todas")
      df <- df %>% filter(COMUNA == input$comuna)
    df
  })
  
  # ---- Gráfico ----
  output$plot_candidato <- renderPlot({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    
    df <- df %>% mutate(valor = .data[[input$candidato]]) %>%
      arrange(valor) %>%
      mutate(COMUNA = factor(COMUNA, levels = COMUNA))
    
    ggplot(df, aes(COMUNA, valor)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste0("Votación de ", input$candidato),
        x = "Comuna", y = "%"
      ) +
      theme_minimal()
  })
  
  # ---- Tabla ----
  output$tabla_resumen <- renderTable({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    df %>%
      mutate(valor = .data[[input$candidato]]) %>%
      arrange(desc(valor)) %>%
      select(Región, COMUNA, valor)
  })
  
  # ---- MAPA ----
  output$mapa <- renderLeaflet({
    
    df <- datos_filtrados()
    req(nrow(df) > 0)
    
    df <- df %>% 
      filter(!is.na(lon), !is.na(lat)) %>%
      mutate(
        valor = .data[[input$candidato]],
        electores = `Electores en padrón definitivo nov 25`
      )
    
    max_padron <- max(df$electores, na.rm = TRUE)
    
    # Candidatos disponibles
    candidatos_disponibles <- c("Jara", "Kast", "Parisi", "Kaiser")
    
    # Colores fijos + paleta aleatoria para los demás
    base_colors <- c(
      Jara   = "#ff0000",  # rojo
      Kast   = "#0000ff",  # azul
      Parisi = "#00aa00"   # verde
    )
    random_palette <- c(
      "#ff7f00", "#6a3d9a", "#b15928",
      "#a6cee3", "#b2df8a", "#fb9a99"
    )
    
    # Asignar colores a todos los candidatos (los no definidos obtienen uno aleatorio)
    candidate_colors <- base_colors
    otros_para_color <- setdiff(candidatos_disponibles, names(base_colors))
    if (length(otros_para_color) > 0) {
      set.seed(123)  # opcional, para reproducibilidad
      for (nm in otros_para_color) {
        candidate_colors[nm] <- sample(random_palette, 1)
      }
    }
    
    df <- df %>%
      rowwise() %>%
      mutate(
        radio = 3 + 10 * sqrt(electores / max_padron),
        
        # Bloque HTML para "otros candidatos"
        otros_html = {
          cand_sel <- input$candidato
          otros <- setdiff(candidatos_disponibles, cand_sel)
          
          # Valores de todos los candidatos en esta fila
          valores_cand <- c(
            Jara   = get("Jara"),
            Kast   = get("Kast"),
            Parisi = get("Parisi"),
            Kaiser = get("Kaiser")
          )
          
          valores_otros <- valores_cand[otros]
          orden <- order(valores_otros, decreasing = TRUE, na.last = TRUE)
          otros_ordenados <- otros[orden]
          colores_otros <- candidate_colors[otros_ordenados]
          
          paste0(
            mapply(
              function(cn, col) {
                barra_html(valores_cand[cn], cn, col)
              },
              otros_ordenados,
              colores_otros,
              SIMPLIFY = TRUE,
              USE.NAMES = FALSE
            ),
            collapse = ""
          )
        },
        
        # --- Bloque HTML para Género (ordenado de mayor a menor) ---
        genero_html = {
          genero_vals <- c(
            Mujeres = mujeres_p,
            Hombres = hombres_p
          )
          genero_colors <- c(
            Mujeres = "#ff6384",
            Hombres = "#36a2eb"
          )
          ord <- order(genero_vals, decreasing = TRUE, na.last = TRUE)
          paste0(
            mapply(
              function(nm) barra_html(genero_vals[[nm]], nm, genero_colors[[nm]]),
              names(genero_vals)[ord],
              SIMPLIFY = TRUE,
              USE.NAMES = FALSE
            ),
            collapse = ""
          )
        },
        
        # --- Bloque HTML para Edad (ordenado de mayor a menor) ---
        edad_html = {
          edad_vals <- c(
            `18-19` = `Edad 18-19_p`,
            `20-29` = `Edad 20-29_p`,
            `30-39` = `Edad 30-39_p`,
            `40-49` = `Edad 40-49_p`,
            `50-59` = `Edad 50-59_p`,
            `60-69` = `Edad 60-69_p`,
            `70-79` = `Edad 70-79_p`,
            `80-89` = `Edad 80-89_p`,
            `90+`   = `90 o más_p`
          )
          edad_colors <- c(
            `18-19` = "#8dd3c7",
            `20-29` = "#ffffb3",
            `30-39` = "#bebada",
            `40-49` = "#fb8072",
            `50-59` = "#80b1d3",
            `60-69` = "#fdb462",
            `70-79` = "#b3de69",
            `80-89` = "#fccde5",
            `90+`   = "#bc80bd"
          )
          ord <- order(edad_vals, decreasing = TRUE, na.last = TRUE)
          paste0(
            mapply(
              function(nm) barra_html(edad_vals[[nm]], nm, edad_colors[[nm]]),
              names(edad_vals)[ord],
              SIMPLIFY = TRUE,
              USE.NAMES = FALSE
            ),
            collapse = ""
          )
        },
        
        # --- Popup completo: dos columnas (izq: comuna+votos, der: género+edad) ---
        popup_text = paste0(
          "<div style='display:flex; gap:12px; min-width:340px;'>",
          "<div style='flex:1; min-width:0;'>",
          "<b>", COMUNA, "</b> (", Región, ")<br>",
          "<b>", input$candidato, ":</b> ", round(valor, 1), "%<br>",
          "<b>Padrón definitivo:</b> ", electores, "<br><br>",
          "<b>Otros candidatos</b><br>",
          otros_html,
          "</div>",
          "<div style='flex:1; min-width:0;'>",
          "<b>Género</b><br>",
          genero_html,
          "<br>",
          "<b>Edad</b><br>",
          edad_html,
          "</div>",
          "</div>"
        )
      ) %>%
      ungroup()
    
    pal <- colorNumeric("YlOrRd", df$valor)
    
    # bounding box
    bb <- df %>% summarise(
      min_lon = min(lon), max_lon = max(lon),
      min_lat = min(lat), max_lat = max(lat)
    )
    
    leaflet(df) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~radio,
        color = ~pal(valor),
        fillOpacity = 0.8,
        stroke = FALSE,
        label = ~paste0(COMUNA, " (", Región, ")"),
        popup = ~popup_text,
        popupOptions = popupOptions(maxWidth = 500)
      ) %>%
      fitBounds(bb$min_lon, bb$min_lat, bb$max_lon, bb$max_lat) %>%
      addLegend("bottomright", pal = pal, values = ~valor,
                title = paste0("% votos ", input$candidato))
  })
}

shinyApp(ui, server)

