library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(readxl)
library(htmltools)

# =========================
# 1. CARGA DE DATOS
# =========================

datos <- readxl::read_excel(
  "LISTO-PARA MAPEAR.xlsx"
)

comunas <- sf::st_read(
  "SHAPES COMUNAS/comunas.shp",
  quiet = TRUE
)

mapa_join <- comunas %>%
  left_join(datos, by = c("comuna" = "comuna"))

# =========================
# 2. FUNCIONES AUXILIARES
# =========================

get_variable <- function(indicador, anio) {
  if (indicador == "total" && anio == "2010") return("IndCente2010")
  if (indicador == "total" && anio == "2022") return("IndCente2022")
  
  if (indicador == "mujeres" && anio == "2010") return("MujIndCente2010")
  if (indicador == "mujeres" && anio == "2022") return("MujIndCente2022")
  
  if (indicador == "masculinidad" && anio == "2010") return("Indicemasculinidad2010")
  if (indicador == "masculinidad" && anio == "2022") return("Indicemasculinidad2022")
  
  if (indicador == "variacion_total") return("VariacionCentTotal")
  if (indicador == "variacion_mujeres") return("VariacionCentMujeres")
  if (indicador == "variacion_masculinidad") return("VariacionIndicemasculinidad")
  
  NULL
}

get_titulo_leyenda <- function(indicador, anio) {
  
  if (indicador == "total") {
    return("Centenarios por cada 100.000 habitantes")
  }
  
  if (indicador == "mujeres") {
    return("Mujeres centenarias por cada 100.000 mujeres")
  }
  
  if (indicador == "masculinidad") {
    return("Varones centenarios por cada 100 mujeres centenarias")
  }
  
  if (indicador == "variacion_total") {
    return("Cambio porcentual del índice de centenarios entre 2010 y 2022")
  }
  
  if (indicador == "variacion_mujeres") {
    return("Cambio porcentual del índice de mujeres centenarias entre 2010 y 2022")
  }
  
  if (indicador == "variacion_masculinidad") {
    return("Cambio en la relación entre varones centenarios y mujeres centenarias (2010–2022)")
  }
}


get_pal <- function(indicador, valores) {
    
    if (indicador == "total") {
      return(colorNumeric(c("#f3e5c8", "#f4b183", "#d55a3a", "#7f2704"), valores, na.color = "#dddddd"))
    }
    
    if (indicador == "mujeres") {
      return(colorNumeric(c("#fde0ef", "#f768a1", "#c51b8a", "#7a0177"), valores, na.color = "#dddddd"))
    }
    
    if (indicador == "masculinidad") {
      return(colorNumeric(c("#e8f1fa", "#9ecae1", "#4292c6", "#08519c"), valores, na.color = "#dddddd"))
    }
    
    if (grepl("variacion", indicador)) {
      return(colorNumeric(c("#f3e5c8", "#f4b183", "#d55a3a", "#7f2704"), valores, na.color = "#dddddd"))
    }
  }
get_hallazgo <- function(indicador, anio) {
  
  if (indicador == "total" && anio == "2010") {
    return("La longevidad extrema ya mostraba una distribución desigual: varias comunas del norte concentraban niveles relativamente más altos.")
  }
  
  if (indicador == "total" && anio == "2022") {
    return("Doce años después persiste ese patrón territorial, aunque con ciertos reacomodamientos internos.")
  }
  
  if (indicador == "mujeres" && anio == "2010") {
    return("La distribución femenina explicaba gran parte del mapa general de centenarios.")
  }
  
  if (indicador == "mujeres" && anio == "2022") {
    return("La supervivencia femenina continúa siendo el principal sostén territorial de la centenariedad.")
  }
  
  if (indicador == "masculinidad" && anio == "2010") {
    return("La brecha entre varones y mujeres centenarias variaba entre comunas, revelando diferencias territoriales de supervivencia.")
  }
  
  if (indicador == "masculinidad" && anio == "2022") {
    return("Persisten contrastes comunales en la composición por sexo, dentro de un fenómeno claramente feminizado.")
  }
  
  if (indicador == "variacion_total") {
    return("El crecimiento fue desigual: algunas comunas aumentaron su peso relativo más que otras.")
  }
  
  if (indicador == "variacion_mujeres") {
    return("Permite identificar dónde el crecimiento reciente estuvo impulsado principalmente por mujeres.")
  }
  
  if (indicador == "variacion_masculinidad") {
    return("La brecha de género no evolucionó igual en toda la ciudad: algunas comunas se equilibraron parcialmente y otras reforzaron la feminización.")
  }
  
  ""
}

# =========================
# 3. UI
# =========================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: Arial, sans-serif; }
.titulo-principal {
  font-size: 52px;
  font-weight: 800;
  color: #1f2d3d;
  margin-bottom: 6px;
  text-align: center;
}

.subtitulo {
  font-size: 26px;
  color: #555;
  margin-bottom: 6px;
  text-align: center;
}

.bajada {
  font-size: 16px;
  color: #666;
  margin-bottom: 18px;
  text-align: center;
}
      .panel-box { background: #f8f9fa; padding: 14px; border-radius: 10px; margin-bottom: 14px; }
      .hallazgo-box { background: #fff8e7; padding: 12px; border-left: 4px solid #d4a017; border-radius: 6px; }
    "))
  ),
  
  fluidRow(
    column(
      12,
      align = "center",
      div(class = "titulo-principal", "Geografía de los centenarios en CABA"),
      div(class = "subtitulo", "Por comunas, 2010 y 2022"),
      div(class = "bajada", "Los datos por sí solos no explican el fenómeno. El análisis espacial permite revelar patrones ocultos de longevidad extrema en la ciudad.")
    )
  ),
  
  fluidRow(
    column(
      3,
      div(
        class = "panel-box",
        h4("¿Qué querés explorar?"),
        radioButtons(
          "indicador",
          label = NULL,
          choices = c(
            "Dónde se concentran los centenarios" = "total",
            "Dónde se concentran las mujeres centenarias" = "mujeres",
            "Cómo se feminiza la longevidad extrema" = "masculinidad",
            "Dónde más crecieron los centenarios" = "variacion_total",
            "Dónde más crecieron las mujeres centenarias" = "variacion_mujeres",
            "Dónde más cambió la brecha de género" = "variacion_masculinidad"
          ),
          selected = "total"
        ),
        
        conditionalPanel(
          condition = "input.indicador == 'total' || input.indicador == 'mujeres' || input.indicador == 'masculinidad'",
          h4("Año"),
          radioButtons("anio", NULL, choices = c("2010", "2022"), selected = "2022", inline = TRUE)
        ),
        
        checkboxInput("mostrar_ranking", "Mostrar ranking", TRUE),
        checkboxInput("mostrar_popup", "Mostrar popup", TRUE)
      )
    ),
    
    column(
      7,
      leafletOutput("mapa", height = 830)
    ),
    
    column(
      2,
      conditionalPanel(
        condition = "input.mostrar_ranking == true",
        div(
          class = "panel-box",
          h4("Top comunas"),
          tableOutput("ranking")
        )
      ),
      div(
        class = "hallazgo-box",
        h4("¿Qué revela este mapa?"),
        textOutput("hallazgo")
      )
    )
  )
)

# =========================
# 4. SERVER
# =========================

server <- function(input, output, session) {
  
  datos_mapa <- reactive({
    
    var <- get_variable(input$indicador, input$anio)
    
    df <- mapa_join
    df$valor_actual <- df[[var]]
    
    df$total_sel <- ifelse(input$anio == "2010", df$Cente2010, df$Cente2022)
    df$varones_sel <- ifelse(input$anio == "2010", df$VaronesCente2010, df$VaronesCente2022)
    df$mujeres_sel <- ifelse(input$anio == "2010", df$MujCente2010, df$MujCente2022)
    
    df
  })
  
  output$mapa <- renderLeaflet({
    
    df <- datos_mapa()
    pal <- get_pal(input$indicador, df$valor_actual)
    titulo_leyenda <- get_titulo_leyenda(input$indicador, input$anio)
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(valor_actual),
        weight = 2.2,
        color = "#3f3f3f",
        opacity = 0.9,
        fillOpacity = 0.85,
        popup = ~if (isTRUE(input$mostrar_popup)) {
          paste0(
            "<b>Comuna: </b>", comuna, "<br>",
            "<b>", titulo_leyenda, ":</b> ", round(valor_actual, 1), "<br>",
            "<b>Total centenarios:</b> ", total_sel, "<br>",
            "<b>Varones centenarios:</b> ", varones_sel, "<br>",
            "<b>Mujeres centenarias:</b> ", mujeres_sel
          )
        } else {
          NULL
        }
      ) %>%
      addLabelOnlyMarkers(
        data = st_centroid(df),
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        label = ~as.character(comuna),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "font-weight" = "700",
            "font-size" = "16px",
            "color" = "#1f2d3d",
            "text-shadow" = "1px 1px 2px white"
          )
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~valor_actual,
        title = titulo_leyenda,
        position = "bottomright"
      )
  })
  
  output$ranking <- renderTable({
    
    df <- datos_mapa() |>
      st_drop_geometry() |>
      select(comuna, valor_actual) |>
      arrange(desc(valor_actual)) |>
      slice(1:5)
    
    df$comuna <- as.integer(df$comuna)
    df$valor_actual <- sprintf("%.1f", df$valor_actual)
    
    names(df) <- c("Comuna", "Valor")
    df
  })
  
  output$hallazgo <- renderText({
    get_hallazgo(input$indicador, input$anio)
  })
}

shinyApp(ui, server)

