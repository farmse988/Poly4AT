#' Open Shiny App
#'
#' Launches the Poly4AT Shiny application for querying geographic data based on coordinates.
#'
#' @return A Shiny application.
#' @export
#'
#' @examples
#' poly4AT_processor()



poly4AT_processor <- function(...) {
  library(shiny)
  library(sf)
  library(leaflet)
  library(geojsonsf)
  library(httr)
  library(jsonlite)
  library(bslib)
  library(shinydashboard)
  library(DT)
  library(writexl)
  library(leaflet.extras)


  ui <- dashboardPage(
    dashboardHeader(title = "Poly4AT"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Einzelkoordinate abfragen", tabName = "Singlerequest", icon = icon("location-dot")),
        menuItem("Koordinatenliste abfragen", tabName = "Mulitrequest", icon = icon("location-dot")),
        menuItem("Über Poly4AT", tabName = "About", icon = icon("circle-info"))
      )
    ),
    skin = "black",
    dashboardBody(
      tabItems(
        tabItem(tabName = "Singlerequest",
                fluidRow(
                  column(width = 4,
                         textInput("latitude", "Latitude:", value = ""),
                         textInput("longitude", "Longitude:", value = ""),
                         selectInput("year", "Jahr auswählen:", choices = NULL),
                         actionButton("updateMap", "Polygone anzeigen")),
                  column(width = 8,
                         conditionalPanel(condition = "!output.loading",
                                          leafletOutput("map")))
                )),
        tabItem(tabName = "Mulitrequest",
                fluidRow(
                  column(width = 4,
                         box(title = "Daten-Upload", status = "primary", solidHeader = TRUE, width = 12,
                             fileInput('file1', 'Excel-Datei mit Koordinaten hochladen
                                     (Spalte 1: Name, Spalte 2: latitude, Spalte 3: longitude)',
                                       accept = c(".xlsx")),
                             actionButton("showTable", "Tabelle mit Koordinaten anzeigen", icon = icon("table"), style = "margin-bottom: 10px; width: 100%;"),
                             br(),
                             dataTableOutput("file2")
                         )
                  ),

                  column(width = 8,
                         box(maximizable = TRUE,
                             title = "Jahresauswahl und Polygone", status = "primary", solidHeader = TRUE, width = 12,
                             fluidRow(
                               column(width = 6,
                                      selectInput("yearmulti", "Jahr auswählen:", choices = NULL, width = '100%')),
                               column(width = 6,
                                      actionButton("showmapmulti", "Polygone anzeigen", icon = icon("map"), style = "width: 100%;"))
                             ),
                             br(),

                             conditionalPanel(condition = "!output.loading",
                                              leafletOutput("mapmulti", height = "500px")),
                             br()),

                         box(
                           width = 12,
                           actionButton("showshape", "Informationen anzeigen", icon = icon("info-circle"), style = "margin-bottom: 10px; width: 100%;"),
                           br(),
                           dataTableOutput("shape"),
                           style = "overflow-y: auto; overflow-x: auto;"),

                         box(
                           downloadButton(outputId = "downloadShape", label = "Download Shape-Datei", style = "width: 100%;")
                         )
                  )
                )
        ),

        tabItem(
          tabName = "About",
          fluidRow(
            column(
              width = 8, offset = 2,
              style = "text-align: justify;",
              h2(strong("Über Poly4AT")),
              div(style = "margin-bottom: 20px;",
                  p("Diese Website wurde entwickelt, um die API-Schnittstelle zu INVEKOS Feldstück-Polygone auch ohne Programmierkenntnisse nutzen zu können. Die App ermöglicht es, eine einzelne Koordinate abzufragen sowie eine Koordinatenliste hochzuladen, um Informationen über die Schläge zu erhalten.")
              ),

              h3(strong("Datenweiterverarbeitung")),
              div(style = "margin-bottom: 20px;",
                  p("Die auf dieser Website angezeigten Daten stammen von der INVEKOS API-Schnittstelle. Die Weiterverwendung der Daten ist nur mit entsprechender Zitierung von AMA INVEKOS erlaubt."),

                  p("Für die Daten haben wir folgenden Zitiervorschlag:"),
                  tags$blockquote(
                    "AMA. (Jahr der Abfrage). OGC Features API [API]. Abgerufen am 1. Oktober 2024, von https://gis.lfrz.gv.at/ogcapi009501/ogc/features/api",
                    style = "font-style: italic; color: #555;"
                  ),

                  p("Für Poly4AT verwenden Sie die Citation in R:"),
                  tags$blockquote(
                    "Wieser S (2024). Poly4AT: Access INVEKOS API for Field Polygons. R package version 1.0.",
                    style = "font-style: italic; color: #555;"
                  ),
                  p("Hier ist der BibTeX-Eintrag für LaTeX-Benutzer:"),
                  tags$blockquote(
                    "@Manual{Poly4AT,\n",
                    "  title = {Poly4AT: Access INVEKOS API for Field Polygons},\n",
                    "  author = {Sebastian Wieser},\n",
                    "  year = {2024},\n",
                    "  note = {R package version 1.0}\n",
                    "}",
                    style = "font-family: monospace; font-size: 14px; color: #555;"
                  )
              ),

              h3(strong("Datenschutz")),
              div(style = "margin-bottom: 20px;",
                  p("Bitte beachten Sie, dass wir keine persönlichen Daten ohne Zustimmung sammeln oder weitergeben."),
                  tags$ul(
                    tags$li("Ihre Inputdaten werden nicht gespeichert und sind nach der Sitzung gelöscht.")
                  )
              ),

              h3(strong("Lizenz")),
              div(style = "margin-bottom: 20px;",
                  p("Der Inhalt und der Quellcode dieser Website unterliegen der Lizenz [SWT]. Dies bedeutet, dass Sie den Quellcode verwenden, modifizieren und weiterverbreiten können, sofern die ursprüngliche Urheberschaft anerkannt wird.")
              ),

              h3(strong("Kontakt")),
              p("Wenn Sie Fragen oder Anmerkungen bezüglich Poly4AT haben, kontaktieren Sie uns bitte unter ",
                a("poly4at@gmail.com", href = "mailto:poly4at@gmail.com"))
            )
          )
        )
      )
    )
  )





  server <- function(input, output, session) {

    all_filtered_sf <- reactiveVal(NULL)
    coordinatesinput <- reactiveValues(coordinates = NULL)

    output$loading <- renderUI({
      actionButton("updateMap", "Update Map")
    })


    load(system.file("data/austria_boundary.RData", package = "Poly4AT"))

    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
        addFullscreenControl() %>%
        setView(lng = 14.5501, lat = 47.5162, zoom = 7) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Street"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    observe({
      res <- GET("https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/openapi?f=application%2Fvnd.oai.openapi%2Bjson%3Bversion%3D3.0")
      data <- fromJSON(content(res, "text"))
      InfoDaten <- as.data.frame(data$components$parameters$collectionId$schema$enum)

      colnames(InfoDaten)[1] <- "Year"
      InfoDaten <- InfoDaten[grep("invekos_schlaege", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]
      InfoDaten <- InfoDaten[grep("polygon", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]

      InfoDaten$Year <- sub("i009501:", "", InfoDaten$Year)
      updateSelectInput(session, "year", choices = InfoDaten$Year)
    })

    observeEvent(input$updateMap, {
      latitude <- as.numeric(input$latitude)
      longitude <- as.numeric(input$longitude)
      selected_year <- input$year

      point <- st_point(c(longitude, latitude))
      point_sf <- st_sfc(point, crs = st_crs(austria_boundary))


      if (st_contains(austria_boundary, point_sf, sparse = FALSE)) {
        bbox <- paste(longitude - 0.001, latitude - 0.001, longitude + 0.001, latitude + 0.001, sep = ",")

        base_url <- "https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/collections/"

        get_thredds_url <- function(collection, bbox) {
          part1 <- base_url
          part2 <- collection
          part3 <- "/items?limit=100&bbox="
          part4 <- bbox
          part5 <- "&filter-lang=cql-text&additionalProp1="
          return(paste0(part1, part2, part3, part4, part5))
        }

        url <- get_thredds_url(selected_year, bbox)

        AnfrageDaten2 <- tryCatch({
          geojson_sf(url)
        }, error = function(e) {
          NULL
        })

        if (!is.null(AnfrageDaten2)) {
          contains <- st_contains(AnfrageDaten2, point)
          contains_ids <- which(lengths(contains) > 0)

          leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(data = AnfrageDaten2,
                        popup = ~paste("Fläche ha: ", sprintf("%.1f", sl_flaeche_brutto_ha),
                                       "<br>",
                                       "Schlagnutzung: ", snar_bezeichnung)) %>%
            addMarkers(lng = longitude, lat = latitude) %>%
            setView(lng = longitude, lat = latitude, zoom = 15)
        } else {
          showModal(modalDialog(
            title = "Error",
            "Failed to retrieve data from the server. Please try again later."
          ))
        }
      } else {
        showModal(modalDialog(
          title = "Fehler",
          "Die eingegebenen Koordinaten liegen außerhalb von Österreich. Bitte versuchen Sie es erneut."
        ))
      }
    })

    observeEvent(input$showTable, {
      output$file2 <- renderDataTable({
        inFile <- head(input$file1)
        if (is.null(inFile))
          return(NULL)
        readxl::read_excel(inFile$datapath)
      })
    })

    observe({
      res <- GET("https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/openapi?f=application%2Fvnd.oai.openapi%2Bjson%3Bversion%3D3.0")
      data <- fromJSON(content(res, "text"))
      InfoDaten <- as.data.frame(data$components$parameters$collectionId$schema$enum)

      colnames(InfoDaten)[1] <- "Year"
      InfoDaten <- InfoDaten[grep("invekos_schlaege", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]
      InfoDaten <- InfoDaten[grep("polygon", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]

      InfoDaten$Year <- sub("i009501:", "", InfoDaten$Year)
      updateSelectInput(session, "yearmulti", choices = InfoDaten$Year)
    })

    output$mapmulti <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
        addFullscreenControl() %>%
        setView(lng = 14.5501, lat = 47.5162, zoom = 7) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Street"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    observeEvent(input$showmapmulti, {
      selected_year <- input$yearmulti
      inFile <- readxl::read_excel(input$file1$datapath)

      coordinates <- inFile[, c("Name", "latitude", "longitude")]


      valid_coordinates <- list()
      invalid_coordinates <- list()

      for (i in 1:nrow(coordinates)) {
        point <- st_point(c(coordinates$longitude[i], coordinates$latitude[i]))
        point_sf <- st_sfc(point, crs = st_crs(austria_boundary))

        if (st_contains(austria_boundary, point_sf, sparse = FALSE)) {
          valid_coordinates[[length(valid_coordinates) + 1]] <- coordinates[i, ]
        } else {
          invalid_coordinates[[length(invalid_coordinates) + 1]] <- coordinates[i, ]
        }
      }

      if (length(invalid_coordinates) > 0) {
        showModal(modalDialog(
          title = "Fehler",
          paste("Die folgenden Koordinaten liegen außerhalb von Österreich:",
                paste(sapply(invalid_coordinates, function(x) x[1]), collapse = ", "))
        ))
      }

      coordinates <- do.call(rbind, valid_coordinates)

      coordinates$bbox <- mapply(function(lon, lat) {
        paste(lon - 0.001, lat - 0.001, lon + 0.001, lat + 0.001, sep = ",")
      }, coordinates$longitude, coordinates$latitude)

      base_url <- "https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/collections/"

      get_thredds_url <- function(collection, bbox) {
        part1 <- base_url
        part2 <- collection
        part3 <- "/items?limit=100&bbox="
        part4 <- bbox
        part5 <- "&filter-lang=cql-text&additionalProp1="
        return(paste0(part1, part2, part3, part4, part5))
      }

      ergebnisse_liste <- list()

      for (i in 1:nrow(coordinates)) {
        bbox <- coordinates$bbox[i]
        url <- get_thredds_url(selected_year, bbox)
        ergebnisse_liste[[coordinates$Name[i]]] <- geojson_sf(url)
      }

      filtered_polygons <- list()

      for (i in seq_along(ergebnisse_liste)) {
        name <- names(ergebnisse_liste)[i]
        current_result <- ergebnisse_liste[[i]]
        current_point <- st_point(c(coordinates$longitude[i], coordinates$latitude[i]))
        current_point <- st_sfc(current_point, crs = 4326)
        contains <- st_contains(current_result, current_point)
        contains_ids <- which(lengths(contains) > 0)
        filtered_polygons[[name]] <- current_result[contains_ids, ]
      }

      all_filtered_sf(all_filtered_sf <- do.call(rbind, filtered_polygons))

      observe({
        coordinatesinput$coordinates <- coordinates
      })

      leafletProxy("mapmulti") %>%
        clearShapes() %>%
        addProviderTiles('Esri.WorldImagery') %>%
        addPolygons(data = all_filtered_sf(),
                    popup = paste("Fläche ha: ", sprintf("%.1f", all_filtered_sf()$sl_flaeche_brutto_ha),
                                  "<br>",
                                  "Schlagnutzung: ", all_filtered_sf()$snar_bezeichnung)) %>%
        addMarkers(data = coordinates,
                   popup = paste(coordinates$Name)) %>%
        setView(lng = mean(coordinates$longitude), lat = mean(coordinates$latitude), zoom = 12)
    })

    observeEvent(input$showshape, {
      output$shape <- renderDataTable({
        tableshape <- st_drop_geometry(all_filtered_sf())
        tableshape$Name <- rownames(tableshape)
        coordinates <- coordinatesinput$coordinates
        tableshape <- merge(tableshape, coordinates[,-4], by="Name")
        tableshape
      })
    })

    shape_map <- all_filtered_sf

    output$downloadShape <- downloadHandler(
      filename = function() {
        "Poly4AT.gpkg"
      },
      content = function(file) {
        st_write(all_filtered_sf(), file)
      },
      contentType = "application/octet-stream"
    )
  }

  shinyApp(ui = ui, server = server)
}


poly4AT_processor()
