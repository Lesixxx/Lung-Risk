library(shiny)
library(shinythemes) # Dodanie pakietu do stylów

ui <- fluidPage(
  theme = shinytheme("flatly"), # Wybór tematu z shinythemes
  titlePanel("LungRisk: Analiza ryzyka raka płuc"),
  
  tabsetPanel(
    # Strona główna
    tabPanel("Strona główna",
             fluidRow(
               column(
                 12,
                 align = "center", # Wyrównanie do środka
                 h1("Witaj w aplikacji LungRisk!", style = "color: #2c3e50; font-weight: bold;"),
                 tags$img(src = "https://www.aktualnosci-medyczne.pl/wp-content/uploads/2019/11/3-768x768.png", height = "300px", style = "margin-bottom: 20px;"), # Zdjęcie lub grafika
                 div(
                   style = "text-align: justify; font-size: 16px; margin: 20px;",
                   HTML(paste(
                     "<p>W dzisiejszych czasach choroby nowotworowe stanowią jedno z największych wyzwań dla systemów opieki zdrowotnej na całym świecie.</p>",
                     "<p><b>Wczesne wykrywanie</b> oraz precyzyjne przewidywanie ryzyka zachorowania na raka płuc są kluczowe dla skutecznego leczenia i zwiększenia szans na przeżycie pacjentów.</p>",
                     "<p><i>LungRisk</i> to aplikacja webowa, która pozwala analizować ryzyko raka płuc na podstawie różnych czynników zdrowotnych i demograficznych.</p>",
                     "<p>Przejdź do zakładki <b>'Charakterystyka zbioru danych'</b>, aby zapoznać się z danymi wejściowymi.</p>"
                   )))))
    ),
    
    # Druga zakładka: Charakterystyka zbioru danych
    tabPanel("Charakterystyka zbioru danych", 
             sidebarLayout(
               sidebarPanel(
                 uiOutput("variableSelector")  #Wybór zmiennej
               ),
               mainPanel(
                 h3(textOutput("selectedVariable")),
                 plotOutput("variablePlot"), #Wyświetlenie wykresu dla danej zmiennej
                 tableOutput("variableSummary"), 
                 h3("Opis: "),
                 textOutput("variableDescription")
               )
             )
    ),
    
    # Trzecia zakładka: Porównania zmiennych
    tabPanel("Porównanie zmiennych", 
             sidebarLayout(
               sidebarPanel(
                 h3("Opcje porównania"),
                 selectInput(
                   "variable", 
                   "Wybierz zmienną do porównania:",
                   choices = setdiff(names(read.csv("survey lung cancer(2).csv")), "LUNG_CANCER"),
                   selected = "GENDER"
                 )
               ),
               mainPanel(
                 h3("Wykres porównania"),
                 plotOutput("comparisonPlot"), #Wyświetlenie wykresu z porównaniem
                 h3("Opis porównania"),
                 textOutput("comparisonDescription") #Wyświetlenie interpretacji
               )
             )
    ),
    
    # Czwarta zakładka: Tworzenie i wybór modeli
    tabPanel("Tworzenie i wybór modeli",
             h3("Wybierz model do analizy"),
             selectInput(
               inputId = "selectedModel",
               label = "Wybierz model:",
               choices = c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)",
                           "Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")
             ),
             verbatimTextOutput("modelFormula"), #Wyświetlenie postaci modelu
             
             h4("Interpretacja modelu:"),
             uiOutput("modelInterpretation"),  # Wyświetlenie interpretacji współczynników modelu
             h4("Pobierz wybrany model:"),
             downloadButton("downloadModel", "Pobierz model w formacie CSV") # Przycisk eksportu modelu
    ),
    
    # Piąta zakładka: Porównanie modeli
    tabPanel("Porównanie modeli",
             h3("Porównanie wybranych modeli logistycznych i probitowych"),
             h4("Ocena dobroci dopasowania modeli"),
             tableOutput("modelComparison"), # Wyświetlenie porównania modeli
             downloadButton("downloadModelComparison", "Pobierz porównanie modeli"),  # Przycisk eksportu
             h4("Rekomendacja wyboru modelu"),
             uiOutput("rekomendacja")
    ),
    
    # Szósta zakładka: Ocena jakości predykcji
    tabPanel("Ocena jakości predykcji",
             fluidRow(
               column(6, 
                      h4("Tabela miar jakości predykcji - modele both"),
                      tableOutput("comparisonMetrics"), #Wyświetlenie miar jakości predykcji
                      downloadButton("downloadComparisonMetrics", "Pobierz miary jakości predykcji")  # Przycisk eksportu
               ),
               column(6, 
                      h4("Interpretacja"),
                      htmlOutput("tableInterpretation") # Wyświetlanie interpretacji obok tabeli
               )
             ),
             hr(),
             h4("Krzywe ROC"),
             fluidRow(
               column(12,
                      plotOutput("rocCurves"),
                      htmlOutput("rocInterpretation")  # Wyświetlenie interpretacji
               )
             )
    )
  )
)