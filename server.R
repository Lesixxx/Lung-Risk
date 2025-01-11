library(shiny)
library(dplyr)
library(ggplot2)
library(corrplot)
library(vcd)
library(car)
library(VIM)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(lmtest)
library(naniar)
library(gridExtra)
library(pscl)
library(pROC)
library(gridExtra)

# Ładowanie danych z pliku CSV
data <- read.csv("survey lung cancer(2).csv")


###  PRZYGOTOWANIE DANYCH #####

# Przekształcenie danych
data <- data %>%
  mutate(
    GENDER = as.factor(ifelse(GENDER == "M", 1, 0)),
    LUNG_CANCER = as.factor(ifelse(LUNG_CANCER == "YES", 1, 0))
  ) %>%
  mutate(across(
    c(SMOKING, YELLOW_FINGERS, ANXIETY, PEER_PRESSURE, CHRONIC.DISEASE,
      FATIGUE, ALLERGY, WHEEZING, ALCOHOL.CONSUMING, COUGHING, 
      SHORTNESS.OF.BREATH, SWALLOWING.DIFFICULTY, CHEST.PAIN),
    ~ as.factor(ifelse(. == 2, 1, 0))
  ))

# Podsumowanie danych
data<-as.data.frame(data)
summary(data)

### KONIEC PRZYGOTOWANIA DANYCH ###


server <- function(input, output, session) {
  
  output$homeText <- renderText({
    paste(
      "Witaj w aplikacji LungRisk!",
      "W dzisiejszych czasach choroby nowotworowe stanowią jedno z największych wyzwań dla systemów opieki zdrowotnej na całym świecie.",
      "Wczesne wykrywanie oraz precyzyjne przewidywanie ryzyka zachorowania na raka są kluczowe dla skutecznego leczenia i zwiększenia szans na przeżycie pacjentów.",
      "W tym kontekście, modele parametryczne oferują zaawansowane narzędzia analityczne, które mogą znacząco poprawić dokładność i efektywność przewidywania ryzyka zachorowania na raka płuc.",
      "LungRisk to aplikacja webowa, która pozwala analizować ryzyko raka płuc na podstawie różnych czynników zdrowotnych i demograficznych.",
      "Aby rozpocząć, przejdź do zakładki 'Charakterystyka zbioru danych', gdzie możesz zobaczyć charakterystykę zmiennych biorących udział w analizie.",
      sep = "\n\n" # Dodanie odstępów między paragrafami
    )
  })
  
  output$variableSelector <- renderUI({
    selectInput(
      "variable", 
      "Wybierz zmienną do analizy:", 
      choices = names(data),  # Pobieramy nazwy kolumn z pliku CSV
      selected = names(data)[1]
    )
  })
  
  # Wyświetlanie wybranej zmiennej
  output$selectedVariable <- renderText({
    paste("Wybrana zmienna:", input$variable)
  })
  
  # Wyświetlanie wykresu dla wybranej zmiennej
  output$variablePlot <- renderPlot({
    # Histogram dla zmiennej AGE
    if (input$variable == "AGE") {
      ggplot(data, aes(x = AGE)) +
        geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
        labs(x = "Wiek", y = "Częstość") +
        ggtitle("Histogram zmiennej AGE") +
        theme_minimal() + theme(aspect.ratio = 0.5)  # Proporcje osi: wysokość 2x większa niż szerokość
    } else {
      # Wykresy słupkowe (częstość) dla pozostałych zmiennych
      ggplot(data, aes_string(x = input$variable, fill = input$variable)) +
        geom_bar(position = "stack", width = 0.5) +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Dodanie etykiet
        labs(x = input$variable, y = "Liczba", fill = input$variable) +
        ggtitle(paste("Wykres zmiennej", input$variable)) +
        scale_x_discrete(labels = c("Nie", "Tak")) +
        scale_fill_discrete(labels = c("Nie", "Tak")) +
        theme_minimal() +
        theme(aspect.ratio = 0.5)
    }
  })
  
  # Charakterystyka zmiennej AGE
  output$variableSummary <- renderTable({
    if (input$variable == "AGE") {
      summary_data <- summary(data$AGE)
      data.frame(
        Właściwość = names(summary_data),
        Wartość = as.character(summary_data),
        row.names = NULL
      )
    } else {
      NULL  # Dla innych zmiennych brak charakterystyk
    }
  })
  
  # Opis zmiennych
  output$variableDescription <- renderText({
    switch(input$variable,
           "GENDER" = "Zmienna GENDER reprezentuje płeć osób w próbie. Możliwe wartości to M (mężczyźni) i F (kobiety).",
           "AGE" = "Zmienna AGE reprezentuje wiek osób w próbie. Histogram pokazuje rozkład wieku w populacji.",
           "SMOKING" = "Zmienna SMOKING reprezentuje nawyk palenia tytoniu. Wartość 1 oznacza osoby palące, a 0 osoby niepalące.",
           "YELLOW_FINGERS" = "Zmienna YELLOW_FINGERS reprezentuje występowanie żółtych palców, co może być objawem intensywnego palenia. Wartość 1 oznacza obecność tego objawu, a 0 jego brak.",
           "ANXIETY" = "Zmienna ANXIETY reprezentuje występowanie lęku u osób w próbie. Wartość 1 oznacza obecność lęku, a 0 jego brak.",
           "PEER_PRESSURE" = "Zmienna PEER_PRESSURE reprezentuje presję rówieśników odczuwaną przez osoby w próbie. Wartość 1 oznacza, że osoba odczuwa presję, a 0 że jej nie odczuwa.",
           "CHRONIC.DISEASE" = "Zmienna CHRONIC.DISEASE reprezentuje występowanie chorób przewlekłych u osób w próbie. Wartość 1 oznacza obecność choroby przewlekłej, a 0 jej brak.",
           "FATIGUE" = "Zmienna FATIGUE reprezentuje odczuwanie chronicznego zmęczenia przez osoby w próbie. Wartość 1 oznacza występowanie zmęczenia, a 0 jego brak.",
           "ALLERGY" = "Zmienna ALLERGY reprezentuje występowanie alergii u osób w próbie. Wartość 1 oznacza obecność alergii, a 0 jej brak.",
           "WHEEZING" = "Zmienna WHEEZING reprezentuje występowanie świszczącego oddechu. Wartość 1 oznacza obecność tego objawu, a 0 jego brak.",
           "ALCOHOL.CONSUMING" = "Zmienna ALCOHOL.CONSUMING reprezentuje spożywanie alkoholu przez osoby w próbie. Wartość 1 oznacza osoby spożywające alkohol, a 0 osoby niepijące.",
           "COUGHING" = "Zmienna COUGHING reprezentuje występowanie kaszlu. Wartość 1 oznacza obecność kaszlu, a 0 jego brak.",
           "SHORTNESS.OF.BREATH" = "Zmienna SHORTNESS.OF.BREATHE reprezentuje występowanie duszności u osób w próbie. Wartość 1 oznacza obecność duszności, a 0 jej brak.",
           "SWALLOWING.DIFFICULTY" = "Zmienna SWALLOWING.DIFFICULTY reprezentuje trudności z przełykaniem. Wartość 1 oznacza obecność tego objawu, a 0 jego brak.",
           "CHEST.PAIN" = "Zmienna CHEST.PAIN reprezentuje występowanie bólu w klatce piersiowej. Wartość 1 oznacza obecność bólu, a 0 jego brak.",
           "LUNG_CANCER" = "Zmienna LUNG_CANCER reprezentuje występowanie raka płuc. Wartość 1 oznacza jego występowanie, a 0 jego brak.",
           # Domyślna wartość
           "Proszę wybrać zmienną z listy powyżej, aby zobaczyć opis."
    )
  })
  
  # Porównanie zmiennych ze zmienną LUNG_CANCER
  output$comparisonPlot <- renderPlot({
    selected_var <- input$variable
    if (!is.null(selected_var) && selected_var != "LUNG_CANCER") {
      # Dodanie dynamicznych etykiet dla zmiennej AGE i innych zmiennych
      labeled_data <- data %>%
        mutate(
          label_var = if (selected_var == "AGE") {
            # Tworzenie przedziałów wiekowych dla AGE
            cut(AGE, breaks = c(-Inf, 50, 60, 70, Inf), labels = c("<50", "50-60", "60-70", ">70"))
          } else if (selected_var == "GENDER") {
            # Mapowanie 0 -> Kobieta, 1 -> Mężczyzna dla GENDER
            ifelse(GENDER == 1, "Mężczyzna", "Kobieta")
          } else {
            # Mapowanie 0 -> Nie, 1 -> Tak dla innych zmiennych
            ifelse(get(selected_var) == 1, "Tak", "Nie")
          }
        )
      
      # Obliczanie procentów dla wykresu
      percent_data <- labeled_data %>%
        group_by(label_var, LUNG_CANCER) %>%
        summarise(Liczba = n(), .groups = "drop") %>%
        group_by(label_var) %>%
        mutate(Procent = Liczba / sum(Liczba) * 100)
      
      # Tworzenie wykresu (porównanie)
      ggplot(percent_data, aes(x = label_var, y = Procent, fill = as.factor(LUNG_CANCER))) +
        geom_bar(position = "dodge", width = 0.7, stat = "identity", color = "black", size = 0.3) +
        geom_text(aes(label = paste0(round(Procent, 0), "%")),
                  position = position_dodge(width = 0.7), vjust = -0.5, size = 4, color = "black", fontface = "bold") +
        labs(
          title = paste("Procentowy udział przypadków raka płuc z podziałem na", selected_var),
          x = ifelse(selected_var == "AGE", "Przedział wiekowy", selected_var), 
          y = "Procent", fill = "Rak płuc"
        ) +
        scale_fill_manual(values = c("0" = "skyblue", "1" = "lightgreen"), labels = c("Nie", "Tak")) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(color = "black", size = 12, face = "bold")
        )
    }
  })
  
  # Opisy dla każdej zmiennej (porównanie)
  output$comparisonDescription <- renderText({
    switch(input$variable,
           "GENDER" = "Wśród osób przebadanych 90% przebadanych mężczyzn było chorych na raka płuc, natomiast w przypadku kobiet 85% zgłosiło, że zmaga się z tą chorobą.",
           "AGE" = "W grupie wiekowej poniżej 50 lat 80% osób było chorych na raka płuc, podczas gdy w grupie wiekowej 50-60 lat odsetek chorych wyniósł 85%. W grupie wiekowej 60-70 lat 86% osób zgłosiło, że zmaga się z rakiem płuc, natomiast w grupie 70+ było to aż 96% osób.",
           "SMOKING" = "Wśród 89% przebadanych osób palących było chorych na raka płuc, natomiast w przypadku osób niepalących odsetek chorych wyniósł 85%.",
           "YELLOW_FINGERS" = "W grupie badanych 93% osób mających żółte palce było chorych na raka płuc, natomiast w przypadku osób niemających tej przypadłości odsetek chorych wyniósł 80%.",
           "ANXIETY" = "W grupie badanych 92% osób odczuwających lęk było chorych na raka płuc, natomiast w przypadku osób bez tego problemu odsetek chorych wyniósł 83%.",
           "PEER_PRESSURE" = "W grupie badanych 94% osób odczuwających presję rówieśników było chorych na raka płuc, natomiast w przypadku osób nieodczuwających tej presji odsetek chorych wyniósł 81%.",
           "CHRONIC.DISEASE" = "W grupie badanych 91% osób chorujących przewlekle było chorych na raka płuc, natomiast w przypadku osób bez chorób przewlekłych odsetek chorych wyniósł 80%.",
           "FATIGUE" = "W grupie badanych 91% osób zgłaszających zmęczenie było chorych na raka płuc, natomiast w przypadku osób niezgłaszających tego problemu odsetek chorych wyniósł 80%.",
           "ALLERGY" = "W grupie badanych 95% osób mających alergie było chorych na raka płuc, natomiast w przypadku osób bez alergii odsetek chorych wyniósł 75%.",
           "WHEEZING" = "W grupie badanych 95% osób ze świszczącym oddechem było chorych na raka płuc, natomiast w przypadku osób bez tego objawu odsetek chorych wyniósł 78%.",
           "ALCOHOL.CONSUMING" = "W grupie badanych 96% osób spożywających alkohol było chorych na raka płuc, natomiast w przypadku osób niepijących odsetek chorych wyniósł 77%.",
           "COUGHING" = "W grupie badanych 94% osób zgłaszających kaszel było chorych na raka płuc, natomiast w przypadku osób bez tego objawu odsetek chorych wyniósł 78%.",
           "SHORTNESS.OF.BREATH" = "W grupie badanych 89% osób zgłaszających duszność było chorych na raka płuc, natomiast w przypadku osób bez duszności odsetek chorych wyniósł 85%.",
           "SWALLOWING.DIFFICULTY" = "W grupie badanych 97% osób mających trudności z przełykaniem było chorych na raka płuc, natomiast w przypadku osób bez tego problemu odsetek chorych wyniósł 79%.",
           "CHEST.PAIN" = "W grupie badanych 93% osób zgłaszających bóle w klatce piersiowej było chorych na raka płuc, natomiast w przypadku osób bez tego problemu odsetek chorych wyniósł 80%.",
           # Domyślna wartość
           "Brak dostępnych danych porównawczych dla wybranej zmiennej."
    )
  })
  
  
  ### DALSZE PRZYGOTOWANIE DANYCH ###
  # Na podstawie analizy przeprowadzonej w osobnym pliku (Kwiatkowska_Kisiel_Lesiewicz) Ze zbioru predyktorów usuwamy zmienną YELLOW_FINGERS 
  # Podział zbioru na uczący i testowy: Dokonano losowego podziału w proporcji: 70% i 30% odpowiednio.Przy każdym wywołaniu funkcji `sample()` otrzymujemy inne zbiory. 
  # W celu powtarzalności eksperymentu stosuje się funkcję `set.seed()`, która inicjuje „ziarno” dla generatora liczb losowych - dla ustalonego "ziarna" w każdym momencie 
  # i na każdym komputerze otrzymuje się ten sam zestaw liczb losowych. Ustalmy `set.seed()`, abyśmy mogli porównac wyniki.
  
  data2 <- data[,-c(4)]
  set.seed(1257)     #set.seed(NULL) --> usunięcie "ziarna"
  n <- nrow(data2)
  liczby_losowe <- sample(c(1:n), round(0.7*n), replace = FALSE)
  data_uczacy <- data2[liczby_losowe,]
  data_testowy <- data2[-liczby_losowe,]
  
  table(data2$LUNG_CANCER)/n
  table(data_uczacy$LUNG_CANCER)/nrow(data_uczacy)
  table(data_testowy$LUNG_CANCER)/nrow(data_testowy)
  
  # Po podziale danych na zbiór uczący i zbiór testowy proporcja osób u których występuje bądź nie występuje ryzyko zachorowania na raka rozkłada się proporcjonalnie we wszystkich zbiorach.
  # Analiza obserwacji wpływowych i nietypowych - patrz (Kwiatkowska_Kisiel_Lesiewicz) linie 230-270
  ### KONIEC PRZYGOTOWANIA DANYCH ###
  
  #Tworzenie modeli logitowych
  
  logit0 <- glm(LUNG_CANCER ~ GENDER + SMOKING + ANXIETY + PEER_PRESSURE +
                  CHRONIC.DISEASE + FATIGUE + ALLERGY + WHEEZING +
                  ALCOHOL.CONSUMING + COUGHING + SHORTNESS.OF.BREATH +
                  SWALLOWING.DIFFICULTY, 
                data = data_uczacy, family = binomial)
  logit1 <- step(logit0)
  logit2 <- step(logit0, direction = "forward")
  logit3 <- step(logit0, direction = "backward")
  
  #Tworzenie modeli probitowych
  probit0 <- glm(LUNG_CANCER ~ GENDER + SMOKING + ANXIETY+ PEER_PRESSURE+
                   CHRONIC.DISEASE+FATIGUE+ALLERGY+WHEEZING+ALCOHOL.CONSUMING+
                   COUGHING+SHORTNESS.OF.BREATH+SWALLOWING.DIFFICULTY, 
                 data = data_uczacy,family = binomial(link = "probit")) 
  probit1 <- step(probit0)
  probit2 <- step(probit0, direction = "forward")
  probit3 <- step(probit0, direction = "backward")
  
  # Lista modeli
  models <- list(
    "Model logitowy" = logit0,
    "Model logitowy (both)" = logit1,
    "Model logitowy (forward)" = logit2,
    "Model logitowy (backward)" = logit3,
    "Model probitowy" = probit0,
    "Model probitowy (both)" = probit1,
    "Model probitowy (forward)" = probit2,
    "Model probitowy (backward)" = probit3
  )
  
  # Funkcja do automatycznej interpretacji modelu z tekstową interpretacją
  interpretacja_modelu <- function(model) {
    # Uzyskujemy podsumowanie modelu
    model_summary <- summary(model)
    
    # Inicjalizujemy tekstową interpretację
    interpretacja <- paste("<pre></pre><br>")
    
    for (i in 1:nrow(model_summary$coefficients)) {
      coef_name <- rownames(model_summary$coefficients)[i]
      estimate <- model_summary$coefficients[i, "Estimate"]
      std_error <- model_summary$coefficients[i, "Std. Error"]
      z_value <- model_summary$coefficients[i, "z value"]
      p_value <- model_summary$coefficients[i, "Pr(>|z|)"]
      
      # Tworzymy tekst interpretacji dla każdego współczynnika
      interpretacja <- paste(
        interpretacja,
        paste("<strong>", coef_name, "</strong>: ", 
              
              sep = ""
        ))
      
      # Dodajemy interpretację na podstawie p-wartości
      if (p_value < 0.001) {
        interpretacja <- paste(interpretacja, "(Wysoka istotność statystyczna)<br>", sep = "")
      } else if (p_value < 0.01) {
        interpretacja <- paste(interpretacja, "(Istotność statystyczna na poziomie 1%)<br>", sep = "")
      } else if (p_value < 0.05) {
        interpretacja <- paste(interpretacja, "(Istotność statystyczna na poziomie 5%)<br>", sep = "")
      } else {
        interpretacja <- paste(interpretacja, "(Brak istotności statystycznej)<br>", sep = "")
      }
      
      # Interpretacja grupy referencyjnej dla wszystkich modeli
      
      if (coef_name == "(Intercept)") {
        
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy")) {
          odds_ratio <- exp(estimate)  # Eksponent dla uzyskania odds ratio
          
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to kobiety niepalące oraz niepijące, u których nie występuje lęk, nie będące pod presją rówieśników, nie chorujące przewlekle, nie będące zmęczone, u których nie występuje: żadna alergia, świszczący oddech, kaszel, duszności ani trudności z przełykaniem. Szansa na zachorowanie na raka płuc w tej grupie wynosi ",
                  round(odds_ratio, 5),  # Wstawienie ilorazu szans z zaokrągleniem do 5 miejsc po przecinku
                  " , ceteris paribus.<br>", sep = "")
          )
        } else if (selected_model %in% c("Model logitowy (both)")) {
          odds_ratio <- exp(estimate)  # Eksponent dla uzyskania odds ratio
          
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to pacjenci niepalący oraz niepijący, u których nie występuje lęk, nie będący pod presją rówieśników, nie chorujący przewlekle, nie będący zmęczeni, u których nie występuje: żadna alergia, kaszel ani trudności z przełykaniem. Szansa na zachorowanie na raka płuc w tej grupie wynosi ",
                  round(odds_ratio, 5),  # Wstawienie ilorazu szans z zaokrągleniem do 5 miejsc po przecinku
                  " , ceteris paribus.<br>", sep = "")
          )
        } else if (selected_model %in% c("Model logitowy (forward)")) {
          odds_ratio <- exp(estimate)  # Eksponent dla uzyskania odds ratio
          
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to kobiety niepalące oraz niepijące, u których nie występuje lęk, nie będące pod presją rówieśników, nie chorujące przewlekle, nie będące zmęczone, u których nie występuje: żadna alergia, kaszel, świszczący oddech, duszności ani trudności z przełykaniem. Szansa na zachorowanie na raka płuc w tej grupie wynosi ",
                  round(odds_ratio, 5),  # Wstawienie ilorazu szans z zaokrągleniem do 5 miejsc po przecinku
                  " , ceteris paribus.<br>", sep = "")
          )
        } else if (selected_model %in% c("Model logitowy (backward)")) {
          odds_ratio <- exp(estimate)  # Eksponent dla uzyskania odds ratio
          
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to pacjenci niepalący oraz niepijący, u których nie występuje lęk, nie będące pod presją rówieśników, nie chorujący przewlekle, nie będący zmęczeni, u których nie występuje: żadna alergia,kaszel ani trudności z przełykaniem. Szansa na zachorowanie na raka płuc w tej grupie wynosi ",
                  round(odds_ratio, 5),  # Wstawienie ilorazu szans z zaokrągleniem do 5 miejsc po przecinku
                  " , ceteris paribus.<br>", sep = "")
          )
        } else if (selected_model %in% c("Model probitowy")) {
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to kobiety niepalące oraz niepijące, u których nie występuje lęk, nie są pod presją rówieśników, nie chorują przewlekle, nie są zmęczone, u których nie występuje: żadna alergia, świszczący oddech, kaszel, duszności ani trudności z przełykaniem, ceteris paribus.<br> ",
                  sep = "")
          )
          
        } else if (selected_model %in% c("Model probitowy (both)")) {
          z_score_change <- estimate
          probability_change <- (pnorm(estimate) - 0.5) * 100
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to pacjenci niepalący i niepijący, u których nie występuje lęk, nie są pod presją rówieśników, nie chorują przewlekle, nie są zmęczeni, u których nie występuje: żadna alergia,kaszel ani trudności z przełykaniem, ceteris paribus.<br> ",
                  sep = "")
          )
        } else if (selected_model %in% c("Model probitowy (forward)")) {
          z_score_change <- estimate
          probability_change <- (pnorm(estimate) - 0.5) * 100
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to kobiety niepalące oraz niepijące, u których nie występuje lęk, nie są pod presją rówieśników, nie chorują przewlekle, nie są zmęczone, u których nie występuje: żadna alergia, świszczący oddech, kaszel, duszności ani trudności z przełykaniem, ceteris paribus.<br> ",
                  sep = "")
          )
        } else if (selected_model %in% c("Model probitowy (backward)")) {
          z_score_change <- estimate
          probability_change <- (pnorm(estimate) - 0.5) * 100
          interpretacja <- paste(
            interpretacja,
            paste("Grupa referencyjna w tym przypadku to pacjenci niepalący oraz niepijący, u których nie występuje lęk, nie są pod presją rówieśników, nie chorują przewlekle, nie są zmęczeni, u których nie występuje: żadna alergia, kaszel ani trudności z przełykaniem, ceteris paribus.<br> ",
                  sep = "")
          )
        }
        
      }
      
      # Interpretacje parametrów modeli na podstawie zmiennych
      
      if (coef_name == "GENDER1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)  # Obliczenie odds ratio
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100 
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci będący kobietami mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci będący kobietami mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci będący kobietami mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci będący kobietami mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      
      if (coef_name == "SMOKING1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)  # Exponent to obtain odds ratio
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100 
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci palący mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci palący mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci palący mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci palący mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      if (coef_name == "ANXIETY1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)  # Exponent to obtain odds ratio
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100 
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występują stany lęku, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występują stany lęku, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występują stany lęku, mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci, u których występują stany lęku, mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      if (coef_name == "PEER_PRESSURE1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)  
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100 
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje presja rówieśników, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje presja rówieśników, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje presja rówieśników, mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci, u których występuje presja rówieśników, mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      if (coef_name == "CHRONIC.DISEASE1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci chorujący przewlekle mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci chorujący przewlekle mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci chorujący przewlekle mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci chorujący przewlekle mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      if (coef_name == "FATIGUE1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Zmęczeni pacjenci mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Zmęczeni pacjenci mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Zmęczeni pacjenci mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Zmęczeni pacjenci mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      if (coef_name == "ALLERGY1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje alergia, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje alergia, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje alergia, mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci, u których występuje alergia, mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      if (coef_name == "WHEEZING1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje świszczący oddech, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje świszczący oddech, mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci, u których występuje świszczący oddech, mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci, u których występuje świszczący oddech, mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      if (coef_name == "ALCOHOL.CONSUMING1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci pijący alkohol mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci pijący alkohol mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci pijący alkohol mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci pijący alkohol mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      if (coef_name == "COUGHING1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z kaszlem mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z kaszlem mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z kaszlem mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci z kaszlem mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      if (coef_name == "SHORTNESS.OF.BREATH1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z dusznościami mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z dusznościami mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z dusznościami mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci z dusznościami mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
      
      if (coef_name == "SWALLOWING.DIFFICULTY1") {
        selected_model <- input$selectedModel
        model <- models[[selected_model]]
        
        if (selected_model %in% c("Model logitowy", "Model logitowy (both)", "Model logitowy (forward)", "Model logitowy (backward)")) {
          # Obliczenia dla modelu logitowego
          odds_ratio <- exp(estimate)
          percentage_change <- (odds_ratio - 1)
          percentage_change_int <- abs(odds_ratio - 1) * 100
          
          # Interpretacja dla modelu logitowego
          if (odds_ratio > 2) {  # Jeśli odds ratio większe niż 2 (100% więcej)
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z trudnościami w przełykaniu mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else if (percentage_change_int > 100) {
            times_increase <- round(odds_ratio, 2)
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z trudnościami w przełykaniu mają około ", times_increase, 
                    " razy większe szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          } else {
            interpretacja <- paste(
              interpretacja,
              paste("Pacjenci z trudnościami w przełykaniu mają o około ", round(percentage_change_int, 2), 
                    "% ", ifelse(percentage_change > 0, "większe", "mniejsze"), 
                    " szanse na zachorowanie na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                    sep = "")
            )
          }
          
        } else if (selected_model %in% c("Model probitowy", "Model probitowy (both)", "Model probitowy (forward)", "Model probitowy (backward)")) {
          # Obliczenia dla modelu probitowego
          z_score_change <- estimate
          
          # Interpretacja dla modelu probitowego
          interpretacja <- paste(
            interpretacja,
            paste("Pacjenci z trudnościami w przełykaniu mają ", ifelse(z_score_change > 0, "większe", "mniejsze"),
                  " prawdopodobieństwo zachorowania na raka płuc, w stosunku do grupy referencyjnej, ceteris paribus.<br>", 
                  sep = "")
          )
        }
      }
    }  
    
    # Sprawdzenie, czy jakakolwiek interpretacja została wygenerowana
    if (interpretacja == "<strong>Interpretacja wyników modelu:</strong><br><br>") {
      interpretacja <- "Brak istotnych statystycznie współczynników w modelu.<br>"
    }
    
    return(interpretacja)
  }
  
  
  # Wyświetlenie postaci wybranego modelu
  output$modelFormula <- renderPrint({
    selected_model <- input$selectedModel
    model <- models[[selected_model]]
    if (!is.null(model)) {
      summary(model)  # Wyświetla postać modelu
    } else {
      "Wybierz model z listy."
    }
  })
  
  # Renderowanie interpretacji modelu
  output$modelInterpretation <- renderUI({
    selected_model <- input$selectedModel
    model <- models[[selected_model]]
    
    if (!is.null(model)) {
      # Funkcja odpowiedzialna za generowanie interpretacji modelu
      interpretacja <- interpretacja_modelu(model)
      
      # Renderujemy wynik w HTML
      HTML(interpretacja)
    } else {
      "Wybierz model z listy."
    }
  })
  
  
  # Funkcja oceniająca modele
  ocena_modelu_dwum <- function(model) {
    kryterium_AIC <- model$aic
    McFadden<- pR2(model)[4]
    Cragg_Uhler<- pR2(model)[6]
    data.frame(kryterium_AIC, McFadden, Cragg_Uhler)
  }
  
  # Funkcja wybierająca najlepszy model
  wybierz_najlepszy_model <- function(wyniki) {
    najlepszy_AIC <- wyniki[which.min(wyniki$kryterium_AIC), ]
    najlepszy_McFadden <- wyniki[which.max(wyniki$McFadden), ]
    najlepszy_Cragg_Uhler <- wyniki[which.max(wyniki$Cragg_Uhler), ]
    
    data.frame(
      Kryterium = c("Najmniejsze AIC", "Największy McFadden", "Największy Cragg-Uhler"),
      Model = c(najlepszy_AIC$Model, najlepszy_McFadden$Model, najlepszy_Cragg_Uhler$Model)
    )
  }
  
  # Funkcja, która zwraca rekomendację na podstawie najlepszych wyników
  rekomendacja_modelu <- function(wyniki) {
    # Sprawdzamy najlepsze kryterium (AIC, McFadden, Cragg-Uhler)
    najlepszy_AIC <- wyniki[which.min(wyniki$kryterium_AIC), ]
    najlepszy_McFadden <- wyniki[which.max(wyniki$McFadden), ]
    najlepszy_Cragg_Uhler <- wyniki[which.max(wyniki$Cragg_Uhler), ]
    
    # Tworzymy zalecenie na podstawie najlepszego modelu
    rekomendacja <- paste(
      "<strong>Najlepszy model według AIC:</strong> ", najlepszy_AIC$Model, "<br>",
      "<strong>Najlepszy model według McFaddena:</strong> ", najlepszy_McFadden$Model, "<br>",
      "<strong>Najlepszy model według Cragg-Uhler:</strong> ", najlepszy_Cragg_Uhler$Model, "<br><br>",
      "<strong>Wybór modelu zależy od kontekstu:</strong><br>",
      "• AIC: Jeśli zależy Ci na najmniejszym błędzie prognozy.<br>",
      "• McFadden: Jeśli chcesz uwzględnić dopasowanie modelu do danych.<br>",
      "• Cragg-Uhler: Jeśli zależy Ci na prostocie i interpretowalności modelu.<br>",
      sep = ""
    )
    
    return(rekomendacja)
  }
  
  # Zapis wyników w jednej ramce danych
  wyniki_oceny_logit <- rbind(
    cbind(Model = "Model logitowy", ocena_modelu_dwum(logit0)),
    cbind(Model = "Model logitowy (both)", ocena_modelu_dwum(logit1)),
    cbind(Model = "Model logitowy (forward)", ocena_modelu_dwum(logit2)),
    cbind(Model = "Model logitowy (backward)", ocena_modelu_dwum(logit3)),
    cbind(Model = "Model probitowy", ocena_modelu_dwum(probit0)),
    cbind(Model = "Model probitowy (both)", ocena_modelu_dwum(probit1)),
    cbind(Model = "Model probitowy (forward))", ocena_modelu_dwum(probit2)),
    cbind(Model = "Model probitowy (backward)", ocena_modelu_dwum(probit3))
  )
  
  # Wyświetlanie 
  output$modelComparison <- renderTable({
    wyniki_oceny_logit
  }, rownames = FALSE)
  output$bestModel <- renderTable({
    wybierz_najlepszy_model(wyniki_oceny_logit)
  }, rownames = FALSE)
  output$rekomendacja <- renderUI({
    HTML(rekomendacja_modelu(wyniki_oceny_logit))
  })
  
  #W celu porównania oceny jakości predykcji modeli Logit i Probit należy oszacować tablice trafności dla obu tych modeli na zbiorze testowym i uczącym.
  #Tablice trafności dla wybranego punktu odcięcia p*
  #Niech p* = proporcja z próby uczącej
  
  p <- table(data_uczacy$LUNG_CANCER)[2]/nrow(data_uczacy)
  cat("Tablica trafności dla modelu logitowego_1 - próba ucząca\n")
  tab_traf <- data.frame(obserwowane=logit1$y, przewidywane=ifelse(logit1$fitted.values>p, 1, 0))
  table(tab_traf)
  
  cat("Tablica trafności dla modelu probitowego - próba ucząca\n")
  tab_traf <- data.frame(obserwowane=probit1$y, przewidywane=ifelse(probit1$fitted.values>p, 1, 0))
  table(tab_traf)
  
  cat("Tablica trafności dla modelu logitowego - próba testowa\n")
  tab_traf <- data.frame(obserwowane=data_testowy$LUNG_CANCER, przewidywane=ifelse(predict(logit1, data_testowy, type = "response")>p, 1, 0))
  table(tab_traf)
  
  cat("Tablica trafności dla modelu probitowego - próba testowa\n")
  tab_traf <- data.frame(obserwowane=data_testowy$LUNG_CANCER, przewidywane=ifelse(predict(probit1, data_testowy, type = "response")>p, 1, 0))
  table(tab_traf)
  
  miary_pred <- function(model, dane, Y, p = 0.5) {
    tab <- table(obserwowane = Y, przewidywane = ifelse(predict(model, dane, type = "response") > p, 1, 0))
    ACC <- (tab[1, 1] + tab[2, 2]) / sum(tab)
    ER <- (tab[1, 2] + tab[2, 1]) / sum(tab)
    SENS <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
    SPEC <- tab[1, 1] / (tab[1, 1] + tab[1, 2])
    PPV <- tab[2, 2] / (tab[2, 2] + tab[1, 2])
    NPV <- tab[1, 1] / (tab[1, 1] + tab[2, 1])
    miary <- data.frame(ACC, ER, SENS, SPEC, PPV, NPV)
    return(miary)
  }
  
  #Wyświetlanie miar jakości predykcji
  output$comparisonMetrics <- renderTable({
    # Obliczenie miar jakości predykcji
    wyniki <- rbind(
      cbind(Model = "Logitowy", Zbiór = "Testowy", miary_pred(logit1, data_testowy, data_testowy$LUNG_CANCER, p)),
      cbind(Model = "Probitowy", Zbiór = "Testowy", miary_pred(probit1, data_testowy, data_testowy$LUNG_CANCER, p)),
      cbind(Model = "Logitowy", Zbiór = "Uczący", miary_pred(logit1, data_uczacy, data_uczacy$LUNG_CANCER, p)),
      cbind(Model = "Probitowy", Zbiór = "Uczący", miary_pred(probit1, data_uczacy, data_uczacy$LUNG_CANCER, p))
    )
    
    # Zaokrąglenie do 3 miejsc po przecinku
    wyniki[, c("ACC", "ER", "SENS", "SPEC", "PPV", "NPV")] <- round(wyniki[, c("ACC", "ER", "SENS", "SPEC", "PPV", "NPV")], 3)
    
    # Uporządkowanie tabeli
    wyniki <- wyniki[, c("Model", "Zbiór", "ACC", "ER", "SENS", "SPEC", "PPV", "NPV")]
    wyniki
  }, rownames = FALSE)
  
  #Interpretacja wskaźników
  output$tableInterpretation <- renderUI({
    HTML(paste(
      "Na podstawie kryteriów dobroci dopasowania można stwierdzić, że między modelami nie występują duże różnice dlatego do analizy oceny jakości predykcji wybrano model logitowy (both) i probitowy (both), które charakteryzują się najmniejszą wartością kryterium AIC. Poniższa interpretacja przeprowadzona została dla modeli na podstawie zbioru uczącego.<br><br>",
      
      "<strong>• ACC (Accuracy):</strong><br>",
      "Model Logit: Udział liczby trafnie sklasyfikowanych jednostek w ogólnej liczbie jednostek wynosi 87,04%. Jest to wynik zadowalający.<br>",
      "Model Probit: Udział liczby trafnie sklasyfikowanych jednostek w ogólnej liczbie jednostek wynosi 88,89%. Jest to wynik zadowalający.<br><br>",
      
      "<strong>• ER (Error Rate):</strong><br>",
      "Model Logit: Udział liczby błędnie sklasyfikowanych jednostek w ogólnej liczbie jednostek wynosi 12,96%. Jest to wynik zadowalający.<br>",
      "Model Probit: Udział liczby błędnie sklasyfikowanych jednostek w ogólnej liczbie jednostek wynosi 11,11%. Jest to wynik zadowalający.<br><br>",
      
      "<strong>• SENS (Sensitivity):</strong><br>",
      "Model Logit: Udział liczby trafnie oszacowanych 1 w liczbie wszystkich obserwowanych 1 wynosi 85,79%. Jest to wynik zadowalający.<br>",
      "Model Probit: Udział liczby trafnie oszacowanych 1 w liczbie wszystkich obserwowanych 1 wynosi 87,37%. Jest to wynik zadowalający.<br><br>",
      
      "<strong>• SPEC (Specificity):</strong><br>",
      "Model Logit: Udział liczby trafnie oszacowanych 0 w liczbie wszystkich obserwowanych 0 wynosi 96,15%. Jest to wynik zadowalający.<br>",
      "Model Probit: Udział liczby trafnie oszacowanych 0 w liczbie wszystkich obserwowanych 0 wynosi 100%. Jest to wynik zadowalający.<br><br>",
      
      "<strong>• PPV (Positive Predictive Value):</strong><br>",
      "Model Logit: Udział liczby trafnie oszacowanych 1 w liczbie wszystkich prognozowanych 1 wynosi 99,39%. Jest to wynik zadowalający.<br>",
      "Model Probit: Udział liczby trafnie oszacowanych 1 w liczbie wszystkich prognozowanych 1 wynosi 100%. Jest to wynik zadowalający.<br><br>",
      
      "<strong>• NPV (Negative Predictive Value):</strong><br>",
      "Model Logit: Udział liczby trafnie oszacowanych 0 w liczbie wszystkich prognozowanych 0 wynosi 47,08%. Nie jest to do końca zadowalający wynik.<br>",
      "Model Probit: Udział liczby trafnie oszacowanych 0 w liczbie wszystkich prognozowanych 0 wynosi 52%. Nie jest to do końca zadowalający wynik.<br>"
    ))
  })
  
  #Wyświetlanie krzywych ROC
  output$rocCurves <- renderPlot({
    par(mfrow = c(1, 2))  # Dwa wykresy obok siebie
    # Krzywa ROC dla modelu logitowego
    roc_logit <- roc(data_testowy$LUNG_CANCER, predict(logit1, data_testowy, type = "response"))
    plot(roc_logit, main = "Krzywa ROC - Logitowy", col = "blue", lwd = 2, print.auc = TRUE)
    
    # Krzywa ROC dla modelu probitowego
    roc_probit <- roc(data_testowy$LUNG_CANCER, predict(probit1, data_testowy, type = "response"))
    plot(roc_probit, main = "Krzywa ROC - Probitowy", col = "red", lwd = 2, print.auc = TRUE)
  })
  
  #Interpretacja krzywej ROC
  output$rocInterpretation <- renderUI({
    HTML(paste(
      "<strong>Interpretacja krzywych ROC:</strong><br>",
      "Wykresy krzywej ROC dla obu analizowanych modeli są dosyć podobne. Wartości pól pod krzywą ROC również są podobne.Na podstawie pola pod krzywą ROC można stwierdzić, że jokść predykcji w przypadku obu modeli jest zadowalająca.<br>"
    ))
  })
  
  output$downloadModelInterpretation <- downloadHandler(
    filename = function() {
      paste("interpretacja_modelu_", input$selectedModel, ".csv", sep = "")
    },
    content = function(file) {
      selected_model <- input$selectedModel
      model <- models[[selected_model]]
      if (!is.null(model)) {
        interpretacja <- interpretacja_modelu(model)
        write.csv(data.frame(Interpretacja = interpretacja), file, row.names = FALSE)
      }
    }
  )
  
  # Eksport wybranego modelu z zakładki "Tworzenie i wybór modeli"
  output$downloadModel <- downloadHandler(
    filename = function() {
      paste0("model_", gsub(" ", "_", input$selectedModel), ".csv")
    },
    content = function(file) {
      selected_model <- input$selectedModel
      model <- models[[selected_model]]
      
      if (!is.null(model)) {
        coefficients <- summary(model)$coefficients
        coefficients_df <- as.data.frame(coefficients)
        coefficients_df$Współczynnik <- rownames(coefficients_df)
        coefficients_df <- coefficients_df[, c("Współczynnik", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
        colnames(coefficients_df) <- c("Współczynnik", "Estymacja", "Błąd standardowy", "z-wartość", "p-wartość")
        write.csv(coefficients_df, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv(data.frame(Informacja = "Nie wybrano modelu."), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  
  # Eksport wyników porównania modeli (bez "Najlepsze modele")
  output$downloadModelComparison <- downloadHandler(
    filename = function() {
      "porownanie_modeli.csv"
    },
    content = function(file) {
      write.csv(wyniki_oceny_logit, file, row.names = FALSE, fileEncoding = "UTF-8") # Kodowanie UTF-8
    }
  )
  
  # Eksport miar jakości predykcji
  output$downloadComparisonMetrics <- downloadHandler(
    filename = function() {
      "miary_jakosci_predykcji.csv"
    },
    content = function(file) {
      wyniki <- rbind(
        cbind(Model = "Logitowy", Zbiór = "Testowy", miary_pred(logit1, data_testowy, data_testowy$LUNG_CANCER, p)),
        cbind(Model = "Probitowy", Zbiór = "Testowy", miary_pred(probit1, data_testowy, data_testowy$LUNG_CANCER, p)),
        cbind(Model = "Logitowy", Zbiór = "Uczący", miary_pred(logit1, data_uczacy, data_uczacy$LUNG_CANCER, p)),
        cbind(Model = "Probitowy", Zbiór = "Uczący", miary_pred(probit1, data_uczacy, data_uczacy$LUNG_CANCER, p))
      )
      wyniki[, c("ACC", "ER", "SENS", "SPEC", "PPV", "NPV")] <- round(wyniki[, c("ACC", "ER", "SENS", "SPEC", "PPV", "NPV")], 3)
      write.csv(wyniki, file, row.names = FALSE, fileEncoding = "UTF-8") # Kodowanie UTF-8
    }
  )
}