library(shiny)
library(readr)
library(tidyverse)
library(tibble)
library(tm)
library(tidytext)
library(shinythemes)
library(wordcloud)
library(wordcloud2)
library(igraph)
library(ggraph)
library(ggthemes)
library(ggplot2)

# Charger les données
data <- read_csv("~/2 MECEN/Application/initial/data/data2.csv") %>%
  mutate(continent = as.factor(continent),
         country = as.factor(country))
data <- data[,-c(3,4)]

# small data 
small_data <- data |> slice(1:1000)

# Créez un tibble avec les mots vides
stop_words <- stopwords("en")
mots_vides <- tibble(mot = stop_words)


## fonctions 
### 1.Définir une fonction pour créer le graphique en fonction des données "top_"
plot_top_words <- function(data, title) {
  # Créer le graphique avec ggplot
  p <- ggplot(data, aes(x = reorder(mot, n), y = n)) +
    geom_bar(stat = "identity") + 
    coord_flip() +
    labs(x = "",
         y = "Occurences des mots",
         title = "",
         caption = "") +
    theme_minimal()
  
  return(p)
}

# 2.Définir une fonction pour créer le graphique
plot_occurrence_mots <- function(data) {
  # Assurez-vous que la colonne "mot" est de type factor
  data <- mutate(data, mot = factor(mot))
  
  # Créer le graphique avec ggplot
  p <- ggplot(data, aes(x = reorder(mot, total), y = n)) +
    geom_bar(aes(fill = continent), stat = "identity") +
    coord_flip() +
    labs(x = "",
         y = "Occurrence des mots technique dans description par continent",
         fill = "Continent",
         title = "",
         caption = "Données : annonces d'emploi.")
  
  return(p)
}


## Supprimer les mots qui ne sont pas dans la liste de mots techniques
mots_techniques <- c("Python", "R", "SQL", "Java", "C++", "Scala", "Julia", "MATLAB",
                     "Pandas", "NumPy", "SciPy", "TensorFlow", "Keras", "PyTorch", "Scikit-learn", "Spark", "Hadoop",
                     "Data", "mining", "cleaning", "visualization", "wrangling", "Exploratory", "analysis", "Statistical",
                     "Machine", "learning", "Deep", "Regression", "Classification", "Clustering", "Time", "series",
                     "Natural", "language", "processing", "NLP", "Dimensionality", "reduction", "Feature", "engineering", "Cross-validation",
                     "Jupyter", "Notebook", "RStudio", "Git", "Docker", "AWS", "Google", "Cloud", "Platform", "Microsoft", "Azure", "Tableau", "Power", "BI")

mots_techniques <- tolower(mots_techniques)  # Convertir en minuscules pour la correspondance

mots_techniques <- unique(mots_techniques)  # Supprimer les doublons, au cas où

mots_vides_prepositions <- c("à", "au", "aux", "avec", "chez", "dans", "de", "des", "du", "en", "entre", "par", "pour", "sans", "sous", "sur", "vers", "et", "un", "une", "être", "avoir")
mots_vides_pronoms <- c("je", "tu", "il", "elle", "nous", "vous", "ils", "elles", "me", "te", "se", "nous", "vous", "le", "la", "les", "lui", "leur", "moi", "toi", "soi")
mots_vides_contexte <- c("utccategory", "looking", "can", "work", "need", "to", "and", "the", "a", "for", "in", "on", "with", "$", "apply", 
                         "our", "be", "we", "you", "is", "are", "january", "february", "march", "april", "may",
                         "june", "july", "august", "september", "october", "november", "december")
mots_vides_2 <- c(stop_words, mots_vides_contexte, mots_vides_prepositions, mots_vides_pronoms)



## titre
### Sans filtrage 

mots_titre <- data |> 
  unnest_tokens(mot, title) |>
  anti_join(mots_vides)
#top 25 titre 
top_titre <- mots_titre %>%
  count(mot, sort = TRUE) %>%
  head(25) 

### Avec filtrage 
#### filtrage titre
mots_filtres_titre <- mots_titre %>%
  filter(tolower(mot) %in% mots_techniques)  # Filtrer les mots en utilisant la liste de mots techniques

#### Croisement avec continent
mots_filtres_titre %>%
  group_by(continent, mot) %>%
  summarise(n_country = n()) 

#### total
total_titre <- mots_filtres_titre %>%
  count(mot) %>%
  rename(total = n)

titre_f <- mots_filtres_titre %>%
  count(continent, mot) %>%
  left_join(total_titre)  %>%
  arrange(desc(total)) 



## Description 
### sans filtrage 

mots_descr <- data |> 
  unnest_tokens(mot, description) |>
  anti_join(mots_vides)


## top 25 descrption 

top_descr <- mots_descr %>%
  count(mot, sort = TRUE) %>%
  head(25) 


### Avec filtrage 
#### filtrage descr
mots_filtres_descr <- mots_descr %>%
  filter(tolower(mot) %in% mots_techniques)  # Filtrer les mots en utilisant la liste de mots techniques

#### croisement avec continent 
mots_filtres_descr %>%
  group_by(continent, mot) %>%
  summarise(n_country = n())

#### total 
total_descr <- mots_filtres_descr %>%
  count(mot) %>%
  rename(total = n)

descr_f <- mots_filtres_descr %>%
  count(continent, mot) %>%
  left_join(total_titre)  %>%
  arrange(desc(total))



# Define UI for application
ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("DATA WORDS"),
  sidebarLayout(
    sidebarPanel(
      selectInput("table_select", "Choisir un tableau à afficher:",
                  choices = c("Top Titre", "Top Description", "total_titre", "total_descr")),
      selectInput("data_select1","Choisir un jeu de données:",
                  choices = c("top25 mots titre"="top_titre", "top25 mots descrp"="top_descr")),
      #selectInput("data_select2", "Choisir un jeu de données :", 
                  #choices = c("titre_f", "descr_f")),
      selectInput("data_select2", "Choisir un jeu de données :", 
                  choices = c("Mots data titre" = "titre_f", "Mots data description" = " descr_f")),
      selectInput("wordcloud_select", "Choisir un jeu de données :", 
                  choices = c("Top Titre", "Top Description", "total_titre" , "total_descr")),
      textInput("word1_input", "Premier mot:", placeholder = "Entrez un mot"),
      textInput("word2_input", "Deuxième mot:", placeholder = "Entrez un autre mot"),
      actionButton("submit_button", "Afficher les bigrammes"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Base de donnée", dataTableOutput("dataTable")),
        tabPanel("Statistiques", 
                 verbatimTextOutput("summary")),
        tabPanel("Top Occurrence",
                 dataTableOutput("selected_table")),
        tabPanel("Graphique d'occurrence", 
                 plotOutput("plot_top_words")),
        tabPanel("Graphique d'occurrence de mots techniques", 
                 plotOutput("plot_occurrence_mots")),
        tabPanel("Nuage de mots", plotOutput("wordcloud_plot")),
        tabPanel("Graphique de Bigrammes",
                 plotOutput("bigram_plot")),
        tabPanel("Bigrammes individuels",
                 plotOutput("individual_bigram_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$dataTable <- renderDataTable({ data })
  
  output$summary <- renderPrint({ summary(data)
    #Your summary code goes here
  })
  
  # Sélectionner le tableau en fonction du choix de l'utilisateur
  # selected_table <- reactive({
  # if (input$table_select == "Top Titre") {
  #  return(top_titre)
  # } else {
  #    return(top_descr)
  # }
  #})
  
  selected_table <- reactive({
    if (input$table_select == "Top Titre") {
      return(top_titre)
    } else if (input$table_select == "Top Description") {
      return(top_descr)
    } else if (input$table_select == "total_titre") {
      return(total_titre)
    } else {
      return(total_descr)
    }
  })
  
  # Afficher le tableau sélectionné
  output$selected_table <- renderDataTable({
    selected_table()
  })
  
  # Sélectionner les données en fonction du choix de l'utilisateur 
  selected_data <- reactive({
    if (input$data_select1 == "top_titre") {
      return(top_titre)
    } else {
      return(top_descr)
    }
  })
  
  # Afficher le graphique d'occurrence des mots en utilisant la fonction plot_occurrence_mots
  output$plot_top_words <- renderPlot({
    plot_top_words(selected_data())
  })
  
  
  # Sélectionner les données en fonction du choix de l'utilisateur f
  selected_data2 <- reactive({
    if (input$data_select2 == "titre_f") {
      return(titre_f)
    } else {
      return(descr_f)
    }
  })
  
  # Afficher le graphique d'occurrence des mots en utilisant la fonction plot_occurrence_mots f
  output$plot_occurrence_mots <- renderPlot({
    plot_occurrence_mots(selected_data2())
  })
  
  output$wordcloud_plot <- renderPlot({
    if (input$wordcloud_select == "Top Titre") {
      words <- top_titre$mot
      freq <- top_titre$n
    } else if (input$wordcloud_select == "Top Description") {
      words <- top_descr$mot
      freq <- top_descr$n
    } else if (input$wordcloud_select == "total_titre") {
      words <- total_titre$mot
      freq <- total_titre$total
    } else {
      words <- total_descr$mot
      freq <- total_descr$total
    }
    #wordcloud(words, freq)
    wordcloud(words, freq, colors = brewer.pal(8, "Dark2"))
  })
  
  count_bigrams <- function(data) {
    data %>%
      unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% c(mots_vides_2), !word2 %in% c(mots_vides_2)) %>%
      count(word1, word2, sort = TRUE)  # Sélectionner les 100 premiers bigrammes les plus fréquents
  }
  
  visualize_bigrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  }
  
  visualize_bigrams_individual <- function(bigrams) {
    
    set.seed(2016)
    
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
      
      graph_from_data_frame() %>%
      
      ggraph(layout = "fr") +
      
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
      
      geom_node_point(color = "lightblue", size = 5) +
      
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      
      theme_void()
    
  }
  
  individual_words_bigrams <- function(data, word1Value, word2Value) {
    x_Words1 <- count_bigrams(data) %>%
      filter(word1 == word1Value)
    
    x_Words2 <- count_bigrams(data) %>%
      filter(word2 == word2Value)
    
    x_full <- rbind(x_Words1, x_Words2)
    return(x_full)
  }
  output$bigram_plot <- renderPlot({
    corpus_words <- count_bigrams(small_data)%>%
       
      filter(n > 50)
    visualize_bigrams(corpus_words)
  })
  
  output$individual_bigram_plot <- renderPlot({
    word1 <- input$word1_input
    word2 <- input$word2_input
    if (!is.null(word1) && !is.null(word2)) {
      individual_bigrams <- individual_words_bigrams(small_data, word1, word2)%>%
        filter(n > 20)
      visualize_bigrams_individual(individual_bigrams)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
