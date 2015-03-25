library(shiny)
library(knitr)
library(markdown)

# SECTION URL 
hashProxy <- function(inputoutputID) {
  div(id=inputoutputID,class=inputoutputID,tag("div",""));
}
# Fin de SECTION URL 

# popup function (from J. Cheng, Shiny-discuss, 2/7/13)
### server location
source('www/HelpPopup.R')
## source('~/ShinyApps/en-elections/HelpPopup.R')

# Define UI for dataset viewer application
# test
# shinyUI(pageWithSidebar(
shinyUI(fluidPage(title = "en-Elections",

#add stylesheet to control width of sidebar
 includeCSS("www/style0021.css"),                

# choose language
# Bouton "Français"
HTML("<a class='btn' href='http://spark.rstudio.com/detector/elections/'>Français</a>"),

# Application title
fluidRow(
  tags$body(includeHTML("www/help/help_introduction.html")),
  
# Interface pour les actions sur les données :
  # reprise d'une analyse,
  # Example
  column(2, wellPanel(
    # SECTION URL : sert pour afficher les paramètres de l'URL
    textInput("customer", "Name your personal barometer:"),
    # Fin de SECTION URL 
    
    # Aide sur Actions sur les analyses
    div(class="row-fluid",
        div(class="span8",helpPopup(
          "File management",
          includeHTML("www/help/help_file_manage.html"),
          placement='right',
          trigger='hover', 
          glue = tags$a("File management", class = "icon-question-sign")
          ))
    ),

   radioButtons("expertise", "", 
                choices = 
                  c("Resume",
                     "Resume (iPad-iPhone)",
                     "Example",
                    "New analysis"),                      
                selected="New analysis"),

   # Bouton "Recharger la page" et Aide - Recharger
   ### local ou serveur
   # local
   # glue = tags$a(class="btn btn-default", href='/', "New analysis") 
   
   # serveur spark
   #  HTML("<a class='btn' href='http://spark.rstudio.com/detector/en-elections/'>Restart?</a>"),
   
   # serveur shinyApps.io
   # activer cette ligne pour publier sur shinyapps.io
   #  HTML("<a class='btn' href = 'http://webappcb001.shinyapps.io/CanadianElections/'>Restart?</a>"), 
   conditionalPanel(
     condition = "input.expertise == 'New analysis'",
          div(class="row-fluid",
         div(class="span8",helpPopup(
           "Restart",
           includeHTML("www/help/help_recommencer.html"),
           placement='right',
           trigger='hover', 
           glue = tags$a(class="btn btn-default", href='/', "New analysis")
          ))   
  )
   ),
   
    # Only show this panel if the choice is "Resume"
    conditionalPanel(
      condition = "input.expertise == 'Resume'",
      div(class="row-fluid",
        div(class="span8",helpPopup(
          "To resume a previously saved analysis:",
          includeHTML("www/help/help_actions_reprendre.html"),
          placement='right',
          trigger='hover', 
        ))
      ),
      fileInput('factsList', 'Téléchargement des données',
                accept=c('text/csv', 'text/comma-separated-values,text/plain'))
    ),
    
    # Only show this panel if the choice is "Resume (iPad-iPhone)"
    conditionalPanel(
      condition = "input.expertise == 'Resume (iPad-iPhone)'",
      div(class="row-fluid",
        div(class="span1",helpPopup(
          "To Resume on iPad/iPhone a previously saved analysis:",
          includeHTML("www/help/help_actions_reprendre_iPad.html"),
          placement='right',
          trigger='hover', 
          ))
      ),
 #    textInput("startFacts", "URL :","")
 h4("Data table:"),
 tags$textarea(id = "startFacts")
    ),
   conditionalPanel(
     condition = "input.expertise == 'Example'",
     div(class="row-fluid",
      div(class="span8",helpPopup(
        "Example",
        includeHTML("www/help/help_actions_exemple.html"),
        placement='right',
        trigger='hover' 
        ))
     )
   )
  )
  ), # fin du 1er column
  
# Traitement des faits
column(2, wellPanel(  # fin à li 171
  # Aide - Choisir/Ajouter/Retirer
  div(class="row-fluid",
    div(class="span8",helpPopup(
      "Choose/Add/Delete",
      includeHTML("www/help/help_Ajouter_Retirer.html"),
      placement='right',
      trigger='hover', 
      glue = tags$a(class = "icon-question-sign", "Actions on issues")
  ))
  ), 
  
      selectInput("add_del", "",
      choices = c("Choosing an issue",
                  "Adding yourself an issue", 
                  "Removing an issue"),
      selected = "Choosing an issue"),
    #   selected = "Faire un choix"),
 
  conditionalPanel(
    condition = ("input.add_del == 'Choosing an issue'"),
    div(class="row-fluid", 
      div(class="span8", helpPopup(
        "Choosing an issue",
        includeHTML("www/help/help_choisir_fait.html"),
        placement='right',
        trigger='hover', 
        glue = tags$a(h4("1. Choose an issue in the proposed list")) 
        )
        )),
 uiOutput("choix1")
       ),  
 
 conditionalPanel(
   condition = ("input.add_del == 'Adding yourself an issue'"),
   div(class="row-fluid", 
    div(class="span8", helpPopup(
      "Add an issue",
      includeHTML("www/help/help_entrer_fait.html"), 
      placement='right',
      trigger='hover', 
      glue = tags$a(h4("1. Describe an issue"))
       )
      )),
   uiOutput("choix2")
 ),
 
  conditionalPanel(
    condition = ("input.add_del == 'Removing an issue'"),
    div(class="row-fluid", 
      div(class="span8",helpPopup(
        "Remove issue",
        includeHTML("www/help/help_retirer_fait.html"),
        placement='right',
        trigger='hover', 
        glue =  tags$a(h4("Number of the issue to remove"))
    )
    )),
uiOutput("choix3"),
# test si no de fait invalide
# This makes web page load the JS file in the HTML head.
# The call to singleton ensures it's only included once
# in a page. It's not strictly necessary in this case, but
# it's good practice.
singleton(
  tags$head(tags$script(src = "message-handler.js"))
)
# fin test si no de fait invalide
  ),
  
  conditionalPanel(
    condition = ("input.add_del != 'Removing an issue'"),
    div(class="row-fluid", 
      div(class="span8", 
        helpPopup("Points to",includeHTML("www/help/help_pointevers.html"), 
        placement='right',
        trigger='hover', 
        glue = tags$a(h4("2. Choose the party that meets your expectations for the issue chosen")))
)),
  uiOutput("valHyp")
  ),

  conditionalPanel(
    condition = ("input.add_del != 'Removing an issue'"),
    div(class="row-fluid", 
      div(class="span8", 
        helpPopup("Weighting your issue",
        includeHTML("www/help/help_poids.html"), 
        placement='right',
        trigger='hover', 
        glue = tags$a(h4("3. Personal priority for this issue"))
  ))),
  uiOutput("num")
  ),
# ), # fin du 2e wellpanel
# ), fin du 2e column, li 87, reportée plus bas
# ),  

# Publier les résultats
# column(2, wellPanel(
# Bouton "Évaluer" et Aide - Évaluer
  div(class="row-fluid",
      div(class="span8",
        helpPopup("Evaluate",
        includeHTML("www/help/help_evaluer.html"),
        placement='right',
        trigger='hover',
        glue = actionButton("goButton", "Evaluate")
        ))
  ),
# Submit de l'analyse
  # bouton "submit" et aide - Submit
  div(class="row-fluid",
  div(class="span8",
    helpPopup("Sharing your analysis with others",
    includeHTML("www/help/help_soumettre.html"),
    placement='top',
    trigger='hover',
    glue = actionButton("submit_ana", "Share")
    ))
),
# only show if submit_ana >= 1
 conditionalPanel(
   condition = "input.submit_ana >= '1'",
   helpText("Your analysis has been recorded. Thank you.")
   # faire msg variable dans server.R
  ) # dernière instruction du bloc
 ) # fin du wellPanel li 87
), # fin du 2e column, li 89
  
# mainPanel(
column(6,
# SECTION URL    
   includeHTML("URL.js"),
   h3(textOutput("order")),
   hashProxy("hash"),
# Fin de SECTION URL  
    tabsetPanel(
      tabPanel(
        "Issues",
         # Tableau des faits
        div(class="row-fluid",
          div(class="span8",
            helpPopup("Your table of issues",
            includeHTML("www/help/help_tableau_faits.html"),
            placement='bottom',
            trigger='hover', 
            glue = tags$a(h5("Table of issues chosen"))))
        ),     
        dataTableOutput("grid"),
        textInput("userfilename2",h4("Save your table of issues:") ,value = "Issue-Elections-Canada"),
        downloadButton('downloadFacts', 'Save')  
        ),
      
      tabPanel(
        "Results",
        div(class="row-fluid",
        div(class="span8",
          helpPopup("Interpretation of measurements",
          includeHTML("www/help/help_results.html"),
          placement='bottom',
          trigger='hover', 
          glue = tags$a(h5("Results of the combination of your issues"))))
        ), 
        tableOutput("view5"),
        div(class="row-fluid", 
        div(class="span8",
          helpPopup("Weight of conflict",
          includeHTML("www/help/help_contradiction.html"),
          placement='bottom',
          trigger='hover', 
          glue = tags$a(h5("Weight of conflict between issues:"))))
            ),
    textOutput("view5a"),
        textInput("userfilename1",h4("Save results:") ,value="Results-Elections-Canada"),
        downloadButton('downloadResults', 'Save')
      ),
  
# Onglet graphique
tabPanel("Graph",
         div(class="row-fluid",
         div(class="span8",
            helpPopup("The For/Against ratios",
            includeHTML("www/help/help_graphe_Ratios.html"),
            placement='bottom',
            trigger='hover', 
            glue = tags$a(h5("Graph of the For/Against ratios"))))
         ),
         plotOutput("plot0")
),
  
# Onglet Tendance
    tabPanel("Trend",
        div(class="row-fluid",
        div(class="span8",
          helpPopup("Following the Trend",
          includeHTML("www/help/help_trend.html"),
          placement='bottom',
          trigger='hover', 
          glue = tags$a(h5("Evolution or the For/Against ratios"))))
             ),
        plotOutput("plot1"),
        div(class="row-fluid",
        div(class="span8",
          helpPopup("Following the conflict",
          includeHTML("www/help/help_conflit.html"),
          placement='bottom',
          trigger='hover', 
          glue = tags$a(h5("Evolution of the weight of conflict"))))
        ),
      dataTableOutput("grid2")
    ),

# Informations sur la méthode et l'auteur
    tabPanel("About",
      helpText("This application uses the Dempster-Shafer theory to combine incertain facts. For a summary of the  theory, one can consult the Wiki page:",
      a(href= "http://en.wikipedia.org/wiki/Dempster-Shafer_theory", target="_blank","Dempster-Shafer Theory")),

# Coordonnées    
    helpText("Author : Claude Boivin, Stat.ASSQ"),
    tags$body(p("Elections version 0.3.16d"),p('Comments, suggestions : ', a(href="mailto:webapp.cb@gmail.com",'webapp.cb@gmail.com')))
        )
    , id = "onglets", type = "pills") # de tabsetPanel, li 177
) # fin du column, li 176
) # fin du fluidRow, li 36
)) # de fluidpage