library(shiny)
# library(shinyBS)
library(plyr)
library(rjson)
library(RCurl) 
library(bitops)
library(ggplot2)
library(reshape2)
library(RJSONIO)
# functions for the calculations
library(dst)

### Autres fonctions
 source('www/suivi_chrono.R')
# source('~/ShinyApps/en-elections/suivi_chrono.R')

# options(error = browser)
# options(shiny.error=traceback)

# Define server logic 
# SECTION URL
# passer des paramètres dans l'URL pour
# présenter les paramètres dans l'URL
# dans les inputs (permet à l'usager de faire son choix)
# dans l'output (permet à l'usager d'afficher son choix)
url_fields_to_sync <- c("expertise", "customer");

# Define server logic 
shinyServer(function(input, output, session) {
  
  output$order <- renderText({
    paste(
      if((input$expertise == "") | (input$expertise == "New analysis")) "Begin a new analysis of"  else input$expertise,
     if (input$expertise == "Example" ) "" else input$customer)
  })
  
  firstTime <- TRUE
  
  output$hash <- renderText({
    
    newHash = paste(collapse=",",
                    Map(function(field) {
                      paste(sep="=",
                            field,
                            input[[field]])
                    },
                    url_fields_to_sync))
    
    # the VERY FIRST time we pass the input hash up.
    return(
      if (!firstTime) {
        newHash
      } else {
        if (is.null(input$hash)) {
          NULL
        } else {
          firstTime<<-F;
          isolate(input$hash)
        }
      }
    )
  })
  # Fin de SECTION URL  

  # define reactive values
  globvars <- reactiveValues(
    msg = 0,
   wd = c(),
   articles = c(),
   names_abr = c(),
   ztable = c(),
   choix = c(),
   resul = c(),
   con = c(),
   fxa = c(),
   fxb = c(),
   dList = c(),
   description = c(),
   mass = c(),
   Npopfacts = c(),
   newfact = c(),
   n_fact = c(),
   facts = c(),
   f1 = c(),
   r1 = c(),
   title = c(),
   liste_clients = c(),
   cl_results = c(),
   ser_results2 = c(),
   hist = c(),
   gr_hist = c(),
   molten_odds = c(),
   plot_results = c()
    )
 
 ## data for the renderUI function (factdescription1) : test liste d'infos (changer sur le serveur)
 observe({
   isolate({
    ### 
  test_liste_infos<-read.csv("www/Liste de faits.csv", header=TRUE, sep=",")
  # test_liste_infos<-read.csv("~/ShinyApps/en-elections/Liste de faits.csv", header=TRUE, sep=",")
 
  globvars$articles<-paste(test_liste_infos[,2],test_liste_infos[,3],test_liste_infos[,4],test_liste_infos[,5])
   })
 })
  
# lecture de la liste des clients sur le serveur
observe({
  isolate({
  ### changer le répertoire pour appli sur le serveur
    globvars$wd<-"www"
  #  globvars$wd<-"~/ShinyApps/en-elections"
  
  conn1<-paste(globvars$wd,'/liste_clients_jour.csv', sep = "")
  globvars$liste_clients<-read.csv(conn1,header=TRUE, sep=',')
  })
})

# lecture de l'historique
observe({
  isolate({ 
    conn2<-paste(globvars$wd,'/hist_odds.csv', sep = "")
    globvars$hist<-read.csv(conn2,header=TRUE, sep=',')
  })
})

# 1. reset input variables when expertise changes or input type changes
observe({ 
  input$expertise # pour effacer les données entrées lors du changement d'analyse
  updateSelectInput(session, "add_del", label = NULL,
        choices = c("Choosing an issue",
                    "Adding yourself an issue", 
                    "Removing an issue"), 
        selected =  "Choosing an issue")
  
}, label = "obs_level1", priority = 1)

# 2. reset input variables when input type changes
observe({ 
  input$add_del # pour effacer les données entrées lors du changement de type d'entrée
  updateSelectInput(session, "factdescription1", label = NULL,
                    choices = globvars$articles, selected =  (globvars$articles)[1])
  updateTextInput(session,"factdescription2", label = NULL, value = "")
  updateCheckboxGroupInput(session, "fact", label = NULL,
                           choices = globvars$dList, selected = NULL)
  updateSelectInput(session, "decimal",
                    choices = c("null", 
                                "very minor",
                                "minor",
                                "average",
                                "of a great importance", 
                                "very important", 
                                "key issue"
                    ), 
                    selected = "null")
  
  # test
 updateNumericInput(session, "nofait", label = NULL, value = 0)
 #  updateSliderInput(session, "nofait", label = NULL, value = 0)
# fin test

  updateTabsetPanel(session,"onglets",selected = "Faits") 
}, label = "obs_level2", priority = 2)

# 3. reset input 2 and 3 when input 1 changes
observe({ 
  input$factdescription1
  input$factdescription2
  updateCheckboxGroupInput(session, "fact", label = NULL,
                          choices = globvars$dList, selected = NULL)
  updateSelectInput(session, "decimal",
                    choices = c("null", 
                                "very minor",
                                "minor",
                                "average",
                                "of a great importance", 
                                "very important", 
                                "key issue"
                    ), 
                    selected = "null")
  }, label = "obs_level3", priority = 3)

  # Initialisations
  hyp<-reactive({
    # il faut demander input$expertise avant isolate, pour que le
    # changement d'expertise ait un effet
    input$expertise 
    isolate({
   if (input$expertise == "New analysis") {
      ztable<-matrix(c("Conservative Party","Liberal Party","New Democratic Party"),ncol=1)
      globvars$ztable<-as.matrix(ztable)
      globvars$names_abr<-c("seq", "CPC", "LPC", "NDP", "conflict", "nb_Analists")
   }
    return(globvars$ztable)
    })
  })
  
  # New expertise
  # Initialisation du tableau des faits 
  newexp<-reactive({
    # cette fonction hyp() doit être réactive et ne doit pas être dans isolate
    # pour que le changement d'expertise s'effectue
    
    globvars$n_fact<-0 # initialise le no du fait ajouté
    sortie<- t(hyp())  
    isolate({ 
    globvars$choix<-as.matrix(sortie[1,(1:dim(sortie)[2])])
    globvars$resul<-initsing(length(globvars$choix),globvars$choix)
    globvars$con<-0
    globvars$fxa<-matrix(0,nrow=0, ncol=(1+length(globvars$choix)))
    globvars$fxb<-matrix(" ",nrow=0,ncol=1)
    globvars$facts<-as.data.frame(cbind(NULL, globvars$fxa,globvars$fxb))
   colnames(globvars$facts)<-c("No","Support", globvars$choix,"Description")
    return(globvars$choix)
    })
 })
 
  # Example
  an_example<-reactive({
      input$expertise
    # data for an example
    # test
#      data(ex_elections7)
    ###
      zconn1<-(file("www/Ex-Federal-Elections.csv", open = "r"))
    # zconn1<-paste(globvars$wd,'/Ex-Federal-Elections.csv', sep = "")  

      isolate({
#     globvars$facts<-ex_elections7
      globvars$facts<-read.csv(zconn1, header=TRUE, stringsAsFactors =FALSE)
      globvars$facts<- globvars$facts[,-1] # on enlève les nos de ligne
   #   close(zconn1) # ne marche pas sur le serveur pour ce cas
      # fin test
      zz<-colnames(globvars$facts)
      globvars$choix<-matrix(zz[3:(length(zz)-1)],ncol=1) 
      globvars$dList<-as.list(globvars$choix) 
      globvars$fxa<-as.matrix(globvars$facts[,c(-1,-ncol(globvars$facts))])
      globvars$fxb<-as.matrix(globvars$facts[,ncol(globvars$facts)])
     globvars$resul<-initsing(length(globvars$choix),globvars$choix) # pour avoir tous les singletons
     globvars$resul<-nzdsr(dsrwon( dstCombListfacts(as.matrix(globvars$facts[,c(-1,-ncol(globvars$facts))])),globvars$resul))
      globvars$n_fact<-max(as.integer(globvars$facts[,1]))
      return(globvars$choix)
      })
  })
  
  # Input old facts
  old_facts<-reactive({
    input$expertise
    inFile<-input$factsList
    if (is.null(inFile)) {
      # User has not uploaded a file
      return(NULL) 
    } else {
      zconn<-(file(inFile$datapath, open = "r"))
      isolate({
        globvars$facts<-read.csv(zconn, header=TRUE, stringsAsFactors =FALSE)
        globvars$facts<- globvars$facts[,-1] # on enlève les nos de ligne
      })
      close(zconn)
      isolate({
        # test
        cat("old_facts, facts = :")
        print(globvars$facts)
      # fin test
        
        globvars$n_fact<-max(as.integer(globvars$facts[,1]))
      z1<-colnames(globvars$facts)
      globvars$choix<-matrix(z1[3:(length(z1)-1)],ncol=1)   
      # test
        cat("old_facts, choix= :")
        print(globvars$choix)
      # fin test
        # test
      globvars$fxa<-as.matrix(globvars$facts[,c(-1,-ncol(globvars$facts))])
      globvars$fxb<-as.matrix(globvars$facts[,ncol(globvars$facts)])
      # Évaluation du tableau de faits
        # test
      zof1<-as.matrix(globvars$facts[,c(-1,-ncol(globvars$facts))])
      zof2<-apply(zof1, c(1, 2), as.numeric)
      # ajouter initsing pour conserver tous les singletons dans les résultats
      globvars$resul<-initsing(length(globvars$choix),globvars$choix)
      globvars$resul<-nzdsr(dsrwon( dstCombListfacts(zof2),globvars$resul))
     # zgr<-globvars$resul<-dstCombListfacts(zof2)
      return(globvars$choix)
      })
     }
  })
     
   # Reprise d'une analyse sur iPad
   resume<-reactive({
     input$expertise
     resFacts<-input$startFacts
     if (is.null(resFacts)) {
       # User has not pasted a string yet
       return(NULL) 
     } else {
  #    zcon<-connec<-getURL(resFacts)
      isolate({
   #   globvars$facts<-read.csv(textConnection(connec)) 
        globvars$facts<-read.table(text = resFacts, header = TRUE, sep = "\t", quote = "", comment.char = "") # lecture d'un tableau format Excel
        
      globvars$facts<- globvars$facts[,-1]  # on enlève les nos de ligne
      globvars$n_fact<-max(as.integer(globvars$facts[,1]))
      zcol<-colnames(globvars$facts)
      globvars$choix<-matrix(zcol[3:(length(zcol)-1)],ncol=1)  
      globvars$fxa<-as.matrix(globvars$facts[,c(-1,-ncol(globvars$facts))])
      globvars$fxb<-as.matrix(globvars$facts[,ncol(globvars$facts)])
      # Évaluation du tableau de faits
      zo1<-as.matrix(globvars$facts[,c(-1,-ncol(globvars$facts))])
      zo2<-apply(zo1, c(1, 2), as.numeric)
      # ajouter initsing pour conserver tous les singletons dans les résultats
      globvars$resul<-initsing(length(globvars$choix),globvars$choix) # pour avoir tous les singletons
      globvars$resul<-nzdsr(dsrwon( dstCombListfacts(zo2),globvars$resul))  
      return(globvars$choix)
      })
     }
   })
   
# show input type 1
output$choix1<-renderUI({
  selectInput("factdescription1", 
     div(
         "",
    helpText("Mention of the issue (%CPC %LPC %NDP):")),
    choices = globvars$articles, 
 #   selected=(globvars$articles)[1], 
    selected = NULL,
    selectize = FALSE) 
})

# show input type 2
output$choix2<-renderUI({
    textInput("factdescription2", "", "")
})

# show input type 3
output$choix3<-renderUI({
  #test
  numericInput("nofait", 
                   "",
                 value= 0)
  #      sliderInput("nofait", 
  #                "", 
  #                min = 0, max = nrow(globvars$facts),
  #                value = 0, step= 1)
  # slider marche pas, si on n'a pas max de la colonne No de globvars$facts
    # test
})

output$valHyp <- renderUI({    
    if ((input$expertise != "Resume") & (input$expertise != "Resume (iPad-iPhone)") &(input$expertise != "Example")) { newexp()
    }  else if (input$expertise == "Example") { an_example()
      } else if (input$expertise == "Resume (iPad-iPhone)") {resume()
        } else { old_facts() 
        }
    
    globvars$dList<-as.list(globvars$choix)
    
    #test 
    if(input$add_del  != 'Removing an issue') {
      checkboxGroupInput(inputId="fact", 
           label = "",
          #   div(h4("2. Parti que vous favorisez relativement à l'information sélectionnée (cocher un ou plusieurs :")),
            choices = globvars$dList) 
    }
    # test
    })
  
output$num<-renderUI({
  if(input$add_del != 'Removing an issue') {
    selectInput("decimal", label = "",
            choices = c("null", 
                          "very minor",
                          "minor",
                          "average",
                          "of a great importance", 
                          "very important", 
                          "key issue"
            ), selected="null")  
            
    # sliderInput("decimal", 
              # div(h4("3 :"), "Degré de support de l'hypothèse choisie, étant donné votre information :"), 
    #  min = 0, max = 99, value = 0, step= 1)
  }
})

  #show new fact
  new_fact<-reactive({  
 #      on enlève isolate() pour que cette fonction réagisse dans tous les cas 
    input$goButton # on garde le isolate, mais la fn est activée par le goButton
    isolate({
       if (input$add_del == "Choosing an issue") { 
      globvars$description<-as.matrix(input$factdescription1, ncol=1) }
     # test
      if (input$add_del == "Adding yourself an issue") {
  #  else {
      globvars$description<-as.matrix(input$factdescription2, ncol=1) 
    }
      cat("new_fact, description = :") ; print(globvars$description)
      # test
  
    # test
    x<-input$fact
   # code original
    globvars$mass<- 0
    switch(input$decimal,
          "null"  = (globvars$mass<- 0), 
          "very minor" = (globvars$mass<- 1), 
          "minor" =  (globvars$mass<-2), 
          "average" = (globvars$mass <- 3),  
          "of a great importance" =  (globvars$mass<- 4),
          "very important" = (globvars$mass<- 5),
          "key issue" = (globvars$mass<- 6)
   )
# fin code original
  
  # calcul de m direct si m fournie, 
  # transformé si indice de 1 à 6 pour w fourni
  # Si calcul à partir de poids w, il faut :
  # entrer une taille de la population de faits 
      globvars$Npopfacts<- 22
  #   globvars$mass<- as.numeric(input$decimal)
  # cat("decimal= :") ; print(is.numeric(globvars$mass))
      if (globvars$mass < 1) {
        massFact<-globvars$mass
      } else { 
        globvars$mass<- 2^(globvars$mass-1)
        massFact <- 1-10^(-globvars$mass/globvars$Npopfacts) 
      }
   # test
      boolefact<-apply(outer(x,globvars$dList,"=="),2,sum) 
      # considérer le cas où massFact est sur l'ensemble complet
      if (sum(boolefact) == length(globvars$dList)) {
        globvars$newfact<-bca(massFact,t(matrix(boolefact,ncol=1)),globvars$choix)
    } else {
      vv<-c(massFact,1-massFact)
      ff<-matrix(c(boolefact,(rep(1,times=length(boolefact)))),nrow=2, byrow=TRUE)
      globvars$newfact<-bca(vv,ff,globvars$choix)
    }
    colnames(globvars$newfact$DempsterRule)<- c("Support",globvars$dList)
    cat("new_fact, n_fact= = :") 
    print(globvars$n_fact)
    # fin test 
    return(globvars$newfact)
     }) # fin du isolate
  #    }
  })

# Observe pour test si no de fait invalide
observe({
  if (globvars$msg == 1) {
  session$sendCustomMessage(
    type = 'testmessage',
    message = list(Action = "Removing an issue",Erreur = "No de fait invalide",Valeur = input$nofait))
  globvars$msg<- 0 # on remet à 0 l'indicateur de message
  }
})
# test

  #Cumulate table of facts
  Facts<-reactive({
  	input$goButton
    if (input$add_del == "Removing an issue") {
    isolate({
      globvars$n_fact<-max(as.integer(globvars$facts[,1]))  # on retient le nb de lignes du tableau après supression
      zn<-input$nofait
      
      # Test du no de fait et message par observe de globvars$msg si no invalide
      globvars$msg = 0
      zz3<-as.integer((globvars$facts)$No)
      if (!any(zz3 == zn)) { 
        # test
        globvars$msg = 1
        cat("Facts, no du fait en erreur, msg = :") ; print(globvars$msg)
       return()
    # fin test du no de fait
    
      } else {
      globvars$facts<-subset(globvars$facts,No !=zn)
      cat("Facts, no du fait supprimé, zn = :") ; print(zn)
      cat("Facts, facts=") ; print(globvars$facts)
      }
    })
    } else {
      
    # pour faire réagir la fonction, je mets deux input
 #   input$factdescription
 #   input$decimal
      # traitement général de tous les cas 
      isolate({
      globvars$n_fact<- 1+globvars$n_fact # ajout d'un fait
      zz2fxa<-t(as.matrix((globvars$newfact)$DempsterRule[1,]))
      # test
      zzf_add<-as.data.frame(cbind(globvars$n_fact,zz2fxa,globvars$description))
      colnames(zzf_add)<-c("No","Support", as.character(globvars$dList),"Description")
      globvars$facts<-as.data.frame(rbind(globvars$facts,zzf_add))
      })
      }
      isolate ({
        colnames(globvars$facts)<-c("No","Support", as.character(globvars$dList),"Description")
       
        ## name hypothesis and prepare simplified table
        zhyp1<-globvars$facts[,(3:(2+length(globvars$choix)))]
        zhyp2<-apply(zhyp1,c(1,2),as.numeric)
        ident_f<-identRow(zhyp2)
        sf<-as.matrix(globvars$facts[,c(1:2,ncol(globvars$facts))])
        sf[,2]= round(-(globvars$Npopfacts)*log10(1-as.numeric(sf[,2])),1)
        globvars$f1<-data.frame(cbind(ident_f,sf))
        colnames(globvars$f1)<-c("Hypothesis", "No", "Weight", "Description")
#   return(globvars$f1)
	return(globvars$facts)
      })
  }) 

  # Compute and show the result  for new fact added
  calcDempster<-reactive({
    input$goButton # pour réagir à l'ajout de fait
  	new_fact() # nécessaire pour mettre à jour resul
    if (input$add_del == "Removing an issue") {
      # test
    # zf<-globvars$facts<-globvars$facts[-globvars$n_fact,]
     input$nofait
     isolate({
      Facts()
      zf<-globvars$facts
      cat("calcDempster, zf=: ")
      print(zf)
      # fin test
      zf1<-as.matrix(zf[,c(-1,-ncol(zf))])
      zf2<-apply(zf1, c(1, 2), as.numeric)
      # ajouter initsing pour conserver tous les singletons dans les résultats
      globvars$resul<-initsing(length(globvars$choix),globvars$choix)
      globvars$resul<-nzdsr(dsrwon( dstCombListfacts(zf2),globvars$resul))
      globvars$con<-globvars$resul$con
      # test
      cat("delete, globvars$con = :") ; print(globvars$con) 
         })
       }
  	# bloc pour ajout de fait
    ## test if
    if (input$add_del != "Removing an issue") {
    isolate({
      
      # test
      if (is.null(globvars$newfact)) {
        return()
        } else {
      globvars$resul<-nzdsr(dsrwon(globvars$newfact,globvars$resul)) 
        }
      # test
      
      cat("calcDempster, resul= :")
      print(globvars$resul)
      # test
        })
    }
    ## test
    # pour faire réagir la fonction, je mets les deux input de faits
  #   input$factdescription
  #   input$decimal
        
    # retain singletons
    isolate({
      # test
      singl<-1
      if (singl != 1) {
        zr<-tabresul(globvars$resul)
      } else {
        zr<-tabresulSingl(globvars$resul)
      }
    #  cat("zr = :") ; print(zr)
    # fin test
    r<-round(zr$mbp,digits=3)
    if (is.null(dim(r))) { r<-data.frame(t(r))
    } else {
      r<-data.frame(r)
    } 
    # name hypothesis
    zhyp<-as.matrix(r[,(1:length(globvars$choix))], ncol=length(globvars$choix))
    colnames(zhyp)<-globvars$choix
    ident<-identRow(zhyp)
    
    ## Remove "frame" row if singl != 1 and more than 1 row
    if ((length(ident) > 1) & (singl != 1)) {
      ident<-ident[-length(ident)]
    }
    
    sr<-as.matrix(r[,-(1:length(globvars$choix))], ncol=4)
    if ((nrow(sr) > 1) & (singl != 1)) {
      sr1<-matrix(sr[-nrow(sr),], ncol=4)
    } else {
      sr1<-sr
      # test
      rownames(sr1)<-ident
      # fin test
    }
    # obtain final table
    globvars$r1<-data.frame(cbind(ident,sr1))
    colnames(globvars$r1)<-c("Hypothesis","Support", "Belief", "Plausibility","For/Against")
    
    # mettre à jour l'indice de conflit
    globvars$con<-globvars$resul$con
    
    # mettre à jour le tableau des faits
  	Facts()
      })
 
 # préparation du graphique des résultats
 # test
 # pas de isolate, pour avoir la variable à jour
 
 # tetst
 # yy1<<-globvars$r1
 sr2<-as.data.frame(sr1)
#  if (nrow(yy1) == 0) {
if (nrow(sr2) == 0) {
   return(NULL)
 } else {
   # test
#   yy10<-data.frame(seq = seq(1,nrow(yy1)),yy1[,c(1,5)], row.names=yy1[,1])
  znames<<-rownames(sr2) # ATTENTION : ne marche pas si pas globale 
#   globvars$plot_results<-print(ggplot(yy10, aes(x = factor(Hypothesis), y = For.Against)) + geom_bar(stat = "identity", fill = "green") + scale_x_discrete(labels = rownames(yy10)) + xlab("Party") +ylab("For/Against"))
yyplot<-globvars$plot_results<-print(ggplot(sr2, aes(x = znames, y = Odds)) + geom_bar(stat = "identity", fill = "green") + xlab("Party") +ylab("For/Against Ratio"))
   #     })
 }
 return(globvars$r1)
  })
     
	# Show table of results
  output$view5 <- renderTable({  
 	input$goButton # pour réagir à l'ajout de fait
   isolate({
     if (is.null(globvars$r1)) {
       cat("view5, r1 = :"); print(globvars$r1)
     # initialiser le tableau de résultats  
     globvars$r1<-matrix(0,nrow=0, ncol=5)
     colnames(globvars$r1)<-c("Hypothesis","Support", "Belief", "Plausibility","For/Against")
     return() 
     } else {
       # test OK
#    calcDempster()
    # test
    subset(globvars$r1,select = -Support)  
    }
   })
  })
 
# plot results graph of pl ratios
output$plot0 <- renderPlot({
  input$goButton # pour réagir à l'ajout de fait
  print(globvars$plot_results)
})
    
   # Show conflict indice
  output$view5a <- renderText({
  	# test
  input$goButton # pour réagir à l'ajout de fait
  
  #  test
   # isolate(globvars$resul$con)
  isolate(globvars$con)
  # test
  })
  
  # enregistrer le fichier des faits dans le dossier des téléchargements
   output$downloadResults <- downloadHandler(
    filename = function() {
      nom1<-paste(input$userfilename1)
      paste(nom1, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
    write.csv(calcDempster(), file)
    }
  )
  
   # Show table of facts
   output$grid <- renderDataTable({
    # test
  #  input$expertise # X pas nécessaire
 #    globvars$choix # X pas nécessaire
     # fin test
    input$goButton # pour réagir à l'ajout de fait
    isolate({
      if (is.null(globvars$f1)) {
        # initialiser le tableau des faits  
        globvars$f1<-matrix(0,nrow=0, ncol=4)
        colnames(globvars$f1)<-c("Hypothesis", "No", "Weight", "Description")
        return() 
      } else {   
      calcDempster()  # mettre à jour les résultats et le tableau des faits 
      }
      }) # pour mettre à jour les résultats et le tableau des faits 
   globvars$f1[,c(4,1,3,2)] # Tableau des faits à afficher
   }, options = list(lengthMenu = c(5, 10, 15, 30, 60),
                     pageLength = 10,
                     autoWidth = FALSE)
   )
  
  # enregistrer le fichier des faits dans le dossier des téléchargements
  output$downloadFacts <- downloadHandler(
    filename = function() {
      nom2<-paste(input$userfilename2)
      paste(nom2, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(globvars$facts, file)
    }
  )

# Ajout des résultats du client aux résultats de la journée
# publish<-reactive({
observe({
  # A. déterminer si le résultat est admissible
  cond1 <- 0 # test si premier submit de l'analyse
  # pour prendre seulement le premier submit de l'analyse
  
  cond2 <- 0 # à faire : test  si adr IP déjà dans BD pour la date du jour
  # pour prendre seulement 1 analyse par jour par client
  # 1. détecter si submit est cliqué et tester si première fois
  zs1<-input$submit_ana 
  cat("observe submit_ana= ") ; print(zs1)
  
  # on prend seulement la première soumission
   if (input$submit_ana != 1) {
   isolate(return(NULL))
  } else {
     cond1 = 1
  }
  
  # 2. Tester si adresse IP déja dans base pour la date du jour (fichier test_client = liste_clients_jour.csv)
  #
  # *** cond2 à terminer
  #
  
  IP_adr<-session$request$REMOTE_ADDR
  # test_client  ouvert au début du prog
  date<-as.character(globvars$liste_clients[,1])
  IP_client<-as.character(globvars$liste_clients[,2])
  cond2<<-any(date == Sys.Date() & IP_client == IP_adr)
  cat("test cond2 = : ") ; print(cond2)
 
  # B. Si cond1 == 1 et cond2 == 1,
  # enregistrer fichier des résultats et indice de conflit sur le serveur
  if (cond1 == 1) {
    isolate({
    conflict<-globvars$resul$con  
    globvars$cl_results<-cbind(globvars$resul$DempsterRule,conflict)
    
    # Enregistrer les résultats du client et ajouter client dans liste
    result_client(globvars$cl_results, IP_adr, globvars$wd)
    })
  # fin des enregistrements du client
  
 # C. Combiner le résultat du client avec le résultat global du jour
 isolate({
   conn3<-paste(globvars$wd,"/",init_day_results(globvars$wd),sep="")
   ser_results<-read.csv(conn3,header=TRUE, sep=',')
   x<-list(DempsterRule = ser_results[,-c(1,ncol(ser_results))],con = ser_results[1,ncol(ser_results)])
   y<-list(DempsterRule =globvars$resul$DempsterRule, con = globvars$resul$con)
   zz<-nzdsr(dsrwon(x,y))
   cum_conflict<-zz$con 
   zzr<-zz$DempsterRule
   date<-as.character(Sys.Date())
   globvars$ser_results2<-data.frame(date, zz$DempsterRule,cum_conflict)
  # file_place<-paste(getwd(),"/www/time_data",sep="")
 #  write.csv(globvars$ser_results2, paste(file_place,"/",Sys.Date(),"_ser_results.csv",sep = ""),row.names=FALSE)
 write.csv(globvars$ser_results2, paste(globvars$wd,"/",Sys.Date(),"_ser_results.csv",sep = ""),row.names=FALSE)
  
 # D. ajouter la mise à jour du résultat quotidien à l'historique
globvars$hist<-update_hist(globvars$ser_results2, globvars$hist, globvars$wd)

 # écrire le fichier mis à jour
 file_place<-paste(globvars$wd,"/hist_odds.csv", sep = "")
 write.csv(globvars$hist, file_place,row.names=FALSE)
 
 })
 # fin de if cond1 == 1
  }
 #
})

# E. Plot des résultats
# Graphique de tendance
output$plot1 <- renderPlot({
  # pas de isolate, pour avoir la variable à jour
  # hist lu au début du programme
  zz1<<-globvars$hist
  input$submit_ana  # pour que le graphe soit mis à jour
  isolate({
  zz2<<-globvars$gr_hist<-plot_hist_prep(globvars$hist,globvars$names_abr)
  
# molten1<- melt(globvars$gr_hist[,-c(8,9)], id.vars = "seq")
gr_hist<-subset(globvars$gr_hist,select=-c(conflict, nb_Analists))
  molten1<- melt(gr_hist, id.vars = "seq")
  print(ggplot(molten1, aes(x = seq, y = value, colour = variable)) + geom_line() + scale_x_discrete(labels = rownames(gr_hist)) + xlab("Date") +ylab("For/Against"))
  })
})

output$grid2 <- renderDataTable({
  # Take a dependency on input$
  input$submit_ana  # pour que le tableau soit mis à jour
#  con_ser<-data.frame(date=rownames(globvars$gr_hist),conflit=globvars$gr_hist[,8], nb_Analyses=globvars$gr_hist[,9])
con_ser<-data.frame(date=rownames(globvars$gr_hist),subset(globvars$gr_hist, select = c(conflict, nb_Analists)))
  return(con_ser)
}, options = list(lengthMenu = c(5, 10, 15, 30, 60),
                  pageLength = 30,
                  autoWidth = FALSE
                  )
)
})