# -------------
# fonctions pour suivre les résultats de l'application "Élections dans le temps
# Claude Boivin, Stat.ASSQ
# Québec, Canada
# Début : 2014-01

# enregistrer fichier des résultats du client sur le serveur
result_client<-function(x1,IP_adr, wd) {
# inputs :
# 1. x1 : tableau resul$DempsterRule + resul$con
# 2. IP_adr : adresse IP du client
# 3. wd : file path

# output
# cl_results.csv : Résultats de Dempster Rule + Indice de conflit
  
# NB: le setwd du répertoire est fait dans server.R

  # Écrire les résultats d'un client
date<-Sys.Date()
file_cl<- data.frame(date,IP_adr,x1)
conn<-paste(wd,"/",IP_adr,"-cl_results.csv", sep = "")
write.csv(file_cl,conn,row.names=FALSE)

# ajouter le client dans la liste des clients
con<-subset(x1,select = conflict)
cl<<-data.frame(Date=as.character(Sys.Date()),IP_client=IP_adr,conflit=con)
conn5<<-paste(wd,"/liste_clients_jour.csv", sep = "")
### à revoir
write.table(cl, conn5, col.names=FALSE, row.names=FALSE, sep=",", append=TRUE)
}

#  result_server<-function() {
# Ajout du résultat du  client au global sur serveur
# condition : la date doit être la même

# Déterminer le fichier du serveur à utiliser pour laa journée
init_day_results<-function(wd) {
# Tester si fichier du serveur présent pour la journée
# pas de fichier présent si c'est le premier client
# examiner répertoire du serveur
# test : changer ce répertoire sur le serveur
  
   files_list<-dir(wd)
  
cat("init_day_results, files_list =:") ; print(files_list)
day_results<-paste(Sys.Date(),"_ser_results.csv",sep="")
cat("init_day_results, day_resuts =:") ; print(day_results)
if (any(files_list == day_results) == FALSE) {
  day_results<-"init_ser_results.csv"
}
cat("init_day_results, day_resuts2 =:") ; print(day_results)
return(day_results)
}

# Mettre à jour l'historique
update_hist<-function(x1,x2, wd) {
  # si date du jour présente, remplacer donnée de cette date par le résultat de la journée 
  #  si absente, faire append
  # x1 = ser_results2 : résultat du jour sur serveur mis à jour avec le client
  # x2 = hist_odds.csv : fichier d'historique
  # wd : file path
  
  # 1. ser_results2 : résultat du client combiné avec le résultat global du jour
  #  un fichier de résultats à àjouter à l'historique 
  # output : fichier hist_odds.csv à jour 
  
  x1_list<-list(DempsterRule=subset(x1,select=-c(date,cum_conflict)), con=rename(subset(x1[1,],select=cum_conflict),replace= c("cum_conflict"="conflit")))
  x1_singl<-tabresulSingl(x1_list)
  r<-round(x1_singl$mbp,digits=3)
  r<-data.frame(r)
  znames<-colnames(subset(r,select=-c(mass, Belief, Plausibility, Odds)))
  zodds<-t(subset(r, select=c(Odds)))
  colnames(zodds)<-znames
  zz1<-zodds1<-data.frame(date=as.character(Sys.Date()), zodds, conflit=x1_list$con)
  # tester si date présente dans l'historique
  date<-as.character(x2[,1])
  cond3<-(date[length(date)] == zodds1[1,1])
  cat("update_hist, cond3 = :") ; print(cond3)
  if (cond3 == TRUE) {
    nb_Analyses<- x2[nrow(x2),6]+1
    x2[nrow(x2),]<-cbind(zodds1,nb_Analyses)
    zzx2<-x2
  } else {
    nb_Analyses<-1
    zodds2<-cbind(zodds1,nb_Analyses)
    cat("nbcol zodds2") ; print(dim(zodds2))
    cat("nbcol x2") ; print(dim(x2))
    zzx2<-rbind(x2,zodds2)
  }
  
  # écrire le fichier mis à jour
 # file_place<-paste(wd,"/hist_odds.csv", sep = "")
#  write.csv(x2, paste(file_place,"/","hist_odds.csv",sep = ""),row.names=FALSE)
 # write.csv(zzx2, file_place,row.names=FALSE)
  return(zzx2)
}

# conversion de l'historique en série chronologique
plot_hist_prep<-function(x, znames_abr){
  # x : historique 
  # y : historique modifié pour le graphe
  y<-data.frame(seq = seq(1,nrow(x)),x[,-c(1,8)], row.names=x[,1])
  colnames(y)=znames_abr
 return(y)
}