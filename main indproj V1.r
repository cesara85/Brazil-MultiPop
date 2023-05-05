# Projection 
 
icheck = T #to collect the sums at each stage (sx_propedu, mig,migC,migfinal,)
id.cols = c("Time","region","sex","agest","edu") #also used during the projection
iper = 2020

final = copy(popdt)[,`:=`(births=0,pop1=0,idom=0,odom=0,emi=0,imm=0,edutran=0,deaths=0,poprecl=0)]
#check [see non-negative values for 0-80]
final[Time==2011,by=.(agest),sum(pop)]

final[pop>0&Time==2011,sum(pop)]
# 1,210,827,147

unlink(dir(path_scen,full.names = T))
#check again
if(round(popdt[pop>0&Time==2011,sum(pop)],0)!=1210827147) print("India pop not matching line 17")
ireg = regions;{ #to run for all regions at the same time
# for(ireg in regions) {
   
  iper = 2011
  # iper = iper+5
   print("Until 2091")
  for (iper in seq(initime,fintime-ts,ts))
     {
    print(iper)
    
    
    #adult sx and edu
    {#sx and edu
    #pop1
      
    final.temp = copy(final)[Time == iper&region%in%ireg]# chose an many regions you want
    
    # final.temp[,sum(pop),by=.(agest)]
    # final.temp[,sum(pop)]
    
    #this way sx is not saved as a separate file [births -5 is empty]
    # final.temp[is.nan(pop),pop:=0.001] #??check this out
    # sxdt[region=="IN.UP_rural"&Time==2011&sex=="m"&agest==20]%>%spread(edu,sx)
    
    final.temp <- final.temp[sxdt,on=id.cols,`:=`(sx=sx,pop1=pop*sx,deaths=pop*(1-sx))]
    # final.temp[,sum(deaths)] #40,127,159
    # final.temp[is.na(pop1)]
    # final.temp[region=="IN.BR_rural"&agest==0]
    if(iper < 2026) print("take care of sx edu")
    # final.temp[pop1 < 0 ]
    #eduprop [new transitions]
    # time and age as initime+5, corrected to match initime
    ieduprop <- copy(propdt)[Time==iper+5 & region%in%ireg][
      ,`:=`(Time=Time-5,agest=agest-5)][prop<0,prop:=0.001][
      ,.(edu=edu,prop=prop.table(prop)),by=setdiff(id.cols,"edu")]

      # xx <- final.temp[sex=="f"&agest==15,.(edu,pop)][,prop:=prop.table(.SD$pop)]
    
    #agr pop 
    pop1agr <- copy(final.temp)[agest%in%10:25][,.(pop1=sum(pop1)),by=setdiff(id.cols,"edu")]
    #merge
    ieduprop[pop1agr,on=setdiff(id.cols,"edu"),`:=`(pop1=pop1*prop)]
    #check (0.9999??)
    #bring pop1edu into pop1 (update)    
    final.temp[ieduprop,on=id.cols,`:=`(pop1=i.pop1,edutran = pop1-i.pop1)]
    vars = names(final.temp)[6:15]
    rm(ieduprop,pop1agr)
    }

  #check (edutran = 0)
     if(icheck){ 
       final.summ <-final.temp[,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="sx_eduprop"]
        print(final.summ)
        print(final.summ)
        
        print("UP rural")
        final.reg.summ <-final.temp[region=="IN.UP_urban",lapply(.SD,sum),.SDcols = vars,by=.(Time)
                                    ][,pop1:=pop+births-deaths][,stage:="sx_eduprop"]
        print(final.reg.summ)
        print(final.reg.summ)
        
       }
     # Time     pop births    pop1 emi imm       edutran   deaths stage
  # 1: 2020 7802030      0 7518767   0   0 -1.729177e-10 283262.2  pop1
  
  #emig0 means use given education-specific migration rate
  #Here in any case - we need to adjust for scenarios + net0 
  if(grepl("emig",iscen_text)){
    final.temp[copy(emrdt)[,agest:=agest-5], #2020-2025, age at 2025, so to match with pop1 (with age at 2015)
               on=.(Time,region,sex,edu,agest),`:=`(emr=i.emr,emi=pop1*i.emr)]#end of the period (to be applied, age is not there)
  }   
 
  if(grepl("_dom",iscen_text)){
       #Bilateral implementation
    #-5 to be added to newborns
    # stop("xxxa;'dsflkj")   
    pop.origin = copy(final.temp)[agest>-5&agest<=75,.(region,Time,sex,edu,agest,pop1)]
      
       dom.iper <- copy(migOD_AG)[,Time:=iper][dom.ssp, on=.(Time,region),mrate.pred := mrate.pred * ssp.adj]
       
       dom.temp = merge(pop.origin,dom.iper,allow.cartesian=TRUE)
       dom.temp[,odom:=pop1*mrate.pred]
       dom.temp[,sum(odom)]
       
       odom.temp <- copy(dom.temp)[,by=.(Time,region,sex,agest,edu),.(odom=sum(.SD$odom))]
       idom.temp <- copy(dom.temp)[,by=.(Time,dest,sex,agest,edu),.(idom=sum(.SD$odom))
                                   ][,setnames(.SD,"dest","region")]
       
       final.temp[odom.temp,on=id.cols,odom:=i.odom]
       final.temp[idom.temp,on=id.cols,idom:=i.idom]
     }   
     
    
    
  final.temp[,pop1:=pop1-emi+imm-odom+idom]
   # copy(final.temp)[,.(corr = sum(emi)/sum(imm))]
   if(icheck) { 
     final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
        ][,stage:="mig"]
      final.summ = rbind(final.summ,final.summX)
      print(final.summ)
      
      print("UP rural")
      final.reg.summX <-final.temp[region=="IN.UP_urban"][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,stage:="mig"]
      final.reg.summ = rbind(final.reg.summ,final.reg.summX)
      print(final.reg.summ)
   }
   
     
  #births
  birthsx <- final.temp[sex=="f"][,`:=`(odom=NULL,idom=NULL,emi=NULL,imm=NULL,edutran=NULL,sex=NULL,sx=NULL)][,#this are still empty
      `:=`(popavg=.5*(.SD$pop+shift(.SD$pop1,1,NA,"lag"))),by = .(region,edu)] [,
      `:=`(pop1=NULL,pop=NULL,deaths=NULL)][agest%in%15:49,][asfrdt,#age 15 at the start of iper
       on=setdiff(id.cols,"sex"),#get the asfr 
      `:=`(births=(popavg*ts)*(i.asfr/1000))][#calculate births
        ,popavg:=NULL][srbdt, #bring srb
       on=.(region,Time),`:=`(m=births*i.srb/(1+i.srb),f=births*1/(1+i.srb))][,births:=NULL]
  birthsx <- melt(birthsx,id=setdiff(id.cols,"sex"),variable.name = "sex",value.name = "births")
  
  #update births to mother's row (later to update the final initime)
  final.temp[birthsx,on=id.cols,`:=`(births=i.births)]
  # final.summ <-final.temp[region=="reg356",lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-pop1]
  
  if(icheck) {
    final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
    ][,pop1:=pop+births-deaths][,stage:="births"]
    final.summ = rbind(final.summ,final.summX)
    print(final.summ)
    
    print("UP rural")
    
    final.reg.summX <-final.temp[region=="IN.UP_urban"][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="births"]
    final.reg.summ = rbind(final.reg.summ,final.reg.summX)
    print(final.reg.summ)
    
  } 
  
  #total births (by edu and sex) will be added to the -5 at final.temp initime
  birthstot = birthsx[,.(pop=sum(births)),by=setdiff(id.cols,"agest")][,agest:=-5]
  
  #update births [ need this for emort e015]
  final.temp[copy(birthstot),on=id.cols,`:=`(pop=i.pop)]
  
  # final.temp[agest==-5,sum(pop)]
  # final.temp[,sum(births)]
  #emort
  # <15 + new born.. get nsx corresponding to the difference in e0_e15
  birthstot <- birthstot[sxdt,on=id.cols,`:=`(pop1=pop*sx,deaths=pop*(1-sx))]
  #add pop pop1 deaths 
  final.temp[copy(birthstot),on=id.cols,`:=`(pop1=i.pop1,deaths=i.deaths)]
  #check this..

  if(icheck) {
    final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
    ][,pop1:=pop+births-deaths][,stage:="births"]
    final.summ = rbind(final.summ,final.summX)
    print(final.summ)
    
    print("UP Urban")
    final.reg.summX <-final.temp[region=="IN.UP_urban"][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="births"]
    final.reg.summ = rbind(final.reg.summ,final.reg.summX)
    print(final.reg.summ)
  }
  
  

  {#Reclassification
    if (T) {
      area.vars = c("state","residence")
      proprur <- final.temp[, .(pop1 = sum(pop1)), region]
      
      proprur[, c(area.vars) := tstrsplit(proprur$region, "_")]
      proprur <- dcast(proprur, state ~ residence, value.var = "pop1")
      proprur[, newperural := rural / (rural + urban)]
      proprur$gap <- reclass.dt$gap[match(proprur$region, reclass.dt$area)]
      
      
      
      #conditional computation of reclasstr:
      if (exists("glmMigrReclass")) {
        proprur[, `:=` (reclasstr = (predict(glmMigrReclass, data.frame(perural = proprur$newperural), type = "response") + gap) * SSP$recl[iPr])] #1.05ms
        proprur[areas.oldtr$area, reclasstr := areas.oldtr$reclasstr]
        proprur <- proprur[, .(region, reclasstr = reclasstr / (reclass.period / 5))]
      } else {
        proprur <- proprur[,.(state)][reclass.dt,on=.(state),reclasstr:=i.reclasstr*reclass.ssp[Time==iper,ssp.adj]]
      }
      
      # resProj[, pop3.shift.total := rep(resProj[, .(sum(pop3.shift)), by = setdiff(p.vars, "residence")]$V1, each = 2)]
      # resProj[, `:=` (perural = pop3.shift / pop3.shift.total), ]
      # if(iper == 2016) stop("samire")
      
      #calculate current prop rural 
      final.temp.state <- copy(final.temp)[,c(area.vars) := tstrsplit(region, "_")
                 ][, .(stpop1 = sum(pop1)), by =  .(sex, agest, edu, state)]
      
      final.temp[,c(area.vars) := tstrsplit(region, "_")
                 ][final.temp.state,on=.(sex, agest, edu, state),`:=`(stpop1=i.stpop1,perural=pop1/stpop1)]
      
      final.temp[is.nan(perural),perural:=0]
      #apply constant rate of reclassification [FOR NOW]
      final.temp <- final.temp[proprur, on = "state", nomatch = NA]

      # final.temp[state=="IN.UP"&agest==20&edu=="e1"&sex=="f"]
      #rural will lose people #same percetange over all age-groups
      final.temp[residence == "rural", `:=`(poprecl = - (stpop1 * perural * reclasstr))]
      #urban will gain
      final.temp[residence == "urban", poprecl:= (stpop1 * (1-perural) * reclasstr)]
      
      # sum(final.temp[,sum(poprecl)])
      
      
      final.temp[,pop1:=pop1+poprecl] #update
      
      #replace all nas with 0
      for (j in c("perural", "pop1")) set(final.temp, which(is.na(final.temp[[j]])), j, 0)
    } 
  }#Reclass
  

  #add in final total births initime age -5
  final[copy(birthstot),on=id.cols,`:=`(pop=i.pop)] #pop1 will be updated later

  #Update the final with pop1, births, mig, edu transition
  final[copy(final.temp),on=id.cols,
        `:=`(deaths=i.deaths,births=i.births,imm=i.imm,emi=i.emi,odom=i.odom,idom=i.idom,edutran=i.edutran,poprecl=i.poprecl)] #new
  
  
  #End of the period age and Time
  #prepare for the next year [5+]
  final.temp.end<-copy(final.temp)[,`:=`(Time=Time+ts,agest=agest+ts,pop=pop1,pop1=NULL)]
  final.temp.end[agest>=120,agest:=120][,pop:=sum(pop),by=id.cols]
  
  # final.temp[,sum(pop)]
  # final.temp.end[,sum(pop)]
   
  #add end of the year population to the final
  final[final.temp.end,on=id.cols,`:=`(pop = i.pop)]
  
  if(icheck){

    final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
    ][,pop1:=pop+births-deaths][,stage:="reclass"]
    final.summ = rbind(final.summ,final.summX)
    print(final.summ)
    
    print("UP Urban")
    final.reg.summX <-final.temp[region=="IN.UP_urban"][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="reclass"]
    final.reg.summ = rbind(final.reg.summ,final.reg.summX)
    print(final.reg.summ)
    }
  
  }#loop of iper
}#for single country 
{

  
final <- final[pop==-999,pop:=-0.00001]#for year 2100 births
save(final,file=paste0(path_scen,"res_",iscen_fullname,".RData",sep=""))

vars = setdiff(names(final),id.cols)
# [region=="reg108"]
final.summ.temp <-final[,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time)][
  ,`:=`(pop=pop-births)][,pop1:=NULL] #births are already in 'pop'
print("get absolute edu transitions")

DT::datatable(data = round(final.summ.temp,0))




if(final.summ.temp[,sum(emi)]==0){
  print(round(final.summ.temp[,edutran:=NULL][,emi:=NULL][,imm:=NULL],0))
} else {
  print(round(final.summ.temp[,edutran:=NULL],0))
}

library(gt)
round(final.summ.temp,0)%>%
  gt() %>%
  tab_header(
    title = iscen_fullname,
    # subtitle = glue::glue("{Global Population} to {}")
  )%>% gtsave(paste0("../results/summary table",iscen_fullname,".png"))

#save results
# username = "kc"
#these files will be loaded for running different scenarios

#save dttosave
for(ifile in dttosave) {
  xxx<-get(ifile);save(xxx,file=paste(path_scen,ifile,".RData",sep=""))
  # if(username=="kc") save(xxx,file=paste(pdrive_path_scen,ifile,".RData",sep=""))
}  


#quick pyramid
if(F){
  # final
  dir(path_scen)
  dir(path_scen,pattern = "res_")
  # load(file=paste(path_scen,"res_",iscen_fullname,as.numeric(Sys.time()),".RData",sep=""))
  final<-final[,scen:="Med"][Time<2096]
  source("funstack from mcbm.r")
  regions
  ireg = "IN.MH_urban"#regions[1]
  icnt =  ireg
  
  # function(figval,ivar,iage,isex,iregions,icnt,itob,iiscen,ipropgraph=F,iscale=1,ireg=ireg)
  
  ggpyr2011<-funpyrwrapper_mcbm(figval = copy(final),
                           ivar="pop",
                           iTime=2011,
                           iiscen="Med",#can be deleted
                           iscale=1000)
  ggpyr2011
  ggpyr.col<-funpyrwrapper_mcbm(figval = copy(final),
                                ivar="pop",
                                iTime = unique(final$Time),
                                iiscen="Med",#can be deleted
                                iscale=1000)
  ggpyr.col
}
}#End Projection
# See "Report WIC3.Rmd"
