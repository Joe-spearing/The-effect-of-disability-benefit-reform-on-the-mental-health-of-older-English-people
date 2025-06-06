rm(list=ls())
set.seed(811072144)
library(haven)
require(aod)
library(stargazer)
library(rdrobust)
library(stringr)
library(rdd)
library(estimatr) 

#Load ELSA data
data_directory<-
  'J:\\Projects\\Lomas\\Data\\UKDA-5050-stata\\stata\\stata13_se'
  #'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\PIP_RDD\\ELSA_EUL_data'
charts<-
  'J:\\Projects\\Lomas\\Charts'
  #'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\PIP_RDD\\charts'
setwd(data_directory)
common.vars<-as.data.frame(read_dta('wave_0_common_variables_v2.DTA'))
wave_1_core_data<-as.data.frame(read_dta('wave_1_core_data_v3.DTA'))
wave_1_fin_data<-as.data.frame(read_dta('wave_1_financial_derived_variables.DTA'))
wave_0_1998_data<-as.data.frame(read_dta('wave_0_1998_data.DTA'))
wave_0_1999_data<-as.data.frame(read_dta('wave_0_1999_data.DTA'))
wave_0_2001_data<-as.data.frame(read_dta('wave_0_2001_data.DTA'))

wave_2_core_data<-as.data.frame(read_dta('wave_2_core_data_v4.DTA'))
wave_2_fin_data<-as.data.frame(read_dta('wave_2_financial_derived_variables.DTA'))

wave_3_core_data<-as.data.frame(read_dta('wave_3_elsa_data_v4.DTA'))
wave_3_fin_data<-as.data.frame(read_dta('wave_3_financial_derived_variables.DTA'))

wave_4_core_data<-as.data.frame(read_dta('wave_4_elsa_data_v3.DTA'))
wave_4_fin_data<-as.data.frame(read_dta('wave_4_financial_derived_variables.DTA'))

wave_5_core_data<-as.data.frame(read_dta('wave_5_elsa_data_v4.DTA'))
wave_5_fin_data<-as.data.frame(read_dta('wave_5_financial_derived_variables.DTA'))

wave_6_core_data<-as.data.frame(read_dta('wave_6_elsa_data_v2.DTA'))
wave_6_fin_data<-as.data.frame(read_dta('wave_6_financial_derived_variables.DTA'))

wave_7_core_data<-as.data.frame(read_dta('wave_7_elsa_data.DTA'))
wave_7_fin_data<-as.data.frame(read_dta('wave_7_financial_derived_variables.DTA'))

wave_8_core_data<-as.data.frame(read_dta('wave_8_elsa_data_eul_v2.DTA'))
wave_8_fin_data<-as.data.frame(read_dta('wave_8_elsa_financial_dvs_eul_v1.DTA'))
#wave_8_prim_data<-as.data.frame(read_dta('wave_8_elsa_data_sl_primary_v2_protect.DTA'))

wave_9_core_data<-as.data.frame(read_dta('wave_9_elsa_data_eul_v1.DTA'))
wave_9_fin_data<-as.data.frame(read_dta('wave_9_financial_derived_variables.DTA'))

wave_10_core_data<-as.data.frame(read_dta('wave_10_elsa_data_eul_v4.DTA'))
wave_10_fin_data<-as.data.frame(read_dta('wave_10_financial_derived_variables.DTA'))

#deaths<-as.data.frame(read_dta('elsa_endoflife_w6archive.DTA'))

#Do the following test: see if you can find
#a couple, one of whom claims and the other of 
#whom does not.
#claim.hh<-unique(subset(wave_1_fin_data,dla_r_i>0&futype==3)[,'idahhw1'])
#df<-subset(wave_1_fin_data,idahhw1%in%claim.hh)
#cbind(df[,'idauniq'],df[,'dla_r_i'],df[,'idahhw1'])

month.key<-cbind(c("Apr",
                   "May",
                   "Jun",
                   "Jul",
                   "Aug",
                   "Sep",
                   "Oct",
                   "Nov",
                   "Dec",
                   "Jan",
                   "Feb",
                   "Mar"),
                 c(4,5,6,7,8,9,10,11,12,1,2,3))
retirement.ages<-read.csv('retirement_ages_and_dob.CSV')
retirement.ages<-retirement.ages[1:(nrow(retirement.ages)-1),]
start.yr<-1900+as.numeric(substr(retirement.ages[,'Start.DoB.range'],8,9))
start.mon<-month.key[match(substr(retirement.ages[,'Start.DoB.range'],4,6),
                           month.key[,1]),2]
end.yr<-1900+as.numeric(substr(retirement.ages[,'End.DoB.range'],8,9))
end.mon<-month.key[match(substr(retirement.ages[,'End.DoB.range'],4,6),
                         month.key[,1]),2]
retirement.day<-substr(retirement.ages[,'Date.of.retirement'],1,2)
retirement.month<-month.key[match(substr(retirement.ages[,'Date.of.retirement'],4,6),
                                  month.key[,1]),2]
retirement.year<-2000+
  as.numeric(substr(retirement.ages[,'Date.of.retirement'],8,9))
retirement.age.yr<-substr(retirement.ages[,'Age.of.retirement'],1,2)
retirement.age.month<-substr(retirement.ages[,'Age.of.retirement'],
                             14,max(14,nchar(retirement.ages[,'Age.of.retirement'])-7))

retirement.ages<-cbind(retirement.ages,
                       start.yr,
                       start.mon,
                       end.yr,
                       end.mon,
                       retirement.day,
                       retirement.month,
                       retirement.year,
                       retirement.age.yr,
                       retirement.age.month)

#key variables
##########################
#age: dhager
#PIP: pip_r_i  pip_p_i iahdnpi
#DLA: dla_r_i  dla_p_i iahdndl
#sex
#education
#year of birth: didbny
#########################
#variable lists
sex<-c('dhsex','DhSex','dhsex','dhsex','dhsex','DhSex','DhSex','indsex','indsex','indsex')
health<-c('hegenh','Hehelf','hegenh','hehelf','hehelf','Hehelf','Hehelf','hehelf','hehelf','hehelf')
age<-c('dhager','dhager','dhager','heage','heage','indager','indager','indager','indager','indager')
yob<-c('indobyr','indobyr','indobyr','indobyr','indobyr','Indobyr','Indobyr','indobyr',
       'indobyr','indobyr')
#mob<-c('indobm','indobm','indobm','indobm','indobm','Indobm','Indobm','indobm',
#       'indobm')
non.housing.wealth<-c(
  'nettotnhw_bu_s','nettotnhw_bu_s','nettotnhw_bu_s','nettotnhw_bu_s','nettotnhw_bu_s',
  'nettotnhw_bu_s','nettotnhw_bu_s','nettotnhw_bu_s','nettotnhw_bu_s','nettotnhw_bu_s'
)
housing.wealth<-c(
  'nethw_bu_s','nethw_bu_s','nethw_bu_s','nethw_bu_s','nethw_bu_s',
  'nethw_bu_s','nethw_bu_s','nethw_bu_s','nethw_bu_s','nethw_bu_s'
)
dla.r<-c('dla_r_i','dla_r_i','dla_r_i','dla_r_i','dla_r_i',
         'dla_r_i','dla_r_i','dla_r_i','dla_r_i','dla_r_i')
dla.p<-c('dla_p_i','dla_p_i','dla_p_i','dla_p_i','dla_p_i',
         'dla_p_i','dla_p_i','dla_p_i','dla_p_i','dla_p_i')
pip.r<-c(NA,NA,NA,NA,NA,NA,'pip_r_i','pip_r_i','pip_r_i','pip_r_i')
pip.p<-c(NA,NA,NA,NA,NA,NA,'pip_p_i','pip_p_i','pip_p_i','pip_p_i')
aa.r<-c('attall_r_i','attall_r_i','attall_r_i','attall_r_i',
'attall_r_i','attall_r_i','attall_r_i','attall_r_i',
'attall_r_i','attall_r_i')

year<-c('iintdty','iintdty','iintdaty','iintdaty','iintdaty',
        'iintdaty','iintdaty','iintdaty','iintdaty','iintdaty')
education<-c('topqual3','topqual3','topqual3','topqual3','topqual3',
             'topqual3','topqual3','topqual3','topqual3','topqual3')
month<-c('iintdtm','iintdtm','iintdatm','iintdatm','iintdatm',
         'iintdatm','iintdatm','iintdatm','iintdatm','iintdatm')
scghqa<-c('scghqa','scghqa','scghqa','scghqa','scghqa',
          'scghqa','scghqa','scghqa','scghqa','scghqa')
scghqb<-c('scghqb','scghqb','scghqb','scghqb','scghqb',
          'scghqb','scghqb','scghqb','scghqb','scghqb')
scghqc<-c('scghqc','scghqc','scghqc','scghqc','scghqc',
          'scghqc','scghqc','scghqc','scghqc','scghqc')
scghqd<-c('scghqd','scghqd','scghqd','scghqd','scghqd',
          'scghqd','scghqd','scghqd','scghqd','scghqd')
scghqe<-c('scghqe','scghqe','scghqe','scghqe','scghqe',
          'scghqe','scghqe','scghqe','scghqe','scghqe')
scghqf<-c('scghqf','scghqf','scghqf','scghqf','scghqf',
          'scghqf','scghqf','scghqf','scghqf','scghqf')
scghqg<-c('scghqg','scghqg','scghqg','scghqg','scghqg',
          'scghqg','scghqg','scghqg','scghqg','scghqg')
scghqh<-c('scghqh','scghqh','scghqh','scghqh','scghqh',
          'scghqh','scghqh','scghqh','scghqh','scghqh')
scghqi<-c('scghqi','scghqi','scghqi','scghqi','scghqi',
          'scghqi','scghqi','scghqi','scghqi','scghqi')
scghqj<-c('scghqj','scghqj','scghqj','scghqj','scghqj',
          'scghqj','scghqj','scghqj','scghqj','scghqj')
scghqk<-c('scghqk','scghqk','scghqk','scghqk','scghqk',
          'scghqk','scghqk','scghqk','scghqk','scghqk')
scghql<-c('scghql','scghql','scghql','scghql','scghql',
          'scghql','scghql','scghql','scghql','scghql')
ced1<-c('psceda','PScedA','psceda','psceda','psceda','PScedA','PScedA',
        'psceda','psceda','psceda')
ced2<-c('pscedb','PScedB','pscedb','pscedb','pscedb','PScedB',
        'PScedB','pscedb','pscedb','pscedb')
ced3<-c('pscedc','PScedC','pscedc','pscedc','pscedc',
        'PScedC','PScedC','pscedc','pscedc','pscedc')
ced4<-c('pscedd','PScedD','pscedd','pscedd','pscedd',
        'PScedD','PScedD','pscedd','pscedd','pscedd')
ced5<-c('pscede','PScedE','pscede','pscede','pscede',
        'PScedE','PScedE','pscede','pscede','pscede')
ced6<-c('pscedf','PScedF','pscedf','pscedf','pscedf',
        'PScedF','PScedF','pscedf','pscedf','pscedf')
ced7<-c('pscedg','PScedG','pscedg','pscedg','pscedg',
        'PScedG','PScedG','pscedg','pscedg','pscedg')
ced8<-c('pscedh','PScedH','pscedh','pscedh','pscedh',
        'PScedH','PScedH','pscedh','pscedh','pscedh')

outcome.vars<-c(NA,'w2indout','w3indout','finstat','w5indout',
               'w6indout','w7indout','w8indout','w9indout')

lm.status<-c('wpdes','wpdes','wpdes','wpdes','wpdes',
           'WpDes','WpDes','wpdes','wpdes','wpdes')

mycesd<-ifelse(wave_2_core_data[,ced1[2]]<0|
         wave_2_core_data[,ced2[2]]<0|
         wave_2_core_data[,ced3[2]]<0|
         wave_2_core_data[,ced4[2]]<0|
         wave_2_core_data[,ced5[2]]<0|
         wave_2_core_data[,ced6[2]]<0|
         wave_2_core_data[,ced7[2]]<0|
         wave_2_core_data[,ced8[2]]<0,NA,
        as.numeric(wave_2_core_data[,ced1[2]]==1)+
        as.numeric(wave_2_core_data[,ced2[2]]==1)+
        as.numeric(wave_2_core_data[,ced3[2]]==1)+
        as.numeric(wave_2_core_data[,ced4[2]]==2)+
        as.numeric(wave_2_core_data[,ced5[2]]==1)+
        as.numeric(wave_2_core_data[,ced6[2]]==2)+
        as.numeric(wave_2_core_data[,ced7[2]]==1)+
        as.numeric(wave_2_core_data[,ced8[2]]==1))
data.cesd<-ifelse(wave_2_core_data[,'totpsc']<0,NA,
                  wave_2_core_data[,'totpsc'])

#Function for extracting variables
fun_extract_vars<-function(core_data,fin_data,pip_period,df.no){
  
  pip.var<-if(pip_period==0){
    matrix(NA,nrow=nrow(core_data),ncol=1)
  }else{
    fin_data[match(core_data[,'idauniq'],fin_data[,'idauniq']),pip.r[df.no]]
  }
  
  educ.var<-ifelse(is.na(wave_0_1998_data[match(core_data[,'idauniq'],
                                                wave_0_1998_data[,'idauniq']),
                                          education[df.no]]),
                   ifelse(is.na(wave_0_1999_data[match(core_data[,'idauniq'],
                                                       wave_0_1999_data[,'idauniq']),education[df.no]]),
                          wave_0_2001_data[match(core_data[,'idauniq'],
                                                 wave_0_2001_data[,'idauniq']),education[df.no]],
                          wave_0_1999_data[match(core_data[,'idauniq'],wave_0_1999_data[,'idauniq']),
                                           education[df.no]]),              
                   wave_0_1998_data[match(core_data[,'idauniq'],
                                          wave_0_1998_data[,'idauniq']),education[df.no]])
  
  df<-as.data.frame(cbind(core_data[,'idauniq'],
                          core_data[,sex[df.no]],
                          core_data[,health[df.no]],
                          core_data[,age[df.no]],
                          core_data[,yob[df.no]],
                          #                  core_data[,mob[df.no]],
                          core_data[,ced1[df.no]],
                          core_data[,ced2[df.no]],
                          core_data[,ced3[df.no]],
                          core_data[,ced4[df.no]],
                          core_data[,ced5[df.no]],
                          core_data[,ced6[df.no]],
                          core_data[,ced7[df.no]],
                          core_data[,ced8[df.no]],
                          fin_data[match(core_data[,'idauniq'],fin_data[,'idauniq']),non.housing.wealth[df.no]]+
                          fin_data[match(core_data[,'idauniq'],fin_data[,'idauniq']),housing.wealth[df.no]],
                          fin_data[match(core_data[,'idauniq'],fin_data[,'idauniq']),dla.r[df.no]],
                          fin_data[match(core_data[,'idauniq'],fin_data[,'idauniq']),aa.r[df.no]],
                          pip.var,core_data[,year[df.no]],
                          core_data[,month[df.no]],
                          educ.var,
                          core_data[,lm.status[df.no]],
                          df.no
  ))
  colnames(df)<-c('idauniq','sex','health','age','yob',
                  #'mob',
                  'CESD1','CESD2','CESD3','CESD4','CESD5','CESD6',
                  'CESD7','CESD8',
                  'assets','dla','attendance_allowance','pip','year','month','education',
                  'work_status','wave_number')
  
  return(df)
}

#lists of core and financial datasets
core.data<-list(wave_1_core_data,wave_2_core_data,wave_3_core_data,
                wave_4_core_data,wave_5_core_data,wave_6_core_data,
                wave_7_core_data,wave_8_core_data,wave_9_core_data,
                wave_10_core_data)
fin.data<-list(wave_1_fin_data,wave_2_fin_data,wave_3_fin_data,
               wave_4_fin_data,wave_5_fin_data,wave_6_fin_data,
               wave_7_fin_data,wave_8_fin_data,wave_9_fin_data,
               wave_10_fin_data)

#for each data frame, extract the variables and name the data frame
for (val in seq(from=1,to=10,by=1)){
  this_df<-as.data.frame(fun_extract_vars(
                as.data.frame(core.data[val]),
                as.data.frame(fin.data[val]),
                as.numeric(val>6),
                val))
  assign(paste('df_',val,sep=''),this_df)
}

df_full<-rbind(df_1,
               df_2,
               df_3,
               df_4,
               df_5,
               df_6,
               df_7,
               df_8,
               df_9,
               df_10)

#setwd('C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\Data')
cpi.data<-read.csv('CPI_index.CSV')
cpi.index<-as.data.frame(
  cpi.data[match(df_full[,'year'],cpi.data[,1]),2])
colnames(cpi.index)<-'CPI_index'
df_full<-cbind(df_full,cpi.index)

#convert financial variables to real numbers
for (var in c('dla','pip','attendance_allowance')){
  df_full[,var]<-df_full[,var]*100/
    df_full[,'CPI_index']
}

######################
#Adding attrition and comparing to deaths
setwd(data_directory)
death.data<-as.data.frame(read_dta('index_file_wave_0-wave_5_v2.DTA'))
df<-death.data[,c('outindw1','outindw2','outindw3','outindw4','outindw5')]
#for each wave, calculate the share of attritors who die
length(subset(df,outindw1==79|outindw1==99)[,'outindw1'])/
length(subset(df,outindw1>23)[,'outindw1'])
length(subset(df,outindw2==90|outindw2==99)[,'outindw2'])/
  length(subset(df,outindw2>23)[,'outindw2'])
length(subset(df,outindw3==95|outindw3==99)[,'outindw3'])/
  length(subset(df,outindw3>25)[,'outindw3'])
length(subset(df,outindw4==95)[,'outindw4'])/
  length(subset(df,outindw4>25)[,'outindw4'])
length(subset(df,outindw5==95)[,'outindw5'])/
  length(subset(df,outindw5>25)[,'outindw5'])
#Create an 'attrition' variable
attrit.next.wave<-as.data.frame(
                    ifelse(is.na(
                          match(interaction(df_full[,'idauniq'],
                                           df_full[,'wave_number']+1),
                         interaction(df_full[,'idauniq'],
                                           df_full[,'wave_number']))),
                         1,0))
colnames(attrit.next.wave)<-'attrit_next_wave'
df_full<-cbind(df_full,attrit.next.wave)

######################
#Data cleaning
#Set DLA and PIP NAs to zero
#df_full[,'dla']<-ifelse(is.na(df_full[,'dla']),0,df_full[,'dla'])
#df_full[,'pip']<-ifelse(is.na(df_full[,'pip']),0,df_full[,'pip'])
df_full[,'pip']<-ifelse(df_full[,'wave_number']<7,0,df_full[,'pip'])

#create some dependent variables
dla.pos<-as.data.frame(df_full[,'dla']>0)
pip.pos<-as.data.frame(df_full[,'pip']>0)
dla.or.pip.pos<-as.data.frame(df_full[,'pip']>0|df_full[,'dla']>0)
dla.pip.payments<-as.data.frame(df_full[,'pip']+df_full[,'dla'])
benefits.payments<-as.data.frame(df_full[,'pip']+df_full[,'dla']+df_full[,'attendance_allowance'])
cesd<-as.data.frame(
  ifelse(df_full[,'CESD1']<0|
               df_full[,'CESD2']<0|
               df_full[,'CESD3']<0|
               df_full[,'CESD4']<0|
               df_full[,'CESD5']<0|
               df_full[,'CESD6']<0|
               df_full[,'CESD7']<0|
               df_full[,'CESD8']<0,NA,
                     as.numeric(df_full[,'CESD1']==1)+
                       as.numeric(df_full[,'CESD2']==1)+
                       as.numeric(df_full[,'CESD3']==1)+
                       as.numeric(df_full[,'CESD4']==2)+
                       as.numeric(df_full[,'CESD5']==1)+
                       as.numeric(df_full[,'CESD6']==2)+
                       as.numeric(df_full[,'CESD7']==1)+
                       as.numeric(df_full[,'CESD8']==1)))
good.health<-as.data.frame(
  ifelse(df_full[,'health']>0&df_full[,'health']<4,1,
                    ifelse(df_full[,'health']>3,0,NA)))
for (val in seq(from=1,to=8,by=1)){
  df_full[,paste('CESD',val,sep='')]<-
    ifelse(df_full[,paste('CESD',val,sep='')]<0,NA,
           df_full[,paste('CESD',val,sep='')])
}
table(good.health)

retired<-as.data.frame(as.numeric(df_full[,'work_status']==1))
working<-as.data.frame(as.numeric(df_full[,'work_status']==2))
colnames(retired)<-'retired'
colnames(working)<-'working'
colnames(dla.pos)<-'dla_positive'
colnames(pip.pos)<-'pip_positive'
colnames(dla.or.pip.pos)<-'dla_or_pip_positive'
colnames(dla.pip.payments)<-'total_dla_pip_payments'
colnames(benefits.payments)<-'total_benefit_payments'
colnames(good.health)<-'good_health'
colnames(cesd)<-'CESD'
df_full<-cbind(df_full,dla.pos,pip.pos,dla.or.pip.pos,dla.pip.payments,
               benefits.payments,
               good.health,cesd,working,retired)
ylist<-c('dla_positive','pip_positive',
         'total_dla_pip_payments',
         'dla_or_pip_positive','total_benefit_payments',
         'good_health','CESD','attrit_next_wave',
         'CESD1','CESD2','CESD3','CESD4','CESD5','CESD6',
         'CESD7','CESD8','retired','working','sex')
#Create 'year pair' variable
year.pair<-as.data.frame(2*floor(df_full[,'year']/2))
colnames(year.pair)<-'year_pair'
df_full<-cbind(df_full,year.pair)

#Now I'm going to 'make up' a randomized month of birth variable
#mob<-cbind(unique(df_full[,'idauniq']), round(runif(length(unique(df_full[,'idauniq'])),0.5,12.5)))

#Attempt to get dobmonth variable in
mobdata <-subset(wave_8_prim_data,,select=c('idauniq','dobmonth'))
colnames(mobdata)[1]<-'idauniq'
colnames(mobdata)[2]<-'mob'
df_full<-merge (df_full,mobdata)

#mobdata <- as.data.frame(mob)
#colnames(mobdata)[1]<-'idauniq'
#colnames(mobdata)[2]<-'mob'
#df_full<-merge (df_full,mobdata)

my.age<-as.data.frame(
  ifelse(df_full[,'year']>0&df_full[,'yob']>0&
           df_full[,'month']>0&df_full[,'mob']>0,
         floor((12*(df_full[,'year']-df_full[,'yob'])+
                  (df_full[,'month']-df_full[,'mob']))/12),
         NA))

#issue when month of survey is month of birth
my.age[,1]<-ifelse(df_full[,'mob']==df_full[,'month']&abs(my.age[,1]-df_full[,'age'])<=1,
df_full[,'age'],
my.age[,1])

colnames(my.age)<-'my_age'
df_full<-cbind(df_full,my.age)

df_full[,'my_age']<-ifelse(is.na(df_full[,'my_age']),
                           df_full[,'age'],
                           df_full[,'my_age'])

#Construct running variable
run.var<-as.data.frame(
  (12*df_full[,'yob']+df_full[,'mob']+0.5)-
    ((8/30)+4+12*1948))
colnames(run.var)<-'running_variable'
df_full<-cbind(df_full,run.var)
df_full<-subset(df_full,is.na(running_variable)==FALSE)

can_retire<-function(df){
  
  match.to.start.dob.range<-
    match(as.character(interaction(df[,'mob'],df[,'yob'])),
          as.character(interaction(retirement.ages[,'start.mon'],
                                   retirement.ages[,'start.yr'])))
  
  match.to.end.dob.range<-
    match(as.character(interaction(df[,'mob'],df[,'yob'])),
          as.character(interaction(retirement.ages[,'end.mon'],
                                   retirement.ages[,'end.yr'])))
  
  #Using the dates where the retirement month and year are given
  min.retirement.month<-retirement.ages[match.to.end.dob.range,'retirement.month']
  min.retirement.yr<-retirement.ages[match.to.end.dob.range,'retirement.year']
  
  max.retirement.month<-retirement.ages[match.to.start.dob.range,'retirement.month']
  max.retirement.yr<-retirement.ages[match.to.start.dob.range,'retirement.year']
  
  #Using the dates where the retirement age is given
  min.retirement.month.age<-ifelse(
    as.numeric(retirement.ages[match.to.end.dob.range,'retirement.age.month'])+
      df[,'mob']>12,
    as.numeric(retirement.ages[match.to.end.dob.range,'retirement.age.month'])+
      df[,'mob']-12,
    as.numeric(retirement.ages[match.to.end.dob.range,'retirement.age.month'])+
      df[,'mob'])
  
  min.retirement.yr.age<-ifelse(
    as.numeric(retirement.ages[match.to.end.dob.range,'retirement.age.month'])+
      df[,'mob']>12,
    as.numeric(retirement.ages[match.to.end.dob.range,'retirement.age.yr'])+
      df[,'yob']+1,
    as.numeric(retirement.ages[match.to.end.dob.range,'retirement.age.yr'])+
      df[,'yob'])
  
  max.retirement.month.age<-ifelse(
    as.numeric(retirement.ages[match.to.start.dob.range,'retirement.age.month'])+
      df[,'mob']>12,
    as.numeric(retirement.ages[match.to.start.dob.range,'retirement.age.month'])+
      df[,'mob']-12,
    as.numeric(retirement.ages[match.to.start.dob.range,'retirement.age.month'])+
      df[,'mob'])
  
  max.retirement.yr.age<-
    ifelse(
      as.numeric(retirement.ages[match.to.start.dob.range,'retirement.age.month'])+
        df[,'mob']>12,
      as.numeric(retirement.ages[match.to.start.dob.range,'retirement.age.yr'])+
        df[,'yob']+1,
      as.numeric(retirement.ages[match.to.start.dob.range,'retirement.age.yr'])+
        df[,'yob'])
  
  max.retirement.month<-as.numeric(
    ifelse(is.na(max.retirement.month),
           max.retirement.month.age,
           max.retirement.month))
  max.retirement.yr<-as.numeric(
    ifelse(is.na(max.retirement.yr),
           max.retirement.yr.age,
           max.retirement.yr))
  min.retirement.month<-as.numeric(
    ifelse(is.na(min.retirement.month),
           min.retirement.month.age,
           min.retirement.month))
  min.retirement.yr<-as.numeric(
    ifelse(is.na(min.retirement.yr),
           min.retirement.yr.age,
           min.retirement.yr))
  
  definitely.retired<-(df[,'month']+12*df[,'year'])>
    max.retirement.month+max.retirement.yr*12
  definitely.not.retired<-(df[,'month']+12*df[,'year'])<
    min.retirement.month+min.retirement.yr*12
  
  #women under 60 are all definitely not retired
  definitely.retired<-ifelse(
    df[,'my_age']<60&df[,'sex']==2,0,definitely.retired
  )
  definitely.not.retired<-ifelse(
    df[,'my_age']<60&df[,'sex']==2,1,definitely.not.retired
  )
  #men under 65 are all definitely not retired
  definitely.retired<-ifelse(
    df[,'my_age']<65&df[,'sex']==1,0,definitely.retired
  )
  definitely.not.retired<-ifelse(
    df[,'my_age']<65&df[,'sex']==1,1,definitely.not.retired
  )
  
  #If you're born after October 1954 and before April 1960,
  #you're definitely retired if you're over 66 and
  #definitely not retired if you're under 66
  definitely.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>10+12*1954)&
      ((df[,'mob']+12*df[,'yob'])<4+12*1960)&df[,'my_age']<66,
    0,definitely.retired
  )
  definitely.not.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>10+12*1954)&
      ((df[,'mob']+12*df[,'yob'])<4+12*1960)&df[,'my_age']<66,
    1,definitely.not.retired
  )
  definitely.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>10+12*1954)&
      ((df[,'mob']+12*df[,'yob'])<4+12*1960)&df[,'my_age']>=66,
    1,definitely.retired
  )
  definitely.not.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>10+12*1954)&
      ((df[,'mob']+12*df[,'yob'])<4+12*1960)&df[,'my_age']>=66,
    0,definitely.not.retired
  )
  
  #If you're born after April 1951, and you're older than
  #67, you're definitely retired
  definitely.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>4+12*1961)&df[,'my_age']<67,
    0,definitely.retired
  )
  definitely.not.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>4+12*1961)&df[,'my_age']<67,
    1,definitely.not.retired
  )
  definitely.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>4+12*1961)&df[,'my_age']>=67,
    1,definitely.retired
  )
  definitely.not.retired<-ifelse(
    ((df[,'mob']+12*df[,'yob'])>4+12*1961)&df[,'my_age']>=67,
    0,definitely.not.retired
  )
  
  #If you're older than 67, you're definitely retired
  definitely.retired<-ifelse(df[,'my_age']>66,
                             1,definitely.retired
  )
  
  #If you're older than 60, born before April 1950,
  #and a woman, you're definitely retired
  definitely.retired<-ifelse(
    df[,'my_age']>=60&df[,'sex']==2&
      ((df[,'mob']+12*df[,'yob'])<4+1950*12),
    1,definitely.retired
  )
  #If you're younger than 60, you're definitely not retired
  definitely.not.retired<-ifelse(
    df[,'my_age']<60&
      ((df[,'mob']+12*df[,'yob'])<4+1950*12),
    1,definitely.not.retired
  )
  #If you're older than 65, born before December 1953,
  #and a man, you're definitely retired
  definitely.retired<-ifelse(
    df[,'my_age']>=65&df[,'sex']==1&
      ((df[,'mob']+12*df[,'yob'])<12+1953*12),
    1,definitely.retired
  )
  #If you're younger than 65 and a man, you're definitely not retired
  definitely.not.retired<-ifelse(
    df[,'my_age']<65&df[,'sex']==1,
    1,definitely.not.retired
  )
  
  return(cbind(definitely.retired,definitely.not.retired,
               min.retirement.month+min.retirement.yr*12,
               max.retirement.month+max.retirement.yr*12))
  
}

df_full<-cbind(df_full,can_retire(df_full))
colnames(df_full)[(ncol(df_full)-1):ncol(df_full)]<-
  c('earliest_retirement_month','latest_retirement_month')

#If you are a woman born before april 1950, your earliest retiement month
#and latest retirement month are in the month of your 60th birthday
df_full[,'earliest_retirement_month']<-ifelse(
  df_full[,'sex']==2&(df_full[,'yob']<1950|(df_full[,'yob']==1950&
                                              df_full[,'mob']<4)),
  12*(df_full[,'yob']+60)+df_full[,'mob'],
  df_full[,'earliest_retirement_month']
)
df_full[,'latest_retirement_month']<-ifelse(
  df_full[,'sex']==2&(df_full[,'yob']<1950|(df_full[,'yob']==1950&
                                              df_full[,'mob']<4)),
  12*(df_full[,'yob']+60)+df_full[,'mob'],
  df_full[,'latest_retirement_month']
)

#If you are a man born before December 1953, your earliest retiement month
#and latest retirement month are in the month of your 65th birthday
df_full[,'earliest_retirement_month']<-ifelse(
  df_full[,'sex']==1&(df_full[,'yob']<1953|(df_full[,'yob']==1953&
                                              df_full[,'mob']<12)),
  12*(df_full[,'yob']+65)+df_full[,'mob'],
  df_full[,'earliest_retirement_month']
)
df_full[,'latest_retirement_month']<-ifelse(
  df_full[,'sex']==1&(df_full[,'yob']<1953|(df_full[,'yob']==1953&
                                              df_full[,'mob']<12)),
  12*(df_full[,'yob']+65)+df_full[,'mob'],
  df_full[,'latest_retirement_month']
)

#If you are born after April 1960, your earliest retiement month
#and latest retirement month are in the month of your 66th birthday
df_full[,'earliest_retirement_month']<-ifelse(
  (df_full[,'yob']>1960|(df_full[,'yob']==1960&
                                              df_full[,'mob']>4)),
  12*(df_full[,'yob']+66)+df_full[,'mob'],
  df_full[,'earliest_retirement_month']
)
df_full[,'latest_retirement_month']<-ifelse(
  (df_full[,'yob']>1960|(df_full[,'yob']==1960&
                                              df_full[,'mob']>4)),
  12*(df_full[,'yob']+66)+df_full[,'mob'],
  df_full[,'latest_retirement_month']
)

#What share of people have ambiguous retirement months?
amb.retirement.month<-lm(latest_retirement_month!=earliest_retirement_month~
     factor(yob),data=df_full)
nrow(subset(df_full,latest_retirement_month!=earliest_retirement_month))/
nrow(subset(df_full,latest_retirement_month>0))

mean.retirement.month<-as.data.frame((df_full[,'latest_retirement_month']+
  df_full[,'earliest_retirement_month'])/2)
colnames(mean.retirement.month)<-'mean_retirement_month'
df_full<-cbind(df_full,mean.retirement.month)

time.to.retirement<-as.data.frame(df_full[,'mean_retirement_month']-
                                    12*df_full[,'year']-
                                    df_full[,'month'])
colnames(time.to.retirement)<-'time_to_retirement'
df_full<-cbind(df_full,time.to.retirement)
                                    
plot(density(subset(df_full,time_to_retirement>-5000)[,'time_to_retirement']))

png('retirement_rds.png',width=400,height=200)
par(mar=c(4,2,2,2),mfrow=c(1,2))
rdplot(y=subset(df_full,time_to_retirement>-5000&time_to_retirement!=0)[,'CESD'],
       x=subset(df_full,time_to_retirement>-5000&time_to_retirement!=0)[,'time_to_retirement'],
       p=1,kernel='triangular',h=60,title='CESD')
rdplot(y=subset(df_full,time_to_retirement>-5000&time_to_retirement!=0)[,'good_health'],
       x=subset(df_full,time_to_retirement>-5000&time_to_retirement!=0)[,'time_to_retirement'],
       p=1,kernel='triangular',h=60,title='Good Health')
dev.off()

#There should be no people definitely retired and definitely not retired
subset(df_full,definitely.retired==1&definitely.not.retired==1)
#df_full[,'definitely.retired']<-
 # ifelse(df_full[,'definitely.not.retired']==1,
  #       0,df_full[,'definitely.retired'])
#df_full[,'definitely.not.retired']<-
 # ifelse(df_full[,'definitely.retired']==1,
  #       0,df_full[,'definitely.not.retired'])

table(subset(df_full,is.na(definitely.retired)&is.na(definitely.not.retired))[,'my_age'])
table(subset(df_full,is.na(definitely.retired))[,'yob'])
table(subset(df_full,is.na(definitely.not.retired))[,'yob'])

#We have na definitely not retired for 60+ - think about this

#plot definitely retired, not retired, ambiguous against age. Men
ret.by.age<-lm(definitely.retired~factor(my_age),data=subset(df_full,sex==1))
not.ret.by.age<-lm(definitely.not.retired~factor(my_age),data=subset(df_full,sex==1))
amb.by.age<-lm((definitely.retired==0)&(definitely.not.retired==0)~
                 factor(my_age),data=subset(df_full,sex==1))

par(mar=c(4,2,2,2))
plot(seq(from=60,to=68,by=1),
     coef(ret.by.age)[1]+
       coef(ret.by.age)[paste('factor(my_age)',seq(from=60,to=68,by=1),sep='')],
     ylim=c(0,1),type='l',ylab=' ',xlab='age')
lines(seq(from=60,to=68,by=1),
      coef(not.ret.by.age)[1]+
        coef(not.ret.by.age)[paste('factor(my_age)',seq(from=60,to=68,by=1),sep='')],
      col=2)
lines(seq(from=60,to=68,by=1),
      coef(amb.by.age)[1]+
        coef(amb.by.age)[paste('factor(my_age)',seq(from=60,to=68,by=1),sep='')],
      col=3)

#drop all obs before start of 2019 to check all ambiguous disappear? no, they don't
#They are always aged 65, though. Sort this
subset(df_full,definitely.retired==0&definitely.not.retired==0&year==2019)
table(subset(df_full,definitely.retired==0&definitely.not.retired==0)[,'year'])

#plot definitely retired, not retired, ambiguous against age. Women
ret.by.age<-lm(definitely.retired~factor(my_age),data=subset(df_full,sex==2))
not.ret.by.age<-lm(definitely.not.retired~factor(my_age),
                   data=subset(df_full,sex==2))
amb.by.age<-lm((definitely.retired==0)&(definitely.not.retired==0)~
                 factor(my_age),data=subset(df_full,sex==2))

par(mar=c(4,2,2,2))
plot(seq(from=50,to=68,by=1),
     coef(ret.by.age)[1]+
       coef(ret.by.age)[paste('factor(my_age)',seq(from=50,to=68,by=1),sep='')],
     ylim=c(0,1),type='l',ylab=' ',xlab='age')
lines(seq(from=50,to=68,by=1),
      coef(not.ret.by.age)[1]+
        coef(not.ret.by.age)[paste('factor(my_age)',seq(from=50,to=68,by=1),sep='')],
      col=2)
lines(seq(from=50,to=68,by=1),
      coef(amb.by.age)[1]+
        coef(amb.by.age)[paste('factor(my_age)',seq(from=50,to=68,by=1),sep='')],
      col=3)

#drop period before women retired 65 (year 2018?)

plot(seq(from=50,to=68,by=1),
     coef(amb.by.age)[1]+
       coef(amb.by.age)[paste('factor(my_age)',seq(from=50,to=68,by=1),sep='')],
     col=3,type='l')


#plot month of birth against max age
#plot month of birth against min age
dob.vs.ret.date<-
  cbind((subset(df_full,sex==2&yob>0&earliest_retirement_month>0)[,'yob']*12+
           subset(df_full,sex==2&yob>0&earliest_retirement_month>0)[,'mob'])/12,
        subset(df_full,sex==2&yob>0&earliest_retirement_month>0)[,'earliest_retirement_month']/12)
dob.vs.ret.date<-cbind(sort(dob.vs.ret.date[,1]),
                       dob.vs.ret.date[match(sort(dob.vs.ret.date[,1]),
                                             dob.vs.ret.date[,1]),2])
plot(dob.vs.ret.date[,1],dob.vs.ret.date[,2],type='l')
lines(dob.vs.ret.date[,1],dob.vs.ret.date[,1]+60)

#consistent with expectations about retirement age for women
dob.vs.ret.date<-
  cbind((subset(df_full,sex==2&yob>0&latest_retirement_month>0)[,'yob']*12+
           subset(df_full,sex==2&yob>0&latest_retirement_month>0)[,'mob'])/12,
        subset(df_full,sex==2&yob>0&latest_retirement_month>0)[,'latest_retirement_month']/12)
dob.vs.ret.date<-cbind(sort(dob.vs.ret.date[,1]),
                       dob.vs.ret.date[match(sort(dob.vs.ret.date[,1]),
                                             dob.vs.ret.date[,1]),2])

plot(dob.vs.ret.date[,1],dob.vs.ret.date[,2],type='l')
lines(dob.vs.ret.date[,1],dob.vs.ret.date[,1]+60)

drop_if_not_retired<-function(df){
  return(subset(df,definitely.retired==1))
}
drop_if_retired<-function(df){
  return(subset(df,definitely.not.retired==1))
}

drop_min_of_retired_and_not_retired<-function(df){
  
  n.retired<-nrow(drop_if_not_retired(df))
  n.not.retired<-nrow(drop_if_retired(df))
  
  if(n.retired>=n.not.retired){
    df1<-drop_if_not_retired(df)
  }
  if(n.not.retired>n.retired){
    df1<-drop_if_retired(df)
  }
  
  return(df1)
  
}

#list of people who claimed DLA before 2013
#dla.claimants<-unique(subset(df_full,dla>0&year<2013|(year==2014&month<4))[,'idauniq'])
#Alternative definition of DLA claimants: were you claiming DLA in the last wave we see you before 2013
last.wv<-aggregate(wave_number~idauniq,FUN=max,data=subset(df_full,year<2013|(year==2013&month<4)))
last.wv<-cbind(last.wv,df_full[match(interaction(last.wv[,'idauniq'],last.wv[,'wave_number']),
                                     interaction(df_full[,'idauniq'],df_full[,'wave_number'])),'dla'])
#last.wv<-aggregate(wave_number~idauniq,FUN=max,data=subset(df_full,year<2010|(year==2010&month<5)))
#last.wv<-cbind(last.wv,df_full[match(interaction(last.wv[,'idauniq'],last.wv[,'wave_number']),
#                     interaction(df_full[,'idauniq'],df_full[,'wave_number'])),'dla'])

colnames(last.wv)[3]<-'dla'
dla.claimants<-subset(last.wv,dla>0)[,1]
df.rds<-subset(df_full,idauniq%in%dla.claimants&yob>0&year>2011)
stargazer(df.rds,out='summary_statistics')

plot(density(subset(df.rds,is.na(time_to_retirement)==FALSE)[,'time_to_retirement']))

#plot people claiming dla by year
setwd(charts)
setEPS(width=8,height=(16/3))
postscript('share_of_past_dla_claimants_claiming_dla.eps')
par(mar=c(4,2,2,2),mfrow=c(1,1))
claim.by.year<-as.matrix(table(df.rds[,'dla']>0,df.rds[,'year']))
for (yr in seq(from=1,to=11,by=1)){
  claim.by.year[2,yr]<-claim.by.year[2,yr]/sum(claim.by.year[,yr])
}
plot(c(seq(from=2012,to=2019,by=1),seq(from=2021,to=2023,by=1)),
     claim.by.year[2,],type='l',ylim=c(0,1),
     xlab='year',ylab='P(claims DLA)')
claim.by.year<-as.matrix(table(subset(df.rds,yob<1948|(yob==1948&mob<4))[,'dla']>0,
                               subset(df.rds,yob<1948|(yob==1948&mob<4))[,'year']))
for (yr in seq(from=1,to=11,by=1)){
  claim.by.year[2,yr]<-claim.by.year[2,yr]/sum(claim.by.year[,yr])
}
lines(c(seq(from=2012,to=2019,by=1),seq(from=2021,to=2023,by=1)),
      claim.by.year[2,],type='l',col=2,lty=2)
claim.by.year<-as.matrix(table(subset(df.rds,yob>1948|(yob==1948&mob>4))[,'dla']>0,
                               subset(df.rds,yob>1948|(yob==1948&mob>4))[,'year']))
for (yr in seq(from=1,to=11,by=1)){
  claim.by.year[2,yr]<-claim.by.year[2,yr]/sum(claim.by.year[,yr])
}
lines(c(seq(from=2012,to=2019,by=1),seq(from=2021,to=2023,by=1)),
      claim.by.year[2,],type='l',col=3,lty=3)
legend('topright',col=c(1,2,3),lty=c(1,2,3),
       legend=c('all','born before cutoff','born after cutoff'))
dev.off()         

setEPS(width=8,height=(16/3))
postscript('share_of_past_dla_claimants_claiming_pip.eps')
par(mar=c(4,2,2,2),mfrow=c(1,1))
claim.by.year<-as.matrix(table(df.rds[,'pip']>0,df.rds[,'year']))
for (yr in seq(from=1,to=11,by=1)){
  claim.by.year[2,yr]<-claim.by.year[2,yr]/sum(claim.by.year[,yr])
}
plot(c(seq(from=2012,to=2019,by=1),seq(from=2021,to=2023,by=1)),
     claim.by.year[2,],type='l',ylim=c(0,1),
     xlab='year',ylab='P(claims PIP)')
claim.by.year<-as.matrix(table(subset(df.rds,yob<1948|(yob==1948&mob<4))[,'pip']>0,
                               subset(df.rds,yob<1948|(yob==1948&mob<4))[,'year']))
for (yr in seq(from=1,to=11,by=1)){
  claim.by.year[2,yr]<-claim.by.year[2,yr]/sum(claim.by.year[,yr])
}
lines(c(seq(from=2012,to=2019,by=1),seq(from=2021,to=2023,by=1)),
      claim.by.year[2,],type='l',col=2,lty=2)
claim.by.year<-as.matrix(table(subset(df.rds,yob>1948|(yob==1948&mob>4))[,'pip']>0,
                               subset(df.rds,yob>1948|(yob==1948&mob>4))[,'year']))
for (yr in seq(from=1,to=11,by=1)){
  claim.by.year[2,yr]<-claim.by.year[2,yr]/sum(claim.by.year[,yr])
}
lines(c(seq(from=2012,to=2019,by=1),seq(from=2021,to=2023,by=1)),
      claim.by.year[2,],type='l',col=3,lty=3)
legend('topleft',col=c(1,2,3),lty=c(1,2,3),
       legend=c('all','born before cutoff','born after cutoff'))
dev.off()  

#Create main data sets
df.bl<-subset(df_full,idauniq%in%dla.claimants&yob>0&year<2011&
                year>0)
df.for.placebos<-df.bl
ever.claim<-unique(subset(df_full,dla>0)[,'idauniq'])
df.others<-subset(df_full,(idauniq%in%ever.claim==FALSE)&yob>0&year>2013&
                    year>0)
df.others<-subset(df.others,yob!=1948|mob!=4)
df.rds<-subset(df.rds,yob!=1948|mob!=4)
df.bl<-subset(df.bl,yob!=1948|mob!=4)

#Three different data sets
stargazer(df.bl,out='summary_statistics_pre_treatment')
stargazer(df.others,out='summary_statistics_untreated')
stargazer(df.rds,out='summary_statistics_treated_group')

df.affected.full.period<-subset(df_full,idauniq%in%dla.claimants&(yob!=1948|mob!=4))
df.unaffect.full.period<-subset(df_full,(idauniq%in%ever.claim==FALSE)&(yob!=1948|mob!=4))

#calculate optimal bandwidths for y. The object
#optbws stores the optimal bandwidths for each depedent variable
opt.bws<-matrix(nrow=length(ylist),ncol=1)

for (y in ylist){
opt.bw<-
  rdbwselect(y=as.numeric(df.rds[,y]),x=df.rds[,'running_variable'],all=TRUE)
opt.bws[match(y,ylist),1]<-opt.bw$bws[1,1]
}

rownames(opt.bws)<-ylist

stargazer(opt.bws,out='optimal_bandwidths')

#set a bandwidth of choice to be the median (why not?)
my.opt.bw<-median(opt.bws)

plot(density(subset(df_full,yob>0)[,'yob']))
setwd(charts)
setEPS(width=8,height=(16/3))
postscript('mccrary_test.eps')
par(mar=c(4,2,2,2),mfrow=c(1,1))
mccrary.test<-DCdensity(df.bl[,'running_variable'], bin = NULL, bw = my.opt.bw, verbose = TRUE,
                        plot = TRUE, ext.out = TRUE, htest = TRUE)
dev.off()

setEPS(width=8,height=(16/3))
postscript('mccrary_test_post_2018.eps')
par(mar=c(4,2,2,2),mfrow=c(1,1))
mccrary.test<-DCdensity(subset(df.rds,year>2018|(year==2018&month>=4))[,'running_variable'], 
            bin = NULL, bw = my.opt.bw, verbose = TRUE,
            plot = TRUE, ext.out = TRUE, htest = TRUE)
dev.off()
########################
#Running falsification tests
#nb: this does two types of falsification test
# 1: is there a discontinuity in dependent variables before 2013 in our sample?
# 2: is there a discontinuity in dependent variables in the treatment period in a sample of people who never claim DLA?

#create my rd robust, which allows for clustered ses
my_linear_rd_robust<-function(y,h,df){
  
  kernel.weights<-h-abs(subset(df,abs(running_variable)<h)[,'running_variable'])
  
  formula<-paste(y,'~running_variable+(running_variable>0)*running_variable',
                 sep='')
  
  my.reg<-lm_robust(subset(df,abs(running_variable)<h)[,y]~running_variable+(running_variable>0)*running_variable,
                    data=subset(df,abs(running_variable)<h),
                    weights=kernel.weights,
                    cluster=idauniq)
  
  return(c(coef(my.reg)[3],my.reg$std.error[3],my.reg$nobs))
  
}

my_linear_rd_robust_contr<-function(y,h,df){
  
  kernel.weights<-h-abs(subset(df,abs(running_variable)<h)[,'running_variable'])
  
  formula<-paste(y,'~running_variable+(running_variable>0)*running_variable',
                 sep='')
  
  my.reg<-lm_robust(subset(df,abs(running_variable)<h)[,y]~
                  running_variable+
                (running_variable>0)*running_variable+
                  time_to_retirement+
                  (time_to_retirement>0)*time_to_retirement,
                    data=subset(df,abs(running_variable)<h),
                    weights=kernel.weights,
                    cluster=idauniq)
  
  return(c(coef(my.reg)[3],my.reg$std.error[3],my.reg$nobs))
  
}

#p(being retired) by yob in the 
def.retired.by.yob<-lm((definitely.retired==1)~factor(yob),data=df.bl)
plot(seq(from=1914,to=1987,by=1),coef(def.retired.by.yob)[1]+coef(def.retired.by.yob)[paste('factor(yob)',seq(from=1914,to=1987,by=1),sep='')],type='l')
def.not.retired.by.yob<-lm((definitely.not.retired==1)~factor(yob),data=df.bl)
lines(seq(from=1938,to=1987,by=1),coef(def.not.retired.by.yob)[1]+coef(def.not.retired.by.yob)[paste('factor(yob)',seq(from=1938,to=1987,by=1),sep='')],type='l',
      col=2)

#Then, run:
#Test the discontinuities at baseline
ydummylist<-c(ylist,'sex','age')
falsification.tests.bl<-matrix(nrow=2,ncol=length(ydummylist))
colnames(falsification.tests.bl)<-ydummylist
for (y in ydummylist){
  
  rd.est<-my_linear_rd_robust(y,my.opt.bw,
        drop_min_of_retired_and_not_retired(subset(df.bl,abs(running_variable)<my.opt.bw)))
  
  falsification.tests.bl[1,match(y,ydummylist)]<-rd.est[1]
  falsification.tests.bl[2,match(y,ydummylist)]<-rd.est[2]
  
}

#Error in eval(e, x, parent.frame()) :  object 'definitely.retired' not found

rdplot(y=as.numeric(drop_min_of_retired_and_not_retired(subset(df.rds,abs(running_variable)<my.opt.bw&
                                                                 year>2012))[,'sex']),
       x=drop_min_of_retired_and_not_retired(subset(df.rds,abs(running_variable)<my.opt.bw&
                                                      year>2012))[,'running_variable'],
       nbins=60,title=str_replace_all(y,'_',' '),p=1,
       h=my.opt.bw)

falsification.tests.nt<-matrix(nrow=2,ncol=length(ylist))
colnames(falsification.tests.nt)<-ylist
for (y in ydummylist){
  
  rd.est<-my_linear_rd_robust(y,my.opt.bw,drop_min_of_retired_and_not_retired(subset(df.others,abs(running_variable)<my.opt.bw)))
  
  falsification.tests.nt[1,match(y,ylist)]<-rd.est[1]
  falsification.tests.nt[2,match(y,ylist)]<-rd.est[2]
  
}

falsification.tests.pre.2008<-matrix(nrow=2,ncol=length(ylist))
colnames(falsification.tests.pre.2008)<-ylist
for (y in ydummylist){
  
  rd.est<-my_linear_rd_robust(y,my.opt.bw,
      drop_min_of_retired_and_not_retired(
    subset(df.bl,abs(running_variable)<my.opt.bw&
             year<2008&sex==1)))
  
  falsification.tests.pre.2008[1,match(y,ylist)]<-rd.est[1]
  falsification.tests.pre.2008[2,match(y,ylist)]<-rd.est[2]
  
}

stargazer(falsification.tests.pre.2008,out='falsification_men_pre_2008')

falsification.tests.pre.2003<-matrix(nrow=2,ncol=length(ylist))
colnames(falsification.tests.pre.2003)<-ylist
for (y in ydummylist){
  
  rd.est<-my_linear_rd_robust(y,my.opt.bw,
                              drop_min_of_retired_and_not_retired(
                                subset(df.bl,abs(running_variable)<my.opt.bw&
                                         year<2003)))
  
  falsification.tests.pre.2003[1,match(y,ylist)]<-rd.est[1]
  falsification.tests.pre.2003[2,match(y,ylist)]<-rd.est[2]
  
}

stargazer(falsification.tests.pre.2003,out='falsification_women_pre_2003')

falsification.tests.with.controls<-matrix(nrow=2,ncol=length(ylist))
colnames(falsification.tests.with.controls)<-ylist
for (y in ydummylist){
  
  rd.est<-my_linear_rd_robust_contr(y,my.opt.bw,
                                    subset(df.bl,
                                           year<2008&abs(running_variable)<my.opt.bw))
    
  falsification.tests.with.controls[1,match(y,ylist)]<-rd.est[1]
  falsification.tests.with.controls[2,match(y,ylist)]<-rd.est[2]
  
}

stargazer(falsification.tests.with.controls,out='falsification_with_controls_bl')

falsification.tests.with.controls.nt<-matrix(nrow=2,ncol=length(ylist))
colnames(falsification.tests.with.controls.nt)<-ylist
for (y in ydummylist){
  
  rd.est<-my_linear_rd_robust_contr(y,my.opt.bw,
                                    subset(df.unaffect.full.period,
                                           abs(running_variable)<my.opt.bw))
  
  falsification.tests.with.controls.nt[1,match(y,ylist)]<-rd.est[1]
  falsification.tests.with.controls.nt[2,match(y,ylist)]<-rd.est[2]
  
}

stargazer(falsification.tests.with.controls.nt,out='falsification_with_controls')


bl.estimates<-matrix(ncol=length(ylist),
                     nrow=3)
colnames(bl.estimates)<-ylist
rownames(bl.estimates)<-c('estimate','standard error','obs')
bl.estimates.men<-bl.estimates
bl.estimates.women<-bl.estimates
for (y in ylist){

  #All people
  rd.est<-my_linear_rd_robust(y,my.opt.bw,
          drop_min_of_retired_and_not_retired(subset(df.rds,
                year>2012&abs(running_variable)<my.opt.bw)))
  
  
  bl.estimates[1,match(y,ylist)]<-rd.est[1]
  bl.estimates[2,match(y,ylist)]<-rd.est[2]
  bl.estimates[3,match(y,ylist)]<-rd.est[3]
  
  #Men
  rd.est<-my_linear_rd_robust(y,my.opt.bw,
                  drop_min_of_retired_and_not_retired(subset(df.rds,
                 sex==1&year>2012&abs(running_variable)<my.opt.bw)))
  
  
  bl.estimates.men[1,match(y,ylist)]<-rd.est[1]
  bl.estimates.men[2,match(y,ylist)]<-rd.est[2]
  bl.estimates.men[3,match(y,ylist)]<-rd.est[3]
  
  #All people
  rd.est<-my_linear_rd_robust(y,my.opt.bw,
                              drop_min_of_retired_and_not_retired(subset(df.rds,
                             sex==2&year>2012&abs(running_variable)<my.opt.bw)))
  
  
  bl.estimates.women[1,match(y,ylist)]<-rd.est[1]
  bl.estimates.women[2,match(y,ylist)]<-rd.est[2]
  bl.estimates.women[3,match(y,ylist)]<-rd.est[3]
  
  
}  

bl.estimates.contr<-matrix(ncol=length(ylist),
                     nrow=3)
colnames(bl.estimates.contr)<-ylist
rownames(bl.estimates.contr)<-c('estimate','standard error','obs')
bl.estimates.contr.men<-bl.estimates.contr
bl.estimates.contr.women<-bl.estimates.contr
for (y in ylist){
  
  #All people
  rd.est<-my_linear_rd_robust_contr(y,my.opt.bw,
                              subset(df.rds,
                      year>2012&abs(running_variable)<my.opt.bw))
  
  
  bl.estimates.contr[1,match(y,ylist)]<-rd.est[1]
  bl.estimates.contr[2,match(y,ylist)]<-rd.est[2]
  bl.estimates.contr[3,match(y,ylist)]<-rd.est[3]
  
  #Men
  rd.est<-my_linear_rd_robust_contr(y,my.opt.bw,
                  subset(df.rds,
                 sex==1&year>2012&abs(running_variable)<my.opt.bw))
  
  
  bl.estimates.contr.men[1,match(y,ylist)]<-rd.est[1]
  bl.estimates.contr.men[2,match(y,ylist)]<-rd.est[2]
  bl.estimates.contr.men[3,match(y,ylist)]<-rd.est[3]
  
  #Women
  rd.est<-my_linear_rd_robust_contr(y,my.opt.bw,
                                    subset(df.rds,
              sex==2&year>2012&abs(running_variable)<my.opt.bw))
  
  
  bl.estimates.contr.women[1,match(y,ylist)]<-rd.est[1]
  bl.estimates.contr.women[2,match(y,ylist)]<-rd.est[2]
  bl.estimates.contr.women[3,match(y,ylist)]<-rd.est[3]
  
}  

stargazer(falsification.tests.bl,out='falsification_test_pre_treatment')
stargazer(falsification.tests.nt,out='falsification_test_untreated')
stargazer(bl.estimates,out='selectively_dropped_results')
stargazer(bl.estimates.men,out='selectively_dropped_results_men')
stargazer(bl.estimates.women,out='selectively_dropped_results_women')
stargazer(bl.estimates.contr,out='controlled_results')
stargazer(bl.estimates.contr.men,out='controlled_results_men')
stargazer(bl.estimates.contr.women,out='controlled_results_women')

setwd(charts)
for (y in ylist){
#All
setEPS(width=8,height=(16/3))
postscript(paste('rd_plot_',y,'.eps',sep=''))
par(mar=c(4,2,2,2),mfrow=c(1,1))

df<-drop_min_of_retired_and_not_retired(subset(df.rds,
                year>2012&abs(running_variable)<my.opt.bw))

  rdplot(y=as.numeric(df[,y]),x=df[,'running_variable'],
       nbins=60,title=str_replace_all(y,'_',' '),p=1,
       h=my.opt.bw)
dev.off()
#Men
setEPS(width=8,height=(16/3))
postscript(paste('rd_plot_',y,'_men','.eps',sep=''))
par(mar=c(4,2,2,2),mfrow=c(1,1))

df<-drop_min_of_retired_and_not_retired(subset(df.rds,
                  sex==1&year>2012&abs(running_variable)<my.opt.bw))

rdplot(y=as.numeric(df[,y]),x=df[,'running_variable'],
       nbins=60,title=str_replace_all(y,'_',' '),p=1,
       h=my.opt.bw)
dev.off()
#Women
setEPS(width=8,height=(16/3))
postscript(paste('rd_plot_',y,'_women','.eps',sep=''))
par(mar=c(4,2,2,2),mfrow=c(1,1))

df<-drop_min_of_retired_and_not_retired(subset(df.rds,
            sex==2&year>2012&abs(running_variable)<my.opt.bw))

rdplot(y=as.numeric(df[,y]),x=df[,'running_variable'],
       nbins=60,title=str_replace_all(y,'_',' '),p=1,
       h=my.opt.bw)
dev.off()
}

y<-'attrit_next_wave'
rdplot(y=as.numeric(subset(df.rds)[,y]),x=subset(df.rds)[,'running_variable'],
       nbins=60,title=str_replace_all(y,'_',' '),p=1,
       h=my.opt.bw)
rdplot(y=as.numeric(subset(df.rds,yob!=1948)[,y]),x=subset(df.rds,yob!=1948)[,'running_variable'],
       nbins=60,title=str_replace_all(y,'_',' '),p=1,
       h=my.opt.bw)

#Estimate the effect just amongst men and women 
#from April 2018
post.2018.estimates<-matrix(ncol=length(ylist),
                     nrow=3)
colnames(post.2018.estimates)<-ylist
rownames(post.2018.estimates)<-c('estimate','standard error','obs')
post.2018.estimates.men<-post.2018.estimates
post.2018.estimates.women<-post.2018.estimates
for (y in ylist){

  #All
  rd.est<-my_linear_rd_robust(y,60,subset(df.rds,
                          (year>2018)|year==2018&month>=4))

  post.2018.estimates[1,match(y,ylist)]<-rd.est[1]
  post.2018.estimates[2,match(y,ylist)]<-rd.est[2]
  post.2018.estimates[3,match(y,ylist)]<-rd.est[3]
  
  #Men
  rd.est<-my_linear_rd_robust(y,60,subset(df.rds,
                                sex==1&(year>2018)|year==2018&month>=4))
  
  post.2018.estimates.men[1,match(y,ylist)]<-rd.est[1]
  post.2018.estimates.men[2,match(y,ylist)]<-rd.est[2]
  post.2018.estimates.men[3,match(y,ylist)]<-rd.est[3]
  
  #Women
  rd.est<-my_linear_rd_robust(y,60,subset(df.rds,
                            sex==2&(year>2018)|year==2018&month>=4))
  
  post.2018.estimates.women[1,match(y,ylist)]<-rd.est[1]
  post.2018.estimates.women[2,match(y,ylist)]<-rd.est[2]
  post.2018.estimates.women[3,match(y,ylist)]<-rd.est[3]
  
}

stargazer(post.2018.estimates,out='main_results')
stargazer(post.2018.estimates.men,out='main_results_men')
stargazer(post.2018.estimates.women,out='main_results_women')

table(subset(df.rds,
             (year>2018)|year==2018&month>=4&CESD>=0&
               abs(running_variable)<60)[,'sex'])

setwd(charts)
for (y in ylist){
  #All
  setEPS(width=8,height=(16/3))
  postscript(paste('rd_plot_',y,'post_2018.eps',sep=''))
  par(mar=c(4,2,2,2),mfrow=c(1,1))
  df<-subset(df.rds,(year>2018)|year==2018&month>=4)
  
  rdplot(y=as.numeric(df[,y]),x=df[,'running_variable'],
         nbins=60,title=str_replace_all(y,'_',' '),p=1,
         h=my.opt.bw)
  dev.off()
  #Men
  setEPS(width=8,height=(16/3))
  postscript(paste('rd_plot_',y,'post_2018_men.eps',sep=''))
  par(mar=c(4,2,2,2),mfrow=c(1,1))
  df<-subset(df.rds,(year>2018)|year==2018&month>=4&sex==1)
  
  rdplot(y=as.numeric(df[,y]),x=df[,'running_variable'],
         nbins=60,title=str_replace_all(y,'_',' '),p=1,
         h=my.opt.bw)
  dev.off()
  #Women
  setEPS(width=8,height=(16/3))
  postscript(paste('rd_plot_',y,'post_2018_women.eps',sep=''))
  par(mar=c(4,2,2,2),mfrow=c(1,1))
  df<-subset(df.rds,(year>2018)|year==2018&month>=4&sex==2)
  
  rdplot(y=as.numeric(df[,y]),x=df[,'running_variable'],
         nbins=60,title=str_replace_all(y,'_',' '),p=1,
         h=my.opt.bw)
  dev.off()
}

#make a chart showing effects by year for each dependent variable:
est.over.time<-matrix(nrow=4,ncol=length(ylist))
se.over.time<-matrix(nrow=4,ncol=length(ylist))

est.over.time.from.2000<-matrix(nrow=9,ncol=length(ylist))
se.over.time.from.2000<-matrix(nrow=9,ncol=length(ylist))
est.over.time.from.2000.unaffected<-matrix(nrow=9,ncol=length(ylist))
se.over.time.from.2000.unaffected<-matrix(nrow=9,ncol=length(ylist))

#function which returns dynamic estimates from a particular year
my_dynamic_treat_est<-function(y,h,df){
  
  coefficients<-c()
  ses<-c()
  
  for (yr.p in seq(from=2002,to=2022,by=2)){
    
    year.pair.estimate<-my_linear_rd_robust(y,my.opt.bw,
                  drop_min_of_retired_and_not_retired(subset(df,
                  year_pair==yr.p&abs(running_variable)<my.opt.bw)))
    
    coefficients<-c(coefficients,year.pair.estimate[1])
    ses<-c(ses,year.pair.estimate[2])
  
    }
  
  return(matrix(data=c(coefficients,ses),nrow=length(coefficients),ncol=2))
  
}

ylist.dynamic.eff<-ylist[5:length(ylist)]
ylist.dynamic.eff<-c(ylist.dynamic.eff[1:2],
                     ylist.dynamic.eff[4:length(ylist.dynamic.eff)])

est.over.time.from.2000<-matrix(nrow=11,ncol=length(ylist))
se.over.time.from.2000<-matrix(nrow=11,ncol=length(ylist))

est.over.time.from.2000.unaffected<-matrix(nrow=11,ncol=length(ylist))
se.over.time.from.2000.unaffected<-matrix(nrow=11,ncol=length(ylist))


for (y in ylist.dynamic.eff){
 
  ########################################
  #From 2000
  ########################################
  
  reg.rd<-my_dynamic_treat_est(y,my.opt.bw,df.affected.full.period)
  
  est.over.time.from.2000[,match(y,ylist)]<-reg.rd[,1]
  
  se.over.time.from.2000[,match(y,ylist)]<-reg.rd[,2]
  
  png(paste('dynamic_treatment_on_',y,'from_2002','.png',sep=''),width=400,height=200)
  plot(seq(from=2002,to=2022,by=2),
       est.over.time.from.2000[,match(y,ylist)],type='o',
       main=str_replace_all(y,'_',' '),
       ylab='',
       xlab='year',
       ylim=c(min(est.over.time.from.2000[,match(y,ylist)]-1.96*se.over.time.from.2000[,match(y,ylist)]),
              max(est.over.time.from.2000[,match(y,ylist)]+1.96*se.over.time.from.2000[,match(y,ylist)])))
  segments(x0=seq(from=2002,to=2022,by=2),
           est.over.time.from.2000[,match(y,ylist)]-1.96*se.over.time.from.2000[,match(y,ylist)],
           x1=seq(from=2002,to=2022,by=2),
           est.over.time.from.2000[,match(y,ylist)]+1.96*se.over.time.from.2000[,match(y,ylist)])
  lines(seq(from=2002,to=2022,by=1),seq(from=0,to=0,length.out=21))
  dev.off()
  
  ########################################
  #From 2000 for the unaffected group
  ########################################
  
  reg.rd<-my_dynamic_treat_est(y,my.opt.bw,df.unaffect.full.period)
  
  est.over.time.from.2000.unaffected[,match(y,ylist)]<-reg.rd[,1]
  
  se.over.time.from.2000.unaffected[,match(y,ylist)]<-reg.rd[,2]
  
  png(paste('dynamic_treatment_on_',y,'from_2002_unaffected_people','.png',sep=''),width=400,height=200)
  plot(seq(from=2002,to=2022,by=2),
       est.over.time.from.2000.unaffected[,match(y,ylist)],type='o',
       main=str_replace_all(y,'_',' '),
       xlab='year',
       ylab='',
       ylim=c(min(est.over.time.from.2000.unaffected[,match(y,ylist)]-1.96*se.over.time.from.2000.unaffected[,match(y,ylist)]),
              max(est.over.time.from.2000.unaffected[,match(y,ylist)]+1.96*se.over.time.from.2000.unaffected[,match(y,ylist)])))
  segments(x0=seq(from=2002,to=2022,by=2),
           est.over.time.from.2000.unaffected[,match(y,ylist)]-1.96*se.over.time.from.2000.unaffected[,match(y,ylist)],
           x1=seq(from=2002,to=2022,by=2),
           est.over.time.from.2000.unaffected[,match(y,ylist)]+1.96*se.over.time.from.2000.unaffected[,match(y,ylist)])
  lines(seq(from=2002,to=2022,by=1),seq(from=0,to=0,length.out=21))
  dev.off()
  
}

###################
#CESD placebo tests
###################
placeob.tests.CESD<-matrix(nrow=301,ncol=2)
for (val in seq(from=-150,to=150,by=1)){
  rd.est<-rdrobust(y=subset(df.for.placebos,running_variable!=val)[,y],
                   x=subset(df.for.placebos,running_variable!=val)[,'running_variable'],h=my.opt.bw,
                   kernel='tri',p=1,c=val)
  placeob.tests.CESD[match(val,seq(from=-150,to=150,by=1)),1]<-rd.est$Estimate[1]
  placeob.tests.CESD[match(val,seq(from=-150,to=150,by=1)),2]<-rd.est$se[1]
}

png('placebo_tests_different_cutoffs.png',width=400,height=200)
plot(seq(from=-150,to=150,by=1),placeob.tests.CESD[,1],type='l',
     ylim=c(min(placeob.tests.CESD[,1]-1.96*placeob.tests.CESD[,2]),
            max(placeob.tests.CESD[,1]+1.96*placeob.tests.CESD[,2])))
lines(seq(from=-150,to=150,by=1),placeob.tests.CESD[,1]+1.96*placeob.tests.CESD[,2],col=2)
lines(seq(from=-150,to=150,by=1),placeob.tests.CESD[,1]-1.96*placeob.tests.CESD[,2],col=2)
lines(seq(from=-150,to=150,by=1),seq(from=0,to=0,length.out=301))
dev.off()

######
#Just the April ones
png('placebo_tests_different_cutoffs_april_only.png',width=400,height=200)
plot(seq(from=-144,to=144,by=12),placeob.tests.CESD[match(seq(from=-144,to=144,by=12),seq(from=-150,to=150,by=1)),1],type='o',
     ylim=c(min(placeob.tests.CESD[,1]-1.96*placeob.tests.CESD[,2]),
            max(placeob.tests.CESD[,1]+1.96*placeob.tests.CESD[,2])),xaxt='n',
     ylab='discontinuity in DoB at April',xlab='year')
segments(x0=seq(from=-144,to=144,by=12),
         y0=placeob.tests.CESD[match(seq(from=-144,to=144,by=12),seq(from=-150,to=150,by=1)),1]+
           1.96*placeob.tests.CESD[match(seq(from=-144,to=144,by=12),seq(from=-150,to=150,by=1)),2],
         x1=seq(from=-144,to=144,by=12),
         y1=placeob.tests.CESD[match(seq(from=-144,to=144,by=12),seq(from=-150,to=150,by=1)),1]-
           1.96*placeob.tests.CESD[match(seq(from=-144,to=144,by=12),seq(from=-150,to=150,by=1)),2])
lines(seq(from=-150,to=150,by=1),seq(from=0,to=0,length.out=301))
axis(side=1,at=seq(from=-144,to=144,by=12),labels=seq(from=1936,to=1960,by=1))
dev.off()