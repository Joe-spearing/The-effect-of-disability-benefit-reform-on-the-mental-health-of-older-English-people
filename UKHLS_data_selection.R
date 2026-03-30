rm(list=ls())
setwd('J:\\Data\\UKHLS\\stata\\stata14_se\\bhps')
library(haven)
###

#upload this data: BHPS
count.to.seventeen<-seq(from=1,to=18,by=1)
letter.list<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r')
#for each number
for (val in count.to.seventeen){
  #upload data: indresp
  file.name<-paste('b',letter.list[val],'_indresp_protect.dta',sep='')
  df<-read_dta(file.name)
  df_name<-paste('indresp_',val,sep='')
  assign(df_name,df)
  #hhresp
  file.name<-paste('b',letter.list[val],'_hhresp_protect.dta',sep='')
  df<-read_dta(file.name)
  df_name<-paste('hhresp_',val,sep='')
  assign(df_name,df)
  #income
  file.name<-paste('b',letter.list[val],'_income_protect.dta',sep='')
  df<-read_dta(file.name)
  df_name<-paste('income_',val,sep='')
  assign(df_name,df)
}

setwd('J:\\Data\\UKHLS\\stata\\stata14_se\\ukhls')
#and now the understanding society data
count.to.twelve<-seq(from=1,to=12,by=1)
#for each number
for (val in count.to.twelve){
  #upload data: indresp
  file.name<-paste(letter.list[val],'_indresp_protect.dta',sep='')
  df<-read_dta(file.name)
  df_name<-paste('indresp_',val+18,sep='')
  assign(df_name,df)
  #hhresp
  file.name<-paste(letter.list[val],'_hhresp_protect.dta',sep='')
  df<-read_dta(file.name)
  df_name<-paste('hhresp_',val+18,sep='')
  assign(df_name,df)
  #income
  file.name<-paste(letter.list[val],'_income_protect.dta',sep='')
  df<-read_dta(file.name)
  df_name<-paste('income_',val+18,sep='')
  assign(df_name,df)
}

cross.wave.data<-as.data.frame(read_dta('xwavedat_protect.DTA'))

#create some df lists
indresp.list<-list(indresp_1,indresp_2,indresp_3,indresp_4,indresp_5,indresp_6,indresp_7,indresp_8,indresp_9,indresp_10,
                   indresp_11,indresp_12,indresp_13,indresp_14,indresp_15,indresp_16,indresp_17,indresp_18,indresp_19,
                   indresp_20,indresp_21,indresp_22,indresp_23,indresp_24,indresp_25,indresp_26,indresp_27,indresp_28,
                   indresp_29,indresp_30)

hhresp.list<-list(hhresp_1,hhresp_2,hhresp_3,hhresp_4,hhresp_5,hhresp_6,hhresp_7,hhresp_8,hhresp_9,hhresp_10,
                  hhresp_11,hhresp_12,hhresp_13,hhresp_14,hhresp_15,hhresp_16,hhresp_17,hhresp_18,hhresp_19,
                  hhresp_20,hhresp_21,hhresp_22,hhresp_23,hhresp_24,hhresp_25,hhresp_26,hhresp_27,hhresp_28,
                  hhresp_29,hhresp_30)

income.list<-list(income_1,income_2,income_3,income_4,income_5,income_6,income_7,income_8,income_9,income_10,
                  income_11,income_12,income_13,income_14,income_15,income_16,income_17,income_18,income_19,
                  income_20,income_21,income_22,income_23,income_24,income_25,income_26,income_27,income_28,
                  income_29,income_30)

#for the BHPS waves
#given we're in wave val
for (val in count.to.seventeen){
#get a list of variables (and names) I want to extract
person.ID<-'pid'
sex<-paste('b',letter.list[val],'_sex',sep='')

age<-paste('b',letter.list[val],'_age',sep='')

GHQ1<-paste('b',letter.list[val],'_scghqa',sep='')
GHQ2<-paste('b',letter.list[val],'_scghqb',sep='')
GHQ3<-paste('b',letter.list[val],'_scghqc',sep='')
GHQ4<-paste('b',letter.list[val],'_scghqd',sep='')
GHQ5<-paste('b',letter.list[val],'_scghqe',sep='')
GHQ6<-paste('b',letter.list[val],'_scghqf',sep='')
GHQ7<-paste('b',letter.list[val],'_scghqg',sep='')
GHQ8<-paste('b',letter.list[val],'_scghqh',sep='')
GHQ9<-paste('b',letter.list[val],'_scghqi',sep='')
GHQ10<-paste('b',letter.list[val],'_scghqj',sep='')
GHQ11<-paste('b',letter.list[val],'_scghqk',sep='')
GHQ12<-paste('b',letter.list[val],'_scghql',sep='')

claim.dla<-ifelse(val>7,
                  paste('b',letter.list[val],'_f128',sep=''),
                  paste('b',letter.list[val],'_f123',sep=''))

interview.month<-paste('b',letter.list[val],'_istrtdatm',sep='')
interview.year<-paste('b',letter.list[val],'_istrtdaty',sep='')
educ<-paste('b',letter.list[val],'_hiqual_dv',sep='')
employment.stat<-paste('b',letter.list[val],'_jbstat',sep='')

income.receipt<-paste('b',letter.list[val],'_ficode',sep='')
income.amount<-paste('b',letter.list[val],'_frval',sep='')
income.period<-paste('b',letter.list[val],'_frw',sep='')

birth.month<-'birthm'
birth.year<-'birthy'
birth.month.dv<-'doby_dv'
birth.year.dv<-'dobm_dv'

#find the indresp df we want
indresp_n<-as.data.frame(indresp.list[val])
#and the hhresp
hhresp_n<-as.data.frame(hhresp.list[val])
#and the income
income_n<-as.data.frame(income.list[val])

benefits.df<-matrix(ncol=0,nrow=nrow(indresp_n))
for (val.1 in c(10,11,41)){
  #get the income I want
  colnames(income_n)[match(income.receipt,colnames(income_n))]<-'ficode'
  df.income<-subset(income_n,ficode==val.1)
  #get the income amount and add it
  benefits.df<-cbind(benefits.df,df.income[match(indresp_n[,'pidp'],
                            df.income[,'pidp']),income.amount],
                     df.income[match(indresp_n[,'pidp'],
                                     df.income[,'pidp']),income.period])
}


#ditto for year and month of birth
mob<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'birthm'])
yob<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'birthy'])
mob.dv<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'dobm_dv'])
yob.dv<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'doby_dv'])

#annoyingly, we also need to pick out the right personal identifier (we want the cross wave one)
pidp<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),
                    as.numeric(as.character(unlist(cross.wave.data[,2])))),'pidp'])
hhid<-indresp_n[,paste('b',letter.list[val],'_hidp',sep='')]

dla.var<-ifelse(rep(val,nrow(indresp_n))==1,NA,
                indresp_n[,claim.dla])

#stick this all in a df
df<-as.data.frame(cbind(pidp,
                        indresp_n[,sex],
                        NA,
                        indresp_n[,age],
                        yob,
                        yob.dv,
                        mob,
                        mob.dv,
                        indresp_n[,GHQ1],indresp_n[,GHQ2],
                        indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],
                        indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],
                        indresp_n[,GHQ9],
                        indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],
                        NA,
                        benefits.df,
                        indresp_n[,interview.month],
                        indresp_n[,interview.year],
                        indresp_n[,educ],
                        indresp_n[,employment.stat],
                        dla.var,
                        NA,
                        val,
                        hhid))
                        

#name columns
colnames(df)<-c('pid','sex','health','age',
                'yob','yob_dv','mob','mob_dv',
                'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                'GHQ11','GHQ12','assets',
                'DLA','DLA_period','AA','AA_period','PIP','PIP_period',
                'month','year','education','work_status',
                'claims_DLA','claims_PIP','wave_number',
                'hhid')

#name df
df.name<-paste('wave',val,'data',sep='')

assign(df.name,df)

}

#now for the US waves:
for (val in count.to.twelve){
  #get a list of variables (and names) I want to extract
  person.ID<-'pidp'
  sex<-paste(letter.list[val],'_sex',sep='')
  health.in.general<-ifelse(val==1,paste(letter.list[val],'_sf1',sep=''),
                            paste(letter.list[val],'_scsf1',sep=''))
  age<-paste(letter.list[val],'_age_dv',sep='')
  
  employment.stat<-paste(letter.list[val],'_jbstat',sep='')
  
  GHQ1<-paste(letter.list[val],'_scghqa',sep='')
  GHQ2<-paste(letter.list[val],'_scghqb',sep='')
  GHQ3<-paste(letter.list[val],'_scghqc',sep='')
  GHQ4<-paste(letter.list[val],'_scghqd',sep='')
  GHQ5<-paste(letter.list[val],'_scghqe',sep='')
  GHQ6<-paste(letter.list[val],'_scghqf',sep='')
  GHQ7<-paste(letter.list[val],'_scghqg',sep='')
  GHQ8<-paste(letter.list[val],'_scghqh',sep='')
  GHQ9<-paste(letter.list[val],'_scghqi',sep='')
  GHQ10<-paste(letter.list[val],'_scghqj',sep='')
  GHQ11<-paste(letter.list[val],'_scghqk',sep='')
  GHQ12<-paste(letter.list[val],'_scghql',sep='')
 
  claim.dla<-paste(letter.list[val],'_bendis5',sep='')
  claim.pip<-paste(letter.list[val],'_bendis12',sep='')
  
  educ<-paste(letter.list[val],'_hiqual_dv',sep='')
  
  hh.identifier<-paste(letter.list[val],'_hidp',sep='')
  
  interview.month<-paste(letter.list[val],'_istrtdatm',sep='')
  interview.year<-paste(letter.list[val],'_istrtdaty',sep='')
  interview.day<-paste(letter.list[val],'_istrtdatd',sep='')
  birth.month<-'birthm'
  birth.year<-'birthy'
  
  income.receipt<-paste(letter.list[val],'_ficode',sep='')
  income.amount<-paste(letter.list[val],'_frval',sep='')
  income.period<-paste(letter.list[val],'_frwc',sep='')
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val+18])
  #and the hhresp
  hhresp_n<-as.data.frame(hhresp.list[val+18])
  #and the income
  income_n<-as.data.frame(income.list[val+18])
  
  benefits.df<-matrix(ncol=0,nrow=nrow(indresp_n))
  for (val.1 in c(10,11,41)){
    #get the income I want
    colnames(income_n)[match(income.receipt,colnames(income_n))]<-'ficode'
    df.income<-subset(income_n,ficode==val.1)
    #get the income amount and add it
    benefits.df<-cbind(benefits.df,df.income[match(indresp_n[,'pidp'],
                                                   df.income[,'pidp']),
                                             income.amount],
                       df.income[match(indresp_n[,'pidp'],
                                       df.income[,'pidp']),income.period])
  }
  
  #ditto for year and month of birth
  mob<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,1])))),'birthm'])
  yob<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,1])))),'birthy'])
  mob.dv<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,1])))),'dobm_dv'])
  yob.dv<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,1])))),'doby_dv'])
  
  #annoyingly, we also need to pick out the right personal identifier (we want the cross wave one)
  pidp<-indresp_n[,person.ID]
  
  pip.var<-ifelse(rep(val,nrow(indresp_n))<3,NA,
                  indresp_n[,claim.pip])
  
  #stick this all in a df
  df<-as.data.frame(cbind(pidp,
                          indresp_n[,sex],
                          indresp_n[,health.in.general],
                          indresp_n[,age],
                          yob,
                          yob.dv,
                          mob,
                          mob.dv,
                          indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],
                          indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],
                          indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],
                          NA,
                          benefits.df,
                          indresp_n[,interview.month],
                          indresp_n[,interview.year],
                          indresp_n[,educ],
                          indresp_n[,employment.stat],
                          indresp_n[,claim.dla],
                          pip.var,
                          val+18,
                          indresp_n[,hh.identifier]))
  
  #name columns
  colnames(df)<-c('pid','sex','health','age',
                  'yob','yob_dv','mob','mob_dv',
                  'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                  'GHQ11','GHQ12','assets',
                  'DLA','DLA_period','AA','AA_period','PIP','PIP_period',
                  'month','year','education','work_status',
                  'claims_DLA','claims_PIP','wave_number',
                  'hhid')
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  
}

for (var in c('work_status','month',
              'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5',
              'GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
              'GHQ11','GHQ12','sex',
              'education','claims_DLA','claims_PIP')){
wave1data[,var]<-as.numeric(wave1data[,var])
wave2data[,var]<-as.numeric(wave2data[,var])
wave3data[,var]<-as.numeric(wave3data[,var])
wave4data[,var]<-as.numeric(wave4data[,var])
wave5data[,var]<-as.numeric(wave5data[,var])
wave6data[,var]<-as.numeric(wave6data[,var])
wave7data[,var]<-as.numeric(wave7data[,var])
wave8data[,var]<-as.numeric(wave8data[,var])
wave9data[,var]<-as.numeric(wave9data[,var])
wave10data[,var]<-as.numeric(wave10data[,var])
wave11data[,var]<-as.numeric(wave11data[,var])
wave12data[,var]<-as.numeric(wave12data[,var])
wave13data[,var]<-as.numeric(wave13data[,var])
wave14data[,var]<-as.numeric(wave14data[,var])
wave15data[,var]<-as.numeric(wave15data[,var])
wave16data[,var]<-as.numeric(wave16data[,var])
wave17data[,var]<-as.numeric(wave17data[,var])
wave18data[,var]<-as.numeric(wave18data[,var])
wave19data[,var]<-as.numeric(wave19data[,var])
wave20data[,var]<-as.numeric(wave20data[,var])
wave21data[,var]<-as.numeric(wave21data[,var])
wave22data[,var]<-as.numeric(wave22data[,var])
wave23data[,var]<-as.numeric(wave23data[,var])
wave24data[,var]<-as.numeric(wave24data[,var])
wave25data[,var]<-as.numeric(wave25data[,var])
wave26data[,var]<-as.numeric(wave26data[,var])
wave27data[,var]<-as.numeric(wave27data[,var])
wave28data[,var]<-as.numeric(wave28data[,var])
wave29data[,var]<-as.numeric(wave29data[,var])
wave30data[,var]<-as.numeric(wave30data[,var])
}

#create a 'full data set'
full.data.set<-rbind(wave1data,wave2data,wave3data,wave4data,wave5data,wave6data,wave7data,wave8data,wave9data,
                    wave10data,wave11data,wave12data,
                     wave13data,wave14data,wave15data,wave16data,wave17data,wave18data,wave19data,
                     wave20data,wave21data,wave22data,wave23data,
                     wave24data,wave25data,wave26data,wave27data,wave28data,
                     wave29data,wave30data)

#check that the overlapping pids work
bhps.pids<-unique(subset(full.data.set,wave_number<19)[,'pid'])
ukhls.pids<-unique(subset(full.data.set,wave_number>18)[,'pid'])

bhps.pids[bhps.pids%in%ukhls.pids]

full.data.set['77915',]
this.income<-subset(income_8,pidp=='1897282')
subset(full.data.set,pid==612225765)
subset(full.data.set,pid==223725)
subset(full.data.set,pid==272262485)
subset(full.data.set,pid==409168245)

cor(full.data.set[,'mob'],full.data.set[,'mob_dv'])
plot(full.data.set[,'yob'],full.data.set[,'yob_dv'])

plot(table(subset(full.data.set,mob>0)[,'mob']),xlim=c(0,13))
lines(seq(from=1.1,12.1,by=1),
  table(subset(full.data.set,mob_dv>0)[,'mob_dv']),lty=2)

#save this new data
write.csv(full.data.set,'UKHLS_pip_rd_data.CSV')
#full.data.set<-read.csv('UKHLS_pip_rd_data.CSV')

cpi.data<-read.csv('CPI_index.CSV')
cpi.index<-as.data.frame(
  cpi.data[match(full.data.set[,'year'],cpi.data[,1]),2])
colnames(cpi.index)<-'CPI_index'
full.data.set<-cbind(full.data.set,cpi.index)

######################
#Data cleaning

#Set PIP and DLA NAs to zero
for (var in c('DLA','PIP','AA','claims_DLA','claims_PIP')){
  full.data.set[,var]<-ifelse(is.na(full.data.set[,var]),0,
                              full.data.set[,var])                        
}

#for some other variables, set their negatives to NA
for (var in c('sex','health','age','yob',
              'yob_dv','mob','mob_dv','GHQ1',
              'GHQ2','GHQ3','GHQ4','GHQ5','GHQ6',
              'GHQ7','GHQ8','GHQ9','GHQ10','GHQ11',
              'GHQ12','DLA_period','AA_period',
              'PIP_period','month','year',
              'education','work_status','claims_DLA',
              'claims_PIP')){
  full.data.set[,var]<-ifelse(full.data.set[,var]<0,NA,
                              full.data.set[,var])
}

#convert financial variables to real numbers in weekly amounts
for (var in c('DLA','PIP','AA')){
  full.data.set[,var]<-full.data.set[,var]*100/
                      full.data.set[,'CPI_index']
  
  #First for BHPS
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']<19,
                              full.data.set[,var]/
                              full.data.set[,paste(var,'_period',sep='')],
                              full.data.set[,var])
                              
  #Then UKHLS                            
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                              full.data.set[,paste(var,'_period',sep='')]<5,
                              full.data.set[,var]/
                              full.data.set[,paste(var,'_period',sep='')],
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                              full.data.set[,paste(var,'_period',sep='')]==5,
                              full.data.set[,var]/4.333333,
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                              full.data.set[,paste(var,'_period',sep='')]==7,
                              full.data.set[,var]/(2*4.333333),
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                              full.data.set[,paste(var,'_period',sep='')]==8,
                              full.data.set[,var]/(6.5),
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                                full.data.set[,paste(var,'_period',sep='')]==9,
                              full.data.set[,var]/(5.777778),
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                                full.data.set[,paste(var,'_period',sep='')]==26,
                              full.data.set[,var]/26,
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                                full.data.set[,paste(var,'_period',sep='')]==52,
                              full.data.set[,var]/(52),
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                                full.data.set[,paste(var,'_period',sep='')]==95,
                              NA,
                              full.data.set[,var])
  
  full.data.set[,var]<-ifelse(full.data.set[,'wave_number']>18&
                                full.data.set[,paste(var,'_period',sep='')]==96,
                              NA,
                              full.data.set[,var])
  
}

table(full.data.set[,'DLA']>0)

#df.test<-as.data.frame(
#       cbind(full.data.set[,var],full.data.set[,paste(var,'_period',sep='')],
#               full.data.set[,var]/
#                full.data.set[,paste(var,'_period',sep='')]))

table(subset(full.data.set,DLA>2000)[,'wave_number'])

full.data.set[match(max(subset(full.data.set,DLA>=0)[,'DLA']),
                    full.data.set[,'DLA']),]
plot(density(subset(full.data.set,DLA>=0)[,'DLA']))

#this.income<-subset(income_21,pidp==136690207)
#this.income<-subset(income_20,pidp==71796445)

#Set DLA and PIP NAs to zero
full.data.set[,'DLA']<-ifelse(is.na(full.data.set[,'DLA']),0,
                              full.data.set[,'DLA'])
full.data.set[,'PIP']<-ifelse(is.na(full.data.set[,'PIP']),0,
                              full.data.set[,'PIP'])

#create some dependent variables
dla.pos<-as.data.frame(full.data.set[,'DLA']>0)
pip.pos<-as.data.frame(full.data.set[,'PIP']>0)
dla.or.pip.pos<-as.data.frame(full.data.set[,'PIP']>0|
                                full.data.set[,'DLA']>0)
dla.pip.payments<-as.data.frame(full.data.set[,'PIP']+
                                full.data.set[,'DLA'])
benefits.payments<-as.data.frame(full.data.set[,'PIP']+
                                full.data.set[,'DLA']+
                                   full.data.set[,'AA'])
for (val in seq(from=1,to=12,by=1)){
  full.data.set[,paste('GHQ',val,sep='')]<-ifelse(full.data.set[,paste('GHQ',val,sep='')]<3,
                                            0,1)
}

ghq12.caseness<-as.data.frame(full.data.set[,'GHQ1']+
                              full.data.set[,'GHQ2']+
                              full.data.set[,'GHQ3']+
                              full.data.set[,'GHQ4']+
                              full.data.set[,'GHQ5']+
                              full.data.set[,'GHQ6']+
                              full.data.set[,'GHQ7']+
                              full.data.set[,'GHQ8']+
                              full.data.set[,'GHQ9']+
                              full.data.set[,'GHQ10']+
                              full.data.set[,'GHQ11']+
                              full.data.set[,'GHQ12'])

good.health<-as.data.frame(
  ifelse(full.data.set[,'health']>0&full.data.set[,'health']<4,1,
         ifelse(full.data.set[,'health']>3,0,NA)))
table(good.health)

retired<-as.data.frame(as.numeric(full.data.set[,'work_status']==4))
working<-as.data.frame(as.numeric(full.data.set[,'work_status']==2|
                                  full.data.set[,'work_status']==1))
colnames(retired)<-'retired'
colnames(working)<-'working'
colnames(dla.pos)<-'dla_positive'
colnames(pip.pos)<-'pip_positive'
colnames(dla.or.pip.pos)<-'dla_or_pip_positive'
colnames(dla.pip.payments)<-'total_dla_pip_payments'
colnames(benefits.payments)<-'total_benefit_payments'
colnames(good.health)<-'good_health'
colnames(ghq12.caseness)<-'GHQ12_caseness'
high.ghq12caseness<-as.data.frame(as.numeric(ghq12.caseness[,'GHQ12_caseness']>8))
colnames(high.ghq12caseness)<-'high_GHQ12_caseness'

full.data.set<-cbind(full.data.set,dla.pos,pip.pos,dla.or.pip.pos,dla.pip.payments,
               benefits.payments,
               good.health,ghq12.caseness,working,retired,high.ghq12caseness)

my.age<-as.data.frame(
  ifelse(full.data.set[,'year']>0&full.data.set[,'yob']>0&
         full.data.set[,'month']>0&full.data.set[,'mob']>0,
         floor((12*(full.data.set[,'year']-full.data.set[,'yob'])+
                  (full.data.set[,'month']-full.data.set[,'mob']))/12),
         NA))

#issue when month of survey is month of birth
my.age[,1]<-ifelse(full.data.set[,'mob']==full.data.set[,'month']&
                   abs(my.age[,1]-full.data.set[,'age'])<=1,
                   full.data.set[,'age'],
                   my.age[,1])

colnames(my.age)<-'my_age'
full.data.set<-cbind(full.data.set,my.age)

full.data.set[,'my_age']<-ifelse(is.na(full.data.set[,'my_age']),
                           full.data.set[,'age'],
                           full.data.set[,'my_age'])

subset(full.data.set,age!=my_age)
subset(full.data.set,pid==68002729)

plot(full.data.set[,'my_age'],
     full.data.set[,'age'])
cor(subset(full.data.set,is.na(age)==FALSE&is.na(my_age)==FALSE)[,'my_age'],
    subset(full.data.set,is.na(age)==FALSE&is.na(my_age)==FALSE)[,'age'])

#Construct running variable
run.var<-(12*full.data.set[,'yob']+full.data.set[,'mob']+0.5)-
  ((8/30)+4+12*1948)
run.var.sq<-run.var^2

run.var<-as.data.frame(
  run.var)
colnames(run.var)<-'running_variable'

run.var.sq<-as.data.frame(
  run.var.sq)
colnames(run.var.sq)<-'running_variable_sq'

full.data.set<-cbind(full.data.set,run.var,run.var.sq)
full.data.set<-subset(full.data.set,is.na(running_variable)==FALSE)

cor(subset(full.data.set,mob>=0&mob_dv>=0)[,'mob'],
subset(full.data.set,mob>=0&mob_dv>=0)[,'mob_dv'])

stargazer(full.data.set)

#function for creating p.observed
attach_observed_propensity_score<-function(df,id,wave.var,predictors,discrete){
  
  pids.in.sample<-unique(subset(df,wave_number<=20)[,id])
  observations.df<-as.data.frame(
    cbind(rep(pids.in.sample,times=28),
          rep(seq(from=1,to=28,by=1),each=length(pids.in.sample)))
  )
  colnames(observations.df)<-c(id,wave.var)
  observed<-as.numeric(
    is.na(match(interaction(observations.df[,id],
                            observations.df[,wave.var]),
                interaction(df[,id],
                            df[,wave.var])))==FALSE)
 
  observations.df<-cbind(observations.df,
                         observed)
  
  last.wave.each<-aggregate(as.formula(paste(wave.var,'~',id,sep='')),
                            FUN=max,
                            data=df[df[,wave.var]<=20,])
  
  for (var in predictors){
    new.var<-as.data.frame(
              df[match(interaction(observations.df[,id],
                last.wave.each[match(observations.df[,id],last.wave.each[,1]),2]),
                interaction(df[,id],df[,wave.var])),var])
    colnames(new.var)<-var
    
    observations.df<-cbind(observations.df,new.var)
    
  }
  
  observed.model<-glm(as.formula(paste('observed ~ ',
                    paste(paste('factor(', wave.var, ')*',
                                    predictors[discrete==0],sep='',
                                    collapse = "+"),
                              paste('factor(', wave.var, ')*factor(',
                                    predictors[discrete==1],')',sep='',
                                    collapse = "+"),sep='+',collapse = "+"))),
                      family = binomial(link = "logit"), 
                      data = subset(observations.df,wave_number>19),
                    na.action = na.exclude)
  
  model.prediction<-cbind(subset(observations.df,wave_number>19),predict(observed.model))
  
  df<-cbind(df,
            model.prediction[match(interaction(df[,id],df[,wave.var]),
                  interaction(model.prediction[,id],model.prediction[,wave.var])),'predict(observed.model)'])
  
  colnames(df)[ncol(df)]<-'predicted_p_observed'
  
  df[,'predicted_p_observed']<-exp(df[,'predicted_p_observed'])/(1+exp(df[,'predicted_p_observed']))
  
  return(df)

}

df.new<-attach_observed_propensity_score(full.data.set,'pid','wave_number',
                                         c('sex','health','my_age','education','GHQ12_caseness'),
                                         c(1,1,0,1,0))

#save this new data
write.csv(df.new,'clean_UKHLS_pip_rd_data_v2.CSV')

##################
#Adding in deaths#
##################

present.in.wave.28<-unique(subset(df.new,wave_number>=28)[,'pid'])
dead.before.wave.28<-subset(cross.wave.data,dcsedw_dv<=28&dcsedw_dv>0)[,'pidp']

deaths.df<-as.data.frame(
           cbind(c(present.in.wave.28,dead.before.wave.28),
                 c(seq(from=0,to=0,length.out=length(present.in.wave.28)),
                 seq(from=1,to=1,length.out=length(dead.before.wave.28)))))
colnames(deaths.df)<-c('pid','died_before_wave_28')

write.csv(deaths.df,'deaths_by_wave_28.CSV')

#Notes: drop if DLA>200
#Add region

plot(density(subset(full.data.set,PIP<=100&PIP>=0)[,'PIP']))

#share of DLA claimants not receiving DLA income
nrow(subset(full.data.set,claims_DLA==1&dla_positive==0))/
  nrow(subset(full.data.set,claims_DLA==1))
#share of PIP claimants not receiving PIP income
nrow(subset(full.data.set,claims_PIP==1&pip_positive==0))/
  nrow(subset(full.data.set,claims_PIP==1))
#share of non-DLA claimants with DLA income
nrow(subset(full.data.set,claims_DLA==0&dla_positive==1))/
  nrow(subset(full.data.set,claims_DLA==0))
#share of non-PIP claimants with PIP income
nrow(subset(full.data.set,claims_PIP==0&pip_positive==1))/
  nrow(subset(full.data.set,claims_PIP==0))

#how many people claim DLA/PIP but never have DLA/PIP income?
sample.period.income.and.claims<-cbind(aggregate(DLA~pid,FUN=sum,
                                       data=subset(full.data.set,DLA>=0&claims_DLA>=0&PIP>=0&claims_PIP>=0)),
                                       aggregate(claims_DLA~pid,FUN=sum,
                                       data=subset(full.data.set,DLA>=0&claims_DLA>=0&PIP>=0&claims_PIP>=0)),
                                       aggregate(PIP~pid,FUN=sum,
                                                 data=subset(full.data.set,DLA>=0&claims_DLA>=0&PIP>=0&claims_PIP>=0)),
                                       aggregate(claims_PIP~pid,FUN=sum,
                                                 data=subset(full.data.set,DLA>=0&claims_DLA>=0&PIP>=0&claims_PIP>=0)))

nrow(subset(sample.period.income.and.claims,DLA==0&claims_DLA>0))/
  nrow(subset(sample.period.income.and.claims,claims_DLA>0))
nrow(subset(sample.period.income.and.claims,PIP==0&claims_PIP>0))/
  nrow(subset(sample.period.income.and.claims,claims_PIP>0))
