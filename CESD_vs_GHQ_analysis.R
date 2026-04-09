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
  'J:\\Data\\UKDA-5050-stata\\stata\\stata13_se'
#'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\PIP_RDD\\ELSA_EUL_data'
setwd(data_directory)

wave_3_core_data<-as.data.frame(read_dta('wave_3_elsa_data_v4.DTA'))
wave_3_fin_data<-as.data.frame(read_dta('wave_3_financial_derived_variables.DTA'))

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

#Drop missings
wave_3_core_data<-subset(wave_3_core_data,
                         scghqa>0&
                           scghqb>0&
                           scghqc>0&
                           scghqd>0&
                           scghqe>0&
                           scghqf>0&
                           scghqg>0&
                           scghqh>0&
                           scghqi>0&
                           scghqj>0&
                           scghqk>0&
                           scghql>0&
                           psceda>0&
                           pscedb>0&
                           pscedc>0&
                           pscedd>0&
                           pscede>0&
                           pscedf>0&
                           pscedg>0&
                           pscedh>0)

#Create CESD score
cesd<-as.data.frame(
  ifelse(wave_3_core_data[,'psceda']<0|
           wave_3_core_data[,'pscedb']<0|
           wave_3_core_data[,'pscedc']<0|
           wave_3_core_data[,'pscedd']<0|
           wave_3_core_data[,'pscede']<0|
           wave_3_core_data[,'pscedf']<0|
           wave_3_core_data[,'pscedg']<0|
           wave_3_core_data[,'pscedh']<0,NA,
         as.numeric(wave_3_core_data[,'psceda']==1)+
           as.numeric(wave_3_core_data[,'pscedb']==1)+
           as.numeric(wave_3_core_data[,'pscedc']==1)+
           as.numeric(wave_3_core_data[,'pscedd']==2)+
           as.numeric(wave_3_core_data[,'pscede']==1)+
           as.numeric(wave_3_core_data[,'pscedf']==2)+
           as.numeric(wave_3_core_data[,'pscedg']==1)+
           as.numeric(wave_3_core_data[,'pscedh']==1)))
colnames(cesd)<-'CESD'

#And GHQ12
ghq12<-as.data.frame(
  ifelse(wave_3_core_data[,'scghqa']<0|
           wave_3_core_data[,'scghqb']<0|
           wave_3_core_data[,'scghqc']<0|
           wave_3_core_data[,'scghqd']<0|
           wave_3_core_data[,'scghqe']<0|
           wave_3_core_data[,'scghqf']<0|
           wave_3_core_data[,'scghqg']<0|
           wave_3_core_data[,'scghqh']<0|
           wave_3_core_data[,'scghqi']<0|
           wave_3_core_data[,'scghqj']<0|
           wave_3_core_data[,'scghqk']<0|
           wave_3_core_data[,'scghql']<0,NA,
           as.numeric(wave_3_core_data[,'scghqa']>2)+
           as.numeric(wave_3_core_data[,'scghqb']>2)+
           as.numeric(wave_3_core_data[,'scghqc']>2)+
           as.numeric(wave_3_core_data[,'scghqd']>2)+
           as.numeric(wave_3_core_data[,'scghqe']>2)+
           as.numeric(wave_3_core_data[,'scghqf']>2)+
           as.numeric(wave_3_core_data[,'scghqg']>2)+
           as.numeric(wave_3_core_data[,'scghqh']>2)+
           as.numeric(wave_3_core_data[,'scghqi']>2)+
           as.numeric(wave_3_core_data[,'scghqj']>2)+
           as.numeric(wave_3_core_data[,'scghqk']>2)+
           as.numeric(wave_3_core_data[,'scghql']>2)))
colnames(ghq12)<-'GHQ12_caseness'

wave_3_core_data<-cbind(wave_3_core_data,
                        cesd,ghq12)

df<-subset(wave_3_core_data,is.na(GHQ12_caseness))

my_cdf<-function(x){
  
  var.cdf<-table(x)
  
  for (i in seq(from=2,to=length(var.cdf),by=1)){
    var.cdf[i]<-var.cdf[i-1]+var.cdf[i] 
  }
  
  var.cdf<-var.cdf/max(var.cdf)
  
  return(cbind(names(var.cdf),as.matrix(var.cdf)))
  
}

#Model CESD using the GHQ12
CESD.function.of.ghq12<-lm(CESD~factor(scghqa)+factor(scghqb)+factor(scghqc)+
        factor(scghqd)+factor(scghqe)+factor(scghqf)+
        factor(scghqg)+factor(scghqh)+factor(scghqi)+
        factor(scghqj)+factor(scghqk)+factor(scghql),
        data=wave_3_core_data)

predicted.cesd<-as.data.frame(predict(CESD.function.of.ghq12))
colnames(predicted.cesd)<-'predicted_CESD'
wave_3_core_data<-cbind(wave_3_core_data,predicted.cesd)

cor(wave_3_core_data[,c('CESD','GHQ12_caseness','predicted_CESD')])
stargazer(wave_3_core_data[,c('CESD','GHQ12_caseness','predicted_CESD')])

plot(my_cdf(wave_3_core_data[,'CESD']/
              max(wave_3_core_data[,'CESD'])),ylim=c(0,1),
     type='l',main='Distribution of CESD and GHQ12',
     xlab='standardised score',ylab='cumulative dist')
lines(my_cdf(wave_3_core_data[,'GHQ12_caseness']/
               max(wave_3_core_data[,'GHQ12_caseness'])),col=2,
      lty=2)
lines(my_cdf(wave_3_core_data[,'predicted_CESD']/
               max(wave_3_core_data[,'predicted_CESD'])),col=3,
      lty=3)
legend('bottomright',legend=c('CESD','GHQ12 caseness','Predicted_CESD'),
       col=c(1,2,3),lty=c(1,2,3))

write.csv(coef(CESD.function.of.ghq12),'predict_CESD_reg.CSV')

test