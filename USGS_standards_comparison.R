#pulling violations from Mohawk data
#Alene Onion and Charles Stoll
#2017.12.06


################################################################
# SET PRIMARY VARIABLES FOR SCRIPT
################################################################

# Data input/output locations
outputdata_Location<-"C:/NYBackup/CStoll.CDrive/Wallkill/Violation_Analysis_Data/Output/"#<--Folder where produced data tables will be placed
inputdata_Location<-"C:/NYBackup/CStoll.CDrive/Wallkill/Violation_Analysis_Data/Input/"#<--Folder where input data will be taken from

# Location where lab analysis was performed; this defines table formatting
analysis_lab<-"ALS" ##Possible inputs: "USGS" ; "ALS"

# Data table of chemistry values
data_table<-paste(inputdata_Location,"","RIBS_Wallkill_Chem_Input.csv",sep="")#<--Data table of water chemistry to be compared with standards tables
check_output<-paste(outputdata_Location,"","mismatched_units.csv",sep="")#<--Table output of parameters where units are different than ug/L; the script should adjust for these, however, if there are unit conversion errors this will serve as a reference point
data_output<-paste(outputdata_Location,"","TotalData_preStandardsCompare.csv",sep="")#<--Final version of data w/classifications before comparing with standards table
finalOutput_csv<-paste(outputdata_Location,"","RIBS_Wallkill_Chem_Input_Violations.csv",sep="")#<--Final output file name

# Data table of waterbody classifications
classification_table<-paste(inputdata_Location,"","Mohawk.data.classifications.csv",sep="")#<--Waterbody classification table

# Standards tables
standardsA_table<-paste(inputdata_Location,"Parameter.Standards.Tables/","ClassA_Standards.csv",sep="")#<--Class A water quality standards table
standardsB_table<-paste(inputdata_Location,"Parameter.Standards.Tables/","ClassB_Standards.csv",sep="")#<--Class B water quality standards table
standardsC_table<-paste(inputdata_Location,"Parameter.Standards.Tables/","ClassC_Standards.csv",sep="")#<--Class C water quality standards table
standardsD_table<-paste(inputdata_Location,"Parameter.Standards.Tables/","ClassD_Standards.csv",sep="")#<--Class D water quality standards table
dissolved_oxygen<-paste(inputdata_Location,"Parameter.Standards.Tables/","Dissolved_Oxygen_Standards.csv",sep="")#<--table of dissolved oxygen standards
class_abc_T_TS_ammonia<-paste(inputdata_Location,"Parameter.Standards.Tables/Ammonia/","ClassABC_Ammonia_AquatChron_W_T_TS_specification.csv",sep="")#<--table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification
class_abc_wOut_T_TS_ammonia<-paste(inputdata_Location,"Parameter.Standards.Tables/Ammonia/","ClassABC_Ammonia_AquatChron_Wout_T_TS_specification.csv",sep="")#<--table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification
class_D_wOut_TS_ammonia<-paste(inputdata_Location,"Parameter.Standards.Tables/Ammonia/","ClassD_Ammonia_AquatChron_Wout_T_TS_specification.csv",sep="")#<--table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification


################################################################
# Read csv tables
################################################################
class<-read.csv(file=classification_table, header = TRUE, sep = ",")
data<-read.csv(file=data_table, header = TRUE, sep = ",")
standardsA <- read.csv(file=standardsA_table, header = TRUE, sep = ",")
standardsB <- read.csv(file=standardsB_table, header = TRUE, sep = ",")
standardsC <- read.csv(file=standardsC_table, header = TRUE, sep = ",")
standardsD <- read.csv(file=standardsD_table, header = TRUE, sep = ",")
diss_oxygen <- read.csv(file=dissolved_oxygen, header = TRUE, sep = ",")
abc_T_TS_ammonia <- read.csv(file=class_abc_T_TS_ammonia, header=TRUE, sep = ",")
abc_wOut_T_TS_ammonia <- read.csv(file=class_abc_wOut_T_TS_ammonia, header=TRUE, sep = ",")
D_wOut_TS_ammonia <- read.csv(file=class_D_wOut_TS_ammonia, header=TRUE, sep = ",")

################################################################
#Remove data that has detection limit remarks
################################################################

#Remove data records with REMRK as these reflect less than detection limits
data <- subset(data, data$REMRK=="" | data$REMRK=="A" | data$REMRK=="E" | data$REMRK=="M")

################################################################
#Add waterbody classifications to the data table
################################################################

#restrict the class table to only the columns we need
keep <- c("LOCATION","RIVMILE","STATION_ID","CLASSIFICA","STANDARD")
class <- class[keep]
rm(keep)

#rename the STATION.ID to STAID
names(class)[names(class)=="STATION_ID"]<-"STAID"

#convert the class$RIVMILE to a factor
class$RIVMILE <- as.factor(class$RIVMILE)

#merge class with the data table
data <- merge(data,class,by=c("STAID"))

#housekeeping
rm(class)

################################################################
#clean up data table
################################################################

#pull only columns we need 
#keep <- c("LOCATION","RIVMILE","STAID","LOCAL","DATES","LATDD","LNGDD","PCODE","PSNAM","UNITS","VALUE","REMRK","CLASSIFICA","STANDARD")
keep<-c("LOCATION","RIVMILE","STAID","LOCAL","DATES","TIMES","MEDIM","STYPE","SAMPL","LABNO","PRJCT","ASTAT","LATDD","LNGDD",
        "PCODE","PSNAM","UNITS","VALUE","REMRK","DQIND","ANENT","CLASSIFICA","STANDARD")
data <- data[keep]
rm(keep)

#convert date field to date type
data$DATES <- as.character(data$DATES)
data$DATES <- as.Date(data$DATES,"%Y%m%d")

#create a sample column with LOCATION, RIVMILE, DATES, concatenated
data$sample <- do.call(paste,c(data[c("STAID", "LOCATION","RIVMILE","DATES", "TIMES", "MEDIM", "STYPE")],sep="_"))

#rename parameter field to Parameter.Names
names(data)[names(data)=="PSNAM"] <-"Parameter.Names"

#convert units column to a character
data$UNITS<-as.character(data$UNITS)

################################################################
#check for unit consistency between data and standards tables
################################################################

#merge with data table to pull out parameters from the data table that are in the standards table
check <- merge(data,standardsA,by=c("PCODE", "Parameter.Names"))

#make sure matching units are in the same format so that they are not considered different 
check$UNITS<-as.character(check$UNITS)
check$Units<-as.character(check$Units)
check$UNITS <- tolower(check$UNITS)
check$Units <- tolower(check$Units)
check$Units <- ifelse(check$Units==" ug/l","ug/l",check$Units)
check$check <- ifelse(check$UNITS==check$Units,1,0)

#isolate the records with different units
check<-check[check$check==0,]
check <- unique(check[c("PCODE","Parameter.Names", "UNITS","Units")])

#now print these inconsistencies for the user to see
print(check)
write.csv(check,file=check_output,row.names=FALSE)

#housekeeping
rm(check,check_output)

################################################################
#Converting units when reported different than what is in standards table
################################################################

data$VALUE<-ifelse(data$UNITS=="mg/l as N",(1000*data$VALUE),data$VALUE)
data$UNITS<-ifelse(data$UNITS=="mg/l as N","ug/l",data$UNITS)

#Not working with NH4+. However, this snipit of code will convert if needed
# data$VALUE<-ifelse(data$UNITS=="mg/l NH4",(1000*data$VALUE),data$VALUE)
# data$UNITS<-ifelse(data$UNITS=="mg/l NH4","ug/l",data$UNITS)

data$VALUE<-ifelse(data$UNITS=="mg/l",(1000*data$VALUE),data$VALUE)
data$UNITS<-ifelse(data$UNITS=="mg/l","ug/l",data$UNITS)

data$VALUE<-ifelse(data$UNITS=="ng/l",(0.001*data$VALUE),data$VALUE)
data$UNITS<-ifelse(data$UNITS=="ng/l","ug/l",data$UNITS)

# Output data table w/classifications before comparing to standards
write.csv(data,file=data_output,row.names=FALSE)

################################################################
#reformat table fields with equations
################################################################

standardsA$Aquatic.Chronic.<-as.character(standardsA$Aquatic.Chronic.)
standardsA$Aquatic..Acute.<-as.character(standardsA$Aquatic..Acute.)
standardsB$Aquatic.Chronic.<-as.character(standardsB$Aquatic.Chronic.)
standardsB$Aquatic..Acute.<-as.character(standardsB$Aquatic..Acute.)
standardsC$Aquatic.Chronic.<-as.character(standardsC$Aquatic.Chronic.)
standardsC$Aquatic..Acute.<-as.character(standardsC$Aquatic..Acute.)
standardsD$Aquatic.Chronic.<-as.character(standardsD$Aquatic.Chronic.)
standardsD$Aquatic..Acute.<-as.character(standardsD$Aquatic..Acute.)


################################################################
#subset data by waterbody classification 
################################################################

#preview the class and standards in this data
unique(data[c("CLASSIFICA","STANDARD")])

#create data subset of class A data and distinct samples in classA data
classA=subset(data,data$CLASSIFICA == "A")
samplesA<-unique(classA$sample)
nsamplesA <-length(samplesA)
nA=0
#create df of class B data
classB=subset(data,data$CLASSIFICA == "B")
samplesB<-unique(classB$sample)
nsamplesB <- length(samplesB)
nB=0
#create df of class C data
classC=subset(data,data$CLASSIFICA == "C")
samplesC<-unique(classC$sample)
nsamplesC <- length(samplesC)
nC=0
#create df of class D data
classD=subset(data,data$CLASSIFICA == "D")
samplesD<-unique(classD$sample)
nsamplesD <- length(samplesD)
nD=0

#create list from subsets
data_subset_classes<-list("classA","classB","classC","classD")

################################################################
#populate standards table formulas and compare to data table
################################################################
for(classes in data_subset_classes){
  if (classes=="classA"){
    nsam<-nsamplesA
    standsample <- standardsA
  }
  if (classes=="classB"){
    nsam<-nsamplesB
    standsample <- standardsB
  } 
  if (classes=="classC"){
    nsam<-nsamplesC
    standsample <- standardsC
  } 
  if (classes=="classD"){
    nsam<-nsamplesD
    standsample <- standardsD
  } 
  
  for(i in 1:nsam){
    
    #subset data to just one sample
    if(classes=="classA"){
      sample <- classA[classA$sample == samplesA[i],]
    }
    if(classes=="classB"){
      sample <- classB[classB$sample == samplesB[i],]
    }
    if(classes=="classC"){
      sample <- classC[classC$sample == samplesC[i],]
    }
    if(classes=="classD"){
      sample <- classD[classD$sample == samplesD[i],]
    }
    
    #calculate the sample unique standards that require sample hardness
    if("903" %in% sample$PCODE){
      hardness <- sample$VALUE[sample$PCODE=="903"]
      cadmiumAC <- (0.85)*(exp((0.7852*(log(hardness)))-2.715))
      cadmiumAA <- (0.85)*(exp((1.128*(log(hardness)))-3.6867))
      copperAC <- (0.96)*(exp((0.8545*(log(hardness)))-1.702))
      copperAA <- (0.96)*(exp((0.9422*(log(hardness)))-1.7))
      fluorideAC <- (0.02)*(exp((0.907*(log(hardness)))+7.394))
      leadAC <- (1.46203 -((log(hardness))*(0.145712)))*(exp((1.273*(log(hardness)))-4.297))
      leadAA <-(1.46203-((log(hardness))*(0.145712)))*(exp((1.273*(log(hardness)))-1.052))
      nickelAC<-(0.997)*(exp((0.846*(log(hardness)))+0.0584))
      nickelAA<-(0.998)*(exp((0.846*(log(hardness)))+2.255))
      zincAC<-(exp(0.85*(log(hardness))+0.50))
      zincAA<-0.978*(exp((0.8473*(log(hardness)))+0.884))
      
      #Now replace them in the standards table
      #first the aquatic Chronic
      standardsAC <- c(cadmiumAC,copperAC,fluorideAC,leadAC,nickelAC,zincAC)
      paramsAC<-c("1025","1040","951","1049","1065","1090")
      #paramsAC<-c("Cadmium, wf","Copper, wf","Fluoride, wu","Lead, wf","Nickel, wf","Zinc, wf")
      nstandardsAC <- length(standardsAC)
      for(j in 1:nstandardsAC){
        standsample$Aquatic.Chronic.<-ifelse(standsample$PCODE==paramsAC[j],standardsAC[j],standsample$Aquatic.Chronic.)
      }#<--closes aquatic chronic hardness dependent standards update
      
      #housekeeping
      rm(j,standardsAC,paramsAC,nstandardsAC)
      
      #now for aquatic acute
      standardsAA<-c(cadmiumAA,copperAA,leadAA,nickelAA,zincAA)
      #paramsAA <-c("Cadmium, wf","Copper, wf","Lead, wf","Nickel, wf","Zinc, wf")
      paramsAA<-c("1025","1040","1049","1065","1090")
      nstandardsAA <- length(standardsAA)
      for(j in 1:nstandardsAA){
        standsample$Aquatic..Acute.<-ifelse(standsample$PCODE==paramsAA[j],standardsAA[j],standsample$Aquatic..Acute.)
      }#<--closes aquatic acute hardness dependent standards update
      
      #housekeeping
      rm(j,standardsAA,paramsAA,nstandardsAA)
      rm(hardness,cadmiumAC,cadmiumAA,copperAC,copperAA,fluorideAC,leadAC,leadAA,nickelAC,nickelAA,zincAC,zincAA)
      
    }#<--closes hardness dependent standards update
    
    #compare the values to the standards in this particular sample
    
    #first convert standard fields to numeric field
    standsample$Aquatic.Chronic.<-as.numeric(standsample$Aquatic.Chronic.)
    standsample$Aquatic..Acute.<-as.numeric(standsample$Aquatic..Acute.)
    standsample$Health..Water.Source.<-as.numeric(standsample$Health..Water.Source.)
    standsample$Health.Fish.Consumption.<-as.numeric(standsample$Health.Fish.Consumption.)
    standsample$Wildlife<-as.numeric(standsample$Wildlife, na.strings="")
    standsample$Aesthetic.Water.Source.<-as.numeric(standsample$Aesthetic.Water.Source.)
    standsample$Aesthetic.Food.Source.<-as.numeric(standsample$Aesthetic.Food.Source.)
    standsample$Recreation<-as.numeric(standsample$Recreation)
    
    #merge the standards and sample table and compare values to standards for each sample
    sampleoutput <- merge(sample,standsample,by=c("PCODE", "Parameter.Names"))
    sampleoutput$HWS<-ifelse(is.na(sampleoutput$Health..Water.Source.),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Health..Water.Source.,"V","NV"))
    sampleoutput$HFS<-ifelse(is.na(sampleoutput$Health.Fish.Consumption.),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Health.Fish.Consumption.,"V","NV"))
    sampleoutput$AC<-ifelse(is.na(sampleoutput$Aquatic.Chronic.),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Aquatic.Chronic.,"V","NV"))
    sampleoutput$AA<-ifelse(is.na(sampleoutput$Aquatic..Acute.),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Aquatic..Acute.,"V","NV"))
    sampleoutput$W<-ifelse(is.na(sampleoutput$Wildlife),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Wildlife,"V","NV"))
    sampleoutput$EWS<-ifelse(is.na(sampleoutput$Aesthetic.Water.Source.),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Aesthetic.Water.Source.,"V","NV"))
    sampleoutput$EFS<-ifelse(is.na(sampleoutput$Aesthetic.Food.Source.),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Aesthetic.Food.Source.,"V","NV"))
    sampleoutput$R<-ifelse(is.na(sampleoutput$Recreation),"NS",ifelse(sampleoutput$VALUE>sampleoutput$Recreation,"V","NV"))
    
    #now merge again with the sample table and keep all the extra parameters
    if(analysis_lab=="USGS"){
      sampleoutput <- merge(sampleoutput,sample,by=c("LOCATION","RIVMILE","STAID","LOCAL","DATES","TIMES","MEDIM","STYPE","SAMPL","LABNO","PRJCT","ASTAT","LATDD","LNGDD",
                                                     "PCODE","Parameter.Names", "UNITS","VALUE","REMRK","DQIND","ANENT","CLASSIFICA","STANDARD","sample"),all=TRUE)
    }#<--closes USGS dependent sampleoutput merge assignment
    
    #compare pH, dissolved oxygen, and Ammonia to standards
    if(classes=="classA"){
      
      #compare measured pH to standards
      if("400" %in% sample$PCODE){
        #create variables to compare pH to pH high/low standard values
        pH_value<-sample$VALUE[sample$PCODE=="400"]
        pH_HWS_high_standard<-standsample$Health..Water.Source.[standsample$PCODE=="400.1"]
        pH_HWS_low_standard<-standsample$Health..Water.Source.[standsample$PCODE=="400.2"]
        pH_AC_high_standard<-standsample$Aquatic.Chronic.[standsample$PCODE=="400.1"]
        pH_AC_low_standard<-standsample$Aquatic.Chronic.[standsample$PCODE=="400.2"]
        
        #write the standards values into the output table
        pH_AC_standard<-paste(pH_AC_high_standard,pH_AC_low_standard,sep="-")
        sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="400"]<-pH_AC_standard
        pH_HWS_standard<-paste(pH_HWS_high_standard,pH_HWS_low_standard,sep="-")
        sampleoutput$Health..Water.Source.[sampleoutput$PCODE=="400"]<-pH_HWS_standard
        
        #compare pH to standards and write determination into output table
        sampleoutput$AC[sampleoutput$PCODE=="400"]<-ifelse(is.na(sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="400"]),"NS",
                                                           ifelse(pH_value>pH_AC_high_standard,"V",ifelse(pH_value<pH_AC_low_standard,"V", "NV")))
        
        sampleoutput$HWS[sampleoutput$PCODE=="400"]<-ifelse(is.na(sampleoutput$Health..Water.Source.[sampleoutput$PCODE=="400"]),"NS",
                                                            ifelse(pH_value>pH_HWS_high_standard,"V",ifelse(pH_value<pH_HWS_low_standard, "V", "NV")))
        
        sampleoutput$HFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$AA[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="400"]<-"NS"
      }#<--closes class A pH sample comparison
      
      #compare dissolved oxygen to standards
      if("300" %in% sample$PCODE){
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="A"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(
            sample$VALUE[sample$PCODE=="300"]<diss_oxygen$A,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$A
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$A
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="A-S"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(
            sample$VALUE[sample$PCODE=="300"]<diss_oxygen$A.S,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.S
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.S
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="A(T)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(
            sample$VALUE[sample$PCODE=="300"]<diss_oxygen$A.T.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.T.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.T.
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="A-S(T)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(
            sample$VALUE[sample$PCODE=="300"]<diss_oxygen$A.S.T.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.S.T.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.S.T.
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="A(TS)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(
            sample$VALUE[sample$PCODE=="300"]<diss_oxygen$A.TS.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.TS.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.TS.
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="A-S(TS)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(
            sample$VALUE[sample$PCODE=="300"]<diss_oxygen$A.S.TS.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.S.TS.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$A.S.TS.
        }
        sampleoutput$AA[sampleoutput$PCODE=="300"]<-sampleoutput$AC[sampleoutput$PCODE=="300"]
        sampleoutput$HWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="300"]<-"NS"
      }#<--closes class A dissolved oxygen sample comparison
      
      #compare Ammonia to standards
      if("610" %in% sample$PCODE){
        Ammonia_total<-sample$VALUE[sample$PCODE=="610"]
        
        #first calculate freshwater Un-ionized Ammonia (NH3) from known Ammonia mg/l as N (Total Ammonia Nitrogen (NH3+NH4+ = N))
        #########
        ## UIA = Un-ionized Ammonia (NH3)
        ## f_NH3 = fraction of un-ionized ammonia
        ## pH = potential Hydrogen (standard units;moles per liter)
        ## T = temperature (degrees Celcius)
        #########
        ## (f)NH3 = (1 / (1 + 10^(pK - pH))) 
        ## where pK = 0.09018 + (2729.92/273.2 + T)
        #########
        
        #get pH and temperature values to plug into the formula
        if("400" %in% sample$PCODE){
          pH<-sample$VALUE[sample$PCODE=="400"]
          
          if("10" %in% sample$PCODE){
            water_temp<-sample$VALUE[sample$PCODE=="10"]
            pK=0.09018+(2729.92/(273.2+water_temp))
            f_NH3=(1/(1+(10^(pK-pH))))
            
            #multiply f_NH3 by Ammonia mg/l as N to get the Un-ionized Ammonia (UIA)
            UIA<-f_NH3*Ammonia_total
            
            #compare UIA for class A trout and trout spawning streams to aquatic standards
            if(sample$STANDARD[sampleoutput$PCODE=="610"]=="A(T)" | sample$STANDARD[sampleoutput$PCODE=="610"]=="A(TS)"){
              
              #identify the columns in the standards table to use in the linear interpolation
              #isolate table rows using pH
              r_x1 <- ifelse(pH>=6.5 & pH<6.75, r_x1<-6.5,ifelse(pH>=6.75 & pH<7, r_x1<-6.75,ifelse(pH>=7 & pH<7.25, r_x1<-7, ifelse(
                pH>=7.25 & pH<7.5, r_x1<-7.25,ifelse(pH>=7.5 & pH<7.75, r_x1<-7.5, ifelse(pH>=7.75 & pH<8, r_x1<-7.75, ifelse(pH>=8 & pH<=9, r_x1<-8, "pH out of bounds")))))))
              r_x2 <- ifelse(pH>=6.5 & pH<6.75, r_x2<-6.75,ifelse(pH>=6.75 & pH<7, r_x2<-7,ifelse(pH>=7 & pH<7.25, r_x2<-7.25, ifelse(
                pH>=7.25 & pH<7.5, r_x2<-7.5,ifelse(pH>=7.5 & pH<7.75, r_x2<-7.75, ifelse(pH>=7.75 & pH<8, r_x2<-8, ifelse(pH>=8 & pH<=9, r_x2<-9, "pH out of bounds")))))))
              
              #isolate table columns and standard values using water temperature column and pH row assignment values
              c_y1<- ifelse(water_temp>=0 & water_temp<5, c_y1<-abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y1<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y1<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<=30, c_y1<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1] ,"water temp out of bounds"))))
              
              c_y2<-ifelse(water_temp>=0 & water_temp<5, c_y2<-abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=5 & water_temp<10, c_y2<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=10 & water_temp<15, c_y2<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=15 & water_temp<=30, c_y2<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x2] ,"water temp out of bounds"))))
              
              c_y3<- ifelse(water_temp>=0 & water_temp<5, c_y3<-0, ifelse(water_temp>=5 & water_temp<10, c_y3<-5, ifelse(
                water_temp>=10 & water_temp<15, c_y3<-10, ifelse(water_temp>=15 & water_temp<=30, c_y3<-15 ,"water temp out of bounds"))))
              
              c_y4<-ifelse(water_temp>=0 & water_temp<5, c_y4<-5, ifelse(water_temp>=5 & water_temp<10, c_y4<-10, ifelse(
                water_temp>=10 & water_temp<15, c_y4<-15, ifelse(water_temp>=15 & water_temp<=30, c_y4<-30 ,"water temp out of bounds"))))
              
              c_y5<-ifelse(water_temp>=0 & water_temp<5, c_y5<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y5<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y5<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<=30, c_y5<-abc_T_TS_ammonia$X30.C[abc_T_TS_ammonia$pH==r_x1] ,"water temp out of bounds"))))
              
              #########
              ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
              #########
              ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
              ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
              ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
              ### 
              ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
              ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
              ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
              #########
              
              #interpolate for pH in standards table
              pH_interpol<-c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1))
              temp_interpol<-c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              interpol_standard<-((pH_interpol+temp_interpol)/2)
              sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="610"]<-interpol_standard
              
              #compare UIA to interpolated standard value
              sampleoutput$AC[sampleoutput$PCODE=="610"]<-ifelse(UIA>interpol_standard, "V", ifelse(
                UIA<interpol_standard, "NV", ifelse(is.na(UIA), "UIA error, calculated as NA", ifelse(is.na(interpol_standard), "interpolation error, standard is NA", ifelse(
                  UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
              
            }#<--closes class A trout and trout spawning streams ammonia sample comparison
            
            #compare UIA for class A non-trout and non-trout spawning streams to aquatic standards
            if(sample$STANDARD[sampleoutput$PCODE=="610"]=="A"){
              
              #identify the columns in the standards table to use in the linear interpolation
              #isolate table rows using pH
              r_x1 <- ifelse(pH>=6.5 & pH<6.75, r_x1<-6.5,ifelse(pH>=6.75 & pH<7, r_x1<-6.75,ifelse(pH>=7 & pH<7.25, r_x1<-7, ifelse(
                pH>=7.25 & pH<7.5, r_x1<-7.25,ifelse(pH>=7.5 & pH<7.75, r_x1<-7.5, ifelse(pH>=7.75 & pH<8, r_x1<-7.75, ifelse(pH>=8 & pH<=9, r_x1<-8, "pH out of bounds")))))))
              r_x2 <- ifelse(pH>=6.5 & pH<6.75, r_x2<-6.75,ifelse(pH>=6.75 & pH<7, r_x2<-7,ifelse(pH>=7 & pH<7.25, r_x2<-7.25, ifelse(
                pH>=7.25 & pH<7.5, r_x2<-7.5,ifelse(pH>=7.5 & pH<7.75, r_x2<-7.75, ifelse(pH>=7.75 & pH<8, r_x2<-8, ifelse(pH>=8 & pH<=9, r_x2<-9, "pH out of bounds")))))))
              
              #isolate table columns and standard values using water temperature column and pH row assignment values 
              c_y1<- ifelse(water_temp>=0 & water_temp<5, c_y1<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y1<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y1<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<20, c_y1<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=20 & water_temp<=30, c_y1<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds")))))
              
              c_y2<-ifelse(water_temp>=0 & water_temp<5, c_y2<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=5 & water_temp<10, c_y2<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=10 & water_temp<15, c_y2<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=15 & water_temp<20, c_y2<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                      water_temp>=20 & water_temp<=30, c_y2<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x2] , "water temp out of bounds")))))
              
              c_y3<- ifelse(water_temp>=0 & water_temp<5, c_y3<-0, ifelse(water_temp>=5 & water_temp<10, c_y3<-5, ifelse(
                water_temp>=10 & water_temp<15, c_y3<-10, ifelse(water_temp>=15 & water_temp<20, c_y3<-15, ifelse(
                  water_temp>=20 & water_temp<=30, c_y3<-20 , "water temp out of bounds")))))
              
              c_y4<-ifelse(water_temp>=0 & water_temp<5, c_y4<-5, ifelse(water_temp>=5 & water_temp<10, c_y4<-10, ifelse(
                water_temp>=10 & water_temp<15, c_y4<-15, ifelse(water_temp>15 & water_temp<20, c_y4<-20, ifelse(
                  water_temp>=20 & water_temp<=30, c_y4<-30 , "water temp out of bounds")))))
              
              c_y5<-ifelse(water_temp>=0 & water_temp<5, c_y5<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y5<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y5<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<20, c_y5<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=20 & water_temp<=30, c_y5<-abc_wOut_T_TS_ammonia$X30.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds")))))
              
              #########
              ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
              #########
              ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
              ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
              ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
              ### 
              ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
              ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
              ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
              #########
              
              #interpolate for pH in standards table
              pH_interpol<-c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1))
              temp_interpol<-c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              interpol_standard<-((pH_interpol+temp_interpol)/2)
              sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="610"]<-interpol_standard
              
              #compare UIA to interpolated standard value
              sampleoutput$AC[sampleoutput$PCODE=="610"]<-ifelse(UIA>interpol_standard, "V", ifelse(
                UIA<interpol_standard, "NV", ifelse(is.na(UIA), "UIA error, calculated as NA", ifelse(is.na(interpol_standard), "interpolation error, standard is NA", ifelse(
                  UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
              
            }#<--closes class A non-trout and trout spawning streams ammonia comparison loop
          }#<-closes water temperature value query in the class A ammonia sample comparison
        }#<-closes pH value query in the class A ammonia sample comparison
        
        #perform comparisons for class A HWS against table value
        Ammonia_total_standard <- standardsA$Health..Water.Source.[sampleoutput$PCODE=="610"]
        sampleoutput$HWS[sampleoutput$PCODE=="610"]<-ifelse(Ammonia_total>Ammonia_total_standard, "V", 
                                                            ifelse(Ammonia_total<Ammonia_total_standard, "NV", 
                                                                   ifelse(is.na(Ammonia_total), "Ammonia total NA", 
                                                                          ifelse(is.na(Ammonia_total_standard), "Ammonia HWS standard is NA", "error in HWS comparison"))))
        
        #fill in the fields where there are no standards
        sampleoutput$HFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$AA[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="400"]<-"NS"
        
      }#<--closes ammonia class A sample comparisons
      
      #save this as an output file
      if(nA==0){
        outputA<-sampleoutput
        nA<-nA+1
      }
      if(nA>0){
        outputA<-merge(outputA,sampleoutput,all=TRUE)
      }
    }#<--closes class A subset comparison
    
    if(classes=="classB"){
      
      #compare measured pH to standards
      if("400" %in% sample$PCODE){
        #create variables to compare pH to pH high/low standard values
        pH_value<-sample$VALUE[sample$PCODE=="400"]
        pH_AC_high_standard<-standsample$Aquatic.Chronic.[standsample$PCODE=="400.1"]
        pH_AC_low_standard<-standsample$Aquatic.Chronic.[standsample$PCODE=="400.2"]
        
        #write the standards values into the output table
        pH_AC_standard<-paste(pH_AC_high_standard,pH_AC_low_standard,sep="-")
        sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="400"]<-pH_AC_standard
        
        #compare pH to standards and write determination into output table
        sampleoutput$AC[sampleoutput$PCODE=="400"]<-ifelse(is.na(pH_AC_high_standard),"NS",
                                                           ifelse(pH_value>pH_AC_high_standard,"V",
                                                                  ifelse(pH_value<pH_AC_low_standard, "V", "NV")))
        
        sampleoutput$HWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$AA[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="400"]<-"NS"
      }#<--closes class B pH sample comparison
      
      #compare dissolved oxygen to standards
      if("300" %in% sample$PCODE){
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="B"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(sample$VALUE[sample$PCODE=="300"]<diss_oxygen$B,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$B
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$B
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="B(T)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(sample$VALUE[sample$PCODE=="300"]<diss_oxygen$B.T.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$B.T.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$B.T.
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="B(TS)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(sample$VALUE[sample$PCODE=="300"]<diss_oxygen$B.TS.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$B.TS.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$B.TS.
        }
        sampleoutput$AA[sampleoutput$PCODE=="300"]<-sampleoutput$AC[sampleoutput$PCODE=="300"]
        sampleoutput$HWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="300"]<-"NS"
      }#<--closes class B dissolved oxygen sample comparison
      
      #compare Ammonia to standards
      if("610" %in% sample$PCODE){
        Ammonia_total<-sample$VALUE[sample$PCODE=="610"]
        
        #first calculate freshwater Un-ionized Ammonia (NH3) from known Ammonia mg/l as N (Total Ammonia Nitrogen (NH3+NH4+ = N))
        #########
        ## UIA = Un-ionized Ammonia (NH3)
        ## f_NH3 = fraction of un-ionized ammonia
        ## pH = potential Hydrogen (standard units;moles per liter)
        ## T = temperature (degrees Celcius)
        #########
        ## (f)NH3 = (1 / (1 + 10^(pK - pH))) 
        ## where pK = 0.09018 + (2729.92/273.2 + T)
        #########
        
        #get pH and temperature values to plug into the formula
        if("400" %in% sample$PCODE){
          pH<-sample$VALUE[sample$PCODE=="400"]
          
          if("10" %in% sample$PCODE){
            water_temp<-sample$VALUE[sample$PCODE=="10"]
            pK=0.09018+(2729.92/(273.2+water_temp))
            f_NH3=(1/(1+(10^(pK-pH))))
            
            #multiply f_NH3 by Ammonia mg/l as N to get the Un-ionized Ammonia (UIA)
            UIA<-f_NH3*Ammonia_total
            
            #compare UIA for class B trout and trout spawning streams to aquatic standards
            if(sample$STANDARD[sampleoutput$PCODE=="610"]=="B(T)" | sample$STANDARD[sampleoutput$PCODE=="610"]=="B(TS)"){
              
              #identify the columns in the standards table to use in the linear interpolation
              #isolate table rows using pH
              r_x1 <- ifelse(pH>=6.5 & pH<6.75, r_x1<-6.5,ifelse(pH>=6.75 & pH<7, r_x1<-6.75,ifelse(pH>=7 & pH<7.25, r_x1<-7, ifelse(
                pH>=7.25 & pH<7.5, r_x1<-7.25,ifelse(pH>=7.5 & pH<7.75, r_x1<-7.5, ifelse(pH>=7.75 & pH<8, r_x1<-7.75, ifelse(pH>=8 & pH<=9, r_x1<-8, "pH out of bounds")))))))
              r_x2 <- ifelse(pH>=6.5 & pH<6.75, r_x2<-6.75,ifelse(pH>=6.75 & pH<7, r_x2<-7,ifelse(pH>=7 & pH<7.25, r_x2<-7.25, ifelse(
                pH>=7.25 & pH<7.5, r_x2<-7.5,ifelse(pH>=7.5 & pH<7.75, r_x2<-7.75, ifelse(pH>=7.75 & pH<8, r_x2<-8, ifelse(pH>=8 & pH<=9, r_x2<-9, "pH out of bounds")))))))
              
              #isolate table columns and standard values using water temperature column and pH row assignment values
              c_y1<- ifelse(water_temp>=0 & water_temp<5, c_y1<-abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y1<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y1<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<=30, c_y1<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1] ,"water temp out of bounds"))))
              
              c_y2<-ifelse(water_temp>=0 & water_temp<5, c_y2<-abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=5 & water_temp<10, c_y2<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=10 & water_temp<15, c_y2<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=15 & water_temp<=30, c_y2<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x2] ,"water temp out of bounds"))))
              
              c_y3<- ifelse(water_temp>=0 & water_temp<5, c_y3<-0, ifelse(water_temp>=5 & water_temp<10, c_y3<-5, ifelse(
                water_temp>=10 & water_temp<15, c_y3<-10, ifelse(water_temp>=15 & water_temp<=30, c_y3<-15 ,"water temp out of bounds"))))
              
              c_y4<-ifelse(water_temp>=0 & water_temp<5, c_y4<-5, ifelse(water_temp>=5 & water_temp<10, c_y4<-10, ifelse(
                water_temp>=10 & water_temp<15, c_y4<-15, ifelse(water_temp>=15 & water_temp<=30, c_y4<-30 ,"water temp out of bounds"))))
              
              c_y5<-ifelse(water_temp>=0 & water_temp<5, c_y5<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y5<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y5<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<=30, c_y5<-abc_T_TS_ammonia$X30.C[abc_T_TS_ammonia$pH==r_x1] ,"water temp out of bounds"))))
              
              #########
              ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
              #########
              ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
              ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
              ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
              ### 
              ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
              ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
              ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
              #########
              
              #interpolate for pH in standards table
              pH_interpol<-c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1))
              temp_interpol<-c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              interpol_standard<-((pH_interpol+temp_interpol)/2)
              sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="610"]<-interpol_standard
              
              #compare UIA to interpolated standard value
              sampleoutput$AC[sampleoutput$PCODE=="610"]<-ifelse(UIA>interpol_standard, "V", 
                                                                 ifelse(UIA<interpol_standard, "NV", 
                                                                        ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                               ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                      ifelse(UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
              
            }#<--closes class B trout and trout spawning streams ammonia sample comparison
            
            #compare UIA for class B non-trout and non-trout spawning streams to aquatic standards
            if(sample$STANDARD[sampleoutput$PCODE=="610"]=="B"){
              
              #identify the columns in the standards table to use in the linear interpolation
              #isolate table rows using pH
              r_x1 <- ifelse(pH>=6.5 & pH<6.75, r_x1<-6.5,ifelse(pH>=6.75 & pH<7, r_x1<-6.75,ifelse(pH>=7 & pH<7.25, r_x1<-7, ifelse(
                pH>=7.25 & pH<7.5, r_x1<-7.25,ifelse(pH>=7.5 & pH<7.75, r_x1<-7.5, ifelse(pH>=7.75 & pH<8, r_x1<-7.75, ifelse(pH>=8 & pH<=9, r_x1<-8, "pH out of bounds")))))))
              r_x2 <- ifelse(pH>=6.5 & pH<6.75, r_x2<-6.75,ifelse(pH>=6.75 & pH<7, r_x2<-7,ifelse(pH>=7 & pH<7.25, r_x2<-7.25, ifelse(
                pH>=7.25 & pH<7.5, r_x2<-7.5,ifelse(pH>=7.5 & pH<7.75, r_x2<-7.75, ifelse(pH>=7.75 & pH<8, r_x2<-8, ifelse(pH>=8 & pH<=9, r_x2<-9, "pH out of bounds")))))))
              
              #isolate table columns and standard values using water temperature column and pH row assignment values 
              c_y1<- ifelse(water_temp>=0 & water_temp<5, c_y1<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y1<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y1<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<20, c_y1<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=20 & water_temp<=30, c_y1<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds")))))
              
              c_y2<-ifelse(water_temp>=0 & water_temp<5, c_y2<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=5 & water_temp<10, c_y2<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=10 & water_temp<15, c_y2<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=15 & water_temp<20, c_y2<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                      water_temp>=20 & water_temp<=30, c_y2<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x2] , "water temp out of bounds")))))
              
              c_y3<- ifelse(water_temp>=0 & water_temp<5, c_y3<-0, ifelse(water_temp>=5 & water_temp<10, c_y3<-5, ifelse(
                water_temp>=10 & water_temp<15, c_y3<-10, ifelse(water_temp>=15 & water_temp<20, c_y3<-15, ifelse(
                  water_temp>=20 & water_temp<=30, c_y3<-20 , "water temp out of bounds")))))
              
              c_y4<-ifelse(water_temp>=0 & water_temp<5, c_y4<-5, ifelse(water_temp>=5 & water_temp<10, c_y4<-10, ifelse(
                water_temp>=10 & water_temp<15, c_y4<-15, ifelse(water_temp>15 & water_temp<20, c_y4<-20, ifelse(
                  water_temp>=20 & water_temp<=30, c_y4<-30 , "water temp out of bounds")))))
              
              c_y5<-ifelse(water_temp>=0 & water_temp<5, c_y5<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y5<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y5<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<20, c_y5<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=20 & water_temp<=30, c_y5<-abc_wOut_T_TS_ammonia$X30.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds")))))
              
              #########
              ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
              #########
              ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
              ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
              ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
              ### 
              ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
              ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
              ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
              #########
              
              #interpolate for pH in standards table
              pH_interpol<-c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1))
              temp_interpol<-c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              interpol_standard<-((pH_interpol+temp_interpol)/2)
              sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="610"]<-interpol_standard
              
              #compare UIA to interpolated standard value
              sampleoutput$AC[sampleoutput$PCODE=="610"]<-ifelse(UIA>interpol_standard, "V", 
                                                                 ifelse(UIA<interpol_standard, "NV", 
                                                                        ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                               ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                      ifelse(UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
              
            }#<--closes class B non-trout and non-trout-spawning streams ammonia sample comparison
          }#<-closes water temperature value query in the class B ammonia sample comparison
        }#<-closes pH value query in the class B ammonia sample comparison
        
        #fill in the fields where there are no standards
        sampleoutput$HWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$AA[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="400"]<-"NS"
        
      }#<--closes ammonia class B sample comparisons
      
      #save this as an output file
      if(nB==0){
        outputB<-sampleoutput
        nB<-nB+1
      }
      if(nB>0){
        outputB<-merge(outputB,sampleoutput,all=TRUE)
      }
    }#<--closes class B subset comparisons
    
    if(classes=="classC"){
      
      #compare measured pH to standards
      if("400" %in% sample$PCODE){
        #create variables to compare pH to pH high/low standard values
        pH_value<-sample$VALUE[sample$PCODE=="400"]
        pH_AC_high_standard<-standsample$Aquatic.Chronic.[standsample$PCODE=="400.1"]
        pH_AC_low_standard<-standsample$Aquatic.Chronic.[standsample$PCODE=="400.2"]
        
        #write the standards values into the output table
        pH_AC_standard<-paste(pH_AC_high_standard,pH_AC_low_standard,sep="-")
        sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="400"]<-pH_AC_standard
        
        #compare pH to standards and write determination into output table
        sampleoutput$AC[sampleoutput$PCODE=="400"]<-ifelse(is.na(pH_AC_high_standard),"NS",
                                                           ifelse(pH_value>pH_AC_high_standard,"V",
                                                                  ifelse(pH_value<pH_AC_low_standard, "V", "NV")))
        
        sampleoutput$HWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$AA[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="400"]<-"NS"
      }#<--closes the class C pH sample comparison
      
      #compare dissolved oxygen to standards
      if("300" %in% sample$PCODE){
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="C"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(sample$VALUE[sample$PCODE=="300"]<diss_oxygen$C,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$C
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$C
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="C(T)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(sample$VALUE[sample$PCODE=="300"]<diss_oxygen$C.T.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$C.T.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$C.T.
        }
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="C(TS)"){
          sampleoutput$AC[sampleoutput$PCODE=="300"]<-ifelse(sample$VALUE[sample$PCODE=="300"]<diss_oxygen$C.TS.,"V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="300"]<-diss_oxygen$C.TS.
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$C.TS.
        }
        sampleoutput$AA[sampleoutput$PCODE=="300"]<-sampleoutput$AC[sampleoutput$PCODE=="300"]
        sampleoutput$HWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="300"]<-"NS"
      }#<--closes the class C dissolved oxygen sample comparison
      
      #compare Ammonia to standards
      if("610" %in% sample$PCODE){
        Ammonia_total<-sample$VALUE[sample$PCODE=="610"]
        
        #first calculate freshwater Un-ionized Ammonia (NH3) from known Ammonia mg/l as N (Total Ammonia Nitrogen (NH3+NH4+ = N))
        #########
        ## UIA = Un-ionized Ammonia (NH3)
        ## f_NH3 = fraction of un-ionized ammonia
        ## pH = potential Hydrogen (standard units;moles per liter)
        ## T = temperature (degrees Celcius)
        #########
        ## (f)NH3 = (1 / (1 + 10^(pK - pH))) 
        ## where pK = 0.09018 + (2729.92/273.2 + T)
        #########
        
        #get pH and temperature values to plug into the formula
        if("400" %in% sample$PCODE){
          pH<-sample$VALUE[sample$PCODE=="400"]
          
          if("10" %in% sample$PCODE){
            water_temp<-sample$VALUE[sample$PCODE=="10"]
            pK=0.09018+(2729.92/(273.2+water_temp))
            f_NH3=(1/(1+(10^(pK-pH))))
            
            #multiply f_NH3 by Ammonia mg/l as N to get the Un-ionized Ammonia (UIA)
            UIA<-f_NH3*Ammonia_total
            
            #compare UIA for class C trout and trout spawning streams to aquatic standards
            if(sample$STANDARD[sampleoutput$PCODE=="610"]=="C(T)" | sample$STANDARD[sampleoutput$PCODE=="610"]=="C(TS)"){
              
              #identify the columns in the standards table to use in the linear interpolation
              #isolate table rows using sample pH
              r_x1 <- ifelse(pH>=6.5 & pH<6.75, r_x1<-6.5,ifelse(pH>=6.75 & pH<7, r_x1<-6.75,ifelse(pH>=7 & pH<7.25, r_x1<-7, ifelse(
                pH>=7.25 & pH<7.5, r_x1<-7.25,ifelse(pH>=7.5 & pH<7.75, r_x1<-7.5, ifelse(pH>=7.75 & pH<8, r_x1<-7.75, ifelse(pH>=8 & pH<=9, r_x1<-8, "pH out of bounds")))))))
              r_x2 <- ifelse(pH>=6.5 & pH<6.75, r_x2<-6.75,ifelse(pH>=6.75 & pH<7, r_x2<-7,ifelse(pH>=7 & pH<7.25, r_x2<-7.25, ifelse(
                pH>=7.25 & pH<7.5, r_x2<-7.5,ifelse(pH>=7.5 & pH<7.75, r_x2<-7.75, ifelse(pH>=7.75 & pH<8, r_x2<-8, ifelse(pH>=8 & pH<=9, r_x2<-9, "pH out of bounds")))))))
              
              #isolate table columns and standard values using water temperature column and pH row assignment values
              c_y1<- ifelse(water_temp>=0 & water_temp<5, c_y1<-abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y1<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y1<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<=30, c_y1<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1] ,"water temp out of bounds"))))
              
              c_y2<-ifelse(water_temp>=0 & water_temp<5, c_y2<-abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=5 & water_temp<10, c_y2<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=10 & water_temp<15, c_y2<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=15 & water_temp<=30, c_y2<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x2] ,"water temp out of bounds"))))
              
              c_y3<- ifelse(water_temp>=0 & water_temp<5, c_y3<-0, ifelse(water_temp>=5 & water_temp<10, c_y3<-5, ifelse(
                water_temp>=10 & water_temp<15, c_y3<-10, ifelse(water_temp>=15 & water_temp<=30, c_y3<-15 ,"water temp out of bounds"))))
              
              c_y4<-ifelse(water_temp>=0 & water_temp<5, c_y4<-5, ifelse(water_temp>=5 & water_temp<10, c_y4<-10, ifelse(
                water_temp>=10 & water_temp<15, c_y4<-15, ifelse(water_temp>=15 & water_temp<=30, c_y4<-30 ,"water temp out of bounds"))))
              
              c_y5<-ifelse(water_temp>=0 & water_temp<5, c_y5<-abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y5<-abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y5<-abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<=30, c_y5<-abc_T_TS_ammonia$X30.C[abc_T_TS_ammonia$pH==r_x1] ,"water temp out of bounds"))))
              
              #########
              ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
              #########
              ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
              ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
              ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
              ### 
              ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
              ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
              ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
              #########
              
              #interpolate for pH in standards table
              pH_interpol<-c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1))
              temp_interpol<-c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              interpol_standard<-((pH_interpol+temp_interpol)/2)
              sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="610"]<-interpol_standard
              
              #compare UIA to interpolated standard value
              sampleoutput$AC[sampleoutput$PCODE=="610"]<-ifelse(UIA>interpol_standard, "V", 
                                                                 ifelse(UIA<interpol_standard, "NV", 
                                                                        ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                               ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                      ifelse(UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
              
            }#<--closes class C trout and trout spawning streams ammonia sample comparison
            
            #compare UIA for class C non-trout and non-trout spawning streams to aquatic standards
            if(sample$STANDARD[sampleoutput$PCODE=="610"]=="C"){
              
              #identify the columns in the standards table to use in the linear interpolation
              #isolate table rows using pH
              r_x1 <- ifelse(pH>=6.5 & pH<6.75, r_x1<-6.5,ifelse(pH>=6.75 & pH<7, r_x1<-6.75,ifelse(pH>=7 & pH<7.25, r_x1<-7, ifelse(
                pH>=7.25 & pH<7.5, r_x1<-7.25,ifelse(pH>=7.5 & pH<7.75, r_x1<-7.5, ifelse(pH>=7.75 & pH<8, r_x1<-7.75, ifelse(pH>=8 & pH<=9, r_x1<-8, "pH out of bounds")))))))
              r_x2 <- ifelse(pH>=6.5 & pH<6.75, r_x2<-6.75,ifelse(pH>=6.75 & pH<7, r_x2<-7,ifelse(pH>=7 & pH<7.25, r_x2<-7.25, ifelse(
                pH>=7.25 & pH<7.5, r_x2<-7.5,ifelse(pH>=7.5 & pH<7.75, r_x2<-7.75, ifelse(pH>=7.75 & pH<8, r_x2<-8, ifelse(pH>=8 & pH<=9, r_x2<-9, "pH out of bounds")))))))
              
              #isolate table columns and standard values using water temperature column and pH row assignment values 
              c_y1<- ifelse(water_temp>=0 & water_temp<5, c_y1<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y1<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y1<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<20, c_y1<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=20 & water_temp<=30, c_y1<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds")))))
              
              c_y2<-ifelse(water_temp>=0 & water_temp<5, c_y2<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=5 & water_temp<10, c_y2<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=10 & water_temp<15, c_y2<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=15 & water_temp<20, c_y2<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                      water_temp>=20 & water_temp<=30, c_y2<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x2] , "water temp out of bounds")))))
              
              c_y3<- ifelse(water_temp>=0 & water_temp<5, c_y3<-0, ifelse(water_temp>=5 & water_temp<10, c_y3<-5, ifelse(
                water_temp>=10 & water_temp<15, c_y3<-10, ifelse(water_temp>=15 & water_temp<20, c_y3<-15, ifelse(
                  water_temp>=20 & water_temp<=30, c_y3<-20 , "water temp out of bounds")))))
              
              c_y4<-ifelse(water_temp>=0 & water_temp<5, c_y4<-5, ifelse(water_temp>=5 & water_temp<10, c_y4<-10, ifelse(
                water_temp>=10 & water_temp<15, c_y4<-15, ifelse(water_temp>15 & water_temp<20, c_y4<-20, ifelse(
                  water_temp>=20 & water_temp<=30, c_y4<-30 , "water temp out of bounds")))))
              
              c_y5<-ifelse(water_temp>=0 & water_temp<5, c_y5<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=5 & water_temp<10, c_y5<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=10 & water_temp<15, c_y5<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=15 & water_temp<20, c_y5<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=20 & water_temp<=30, c_y5<-abc_wOut_T_TS_ammonia$X30.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds")))))
              
              #########
              ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
              #########
              ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
              ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
              ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
              ### 
              ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
              ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
              ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
              #########
              
              #interpolate for pH in standards table
              pH_interpol<-c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1))
              temp_interpol<-c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
              interpol_standard<-((pH_interpol+temp_interpol)/2)
              sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="610"]<-interpol_standard
              
              #compare UIA to interpolated standard value
              sampleoutput$AC[sampleoutput$PCODE=="610"]<-ifelse(UIA>interpol_standard, "V", 
                                                                 ifelse(UIA<interpol_standard, "NV", 
                                                                        ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                               ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                      ifelse(UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
              
            }#<--closes class C non-trout and non-trout spawning streams ammonia sample comparison
          }#<-closes water temperature value query in the class C ammonia sample comparison
        }#<-closes pH value query in the class C ammonia sample comparison
        
        #fill in the fields where there are no standards
        sampleoutput$HWS[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$AA[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="610"]<-"NS"
        
      }#<--closes ammonia class C ammonia sample comparison
      
      #save this as an output file
      if(nC==0){
        outputC<-sampleoutput
        nC<-nC+1
      }
      if(nC>0){
        outputC<-merge(outputC,sampleoutput,all=TRUE)
      }
    }#<--closes class C subset comparisons
    
    if(classes=="classD"){
      
      #compare measured pH to standards
      if("400" %in% sample$PCODE){
        #create variables to compare pH to pH high/low standard values
        pH_value<-sample$VALUE[sample$PCODE=="400"]
        pH_AA_high_standard<-standsample$Aquatic..Acute.[standsample$PCODE=="400.1"]
        pH_AA_low_standard<-standsample$Aquatic..Acute.[standsample$PCODE=="400.2"]
        
        #write the standards values into the output table
        pH_AA_standard<-paste(pH_AA_high_standard,pH_AA_low_standard,sep="-")
        sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="400"]<-pH_AA_standard
        
        #compare pH to standards and write determination into output table
        sampleoutput$AA[sampleoutput$PCODE=="400"]<-ifelse(is.na(pH_AA_high_standard),"NS",
                                                           ifelse(pH_value>pH_AA_high_standard,"V",
                                                                  ifelse(pH_value<pH_AA_low_standard, "V", "NV")))
        
        sampleoutput$HWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$AC[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="400"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="400"]<-"NS"
      }#<--closes the pH class D sample comparison
      
      #compare dissolved oxygen to standards
      if("300" %in% sample$PCODE){
        if(sample$STANDARD[sampleoutput$PCODE=="300"]=="D"){
          sampleoutput$AA[sampleoutput$PCODE=="300"]<-ifelse(sample$VALUE[sample$PCODE=="300"]<diss_oxygen$D,"V", "NV")
          sampleoutput$Aquatic..Acute.[sampleoutput$PCODE=="300"]<-diss_oxygen$D
        }
        sampleoutput$AC[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$HWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$HFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="300"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="300"]<-"NS"
      }#--closes the dissolved oxygen class D sample comparison
      
      #compare Ammonia to standards
      if("610" %in% sample$PCODE){
        Ammonia_total<-sample$VALUE[sample$PCODE=="610"]
        
        #first calculate freshwater Un-ionized Ammonia (NH3) from known Ammonia mg/l as N (Total Ammonia Nitrogen (NH3+NH4+ = N))
        #########
        ## UIA = Un-ionized Ammonia (NH3)
        ## f_NH3 = fraction of un-ionized ammonia
        ## pH = potential Hydrogen (standard units;moles per liter)
        ## T = temperature (degrees Celcius)
        #########
        ## (f)NH3 = (1 / (1 + 10^(pK - pH))) 
        ## where pK = 0.09018 + (2729.92/273.2 + T)
        #########
        
        #get pH and temperature values to plug into the formula
        if("400" %in% sample$PCODE){
          pH<-sample$VALUE[sample$PCODE=="400"]
          
          if("10" %in% sample$PCODE){
            water_temp<-sample$VALUE[sample$PCODE=="10"]
            pK=0.09018+(2729.92/(273.2+water_temp))
            f_NH3=(1/(1+(10^(pK-pH))))
            
            #multiply f_NH3 by Ammonia mg/l as N to get the Un-ionized Ammonia (UIA)
            UIA<-f_NH3*Ammonia_total 
            
            #identify the columns in the standards table to use in the linear interpolation
            #isolate table rows using pH
            r_x1 <- ifelse(pH>=6.5 & pH<6.75, r_x1<-6.5,ifelse(pH>=6.75 & pH<7, r_x1<-6.75,ifelse(pH>=7 & pH<7.25, r_x1<-7, ifelse(
              pH>=7.25 & pH<7.5, r_x1<-7.25,ifelse(pH>=7.5 & pH<7.75, r_x1<-7.5, ifelse(pH>=7.75 & pH<8, r_x1<-7.75, ifelse(pH>=8 & pH<=9, r_x1<-8, "pH out of bounds")))))))
            r_x2 <- ifelse(pH>=6.5 & pH<6.75, r_x2<-6.75,ifelse(pH>=6.75 & pH<7, r_x2<-7,ifelse(pH>=7 & pH<7.25, r_x2<-7.25, ifelse(
              pH>=7.25 & pH<7.5, r_x2<-7.5,ifelse(pH>=7.5 & pH<7.75, r_x2<-7.75, ifelse(pH>=7.75 & pH<8, r_x2<-8, ifelse(pH>=8 & pH<=9, r_x2<-9, "pH out of bounds")))))))
            
            #isolate table columns and standard values using water temperature column and pH row assignment values 
            c_y1<- ifelse(water_temp>=0 & water_temp<5, c_y1<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=5 & water_temp<10, c_y1<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=10 & water_temp<15, c_y1<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=15 & water_temp<20, c_y1<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=20 & water_temp<25, c_y1<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=25 & water_temp<=30, c_y1<-abc_wOut_T_TS_ammonia$X25.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds"))))))
            
            c_y2<-ifelse(water_temp>=0 & water_temp<5, c_y2<-abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
              water_temp>=5 & water_temp<10, c_y2<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=10 & water_temp<15, c_y2<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=15 & water_temp<20, c_y2<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=20 & water_temp<25, c_y2<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                      water_temp>=25 & water_temp<=30, c_y2<-abc_wOut_T_TS_ammonia$X25.C[abc_wOut_T_TS_ammonia$pH==r_x2] , "water temp out of bounds"))))))
            
            c_y3<- ifelse(water_temp>=0 & water_temp<5, c_y3<-0, ifelse(water_temp>=5 & water_temp<10, c_y3<-5, ifelse(
              water_temp>=10 & water_temp<15, c_y3<-10, ifelse(water_temp>=15 & water_temp<20, c_y3<-15, ifelse(
                water_temp>=20 & water_temp<25, c_y3<-20, ifelse(water_temp>=25 & water_temp<=30, c_y3<-25  , "water temp out of bounds"))))))
            
            c_y4<-ifelse(water_temp>=0 & water_temp<5, c_y4<-5, ifelse(water_temp>=5 & water_temp<10, c_y4<-10, ifelse(
              water_temp>=10 & water_temp<15, c_y4<-15, ifelse(water_temp>15 & water_temp<20, c_y4<-20, ifelse(
                water_temp>=20 & water_temp<25, c_y4<-25, ifelse(water_temp>=25 & water_temp<=30, c_y4<-30  , "water temp out of bounds"))))))
            
            c_y5<-ifelse(water_temp>=0 & water_temp<5, c_y5<-abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=5 & water_temp<10, c_y5<-abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=10 & water_temp<15, c_y5<-abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=15 & water_temp<20, c_y5<-abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=20 & water_temp<25, c_y5<-abc_wOut_T_TS_ammonia$X25.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                      water_temp>=25 & water_temp<=30, c_y5<-abc_wOut_T_TS_ammonia$X30.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds"))))))
            
            #########
            ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
            #########
            ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) || y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
            ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
            ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
            ### 
            ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) || x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
            ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
            ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
            ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
            #########
            
            #interpolate for pH in standards table
            pH_interpol<-c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1))
            temp_interpol<-c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
            interpol_standard<-((pH_interpol+temp_interpol)/2)
            sampleoutput$Aquatic.Chronic.[sampleoutput$PCODE=="610"]<-interpol_standard
            
            #compare UIA to interpolated standard value
            sampleoutput$AC[sampleoutput$PCODE=="610"]<-ifelse(UIA>interpol_standard, "V", 
                                                               ifelse(UIA<interpol_standard, "NV", 
                                                                      ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                             ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                    ifelse(UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
            
          }#<-closes water temperature value query in the class D ammonia sample comparison
        }#<-closes pH value query in the class D ammonia sample comparison
        
        #fill in the fields where there are no standards
        sampleoutput$HWS[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$AA[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$EWS[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$EFS[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$W[sampleoutput$PCODE=="610"]<-"NS"
        sampleoutput$R[sampleoutput$PCODE=="610"]<-"NS"
        
      }#<--closes ammonia class D sample comparison
      
      #save this as an output file 
      if(nD==0){
        outputD<-sampleoutput
        nD<-nD+1
      }
      if(nD>0){
        outputD<-merge(outputD,sampleoutput,all=TRUE)
      }
    }#<--closes the class D subset comparison
    
  }#<--closes the sample grouping comparison
  
}#<--closes the classes subset comparison

#merge all output tables together for final output
finalOutput<-merge(outputA,outputB,all=TRUE)
finalOutput<-merge(finalOutput,outputC,all=TRUE)
finalOutput<-merge(finalOutput,outputD,all=TRUE)

finalOutput$Health..Water.Source.<-ifelse(is.na(finalOutput$Health..Water.Source.),"NS",finalOutput$Health..Water.Source.)
finalOutput$Health.Fish.Consumption.<-ifelse(is.na(finalOutput$Health.Fish.Consumption.),"NS",finalOutput$Health.Fish.Consumption.)
finalOutput$Aquatic.Chronic.<-ifelse(is.na(finalOutput$Aquatic.Chronic.),"NS",finalOutput$Aquatic.Chronic.)
finalOutput$Aquatic..Acute.<-ifelse(is.na(finalOutput$Aquatic..Acute.),"NS",finalOutput$Aquatic..Acute.)
finalOutput$Wildlife<-ifelse(is.na(finalOutput$Wildlife),"NS",finalOutput$Wildlife)
finalOutput$Aesthetic.Water.Source.<-ifelse(is.na(finalOutput$Aesthetic.Water.Source.),"NS",finalOutput$Aesthetic.Water.Source.)
finalOutput$Aesthetic.Food.Source.<-ifelse(is.na(finalOutput$Aesthetic.Food.Source.),"NS",finalOutput$Aesthetic.Food.Source.)
finalOutput$Recreation<-ifelse(is.na(finalOutput$Recreation),"NS",finalOutput$Recreation)

finalOutput$HWS<-ifelse(is.na(finalOutput$HWS),"NS",ifelse(finalOutput$HWS=="NV","NV",ifelse(finalOutput$HWS=="V","V",ifelse(finalOutput$HWS=="NS","NS","NS"))))
finalOutput$HFS<-ifelse(is.na(finalOutput$HFS),"NS",ifelse(finalOutput$HFS=="NV","NV",ifelse(finalOutput$HFS=="V","V",ifelse(finalOutput$HFS=="NS","NS","NS"))))
finalOutput$AC<-ifelse(is.na(finalOutput$AC),"NS",ifelse(finalOutput$AC=="NV","NV",ifelse(finalOutput$AC=="V","V",ifelse(finalOutput$AC=="NS","NS","NS"))))
finalOutput$AA<-ifelse(is.na(finalOutput$AA),"NS",ifelse(finalOutput$AA=="NV","NV",ifelse(finalOutput$AA=="V","V",ifelse(finalOutput$AA=="NS","NS","NS"))))
finalOutput$W<-ifelse(is.na(finalOutput$W),"NS",ifelse(finalOutput$W=="NV","NV",ifelse(finalOutput$W=="V","V",ifelse(finalOutput$W=="NS","NS","NS"))))
finalOutput$EWS<-ifelse(is.na(finalOutput$EWS),"NS",ifelse(finalOutput$EWS=="NV","NV",ifelse(finalOutput$EWS=="V","V",ifelse(finalOutput$EWS=="NS","NS","NS"))))
finalOutput$EFS<-ifelse(is.na(finalOutput$EFS),"NS",ifelse(finalOutput$EFS=="NV","NV",ifelse(finalOutput$EFS=="V","V",ifelse(finalOutput$EFS=="NS","NS","NS"))))
finalOutput$R<-ifelse(is.na(finalOutput$R),"NS",ifelse(finalOutput$R=="NV","NV",ifelse(finalOutput$R=="V","V",ifelse(finalOutput$R=="NS","NS","NS"))))

#pull only columns we need
keep<-c("LOCATION","RIVMILE","STAID","LOCAL","DATES","TIMES","MEDIM","STYPE","SAMPL","LABNO","PRJCT","ASTAT","LATDD","LNGDD","PCODE","Parameter.Names","UNITS","VALUE","REMRK","DQIND","ANENT","CLASSIFICA","STANDARD",
        "Health..Water.Source.","Health.Fish.Consumption.","Aquatic.Chronic.","Aquatic..Acute.","Wildlife","Aesthetic.Water.Source.","Aesthetic.Food.Source.","Recreation","HWS","HFS","AC","AA","W","EWS","EFS", "R")
finalOutput <- finalOutput[keep]
rm(keep)

#write final output table
write.csv(finalOutput,file=finalOutput_csv,row.names=FALSE)#C:/NYBackup/CStoll.CDrive/R_scripts/standards/outputs/

################################################################
#final table clean up
################################################################

#housekeeping
rm(classA,classB,classC,classD,data_subset_classes,
   outputA,outputB,outputC,outputD,
   standardsA,standardsB,standardsC,standardsD,
   abc_T_TS_ammonia, abc_wOut_T_TS_ammonia, D_wOut_TS_ammonia,
   diss_oxygen, sample, sampleoutput, standsample)

#tidy up
rm(list=ls())