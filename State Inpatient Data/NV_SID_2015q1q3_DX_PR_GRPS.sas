/*******************************************************************            
* Creation Date: 12/01/2017                                                     
*   NV_SID_2015q1q3_DX_PR_GRPS.SAS:                                             
*      THE SAS CODE SHOWN BELOW WILL LOAD THE ASCII                             
*      INPATIENT STAY DX_PR_GRPS FILE INTO SAS                                  
*******************************************************************/            
                                                                                
                                                                                
***************************************************************;                
* ----------------------------------------------------------- *;                
* |  NOTICE: Use of HCUP data constitutes acceptance of the | *;                
* |  terms and conditions of the HCUP Data Use Agreement.   | *;                
* ----------------------------------------------------------- *;                
***************************************************************;                
                                                                                
                                                                                
***********************************************;                                
*  Create SAS informats for missing values     ;                                
***********************************************;                                
PROC FORMAT;                                                                    
  INVALUE N2PF                                                                  
    '-9' = .                                                                    
    '-8' = .A                                                                   
    '-6' = .C                                                                   
    '-5' = .N                                                                   
    OTHER = (|2.|)                                                              
  ;                                                                             
  INVALUE N3PF                                                                  
    '-99' = .                                                                   
    '-88' = .A                                                                  
    '-66' = .C                                                                  
    OTHER = (|3.|)                                                              
  ;                                                                             
  INVALUE N4PF                                                                  
    '-999' = .                                                                  
    '-888' = .A                                                                 
    '-666' = .C                                                                 
    OTHER = (|4.|)                                                              
  ;                                                                             
  INVALUE N4P1F                                                                 
    '-9.9' = .                                                                  
    '-8.8' = .A                                                                 
    '-6.6' = .C                                                                 
    OTHER = (|4.1|)                                                             
  ;                                                                             
  INVALUE N5PF                                                                  
    '-9999' = .                                                                 
    '-8888' = .A                                                                
    '-6666' = .C                                                                
    OTHER = (|5.|)                                                              
  ;                                                                             
  INVALUE N5P2F                                                                 
    '-9.99' = .                                                                 
    '-8.88' = .A                                                                
    '-6.66' = .C                                                                
    OTHER = (|5.2|)                                                             
  ;                                                                             
  INVALUE N6PF                                                                  
    '-99999' = .                                                                
    '-88888' = .A                                                               
    '-66666' = .C                                                               
    OTHER = (|6.|)                                                              
  ;                                                                             
  INVALUE N6P2F                                                                 
    '-99.99' = .                                                                
    '-88.88' = .A                                                               
    '-66.66' = .C                                                               
    OTHER = (|6.2|)                                                             
  ;                                                                             
  INVALUE N7P2F                                                                 
    '-999.99' = .                                                               
    '-888.88' = .A                                                              
    '-666.66' = .C                                                              
    OTHER = (|7.2|)                                                             
  ;                                                                             
  INVALUE N7P4F                                                                 
    '-9.9999' = .                                                               
    '-8.8888' = .A                                                              
    '-6.6666' = .C                                                              
    OTHER = (|7.4|)                                                             
  ;                                                                             
  INVALUE N8PF                                                                  
    '-9999999' = .                                                              
    '-8888888' = .A                                                             
    '-6666666' = .C                                                             
    OTHER = (|8.|)                                                              
  ;                                                                             
  INVALUE N8P2F                                                                 
    '-9999.99' = .                                                              
    '-8888.88' = .A                                                             
    '-6666.66' = .C                                                             
    OTHER = (|8.2|)                                                             
  ;                                                                             
  INVALUE N9PF                                                                  
    '-99999999' = .                                                             
    '-88888888' = .A                                                            
    '-66666666' = .C                                                            
    OTHER = (|9.|)                                                              
  ;                                                                             
  INVALUE N9P2F                                                                 
    '-99999.99' = .                                                             
    '-88888.88' = .A                                                            
    '-66666.66' = .C                                                            
    OTHER = (|9.2|)                                                             
  ;                                                                             
  INVALUE N10PF                                                                 
    '-999999999' = .                                                            
    '-888888888' = .A                                                           
    '-666666666' = .C                                                           
    OTHER = (|10.|)                                                             
  ;                                                                             
  INVALUE N10P4F                                                                
    '-9999.9999' = .                                                            
    '-8888.8888' = .A                                                           
    '-6666.6666' = .C                                                           
    OTHER = (|10.4|)                                                            
  ;                                                                             
  INVALUE N10P5F                                                                
    '-999.99999' = .                                                            
    '-888.88888' = .A                                                           
    '-666.66666' = .C                                                           
    OTHER = (|10.5|)                                                            
  ;                                                                             
  INVALUE DATE10F                                                               
    '-999999999' = .                                                            
    '-888888888' = .A                                                           
    '-666666666' = .C                                                           
    OTHER = (|MMDDYY10.|)                                                       
  ;                                                                             
  INVALUE N11PF                                                                 
    '-9999999999' = .                                                           
    '-8888888888' = .A                                                          
    '-6666666666' = .C                                                          
    OTHER = (|11.|)                                                             
  ;                                                                             
  INVALUE N11P2F                                                                
    '-9999999.99' = .                                                           
    '-8888888.88' = .A                                                          
    '-6666666.66' = .C                                                          
    OTHER = (|11.2|)                                                            
  ;                                                                             
  INVALUE N12P2F                                                                
    '-99999999.99' = .                                                          
    '-88888888.88' = .A                                                         
    '-66666666.66' = .C                                                         
    OTHER = (|12.2|)                                                            
  ;                                                                             
  INVALUE N12P5F                                                                
    '-99999.99999' = .                                                          
    '-88888.88888' = .A                                                         
    '-66666.66666' = .C                                                         
    OTHER = (|12.5|)                                                            
  ;                                                                             
  INVALUE N13PF                                                                 
    '-999999999999' = .                                                         
    '-888888888888' = .A                                                        
    '-666666666666' = .C                                                        
    OTHER = (|13.|)                                                             
  ;                                                                             
  INVALUE N15P2F                                                                
    '-99999999999.99' = .                                                       
    '-88888888888.88' = .A                                                      
    '-66666666666.66' = .C                                                      
    OTHER = (|15.2|)                                                            
  ;                                                                             
  RUN;                                                                          
                                                                                
                                                                                
*******************************;                                                
*  Data Step                  *;                                                
*******************************;                                                
DATA NV_SIDC_2015q1q3_DX_PR_GRPS;                                               
INFILE 'NV_SID_2015q1q3_DX_PR_GRPS.ASC' FIRSTOBS=3 LRECL = 929;                 
                                                                                
*** Variable attribute ***;                                                     
ATTRIB                                                                          
  BODYSYSTEM1                LENGTH=3                                           
  LABEL="Body system 1"                                                         
                                                                                
  BODYSYSTEM2                LENGTH=3                                           
  LABEL="Body system 2"                                                         
                                                                                
  BODYSYSTEM3                LENGTH=3                                           
  LABEL="Body system 3"                                                         
                                                                                
  BODYSYSTEM4                LENGTH=3                                           
  LABEL="Body system 4"                                                         
                                                                                
  BODYSYSTEM5                LENGTH=3                                           
  LABEL="Body system 5"                                                         
                                                                                
  BODYSYSTEM6                LENGTH=3                                           
  LABEL="Body system 6"                                                         
                                                                                
  BODYSYSTEM7                LENGTH=3                                           
  LABEL="Body system 7"                                                         
                                                                                
  BODYSYSTEM8                LENGTH=3                                           
  LABEL="Body system 8"                                                         
                                                                                
  BODYSYSTEM9                LENGTH=3                                           
  LABEL="Body system 9"                                                         
                                                                                
  BODYSYSTEM10               LENGTH=3                                           
  LABEL="Body system 10"                                                        
                                                                                
  BODYSYSTEM11               LENGTH=3                                           
  LABEL="Body system 11"                                                        
                                                                                
  BODYSYSTEM12               LENGTH=3                                           
  LABEL="Body system 12"                                                        
                                                                                
  BODYSYSTEM13               LENGTH=3                                           
  LABEL="Body system 13"                                                        
                                                                                
  BODYSYSTEM14               LENGTH=3                                           
  LABEL="Body system 14"                                                        
                                                                                
  BODYSYSTEM15               LENGTH=3                                           
  LABEL="Body system 15"                                                        
                                                                                
  BODYSYSTEM16               LENGTH=3                                           
  LABEL="Body system 16"                                                        
                                                                                
  BODYSYSTEM17               LENGTH=3                                           
  LABEL="Body system 17"                                                        
                                                                                
  BODYSYSTEM18               LENGTH=3                                           
  LABEL="Body system 18"                                                        
                                                                                
  BODYSYSTEM19               LENGTH=3                                           
  LABEL="Body system 19"                                                        
                                                                                
  BODYSYSTEM20               LENGTH=3                                           
  LABEL="Body system 20"                                                        
                                                                                
  BODYSYSTEM21               LENGTH=3                                           
  LABEL="Body system 21"                                                        
                                                                                
  BODYSYSTEM22               LENGTH=3                                           
  LABEL="Body system 22"                                                        
                                                                                
  BODYSYSTEM23               LENGTH=3                                           
  LABEL="Body system 23"                                                        
                                                                                
  BODYSYSTEM24               LENGTH=3                                           
  LABEL="Body system 24"                                                        
                                                                                
  BODYSYSTEM25               LENGTH=3                                           
  LABEL="Body system 25"                                                        
                                                                                
  BODYSYSTEM26               LENGTH=3                                           
  LABEL="Body system 26"                                                        
                                                                                
  BODYSYSTEM27               LENGTH=3                                           
  LABEL="Body system 27"                                                        
                                                                                
  BODYSYSTEM28               LENGTH=3                                           
  LABEL="Body system 28"                                                        
                                                                                
  BODYSYSTEM29               LENGTH=3                                           
  LABEL="Body system 29"                                                        
                                                                                
  BODYSYSTEM30               LENGTH=3                                           
  LABEL="Body system 30"                                                        
                                                                                
  BODYSYSTEM31               LENGTH=3                                           
  LABEL="Body system 31"                                                        
                                                                                
  BODYSYSTEM32               LENGTH=3                                           
  LABEL="Body system 32"                                                        
                                                                                
  BODYSYSTEM33               LENGTH=3                                           
  LABEL="Body system 33"                                                        
                                                                                
  CHRON1                     LENGTH=3                                           
  LABEL="Chronic condition indicator 1"                                         
                                                                                
  CHRON2                     LENGTH=3                                           
  LABEL="Chronic condition indicator 2"                                         
                                                                                
  CHRON3                     LENGTH=3                                           
  LABEL="Chronic condition indicator 3"                                         
                                                                                
  CHRON4                     LENGTH=3                                           
  LABEL="Chronic condition indicator 4"                                         
                                                                                
  CHRON5                     LENGTH=3                                           
  LABEL="Chronic condition indicator 5"                                         
                                                                                
  CHRON6                     LENGTH=3                                           
  LABEL="Chronic condition indicator 6"                                         
                                                                                
  CHRON7                     LENGTH=3                                           
  LABEL="Chronic condition indicator 7"                                         
                                                                                
  CHRON8                     LENGTH=3                                           
  LABEL="Chronic condition indicator 8"                                         
                                                                                
  CHRON9                     LENGTH=3                                           
  LABEL="Chronic condition indicator 9"                                         
                                                                                
  CHRON10                    LENGTH=3                                           
  LABEL="Chronic condition indicator 10"                                        
                                                                                
  CHRON11                    LENGTH=3                                           
  LABEL="Chronic condition indicator 11"                                        
                                                                                
  CHRON12                    LENGTH=3                                           
  LABEL="Chronic condition indicator 12"                                        
                                                                                
  CHRON13                    LENGTH=3                                           
  LABEL="Chronic condition indicator 13"                                        
                                                                                
  CHRON14                    LENGTH=3                                           
  LABEL="Chronic condition indicator 14"                                        
                                                                                
  CHRON15                    LENGTH=3                                           
  LABEL="Chronic condition indicator 15"                                        
                                                                                
  CHRON16                    LENGTH=3                                           
  LABEL="Chronic condition indicator 16"                                        
                                                                                
  CHRON17                    LENGTH=3                                           
  LABEL="Chronic condition indicator 17"                                        
                                                                                
  CHRON18                    LENGTH=3                                           
  LABEL="Chronic condition indicator 18"                                        
                                                                                
  CHRON19                    LENGTH=3                                           
  LABEL="Chronic condition indicator 19"                                        
                                                                                
  CHRON20                    LENGTH=3                                           
  LABEL="Chronic condition indicator 20"                                        
                                                                                
  CHRON21                    LENGTH=3                                           
  LABEL="Chronic condition indicator 21"                                        
                                                                                
  CHRON22                    LENGTH=3                                           
  LABEL="Chronic condition indicator 22"                                        
                                                                                
  CHRON23                    LENGTH=3                                           
  LABEL="Chronic condition indicator 23"                                        
                                                                                
  CHRON24                    LENGTH=3                                           
  LABEL="Chronic condition indicator 24"                                        
                                                                                
  CHRON25                    LENGTH=3                                           
  LABEL="Chronic condition indicator 25"                                        
                                                                                
  CHRON26                    LENGTH=3                                           
  LABEL="Chronic condition indicator 26"                                        
                                                                                
  CHRON27                    LENGTH=3                                           
  LABEL="Chronic condition indicator 27"                                        
                                                                                
  CHRON28                    LENGTH=3                                           
  LABEL="Chronic condition indicator 28"                                        
                                                                                
  CHRON29                    LENGTH=3                                           
  LABEL="Chronic condition indicator 29"                                        
                                                                                
  CHRON30                    LENGTH=3                                           
  LABEL="Chronic condition indicator 30"                                        
                                                                                
  CHRON31                    LENGTH=3                                           
  LABEL="Chronic condition indicator 31"                                        
                                                                                
  CHRON32                    LENGTH=3                                           
  LABEL="Chronic condition indicator 32"                                        
                                                                                
  CHRON33                    LENGTH=3                                           
  LABEL="Chronic condition indicator 33"                                        
                                                                                
  DXMCCS1                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 1"                                         
                                                                                
  DXMCCS2                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 2"                                         
                                                                                
  DXMCCS3                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 3"                                         
                                                                                
  DXMCCS4                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 4"                                         
                                                                                
  DXMCCS5                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 5"                                         
                                                                                
  DXMCCS6                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 6"                                         
                                                                                
  DXMCCS7                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 7"                                         
                                                                                
  DXMCCS8                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 8"                                         
                                                                                
  DXMCCS9                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 9"                                         
                                                                                
  DXMCCS10                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 10"                                        
                                                                                
  DXMCCS11                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 11"                                        
                                                                                
  DXMCCS12                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 12"                                        
                                                                                
  DXMCCS13                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 13"                                        
                                                                                
  DXMCCS14                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 14"                                        
                                                                                
  DXMCCS15                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 15"                                        
                                                                                
  DXMCCS16                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 16"                                        
                                                                                
  DXMCCS17                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 17"                                        
                                                                                
  DXMCCS18                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 18"                                        
                                                                                
  DXMCCS19                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 19"                                        
                                                                                
  DXMCCS20                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 20"                                        
                                                                                
  DXMCCS21                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 21"                                        
                                                                                
  DXMCCS22                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 22"                                        
                                                                                
  DXMCCS23                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 23"                                        
                                                                                
  DXMCCS24                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 24"                                        
                                                                                
  DXMCCS25                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 25"                                        
                                                                                
  DXMCCS26                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 26"                                        
                                                                                
  DXMCCS27                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 27"                                        
                                                                                
  DXMCCS28                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 28"                                        
                                                                                
  DXMCCS29                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 29"                                        
                                                                                
  DXMCCS30                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 30"                                        
                                                                                
  DXMCCS31                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 31"                                        
                                                                                
  DXMCCS32                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 32"                                        
                                                                                
  DXMCCS33                   LENGTH=$11                                         
  LABEL="Multi-Level CCS:  Diagnosis 33"                                        
                                                                                
  E_MCCS1                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  E Code 1"                                            
                                                                                
  E_MCCS2                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  E Code 2"                                            
                                                                                
  E_MCCS3                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  E Code 3"                                            
                                                                                
  E_MCCS4                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  E Code 4"                                            
                                                                                
  E_MCCS5                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  E Code 5"                                            
                                                                                
  E_MCCS6                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  E Code 6"                                            
                                                                                
  E_MCCS7                    LENGTH=$11                                         
  LABEL="Multi-Level CCS:  E Code 7"                                            
                                                                                
  INJURY                     LENGTH=3                                           
  LABEL=                                                                        
                                                                                
"Injury diagnosis reported on record (1:DX1 is an injury; 2:DX2+ is an injury; 0
:No injury)"                                                                    
                                                                                
  INJURY_CUT                 LENGTH=3                                           
  LABEL="Injury by cutting or piercing (by E codes)"                            
                                                                                
  INJURY_DROWN               LENGTH=3                                           
  LABEL="Injury by drowning or submersion (by E codes)"                         
                                                                                
  INJURY_FALL                LENGTH=3                                           
  LABEL="Injury by falling (by E codes)"                                        
                                                                                
  INJURY_FIRE                LENGTH=3                                           
  LABEL="Injury by fire, flame or hot object (by E codes)"                      
                                                                                
  INJURY_FIREARM             LENGTH=3                                           
  LABEL="Injury by firearm (by E codes)"                                        
                                                                                
  INJURY_MACHINERY           LENGTH=3                                           
  LABEL="Injury by machinery (by E codes)"                                      
                                                                                
  INJURY_MVT                 LENGTH=3                                           
  LABEL="Injury involving motor vehicle traffic (by E codes)"                   
                                                                                
  INJURY_NATURE              LENGTH=3                                           
  LABEL="Injury involving nature or environmental factors (by E codes)"         
                                                                                
  INJURY_POISON              LENGTH=3                                           
  LABEL="Injury by poison (by E codes)"                                         
                                                                                
  INJURY_STRUCK              LENGTH=3                                           
  LABEL="Injury from being struck by or against (by E codes)"                   
                                                                                
  INJURY_SUFFOCATION         LENGTH=3                                           
  LABEL="Injury by suffocation (by E codes)"                                    
                                                                                
  INTENT_ASSAULT             LENGTH=3                                           
  LABEL="Injury by assault indicated on the record (by E codes)"                
                                                                                
  INTENT_SELF_HARM           LENGTH=3                                           
  LABEL=                                                                        
  "Intentional self harm indicated on the record (by diagnosis and/or E codes)" 
                                                                                
  INTENT_UNINTENTIONAL       LENGTH=3                                           
  LABEL="Unintentional injury indicated on the record (by E codes)"             
                                                                                
  KEY                        LENGTH=8                      FORMAT=Z15.          
  LABEL="HCUP record identifier"                                                
                                                                                
  MULTINJURY                 LENGTH=3                                           
  LABEL="More than one injury diagnosis reported on record"                     
                                                                                
  PCLASS1                    LENGTH=3                                           
  LABEL="Procedure class 1"                                                     
                                                                                
  PCLASS2                    LENGTH=3                                           
  LABEL="Procedure class 2"                                                     
                                                                                
  PCLASS3                    LENGTH=3                                           
  LABEL="Procedure class 3"                                                     
                                                                                
  PCLASS4                    LENGTH=3                                           
  LABEL="Procedure class 4"                                                     
                                                                                
  PCLASS5                    LENGTH=3                                           
  LABEL="Procedure class 5"                                                     
                                                                                
  PCLASS6                    LENGTH=3                                           
  LABEL="Procedure class 6"                                                     
                                                                                
  PCLASS7                    LENGTH=3                                           
  LABEL="Procedure class 7"                                                     
                                                                                
  PCLASS8                    LENGTH=3                                           
  LABEL="Procedure class 8"                                                     
                                                                                
  PCLASS9                    LENGTH=3                                           
  LABEL="Procedure class 9"                                                     
                                                                                
  PCLASS10                   LENGTH=3                                           
  LABEL="Procedure class 10"                                                    
                                                                                
  PCLASS11                   LENGTH=3                                           
  LABEL="Procedure class 11"                                                    
                                                                                
  PCLASS12                   LENGTH=3                                           
  LABEL="Procedure class 12"                                                    
                                                                                
  PCLASS13                   LENGTH=3                                           
  LABEL="Procedure class 13"                                                    
                                                                                
  PCLASS14                   LENGTH=3                                           
  LABEL="Procedure class 14"                                                    
                                                                                
  PCLASS15                   LENGTH=3                                           
  LABEL="Procedure class 15"                                                    
                                                                                
  PCLASS16                   LENGTH=3                                           
  LABEL="Procedure class 16"                                                    
                                                                                
  PCLASS17                   LENGTH=3                                           
  LABEL="Procedure class 17"                                                    
                                                                                
  PCLASS18                   LENGTH=3                                           
  LABEL="Procedure class 18"                                                    
                                                                                
  PCLASS19                   LENGTH=3                                           
  LABEL="Procedure class 19"                                                    
                                                                                
  PCLASS20                   LENGTH=3                                           
  LABEL="Procedure class 20"                                                    
                                                                                
  PCLASS21                   LENGTH=3                                           
  LABEL="Procedure class 21"                                                    
                                                                                
  PCLASS22                   LENGTH=3                                           
  LABEL="Procedure class 22"                                                    
                                                                                
  PCLASS23                   LENGTH=3                                           
  LABEL="Procedure class 23"                                                    
                                                                                
  PCLASS24                   LENGTH=3                                           
  LABEL="Procedure class 24"                                                    
                                                                                
  PCLASS25                   LENGTH=3                                           
  LABEL="Procedure class 25"                                                    
                                                                                
  PRMCCS1                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 1"                                         
                                                                                
  PRMCCS2                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 2"                                         
                                                                                
  PRMCCS3                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 3"                                         
                                                                                
  PRMCCS4                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 4"                                         
                                                                                
  PRMCCS5                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 5"                                         
                                                                                
  PRMCCS6                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 6"                                         
                                                                                
  PRMCCS7                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 7"                                         
                                                                                
  PRMCCS8                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 8"                                         
                                                                                
  PRMCCS9                    LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 9"                                         
                                                                                
  PRMCCS10                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 10"                                        
                                                                                
  PRMCCS11                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 11"                                        
                                                                                
  PRMCCS12                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 12"                                        
                                                                                
  PRMCCS13                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 13"                                        
                                                                                
  PRMCCS14                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 14"                                        
                                                                                
  PRMCCS15                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 15"                                        
                                                                                
  PRMCCS16                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 16"                                        
                                                                                
  PRMCCS17                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 17"                                        
                                                                                
  PRMCCS18                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 18"                                        
                                                                                
  PRMCCS19                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 19"                                        
                                                                                
  PRMCCS20                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 20"                                        
                                                                                
  PRMCCS21                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 21"                                        
                                                                                
  PRMCCS22                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 22"                                        
                                                                                
  PRMCCS23                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 23"                                        
                                                                                
  PRMCCS24                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 24"                                        
                                                                                
  PRMCCS25                   LENGTH=$8                                          
  LABEL="Multi-Level CCS:  Procedure 25"                                        
                                                                                
  U_BLOOD                    LENGTH=3                                           
  LABEL="Utilization Flag: Blood"                                               
                                                                                
  U_CATH                     LENGTH=3                                           
  LABEL="Utilization Flag: Cardiac Catheterization Lab"                         
                                                                                
  U_CCU                      LENGTH=3                                           
  LABEL="Utilization Flag: Coronary Care Unit (CCU)"                            
                                                                                
  U_CHESTXRAY                LENGTH=3                                           
  LABEL="Utilization Flag: Chest X-Ray"                                         
                                                                                
  U_CTSCAN                   LENGTH=3                                           
  LABEL="Utilization Flag: Computed Tomography Scan"                            
                                                                                
  U_DIALYSIS                 LENGTH=3                                           
  LABEL="Utilization Flag: Renal Dialysis"                                      
                                                                                
  U_ECHO                     LENGTH=3                                           
  LABEL="Utilization Flag: Echocardiology"                                      
                                                                                
  U_ED                       LENGTH=3                                           
  LABEL="Utilization Flag: Emergency Room"                                      
                                                                                
  U_EEG                      LENGTH=3                                           
  LABEL="Utilization Flag: Electroencephalogram"                                
                                                                                
  U_EKG                      LENGTH=3                                           
  LABEL="Utilization Flag: Electrocardiogram"                                   
                                                                                
  U_EPO                      LENGTH=3                                           
  LABEL="Utilization Flag: EPO"                                                 
                                                                                
  U_ICU                      LENGTH=3                                           
  LABEL="Utilization Flag: Intensive Care Unit (ICU)"                           
                                                                                
  U_LITHOTRIPSY              LENGTH=3                                           
  LABEL="Utilization Flag: Lithotripsy"                                         
                                                                                
  U_MHSA                     LENGTH=3                                           
  LABEL="Utilization Flag: Mental Health and Substance Abuse"                   
                                                                                
  U_MRT                      LENGTH=3                                           
  LABEL="Utilization Flag: Magnetic Resonance Technology"                       
                                                                                
  U_NEWBN2L                  LENGTH=3                                           
  LABEL="Utilization Flag: Nursery Level II"                                    
                                                                                
  U_NEWBN3L                  LENGTH=3                                           
  LABEL="Utilization Flag: Nursery Level III"                                   
                                                                                
  U_NEWBN4L                  LENGTH=3                                           
  LABEL="Utilization Flag: Nursery Level IV"                                    
                                                                                
  U_NUCMED                   LENGTH=3                                           
  LABEL="Utilization Flag: Nuclear Medicine"                                    
                                                                                
  U_OBSERVATION              LENGTH=3                                           
  LABEL="Utilization Flag: Observation Room"                                    
                                                                                
  U_OCCTHERAPY               LENGTH=3                                           
  LABEL="Utilization Flag: Occupational Therapy"                                
                                                                                
  U_ORGANACQ                 LENGTH=3                                           
  LABEL="Utilization Flag: Organ Acquisition"                                   
                                                                                
  U_OTHIMPLANTS              LENGTH=3                                           
  LABEL="Utilization Flag: Other Implants"                                      
                                                                                
  U_PACEMAKER                LENGTH=3                                           
  LABEL="Utilization Flag: Pacemaker"                                           
                                                                                
  U_PHYTHERAPY               LENGTH=3                                           
  LABEL="Utilization Flag: Physical Therapy"                                    
                                                                                
  U_RADTHERAPY               LENGTH=3                                           
  LABEL=                                                                        
  "Utilization Flag: Radiology - Therapeutic and/or Chemotherapy Administration"
                                                                                
  U_RESPTHERAPY              LENGTH=3                                           
  LABEL="Utilization Flag: Respiratory Services"                                
                                                                                
  U_SPEECHTHERAPY            LENGTH=3                                           
  LABEL="Utilization Flag: Speech - Language Pathology"                         
                                                                                
  U_STRESS                   LENGTH=3                                           
  LABEL="Utilization Flag: Cardiac Stress Test"                                 
                                                                                
  U_ULTRASOUND               LENGTH=3                                           
  LABEL="Utilization Flag: Ultrasound"                                          
  ;                                                                             
                                                                                
                                                                                
*** Input the variables from the ASCII file ***;                                
INPUT                                                                           
      @1      BODYSYSTEM1                   N2PF.                               
      @3      BODYSYSTEM2                   N2PF.                               
      @5      BODYSYSTEM3                   N2PF.                               
      @7      BODYSYSTEM4                   N2PF.                               
      @9      BODYSYSTEM5                   N2PF.                               
      @11     BODYSYSTEM6                   N2PF.                               
      @13     BODYSYSTEM7                   N2PF.                               
      @15     BODYSYSTEM8                   N2PF.                               
      @17     BODYSYSTEM9                   N2PF.                               
      @19     BODYSYSTEM10                  N2PF.                               
      @21     BODYSYSTEM11                  N2PF.                               
      @23     BODYSYSTEM12                  N2PF.                               
      @25     BODYSYSTEM13                  N2PF.                               
      @27     BODYSYSTEM14                  N2PF.                               
      @29     BODYSYSTEM15                  N2PF.                               
      @31     BODYSYSTEM16                  N2PF.                               
      @33     BODYSYSTEM17                  N2PF.                               
      @35     BODYSYSTEM18                  N2PF.                               
      @37     BODYSYSTEM19                  N2PF.                               
      @39     BODYSYSTEM20                  N2PF.                               
      @41     BODYSYSTEM21                  N2PF.                               
      @43     BODYSYSTEM22                  N2PF.                               
      @45     BODYSYSTEM23                  N2PF.                               
      @47     BODYSYSTEM24                  N2PF.                               
      @49     BODYSYSTEM25                  N2PF.                               
      @51     BODYSYSTEM26                  N2PF.                               
      @53     BODYSYSTEM27                  N2PF.                               
      @55     BODYSYSTEM28                  N2PF.                               
      @57     BODYSYSTEM29                  N2PF.                               
      @59     BODYSYSTEM30                  N2PF.                               
      @61     BODYSYSTEM31                  N2PF.                               
      @63     BODYSYSTEM32                  N2PF.                               
      @65     BODYSYSTEM33                  N2PF.                               
      @67     CHRON1                        N2PF.                               
      @69     CHRON2                        N2PF.                               
      @71     CHRON3                        N2PF.                               
      @73     CHRON4                        N2PF.                               
      @75     CHRON5                        N2PF.                               
      @77     CHRON6                        N2PF.                               
      @79     CHRON7                        N2PF.                               
      @81     CHRON8                        N2PF.                               
      @83     CHRON9                        N2PF.                               
      @85     CHRON10                       N2PF.                               
      @87     CHRON11                       N2PF.                               
      @89     CHRON12                       N2PF.                               
      @91     CHRON13                       N2PF.                               
      @93     CHRON14                       N2PF.                               
      @95     CHRON15                       N2PF.                               
      @97     CHRON16                       N2PF.                               
      @99     CHRON17                       N2PF.                               
      @101    CHRON18                       N2PF.                               
      @103    CHRON19                       N2PF.                               
      @105    CHRON20                       N2PF.                               
      @107    CHRON21                       N2PF.                               
      @109    CHRON22                       N2PF.                               
      @111    CHRON23                       N2PF.                               
      @113    CHRON24                       N2PF.                               
      @115    CHRON25                       N2PF.                               
      @117    CHRON26                       N2PF.                               
      @119    CHRON27                       N2PF.                               
      @121    CHRON28                       N2PF.                               
      @123    CHRON29                       N2PF.                               
      @125    CHRON30                       N2PF.                               
      @127    CHRON31                       N2PF.                               
      @129    CHRON32                       N2PF.                               
      @131    CHRON33                       N2PF.                               
      @133    DXMCCS1                       $CHAR11.                            
      @144    DXMCCS2                       $CHAR11.                            
      @155    DXMCCS3                       $CHAR11.                            
      @166    DXMCCS4                       $CHAR11.                            
      @177    DXMCCS5                       $CHAR11.                            
      @188    DXMCCS6                       $CHAR11.                            
      @199    DXMCCS7                       $CHAR11.                            
      @210    DXMCCS8                       $CHAR11.                            
      @221    DXMCCS9                       $CHAR11.                            
      @232    DXMCCS10                      $CHAR11.                            
      @243    DXMCCS11                      $CHAR11.                            
      @254    DXMCCS12                      $CHAR11.                            
      @265    DXMCCS13                      $CHAR11.                            
      @276    DXMCCS14                      $CHAR11.                            
      @287    DXMCCS15                      $CHAR11.                            
      @298    DXMCCS16                      $CHAR11.                            
      @309    DXMCCS17                      $CHAR11.                            
      @320    DXMCCS18                      $CHAR11.                            
      @331    DXMCCS19                      $CHAR11.                            
      @342    DXMCCS20                      $CHAR11.                            
      @353    DXMCCS21                      $CHAR11.                            
      @364    DXMCCS22                      $CHAR11.                            
      @375    DXMCCS23                      $CHAR11.                            
      @386    DXMCCS24                      $CHAR11.                            
      @397    DXMCCS25                      $CHAR11.                            
      @408    DXMCCS26                      $CHAR11.                            
      @419    DXMCCS27                      $CHAR11.                            
      @430    DXMCCS28                      $CHAR11.                            
      @441    DXMCCS29                      $CHAR11.                            
      @452    DXMCCS30                      $CHAR11.                            
      @463    DXMCCS31                      $CHAR11.                            
      @474    DXMCCS32                      $CHAR11.                            
      @485    DXMCCS33                      $CHAR11.                            
      @496    E_MCCS1                       $CHAR11.                            
      @507    E_MCCS2                       $CHAR11.                            
      @518    E_MCCS3                       $CHAR11.                            
      @529    E_MCCS4                       $CHAR11.                            
      @540    E_MCCS5                       $CHAR11.                            
      @551    E_MCCS6                       $CHAR11.                            
      @562    E_MCCS7                       $CHAR11.                            
      @573    INJURY                        N2PF.                               
      @575    INJURY_CUT                    N2PF.                               
      @577    INJURY_DROWN                  N2PF.                               
      @579    INJURY_FALL                   N2PF.                               
      @581    INJURY_FIRE                   N2PF.                               
      @583    INJURY_FIREARM                N2PF.                               
      @585    INJURY_MACHINERY              N2PF.                               
      @587    INJURY_MVT                    N2PF.                               
      @589    INJURY_NATURE                 N2PF.                               
      @591    INJURY_POISON                 N2PF.                               
      @593    INJURY_STRUCK                 N2PF.                               
      @595    INJURY_SUFFOCATION            N2PF.                               
      @597    INTENT_ASSAULT                N2PF.                               
      @599    INTENT_SELF_HARM              N2PF.                               
      @601    INTENT_UNINTENTIONAL          N2PF.                               
      @603    KEY                           15.                                 
      @618    MULTINJURY                    N2PF.                               
      @620    PCLASS1                       N2PF.                               
      @622    PCLASS2                       N2PF.                               
      @624    PCLASS3                       N2PF.                               
      @626    PCLASS4                       N2PF.                               
      @628    PCLASS5                       N2PF.                               
      @630    PCLASS6                       N2PF.                               
      @632    PCLASS7                       N2PF.                               
      @634    PCLASS8                       N2PF.                               
      @636    PCLASS9                       N2PF.                               
      @638    PCLASS10                      N2PF.                               
      @640    PCLASS11                      N2PF.                               
      @642    PCLASS12                      N2PF.                               
      @644    PCLASS13                      N2PF.                               
      @646    PCLASS14                      N2PF.                               
      @648    PCLASS15                      N2PF.                               
      @650    PCLASS16                      N2PF.                               
      @652    PCLASS17                      N2PF.                               
      @654    PCLASS18                      N2PF.                               
      @656    PCLASS19                      N2PF.                               
      @658    PCLASS20                      N2PF.                               
      @660    PCLASS21                      N2PF.                               
      @662    PCLASS22                      N2PF.                               
      @664    PCLASS23                      N2PF.                               
      @666    PCLASS24                      N2PF.                               
      @668    PCLASS25                      N2PF.                               
      @670    PRMCCS1                       $CHAR8.                             
      @678    PRMCCS2                       $CHAR8.                             
      @686    PRMCCS3                       $CHAR8.                             
      @694    PRMCCS4                       $CHAR8.                             
      @702    PRMCCS5                       $CHAR8.                             
      @710    PRMCCS6                       $CHAR8.                             
      @718    PRMCCS7                       $CHAR8.                             
      @726    PRMCCS8                       $CHAR8.                             
      @734    PRMCCS9                       $CHAR8.                             
      @742    PRMCCS10                      $CHAR8.                             
      @750    PRMCCS11                      $CHAR8.                             
      @758    PRMCCS12                      $CHAR8.                             
      @766    PRMCCS13                      $CHAR8.                             
      @774    PRMCCS14                      $CHAR8.                             
      @782    PRMCCS15                      $CHAR8.                             
      @790    PRMCCS16                      $CHAR8.                             
      @798    PRMCCS17                      $CHAR8.                             
      @806    PRMCCS18                      $CHAR8.                             
      @814    PRMCCS19                      $CHAR8.                             
      @822    PRMCCS20                      $CHAR8.                             
      @830    PRMCCS21                      $CHAR8.                             
      @838    PRMCCS22                      $CHAR8.                             
      @846    PRMCCS23                      $CHAR8.                             
      @854    PRMCCS24                      $CHAR8.                             
      @862    PRMCCS25                      $CHAR8.                             
      @870    U_BLOOD                       N2PF.                               
      @872    U_CATH                        N2PF.                               
      @874    U_CCU                         N2PF.                               
      @876    U_CHESTXRAY                   N2PF.                               
      @878    U_CTSCAN                      N2PF.                               
      @880    U_DIALYSIS                    N2PF.                               
      @882    U_ECHO                        N2PF.                               
      @884    U_ED                          N2PF.                               
      @886    U_EEG                         N2PF.                               
      @888    U_EKG                         N2PF.                               
      @890    U_EPO                         N2PF.                               
      @892    U_ICU                         N2PF.                               
      @894    U_LITHOTRIPSY                 N2PF.                               
      @896    U_MHSA                        N2PF.                               
      @898    U_MRT                         N2PF.                               
      @900    U_NEWBN2L                     N2PF.                               
      @902    U_NEWBN3L                     N2PF.                               
      @904    U_NEWBN4L                     N2PF.                               
      @906    U_NUCMED                      N2PF.                               
      @908    U_OBSERVATION                 N2PF.                               
      @910    U_OCCTHERAPY                  N2PF.                               
      @912    U_ORGANACQ                    N2PF.                               
      @914    U_OTHIMPLANTS                 N2PF.                               
      @916    U_PACEMAKER                   N2PF.                               
      @918    U_PHYTHERAPY                  N2PF.                               
      @920    U_RADTHERAPY                  N2PF.                               
      @922    U_RESPTHERAPY                 N2PF.                               
      @924    U_SPEECHTHERAPY               N2PF.                               
      @926    U_STRESS                      N2PF.                               
      @928    U_ULTRASOUND                  N2PF.                               
      ;                                                                         
                                                                                
                                                                                
RUN;
