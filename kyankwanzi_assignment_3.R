

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)


# KYANKWANZI


## data of Kyankwanzi Only from NewPlan
Kyankwanzi_Control = read_excel("kyankwanzi_assignment_2.xlsx", 
                                sheet = "Kyankwanzi_NewPlan")

## Number of Interests Control
length(unique(Kyankwanzi_Control$PAP_VALUATION_ASSESSMENT_REF))



## data from Total and Bank
Kyankwanzi_From_Bank = read_excel("kyankwanzi_assignment_2.xlsx", 
                                     sheet = "Kyankwanzi_From_Bank (2)")

## Remove empty rows
Kyankwanzi_From_Bank = 
  Kyankwanzi_From_Bank[!is.na(Kyankwanzi_From_Bank$`PAP VALUATION ASSESSMENT REFS`), ]


## Number of Interests From Bank Without Duplicates
length(unique(Kyankwanzi_From_Bank$`PAP VALUATION ASSESSMENT REFS`))



## names
colnames(Kyankwanzi_Control)
colnames(Kyankwanzi_From_Bank)



# Joining


## left join
Kyankwanzi_joined = left_join(x = Kyankwanzi_Control, 
                           y = Kyankwanzi_From_Bank, 
                           by = c("PAP REF NO" = "EACOP PAP REF NO"))


Unaccounted_1 = anti_join(x = Kyankwanzi_Control, 
                          y = Kyankwanzi_From_Bank, 
                          by = c("PAP REF NO" = "EACOP PAP REF NO"))


Unaccounted_2 = anti_join(x = Kyankwanzi_From_Bank, 
                          y = Kyankwanzi_Control, 
                          by = c("EACOP PAP REF NO" = "PAP REF NO"))



# Compare Interests
colnames(Kyankwanzi_joined)
colnames(Kyankwanzi_joined)[19] = "CENTENARY BANK ACCOUNT NAME(s):"


compare_interests_1 = subset(Kyankwanzi_joined,
                             select = c("PAP REF NO", 
                                        "PAP_NAME",
                                        "PAP NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REFS",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE.x"))  



compare_interests_2 = subset(Kyankwanzi_joined,
                           select = c("PAP REF NO", 
                                      "PAP_NAME",
                                      "PAP NAME",
                                      "PAP_VALUATION_ASSESSMENT_REF",
                                      "PAP VALUATION ASSESSMENT REFS",
                                      "DISTRICT.x",
                                      "SUBCOUNTY",
                                      "VILLAGE.x",
                                      "Type of ID",
                                      "ID Number",
                                      "PAP SPOUSE NAME(s):",
                                      "CENTENARY BANK ACCOUNT NUMBER:",
                                      "CENTENARY BANK ACCOUNT NAME(s):",
                                      "Date Bank Account Opened:",
                                      "SINGLE or JOINT Bank Account:",
          "Did PAP (+ SPOUSE) attend Bank A/C training and FLT? (Y/N)",
                                      "COMMENTS:"))                


# clean data from Mathias BAO
Kyankwanzi_BAO = read_excel("Kyankwanzi Analyzed v5 Nov-23-2021.xlsx", 
                                     sheet = "Kyankwanzi BAO")

## Remove empty rows
Kyankwanzi_BAO = 
  Kyankwanzi_BAO[!is.na(Kyankwanzi_BAO$PAP_name), ]


## Remove Dashes
Kyankwanzi_BAO$PAP_name_No_Dash = gsub(pattern = "-", 
                                       replacement = " ",
                                       x = Kyankwanzi_BAO$PAP_name,
                                       ignore.case = TRUE)

Kyankwanzi_BAO$spouse_name_No_Dash = gsub(pattern = "-", 
                                       replacement = " ",
                                       x = Kyankwanzi_BAO$spouse_name,
                                       ignore.case = TRUE)



# Joining with Mathias
colnames(compare_interests_2)
colnames(Kyankwanzi_BAO)


Kyankwanzi_joined_3 = left_join(x = compare_interests_2, 
                              y = Kyankwanzi_BAO, 
                              by = c("PAP REF NO" = "PAP_RefNo_Corrected"))


colnames(Kyankwanzi_joined_3)


compare_interests_3 = subset(Kyankwanzi_joined_3,
      select = c("PAP REF NO", 
                 "PAP_NAME",
                 "PAP NAME",
                 "PAP_name_No_Dash",
                 "PAP_VALUATION_ASSESSMENT_REF",
                 "PAP VALUATION ASSESSMENT REFS",
                 "DISTRICT.x",
                 "SUBCOUNTY",
                 "VILLAGE.x",
                 "Type of ID",
                 "type_ID", 
                 "ID Number",
                 "ID_NO",
                 "PAPhve_spouse",
                 "NO_spouse",
                 "PAP SPOUSE NAME(s):",
                 "spouse_name_No_Dash",
                 "spouse-IDno",
                 "CENTENARY BANK ACCOUNT NUMBER:",
                 "centenary-bankaccount",
                 "CENTENARY BANK ACCOUNT NAME(s):",
                 "centenary-accountname",
                 "Date Bank Account Opened:",
                 "date-bankaccountopened",
                 "SINGLE or JOINT Bank Account:",
                 "Did PAP (+ SPOUSE) attend Bank A/C training and FLT? (Y/N)",
                 "PAP-spouse-banktraing",
                 "COMMENTS:")) 

write.csv(compare_interests_3, 
          file="compare_interests_3.csv", row.names = FALSE)



#  Joining with Centenary Core Banking System

centenary_core_Jan_25_2022 = read_excel("EACOP Accounts 25-01-2022.xlsx", 
                                        sheet = "Qualified accts ")

colnames(centenary_core_Jan_25_2022)

colnames(centenary_core_Jan_25_2022)[
  which(colnames(centenary_core_Jan_25_2022) == "NATIONAL ID")] = "ID_NO"

centenary_core_needed = subset(centenary_core_Jan_25_2022,
                               select = c("DESCRIPTION",
                                          "ACCOUNT_DESCRIPTION", 
                                          "FIRST_NAME", 
                                          "SURNAME",
                                          "ID_NO",
                                          "ACCOUNT_NUMBER",
                                          "ACCT_TYPE",
                                          "CUST_ID",
                                          "MOBILE_TEL",
                                          "DATE_OF_BIRTH",
                                          "TELEPHONE_1",
                                          "ADDRESS_1"))


str(centenary_core_needed)
centenary_core_needed$ACCOUNT_NUMBER =
  as.character(centenary_core_needed$ACCOUNT_NUMBER)

centenary_core_needed$ACCOUNT_DESCRIPTION =
  as.character(centenary_core_needed$ACCOUNT_DESCRIPTION)


length(which(compare_interests_3$`CENTENARY BANK ACCOUNT NUMBER:` %in% 
               centenary_core_needed$ACCOUNT_NUMBER))

length(which(compare_interests_3$`centenary-bankaccount` %in% 
               centenary_core_needed$ACCOUNT_NUMBER))

length(which(compare_interests_3$`CENTENARY BANK ACCOUNT NAME(s):` %in% 
               centenary_core_needed$ACCOUNT_DESCRIPTION))

length(which(compare_interests_3$`centenary-accountname` %in% 
               centenary_core_needed$ACCOUNT_DESCRIPTION))

length(which(compare_interests_3$PAP_NAME %in% 
               centenary_core_needed$ACCOUNT_DESCRIPTION))

length(unique(compare_interests_3$PAP_NAME))

length(unique(compare_interests_3$PAP_VALUATION_ASSESSMENT_REF))

centenary_core_needed$DESC_1 = 
  paste(centenary_core_needed$FIRST_NAME,
        centenary_core_needed$SURNAME)

centenary_core_needed$DESC_2 = 
  paste(centenary_core_needed$SURNAME,
        centenary_core_needed$FIRST_NAME)

write.csv(centenary_core_needed, 
          file="centenary_core_needed.csv", row.names = FALSE)


colnames(compare_interests_3)[
  which(colnames(compare_interests_3) == "ID_NO")] = "ID_NO_1"


Kyankwanzi_joined_4_left = sqldf("select *
                      from compare_interests_3 a
                      left join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                            method = "raw")


Kyankwanzi_joined_4_inner = sqldf("select *
                      from compare_interests_3 a
                      inner join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                            method = "raw")


Interests_Without_Accounts = sqldf("select * 
             from Kyankwanzi_joined_4_left a
             where  not exists (
             select null from Kyankwanzi_joined_4_inner b
             where  a.PAP_NAME = b.PAP_NAME
             and  a.PAP_VALUATION_ASSESSMENT_REF = 
            b.PAP_VALUATION_ASSESSMENT_REF );", 
                                   method = "raw")

Interests_Without_Accounts = Interests_Without_Accounts %>% 
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Lost_and_Found = Interests_Without_Accounts[
  which(Interests_Without_Accounts$`CENTENARY BANK ACCOUNT NUMBER:` %in%
          centenary_core_needed$ACCOUNT_NUMBER |
          Interests_Without_Accounts$`centenary-bankaccount` %in%
          centenary_core_needed$ACCOUNT_NUMBER |
          (Interests_Without_Accounts$`ID Number` %in%
             centenary_core_needed$ID_NO &
             
             (!(Interests_Without_Accounts$`ID Number` == "N/A" |
                  Interests_Without_Accounts$`ID Number` == "NA" |
                  is.na(Interests_Without_Accounts$`ID Number`)) &
                
                (startsWith(toupper(Interests_Without_Accounts$`ID Number`), 
                            prefix = "CF") |
                   startsWith(toupper(Interests_Without_Accounts$`ID Number`), 
                              prefix = "CM") )) )), ]


write.csv(Lost_and_Found, 
          file="Lost_and_Found.csv", row.names = FALSE)


colnames(Lost_and_Found)[
  which(colnames(Lost_and_Found) == "ID Number")] = "ID_Number"

colnames(Lost_and_Found)[
  which(colnames(Lost_and_Found) == "centenary-bankaccount")] = 
  "centenary_bankaccount"

colnames(Lost_and_Found)[
  which(colnames(Lost_and_Found) == "CENTENARY BANK ACCOUNT NUMBER:")] = 
  "CENTENARY_BANK_ACCOUNT_NUMBER"


## jump this line ..
Lost_and_Found_new = read_excel("Sembabule_Feb_1_2022.xlsx", 
                                sheet = "Lost_and_Found_2")

## Just trying out something ...
Lost_and_Found_new = Lost_and_Found[, 1:28]

colnames(Lost_and_Found_new)[
  which(colnames(Lost_and_Found_new) == "ID Number")] = "ID_Number"

colnames(Lost_and_Found_new)[
  which(colnames(Lost_and_Found_new) == "centenary-bankaccount")] = 
  "centenary_bankaccount"

colnames(Lost_and_Found_new)[
  which(colnames(Lost_and_Found_new) == "CENTENARY BANK ACCOUNT NUMBER:")] = 
  "CENTENARY_BANK_ACCOUNT_NUMBER"


Lost_and_Found_joined = sqldf("SELECT *
             FROM Lost_and_Found_new l
             JOIN centenary_core_needed c
             ON l.CENTENARY_BANK_ACCOUNT_NUMBER = c.ACCOUNT_NUMBER 
                OR l.centenary_bankaccount = c.ACCOUNT_NUMBER
                OR l.ID_Number = c.ID_NO;", 
                              method = "raw")

Lost_and_Found_joined =  Lost_and_Found_joined %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Interests_Without_Accounts = Interests_Without_Accounts[
  -which(Interests_Without_Accounts$`CENTENARY BANK ACCOUNT NUMBER:` %in%
           centenary_core_needed$ACCOUNT_NUMBER |
           Interests_Without_Accounts$`centenary-bankaccount` %in%
           centenary_core_needed$ACCOUNT_NUMBER |
           (Interests_Without_Accounts$`ID Number` %in%
              centenary_core_needed$ID_NO &
              
              (!(Interests_Without_Accounts$`ID Number` == "N/A" |
                   Interests_Without_Accounts$`ID Number` == "NA" |
                   is.na(Interests_Without_Accounts$`ID Number`)) &
                 
                 (startsWith(toupper(Interests_Without_Accounts$`ID Number`), 
                             prefix = "CF") |
                    startsWith(toupper(Interests_Without_Accounts$`ID Number`), 
                               prefix = "CM") )) )), ]

write.csv(Interests_Without_Accounts, 
          file="Interests_Without_Accounts.csv", row.names = FALSE)

write.csv(Lost_and_Found_new, 
          file="Lost_and_Found_new.csv", row.names = FALSE)


Kyankwanzi_joined_4_left = 
  Kyankwanzi_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Kyankwanzi_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
  Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF

which(Kyankwanzi_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
        Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)


check =
  Kyankwanzi_joined_4_left[
    (which(Kyankwanzi_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
             Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


Kyankwanzi_joined_4_left =
  Kyankwanzi_joined_4_left[
    -(which(Kyankwanzi_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
              Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


colnames(Lost_and_Found_joined) = colnames(Kyankwanzi_joined_4_left)

Kyankwanzi_joined_4_left = 
  rbind(Kyankwanzi_joined_4_left,
        Lost_and_Found_joined)

write.csv(Kyankwanzi_joined_4_left, 
          file="Kyankwanzi_joined_4_left.csv", row.names = FALSE)


Kyankwanzi_joined_4_left = 
  Kyankwanzi_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

Kyankwanzi_joined_4 = Kyankwanzi_joined_4_left


colnames(Kyankwanzi_joined_4)[
  which(colnames(Kyankwanzi_joined_4) == 
          "CENTENARY_BANK_ACCOUNT_NAME")] = "CENTENARY BANK ACCOUNT NAME(s):"

colnames(Kyankwanzi_joined_4)[
  which(colnames(Kyankwanzi_joined_4) == 
          "CENTENARY_BANK_ACCOUNT_NUMBER")] = "CENTENARY BANK ACCOUNT NUMBER:"


missed_bank_account_names = subset(Kyankwanzi_joined_4,
      subset = ( (is.na(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
      (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
      (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") ))


missed_bank_account_numbers = subset(Kyankwanzi_joined_4,
    subset = ( (is.na(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`)) |
    (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "N/A") |
    (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "NA") ))


#### ---- checking missing bank account names

missed_bank_account_names = subset(Kyankwanzi_joined_4,
     subset = ( (is.na(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
      (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
      (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") ))


#### ---- only run this code below if it makes sense e.g. "PUBLIC LAND"

Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`[
  which( (is.na(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
           (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] =
  
  Kyankwanzi_joined_4$ACCOUNT_DESCRIPTION[
    which( (is.na(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
             (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
             (Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] 


#### ----- checking whether the bank accounts are correct.

cross_check_bank_accounts = Kyankwanzi_joined_4[
  which(nchar(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`


#### check account numbers in centenary bank system
Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in bank system
Kyankwanzi_joined_4$PAP_NAME[which(
  Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account numbers in Mathias survey data
Kyankwanzi_joined_4$`centenary-bankaccount`[which(
  Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in centenary bank system name description
Kyankwanzi_joined_4$PAP_NAME[which(
  Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )] %in%
  centenary_core_needed$ACCOUNT_DESCRIPTION


### --- Correct Bank Accounts

Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "320365510")] = "3203065510"

Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Kyankwanzi_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "323065754")] = "3203065754"



#  some data from server
data_extra_server = read_excel("EACOP PAP NATIONAL ID.xlsx", 
                               sheet = "Sheet2")


colnames(Kyankwanzi_joined_4)
colnames(data_extra_server)


# Join Again
Kyankwanzi_joined_5 = left_join(x = Kyankwanzi_joined_4, 
                                y = data_extra_server, 
                                by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                         "ASSESSMENT REFERENCE NUMBER"))


colnames(Kyankwanzi_joined_5)

compare_interests_4 = subset(Kyankwanzi_joined_5,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",
                                        "PAP_name_No_Dash", 
                                        "PAP NAME.x",
                                        "PAP_VALUATION_ASSESSMENT_REF",                                      
                                        "DISTRICT.x",                                                         
                                        "SUBCOUNTY",                                                          
                                        "VILLAGE.x",
                                        "ADDRESS_1",
                                        "Type of ID",                                                         
                                        "type_ID",
                                        "ID Number",
                                        "ID_NO_1",
                                        "ID_NO",
                                        "DATE_OF_BIRTH",
                                        "PAPhve_spouse",
                                        "NO_spouse",
                                        "PAP SPOUSE NAME(s):",                                                
                                        "spouse_name_No_Dash",                                                        
                                        "spouse-IDno",                                                        
                                        "CENTENARY BANK ACCOUNT NUMBER:",                                     
                                        "centenary-bankaccount", 
                                        "ACCOUNT_NUMBER",
                                        "ACCT_TYPE",
                                        "CENTENARY BANK ACCOUNT NAME(s):",
                                        "centenary-accountname",
                                        "FIRST_NAME",
                                        "SURNAME",
                                        "CUST_ID",
                                        "ACCOUNT_DESCRIPTION",                                              
                                        "Date Bank Account Opened:", 
                                        "date-bankaccountopened",
                                        "SINGLE or JOINT Bank Account:",                                      
                                        "Did PAP (+ SPOUSE) attend Bank A/C training and FLT? (Y/N)",         
                                        "PAP-spouse-banktraing",                                              
                                        "COMMENTS:",
                                        "MOBILE_TEL",
                                        "TELEPHONE_1")) 


### -- some compromise just to run code ...
which(colnames(Kyankwanzi_joined_5) == "SUBCOUNTY")


compare_interests_4 = Kyankwanzi_joined_5


# check IDs

ids_found_on_server = subset(compare_interests_4,
                             ((compare_interests_4$`ID Number` == "N/A" |
                                 compare_interests_4$`ID Number` == "NA" |
                                 is.na(compare_interests_4$`ID Number`)) &
                                
                                !( compare_interests_4$`ID NUMBER` == "N/A" |
                                     compare_interests_4$`ID NUMBER` == "NA" |
                                     is.na(compare_interests_4$`ID NUMBER`)) &
                                
                                
                                (startsWith(toupper(compare_interests_4$`ID NUMBER`), 
                                            prefix = "CF") |
                                   startsWith(toupper(compare_interests_4$`ID NUMBER`), 
                                              prefix = "CM") )) )


length(which(nchar(ids_found_on_server$`ID NUMBER`) == 14))

length(which(nchar(ids_found_on_server$`ID NUMBER`) != 14))

nchar(ids_found_on_server$`ID NUMBER`)

cross_check_ids_on_server = ids_found_on_server[
  which(nchar(ids_found_on_server$`ID NUMBER`) != 14), ]


### --- Correct IDs

compare_interests_4$`ID NUMBER`[which(
  compare_interests_4$`ID NUMBER` ==
    "CF6018103RAMF")] = "CF76018103RAMF"

compare_interests_4$`ID NUMBER`[which(
  compare_interests_4$`ID NUMBER` ==
    "CM750055104VP1E")] = "CM70055104VP1E"

compare_interests_4$`ID NUMBER`[which(
  compare_interests_4$`ID NUMBER` ==
    "CM7901803AAKC")] = "CM79018103AAKC"

compare_interests_4$`ID NUMBER`[which(
  compare_interests_4$`ID NUMBER` ==
    "CM670181037PD")] = "CM6701810387PD"


### --- Repeat check ....


compare_interests_4$`ID Number`[
  which((compare_interests_4$`ID Number` == "N/A" |
           compare_interests_4$`ID Number` == "NA" |
           is.na(compare_interests_4$`ID Number`)) &
          
          !( compare_interests_4$`ID NUMBER` == "N/A" |
               compare_interests_4$`ID NUMBER` == "NA" |
               is.na(compare_interests_4$`ID NUMBER`)) &
          
          (nchar(compare_interests_4$`ID NUMBER`) == 14) &
          
          (startsWith(toupper(compare_interests_4$`ID NUMBER`), 
                      prefix = "CF") |
             startsWith(toupper(compare_interests_4$`ID NUMBER`), 
                        prefix = "CM") ))]   =
  
  compare_interests_4$`ID NUMBER`[
    which((compare_interests_4$`ID Number` == "N/A" |
             compare_interests_4$`ID Number` == "NA" |
             is.na(compare_interests_4$`ID Number`)) &
            
            !( compare_interests_4$`ID NUMBER` == "N/A" |
                 compare_interests_4$`ID NUMBER` == "NA" |
                 is.na(compare_interests_4$`ID NUMBER`)) &
            
            (nchar(compare_interests_4$`ID NUMBER`) == 14) &
            
            (startsWith(toupper(compare_interests_4$`ID NUMBER`), 
                        prefix = "CF") |
               startsWith(toupper(compare_interests_4$`ID NUMBER`), 
                          prefix = "CM") ))]



ids_found_with_mathias = subset(compare_interests_4,
                                ((compare_interests_4$`ID Number` == "N/A" |
                                    compare_interests_4$`ID Number` == "NA" |
                                    is.na(compare_interests_4$`ID Number`)) &
                                   
                                   !( compare_interests_4$ID_NO == "N/A" |
                                        compare_interests_4$ID_NO == "NA" |
                                        is.na(compare_interests_4$ID_NO)) &
                                   
                                   
                                   (startsWith(toupper(compare_interests_4$ID_NO), 
                                               prefix = "CF") |
                                      startsWith(toupper(compare_interests_4$ID_NO), 
                                                 prefix = "CM") )) )



## -- crosscheck ids from bank

nchar(compare_interests_4$`ID Number`)

cross_check_ids_from_bank = compare_interests_4[
  which(nchar(compare_interests_4$`ID Number`) != 14), ]

### Mistake on ID number CM41081050ENC



write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)

write.csv(cross_check_ids_on_server, 
          file="cross_check_ids_on_server.csv", row.names = FALSE)


colnames(compare_interests_4)

compare_interests_5 = subset(compare_interests_4,
    select = c("PAP REF NO.x",
               "PAP_NAME",                                                           
               "PAP_VALUATION_ASSESSMENT_REF",                                       
               "DISTRICT.x",                                                         
               "SUBCOUNTY",                                                          
               "VILLAGE.x",                                                          
               "ACCOUNT_NUMBER",                                     
               "ACCOUNT_DESCRIPTION",
               "ID_NO",
               "ACCT_TYPE",
               "PAP SPOUSE NAME(s):",
               "spouse-IDno")) 


colnames(compare_interests_5) = 
  c("PAP REF NO",
    "PAP_NAME",                                                           
    "PAP_VALUATION_ASSESSMENT_REF",                                       
    "DISTRICT",                                                         
    "SUBCOUNTY",                                                          
    "VILLAGE",                                                          
    "CENTENARY_BANK_ACCOUNT_NUMBER",                                     
    "ACCOUNT_DESCRIPTION",
    "ID_NO",
    "ACCOUNT_TYPE",
    "SPOUSE_NAME",
    "SPOUSE_ID")


write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)

write.csv(compare_interests_5, 
          file="compare_interests_5.csv", row.names = FALSE)



t = unique(compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER)
t = t[-which(t=="0"|is.na(t))]
## t

unique_names = sapply(t, FUN = function(i){
  shared_accounts = c()
  
  names = compare_interests_5$PAP_NAME[
    which(compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER == i)]
  
  print(length(unique(names)))
  print(unique(names))
  print(i)
  
  if( length(unique(names)) > 1){
    
    shared_accounts = c(shared_accounts, i, unique(names) )
    return(return(shared_accounts))
  }else{ 
    return(NULL)}
  
})



unique_names[["3203151317"]]


Disputed_Accounts = subset(compare_interests_5,
       compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
        "3203151317" )

write.csv(Disputed_Accounts, 
          file="Disputed_Accounts.csv", row.names = FALSE)




########### ---------------------------------------- ################


# SECTION TWO ... NOT FROM SCRATCH ...


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)


compare_interests_5 = read_csv("compare_interests_5.csv")

Disputed_Accounts = 
  subset(compare_interests_5,
         compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
          "3203151317" )


# Unaccounted for accounts in Centenary Core Banking System
centenary_core_needed = read_csv("centenary_core_needed.csV")


Unaccounted_bank_account_numbers = 
        anti_join(x = compare_interests_5, 
                  y = centenary_core_needed, 
                  by = c("CENTENARY_BANK_ACCOUNT_NUMBER" = 
                         "ACCOUNT_NUMBER"))


wrong_account_numbers = subset(Unaccounted_bank_account_numbers,
   (startsWith(Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER, 
                        prefix = "3") |
   startsWith(Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER, 
                        prefix = "32") ))



### add them only if they are mutually exclusive
Unaccounted_bank_account_numbers = rbind(Disputed_Accounts,
                                         Unaccounted_bank_account_numbers)


write.csv(Unaccounted_bank_account_numbers, 
          file="Unaccounted_bank_account_numbers.csv", row.names = FALSE)


write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)

write.csv(compare_interests_5, 
          file="compare_interests_5.csv", row.names = FALSE)




# SPOUSES ....


## Spouses data from Mathias BAO
Spouses_BAO = read_excel("Kyankwanzi Analyzed v5 Nov-23-2021.xlsx", 
                         sheet = "Spouses BAO")


Spouses_BAO$village_No_Dash = gsub(pattern = "-", 
                                   replacement = " ",
                                   x = Spouses_BAO$village,
                                   ignore.case = TRUE)

Spouses_BAO$PAP_name_No_Dash = gsub(pattern = "-", 
                                   replacement = " ",
                                   x = Spouses_BAO$PAP_name,
                                   ignore.case = TRUE)

Spouses_BAO$PAP_name_No_Dash = toupper(Spouses_BAO$PAP_name_No_Dash)
Spouses_BAO$subcounty = toupper(Spouses_BAO$subcounty)
Spouses_BAO$village_No_Dash = toupper(Spouses_BAO$village_No_Dash)


## Spouses data from Control
Spouses_Control = read_excel("Kyankwanzi Analyzed v5 Nov-23-2021.xlsx", 
                             sheet = "Spouses Control")



## join 

Spouses_joined = full_join(x = Spouses_Control, 
                           y = Spouses_BAO, 
                           by = c("PAP REF NO" = "PAP_RefNo_Corrected"))

colnames(Spouses_joined)


Spouses_compared_1 = subset(Spouses_joined,
                            select = c("PAP REF NO",
                                       "PAP_RefNo",
                                       "PAP NAME",
                                       "PAP_name",
                                       "PAP SPOUSE NAME:  (SPOUSE 1)",
                                       "PAP SPOUSE NAME:  (SPOUSE 2)",
                                       "spouse_name_No_Dash",
                                       "spouse-IDno",
                                       "DISTRICT",
                                       "SUBCOUNTY",
                                       "VILLAGE",
                                       "village_No_Dash",
                                       "NO_spouse",
                                       "PAPhve_spouse") )



# join with master list

Spouses_joined_3 = left_join(x = compare_interests_4,
                             y = Spouses_compared_1,
                             by = c("PAP REF NO.x" = "PAP REF NO"))


Unaccounted_spouses_3 = anti_join(x = compare_interests_4,
                                  y = Spouses_compared_1,
                                  by = c("PAP REF NO.x" = "PAP REF NO"))


Unaccounted_spouses_3.2 = anti_join(x = Spouses_compared_1, 
                                    y = compare_interests_4,
                                    by = c("PAP REF NO" = "PAP REF NO.x"))



## Therefore .......

Kyankwanzi_joined_6 = Spouses_joined_3

colnames(Kyankwanzi_joined_6)

compare_interests_6 = subset(Kyankwanzi_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "PAP_RefNo",
                                        "PAP_NAME",
                                        "PAP_name_No_Dash",
                                        "PAP_name",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse_name_No_Dash.x",
                                        "spouse_name_No_Dash.y", 
                                        "spouse-IDno.x",
                                        "spouse-IDno.y",
                                        "DISTRICT.x",
                                        "SUBCOUNTY.x",
                                        "VILLAGE.x",
                                        "VILLAGE",
                                        "village_No_Dash" ,
                                        "PAPhve_spouse.x", 
                                        "PAPhve_spouse.y",
                                        "NO_spouse.x",
                                        "NO_spouse.y",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REFS")  )


write.csv(Kyankwanzi_joined_6, 
          file="Kyankwanzi_joined_6.csv", row.names = FALSE)

write.csv(compare_interests_6, 
          file="compare_interests_6.csv", row.names = FALSE)



## Fill in Spouse ID column ....

missed_spouse_names = subset(compare_interests_6,
          subset = ( (is.na(compare_interests_6$spouse_name_No_Dash.x)) |
          (compare_interests_6$spouse_name_No_Dash.x == "N/A") |
          (compare_interests_6$spouse_name_No_Dash.x == "N A") |
          (compare_interests_6$spouse_name_No_Dash.x == "NA") ))


Kyankwanzi_joined_6$spouse_name_No_Dash.x[
  which( (is.na(Kyankwanzi_joined_6$spouse_name_No_Dash.x)) |
           (Kyankwanzi_joined_6$spouse_name_No_Dash.x == "N/A") |
           (Kyankwanzi_joined_6$spouse_name_No_Dash.x == "N A") |
           (Kyankwanzi_joined_6$spouse_name_No_Dash.x == "NA") )] =
  
  Kyankwanzi_joined_6$spouse_name_No_Dash.y[
    which( (is.na(Kyankwanzi_joined_6$spouse_name_No_Dash.x)) |
             (Kyankwanzi_joined_6$spouse_name_No_Dash.x == "N/A") |
             (Kyankwanzi_joined_6$spouse_name_No_Dash.x == "N A") |
             (Kyankwanzi_joined_6$spouse_name_No_Dash.x == "NA") )] 



missed_spouse_names_main = subset(compare_interests_6,
    subset = ( (is.na(compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
             (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
            (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
            (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") ))


Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`[
  which( (is.na(Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
           (Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
           (Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
           (Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )] =
  
  Kyankwanzi_joined_6$spouse_name_No_Dash.x[
    which( (is.na(Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
             (Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
             (Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
             (Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )]



missed_spouse_IDs = subset(compare_interests_6,
                           subset = ( (is.na(compare_interests_6$`spouse-IDno.x`)) |
                                        (compare_interests_6$`spouse-IDno.x` == "N/A") |
                                        (compare_interests_6$`spouse-IDno.x` == "N A") |
                                        (compare_interests_6$`spouse-IDno.x` == "NA") ))


Kyankwanzi_joined_6$`spouse-IDno.x`[
  which( (is.na(Kyankwanzi_joined_6$`spouse-IDno.x`)) |
           (Kyankwanzi_joined_6$`spouse-IDno.x` == "N/A") |
           (Kyankwanzi_joined_6$`spouse-IDno.x` == "N A") |
           (Kyankwanzi_joined_6$`spouse-IDno.x` == "NA") )] =
  
  Kyankwanzi_joined_6$`spouse-IDno.y`[
    which( (is.na(Kyankwanzi_joined_6$`spouse-IDno.x`)) |
             (Kyankwanzi_joined_6$`spouse-IDno.x` == "N/A") |
             (Kyankwanzi_joined_6$`spouse-IDno.x` == "N A") |
             (Kyankwanzi_joined_6$`spouse-IDno.x` == "NA") )] 



Kyankwanzi_joined_6$NO_spouse.x[
  which( (is.na(Kyankwanzi_joined_6$NO_spouse.x)) |
           (Kyankwanzi_joined_6$NO_spouse.x == "N/A") |
           (Kyankwanzi_joined_6$NO_spouse.x == "N A") |
           (Kyankwanzi_joined_6$NO_spouse.x == "NA") )] =
  
  Kyankwanzi_joined_6$NO_spouse.y[
    which( (is.na(Kyankwanzi_joined_6$NO_spouse.x)) |
             (Kyankwanzi_joined_6$NO_spouse.x == "N/A") |
             (Kyankwanzi_joined_6$NO_spouse.x == "N A") |
             (Kyankwanzi_joined_6$NO_spouse.x == "NA") )] 



Kyankwanzi_joined_6$PAPhve_spouse.x[
  which( (is.na(Kyankwanzi_joined_6$PAPhve_spouse.x)) |
           (Kyankwanzi_joined_6$PAPhve_spouse.x == "N/A") |
           (Kyankwanzi_joined_6$PAPhve_spouse.x == "N A") |
           (Kyankwanzi_joined_6$PAPhve_spouse.x == "NA") )] =
  
  Kyankwanzi_joined_6$PAPhve_spouse.y[
    which( (is.na(Kyankwanzi_joined_6$PAPhve_spouse.x)) |
             (Kyankwanzi_joined_6$PAPhve_spouse.x == "N/A") |
             (Kyankwanzi_joined_6$PAPhve_spouse.x == "N A") |
             (Kyankwanzi_joined_6$PAPhve_spouse.x == "NA") )] 



Kyankwanzi_joined_6$`PAP SPOUSE NAME:  (SPOUSE 2)`[ which(
  (Kyankwanzi_joined_6$PAP_NAME == "KATO ZZIWA DAMIANO") & 
    (Kyankwanzi_joined_6$PAP_VALUATION_ASSESSMENT_REF ==
       "LWE/NDA/KIB/L003-04-00/842") &
    (Kyankwanzi_joined_6$SUBCOUNTY == "NDAGWE") &
    (Kyankwanzi_joined_6$VILLAGE == "KIBANYI") ) ] = 
  "NAMATOVU MOLLY"



compare_interests_6 = subset(Kyankwanzi_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "PAP_RefNo",
                                        "PAP_NAME",
                                        "PAP_name_No_Dash",
                                        "PAP_name",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse_name_No_Dash.x",
                                        "spouse_name_No_Dash.y", 
                                        "spouse-IDno.x",
                                        "spouse-IDno.y",
                                        "DISTRICT.x",
                                        "SUBCOUNTY.x",
                                        "VILLAGE.x",
                                        "VILLAGE",
                                        "village_No_Dash" ,
                                        "PAPhve_spouse.x", 
                                        "PAPhve_spouse.y",
                                        "NO_spouse.x",
                                        "NO_spouse.y",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REFS")  )


compare_interests_7 = subset(Kyankwanzi_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",                                                           
                                        "PAP_VALUATION_ASSESSMENT_REF",                                       
                                        "DISTRICT.x",                                                         
                                        "SUBCOUNTY.x",                                                          
                                        "VILLAGE.x",
                                        "FIRST_NAME",
                                        "SURNAME",
                                        "ID_NO",
                                        "MOBILE_TEL",
                                        "DATE_OF_BIRTH",
                                        "CUST_ID",
                                        "ACCT_TYPE",
                                        "ACCOUNT_DESCRIPTION",
                                        "ACCOUNT_NUMBER",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse-IDno.x"))


colnames(compare_interests_7) = 
  c("EACOP_PAP_REF", 
    "PAP_NAME",                                                           
    "VALUATION_ASSESSMENT",                                       
    "DISTRICT",                                                         
    "SUBCOUNTY",                                                          
    "VILLAGE",                                                          
    "PAP_FIRST_NAME",
    "PAP_SURNAME",
    "NATIONAL_ID",
    "MOBILE_TEL",
    "DATE_OF_BIRTH",
    "CUST_ID",
    "ACCOUNT_TYPE",
    "ACCOUNT_DESCRIPTION",
    "CENTENARY_BANK_ACCOUNT_NUMBER",
    "SPOUSE_NAME_1",
    "SPOUSE_NAME_2",
    "SPOUSE_ID")



# Check spouse IDs ....


nchar(Kyankwanzi_joined_6$`spouse-IDno.x`)

length(which(nchar(compare_interests_6$`spouse-IDno.x`) == 14))

length(which(nchar(compare_interests_6$`spouse-IDno.x`) != 14))

cross_check_ids_spouses = compare_interests_6[
  which(  (nchar(compare_interests_6$`spouse-IDno.x`) != 14) &
            ((startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                         prefix = "CF") |
                startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                           prefix = "CM") ))), ]


### --- Correct IDs

Kyankwanzi_joined_6$`spouse-IDno.x`[which(
  Kyankwanzi_joined_6$`spouse-IDno.x` ==
    "CF8093101C2UA")] = "CF78093101C2UA"

Kyankwanzi_joined_6$`spouse-IDno.x`[which(
  Kyankwanzi_joined_6$`spouse-IDno.x` ==
    "CF9431107VU5H")] = "CF94031107VU5H"


write.csv(cross_check_ids_spouses, 
          file="cross_check_ids_spouses.csv", row.names = FALSE)


write.csv(Kyankwanzi_joined_6, 
          file="Kyankwanzi_joined_6.csv", row.names = FALSE)

write.csv(compare_interests_6, 
          file="compare_interests_6.csv", row.names = FALSE)

write.csv(compare_interests_7, 
          file="compare_interests_7.csv", row.names = FALSE)



# Reloading ... NOT FROM SCRATCH ... edit the spouses 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)

compare_interests_4 = read_csv("compare_interests_4.csv")
compare_interests_5 = read_csv("compare_interests_5.csv")
compare_interests_6 = read_csv("compare_interests_6.csv")
compare_interests_7 = read_csv("compare_interests_7.csv")
Kyankwanzi_joined_6 = read_csv("Kyankwanzi_joined_6.csv")



# DISCLOSURE ...

Kyankwanzi_Disclosure = 
  read_excel("6. Kyankwanzi_Disclosure_Automated Database_08.12.2021.xlsx", 
                                   sheet = "KYANKWANZI")

Unaccounted_disclosure_1 = anti_join(x = Kyankwanzi_joined_6, 
                                     y = Kyankwanzi_Disclosure, 
                                     by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                              "PAP VALUATION ASSESSMENT REF"))

Unaccounted_disclosure_2 = anti_join(x = Kyankwanzi_Disclosure,
                                     y = Kyankwanzi_joined_6,
                                     by = c("PAP VALUATION ASSESSMENT REF" = 
                                              "PAP_VALUATION_ASSESSMENT_REF"))


Kyankwanzi_joined_7 = left_join(x = Kyankwanzi_joined_6, 
                                y = Kyankwanzi_Disclosure, 
                                by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                         "PAP VALUATION ASSESSMENT REF"))


compare_interests_7 = subset(Kyankwanzi_joined_7,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "DISTRICT.x",
                                        "SUBCOUNTY.x",
                                        "VILLAGE.x",
                                        "FIRST_NAME",
                                        "SURNAME",
                                        "ID_NO",
                                        "MOBILE_TEL",
                                        "DATE_OF_BIRTH",
                                        "CUST_ID",
                                        "ACCT_TYPE",
                                        "ACCOUNT_DESCRIPTION",
                                        "ACCOUNT_NUMBER",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse-IDno.x",
                                        "PAPhve_spouse.x",
                                        "NO_spouse.x",
                                        "PRE-DISCLOSURE STATUS",
                                        "PERSON WHO DISCLOSED",
                                        "DATE OF PRE-DISCLOSURE"))



colnames(compare_interests_7) = 
  c("EACOP_PAP_REF",
    "PAP_NAME",
    "VALUATION_ASSESSMENT",
    "DISTRICT",
    "SUBCOUNTY",
    "VILLAGE",
    "PAP_FIRST_NAME",
    "PAP_SURNAME",
    "NATIONAL_ID",
    "MOBILE_TEL",
    "DATE_OF_BIRTH",
    "CUST_ID",
    "ACCOUNT_TYPE",
    "ACCOUNT_DESCRIPTION",
    "CENTENARY_BANK_ACCOUNT_NUMBER",
    "SPOUSE_NAME_1",
    "SPOUSE_NAME_2",
    "SPOUSE_ID",
    "PAPhve_spouse",
    "NO_spouse",
    "PRE-DISCLOSURE STATUS",
    "PERSON WHO DISCLOSED",
    "DATE OF PRE-DISCLOSURE")


write.csv(compare_interests_7, 
          file="compare_interests_7.csv", row.names = FALSE)

write.csv(Kyankwanzi_joined_7, 
          file="Kyankwanzi_joined_7.csv", row.names = FALSE)




# ***********************************************************
## GRIEVANCES
# ***********************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_7 = read_csv("compare_interests_7.csv")
Kyankwanzi_joined_7 = read_csv("Kyankwanzi_joined_7.csv")


Grievances_Kyankwanzi = read_excel("Grievances_Kyankwanzi.xlsx", 
                                sheet = "Kyankwanzi (2)")


Grievances_Kyankwanzi[,c(4:8)] = 
  apply(Grievances_Kyankwanzi[,c(4:8)], 2, toupper)


compare_interests_8_left = left_join(x = Grievances_Kyankwanzi,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                           "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Kyankwanzi a
                              left join compare_interests_7 b
                              on a.'Name of grievant' like '%'||b.PAP_NAME||'%'
                              WHERE 1;", 
                                     method = "raw")


check = subset(compare_interests_8_left, select = c("Name of grievant",
                                                    "PAP_NAME",
                                                    "Village",
                                                    "VILLAGE",
                                                    "VALUATION_ASSESSMENT"))


check2 = subset(compare_interests_8_left_sql, select = c("Name of grievant",
                                                         "PAP_NAME",
                                                         "Village",
                                                         "VILLAGE",
                                                         "VALUATION_ASSESSMENT"))


length(unique(compare_interests_8_left$`Name of grievant`))

length(unique(compare_interests_8_left$PAP_NAME))

length(unique(compare_interests_8_left_sql$`Name of grievant`))

length(unique(compare_interests_8_left_sql$PAP_NAME))


check3 = check2[which(((check2$`Name of grievant` != check2$PAP_NAME) &
                         !(is.null(check2$PAP_NAME))) |
                        ((check2$Village != check2$VILLAGE) &
                           !(is.null(check2$VILLAGE)))), ]



### Correct village names

Grievances_Kyankwanzi$Village[
  which(Grievances_Kyankwanzi$`Name of grievant` == "TWINOMUJUNI JOVANS")
] = "KIKANDWA"


Grievances_Kyankwanzi$Village[
  which(Grievances_Kyankwanzi$`Name of grievant` == "MUGGA GERALD")
] = "KIRYAJOBYO WEST"


## Repeat ....

compare_interests_8_left = left_join(x = Grievances_Kyankwanzi,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Kyankwanzi a
                              left join compare_interests_7 b
                              on a.'Name of grievant' like '%'||b.PAP_NAME||'%'
                              WHERE 1;", 
                                     method = "raw")


check = subset(compare_interests_8_left, select = c("Name of grievant",
                                                    "PAP_NAME",
                                                    "Village",
                                                    "VILLAGE",
                                                    "VALUATION_ASSESSMENT"))


check2 = subset(compare_interests_8_left_sql, select = c("Name of grievant",
                                                         "PAP_NAME",
                                                         "Village",
                                                         "VILLAGE",
                                                         "VALUATION_ASSESSMENT"))


length(unique(compare_interests_8_left$`Name of grievant`))

length(unique(compare_interests_8_left$PAP_NAME))

length(unique(compare_interests_8_left_sql$`Name of grievant`))

length(unique(compare_interests_8_left_sql$PAP_NAME))


check3 = check2[which(((check2$`Name of grievant` != check2$PAP_NAME) &
                         !(is.null(check2$PAP_NAME))) |
                        ((check2$Village != check2$VILLAGE) &
                           !(is.null(check2$VILLAGE)))), ]



### Adding to big list ...

Kyankwanzi_joined_8 = left_join(x = Kyankwanzi_joined_7,
                                y = Grievances_Kyankwanzi,
                                by = c("PAP_NAME" = "Name of grievant",
                                        "VILLAGE.x" = "Village"),
                                     keep = TRUE)


compare_interests_8 = subset(Kyankwanzi_joined_8,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "DISTRICT.x",
                                        "SUBCOUNTY.x",
                                        "VILLAGE.x",
                                        "FIRST_NAME",
                                        "SURNAME",
                                        "ID_NO",
                                        "MOBILE_TEL",
                                        "DATE_OF_BIRTH",
                                        "CUST_ID",
                                        "ACCT_TYPE",
                                        "ACCOUNT_DESCRIPTION",
                                        "ACCOUNT_NUMBER",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse-IDno.x",
                                        "PAPhve_spouse.x",
                                        "NO_spouse.x",
                                        "PRE-DISCLOSURE STATUS",
                                        "PERSON WHO DISCLOSED",
                                        "DATE OF PRE-DISCLOSURE",
                                        "Closed out",
                                        "Date of close out"))



colnames(compare_interests_8) = 
  c("EACOP_PAP_REF",
    "PAP_NAME",
    "VALUATION_ASSESSMENT",
    "DISTRICT",
    "SUBCOUNTY",
    "VILLAGE",
    "PAP_FIRST_NAME",
    "PAP_SURNAME",
    "NATIONAL_ID",
    "MOBILE_TEL",
    "DATE_OF_BIRTH",
    "CUST_ID",
    "ACCOUNT_TYPE",
    "ACCOUNT_DESCRIPTION",
    "CENTENARY_BANK_ACCOUNT_NUMBER",
    "SPOUSE_NAME_1",
    "SPOUSE_NAME_2",
    "SPOUSE_ID",
    "PAPhve_spouse",
    "NO_spouse",
    "PRE-DISCLOSURE STATUS",
    "PERSON WHO DISCLOSED",
    "DATE OF PRE-DISCLOSURE",
    "GRIEVANCE CLOSE OUT",
    "DATE OF GRIEVANCE CLOSE OUT")


write.csv(compare_interests_8, 
          file="compare_interests_8.csv", row.names = FALSE)

write.csv(Kyankwanzi_joined_8, 
          file="Kyankwanzi_joined_8.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_8 = read_csv("compare_interests_8.csv")
Kyankwanzi_joined_8 = read_csv("Kyankwanzi_joined_8.csv")



## **********************************************
## SNAGS
## **********************************************


SNAGS_All = read_excel("Comprehensive Snag List 04-16.03.2022.xlsx", 
                       sheet = "Combined Snags List 2",
                       col_types = c("numeric", "text", "text", 
                                     "text", "text", "text", "date", "text", 
                                     "text", "text"))


## change date formats

SNAGS_All$`Date when snag was handled (If fieldwork was needed)` = 
  as.character(
    SNAGS_All$`Date when snag was handled (If fieldwork was needed)`  )

compare_interests_8$`DATE OF PRE-DISCLOSURE` = 
  as.character( compare_interests_8$`DATE OF PRE-DISCLOSURE`)


compare_interests_8$`DATE OF GRIEVANCE CLOSE OUT` = 
  as.character( compare_interests_8$`DATE OF GRIEVANCE CLOSE OUT`)



unique(SNAGS_All$DISTRICT)

SNAGS_Kyankwanzi = subset(SNAGS_All, SNAGS_All$DISTRICT == "KYANKWANZI")



## checking for unaccounted ...

SNAGS_Unaccounted_1a = anti_join(x = compare_interests_8,
                                 y = SNAGS_Kyankwanzi,
                                 by = c("PAP_NAME" = "NAME OF PAP"))


SNAGS_Unaccounted_1b = sqldf("select * 
             from compare_interests_8 a
             where  not exists (
             select null from SNAGS_Kyankwanzi b
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_1c = sqldf("select * 
             from compare_interests_8 a
             where  not exists (
             select null from SNAGS_Kyankwanzi b
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")

SNAGS_Unaccounted_1b$PAP_NAME[
  which(!(SNAGS_Unaccounted_1b$PAP_NAME %in% SNAGS_Unaccounted_1c$PAP_NAME))]


SNAGS_Unaccounted_2a = anti_join(x = SNAGS_Kyankwanzi,
                                 y = compare_interests_8,
                                 by = c("NAME OF PAP" =
                                          "PAP_NAME" ) )


SNAGS_Unaccounted_2b = sqldf("select * 
             from SNAGS_Kyankwanzi b
             where  not exists (
             select null from compare_interests_8 a
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_2c = sqldf("select * 
             from SNAGS_Kyankwanzi b
             where  not exists (
             select null from compare_interests_8 a
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")


SNAGS_Unaccounted_2b$`NAME OF PAP`[
  which(!(SNAGS_Unaccounted_2b$`NAME OF PAP` %in% 
            SNAGS_Unaccounted_2c$`NAME OF PAP` ))]


### Replace names ...

SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                      "KYA/KIK/015")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KIK/015") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KYA/049")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KYA/049") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/NAK/168")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/NAK/168") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KIR/053")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KIR/053") ) ]


## **********

SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KYA/041")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KYA/041") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/LUW/116")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/LUW/116") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KIR/057")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KIR/057") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KIS-W/169")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KIS-W/169") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KYA/031")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KYA/031") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KYA/037")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KYA/037") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KIR/051")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KIR/051") ) ]


SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KIK/020")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KIK/020") ) ]

SNAGS_Kyankwanzi$`NAME OF PAP`[which(SNAGS_Kyankwanzi$`PAP REF.NO.` ==
                                       "KYA/KIK/017")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KYA/KIK/017") ) ]



### Repeat check ...



### Finally joining ...

SNAGS_Kyankwanzi_joined = left_join(x = compare_interests_8,
                                   y = SNAGS_Kyankwanzi,
                                   by = c("PAP_NAME" = "NAME OF PAP"),
                                   keep = TRUE)


## With Duplicates Removed From SNAGS
SNAGS_Kyankwanzi_joined = SNAGS_Kyankwanzi_joined %>%
  distinct(VALUATION_ASSESSMENT, `SNAG ISSUE`, REMARKS,
           .keep_all = TRUE)


SNAGS_Kyankwanzi_joined$VALUATION_ASSESSMENT[which(
  duplicated(SNAGS_Kyankwanzi_joined$VALUATION_ASSESSMENT)
)]




library(sqldf)
check1 = sqldf("select PAP_NAME, VALUATION_ASSESSMENT, 
          group_concat(REMARKS, '; ') REMARKS, 
          group_concat(`SNAG ISSUE`, '; ') `SNAG ISSUE`
          from SNAGS_Kyankwanzi_joined group by VALUATION_ASSESSMENT", 
               method = "raw")


check2 =
  SNAGS_Kyankwanzi_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarise(REMARKS = toString(REMARKS)) %>%
  ungroup()



library(stringr)
check3 =
  SNAGS_Kyankwanzi_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarize(REMARKS = str_c(REMARKS, collapse = "; "))




### Rename columns

colnames(SNAGS_Kyankwanzi_joined)[
  which(colnames(SNAGS_Kyankwanzi_joined) == 
          "Date when snag was handled (If fieldwork was needed)")] = 
  "DATE_SNAG_Handled"


colnames(SNAGS_Kyankwanzi_joined)[
  which(colnames(SNAGS_Kyankwanzi_joined) == 
          "Was the snag issue closed out  (YES/NO)")] = 
  "SNAG_Status_Close_Out"


compare_interests_10 = sqldf("select 
    EACOP_PAP_REF,
    PAP_NAME,
    VALUATION_ASSESSMENT,
    `DISTRICT.x`,
    SUBCOUNTY,
    VILLAGE,
    PAP_FIRST_NAME,
    PAP_SURNAME,
    NATIONAL_ID,
    MOBILE_TEL,
    DATE_OF_BIRTH,
    CUST_ID,
    ACCOUNT_TYPE,
    ACCOUNT_DESCRIPTION,
    CENTENARY_BANK_ACCOUNT_NUMBER,
    SPOUSE_NAME_1,
    SPOUSE_NAME_2,
    SPOUSE_ID,
    PAPhve_spouse,
    NO_spouse,
    `PRE-DISCLOSURE STATUS`, 
    `PERSON WHO DISCLOSED`,
    `DATE OF PRE-DISCLOSURE`,                                                             
    `GRIEVANCE CLOSE OUT`,                                                                 
    `DATE OF GRIEVANCE CLOSE OUT`,    
    DATE_Snag_Handled,
    SNAG_Status_Close_Out,
    group_concat(REMARKS, '; ') REMARKS, 
    group_concat(`SNAG ISSUE`, '; ') `SNAG ISSUE`
    from SNAGS_Kyankwanzi_joined group by VALUATION_ASSESSMENT", 
                             method = "raw")


colnames(compare_interests_10)[
  which(colnames(compare_interests_10) == 
          "DISTRICT.x")] = "DISTRICT"


write.csv(compare_interests_10, 
          file="compare_interests_10.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_10 = read_csv("compare_interests_10.csv")


## **********************************************
## VULNERABLE PAPs
## **********************************************

Vulnerable_Kyankwanzi = read_excel("Vulnerable PAPs ALL Edited.xlsx", 
                       sheet = "KYANKWANZI")


## checking for unaccounted ...

Vulnerable_Unaccounted_1a = anti_join(x = compare_interests_10,
                                 y = Vulnerable_Kyankwanzi,
                                 by = c("EACOP_PAP_REF" = "EACOP Ref.NUMBER"))


Vulnerable_Unaccounted_1b = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Kyankwanzi b
             where a.EACOP_PAP_REF = b.'EACOP Ref.NUMBER');", 
                             method = "raw")


Vulnerable_Unaccounted_1c = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Kyankwanzi b
             where a.EACOP_PAP_REF like '%'||b.'EACOP Ref.NUMBER'||'%' );", 
                             method = "raw")

Vulnerable_Unaccounted_1b$PAP_NAME[
  which(!(Vulnerable_Unaccounted_1b$PAP_NAME %in% Vulnerable_Unaccounted_1c$PAP_NAME))]


Vulnerable_Unaccounted_2a = anti_join(x = Vulnerable_Kyankwanzi,
                                 y = compare_interests_10,
                                 by = c("EACOP Ref.NUMBER" = "EACOP_PAP_REF") )


Vulnerable_Unaccounted_2b = sqldf("select * 
             from Vulnerable_Kyankwanzi b
             where  not exists (
             select null from compare_interests_10 a
             where  a.EACOP_PAP_REF = b.'EACOP Ref.NUMBER');", 
                             method = "raw")


Vulnerable_Unaccounted_2c = sqldf("select * 
             from Vulnerable_Kyankwanzi b
             where  not exists (
             select null from compare_interests_10 a
             where a.EACOP_PAP_REF like '%'||b.'EACOP Ref.NUMBER'||'%');", 
                             method = "raw")


Vulnerable_Unaccounted_2b$`EACOP Ref.NUMBER`[
  which(!(Vulnerable_Unaccounted_2b$`EACOP Ref.NUMBER` %in% 
            Vulnerable_Unaccounted_2c$`EACOP Ref.NUMBER` ))]


### Finally joining ...

Kyankwanzi_joined_11 = left_join(x = compare_interests_10,
                                    y = Vulnerable_Kyankwanzi,
                                    by = c("EACOP_PAP_REF" = 
                                             "EACOP Ref.NUMBER"),
                                    keep = TRUE)


## With Duplicates Removed From Vulnerable
Kyankwanzi_joined_11 = Kyankwanzi_joined_11 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kyankwanzi_joined_11$VALUATION_ASSESSMENT[which(
  duplicated(Kyankwanzi_joined_11$VALUATION_ASSESSMENT)
)]


check1 = subset(Kyankwanzi_joined_11, 
                select = c("PAP_NAME", 
                           "Name of PAP",
                           "EACOP_PAP_REF",
                           "EACOP Ref.NUMBER"))

colnames(Kyankwanzi_joined_11)[
  which(startsWith(colnames(Kyankwanzi_joined_11), 
                   prefix = "Description of") )] = "Vulnerability"


compare_interests_11 = 
  subset(Kyankwanzi_joined_11, 
  select = colnames(Kyankwanzi_joined_11)[c(1:29,36,37)] )

write.csv(compare_interests_11, 
          file="compare_interests_11.csv", row.names = FALSE)

write.csv(Kyankwanzi_joined_11, 
          file="Kyankwanzi_joined_11.csv", row.names = FALSE)



# UPDATE RECORDS WITH NEW BANK INFORMATION ...

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

All_IWA_inner = read_csv("All_IWA_inner.csv")
compare_interests_11 = read_csv("compare_interests_11.csv")



## Doing it the hard way


### Update 1 ***************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KYA/GAY/KYA/L001-00-00/6485")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KYA/GAY/KYA/L001-00-00/6485")]



### Update 2 **************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$EACOP_PAP_REF == "KYA/LUW/119")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$`PAP REF NO` ==  "KYA/LUW/119")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$EACOP_PAP_REF == 
          "KYA/LUW/119")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$`PAP REF NO` ==
            "KYA/LUW/119")]


## save update
write.csv(compare_interests_11, 
          file="compare_interests_11.csv", row.names = FALSE)



## The following code failed to be deployed successfully

colnames(compare_interests_11)

colnames(All_IWA_inner)

### rename columns ...

colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == "DISTRICT.x")] =
  "DISTRICT"

colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "PAP REF NO")] = "EACOP_PAP_REF"

colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
              "PAP_VALUATION_ASSESSMENT_REF")] = "VALUATION_ASSESSMENT"

colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "ACCT_TYPE")] = 
  "ACCOUNT_TYPE"


colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "ACCOUNT_NUMBER")] = 
  "CENTENARY_BANK_ACCOUNT_NUMBER"


colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "ID_NO")] = 
  "NATIONAL_ID"


colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "spouse_name_No_Dash")] = 
  "SPOUSE_NAME_1"

colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "spouse-IDno")] = 
  "SPOUSE_ID"

colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "FIRST_NAME")] = 
  "PAP_FIRST_NAME"

colnames(All_IWA_inner)[which(colnames(All_IWA_inner) == 
                                "SURNAME")] = 
  "PAP_SURNAME"


yy = colnames(All_IWA_inner)[
  which( colnames(All_IWA_inner) %in% colnames(compare_interests_11)) ]


All_IWA_inner$PAP_NAME[which(All_IWA_inner$EACOP_PAP_REF %in%
                               compare_interests_11$EACOP_PAP_REF)]

All_IWA_inner[["PAP_NAME"]][which(All_IWA_inner$EACOP_PAP_REF %in%
                               compare_interests_11$EACOP_PAP_REF)]


compare_interests_11$PAP_NAME[
  which( compare_interests_11$EACOP_PAP_REF %in%
            All_IWA_inner$EACOP_PAP_REF ) ]

compare_interests_11[["PAP_NAME"]][
  which( compare_interests_11$EACOP_PAP_REF %in%
           All_IWA_inner$EACOP_PAP_REF ) ]



All_IWA_inner$PAP_NAME[which( (All_IWA_inner$EACOP_PAP_REF %in%
                               compare_interests_11$EACOP_PAP_REF) &
                               
                               (All_IWA_inner$VALUATION_ASSESSMENT %in%
                                  compare_interests_11$VALUATION_ASSESSMENT) )]


compare_interests_11$PAP_NAME[
  which( (compare_interests_11$EACOP_PAP_REF %in%
           All_IWA_inner$EACOP_PAP_REF ) &
           
           (compare_interests_11$VALUATION_ASSESSMENT %in%
              All_IWA_inner$VALUATION_ASSESSMENT) )]



All_IWA_inner$EACOP_PAP_REF[which( (All_IWA_inner$EACOP_PAP_REF %in%
                                 compare_interests_11$EACOP_PAP_REF) &
                                
                                (All_IWA_inner$VALUATION_ASSESSMENT %in%
                                   compare_interests_11$VALUATION_ASSESSMENT) )]


compare_interests_11$EACOP_PAP_REF[
  which( (compare_interests_11$EACOP_PAP_REF %in%
            All_IWA_inner$EACOP_PAP_REF ) &
           
           (compare_interests_11$VALUATION_ASSESSMENT %in%
              All_IWA_inner$VALUATION_ASSESSMENT) )]



All_IWA_inner[["EACOP_PAP_REF"]][which( (All_IWA_inner$EACOP_PAP_REF %in%
                                      compare_interests_11$EACOP_PAP_REF) &
                                     
                                     (All_IWA_inner$VALUATION_ASSESSMENT %in%
                                        compare_interests_11$VALUATION_ASSESSMENT) )]


compare_interests_11[["EACOP_PAP_REF"]][
  which( (compare_interests_11$EACOP_PAP_REF %in%
            All_IWA_inner$EACOP_PAP_REF ) &
           
           (compare_interests_11$VALUATION_ASSESSMENT %in%
              All_IWA_inner$VALUATION_ASSESSMENT) )]



All_IWA_inner$VALUATION_ASSESSMENT[which( (All_IWA_inner$EACOP_PAP_REF %in%
                                      compare_interests_11$EACOP_PAP_REF) &
                                     
                            (All_IWA_inner$VALUATION_ASSESSMENT %in%
                            compare_interests_11$VALUATION_ASSESSMENT) )]


compare_interests_11$VALUATION_ASSESSMENT[
  which( (compare_interests_11$EACOP_PAP_REF %in%
            All_IWA_inner$EACOP_PAP_REF ) &
           
           (compare_interests_11$VALUATION_ASSESSMENT %in%
              All_IWA_inner$VALUATION_ASSESSMENT) )]

yy


## make changes

yy2 <- setdiff(yy, c("EACOP_PAP_REF", "PAP_NAME", "VALUATION_ASSESSMENT",
                     "DISTRICT", "SUBCOUNTY", "VILLAGE") )


sapply(yy2, FUN = function(y){
  
  print(y)
  
  compare_interests_11[[y]][
    which( (compare_interests_11$EACOP_PAP_REF %in%
              All_IWA_inner$EACOP_PAP_REF ) &
             
             (compare_interests_11$VALUATION_ASSESSMENT %in%
                All_IWA_inner$VALUATION_ASSESSMENT) )] <<-
    
    
    All_IWA_inner[[y]][which( (All_IWA_inner$EACOP_PAP_REF %in%
                                        compare_interests_11$EACOP_PAP_REF) &
                                       
                                       (All_IWA_inner$VALUATION_ASSESSMENT %in%
                                          compare_interests_11$VALUATION_ASSESSMENT) )]
  
  return(NULL)
})




## ************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_11 = read_csv("compare_interests_11.csv")


## **********************************************
## REPLACEMENT HOUSING
## **********************************************


Replacement_Housing = 
  read_excel("TRACKER FOR PHYSICALLY DISPLACED PAPS-31.03.2022.xlsx", 
                          sheet = "Replacement_Housing")


unique(Replacement_Housing$DISTRICT)

R_H_Kyankwanzi = subset(Replacement_Housing,
                        subset = (Replacement_Housing$DISTRICT ==
                                    "KYANKWANZI"))

colnames(R_H_Kyankwanzi)
colnames(compare_interests_11)



## checking for unaccounted ...

R_H_Unaccounted_1a = anti_join(x = compare_interests_11,
                               y = R_H_Kyankwanzi,
                               by = c("EACOP_PAP_REF" = 
                                        "PAP REF NO"))


R_H_Unaccounted_1b = sqldf("select * 
             from compare_interests_11 a
             where  not exists (
             select null from R_H_Kyankwanzi b
             where a.EACOP_PAP_REF like '%'||b.'PAP REF NO'||'%' );", 
                                  method = "raw")


R_H_Unaccounted_1a$PAP_NAME[
  which(!(R_H_Unaccounted_1a$PAP_NAME %in% 
            R_H_Unaccounted_1b$PAP_NAME))]


R_H_Unaccounted_2a = anti_join(x = R_H_Kyankwanzi,
                               y = compare_interests_11,
                               by = c("PAP REF NO" = "EACOP_PAP_REF") )


R_H_Unaccounted_2b = sqldf("select * 
             from R_H_Kyankwanzi b
             where  not exists (
             select null from compare_interests_11 a
             where  a.EACOP_PAP_REF like '%'||b.'PAP REF NO'||'%');", 
                                  method = "raw")


R_H_Unaccounted_2b$`PAP REF NO`[
  which(!(R_H_Unaccounted_2b$`PAP REF NO` %in% 
            R_H_Unaccounted_2a$`PAP REF NO` ))]




## checking for unaccounted ...

R_H_Unaccounted_1a = anti_join(x = compare_interests_11,
                               y = R_H_Kyankwanzi,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "PAP VALUATION ASSESSMENT REF"))


R_H_Unaccounted_1b = sqldf("select * 
             from compare_interests_11 a
             where  not exists (
             select null from R_H_Kyankwanzi b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%' );", 
                           method = "raw")


R_H_Unaccounted_1a$PAP_NAME[
  which(!(R_H_Unaccounted_1a$PAP_NAME %in% 
            R_H_Unaccounted_1b$PAP_NAME))]


R_H_Unaccounted_2a = anti_join(x = R_H_Kyankwanzi,
                               y = compare_interests_11,
                               by = c("PAP VALUATION ASSESSMENT REF" = 
                                        "VALUATION_ASSESSMENT") )


R_H_Unaccounted_2b = sqldf("select * 
             from R_H_Kyankwanzi b
             where  not exists (
             select null from compare_interests_11 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%');", 
                           method = "raw")


R_H_Unaccounted_2b$`PAP REF NO`[
  which(!(R_H_Unaccounted_2b$`PAP REF NO` %in% 
            R_H_Unaccounted_2a$`PAP REF NO` ))]



### Finally joining ...

Kyankwanzi_joined_12 = left_join(x = compare_interests_11,
                                 y = R_H_Kyankwanzi,
                                 by = c("VALUATION_ASSESSMENT" = 
                                          "PAP VALUATION ASSESSMENT REF"),
                                 keep = TRUE)


## With Duplicates Removed
Kyankwanzi_joined_12 = Kyankwanzi_joined_12 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kyankwanzi_joined_12$VALUATION_ASSESSMENT[which(
  duplicated(Kyankwanzi_joined_12$VALUATION_ASSESSMENT)
)]


check1 = subset(Kyankwanzi_joined_12, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "EACOP_PAP_REF",
                           "PAP REF NO"))


colnames(Kyankwanzi_joined_12)[
  which(colnames(Kyankwanzi_joined_12) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Kyankwanzi_joined_12)[
  which(colnames(Kyankwanzi_joined_12) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_12 = 
  subset(Kyankwanzi_joined_12, 
         select = colnames(Kyankwanzi_joined_12)[c(1:31,37:38)] )

write.csv(compare_interests_12, 
          file="compare_interests_12.csv", row.names = FALSE)

write.csv(Kyankwanzi_joined_12, 
          file="Kyankwanzi_joined_12.csv", row.names = FALSE)




## ************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_12 = read_csv("compare_interests_12.csv")


## **********************************************
## GRAVES
## **********************************************


Unmarked_Graves = 
  read_excel("GRAVES.xlsx", sheet = "Unmarked_Graves")

Marked_Graves = 
  read_excel("GRAVES.xlsx", sheet = "Marked_Graves")

unique(Unmarked_Graves$DISTRICT)

unique(Marked_Graves$DISTRICT)

Unmarked_Graves_Kyankwanzi = subset(Unmarked_Graves,
                        subset = (Unmarked_Graves$DISTRICT ==
                                    "KYANKWANZI"))

Marked_Graves_Kyankwanzi = subset(Marked_Graves,
                                   subset = (Marked_Graves$DISTRICT ==
                                                "KYANKWANZI"))


colnames(Unmarked_Graves_Kyankwanzi)
colnames(compare_interests_12)


## checking for unaccounted ...

U_G_Unaccounted_1a = anti_join(x = compare_interests_12,
                               y = Unmarked_Graves_Kyankwanzi,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


U_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_12 a
             where  not exists (
             select null from Unmarked_Graves_Kyankwanzi b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


U_G_Unaccounted_1a$PAP_NAME[
  which(!(U_G_Unaccounted_1a$PAP_NAME %in% 
            U_G_Unaccounted_1b$PAP_NAME))]


U_G_Unaccounted_2a = anti_join(x = Unmarked_Graves_Kyankwanzi,
                               y = compare_interests_12,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


U_G_Unaccounted_2b = sqldf("select * 
             from Unmarked_Graves_Kyankwanzi b
             where  not exists (
             select null from compare_interests_12 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


U_G_Unaccounted_2b$`PAP NAME`[
  which(!(U_G_Unaccounted_2b$`PAP NAME` %in% 
            U_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Kyankwanzi_joined_13a = left_join(x = compare_interests_12,
                                 y = Unmarked_Graves_Kyankwanzi,
                                 by = c("VALUATION_ASSESSMENT" = 
                                          "ASSESSMENT REFERENCE NUMBER"),
                                 keep = TRUE)


## With Duplicates Removed
Kyankwanzi_joined_13a = Kyankwanzi_joined_13a %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kyankwanzi_joined_13a$VALUATION_ASSESSMENT[which(
  duplicated(Kyankwanzi_joined_13a$VALUATION_ASSESSMENT)
)]


check1 = subset(Kyankwanzi_joined_13a, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Kyankwanzi_joined_13a)[
  which(colnames(Kyankwanzi_joined_13a) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Kyankwanzi_joined_13a)[
  which(colnames(Kyankwanzi_joined_13a) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13a = 
  subset(Kyankwanzi_joined_13a, 
         select = colnames(Kyankwanzi_joined_13a)[c(1:33,37)] )



## checking for unaccounted ...

colnames(Marked_Graves_Kyankwanzi)

M_G_Unaccounted_1a = anti_join(x = compare_interests_13a,
                               y = Marked_Graves_Kyankwanzi,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


M_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_13a a
             where  not exists (
             select null from Marked_Graves_Kyankwanzi b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


M_G_Unaccounted_1a$PAP_NAME[
  which(!(M_G_Unaccounted_1a$PAP_NAME %in% 
            M_G_Unaccounted_1b$PAP_NAME))]


M_G_Unaccounted_2a = anti_join(x = Marked_Graves_Kyankwanzi,
                               y = compare_interests_13a,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


M_G_Unaccounted_2b = sqldf("select * 
             from Marked_Graves_Kyankwanzi b
             where  not exists (
             select null from compare_interests_13a a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


M_G_Unaccounted_2b$`PAP NAME`[
  which(!(M_G_Unaccounted_2b$`PAP NAME` %in% 
            M_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Kyankwanzi_joined_13b = left_join(x = compare_interests_13a,
                                  y = Marked_Graves_Kyankwanzi,
                                  by = c("VALUATION_ASSESSMENT" = 
                                           "ASSESSMENT REFERENCE NUMBER"),
                                  keep = TRUE)


## With Duplicates Removed
Kyankwanzi_joined_13b = Kyankwanzi_joined_13b %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kyankwanzi_joined_13b$VALUATION_ASSESSMENT[which(
  duplicated(Kyankwanzi_joined_13b$VALUATION_ASSESSMENT)
)]


check1 = subset(Kyankwanzi_joined_13b, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Kyankwanzi_joined_13b)[
  which(colnames(Kyankwanzi_joined_13b) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Kyankwanzi_joined_13b)[
  which(colnames(Kyankwanzi_joined_13b) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13b = 
  subset(Kyankwanzi_joined_13b, 
         select = colnames(Kyankwanzi_joined_13b)[c(1:34,38)] )


compare_interests_13 = compare_interests_13b

Kyankwanzi_joined_13 = Kyankwanzi_joined_13b


write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)

write.csv(Kyankwanzi_joined_13, 
          file="Kyankwanzi_joined_13.csv", row.names = FALSE)



###########################################################
## REPEAT SNAGS
###########################################################


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_13 = read_csv("compare_interests_13.csv")



## **********************************************
## SNAGS
## **********************************************

SNAGS_All = read_excel("Comprehensive Snag List 04-16.03.2022.xlsx", 
                       sheet = "Combined Snags List 2",
                       col_types = c("numeric", "text", "text", 
                                     "text", "text", "text", "date", "text", 
                                     "text", "text"))


## change date formats

SNAGS_All$`Date when snag was handled (If fieldwork was needed)` = 
  as.character(
    SNAGS_All$`Date when snag was handled (If fieldwork was needed)`  )


unique(SNAGS_All$`SNAG ISSUE`)


library(stringr)


sapply(unique(SNAGS_All$`SNAG ISSUE`),
       FUN = function(i){
         compare_interests_13[,i] <<- 
           ifelse(str_detect(compare_interests_13$`SNAG ISSUE`, 
                             regex(i, ignore_case = TRUE)),"YES",NA)
       })


which(!is.na(compare_interests_13$POA) &
        !is.na(compare_interests_13$`Revocation of Verbal POA`))


## save update
write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)









