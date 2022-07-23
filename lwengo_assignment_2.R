

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)


# LWENGO


## data of Lwengo Only from NewPlan
Lwengo_Control = read_excel("Lwengo_assignment_2.xlsx", 
                                sheet = "Lwengo_NewPlan")

## Number of Interests Control
length(unique(Lwengo_Control$PAP_VALUATION_ASSESSMENT_REF))



## data from Total and Bank
Lwengo_From_Bank = read_excel("Lwengo_assignment_2.xlsx", 
                                     sheet = "Lwengo_From_Bank (2)")

## Remove empty rows
Lwengo_From_Bank = 
  Lwengo_From_Bank[!is.na(Lwengo_From_Bank$`PAP VALUATION ASSESSMENT REF`), ]


## Number of Interests From Bank Without Duplicates
length(unique(Lwengo_From_Bank$`PAP VALUATION ASSESSMENT REF`))

#### --- This is an issue of concern. Why those few interests ?



## names
colnames(Lwengo_Control)
colnames(Lwengo_From_Bank)



# Joining


## left join
Lwengo_joined = left_join(x = Lwengo_Control, 
                           y = Lwengo_From_Bank, 
                           by = c("PAP_NAME" = "PAP NAME",
                                  "VILLAGE" = "VILLAGE"))

Unaccounted_1 = anti_join(x = Lwengo_Control, 
                          y = Lwengo_From_Bank, 
                          by = c("PAP_NAME" = "PAP NAME",
                                   "VILLAGE" = "VILLAGE"))


Unaccounted_2 = anti_join(x = Lwengo_From_Bank, 
                          y = Lwengo_Control, 
                          by = c("PAP NAME" = "PAP_NAME",
                               "VILLAGE" = "VILLAGE"))


#### --- The following lines only apply if there is "Unaccounted_2"


unique(Unaccounted_2$VILLAGE)

unique(Lwengo_Control$VILLAGE)

which(Lwengo_From_Bank$VILLAGE == "Kasambya")

Lwengo_From_Bank$VILLAGE[which(Lwengo_From_Bank$VILLAGE == 
                                   "Kasambya")] = "KASAMBYA"


Lwengo_joined = left_join(x = Lwengo_Control, 
                            y = Lwengo_From_Bank, 
                            by = c("PAP_NAME" = "PAP NAME",
                                   "VILLAGE" = "VILLAGE"))

Unaccounted_1 = anti_join(x = Lwengo_Control, 
                          y = Lwengo_From_Bank, 
                          by = c("PAP_NAME" = "PAP NAME",
                                 "VILLAGE" = "VILLAGE"))


Unaccounted_2 = anti_join(x = Lwengo_From_Bank, 
                          y = Lwengo_Control, 
                          by = c("PAP NAME" = "PAP_NAME",
                                 "VILLAGE" = "VILLAGE"))

unique(Unaccounted_2$`PAP NAME`)

unique(Unaccounted_1$PAP_NAME)

which(Lwengo_From_Bank$`PAP NAME` == "VINCENT TIBEMANYA")

Lwengo_From_Bank$`PAP NAME`[which(Lwengo_From_Bank$`PAP NAME` == 
          "VINCENT TIBEMANYA")] = "TIBEMANYA VINCENT"


### --- Repeat process ...



# Compare Interests
colnames(Lwengo_joined)
colnames(Lwengo_joined)[19] = "CENTENARY BANK ACCOUNT NAME(s):"

compare_interests_1 = subset(Lwengo_joined,
                             select = c("PAP_NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REF",
                                        "PAP REF NO",
                                        "EACOP PAP REF NO.",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE")) 


compare_interests_2 = subset(Lwengo_joined,
                           select = c("PAP_NAME",
                                      "PAP_VALUATION_ASSESSMENT_REF",
                                      "PAP VALUATION ASSESSMENT REF",
                                      "PAP REF NO",
                                      "EACOP PAP REF NO.",
                                      "DISTRICT.x",
                                      "SUBCOUNTY",
                                      "VILLAGE",
                                      "Type of ID",
                                      "ID Number",
                                      "PAP'S SPOUSE NAME(s):",
                                      "CENTENARY BANK ACCOUNT NUMBER:",
                                      "CENTENARY BANK ACCOUNT NAME(s):",
                                      "Date Bank Account Opened:",
                                      "SINGLE or JOINT Bank Account:",
          "Did PAP (+ SPOUSE) attend Bank A/C training and FLT? (Y/N)",
                                      "COMMENTS:"))   




# clean data from Mathias BAO
Lwengo_BAO = read_excel("Lwengo Analyzed.xlsx", 
                                     sheet = "Lwengo_BAO")

## Remove empty rows
Lwengo_BAO = 
  Lwengo_BAO[!is.na(Lwengo_BAO$PAP_name_No_Dash), ]



# Joining with Mathias
colnames(compare_interests_2)
colnames(Lwengo_BAO)


Lwengo_joined_3 = left_join(x = compare_interests_2, 
                              y = Lwengo_BAO, 
                              by = c("PAP_NAME" = "PAP_name_No_Dash",
                                     "VILLAGE" = "village"))


colnames(Lwengo_joined_3)


compare_interests_3 = subset(Lwengo_joined_3,
      select = c("PAP REF NO",
                 "EACOP PAP REF NO.",
                 "PAP_NAME",
                 "PAP_name",
                 "PAP_VALUATION_ASSESSMENT_REF",
                 "PAP VALUATION ASSESSMENT REF",
                 "DISTRICT.x",
                 "SUBCOUNTY",
                 "VILLAGE",
                 "Type of ID",
                 "type_ID", 
                 "ID Number",
                 "ID_NO",
                 "PAPhve_spouse",
                 "NO_spouse",
                 "PAP'S SPOUSE NAME(s):",
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



# Joining with Centenary Core Banking System

centenary_core_Jan_25_2022 = read_excel("EACOP Accounts 25-01-2022.xlsx", 
                                        sheet = "Qualified accts ")

colnames(centenary_core_Jan_25_2022)

colnames(centenary_core_Jan_25_2022)[
  which(colnames(centenary_core_Jan_25_2022) == "NATIONAL ID")] = "ID_NO"

centenary_core_needed = subset(centenary_core_Jan_25_2022,
                               select = c("ACCOUNT_DESCRIPTION", 
                                          "FIRST_NAME", 
                                          "SURNAME",
                                          "ID_NO",
                                          "ACCOUNT_NUMBER",
                                          "ACCT_TYPE",
                                          "DESCRIPTION",
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


colnames(compare_interests_3)[
  which(colnames(compare_interests_3) == "PAP_name")] = "PAP_name2"

Lwengo_joined_4_left = sqldf("select *
                      from compare_interests_3 a
                      left join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                               method = "raw")


Lwengo_joined_4_inner = sqldf("select *
                      from compare_interests_3 a
                      inner join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                                method = "raw")


Interests_Without_Accounts = sqldf("select * 
             from Lwengo_joined_4_left a
             where  not exists (
             select null from Lwengo_joined_4_inner b
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

write.csv(Interests_Without_Accounts, 
          file="Interests_Without_Accounts.csv", row.names = FALSE)


write.csv(Lost_and_Found, 
          file="Lost_and_Found.csv", row.names = FALSE)


## --- After editing file ...

Interests_Without_Accounts = read_excel("Lwengo_Feb_4_2022.xlsx", 
                                        sheet = "Interests_Without_Accounts_2")

Lost_and_Found_new = read_excel("Lwengo_Feb_4_2022.xlsx", 
                                sheet = "Lost_and_Found_2")

### --- Duplicated to remove duplicate columns

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

write.csv(Lost_and_Found_joined, 
          file="Lost_and_Found_joined.csv", row.names = FALSE)


Lwengo_joined_4_left = 
  Lwengo_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Lwengo_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
  Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF

which(Lwengo_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
        Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)


check =
  Lwengo_joined_4_left[
    (which(Lwengo_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
             Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


Lwengo_joined_4_left =
  Lwengo_joined_4_left[
    -(which(Lwengo_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
              Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


colnames(Lost_and_Found_joined) = colnames(Lwengo_joined_4_left)

Lwengo_joined_4_left = 
  rbind(Lwengo_joined_4_left,
        Lost_and_Found_joined)

write.csv(Lwengo_joined_4_left, 
          file="Lwengo_joined_4_left.csv", row.names = FALSE)


Lwengo_joined_4_left = 
  Lwengo_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

Lwengo_joined_4 = Lwengo_joined_4_left


#### ---- checking missing bank account names

missed_bank_account_names = subset(Lwengo_joined_4,
    subset = ( (is.na(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
    (Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
    (Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") ))


Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`[
  which( (is.na(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
         (Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] =
  
Lwengo_joined_4$ACCOUNT_DESCRIPTION[
  which( (is.na(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
           (Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Lwengo_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] 


### ---- checking bank account numbers

cross_check_bank_accounts = Lwengo_joined_4[
  which(nchar(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`


#### check account numbers in centenary bank system
Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in bank system
Lwengo_joined_4$PAP_NAME[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account numbers in Mathias survey data
Lwengo_joined_4$`centenary-bankaccount`[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in centenary bank system name description
Lwengo_joined_4$PAP_NAME[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )] %in%
  centenary_core_needed$ACCOUNT_DESCRIPTION


### --- Correct Bank Accounts

Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "323074107")] = "3203074107"

Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "320374562")] = "3203074562"

Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "32030743256")] = "3203074356"

Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "320373452")] = "3203073452"

Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "320373566")] = "3203073566"


### --- Repeat check for bank account numbers

cross_check_bank_accounts = Lwengo_joined_4[
  which(nchar(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Lwengo_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`



#  some data from server
data_extra_server = read_excel("EACOP PAP NATIONAL ID.xlsx", 
                               sheet = "Sheet2")


colnames(Lwengo_joined_4)
colnames(data_extra_server)


# Join Again
Lwengo_joined_5 = left_join(x = Lwengo_joined_4, 
                            y = data_extra_server, 
                            by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                     "ASSESSMENT REFERENCE NUMBER"))


colnames(Lwengo_joined_5)


compare_interests_4 = subset(Lwengo_joined_5,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "EACOP PAP REF NO.", 
                                        "PAP_NAME",
                                        "PAP_name2", 
                                        "PAP NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",                                       
                                        "PAP VALUATION ASSESSMENT REF",                                      
                                        "DISTRICT.x",                                                         
                                        "SUBCOUNTY",                                                          
                                        "VILLAGE",                                                          
                                        "Type of ID",                                                         
                                        "type_ID", 
                                        "ID Number",
                                        "ID NUMBER",
                                        "ID_NO",
                                        "DATE_OF_BIRTH",
                                        "PAPhve_spouse",
                                        "NO_spouse",
                                        "PAP'S SPOUSE NAME(s):",                                                
                                        "spouse_name_No_Dash",                                                        
                                        "spouse-IDno", 
                                        "ACCOUNT_NUMBER",
                                        "CENTENARY BANK ACCOUNT NUMBER:",                                     
                                        "centenary-bankaccount",                                              
                                        "CENTENARY BANK ACCOUNT NAME(s):",
                                        "FIRST_NAME",
                                        "SURNAME",
                                        "centenary-accountname",
                                        "ACCOUNT_DESCRIPTION",
                                        "CUST_ID",
                                        "Date Bank Account Opened:", 
                                        "date-bankaccountopened",
                                        "ACCT_TYPE",
                                        "SINGLE or JOINT Bank Account:",                                      
                                        "Did PAP (+ SPOUSE) attend Bank A/C training and FLT? (Y/N)",         
                                        "PAP-spouse-banktraing",                                              
                                        "COMMENTS:",
                                        "MOBILE_TEL",
                                        "TELEPHONE_1")) 





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

cross_check_ids_on_server = ids_found_on_server[
  which(nchar(ids_found_on_server$`ID NUMBER`) != 14), ]

cross_check_ids_on_server$`ID NUMBER`
nchar(cross_check_ids_on_server$`ID NUMBER`)


### --- Correct IDs

compare_interests_4$`ID NUMBER`[which(
  compare_interests_4$`ID NUMBER` ==
    "CF8610510FFNK,CF78105104EGIH")] = "CF86105101FFNK,CF78105104EG1H"


compare_interests_4$`ID NUMBER`[which(
  compare_interests_4$`ID NUMBER` ==
    "CF900610350PG")] = "CF7900610350PG"



compare_interests_4$`ID Number`[
  which((compare_interests_4$`ID Number` == "N/A" |
           compare_interests_4$`ID Number` == "NA" |
           is.na(compare_interests_4$`ID Number`)) &
          
          !( compare_interests_4$`ID NUMBER` == "N/A" |
               compare_interests_4$`ID NUMBER` == "NA" |
               is.na(compare_interests_4$`ID NUMBER`)) &
          
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


check_ids_again = subset(ids_found_with_mathias,
                         select = c("ID Number",
                                    "ID NUMBER",
                                    "ID_NO") )


length(which(nchar(ids_found_with_mathias$ID_NO) == 14))

length(which(nchar(ids_found_with_mathias$ID_NO) != 14))

cross_check_ids_with_mathias = ids_found_with_mathias[
  which(nchar(ids_found_with_mathias$ID_NO) != 14), ]

cross_check_ids_with_mathias$ID_NO
nchar(cross_check_ids_with_mathias$ID_NO)


### --- Correct IDs from Mathias

compare_interests_4$ID_NO[which(
  compare_interests_4$ID_NO ==
    "CM9705102DUAD")] = "CM97105102DUAD"

compare_interests_4$ID_NO[which(
  compare_interests_4$ID_NO ==
    "CM8105104Z3VF")] = "CM48105104Z3VF"


compare_interests_4$ID_NO[which(
  compare_interests_4$ID_NO ==
    "CM94105102AEG")] = "CM94105102PAEG"



compare_interests_4$`ID Number`[
  which((compare_interests_4$`ID Number` == "N/A" |
           compare_interests_4$`ID Number` == "NA" |
           is.na(compare_interests_4$`ID Number`)) &
          
          !( compare_interests_4$ID_NO == "N/A" |
               compare_interests_4$ID_NO == "NA" |
               is.na(compare_interests_4$ID_NO)) &
          
          (startsWith(toupper(compare_interests_4$ID_NO), 
                      prefix = "CF") |
             startsWith(toupper(compare_interests_4$ID_NO), 
                        prefix = "CM") ))]   =
  
  compare_interests_4$ID_NO[
    which((compare_interests_4$`ID Number` == "N/A" |
             compare_interests_4$`ID Number` == "NA" |
             is.na(compare_interests_4$`ID Number`)) &
            
            !( compare_interests_4$ID_NO == "N/A" |
                 compare_interests_4$ID_NO == "NA" |
                 is.na(compare_interests_4$ID_NO)) &
            
            (startsWith(toupper(compare_interests_4$ID_NO), 
                        prefix = "CF") |
               startsWith(toupper(compare_interests_4$ID_NO), 
                          prefix = "CM") ))]



## -- crosscheck ids from bank

length(which(nchar(compare_interests_4$`ID Number`) != 14))

length(which(nchar(Lwengo_joined$`ID Number`) != 14))

cross_check_ids_from_bank = compare_interests_4[
  which( (nchar(compare_interests_4$`ID Number`) != 14) &
           ((startsWith(toupper(compare_interests_4$`ID Number`), 
                        prefix = "CF") |
               startsWith(toupper(compare_interests_4$`ID Number`), 
                          prefix = "CM") )) ), ]

cross_check_ids_from_bank$`ID Number`

nchar(cross_check_ids_from_bank$`ID Number`)

#### check IDs in centenary bank system
compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` %in%
    cross_check_ids_from_bank$`ID Number` )]

#### check PAP names in bank system
compare_interests_4$PAP_NAME[which(
  compare_interests_4$`ID Number` %in%
    cross_check_ids_from_bank$`ID Number` )]

#### check PAP names in centenary bank system name description
compare_interests_4$PAP_NAME[which(
  compare_interests_4$`ID Number` %in%
    cross_check_ids_from_bank$`ID Number` )] %in%
  compare_interests_4$ACCOUNT_DESCRIPTION


compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CM9015101W45L")] = "CM90105101W46L"

compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CF6411051O4ZP2H")] = "CM90105101W46L"

compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CF6411051O4ZP2H")] = "CF641051O4ZP2H"

compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CM9015101NAKH")] = "CM90105101NAKH"

compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CM791051031JR5F")] = "CM79105103UR5F"

compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CM5203610081D")] = "CM52036100U81D"


compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CF5034104HGDC")] = "CF75034104HGDC"



# REMOVE DUPLICATES ...

length(unique(compare_interests_4$PAP_VALUATION_ASSESSMENT_REF))

## Duplicate Interests
Duplicate_Interests = 
  compare_interests_4[
    duplicated(compare_interests_4$PAP_VALUATION_ASSESSMENT_REF), ]


## With Duplicates Removed From Interests
compare_interests_4 = compare_interests_4 %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)



colnames(compare_interests_4)

compare_interests_5 = subset(compare_interests_4,
    select = c("PAP REF NO.x",
               "PAP_NAME",                                                           
               "PAP_VALUATION_ASSESSMENT_REF",                                       
               "DISTRICT.x",                                                         
               "SUBCOUNTY",                                                          
               "VILLAGE",
               "FIRST_NAME",
               "SURNAME",
               "ID Number",
               "MOBILE_TEL",
               "DATE_OF_BIRTH",
               "CUST_ID",
               "ACCT_TYPE",
               "ACCOUNT_DESCRIPTION",
               "ACCOUNT_NUMBER",
               "PAP'S SPOUSE NAME(s):",
               "spouse-IDno")) 


colnames(compare_interests_5) = 
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
    "SPOUSE_NAME",
    "SPOUSE_ID")


write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)


write.csv(compare_interests_5, 
          file="compare_interests_5.csv", row.names = FALSE)


t = unique(compare_interests_5$`CENTENARY BANK ACCOUNT NUMBER:`)
t = t[-which(t=="0"|is.na(t))]
t



unique_names = sapply(t, FUN = function(i){
  shared_accounts = c()
  
  names = compare_interests_5$PAP_NAME[
    which(compare_interests_5$`CENTENARY BANK ACCOUNT NUMBER:` == i)]
  
  print(length(unique(names)))
  print(unique(names))
  print(i)
  
  if( length(unique(names)) > 1){
    
    shared_accounts = c(shared_accounts, i, unique(names) )
    return(return(shared_accounts))
  }else{ 
    return(NULL)}
  
  })


## --- some shared account numbers

unique_names[["MORE THAN 3 SIGNATORIES"]]

unique_names[["Missing  NIN"]]

unique_names[["3203073890"]]


Disputed_Accounts = subset(compare_interests_5,
                           compare_interests_5$`CENTENARY BANK ACCOUNT NUMBER:` ==  
                             "3203073890" )

write.csv(Disputed_Accounts, 
          file="Disputed_Accounts.csv", row.names = FALSE)




########### ---------------------------------------- ################


# SECTION TWO ... NOT FROM SCRATCH ...


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)

compare_interests_4 = read_csv("compare_interests_4.csv")
compare_interests_5 = read_csv("compare_interests_5.csv")


# SUB-SECTION ....


Disputed_Accounts = subset(compare_interests_5,
                           compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
                             "3203064730" )

write.csv(Disputed_Accounts, 
          file="Disputed_Accounts.csv", row.names = FALSE)



# Unaccounted for accounts in Centenary Core Banking System

centenary_core_needed$ACCOUNT_NUMBER =
  as.character(centenary_core_needed$ACCOUNT_NUMBER)

centenary_core_needed$ACCOUNT_DESCRIPTION =
  as.character(centenary_core_needed$ACCOUNT_DESCRIPTION)

compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER =
  as.character(compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER)


Unaccounted_bank_account_numbers = 
  anti_join(x = compare_interests_5, 
            y = centenary_core_needed, 
            by = c("CENTENARY_BANK_ACCOUNT_NUMBER" = "ACCOUNT_NUMBER"))


wrong_account_numbers = subset(Unaccounted_bank_account_numbers,
                (startsWith(toupper(
                Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER), 
                                 prefix = "3") |
                startsWith(toupper(
                Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER), 
                                 prefix = "32") ) )


write.csv(Unaccounted_bank_account_numbers, 
          file="Unaccounted_bank_account_numbers.csv", row.names = FALSE)


# Update master list

compare_interests_4$`CENTENARY BANK ACCOUNT NUMBER:`[ which(
  compare_interests_4$`CENTENARY BANK ACCOUNT NUMBER:` == 
     "3203076807+320376830") ] = "3203076807+3203076830"



# Update Bank list
compare_interests_5 = subset(compare_interests_4,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",                                                           
                                        "PAP_VALUATION_ASSESSMENT_REF",                                       
                                        "DISTRICT.x",                                                         
                                        "SUBCOUNTY",                                                          
                                        "VILLAGE",
                                        "FIRST_NAME",
                                        "SURNAME",
                                        "ID Number",
                                        "MOBILE_TEL",
                                        "DATE_OF_BIRTH",
                                        "CUST_ID",
                                        "ACCT_TYPE",
                                        "ACCOUNT_DESCRIPTION",
                                        "ACCOUNT_NUMBER",
                                        "PAP'S SPOUSE NAME(s):",
                                        "spouse-IDno")) 


colnames(compare_interests_5) = 
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
    "SPOUSE_NAME",
    "SPOUSE_ID")


#### Repeat loop ...


centenary_core_needed$ACCOUNT_NUMBER =
  as.character(centenary_core_needed$ACCOUNT_NUMBER)

centenary_core_needed$ACCOUNT_DESCRIPTION =
  as.character(centenary_core_needed$ACCOUNT_DESCRIPTION)

compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER =
  as.character(compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER)


Unaccounted_bank_account_numbers = 
  anti_join(x = compare_interests_5, 
            y = centenary_core_needed, 
            by = c("CENTENARY_BANK_ACCOUNT_NUMBER" = "ACCOUNT_NUMBER"))


wrong_account_numbers = subset(Unaccounted_bank_account_numbers,
                               (startsWith(toupper(
                                 Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER), 
                                 prefix = "3") |
                                  startsWith(toupper(
                                    Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER), 
                                    prefix = "32") ) )


Disputed_Accounts = subset(compare_interests_5,
                           compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
                             "3203073890" )


### add them only if they are mutually exclusive
Unaccounted_bank_account_numbers = rbind(Disputed_Accounts,
                                         Unaccounted_bank_account_numbers)


write.csv(Unaccounted_bank_account_numbers, 
          file="Unaccounted_bank_account_numbers.csv", row.names = FALSE)


write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)

write.csv(compare_interests_5, 
          file="compare_interests_5.csv", row.names = FALSE)



# SPOUSES ....SUB-SECTION


## Spouses data from Mathias BAO
Spouses_BAO = read_excel("Lwengo Analyzed.xlsx", 
                         sheet = "Spouses BAO")


Spouses_BAO$village_No_Dash = gsub(pattern = "-", 
                                   replacement = " ",
                                   x = Spouses_BAO$village,
                                   ignore.case = TRUE)

Spouses_BAO$PAP_name_No_Dash = toupper(Spouses_BAO$PAP_name_No_Dash)
Spouses_BAO$subcounty = toupper(Spouses_BAO$subcounty)
Spouses_BAO$village_No_Dash = toupper(Spouses_BAO$village_No_Dash)


## Spouses data from Control
Spouses_Control = read_excel("Lwengo Analyzed.xlsx", 
                             sheet = "Spouses Control")



## join 

Spouses_joined = full_join(x = Spouses_Control, 
                           y = Spouses_BAO, 
                           by = c("PAP NAME" = "PAP_name_No_Dash",
                                  "SUBCOUNTY" = "subcounty",
                                  "VILLAGE" = "village_No_Dash"))


Spouses_joined_2 = full_join(x = Spouses_Control, 
                             y = Spouses_BAO, 
                             by = c("PAP NAME" = "PAP_name_No_Dash",
                                    "VILLAGE" = "village_No_Dash"))


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
                                       "NO_spouse",
                                       "PAPhve_spouse") )


Spouses_compared_2 = subset(Spouses_joined_2,
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
                                       "NO_spouse",
                                       "PAPhve_spouse") )



# join with master list

Spouses_joined_3 = left_join(x = compare_interests_4,
                             y = Spouses_compared_1,
                             by = c("PAP_NAME" = "PAP NAME",
                                    "SUBCOUNTY" = "SUBCOUNTY",
                                    "VILLAGE" = "VILLAGE") )


Spouses_joined_4 = left_join(x = compare_interests_4,
                             y = Spouses_compared_1,
                             by = c("PAP_NAME" = "PAP NAME",
                                    "VILLAGE" = "VILLAGE") )



Unaccounted_spouses_3 = anti_join(x = compare_interests_4,
                                  y = Spouses_compared_1,
                                  by = c("PAP_NAME" = "PAP NAME",
                                         "SUBCOUNTY" = "SUBCOUNTY",
                                         "VILLAGE" = "VILLAGE") )

Unaccounted_spouses_3.2 = anti_join(x = Spouses_compared_1, 
                                    y = compare_interests_4,
                                    by = c("PAP NAME" = "PAP_NAME",
                                           "SUBCOUNTY" = "SUBCOUNTY",
                                           "VILLAGE" = "VILLAGE") )


Unaccounted_spouses_4 = anti_join(x = compare_interests_4,
                                  y = Spouses_compared_1,
                                  by = c("PAP_NAME" = "PAP NAME",
                                         "VILLAGE" = "VILLAGE") )


Unaccounted_spouses_4.2 = anti_join(x = Spouses_compared_1, 
                                    y = compare_interests_4,
                                    by = c("PAP NAME" = "PAP_NAME",
                                           "VILLAGE" = "VILLAGE") )



# REMOVE DUPLICATES ...

length(unique(Spouses_joined_3$PAP_VALUATION_ASSESSMENT_REF))

## Duplicate Interests
Duplicate_Interests_2 = 
  Spouses_joined_3[
    duplicated(Spouses_joined_3$PAP_VALUATION_ASSESSMENT_REF), ]

## With Duplicates Removed From Interests
Spouses_joined_3 = Spouses_joined_3 %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Lwengo_joined_6 = Spouses_joined_3

colnames(Lwengo_joined_6)

colnames(Lwengo_joined_6)[which(
  colnames(Lwengo_joined_6) == "PAP_name2")] = "PAP_name.x"

colnames(Lwengo_joined_6)[which(
  colnames(Lwengo_joined_6) == "PAP_name")] = "PAP_name.y"

compare_interests_6 = subset(Lwengo_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "EACOP PAP REF NO.",
                                        "PAP REF NO",
                                        "PAP_RefNo",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REF",
                                        "PAP_NAME",
                                        "PAP_name.x",
                                        "PAP_name.y",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse_name_No_Dash.x",
                                        "spouse_name_No_Dash.y", 
                                        "spouse-IDno.x",
                                        "spouse-IDno.y",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE",
                                        "PAPhve_spouse.x", 
                                        "PAPhve_spouse.y",
                                        "NO_spouse.x",
                                        "NO_spouse.y")  )


write.csv(Lwengo_joined_6, 
          file="Lwengo_joined_6.csv", row.names = FALSE)

write.csv(compare_interests_6, 
          file="compare_interests_6.csv", row.names = FALSE)



## Fill in Spouse ID column ....


missed_PAP_names = subset(compare_interests_6,
              subset = ( (is.na(compare_interests_6$PAP_name.x)) |
                       (compare_interests_6$PAP_name.x == "N/A") |
                       (compare_interests_6$PAP_name.x == "N A") |
                       (compare_interests_6$PAP_name.x == "NA") ))


Lwengo_joined_6$PAP_name.x[
  which( (is.na(Lwengo_joined_6$PAP_name.x)) |
           (Lwengo_joined_6$PAP_name.x == "N/A") |
           (Lwengo_joined_6$PAP_name.x == "N A") |
           (Lwengo_joined_6$PAP_name.x == "NA") )] =
  
  Lwengo_joined_6$PAP_name.y[
    which( (is.na(Lwengo_joined_6$PAP_name.x)) |
             (Lwengo_joined_6$PAP_name.x == "N/A") |
             (Lwengo_joined_6$PAP_name.x == "N A") |
             (Lwengo_joined_6$PAP_name.x == "NA") )] 



missed_spouse_names = subset(compare_interests_6,
      subset = ( (is.na(compare_interests_6$spouse_name_No_Dash.x)) |
               (compare_interests_6$spouse_name_No_Dash.x == "N/A") |
               (compare_interests_6$spouse_name_No_Dash.x == "N A") |
               (compare_interests_6$spouse_name_No_Dash.x == "NA") ))


Lwengo_joined_6$spouse_name_No_Dash.x[
  which( (is.na(Lwengo_joined_6$spouse_name_No_Dash.x)) |
           (Lwengo_joined_6$spouse_name_No_Dash.x == "N/A") |
           (Lwengo_joined_6$spouse_name_No_Dash.x == "N A") |
           (Lwengo_joined_6$spouse_name_No_Dash.x == "NA") )] =
  
  Lwengo_joined_6$spouse_name_No_Dash.y[
    which( (is.na(Lwengo_joined_6$spouse_name_No_Dash.x)) |
             (Lwengo_joined_6$spouse_name_No_Dash.x == "N/A") |
             (Lwengo_joined_6$spouse_name_No_Dash.x == "N A") |
             (Lwengo_joined_6$spouse_name_No_Dash.x == "NA") )] 



missed_spouse_names_main = subset(compare_interests_6,
      subset = ( (is.na(compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") ))


Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`[
  which( (is.na(Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
           (Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
           (Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
           (Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )] =
  
  Lwengo_joined_6$spouse_name_No_Dash.x[
    which( (is.na(Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
             (Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
             (Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
             (Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )]



missed_spouse_IDs = subset(compare_interests_6,
                subset = ( (is.na(compare_interests_6$`spouse-IDno.x`)) |
                (compare_interests_6$`spouse-IDno.x` == "N/A") |
                (compare_interests_6$`spouse-IDno.x` == "N A") |
                (compare_interests_6$`spouse-IDno.x` == "NA") ))


Lwengo_joined_6$`spouse-IDno.x`[
  which( (is.na(Lwengo_joined_6$`spouse-IDno.x`)) |
           (Lwengo_joined_6$`spouse-IDno.x` == "N/A") |
           (Lwengo_joined_6$`spouse-IDno.x` == "N A") |
           (Lwengo_joined_6$`spouse-IDno.x` == "NA") )] =
  
  Lwengo_joined_6$`spouse-IDno.y`[
    which( (is.na(Lwengo_joined_6$`spouse-IDno.x`)) |
             (Lwengo_joined_6$`spouse-IDno.x` == "N/A") |
             (Lwengo_joined_6$`spouse-IDno.x` == "N A") |
             (Lwengo_joined_6$`spouse-IDno.x` == "NA") )] 



Lwengo_joined_6$NO_spouse.x[
  which( (is.na(Lwengo_joined_6$NO_spouse.x)) |
           (Lwengo_joined_6$NO_spouse.x == "N/A") |
           (Lwengo_joined_6$NO_spouse.x == "N A") |
           (Lwengo_joined_6$NO_spouse.x == "NA") )] =
  
  Lwengo_joined_6$NO_spouse.y[
    which( (is.na(Lwengo_joined_6$NO_spouse.x)) |
             (Lwengo_joined_6$NO_spouse.x == "N/A") |
             (Lwengo_joined_6$NO_spouse.x == "N A") |
             (Lwengo_joined_6$NO_spouse.x == "NA") )] 



Lwengo_joined_6$PAPhve_spouse.x[
  which( (is.na(Lwengo_joined_6$PAPhve_spouse.x)) |
           (Lwengo_joined_6$PAPhve_spouse.x == "N/A") |
           (Lwengo_joined_6$PAPhve_spouse.x == "N A") |
           (Lwengo_joined_6$PAPhve_spouse.x == "NA") )] =
  
  Lwengo_joined_6$PAPhve_spouse.y[
    which( (is.na(Lwengo_joined_6$PAPhve_spouse.x)) |
             (Lwengo_joined_6$PAPhve_spouse.x == "N/A") |
             (Lwengo_joined_6$PAPhve_spouse.x == "N A") |
             (Lwengo_joined_6$PAPhve_spouse.x == "NA") )] 



Lwengo_joined_6$`PAP SPOUSE NAME:  (SPOUSE 2)`[ which(
  (Lwengo_joined_6$PAP_NAME == "KATO ZZIWA DAMIANO") & 
    (Lwengo_joined_6$PAP_VALUATION_ASSESSMENT_REF ==
       "LWE/NDA/KIB/L003-04-00/842") &
    (Lwengo_joined_6$SUBCOUNTY == "NDAGWE") &
    (Lwengo_joined_6$VILLAGE == "KIBANYI") ) ] = 
  "NAMATOVU MOLLY"



# Check spouse IDs ....


nchar(Lwengo_joined_6$`spouse-IDno.x`)

length(which(nchar(compare_interests_6$`spouse-IDno.x`) == 14))

length(which(nchar(compare_interests_6$`spouse-IDno.x`) != 14))

cross_check_ids_spouses = compare_interests_6[
  which(  (nchar(compare_interests_6$`spouse-IDno.x`) != 14) &
            ((startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                         prefix = "CF") |
                startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                           prefix = "CM") ))), ]



### --- Correct IDs

Lwengo_joined_6$`spouse-IDno.x`[which(
  Lwengo_joined_6$`spouse-IDno.x` ==
    "CM710110279YA")] = "CM6710110279YA"



compare_interests_6 = subset(Lwengo_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "EACOP PAP REF NO.",
                                        "PAP REF NO",
                                        "PAP_RefNo",
                                        "PAP_NAME",
                                        "PAP_name.x",
                                        "PAP_name.y",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse_name_No_Dash.x",
                                        "spouse_name_No_Dash.y", 
                                        "spouse-IDno.x",
                                        "spouse-IDno.y",
                                        "PAPhve_spouse.x", 
                                        "PAPhve_spouse.y",
                                        "NO_spouse.x",
                                        "NO_spouse.y",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REF")  )



compare_interests_7 = subset(Lwengo_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",                                                           
                                        "PAP_VALUATION_ASSESSMENT_REF",                                       
                                        "DISTRICT.x",                                                         
                                        "SUBCOUNTY",                                                          
                                        "VILLAGE",
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



write.csv(cross_check_ids_spouses, 
          file="cross_check_ids_spouses.csv", row.names = FALSE)

write.csv(Lwengo_joined_6, 
          file="Lwengo_joined_6.csv", row.names = FALSE)

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
Lwengo_joined_6 = read_csv("Lwengo_joined_6.csv")



# SNAGS
Lwengo_SNAGS = read_excel("09.12.2021 LWENGO District SNAG LIST.xls", 
           sheet = "LWENGO DISTRICT 30.11.21 (3)")


## join with master list

SNAGS_Unaccounted_1 = anti_join(x = compare_interests_7,
                             y = Lwengo_SNAGS,
                             by = c("VALUATION_ASSESSMENT" = 
                                      "Reference_Number") )


SNAGS_Unaccounted_2 = anti_join(x = Lwengo_SNAGS,
                                y = compare_interests_7,
                                by = c("Reference_Number" =
                                         "VALUATION_ASSESSMENT" ) )

unique(SNAGS_Unaccounted_2$`Reference_Number`)


Lwengo_SNAGS$`Reference_Number`[which(Lwengo_SNAGS$`Reference_Number` ==
                     "\nLWE/LWE/KIN/L001A-00-00/4839")] =
                     "LWE/LWE/KIN/L001A-00-00/4839"


Lwengo_SNAGS$`Reference_Number`[which(Lwengo_SNAGS$`Reference_Number` ==
                                        "LWE/NDA/KIS-B/L001-00-00/4161")] =
                                          "LWE/NDA/KIB/L001-00-00/4161"

unique(SNAGS_Unaccounted_2$`Reference_Number`)


Lwengo_SNAGS_joined = left_join(x = compare_interests_7,
                                y = Lwengo_SNAGS,
                                by = c("VALUATION_ASSESSMENT" = 
                                         "Reference_Number") )


## With Duplicates Removed From SNAGS
Lwengo_SNAGS_joined = Lwengo_SNAGS_joined %>%
  distinct(VALUATION_ASSESSMENT, `Type_Of_SNAG`, .keep_all = TRUE)


write.csv(SNAGS_Unaccounted_2, 
          file="SNAGS_Unaccounted_2.csv", row.names = FALSE)

Lwengo_SNAGS_joined$VALUATION_ASSESSMENT[which(
  duplicated(Lwengo_SNAGS_joined$VALUATION_ASSESSMENT)
)]



library(sqldf)
check1 = sqldf("select PAP_NAME, VALUATION_ASSESSMENT, 
          group_concat(Remarks, '; ') Remarks, 
          group_concat(Type_Of_SNAG, '; ') Type_Of_SNAG
          from Lwengo_SNAGS_joined group by VALUATION_ASSESSMENT", 
          method = "raw")


check3 =
  Lwengo_SNAGS_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarise(Remarks = toString(Remarks)) %>%
  ungroup()



library(stringr)
check4 =
  Lwengo_SNAGS_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarize(Remarks = str_c(Remarks, collapse = "; "))


compare_interests_7 = sqldf("select 
           EACOP_PAP_REF, 
           PAP_NAME,                                                           
           VALUATION_ASSESSMENT,                                       
           DISTRICT,                                                         
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
           Name_of_PAP,
           Date,
           group_concat(Remarks, '; ') Remarks, 
           group_concat(Type_Of_SNAG, '; ') Type_Of_SNAG
           from Lwengo_SNAGS_joined group by VALUATION_ASSESSMENT", 
               method = "raw")


write.csv(compare_interests_7, 
          file="compare_interests_7.csv", row.names = FALSE)



# SECOND REVISED INFO FROM CENTENARY CORE BANKING SYSTEM ......

## Reloading ... 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)

compare_interests_4 = read_csv("compare_interests_4.csv")
compare_interests_5 = read_csv("compare_interests_5.csv")
compare_interests_6 = read_csv("compare_interests_6.csv")
compare_interests_7 = read_csv("compare_interests_7.csv")
Lwengo_joined_6 = read_csv("Lwengo_joined_6.csv")



# ***********************************************************
## GRIEVANCES
# ***********************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_7 = read_csv("compare_interests_7.csv")
Lwengo_joined_6 = read_csv("Lwengo_joined_6.csv")


Grievances_Lwengo = 
  read_excel("Grievances_Lwengo.xlsx", sheet = "Lwengo (2)",
             col_types = c("text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "date", "text", 
                                      "text", "text"))


Grievances_Lwengo[,c(4:8)] = 
  apply(Grievances_Lwengo[,c(4:8)], 2, toupper)


compare_interests_8_left = left_join(x = Grievances_Lwengo,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Lwengo a
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



### Correct PAP names

Grievances_Lwengo$`Name of grievant`[
  which(Grievances_Lwengo$`Name of grievant` == 
          "WALUGEMBE YASIN (SECOND GRIEVANCE)")
] = "WALUGEMBE YASIN"


### Correct Village Names

Grievances_Lwengo$Village[
  which(Grievances_Lwengo$`Name of grievant` == 
          "KYEYUNE FAROUK")
] = "KIBUNDAZA"


Grievances_Lwengo$Village[
  which(Grievances_Lwengo$`Name of grievant` == 
          "NAMATOVU ZAINA")
] = "NANSIITI"


Grievances_Lwengo$Village[
  which(Grievances_Lwengo$`Name of grievant` == 
          "NABYONGA MAURICIA")
] = "NKOMA B"


Grievances_Lwengo$Village[
  which(Grievances_Lwengo$`Name of grievant` == 
          "KIMERA DOMINICO")
] = "LUTOMA"




## Repeat ....

compare_interests_8_left = left_join(x = Grievances_Lwengo,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Lwengo a
                              left join compare_interests_7 b
                              on a.'Name of grievant' like '%'||b.PAP_NAME||'%'
                              WHERE 1;", 
                                     method = "raw")



length(unique(compare_interests_8_left$`Name of grievant`))

length(unique(compare_interests_8_left$PAP_NAME))

length(unique(compare_interests_8_left_sql$`Name of grievant`))

length(unique(compare_interests_8_left_sql$PAP_NAME))


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


check3 = check2[which(((check2$`Name of grievant` != check2$PAP_NAME) &
                         !(is.null(check2$PAP_NAME))) |
                        ((check2$Village != check2$VILLAGE) &
                           !(is.null(check2$VILLAGE)))), ]




### Adding to big list ...

Lwengo_joined_8 = left_join(x = Lwengo_joined_6,
                            y = Grievances_Lwengo,
                            by = c("PAP_NAME" = "Name of grievant",
                                    "VILLAGE" = "Village"),
                            keep = TRUE)


colnames(Lwengo_joined_6)[
  which(colnames(Lwengo_joined_6) == "ID NUMBER")] = "ID_NUMBER_2"

Lwengo_joined_8_sql = sqldf("select *
                           from Lwengo_joined_6 a
                           left join Grievances_Lwengo b
                           on a.PAP_NAME like '%'||b.'Name of grievant'||'%'
                           WHERE 1;", 
                              method = "raw")

length(unique(Lwengo_joined_8$PAP_VALUATION_ASSESSMENT_REF))


## The lines below require independent judgement
Duplicate_Interests = Lwengo_joined_8 %>%
  filter(duplicated(cbind(PAP_NAME, 
                          PAP_VALUATION_ASSESSMENT_REF)))


Lwengo_joined_8 = Lwengo_joined_8 %>%
  distinct(PAP_NAME, PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


compare_interests_8 = subset(Lwengo_joined_8,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE",
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
    "GRIEVANCE CLOSE OUT",
    "DATE OF GRIEVANCE CLOSE OUT")


write.csv(compare_interests_8, 
          file="compare_interests_8.csv", row.names = FALSE)

write.csv(Lwengo_joined_8, 
          file="Lwengo_joined_8.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_8 = read_csv("compare_interests_8.csv")
Lwengo_joined_8 = read_csv("Lwengo_joined_8.csv")



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


compare_interests_8$`DATE OF GRIEVANCE CLOSE OUT` = 
  as.character( compare_interests_8$`DATE OF GRIEVANCE CLOSE OUT`)



unique(SNAGS_All$DISTRICT)

SNAGS_Lwengo = subset(SNAGS_All, SNAGS_All$DISTRICT == "LWENGO")


## checking for unaccounted ...

SNAGS_Unaccounted_1a = anti_join(x = compare_interests_8,
                                 y = SNAGS_Lwengo,
                                 by = c("PAP_NAME" = "NAME OF PAP"))


SNAGS_Unaccounted_1b = sqldf("select * 
             from compare_interests_8 a
             where  not exists (
             select null from SNAGS_Lwengo b
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_1c = sqldf("select * 
             from compare_interests_8 a
             where  not exists (
             select null from SNAGS_Lwengo b
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")

SNAGS_Unaccounted_1b$PAP_NAME[
  which(!(SNAGS_Unaccounted_1b$PAP_NAME %in% SNAGS_Unaccounted_1c$PAP_NAME))]


SNAGS_Unaccounted_2a = anti_join(x = SNAGS_Lwengo,
                                 y = compare_interests_8,
                                 by = c("NAME OF PAP" =
                                          "PAP_NAME" ) )


SNAGS_Unaccounted_2b = sqldf("select * 
             from SNAGS_Lwengo b
             where  not exists (
             select null from compare_interests_8 a
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_2c = sqldf("select * 
             from SNAGS_Lwengo b
             where  not exists (
             select null from compare_interests_8 a
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")


SNAGS_Unaccounted_2b$`NAME OF PAP`[
  which(!(SNAGS_Unaccounted_2b$`NAME OF PAP` %in% 
            SNAGS_Unaccounted_2c$`NAME OF PAP` ))]

SNAGS_Unaccounted_2b$`ASSESSMENT REF.NO`[
  which(!(SNAGS_Unaccounted_2b$`ASSESSMENT REF.NO` %in% 
            SNAGS_Unaccounted_2c$`ASSESSMENT REF.NO` ))]


### Replace names ...

SNAGS_Lwengo$`NAME OF PAP`[which(SNAGS_Lwengo$`ASSESSMENT REF.NO` ==
                  "LWE/LWE/BIJ/L002-00-00/4691")] =
  
  compare_interests_8$PAP_NAME[
    which( compare_interests_8$VALUATION_ASSESSMENT == 
             "LWE/LWE/BIJ/L002-00-00/4691" ) ]


SNAGS_Lwengo$`NAME OF PAP`[which(SNAGS_Lwengo$`ASSESSMENT REF.NO` ==
                                   "LWE/LWE/KIN/L007-00-00/5180")] =
  
  compare_interests_8$PAP_NAME[
    which( compare_interests_8$VALUATION_ASSESSMENT == 
             "LWE/LWE/KIN/L007-00-00/5180" ) ]


SNAGS_Lwengo$`NAME OF PAP`[which(SNAGS_Lwengo$`PAP REF.NO.` ==
                                   "LWE/KAM/271")] =
  
  compare_interests_8$PAP_NAME[
    which( compare_interests_8$EACOP_PAP_REF == "LWE/KAM/271" ) ]


SNAGS_Unaccounted_2b$`ASSESSMENT REF.NO`[
  which(SNAGS_Unaccounted_2b$`ASSESSMENT REF.NO` %in% 
            SNAGS_Unaccounted_1b$VALUATION_ASSESSMENT )]


sapply( SNAGS_Unaccounted_2b$`ASSESSMENT REF.NO`[
  which(SNAGS_Unaccounted_2b$`ASSESSMENT REF.NO` %in% 
          SNAGS_Unaccounted_1b$VALUATION_ASSESSMENT )],
  
  FUN = function(assess_no){
    
    SNAGS_Lwengo$`NAME OF PAP`[which(SNAGS_Lwengo$`ASSESSMENT REF.NO` ==
                                       assess_no)] <<-
      
      compare_interests_8$PAP_NAME[
        which( compare_interests_8$VALUATION_ASSESSMENT == assess_no ) ]
    
    return(NULL)
    
  }
)


### Repeat check ...


### Replace more names ...

SNAGS_Unaccounted_2b$`PAP REF.NO.`[
  which(SNAGS_Unaccounted_2b$`PAP REF.NO.` %in% 
          SNAGS_Unaccounted_1b$EACOP_PAP_REF )]


sapply( SNAGS_Unaccounted_2b$`PAP REF.NO.`[
  which(SNAGS_Unaccounted_2b$`PAP REF.NO.` %in% 
          SNAGS_Unaccounted_1b$EACOP_PAP_REF )],
  
  FUN = function(reg_no){
    
    SNAGS_Lwengo$`NAME OF PAP`[which(SNAGS_Lwengo$`PAP REF.NO.` ==
                                       reg_no)] <<-
      
      compare_interests_8$PAP_NAME[
        which( compare_interests_8$EACOP_PAP_REF == reg_no ) ]
    
    return(NULL)
    
  }
)


### Repeat check ...


### Replace more names ...

sapply( SNAGS_Unaccounted_2b$`PAP REF.NO.`[
  which(!is.na(SNAGS_Unaccounted_2b$`PAP REF.NO.`))],
  
  FUN = function(reg_no){
    
    SNAGS_Lwengo$`NAME OF PAP`[which(SNAGS_Lwengo$`PAP REF.NO.` ==
                                       reg_no)] <<-
      
      compare_interests_8$PAP_NAME[
        which( compare_interests_8$EACOP_PAP_REF == reg_no ) ]
    
    return(NULL)
    
  }
)



### Finally joining ...

SNAGS_Lwengo_joined = left_join(x = compare_interests_8,
                                   y = SNAGS_Lwengo,
                                   by = c("PAP_NAME" = "NAME OF PAP"),
                                   keep = TRUE)


## With Duplicates Removed From SNAGS
SNAGS_Lwengo_joined = SNAGS_Lwengo_joined %>%
  distinct(VALUATION_ASSESSMENT, `SNAG ISSUE`, REMARKS,
           .keep_all = TRUE)


SNAGS_Lwengo_joined$VALUATION_ASSESSMENT[which(
  duplicated(SNAGS_Lwengo_joined$VALUATION_ASSESSMENT)
)]



### Rename columns

colnames(SNAGS_Lwengo_joined)[
  which(colnames(SNAGS_Lwengo_joined) == 
          "Date when snag was handled (If fieldwork was needed)")] = 
  "DATE_SNAG_Handled"


colnames(SNAGS_Lwengo_joined)[
  which(colnames(SNAGS_Lwengo_joined) == 
          "Was the snag issue closed out  (YES/NO)")] = 
  "SNAG_Status_Close_Out"



library(sqldf)
check1 = sqldf("select PAP_NAME, VALUATION_ASSESSMENT, 
          group_concat(REMARKS, '; ') REMARKS, 
          group_concat(`SNAG ISSUE`, '; ') `SNAG ISSUE`
          from SNAGS_Lwengo_joined group by VALUATION_ASSESSMENT", 
               method = "raw")


check2 =
  SNAGS_Lwengo_joined %>%
  group_by(EACOP_PAP_REF,
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
           NO_spouse) %>%
           summarise(REMARKS = toString(REMARKS)) 


library(stringr)
compare_interests_10 =
  SNAGS_Lwengo_joined %>%
  group_by(EACOP_PAP_REF,
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
           `GRIEVANCE CLOSE OUT`,
           `DATE OF GRIEVANCE CLOSE OUT`) %>%
           summarize(REMARKS = str_c(REMARKS, collapse = "; "),
                     `SNAG ISSUE` = str_c(`SNAG ISSUE`, collapse = "; "))


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

Vulnerable_Lwengo = read_excel("Vulnerable PAPs ALL Edited.xlsx", 
                                   sheet = "LWENGO")


## checking for unaccounted ...

Vulnerable_Unaccounted_1a = anti_join(x = compare_interests_10,
                                      y = Vulnerable_Lwengo,
                                      by = c("PAP_NAME" = "Name of PAP"))


Vulnerable_Unaccounted_1b = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Lwengo b
             where a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_1c = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Lwengo b
             where a.PAP_NAME like '%'||b.'Name of PAP'||'%' );", 
                                  method = "raw")

Vulnerable_Unaccounted_1b$PAP_NAME[
  which(!(Vulnerable_Unaccounted_1b$PAP_NAME %in% Vulnerable_Unaccounted_1c$PAP_NAME))]


Vulnerable_Unaccounted_2a = anti_join(x = Vulnerable_Lwengo,
                                      y = compare_interests_10,
                                      by = c("Name of PAP" = "PAP_NAME") )


Vulnerable_Unaccounted_2b = sqldf("select * 
             from Vulnerable_Lwengo b
             where  not exists (
             select null from compare_interests_10 a
             where  a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_2c = sqldf("select * 
             from Vulnerable_Lwengo b
             where  not exists (
             select null from compare_interests_10 a
             where a.PAP_NAME like '%'||b.'Name of PAP'||'%');", 
                                  method = "raw")


Vulnerable_Unaccounted_2b$`Name of PAP`[
  which(!(Vulnerable_Unaccounted_2b$`Name of PAP` %in% 
            Vulnerable_Unaccounted_2c$`Name of PAP` ))]


Vulnerable_Unaccounted_2b$`EACOP Ref.NUMBER`[
  which(!(Vulnerable_Unaccounted_2b$`Name of PAP` %in% 
            Vulnerable_Unaccounted_2c$`Name of PAP` ))]


## creating column for future reference ...

Vulnerable_Lwengo$Name_of_PAP_2 = Vulnerable_Lwengo$`Name of PAP`


## rename

Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                   "NAKIWALA KAMIYAT")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NAKIWALA KAMIYATI") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "KATEREGGA STEPHEN")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == 
            "KATEREGGA STEPHEN(ID)/KATEREGA STEPHEN(LT)") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "LWAKALENGWA VICENT")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == 
            "LWAKALENGWA VICENT(ID)/VICENT RWAKARENGU(LT)") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "LUBINGA FRANCIS")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == 
            "LUBINGA FRANCIS C/O NANYONDO LUCIA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`EACOP Ref.NUMBER` ==
                                        "LWE/LWE/050")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$EACOP_PAP_REF == "LWE/LWE/050") ]



Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`EACOP Ref.NUMBER` ==
                                        "LWE/LWE/065")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$EACOP_PAP_REF == "LWE/LUTO/065") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NANSIKOMBI MARGRET")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NANSIKOMBI MAGRET") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NAKACHWA KARAMANTU")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NAKACWA KARAMANTU") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "BALIZA ELIABU")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "BALIZA ERIABU") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "BALIZA ELIABU")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "BALIZA ERIABU") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`EACOP Ref.NUMBER` ==
                                        "LWE/NKO/112")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$EACOP_PAP_REF == "LWE/NKO/110") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                 "KIYIMBA ELVAIDA KORUKIIKO")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == 
            "KIYIMBA ELVAIDA KOLUKIIKO") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "BWANIKA ISMAH KIZZA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == 
            "BWANIKA ISMA KIZZA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "SSUJJIRA ELIA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "SUJJIRA ELIA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NAMPIJJA SAWUYA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NAMPIJJA SAUYA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NASSALI FLAZIA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NASSALI FARAZIA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "BUZOYA FALASCO")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "BUZOYA FARASCO") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "BAMUHARANA SCOLA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "BAMUHARANA SCHOLA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NANTE MAULICIA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NANTE MAULISIA C/O MUGULA SWAIBU") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "GAVUMUKULYA TADEO")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "GAVAMUKULYA TADEO") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`EACOP Ref.NUMBER` ==
                                        "LWE/BUS/364")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$EACOP_PAP_REF == "LWE/BUS/361") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "LUBEGA PASTORE")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "LUBEGA PASTOLE") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NAJJEMBA IMMACULATE")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NAJEMBA IMMACULATE") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`EACOP Ref.NUMBER` ==
                                        "LWE/NANS/383")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$EACOP_PAP_REF == "LWE/NANS/380") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NASSAAZI EVA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NASAAZI EVA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NAMUKASA GETULIDA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NAMUKASA GETULIDDA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NASSAMULA FIRIDA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NASAMULA FIRIDA") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "KAFEERO ELENESTI")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "KAFEERO ELENESITI") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NAKAWEESI FLORENCE")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NAKAWESI FLORENCE") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NAKAVANDA MARIA GORETH")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NAKAVANDA MARIA GORRETH") ]


Vulnerable_Lwengo$`Name of PAP`[which(Vulnerable_Lwengo$`Name of PAP` ==
                                        "NANSAMBA MARY")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NANSAMBA MARRY") ]



### Finally joining ...

Lwengo_joined_11 = left_join(x = compare_interests_10,
                                 y = Vulnerable_Lwengo,
                                 by = c("PAP_NAME" = 
                                          "Name of PAP"),
                                 keep = TRUE)


## With Duplicates Removed From Vulnerable
Lwengo_joined_11 = Lwengo_joined_11 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Lwengo_joined_11$VALUATION_ASSESSMENT[which(
  duplicated(Lwengo_joined_11$VALUATION_ASSESSMENT)
)]


check1 = subset(Lwengo_joined_11, 
                select = c("PAP_NAME", 
                           "Name_of_PAP_2",
                           "EACOP_PAP_REF",
                           "EACOP Ref.NUMBER"))

colnames(Lwengo_joined_11)[
  which(startsWith(colnames(Lwengo_joined_11), 
                   prefix = "Description of") )] = "Vulnerability"

colnames(Lwengo_joined_11)[
  which(colnames(Lwengo_joined_11) ==
          "Comment")] =
  "Comment_Vulnerability"


compare_interests_11 = 
  subset(Lwengo_joined_11, 
         select = colnames(Lwengo_joined_11)[c(1:24,31,32,33)] )

write.csv(compare_interests_11, 
          file="compare_interests_11.csv", row.names = FALSE)

write.csv(Lwengo_joined_11, 
          file="Lwengo_joined_11.csv", row.names = FALSE)



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
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-23-00/4849")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-23-00/4849")]




### Update 2 ***************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "LWE/NDA/KIB/L003-02-00/2225")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/NDA/KIB/L003-02-00/2225")]


## save update
write.csv(compare_interests_11, 
          file="compare_interests_11.csv", row.names = FALSE)


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

R_H_Lwengo = subset(Replacement_Housing,
                      subset = (Replacement_Housing$DISTRICT ==
                                  "LWENGO"))

colnames(R_H_Lwengo)
colnames(compare_interests_11)



## checking for unaccounted ...

R_H_Unaccounted_1a = anti_join(x = compare_interests_11,
                               y = R_H_Lwengo,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "PAP VALUATION ASSESSMENT REF"))


R_H_Unaccounted_1b = sqldf("select * 
             from compare_interests_11 a
             where  not exists (
             select null from R_H_Lwengo b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%' );", 
                           method = "raw")


R_H_Unaccounted_1a$PAP_NAME[
  which(!(R_H_Unaccounted_1a$PAP_NAME %in% 
            R_H_Unaccounted_1b$PAP_NAME))]


R_H_Unaccounted_2a = anti_join(x = R_H_Lwengo,
                               y = compare_interests_11,
                               by = c("PAP VALUATION ASSESSMENT REF" = 
                                        "VALUATION_ASSESSMENT") )


R_H_Unaccounted_2b = sqldf("select * 
             from R_H_Lwengo b
             where  not exists (
             select null from compare_interests_11 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%');", 
                           method = "raw")


R_H_Unaccounted_2b$`PAP REF NO`[
  which(!(R_H_Unaccounted_2b$`PAP REF NO` %in% 
            R_H_Unaccounted_2a$`PAP REF NO` ))]



### Finally joining ...

Lwengo_joined_12 = left_join(x = compare_interests_11,
                               y = R_H_Lwengo,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "PAP VALUATION ASSESSMENT REF"),
                               keep = TRUE)


## With Duplicates Removed
Lwengo_joined_12 = Lwengo_joined_12 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Lwengo_joined_12$VALUATION_ASSESSMENT[which(
  duplicated(Lwengo_joined_12$VALUATION_ASSESSMENT)
)]


check1 = subset(Lwengo_joined_12, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "EACOP_PAP_REF",
                           "PAP REF NO",
                           "VALUATION_ASSESSMENT",
                           "PAP VALUATION ASSESSMENT REF"))


colnames(Lwengo_joined_12)[
  which(colnames(Lwengo_joined_12) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Lwengo_joined_12)[
  which(colnames(Lwengo_joined_12) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_12 = 
  subset(Lwengo_joined_12, 
         select = colnames(Lwengo_joined_12)[c(1:27,33:34)] )

write.csv(compare_interests_12, 
          file="compare_interests_12.csv", row.names = FALSE)

write.csv(Lwengo_joined_12, 
          file="Lwengo_joined_12.csv", row.names = FALSE)



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

Unmarked_Graves_Lwengo = subset(Unmarked_Graves,
                                   subset = (Unmarked_Graves$DISTRICT ==
                                               "LWENGO"))

Marked_Graves_Lwengo = subset(Marked_Graves,
                                 subset = (Marked_Graves$DISTRICT ==
                                             "LWENGO"))


colnames(Unmarked_Graves_Lwengo)
colnames(compare_interests_12)


## checking for unaccounted ...

U_G_Unaccounted_1a = anti_join(x = compare_interests_12,
                               y = Unmarked_Graves_Lwengo,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


U_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_12 a
             where  not exists (
             select null from Unmarked_Graves_Lwengo b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


U_G_Unaccounted_1a$PAP_NAME[
  which(!(U_G_Unaccounted_1a$PAP_NAME %in% 
            U_G_Unaccounted_1b$PAP_NAME))]


U_G_Unaccounted_2a = anti_join(x = Unmarked_Graves_Lwengo,
                               y = compare_interests_12,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


U_G_Unaccounted_2b = sqldf("select * 
             from Unmarked_Graves_Lwengo b
             where  not exists (
             select null from compare_interests_12 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


U_G_Unaccounted_2b$`PAP NAME`[
  which(!(U_G_Unaccounted_2b$`PAP NAME` %in% 
            U_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Lwengo_joined_13a = left_join(x = compare_interests_12,
                                 y = Unmarked_Graves_Lwengo,
                                 by = c("VALUATION_ASSESSMENT" = 
                                          "ASSESSMENT REFERENCE NUMBER"),
                                 keep = TRUE)


## With Duplicates Removed
Lwengo_joined_13a = Lwengo_joined_13a %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Lwengo_joined_13a$VALUATION_ASSESSMENT[which(
  duplicated(Lwengo_joined_13a$VALUATION_ASSESSMENT)
)]


check1 = subset(Lwengo_joined_13a, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Lwengo_joined_13a)[
  which(colnames(Lwengo_joined_13a) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Lwengo_joined_13a)[
  which(colnames(Lwengo_joined_13a) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13a = 
  subset(Lwengo_joined_13a, 
         select = colnames(Lwengo_joined_13a)[c(1:29,33)] )



## checking for unaccounted ...

colnames(Marked_Graves_Lwengo)

M_G_Unaccounted_1a = anti_join(x = compare_interests_13a,
                               y = Marked_Graves_Lwengo,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


M_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_13a a
             where  not exists (
             select null from Marked_Graves_Lwengo b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


M_G_Unaccounted_1a$PAP_NAME[
  which(!(M_G_Unaccounted_1a$PAP_NAME %in% 
            M_G_Unaccounted_1b$PAP_NAME))]


M_G_Unaccounted_2a = anti_join(x = Marked_Graves_Lwengo,
                               y = compare_interests_13a,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


M_G_Unaccounted_2b = sqldf("select * 
             from Marked_Graves_Lwengo b
             where  not exists (
             select null from compare_interests_13a a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


M_G_Unaccounted_2b$`PAP NAME`[
  which(!(M_G_Unaccounted_2b$`PAP NAME` %in% 
            M_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Lwengo_joined_13b = left_join(x = compare_interests_13a,
                                 y = Marked_Graves_Lwengo,
                                 by = c("VALUATION_ASSESSMENT" = 
                                          "ASSESSMENT REFERENCE NUMBER"),
                                 keep = TRUE)


## With Duplicates Removed
Lwengo_joined_13b = Lwengo_joined_13b %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Lwengo_joined_13b$VALUATION_ASSESSMENT[which(
  duplicated(Lwengo_joined_13b$VALUATION_ASSESSMENT)
)]


check1 = subset(Lwengo_joined_13b, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Lwengo_joined_13b)[
  which(colnames(Lwengo_joined_13b) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Lwengo_joined_13b)[
  which(colnames(Lwengo_joined_13b) == "SUBCOUNTY.x")] =
  "SUBCOUNTY"

colnames(Lwengo_joined_13b)[
  which(colnames(Lwengo_joined_13b) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13b = 
  subset(Lwengo_joined_13b, 
         select = colnames(Lwengo_joined_13b)[c(1:30,34)] )


compare_interests_13 = compare_interests_13b

Lwengo_joined_13 = Lwengo_joined_13b


write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)

write.csv(Lwengo_joined_13, 
          file="Lwengo_joined_13.csv", row.names = FALSE)




### *** May 19th 2022 bank information update ***

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_13 = read_csv("compare_interests_13.csv")
All_IWA_inner = read_csv("All_IWA_inner_May_18.csv")

colnames(compare_interests_13)
colnames(All_IWA_inner)


### Update 1 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "LWE/LWE/NKO/L007A-09-00/1383")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "LWE/LWE/NKO/L007A-09-00/1383")]



### Update 2 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$`PAP REF NO` ==  "LWE/KIB/241")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/241")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/241")]




### Update 3 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$`PAP REF NO` ==  "LWE/KIB/242")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$EACOP_PAP_REF == "LWE/KIB/242")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$`PAP REF NO` == "LWE/KIB/242")]



## save update
write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_13 = read_csv("compare_interests_13.csv")
All_IWA_inner = read_csv("All_IWA_inner_May_18.csv")



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

check_POA = compare_interests_13[
  which(!is.na(compare_interests_13$POA) &
          !is.na(compare_interests_13$`Revocation of Verbal POA`))
  
,]

check_POA_2 = check_POA

check_POA_2$POA = NA


compare_interests_13[
  which(!is.na(compare_interests_13$POA) &
          !is.na(compare_interests_13$`Revocation of Verbal POA`))
  
  ,]$POA = NA


which(!is.na(compare_interests_13$POA) &
        !is.na(compare_interests_13$`Revocation of Verbal POA`))


## save update
write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)




setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_13 = read_csv("compare_interests_13.csv")


