

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)


# Gomba


## data of Gomba Only from NewPlan
Gomba_Control = read_excel("Gomba_assignment_2.xlsx", 
                                sheet = "Gomba_NewPlan")

## Number of Interests Control
length(unique(Gomba_Control$PAP_VALUATION_ASSESSMENT_REF))



## data from Total and Bank
Gomba_From_Bank = read_excel("Gomba_assignment_2.xlsx", 
                                     sheet = "Gomba_From_Bank")

## Remove empty rows
Gomba_From_Bank = 
  Gomba_From_Bank[!is.na(Gomba_From_Bank$`PAP VALUATION ASSESSMENT REFS`), ]


## Number of Interests From Bank Without Duplicates
length(unique(Gomba_From_Bank$`PAP VALUATION ASSESSMENT REFS`))



## names
colnames(Gomba_Control)
colnames(Gomba_From_Bank)

Gomba_Control$VILLAGE[which((Gomba_Control$PAP_NAME == "SUBI CHRISTOPHER") &
                            (Gomba_Control$VILLAGE == "KYENGERA") )] = 
                      "KYETUME"



# Joining


## left join
Gomba_joined = left_join(x = Gomba_Control, 
                           y = Gomba_From_Bank, 
                           by = c("PAP_NAME" = "PAP NAME",
                                  "VILLAGE" = "VILLAGE"))

Gomba_Unaccounted_1 = anti_join(x = Gomba_Control, 
                                y = Gomba_From_Bank, 
                                by = c("PAP_NAME" = "PAP NAME",
                                       "VILLAGE" = "VILLAGE"))


Gomba_Unaccounted_2 = anti_join(x = Gomba_From_Bank, 
                                y = Gomba_Control, 
                                by = c("PAP NAME" = "PAP_NAME",
                                      "VILLAGE" = "VILLAGE"))


#### --- The following lines only apply if there is "Unaccounted_2"




### --- Repeat process ...



# Compare Interests
colnames(Gomba_joined)
colnames(Gomba_joined)[which(colnames(Gomba_joined) == 
    "CENTENARY BANK ACCOUNT NAME(s):  \r\n(PAP and Spouse or Joint PAPs)")] = 
    "CENTENARY BANK ACCOUNT NAME(s):"

compare_interests_1 = subset(Gomba_joined,
                             select = c("PAP_NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REFS",
                                        "PAP REF NO",
                                        "EACOP PAP REF NO.",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE")) 


compare_interests_2 = subset(Gomba_joined,
                           select = c("PAP_NAME",
                                      "PAP_VALUATION_ASSESSMENT_REF",
                                      "PAP VALUATION ASSESSMENT REFS",
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
Gomba_BAO = read_excel("Gomba_assignment_2.xlsx", sheet = "Gomba_BAO")


colnames(compare_interests_2)
colnames(Gomba_BAO)

Gomba_BAO$PAP_name_No_Dash = gsub(pattern = "-", 
                                  replacement = " ",
                                  x = Gomba_BAO$PAP_name,
                                  ignore.case = TRUE)

Gomba_BAO$spouse_name_No_Dash = gsub(pattern = "-", 
                                  replacement = " ",
                                  x = Gomba_BAO$spouse_name,
                                  ignore.case = TRUE)


## Remove empty rows
Gomba_BAO = 
  Gomba_BAO[!is.na(Gomba_BAO$PAP_name_No_Dash), ]


# Joining with Mathias
Gomba_joined_3 = left_join(x = compare_interests_2, 
                              y = Gomba_BAO, 
                              by = c("PAP_NAME" = "PAP_name_No_Dash",
                                     "VILLAGE" = "village"))

colnames(Gomba_joined_3)

Gomba_Unaccounted_3_1 = anti_join(x = compare_interests_2, 
                                  y = Gomba_BAO, 
                                  by = c("PAP_NAME" = "PAP_name_No_Dash",
                                       "VILLAGE" = "village"))


Gomba_Unaccounted_3_2 = anti_join(x = Gomba_BAO, 
                                  y = compare_interests_2, 
                                  by = c("PAP_name_No_Dash" = "PAP_NAME",
                                       "village" = "VILLAGE"))


Gomba_BAO$PAP_name_No_Dash[which(Gomba_BAO$PAP_name == 
      "STEPHEN-MAZIMPAKA-LT-MAIZIMPAKA-STEVEN-ID")] = 
  "STEPHEN MAZIMPAKA(LT)/MAIZIMPAKA STEVEN(ID)"

Gomba_BAO$PAP_name_No_Dash[which(Gomba_BAO$PAP_name == 
   "EPHRAIM-KAZOORA-RWAMATENESI-LT-RWAMATENESI-EPHRAIM-ID")] = 
  "EPHRAIM KAZOORA RWAMATENESI(LT)/RWAMATENESI EPHRAIM(ID)"

### --- Repeat join operation

### --- Repeat join after changing names


Gomba_joined_3_sql = sqldf("select *
                      from compare_interests_2 a
                      left join Gomba_BAO b
                      on a.PAP_NAME like '%'||b.PAP_name_No_Dash||'%'
                      WHERE 1;", 
                            method = "raw")

Gomba_joined_3_sql =  Gomba_joined_3_sql %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)



compare_interests_3 = subset(Gomba_joined_3,
      select = c("PAP REF NO",
                 "EACOP PAP REF NO.",
                 "PAP_NAME",
                 "PAP_name",
                 "PAP_VALUATION_ASSESSMENT_REF",
                 "PAP VALUATION ASSESSMENT REFS",
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

Gomba_joined_4_left = sqldf("select *
                      from compare_interests_3 a
                      left join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                               method = "raw")


Gomba_joined_4_inner = sqldf("select *
                      from compare_interests_3 a
                      inner join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                                method = "raw")


Interests_Without_Accounts = sqldf("select * 
             from Gomba_joined_4_left a
             where  not exists (
             select null from Gomba_joined_4_inner b
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


## --- After editing file ...


Lost_and_Found_new = read_excel("Gomba_Feb_10_2022.xlsx", 
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


Gomba_joined_4_left = 
  Gomba_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Gomba_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
  Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF

which(Gomba_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
        Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)


check =
  Gomba_joined_4_left[
    (which(Gomba_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
             Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


Gomba_joined_4_left =
  Gomba_joined_4_left[
    -(which(Gomba_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
              Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


colnames(Lost_and_Found_joined) = colnames(Gomba_joined_4_left)

Gomba_joined_4_left = 
  rbind(Gomba_joined_4_left,
        Lost_and_Found_joined)

write.csv(Gomba_joined_4_left, 
          file="Gomba_joined_4_left.csv", row.names = FALSE)


Gomba_joined_4_left = 
  Gomba_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

Gomba_joined_4 = Gomba_joined_4_left


#### ---- checking missing bank account names

missed_bank_account_names = subset(Gomba_joined_4,
    subset = ( (is.na(Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
    (Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
    (Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") ))


Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`[
  which( (is.na(Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
         (Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] =
  
Gomba_joined_4$ACCOUNT_DESCRIPTION[
  which( (is.na(Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
           (Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Gomba_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] 


### ---- checking bank account numbers

cross_check_bank_accounts = Gomba_joined_4[
  which(nchar(Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`


#### check account numbers in centenary bank system
Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in bank system
Gomba_joined_4$PAP_NAME[which(
  Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account numbers in Mathias survey data
Gomba_joined_4$`centenary-bankaccount`[which(
  Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in centenary bank system name description
Gomba_joined_4$PAP_NAME[which(
  Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )] %in%
  centenary_core_needed$ACCOUNT_DESCRIPTION


### --- Correct Bank Accounts

### This is perhaps no loner needed in the latest code.


### --- Repeat check for bank account numbers

cross_check_bank_accounts = Gomba_joined_4[
  which(nchar(Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Gomba_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`



#  some data from server
data_extra_server = read_excel("EACOP PAP NATIONAL ID.xlsx", 
                               sheet = "Sheet2")


colnames(Gomba_joined_4)
colnames(data_extra_server)


# Join Again
Gomba_joined_5 = left_join(x = Gomba_joined_4, 
                            y = data_extra_server, 
                            by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                     "ASSESSMENT REFERENCE NUMBER"))


colnames(Gomba_joined_5)


compare_interests_4 = subset(Gomba_joined_5,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "EACOP PAP REF NO.", 
                                        "PAP_NAME",
                                        "PAP_name2", 
                                        "PAP NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",                                       
                                        "PAP VALUATION ASSESSMENT REFS",                                      
                                        "DISTRICT.x",                                                         
                                        "SUBCOUNTY",                                                          
                                        "VILLAGE",                                                          
                                        "Type of ID",                                                         
                                        "type_ID",
                                        "ID_NO",
                                        "ID Number",
                                        "ID_NO_1",
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
                                        "TELEPHONE_1",
                                        "ADDRESS_1")) 





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

### The code that should be here may no longer be necessary.


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
                                    "ID_NO_1",
                                    "ID_NO" ))


length(which(nchar(ids_found_with_mathias$ID_NO) == 14))

length(which(nchar(ids_found_with_mathias$ID_NO) != 14))

cross_check_ids_with_mathias = ids_found_with_mathias[
  which(nchar(ids_found_with_mathias$ID_NO) != 14), ]

cross_check_ids_with_mathias$ID_NO
nchar(cross_check_ids_with_mathias$ID_NO)


### --- Correct IDs from Mathias

### The code that should be here may no longer be necessary.


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

length(which(nchar(Gomba_joined$`ID Number`) != 14))

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


### The code here about change of National IDs may no longer be necessary


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


t = unique(compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER)
t = t[-which(t=="0"|is.na(t))]
t



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


## --- some shared account numbers

unique_names[["3203066208"]]


## Disputed accounts do not apply in this case of Gomba




########### ---------------------------------------- ################


# SECTION TWO ... NOT FROM SCRATCH ...


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)

compare_interests_4 = read_csv("compare_interests_4.csv")
compare_interests_5 = read_csv("compare_interests_5.csv")


# SUB-SECTION ....


## Disputed accounts do not apply in this case of Gomba



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

### This update is not necessary



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


### There is no disputed accounts



write.csv(Unaccounted_bank_account_numbers, 
          file="Unaccounted_bank_account_numbers.csv", row.names = FALSE)


write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)

write.csv(compare_interests_5, 
          file="compare_interests_5.csv", row.names = FALSE)



# SPOUSES .... SUB-SECTION

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)

compare_interests_4 = read_csv("compare_interests_4.csv")
compare_interests_5 = read_csv("compare_interests_5.csv")


## Spouses data from Mathias BAO
Spouses_BAO = read_excel("Gomba_assignment_2.xlsx", 
                         sheet = "Spouses_BAO")


Spouses_BAO$village_No_Dash = gsub(pattern = "-", 
                                   replacement = " ",
                                   x = Spouses_BAO$village,
                                   ignore.case = TRUE)


Spouses_BAO$spouse_name_No_Dash = gsub(pattern = "-", 
                                       replacement = " ",
                                       x = Spouses_BAO$spouse_name,
                                       ignore.case = TRUE)


Spouses_BAO$PAP_name_No_Dash = gsub(pattern = "-", 
                                   replacement = " ",
                                   x = Spouses_BAO$PAP_name,
                                   ignore.case = TRUE)


Spouses_BAO$spouse_name_No_Dash = toupper(Spouses_BAO$spouse_name_No_Dash)
Spouses_BAO$PAP_name_No_Dash = toupper(Spouses_BAO$PAP_name_No_Dash)
Spouses_BAO$subcounty = toupper(Spouses_BAO$subcounty)
Spouses_BAO$village_No_Dash = toupper(Spouses_BAO$village_No_Dash)


## Spouses data from Control
Spouses_Control = read_excel("Gomba_assignment_2.xlsx", 
                             sheet = "Spouses_Control")



## join 

Spouses_BAO$PAP_name_No_Dash[which(Spouses_BAO$PAP_name == 
                "STEPHEN-MAZIMPAKA-LT-MAIZIMPAKA-STEVEN-ID")] = 
  "STEPHEN MAZIMPAKA(LT)/MAIZIMPAKA STEVEN(ID)"


Spouses_BAO$PAP_name_No_Dash[which(Spouses_BAO$PAP_name == 
              "EPHRAIM-KAZOORA-RWAMATENESI-LT-RWAMATENESI-EPHRAIM-ID")] = 
  "EPHRAIM KAZOORA RWAMATENESI(LT)/RWAMATENESI EPHRAIM(ID)"


Spouses_Control$VILLAGE[which((Spouses_Control$`PAP NAME` 
                               == "SUBI CHRISTOPHER") &
                              (Spouses_Control$VILLAGE == 
                                 "KYENGERA") )] = "KYETUME"




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
                                       "PAPhve_spouse"))



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


Gomba_joined_6 = Spouses_joined_3

colnames(Gomba_joined_6)

colnames(Gomba_joined_6)[which(
  colnames(Gomba_joined_6) == "PAP_name2")] = "PAP_name.x"

colnames(Gomba_joined_6)[which(
  colnames(Gomba_joined_6) == "PAP_name")] = "PAP_name.y"

compare_interests_6 = subset(Gomba_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "EACOP PAP REF NO.",
                                        "PAP REF NO",
                                        "PAP_RefNo",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REFS",
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


write.csv(Gomba_joined_6, 
          file="Gomba_joined_6.csv", row.names = FALSE)

write.csv(compare_interests_6, 
          file="compare_interests_6.csv", row.names = FALSE)



## Fill in Spouse ID column ....


missed_PAP_names = subset(compare_interests_6,
              subset = ( (is.na(compare_interests_6$PAP_name.x)) |
                       (compare_interests_6$PAP_name.x == "N/A") |
                       (compare_interests_6$PAP_name.x == "N A") |
                       (compare_interests_6$PAP_name.x == "NA") ))


Gomba_joined_6$PAP_name.x[
  which( (is.na(Gomba_joined_6$PAP_name.x)) |
           (Gomba_joined_6$PAP_name.x == "N/A") |
           (Gomba_joined_6$PAP_name.x == "N A") |
           (Gomba_joined_6$PAP_name.x == "NA") )] =
  
  Gomba_joined_6$PAP_name.y[
    which( (is.na(Gomba_joined_6$PAP_name.x)) |
             (Gomba_joined_6$PAP_name.x == "N/A") |
             (Gomba_joined_6$PAP_name.x == "N A") |
             (Gomba_joined_6$PAP_name.x == "NA") )] 



missed_spouse_names = subset(compare_interests_6,
      subset = ( (is.na(compare_interests_6$spouse_name_No_Dash.x)) |
               (compare_interests_6$spouse_name_No_Dash.x == "N/A") |
               (compare_interests_6$spouse_name_No_Dash.x == "N A") |
               (compare_interests_6$spouse_name_No_Dash.x == "NA") ))


Gomba_joined_6$spouse_name_No_Dash.x[
  which( (is.na(Gomba_joined_6$spouse_name_No_Dash.x)) |
           (Gomba_joined_6$spouse_name_No_Dash.x == "N/A") |
           (Gomba_joined_6$spouse_name_No_Dash.x == "N A") |
           (Gomba_joined_6$spouse_name_No_Dash.x == "NA") )] =
  
  Gomba_joined_6$spouse_name_No_Dash.y[
    which( (is.na(Gomba_joined_6$spouse_name_No_Dash.x)) |
             (Gomba_joined_6$spouse_name_No_Dash.x == "N/A") |
             (Gomba_joined_6$spouse_name_No_Dash.x == "N A") |
             (Gomba_joined_6$spouse_name_No_Dash.x == "NA") )] 



missed_spouse_names_main = subset(compare_interests_6,
      subset = ( (is.na(compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") ))


Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`[
  which( (is.na(Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
           (Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
           (Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
           (Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )] =
  
  Gomba_joined_6$spouse_name_No_Dash.x[
    which( (is.na(Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
             (Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
             (Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
             (Gomba_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )]



missed_spouse_IDs = subset(compare_interests_6,
                subset = ( (is.na(compare_interests_6$`spouse-IDno.x`)) |
                (compare_interests_6$`spouse-IDno.x` == "N/A") |
                (compare_interests_6$`spouse-IDno.x` == "N A") |
                (compare_interests_6$`spouse-IDno.x` == "NA") ))


Gomba_joined_6$`spouse-IDno.x`[
  which( (is.na(Gomba_joined_6$`spouse-IDno.x`)) |
           (Gomba_joined_6$`spouse-IDno.x` == "N/A") |
           (Gomba_joined_6$`spouse-IDno.x` == "N A") |
           (Gomba_joined_6$`spouse-IDno.x` == "NA") )] =
  
  Gomba_joined_6$`spouse-IDno.y`[
    which( (is.na(Gomba_joined_6$`spouse-IDno.x`)) |
             (Gomba_joined_6$`spouse-IDno.x` == "N/A") |
             (Gomba_joined_6$`spouse-IDno.x` == "N A") |
             (Gomba_joined_6$`spouse-IDno.x` == "NA") )] 



Gomba_joined_6$NO_spouse.x[
  which( (is.na(Gomba_joined_6$NO_spouse.x)) |
           (Gomba_joined_6$NO_spouse.x == "N/A") |
           (Gomba_joined_6$NO_spouse.x == "N A") |
           (Gomba_joined_6$NO_spouse.x == "NA") )] =
  
  Gomba_joined_6$NO_spouse.y[
    which( (is.na(Gomba_joined_6$NO_spouse.x)) |
             (Gomba_joined_6$NO_spouse.x == "N/A") |
             (Gomba_joined_6$NO_spouse.x == "N A") |
             (Gomba_joined_6$NO_spouse.x == "NA") )] 



Gomba_joined_6$PAPhve_spouse.x[
  which( (is.na(Gomba_joined_6$PAPhve_spouse.x)) |
           (Gomba_joined_6$PAPhve_spouse.x == "N/A") |
           (Gomba_joined_6$PAPhve_spouse.x == "N A") |
           (Gomba_joined_6$PAPhve_spouse.x == "NA") )] =
  
  Gomba_joined_6$PAPhve_spouse.y[
    which( (is.na(Gomba_joined_6$PAPhve_spouse.x)) |
             (Gomba_joined_6$PAPhve_spouse.x == "N/A") |
             (Gomba_joined_6$PAPhve_spouse.x == "N A") |
             (Gomba_joined_6$PAPhve_spouse.x == "NA") )] 



### Some extra code here ...


# Check spouse IDs ....


nchar(Gomba_joined_6$`spouse-IDno.x`)

length(which(nchar(compare_interests_6$`spouse-IDno.x`) == 14))

length(which(nchar(compare_interests_6$`spouse-IDno.x`) != 14))

cross_check_ids_spouses = compare_interests_6[
  which(  (nchar(compare_interests_6$`spouse-IDno.x`) != 14) &
            ((startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                         prefix = "CF") |
                startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                           prefix = "CM") ))), ]



### --- Correct IDs

### More code needed here ...



compare_interests_6 = subset(Gomba_joined_6,
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
                                        "PAP VALUATION ASSESSMENT REFS")  )



compare_interests_7 = subset(Gomba_joined_6,
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

write.csv(Gomba_joined_6, 
          file="Gomba_joined_6.csv", row.names = FALSE)

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
Gomba_joined_6 = read_csv("Gomba_joined_6.csv")



# ***********************************************************
## GRIEVANCES
# ***********************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_7 = read_csv("compare_interests_7.csv")
Gomba_joined_6 = read_csv("Gomba_joined_6.csv")


Grievances_Gomba = read_excel("Grievances_Gomba.xlsx", 
                                   sheet = "Gomba (2)")


Grievances_Gomba[,c(4:8)] = 
  apply(Grievances_Gomba[,c(4:8)], 2, toupper)


compare_interests_8_left = left_join(x = Grievances_Gomba,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Gomba a
                              left join compare_interests_7 b
                              on a.'Name of grievant' like '%'||b.PAP_NAME||'%'
                              WHERE 1;", 
                                     method = "raw")



length(unique(compare_interests_8_left$`Name of grievant`))

length(unique(compare_interests_8_left$PAP_NAME))

check = subset(compare_interests_8_left, select = c("Name of grievant",
                                                    "PAP_NAME",
                                                    "Village",
                                                    "VILLAGE"))


check2 = subset(compare_interests_8_left_sql, select = c("Name of grievant",
                                                    "PAP_NAME",
                                                    "Village",
                                                    "VILLAGE"))


### Correct PAP names

Grievances_Gomba$`Name of grievant`[
  which(Grievances_Gomba$`Name of grievant` == "ASIIMWE GEOFFREY")
] = "ASIIMWE GEOFREY"


Grievances_Gomba$`Name of grievant`[
  which(Grievances_Gomba$`Name of grievant` == "NUWABINE TOMAS")
] = "NUWABEINE TOMASI"


Grievances_Gomba$`Name of grievant`[
  which(Grievances_Gomba$`Name of grievant` == "KATEERAH MWESIGWA MARTIN")
] = "MWESIGWA MARTIN KATEERAH"





## Repeat ....

compare_interests_8_left = left_join(x = Grievances_Gomba,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Gomba a
                              left join compare_interests_7 b
                              on a.'Name of grievant' like '%'||b.PAP_NAME||'%'
                              WHERE 1;", 
                                     method = "raw")


length(unique(compare_interests_8_left$`Name of grievant`))

length(unique(compare_interests_8_left$PAP_NAME))

check = subset(compare_interests_8_left, select = c("Name of grievant",
                                                    "PAP_NAME",
                                                    "Village",
                                                    "VILLAGE",
                                                    "VALUATION_ASSESSMENT"))


check2 = subset(compare_interests_8_left_sql, select = c("Name of grievant",
                                                         "PAP_NAME",
                                                         "Village",
                                                         "VILLAGE"))



### Adding to big list ...

Gomba_joined_8 = left_join(x = Gomba_joined_6,
                                y = Grievances_Gomba,
                                by = c("PAP_NAME" = "Name of grievant",
                                       "VILLAGE" = "Village"),
                                keep = TRUE)


compare_interests_8 = subset(Gomba_joined_8,
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

write.csv(Gomba_joined_8, 
          file="Gomba_joined_8.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)

compare_interests_8 = read_csv("compare_interests_8.csv")
Gomba_joined_8 = read_csv("Gomba_joined_8.csv")



## **********************************************
## DISCLOSURE 
## **********************************************

Disclosure_Gomba = 
  read_excel("28.11.2021 Gomba District Disclosure Statistics Tracker.xlsx", 
             sheet = "Disclosed to PAPs & Spouses")


Unaccounted_disclosure_1 = 
  anti_join(x = compare_interests_8, 
            y = Disclosure_Gomba, 
            by = c("VALUATION_ASSESSMENT" = 
                     "PAP Reference Number in approved VR"))


Unaccounted_disclosure_2 = 
  anti_join(x = Disclosure_Gomba,
            y = compare_interests_8,
            by = c("PAP Reference Number in approved VR" = 
                     "VALUATION_ASSESSMENT"))


###  Corrections ...

Disclosure_Gomba$`PAP Reference Number in approved VR`[
  which(Disclosure_Gomba$`PAP Reference Number in approved VR` == 
          "GOM/MAD/KYET/L003A-06-01/1167")
] = "GOM/MAD/KYEN/L003A-06-00/1167"



Unaccounted_disclosure_1 = 
  anti_join(x = compare_interests_8, 
            y = Disclosure_Gomba, 
            by = c("VALUATION_ASSESSMENT" = 
                     "PAP Reference Number in approved VR"))


Unaccounted_disclosure_2 = 
  anti_join(x = Disclosure_Gomba,
            y = compare_interests_8,
            by = c("PAP Reference Number in approved VR" = 
                     "VALUATION_ASSESSMENT"))



Gomba_joined_9 = 
  left_join(x = Gomba_joined_8, 
            y = Disclosure_Gomba, 
            by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                     "PAP Reference Number in approved VR"),
            keep = TRUE)


compare_interests_9 = subset(Gomba_joined_9,
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
                                        "Date of Disclosure",
                                        "PAP Disclosed to their values",
                                        "Closed out",
                                        "Date of close out"))



colnames(compare_interests_9) = 
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
    "DATE OF DISCLOSURE",
    "DISCLOSURE DONE",
    "GRIEVANCE STATUS",
    "DATE OF GRIEVANCE CLOSE OUT")


write.csv(compare_interests_9, 
          file="compare_interests_9.csv", row.names = FALSE)

write.csv(Gomba_joined_9, 
          file="Gomba_joined_9.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_9 = read_csv("compare_interests_9.csv")
Gomba_joined_9 = read_csv("Gomba_joined_9.csv")



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

compare_interests_9$`DATE OF DISCLOSURE` = 
  as.character( compare_interests_9$`DATE OF DISCLOSURE`)


compare_interests_9$`DATE OF GRIEVANCE CLOSE OUT` = 
  as.character( compare_interests_9$`DATE OF GRIEVANCE CLOSE OUT`)



unique(SNAGS_All$DISTRICT)

SNAGS_Gomba = subset(SNAGS_All, SNAGS_All$DISTRICT == "GOMBA")


## checking for unaccounted ...

SNAGS_Unaccounted_1a = anti_join(x = compare_interests_9,
                                y = SNAGS_Gomba,
                                by = c("PAP_NAME" = "NAME OF PAP"))


SNAGS_Unaccounted_1b = sqldf("select * 
             from compare_interests_9 a
             where  not exists (
             select null from SNAGS_Gomba b
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                                   method = "raw")


SNAGS_Unaccounted_1c = sqldf("select * 
             from compare_interests_9 a
             where  not exists (
             select null from SNAGS_Gomba b
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")




SNAGS_Unaccounted_2a = anti_join(x = SNAGS_Gomba,
                                y = compare_interests_9,
                                by = c("NAME OF PAP" =
                                         "PAP_NAME" ) )


SNAGS_Unaccounted_2b = sqldf("select * 
             from SNAGS_Gomba b
             where  not exists (
             select null from compare_interests_9 a
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_2c = sqldf("select * 
             from SNAGS_Gomba b
             where  not exists (
             select null from compare_interests_9 a
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")


SNAGS_Gomba$`NAME OF PAP`[which(SNAGS_Gomba$`ASSESSMENT REF.NO` ==
    "GOM/MAD/KYET/L003-00-00/830")] =
  
compare_interests_9$PAP_NAME[
  which(compare_interests_9$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYET/L003-00-00/830")]



#### Repeat cycle ...


SNAGS_Gomba_joined = left_join(x = compare_interests_9,
                               y = SNAGS_Gomba,
                               by = c("PAP_NAME" = "NAME OF PAP"),
                               keep = TRUE)


## >> recent
Gomba_joined_10 = left_join(x = Gomba_joined_9,
                            y = SNAGS_Gomba,
                            by = c("PAP_NAME" = "NAME OF PAP"),
                            keep = TRUE)


## With Duplicates Removed From SNAGS
SNAGS_Gomba_joined = SNAGS_Gomba_joined %>%
  distinct(VALUATION_ASSESSMENT, `SNAG ISSUE`, REMARKS,
           .keep_all = TRUE)


SNAGS_Gomba_joined$VALUATION_ASSESSMENT[which(
  duplicated(SNAGS_Gomba_joined$VALUATION_ASSESSMENT)
)]



## >> recent
Gomba_joined_10 = Gomba_joined_10 %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, `SNAG ISSUE`, REMARKS,
           .keep_all = TRUE)




library(sqldf)
check1 = sqldf("select PAP_NAME, VALUATION_ASSESSMENT, 
          group_concat(REMARKS, '; ') REMARKS, 
          group_concat(`SNAG ISSUE`, '; ') `SNAG ISSUE`
          from SNAGS_Gomba_joined group by VALUATION_ASSESSMENT", 
               method = "raw")


check2 =
  SNAGS_Gomba_joined %>%
  group_by(PAP_NAME, VALUATION_ASSESSMENT) %>%
  summarise(REMARKS = toString(REMARKS),
            `SNAG ISSUE` = toString(`SNAG ISSUE`) ) 

colnames(SNAGS_Gomba_joined)


library(stringr)
check3 =
  SNAGS_Gomba_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarize(REMARKS = str_c(REMARKS, collapse = "; "),
            `SNAG ISSUE` = str_c(`SNAG ISSUE`, collapse = "; ") )


## >> 
snag_info =
  Gomba_joined_10 %>%
  group_by(PAP_VALUATION_ASSESSMENT_REF) %>%
  summarise(REMARKS = toString(REMARKS),
            `SNAG ISSUE` = toString(`SNAG ISSUE`) )




### Rename columns

colnames(SNAGS_Gomba_joined)[
  which(colnames(SNAGS_Gomba_joined) == 
          "Date when snag was handled (If fieldwork was needed)")] = 
  "DATE_SNAG_Handled"


colnames(SNAGS_Gomba_joined)[
  which(colnames(SNAGS_Gomba_joined) == 
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
    `DATE OF DISCLOSURE`,
    `DISCLOSURE DONE`,
    `GRIEVANCE STATUS`,
    `DATE OF GRIEVANCE CLOSE OUT`,
    DATE_Snag_Handled,
    SNAG_Status_Close_Out,
    group_concat(REMARKS, '; ') REMARKS, 
    group_concat(`SNAG ISSUE`, '; ') `SNAG ISSUE`
    from SNAGS_Gomba_joined group by VALUATION_ASSESSMENT", 
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

Vulnerable_Gomba = read_excel("Vulnerable PAPs ALL Edited.xlsx", 
                                  sheet = "GOMBA")


## checking for unaccounted ...

Vulnerable_Unaccounted_1a = anti_join(x = compare_interests_10,
                                      y = Vulnerable_Gomba,
                                      by = c("PAP_NAME" = "Name of PAP"))


Vulnerable_Unaccounted_1b = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Gomba b
             where a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_1c = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Gomba b
             where a.PAP_NAME like '%'||b.'Name of PAP'||'%' );", 
                                  method = "raw")

Vulnerable_Unaccounted_1b$PAP_NAME[
  which(!(Vulnerable_Unaccounted_1b$PAP_NAME %in% Vulnerable_Unaccounted_1c$PAP_NAME))]


Vulnerable_Unaccounted_2a = anti_join(x = Vulnerable_Gomba,
                                      y = compare_interests_10,
                                      by = c("Name of PAP" = "PAP_NAME") )


Vulnerable_Unaccounted_2b = sqldf("select * 
             from Vulnerable_Gomba b
             where  not exists (
             select null from compare_interests_10 a
             where  a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_2c = sqldf("select * 
             from Vulnerable_Gomba b
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

Vulnerable_Gomba$Name_of_PAP_2 = Vulnerable_Gomba$`Name of PAP`


## special case ...

write.csv(Vulnerable_Unaccounted_2c, 
          file="Vulnerable_Unaccounted_2c.csv", row.names = FALSE)



### Finally joining ...

Gomba_joined_11 = sqldf("select *
                         from compare_interests_10 a
                         left join Vulnerable_Gomba b
                         on a.PAP_NAME like '%'||b.'Name of PAP'||'%';", 
                              method = "raw")


## With Duplicates Removed From Vulnerable
Gomba_joined_11 = Gomba_joined_11 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Gomba_joined_11$VALUATION_ASSESSMENT[which(
  duplicated(Gomba_joined_11$VALUATION_ASSESSMENT)
)]


check1 = subset(Gomba_joined_11, 
                select = c("PAP_NAME", 
                           "Name_of_PAP_2",
                           "EACOP_PAP_REF",
                           "EACOP Ref.NUMBER"))

colnames(Gomba_joined_11)[
  which(startsWith(colnames(Gomba_joined_11), 
                   prefix = "Description of") )] = "Vulnerability"

colnames(Gomba_joined_11)[ 
  which(colnames(Gomba_joined_11) == "Comment")] = 
  "Comment_Vulnerability"


compare_interests_11 = 
  subset(Gomba_joined_11, 
         select = colnames(Gomba_joined_11)[c(1:28,35,36)] )

write.csv(compare_interests_11, 
          file="compare_interests_11.csv", row.names = FALSE)

write.csv(Gomba_joined_11, 
          file="Gomba_joined_11.csv", row.names = FALSE)




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

R_H_Gomba = subset(Replacement_Housing,
                    subset = (Replacement_Housing$DISTRICT ==
                                "GOMBA"))

colnames(R_H_Gomba)
colnames(compare_interests_11)



## checking for unaccounted ...

R_H_Unaccounted_1a = anti_join(x = compare_interests_11,
                               y = R_H_Gomba,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "PAP VALUATION ASSESSMENT REF"))


R_H_Unaccounted_1b = sqldf("select * 
             from compare_interests_11 a
             where  not exists (
             select null from R_H_Gomba b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%' );", 
                           method = "raw")


R_H_Unaccounted_1a$PAP_NAME[
  which(!(R_H_Unaccounted_1a$PAP_NAME %in% 
            R_H_Unaccounted_1b$PAP_NAME))]


R_H_Unaccounted_2a = anti_join(x = R_H_Gomba,
                               y = compare_interests_11,
                               by = c("PAP VALUATION ASSESSMENT REF" = 
                                        "VALUATION_ASSESSMENT") )


R_H_Unaccounted_2b = sqldf("select * 
             from R_H_Gomba b
             where  not exists (
             select null from compare_interests_11 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%');", 
                           method = "raw")


R_H_Unaccounted_2b$`PAP REF NO`[
  which(!(R_H_Unaccounted_2b$`PAP REF NO` %in% 
            R_H_Unaccounted_2a$`PAP REF NO` ))]



### Finally joining ...

Gomba_joined_12 = left_join(x = compare_interests_11,
                             y = R_H_Gomba,
                             by = c("VALUATION_ASSESSMENT" = 
                                      "PAP VALUATION ASSESSMENT REF"),
                             keep = TRUE)


## With Duplicates Removed
Gomba_joined_12 = Gomba_joined_12 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Gomba_joined_12$VALUATION_ASSESSMENT[which(
  duplicated(Gomba_joined_12$VALUATION_ASSESSMENT)
)]


check1 = subset(Gomba_joined_12, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "EACOP_PAP_REF",
                           "PAP REF NO",
                           "VALUATION_ASSESSMENT",
                           "PAP VALUATION ASSESSMENT REF"))


colnames(Gomba_joined_12)[
  which(colnames(Gomba_joined_12) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Gomba_joined_12)[
  which(colnames(Gomba_joined_12) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_12 = 
  subset(Gomba_joined_12, 
         select = colnames(Gomba_joined_12)[c(1:30,36:37)] )

write.csv(compare_interests_12, 
          file="compare_interests_12.csv", row.names = FALSE)

write.csv(Gomba_joined_12, 
          file="Gomba_joined_12.csv", row.names = FALSE)



## ************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_12 = read_csv("compare_interests_12.csv")

Gomba_joined_12 = read_csv("Gomba_joined_12.csv")


## **********************************************
## GRAVES
## **********************************************


Unmarked_Graves = 
  read_excel("GRAVES.xlsx", sheet = "Unmarked_Graves")

Marked_Graves = 
  read_excel("GRAVES.xlsx", sheet = "Marked_Graves")

unique(Unmarked_Graves$DISTRICT)

unique(Marked_Graves$DISTRICT)

Unmarked_Graves_Gomba = subset(Unmarked_Graves,
                                  subset = (Unmarked_Graves$DISTRICT ==
                                              "GOMBA"))

Marked_Graves_Gomba = subset(Marked_Graves,
                                subset = (Marked_Graves$DISTRICT ==
                                            "GOMBA"))


colnames(Unmarked_Graves_Gomba)
colnames(compare_interests_12)


## checking for unaccounted ...

U_G_Unaccounted_1a = anti_join(x = compare_interests_12,
                               y = Unmarked_Graves_Gomba,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


U_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_12 a
             where  not exists (
             select null from Unmarked_Graves_Gomba b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


U_G_Unaccounted_1a$PAP_NAME[
  which(!(U_G_Unaccounted_1a$PAP_NAME %in% 
            U_G_Unaccounted_1b$PAP_NAME))]


U_G_Unaccounted_2a = anti_join(x = Unmarked_Graves_Gomba,
                               y = compare_interests_12,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


U_G_Unaccounted_2b = sqldf("select * 
             from Unmarked_Graves_Gomba b
             where  not exists (
             select null from compare_interests_12 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


U_G_Unaccounted_2b$`PAP NAME`[
  which(!(U_G_Unaccounted_2b$`PAP NAME` %in% 
            U_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Gomba_joined_13a = left_join(x = compare_interests_12,
                                y = Unmarked_Graves_Gomba,
                                by = c("VALUATION_ASSESSMENT" = 
                                         "ASSESSMENT REFERENCE NUMBER"),
                                keep = TRUE)


## With Duplicates Removed
Gomba_joined_13a = Gomba_joined_13a %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Gomba_joined_13a$VALUATION_ASSESSMENT[which(
  duplicated(Gomba_joined_13a$VALUATION_ASSESSMENT)
)]


check1 = subset(Gomba_joined_13a, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Gomba_joined_13a)[
  which(colnames(Gomba_joined_13a) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Gomba_joined_13a)[
  which(colnames(Gomba_joined_13a) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13a = 
  subset(Gomba_joined_13a, 
         select = colnames(Gomba_joined_13a)[
           c(1:ncol(compare_interests_12),
            which(colnames(Gomba_joined_13a) == "NO OF UNMARKED GRAVES"))] )



## checking for unaccounted ...

colnames(Marked_Graves_Gomba)

M_G_Unaccounted_1a = anti_join(x = compare_interests_13a,
                               y = Marked_Graves_Gomba,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


M_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_13a a
             where  not exists (
             select null from Marked_Graves_Gomba b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


M_G_Unaccounted_1a$PAP_NAME[
  which(!(M_G_Unaccounted_1a$PAP_NAME %in% 
            M_G_Unaccounted_1b$PAP_NAME))]


M_G_Unaccounted_2a = anti_join(x = Marked_Graves_Gomba,
                               y = compare_interests_13a,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


M_G_Unaccounted_2b = sqldf("select * 
             from Marked_Graves_Gomba b
             where  not exists (
             select null from compare_interests_13a a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


M_G_Unaccounted_2b$`PAP NAME`[
  which(!(M_G_Unaccounted_2b$`PAP NAME` %in% 
            M_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Gomba_joined_13b = left_join(x = compare_interests_13a,
                                y = Marked_Graves_Gomba,
                                by = c("VALUATION_ASSESSMENT" = 
                                         "ASSESSMENT REFERENCE NUMBER"),
                                keep = TRUE)


## With Duplicates Removed
Gomba_joined_13b = Gomba_joined_13b %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Gomba_joined_13b$VALUATION_ASSESSMENT[which(
  duplicated(Gomba_joined_13b$VALUATION_ASSESSMENT)
)]


check1 = subset(Gomba_joined_13b, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Gomba_joined_13b)[
  which(colnames(Gomba_joined_13b) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Gomba_joined_13b)[
  which(colnames(Gomba_joined_13b) == "SUBCOUNTY.x")] =
  "SUBCOUNTY"

colnames(Gomba_joined_13b)[
  which(colnames(Gomba_joined_13b) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13b = 
  subset(Gomba_joined_13b, 
         select = colnames(Gomba_joined_13b)[
           c(1:ncol(compare_interests_13a), 
             which(colnames(Gomba_joined_13b) == "NO OF MARKED GRAVES"))] )


compare_interests_13 = compare_interests_13b

Gomba_joined_13 = Gomba_joined_13b


write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)

write.csv(Gomba_joined_13, 
          file="Gomba_joined_13.csv", row.names = FALSE)




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
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "GOM/MAD/KYEN/L003A-07-00/1166")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "GOM/MAD/KYEN/L003A-07-00/1166")]



## save update
write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)




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




