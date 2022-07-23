
options(error = rlang::entrace)
utils:::make.packages.html(temp = TRUE)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)


# Kakumiro


## data of Kakumiro Only from NewPlan
Kakumiro_Control = read_excel("Kakumiro_assignment_2.xlsx", 
                                sheet = "Kakumiro_NewPlan")

## Number of Interests Control
length(unique(Kakumiro_Control$PAP_VALUATION_ASSESSMENT_REF))



## data from Total and Bank
Kakumiro_From_Bank = read_excel("Kakumiro_assignment_2.xlsx", 
                                     sheet = "Kakumiro_From_Bank (2)")

## Remove empty rows
Kakumiro_From_Bank = 
  Kakumiro_From_Bank[!is.na(Kakumiro_From_Bank$`PAP VALUATION ASSESSMENT REF`), ]


## Number of Interests From Bank Without Duplicates
length(unique(Kakumiro_From_Bank$`PAP VALUATION ASSESSMENT REF`))



## names
colnames(Kakumiro_Control)
colnames(Kakumiro_From_Bank)



# Joining


## left join
Kakumiro_joined = left_join(x = Kakumiro_Control, 
                           y = Kakumiro_From_Bank, 
                           by = c("PAP_NAME" = "PAP NAME",
                                  "VILLAGE" = "VILLAGE"))

Unaccounted_1 = anti_join(x = Kakumiro_Control, 
                          y = Kakumiro_From_Bank, 
                          by = c("PAP_NAME" = "PAP NAME",
                                   "VILLAGE" = "VILLAGE"))


Unaccounted_2 = anti_join(x = Kakumiro_From_Bank, 
                          y = Kakumiro_Control, 
                          by = c("PAP NAME" = "PAP_NAME",
                               "VILLAGE" = "VILLAGE"))


unique(Unaccounted_2$VILLAGE)

unique(Kakumiro_Control$VILLAGE)

which(Kakumiro_From_Bank$VILLAGE == "Kasambya")

Kakumiro_From_Bank$VILLAGE[which(Kakumiro_From_Bank$VILLAGE == 
                                   "Kasambya")] = "KASAMBYA"


Kakumiro_joined = left_join(x = Kakumiro_Control, 
                            y = Kakumiro_From_Bank, 
                            by = c("PAP_NAME" = "PAP NAME",
                                   "VILLAGE" = "VILLAGE"))

Unaccounted_1 = anti_join(x = Kakumiro_Control, 
                          y = Kakumiro_From_Bank, 
                          by = c("PAP_NAME" = "PAP NAME",
                                 "VILLAGE" = "VILLAGE"))


Unaccounted_2 = anti_join(x = Kakumiro_From_Bank, 
                          y = Kakumiro_Control, 
                          by = c("PAP NAME" = "PAP_NAME",
                                 "VILLAGE" = "VILLAGE"))

unique(Unaccounted_2$`PAP NAME`)

unique(Unaccounted_1$PAP_NAME)

which(Kakumiro_From_Bank$`PAP NAME` == "VINCENT TIBEMANYA")

Kakumiro_From_Bank$`PAP NAME`[which(Kakumiro_From_Bank$`PAP NAME` == 
          "VINCENT TIBEMANYA")] = "TIBEMANYA VINCENT"


### Repeat process ...



# Compare Interests
colnames(Kakumiro_joined)
colnames(Kakumiro_joined)[19] = "CENTENARY BANK ACCOUNT NAME(s):"

compare_interests_1 = subset(Kakumiro_joined,
                             select = c("PAP_NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REF",
                                        "PAP REF NO",
                                        "EACOP PAP REF NO.",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE"))  



compare_interests_2 = subset(Kakumiro_joined,
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
Kakumiro_BAO = read_excel("Kakumiro Analyzed Nov-24-2021.xlsx", 
                                     sheet = "Kakumiro BAO")

## Remove empty rows
Kakumiro_BAO = 
  Kakumiro_BAO[!is.na(Kakumiro_BAO$PAP_name_No_Dash), ]



# Joining with Mathias
colnames(compare_interests_2)
colnames(Kakumiro_BAO)


Kakumiro_joined_3 = left_join(x = compare_interests_2, 
                              y = Kakumiro_BAO, 
                              by = c("PAP_NAME" = "PAP_name_No_Dash",
                                     "VILLAGE" = "village"))


colnames(Kakumiro_joined_3)


compare_interests_3 = subset(Kakumiro_joined_3,
      select = c("PAP_NAME",
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
                  "account-type",
                  "Did PAP (+ SPOUSE) attend Bank A/C training and FLT? (Y/N)",
                  "PAP-spouse-banktraing",
                  "COMMENTS:",
                  "PAP REF NO",
                  "EACOP PAP REF NO.")) 



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


colnames(compare_interests_3)[
  which(colnames(compare_interests_3) == "PAP_name")] = "PAP_name2"

Kakumiro_joined_4_left = sqldf("select *
                      from compare_interests_3 a
                      left join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                                 method = "raw")


Kakumiro_joined_4_inner = sqldf("select *
                      from compare_interests_3 a
                      inner join centenary_core_needed b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                                  method = "raw")


Interests_Without_Accounts = sqldf("select * 
             from Kakumiro_joined_4_left a
             where  not exists (
             select null from Kakumiro_joined_4_inner b
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

Interests_Without_Accounts = read_excel("Kakumiro_Feb_3_2022.xlsx", 
                                        sheet = "Interests_Without_Accounts")

Lost_and_Found_new = read_excel("Kakumiro_Feb_3_2022.xlsx", 
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


Kakumiro_joined_4_left = 
  Kakumiro_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Kakumiro_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
  Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF

which(Kakumiro_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
        Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)


check =
  Kakumiro_joined_4_left[
    (which(Kakumiro_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
             Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


Kakumiro_joined_4_left =
  Kakumiro_joined_4_left[
    -(which(Kakumiro_joined_4_left$PAP_VALUATION_ASSESSMENT_REF %in%
              Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


colnames(Lost_and_Found_joined) = colnames(Kakumiro_joined_4_left)

Kakumiro_joined_4_left = 
  rbind(Kakumiro_joined_4_left,
        Lost_and_Found_joined)

write.csv(Kyankwanzi_joined_4_left, 
          file="Kyankwanzi_joined_4_left.csv", row.names = FALSE)


Kakumiro_joined_4_left = 
  Kakumiro_joined_4_left %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

Kakumiro_joined_4 = Kakumiro_joined_4_left


#### ---- checking missing bank account names

missed_bank_account_names = subset(Kakumiro_joined_4,
  subset = ( (is.na(Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
           (Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") ))


Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`[
  which( (is.na(Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
           (Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] =
  
  Kakumiro_joined_4$ACCOUNT_DESCRIPTION[
    which( (is.na(Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
             (Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
             (Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] 


#### ---- checking bank account numbers

cross_check_bank_accounts = Kakumiro_joined_4[
  which(nchar(Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`

#### check account numbers in centenary bank system
Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in bank system
Kakumiro_joined_4$PAP_NAME[which(
  Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account numbers in Mathias survey data
Kakumiro_joined_4$`centenary-bankaccount`[which(
  Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in centenary bank system name description
Kakumiro_joined_4$PAP_NAME[which(
  Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )] %in%
  centenary_core_needed$ACCOUNT_DESCRIPTION


### --- Correct Bank Accounts

Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "320305765")] = "3203065765"

Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Kakumiro_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "320306565")] = "3203065655"


############# NOTES ON ISSUES ################
# The ACCOUNT DESCRITPTION found in the banking system is not consistent.
# Sometimes the ACCOUNT DESCRIPTION is based on FIRST NAME, then SIRNAME, or
# the SPOUSE NAME added or the reverse SIRNAME, then FIRST NAME.



#  some data from server
data_extra_server = read_excel("EACOP PAP NATIONAL ID.xlsx", 
                               sheet = "Sheet2")


colnames(Kakumiro_joined_4)
colnames(data_extra_server)

# Join Again
compare_interests_4 = left_join(x = Kakumiro_joined_4, 
                                y = data_extra_server, 
                                by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                         "ASSESSMENT REFERENCE NUMBER"))


############# NOTES ####################
## Our own REF NOs are not consistent


colnames(compare_interests_4)

compare_interests_4 = subset(compare_interests_4,
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


nchar(ids_found_on_server$`ID NUMBER`)

cross_check_ids_on_server = ids_found_on_server[
  which(nchar(ids_found_on_server$`ID NUMBER`) != 14), ]

length(which(nchar(ids_found_on_server$`ID NUMBER`) == 14))

nchar(cross_check_ids_on_server$`ID NUMBER`)



### --- Correct IDs

compare_interests_4$`ID NUMBER`[which(
  compare_interests_4$`ID NUMBER` ==
    "CM9015101NAKH")] = "CM90105101NAKH"



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


check_ids_again = subset(compare_interests_4,
                         select = c("ID Number",
                                    "ID_NO",
                                    "ID NUMBER") )



## -- crosscheck ids from bank

nchar(compare_interests_4$`ID Number`)

cross_check_ids_from_bank = compare_interests_4[
  which( (nchar(compare_interests_4$`ID Number`) != 14) &
    ((startsWith(toupper(compare_interests_4$`ID Number`), 
                prefix = "CF") |
       startsWith(toupper(compare_interests_4$`ID Number`), 
                  prefix = "CM") )) ), ]


compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CM62009102W8D")] = "CM6200910G2W8D"


write.csv(cross_check_ids_from_bank , 
          file="cross_check_ids_from_bank .csv", row.names = FALSE)



# REMOVE DUPLICATES ...

length(unique(compare_interests_4$PAP_VALUATION_ASSESSMENT_REF))

## Duplicate Interests
Duplicate_Interests = 
  compare_interests_4[
    duplicated(compare_interests_4$PAP_VALUATION_ASSESSMENT_REF), ]


## With Duplicates Removed From Interests
compare_interests_4 = compare_interests_4 %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


## Special Case Bagala Susan
special_case = Duplicate_Interests[
  which(Duplicate_Interests$PAP_NAME == "BAGALA SUZAN"), ]

### Therefore ...
compare_interests_4 = rbind(compare_interests_4, special_case)



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


unique_names[["3203064730"]]

unique_names[["NIN NOT FOUND IN EC WEBSITE, CUSTOMER TO PROVIDE RIGHT NIN"]]

unique_names[["NIN NOT PROVIDED, CUSTOMER TO PROVIDE RIGHT NIN"]]



Disputed_Accounts = subset(compare_interests_5,
                           compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
                             "3203064730" )

write.csv(Disputed_Accounts, 
          file="Disputed_Accounts.csv", row.names = FALSE)




########### ---------------------------------------- ################


# SECTION TWO ... NOT FROM SCRATCH ...


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)


compare_interests_5 = read_csv("compare_interests_5.csv")

Disputed_Accounts = subset(compare_interests_5,
                           compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
                             "3203064730" )



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
compare_interests_4 = read_csv("compare_interests_4.csv")

colnames(wrong_account_numbers)
colnames(compare_interests_4)

compare_interests_4$`ID Number`[ which(
  (compare_interests_4$PAP_NAME == "MUHAIRWE HARISI") & 
  (compare_interests_4$PAP_VALUATION_ASSESSMENT_REF ==
    "KAK/KAT/KYA-A/L006-00-01/1005") &
  (compare_interests_4$SUBCOUNTY == "KATIKARA") &
  (compare_interests_4$VILLAGE == "KYAKAJORO A") 
  ) ] = "CF4900910FHELL"



write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)


length(unique(compare_interests_4$`PAP'S SPOUSE NAME(s):`))
length(unique(compare_interests_4$spouse_name_No_Dash))




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
            by = c("CENTENARY_BANK_ACCOUNT_NUMBER" = 
                     "ACCOUNT_NUMBER"))


wrong_account_numbers = subset(Unaccounted_bank_account_numbers,
        (startsWith(toupper(
        Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER), 
                    prefix = "3") |
        startsWith(toupper(
        Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER), 
                    prefix = "32") ) )


Disputed_Accounts = subset(compare_interests_5,
                           compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
                             "3203064730" )



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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)


## Spouses data from Mathias BAO
Spouses_BAO = read_excel("Kakumiro Analyzed Nov-24-2021.xlsx", 
                          sheet = "Spouses BAO")


Spouses_BAO$village_No_Dash = gsub(pattern = "-", 
                                   replacement = " ",
                                   x = Spouses_BAO$village,
                                   ignore.case = TRUE)

Spouses_BAO$PAP_name_No_Dash = toupper(Spouses_BAO$PAP_name_No_Dash)
Spouses_BAO$subcounty = toupper(Spouses_BAO$subcounty)
Spouses_BAO$village_No_Dash = toupper(Spouses_BAO$village_No_Dash)


## Spouses data from Control
Spouses_Control = read_excel("Kakumiro Analyzed Nov-24-2021.xlsx", 
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
                                       "spouse_name_2",
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
                 "spouse_name_2",
                 "spouse-IDno",
                 "DISTRICT",
                 "SUBCOUNTY",
                 "VILLAGE",
                 "NO_spouse",
                 "PAPhve_spouse") )



# join with master list

Spouses_joined_3 = left_join(x = compare_interests_4,
                              y = Spouses_compared_2,
                              by = c("PAP_NAME" = "PAP NAME",
                                     "SUBCOUNTY" = "SUBCOUNTY",
                                     "VILLAGE" = "VILLAGE") )


Spouses_joined_4 = left_join(x = compare_interests_4,
                             y = Spouses_compared_2,
                             by = c("PAP_NAME" = "PAP NAME",
                                    "VILLAGE" = "VILLAGE") )


Unaccounted_spouses_3 = anti_join(x = compare_interests_4,
                                y = Spouses_compared_2,
                                by = c("PAP_NAME" = "PAP NAME",
                                       "SUBCOUNTY" = "SUBCOUNTY",
                                       "VILLAGE" = "VILLAGE") )

Unaccounted_spouses_3.2 = anti_join(x = Spouses_compared_2, 
                                  y = compare_interests_4,
                                  by = c("PAP NAME" = "PAP_NAME",
                                         "SUBCOUNTY" = "SUBCOUNTY",
                                         "VILLAGE" = "VILLAGE") )


Unaccounted_spouses_4 = anti_join(x = compare_interests_4,
                                  y = Spouses_compared_2,
                                  by = c("PAP_NAME" = "PAP NAME",
                                         "VILLAGE" = "VILLAGE") )


Unaccounted_spouses_4.2 = anti_join(x = Spouses_compared_2, 
                                    y = compare_interests_4,
                                    by = c("PAP NAME" = "PAP_NAME",
                                           "VILLAGE" = "VILLAGE") )



## Therefore .......

Kakumiro_joined_6 = Spouses_joined_4

colnames(Kakumiro_joined_6)

colnames(Kakumiro_joined_6)[which(
  colnames(Kakumiro_joined_6) == "PAP_name2")] = "PAP_name.x"

colnames(Kakumiro_joined_6)[which(
  colnames(Kakumiro_joined_6) == "PAP_name")] = "PAP_name.y"


compare_interests_6 = subset(Kakumiro_joined_6,
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
                   "spouse_name_No_Dash",
                   "spouse_name_2", 
                   "spouse-IDno.x",
                   "spouse-IDno.y",
                   "DISTRICT.x",
                   "SUBCOUNTY.x",
                   "VILLAGE",
                   "PAPhve_spouse.x", 
                   "PAPhve_spouse.y",
                   "NO_spouse.x",
                   "NO_spouse.y")  )


write.csv(Kakumiro_joined_6, 
          file="Kakumiro_joined_6.csv", row.names = FALSE)

write.csv(compare_interests_6, 
          file="compare_interests_6.csv", row.names = FALSE)



## Fill in Spouse ID column ....


missed_PAP_names = subset(compare_interests_6,
      subset = ( (is.na(compare_interests_6$PAP_name.x)) |
      (compare_interests_6$PAP_name.x == "N/A") |
      (compare_interests_6$PAP_name.x == "NA") ))


Kakumiro_joined_6$PAP_name.x[
  which( (is.na(Kakumiro_joined_6$PAP_name.x)) |
           (Kakumiro_joined_6$PAP_name.x == "N/A") |
           (Kakumiro_joined_6$PAP_name.x == "NA") )] =
  
  Kakumiro_joined_6$PAP_name.y[
    which( (is.na(Kakumiro_joined_6$PAP_name.x)) |
             (Kakumiro_joined_6$PAP_name.x == "N/A") |
             (Kakumiro_joined_6$PAP_name.x == "NA") )] 



missed_spouse_names = subset(compare_interests_6,
        subset = ( (is.na(compare_interests_6$spouse_name_No_Dash)) |
                  (compare_interests_6$spouse_name_No_Dash == "N/A") |
                  (compare_interests_6$spouse_name_No_Dash == "NA") ))


Kakumiro_joined_6$spouse_name_No_Dash[
  which( (is.na(Kakumiro_joined_6$spouse_name_No_Dash)) |
           (Kakumiro_joined_6$spouse_name_No_Dash == "N/A") |
           (Kakumiro_joined_6$spouse_name_No_Dash == "NA") )] =
  
  Kakumiro_joined_6$spouse_name_2[
    which( (is.na(Kakumiro_joined_6$spouse_name_No_Dash)) |
             (Kakumiro_joined_6$spouse_name_No_Dash == "N/A") |
             (Kakumiro_joined_6$spouse_name_No_Dash == "NA") )] 



missed_spouse_names_main = subset(compare_interests_6,
   subset = ( (is.na(compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
   (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
   (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
   (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") ))


Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`[
  which( (is.na(Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
           (Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
           (Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
           (Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )] =
  
  Kakumiro_joined_6$spouse_name_No_Dash[
    which( (is.na(Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
             (Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
             (Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
             (Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )]



missed_spouse_IDs = subset(compare_interests_6,
            subset = ( (is.na(compare_interests_6$`spouse-IDno.x`)) |
            (compare_interests_6$`spouse-IDno.x` == "N/A") |
            (compare_interests_6$`spouse-IDno.x` == "N A") |
            (compare_interests_6$`spouse-IDno.x` == "NA") ))


Kakumiro_joined_6$`spouse-IDno.x`[
  which( (is.na(Kakumiro_joined_6$`spouse-IDno.x`)) |
           (Kakumiro_joined_6$`spouse-IDno.x` == "N/A") |
           (Kakumiro_joined_6$`spouse-IDno.x` == "N A") |
           (Kakumiro_joined_6$`spouse-IDno.x` == "NA") )] =
  
  Kakumiro_joined_6$`spouse-IDno.y`[
    which( (is.na(Kakumiro_joined_6$`spouse-IDno.x`)) |
             (Kakumiro_joined_6$`spouse-IDno.x` == "N/A") |
             (Kakumiro_joined_6$`spouse-IDno.x` == "N A") |
             (Kakumiro_joined_6$`spouse-IDno.x` == "NA") )] 



Kakumiro_joined_6$NO_spouse.x[
  which( (is.na(Kakumiro_joined_6$NO_spouse.x)) |
           (Kakumiro_joined_6$NO_spouse.x == "N/A") |
           (Kakumiro_joined_6$NO_spouse.x == "NA") )] =
  
  Kakumiro_joined_6$NO_spouse.y[
    which( (is.na(Kakumiro_joined_6$NO_spouse.x)) |
             (Kakumiro_joined_6$NO_spouse.x == "N/A") |
             (Kakumiro_joined_6$NO_spouse.x == "NA") )] 



Kakumiro_joined_6$PAPhve_spouse.x[
  which( (is.na(Kakumiro_joined_6$PAPhve_spouse.x)) |
           (Kakumiro_joined_6$PAPhve_spouse.x == "N/A") |
           (Kakumiro_joined_6$PAPhve_spouse.x == "NA") )] =
  
  Kakumiro_joined_6$PAPhve_spouse.y[
    which( (is.na(Kakumiro_joined_6$PAPhve_spouse.x)) |
             (Kakumiro_joined_6$PAPhve_spouse.x == "N/A") |
             (Kakumiro_joined_6$PAPhve_spouse.x == "NA") )] 



Kakumiro_joined_6$`PAP SPOUSE NAME:  (SPOUSE 2)`[ which(
  (Kakumiro_joined_6$PAP_NAME == "TAMALE PATRIC") & 
    (Kakumiro_joined_6$PAP_VALUATION_ASSESSMENT_REF ==
       "KAK/NKO/MWE/L001-03-00/1206") &
    (Kakumiro_joined_6$SUBCOUNTY.x == "NKOOKO") &
    (Kakumiro_joined_6$VILLAGE == "MWERUKA") ) ] = 
  "TUMUHIMBISE EDIVENA"



# Check spouse IDs ....


nchar(Kakumiro_joined_6$`spouse-IDno.x`)

cross_check_ids_spouses = compare_interests_6[
  which(  (nchar(compare_interests_6$`spouse-IDno.x`) != 14) &
          ((startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                      prefix = "CF") |
             startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                        prefix = "CM") ))), ]

length(which(nchar(compare_interests_6$`spouse-IDno.x`) == 14))

length(which(nchar(compare_interests_6$`spouse-IDno.x`) != 14))


write.csv(cross_check_ids_spouses, 
          file="cross_check_ids_spouses.csv", row.names = FALSE)



### --- Correct IDs

Kakumiro_joined_6$`spouse-IDno.x`[which(
  Kakumiro_joined_6$`spouse-IDno.x` ==
    "CF6061044TXF")] = "CF760161044TXF"



compare_interests_6 = subset(Kakumiro_joined_6,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "EACOP PAP REF NO.",
                                        "PAP REF NO",
                                        "PAP_RefNo",
                                        "PAP_NAME",
                                        "PAP_name.x",
                                        "PAP_name.y",
                                        "DISTRICT.x",
                                        "SUBCOUNTY.x",
                                        "VILLAGE",
                                        "PAP SPOUSE NAME:  (SPOUSE 1)",
                                        "PAP SPOUSE NAME:  (SPOUSE 2)",
                                        "spouse_name_No_Dash",
                                        "spouse_name_2", 
                                        "spouse-IDno.x",
                                        "spouse-IDno.y",
                                        "PAPhve_spouse.x", 
                                        "PAPhve_spouse.y",
                                        "NO_spouse.x",
                                        "NO_spouse.y",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REF")  )



compare_interests_7 = subset(Kakumiro_joined_6,
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


write.csv(Kakumiro_joined_6, 
          file="Kakumiro_joined_6.csv", row.names = FALSE)

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
Kakumiro_joined_6 = read_csv("Kakumiro_joined_6.csv")



# DISCLOSURE ...

Kakumiro_Disclosure = 
  read_excel("9.2 Kakumiro-Disclosure-Automated Database 19.11.2021.xlsx", 
             sheet = "1. KAKUMIRO_DISCLOSURE")


Unaccounted_disclosure_1 = anti_join(x = Kakumiro_joined_6, 
                              y = Kakumiro_Disclosure, 
                              by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                       "PAP VALUATION ASSESSMENT REF"))

Unaccounted_disclosure_2 = anti_join(x = Kakumiro_Disclosure,
                                     y = Kakumiro_joined_6,
                                     by = c("PAP VALUATION ASSESSMENT REF" = 
                                              "PAP_VALUATION_ASSESSMENT_REF"))


Reconciled_Disclosures_1 = inner_join(x = Kakumiro_joined_6, 
                                      y = Unaccounted_disclosure_2, 
                                      by = c("EACOP PAP REF NO." = 
                                            "PAP REF NO"))

Reconciled_Disclosures_1 = Reconciled_Disclosures_1[
  -(which(is.na(Reconciled_Disclosures_1$`EACOP PAP REF NO.`))), ]

Reconciled_Disclosures_1 = Reconciled_Disclosures_1[
  !duplicated(Reconciled_Disclosures_1$PAP_VALUATION_ASSESSMENT_REF), ]


Unaccounted_disclosure_3 = anti_join(x = Unaccounted_disclosure_2,
                                     y = Reconciled_Disclosures_1,
                                     by = c("PAP REF NO" = 
                                              "EACOP PAP REF NO."))

check_disclosures = subset(Reconciled_Disclosures_1,
                           select = c("EACOP PAP REF NO.",
                                      "PAP_NAME",
                                      "PAP NAME.y",
                                      "PAP_VALUATION_ASSESSMENT_REF",
                                      "PAP VALUATION ASSESSMENT REF.y",
                                      "PRE-DISCLOSURE STATUS",
                                      "PERSON WHO DISCLOSED",
                                      "DATE OF PRE-DISCLOSURE"))



## Update ... Valuation Ref NUmbers

Kakumiro_Disclosure %>% 
  mutate("PAP VALUATION ASSESSMENT REF" = 
           recode(Kakumiro_Disclosure$`PAP VALUATION ASSESSMENT REF`, 
                  "MCPY1/LO-01/F" = "MCPY1/LO-01/M",
                  "MCPY1/LO-01/F/LC-01" = "MCPY1/LO-01/M/LC-01",
                  "MCPY1/LO-01/F/LC-02" = "MCPY1/LO-01/M/LC-02",
                  "MCPY1/LO-01/F/LC-03" = "MCPY1/LO-01/M/LC-03",
                  "MCPY1/LO-01/F/LC-04" = "MCPY1/LO-01/M/LC-04", 
                  "MCPY1/LO-01/F/LC-05" = "MCPY1/LO-01/M/LC-05", 
                  "MCPY1/LO-01/F/LC-06" = "MCPY1/LO-01/M/LC-06", 
                  "MCPY1/LO-01/F/LC-07" = "MCPY1/LO-01/M/LC-07",
                  "MCPY1/LO-01/F/LC-08" = "MCPY1/LO-01/M/LC-08", 
                  "MCPY1/LO-01/F/LC-09" = "MCPY1/LO-01/M/LC-09",
                  "MCPY1/LO-01/F/LC-10" = "MCPY1/LO-01/M/LC-10",
                  "MCPY1/LO-01/F/LC-11" = "MCPY1/LO-01/M/LC-11",
                  "MCPY1/LO-01/F/LC-12" = "MCPY1/LO-01/M/LC-12",
                  "MCPY1/LO-01/F/LC-13" = "MCPY1/LO-01/M/LC-13",
                  "MCPY1/LO-01/F/LC-14" = "MCPY1/LO-01/M/LC-14",
                  "MCPY1/LO-01/F/LC-15" = "MCPY1/LO-01/M/LC-15",
                  "MCPY1/LO-01/F/LC-16" = "MCPY1/LO-01/M/LC-16",
                  "MCPY1/LO-01/F/LC-17" = "MCPY1/LO-01/M/LC-17",
                  "MCPY1/LO-01/F/LC-18" = "MCPY1/LO-01/M/LC-18",
                  "MCPY1/LO-01/F/LC-19" = "MCPY1/LO-01/M/LC-19",
                  "MCPY1/LO-01/F/LC-20" = "MCPY1/LO-01/M/LC-20",
                  "MCPY1/LO-01/F/LC-22" = "MCPY1/LO-01/M/LC-22")) -> 
  Kakumiro_Disclosure


### Repeat cycle ...


## Joining Unaccounted

Unaccounted_disclosure_join = full_join(x = Unaccounted_disclosure_1,
                             y = Unaccounted_disclosure_2,
                             by = c("EACOP PAP REF NO." = 
                                      "PAP REF NO"))

check_disclosures_2 = subset(Unaccounted_disclosure_join,
                           select = c("EACOP PAP REF NO.",
                                      "PAP_NAME",
                                      "PAP NAME.y",
                                      "PAP_VALUATION_ASSESSMENT_REF",
                                      "PAP VALUATION ASSESSMENT REF.y",
                                      "PRE-DISCLOSURE STATUS",
                                      "PERSON WHO DISCLOSED",
                                      "DATE OF PRE-DISCLOSURE"))


Kakumiro_Disclosure_edited = Kakumiro_Disclosure

write.csv(Kakumiro_Disclosure_edited, 
          file="Kakumiro_Disclosure_edited.csv", row.names = FALSE)


Kakumiro_Disclosure_edited = read_csv("Kakumiro_Disclosure_edited_2.csv")


Unaccounted_disclosure_1.2 = anti_join(x = Kakumiro_joined_6, 
                                     y = Kakumiro_Disclosure_edited, 
                                     by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                              "PAP VALUATION ASSESSMENT REF"))

Unaccounted_disclosure_2.2 = anti_join(x = Kakumiro_Disclosure_edited,
                                     y = Kakumiro_joined_6,
                                     by = c("PAP VALUATION ASSESSMENT REF" = 
                                              "PAP_VALUATION_ASSESSMENT_REF"))


Kakumiro_joined_7 = left_join(x = Kakumiro_joined_6, 
                              y = Kakumiro_Disclosure_edited, 
                              by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                      "PAP VALUATION ASSESSMENT REF"))


## Jump the lines below ...
Duplicate_Interests = Kakumiro_joined_7 %>%
  filter(duplicated(cbind(PAP_NAME, 
                          PAP_VALUATION_ASSESSMENT_REF)))

Bagala_Susan = Duplicate_Interests[2,]

Kakumiro_joined_7 = Kakumiro_joined_7 %>%
  distinct(PAP_NAME, PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

Kakumiro_joined_7 = rbind(Kakumiro_joined_7, Bagala_Susan)



#### Continue ...

compare_interests_7 = subset(Kakumiro_joined_7,
                             select = c("PAP REF NO.x",
                                        "PAP_NAME",                                                           
                                        "PAP_VALUATION_ASSESSMENT_REF",                                       
                                        "DISTRICT.x",                                                         
                                        "SUBCOUNTY",                                                          
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
    "PRE_DISCLOSURE_STATUS",
    "PERSON_WHO_DISCLOSED",
    "DATE_OF_PRE_DISCLOSURE")


write.csv(compare_interests_7, 
          file="compare_interests_7.csv", row.names = FALSE)

write.csv(Kakumiro_joined_7, 
          file="Kakumiro_joined_7.csv", row.names = FALSE)



# ***********************************************************
## GRIEVANCES
# ***********************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_7 = read_csv("compare_interests_7.csv")
Kakumiro_joined_7 = read_csv("Kakumiro_joined_7.csv")


Grievances_Kakumiro = 
  read_excel("Grievances_Kakumiro.xlsx", sheet = "Kakumiro (2)",
             col_types = c("text", "text", "date", 
                           "text", "text", "text", "text", "text", 
                           "text", "text", "text", "text", "text", 
                           "text", "text", "text", "text", "text", 
                           "text", "text", "text", "date", "text", 
                           "text", "text"))


Grievances_Kakumiro[,c(4:8)] = 
  apply(Grievances_Kakumiro[,c(4:8)], 2, toupper)


compare_interests_8_left = left_join(x = Grievances_Kakumiro,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Kakumiro a
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

Grievances_Kakumiro$`Name of grievant`[
  which(Grievances_Kakumiro$`Name of grievant` == "TAMALE PATRICK")
] = "TAMALE PATRIC"


Grievances_Kakumiro$`Name of grievant`[
  which(Grievances_Kakumiro$`Name of grievant` == "KATUSHABE AUGUSTINE")
] = "KATUSHABE AUGUSTIN"



### Correct Village names

Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "KEMIGISHA NIGHT")
] = "RWEMIRAMA"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "TUGUMISIRIZE JUSTUS")
] = "KISIITA A"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "KWESIGA GEOFREY")
] = "KISIITA A"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "BULEMU WILSON")
] = "KISIITA A"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "TWEHEYO JUSTUS")
] = "RWAMATA B"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "SUNDAY LIVINGSTONE")
] = "KYAJAWE  A"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "KATARYEBA PAKARASIO")
] = "RWEMIRAMA"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "BEYAKA BENON")
] = "KYAKAJORO A"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "KALULE JOHN")
] = "MPASAANA"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "BANYENZAKI CHARLES")
] = "MABENGERE"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "BEYAKA BENON")
] = "KYAKAJORO A"


Grievances_Kakumiro$Village[
  which(Grievances_Kakumiro$`Name of grievant` == "TUMUSIIME GEORGE")
] = "KIJUNGU"




## Repeat ....

compare_interests_8_left = left_join(x = Grievances_Kakumiro,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Kakumiro a
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

Kakumiro_joined_8 = left_join(x = Kakumiro_joined_7,
                                y = Grievances_Kakumiro,
                                by = c("PAP_NAME" = "Name of grievant",
                                       "VILLAGE.x" = "Village"),
                                keep = TRUE)


colnames(Kakumiro_joined_7)[
  which(colnames(Kakumiro_joined_7) == "ID NUMBER")] = "ID_NUMBER_2"

Kakumiro_joined_8_sql = sqldf("select *
                           from Kakumiro_joined_7 a
                           left join Grievances_Kakumiro b
                           on a.PAP_NAME like '%'||b.'Name of grievant'||'%'
                           and a.'VILLAGE.x' like '%'||b.Village||'%'
                           WHERE 1;", 
                                     method = "raw")

length(unique(Kakumiro_joined_8$PAP_VALUATION_ASSESSMENT_REF))


## The lines below require independent judgement
Duplicate_Interests = Kakumiro_joined_8 %>%
  filter(duplicated(cbind(PAP_NAME, 
                          PAP_VALUATION_ASSESSMENT_REF)))


Kakumiro_joined_8 = Kakumiro_joined_8 %>%
  distinct(PAP_NAME, PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


compare_interests_8 = subset(Kakumiro_joined_8,
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

write.csv(Kakumiro_joined_8, 
          file="Kakumiro_joined_8.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_8 = read_csv("compare_interests_8.csv")
Kakumiro_joined_8 = read_csv("Kakumiro_joined_8.csv")



## **********************************************
## SNAGS
## **********************************************

SNAGS_All = read_excel("Comprehensive Snag List 04-16.03.2022.xlsx", 
                       sheet = "Combined Snags List 2",
                       col_types = c("numeric", "text", "text", 
                                     "text", "text", "text", "date", "text", 
                                     "text", "text"))

unique(SNAGS_All$DISTRICT)

SNAGS_Kakumiro = subset(SNAGS_All, SNAGS_All$DISTRICT == "KAKUMIRO")


## checking for unaccounted ...

SNAGS_Unaccounted_1a = anti_join(x = compare_interests_8,
                                 y = SNAGS_Kakumiro,
                                 by = c("PAP_NAME" = "NAME OF PAP"))


SNAGS_Unaccounted_1b = sqldf("select * 
             from compare_interests_8 a
             where  not exists (
             select null from SNAGS_Kakumiro b
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_1c = sqldf("select * 
             from compare_interests_8 a
             where  not exists (
             select null from SNAGS_Kakumiro b
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")

SNAGS_Unaccounted_1b$PAP_NAME[
which(!(SNAGS_Unaccounted_1b$PAP_NAME %in% SNAGS_Unaccounted_1c$PAP_NAME))]


SNAGS_Unaccounted_2a = anti_join(x = SNAGS_Kakumiro,
                                 y = compare_interests_8,
                                 by = c("NAME OF PAP" =
                                          "PAP_NAME" ) )


SNAGS_Unaccounted_2b = sqldf("select * 
             from SNAGS_Kakumiro b
             where  not exists (
             select null from compare_interests_8 a
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_2c = sqldf("select * 
             from SNAGS_Kakumiro b
             where  not exists (
             select null from compare_interests_8 a
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")


SNAGS_Unaccounted_2b$`NAME OF PAP`[
  which(!(SNAGS_Unaccounted_2b$`NAME OF PAP` %in% 
            SNAGS_Unaccounted_2c$`NAME OF PAP` ))]


### Replace names ...

SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`PAP REF.NO.` ==
                                  "KAK/KAR/079")] =
  
  compare_interests_8$PAP_NAME[
    which( (compare_interests_8$EACOP_PAP_REF == "KAK/KAR/079") &
             (compare_interests_8$VALUATION_ASSESSMENT == 
                "KAK/KAT/KAR/L010-00-00/102") ) ]


SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`ASSESSMENT REF.NO` ==
                                     "KAK/KIS/KIS-A/L004-00-00/3927")] =
  
  compare_interests_8$PAP_NAME[
    which(compare_interests_8$VALUATION_ASSESSMENT == 
              "KAK/KIS/KIS-A/L004-00-00/3927") ]


SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`ASSESSMENT REF.NO` ==
                                     "KAK/KIS/KIS-A/L027-00-00/6498")] =
  
  compare_interests_8$PAP_NAME[
    which(compare_interests_8$VALUATION_ASSESSMENT == 
              "KAK/KIS/KIS-A/L027-00-00/6498") ]


SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`ASSESSMENT REF.NO` ==
                                     "KAK/KAT/KAS/L002-00-00/1018")] =
  
  compare_interests_8$PAP_NAME[
    which(compare_interests_8$VALUATION_ASSESSMENT == 
            "KAK/KAT/KAS/L002-00-00/1018") ]


SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`ASSESSMENT REF.NO` ==
                                     "KAK/KAT/KAS/L001-00-00/1016")] =
  
  compare_interests_8$PAP_NAME[
    which(compare_interests_8$VALUATION_ASSESSMENT == 
            "KAK/KAT/KAS/L001-00-00/1016") ]


SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`ASSESSMENT REF.NO` ==
                                     "KAK/MPA/MAB/L004-00-00/5939")] =
  
  compare_interests_8$PAP_NAME[
    which(compare_interests_8$VALUATION_ASSESSMENT == 
            "KAK/MPA/MAB/L004-00-00/5939") ]


SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`ASSESSMENT REF.NO` ==
                                     "KAK/KAT/KAR/L009-04-00/328")] =
  
  compare_interests_8$PAP_NAME[
    which(compare_interests_8$VALUATION_ASSESSMENT == 
            "KAK/KAT/KAR/L009-04-00/328") ]


SNAGS_Kakumiro$`NAME OF PAP`[which(SNAGS_Kakumiro$`ASSESSMENT REF.NO` ==
                                     "KAK/KAT/KYE/L003-00-00/5944")] =
  
  compare_interests_8$PAP_NAME[
    which(compare_interests_8$VALUATION_ASSESSMENT == 
            "KAK/KAT/KYE/L003-00-00/5944") ]


### Finally joining ...

SNAGS_Kakumiro_joined = left_join(x = compare_interests_8,
                               y = SNAGS_Kakumiro,
                               by = c("PAP_NAME" = "NAME OF PAP"),
                               keep = TRUE)


## With Duplicates Removed From SNAGS
SNAGS_Kakumiro_joined = SNAGS_Kakumiro_joined %>%
  distinct(VALUATION_ASSESSMENT, `SNAG ISSUE`, REMARKS,
           .keep_all = TRUE)


SNAGS_Kakumiro_joined$VALUATION_ASSESSMENT[which(
  duplicated(SNAGS_Kakumiro_joined$VALUATION_ASSESSMENT)
)]



library(sqldf)
check1 = sqldf("select PAP_NAME, VALUATION_ASSESSMENT, 
          group_concat(REMARKS, '; ') REMARKS, 
          group_concat(`SNAG ISSUE`, '; ') `SNAG ISSUE`
          from SNAGS_Kakumiro_joined group by VALUATION_ASSESSMENT", 
               method = "raw")


check2 =
  SNAGS_Kakumiro_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarise(REMARKS = toString(REMARKS)) %>%
  ungroup()



library(stringr)
check3 =
  SNAGS_Kakumiro_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarize(REMARKS = str_c(REMARKS, collapse = "; "))


colnames(SNAGS_Kakumiro_joined)[
  which(colnames(SNAGS_Kakumiro_joined) == 
          "Date when snag was handled (If fieldwork was needed)")] = 
  "DATE_SNAG_Handled"


colnames(SNAGS_Kakumiro_joined)[
  which(colnames(SNAGS_Kakumiro_joined) == 
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
    from SNAGS_Kakumiro_joined group by VALUATION_ASSESSMENT", 
                             method = "raw")


colnames(compare_interests_10)[
  which(colnames(compare_interests_10) == 
          "DISTRICT.x")] = "DISTRICT"



write.csv(compare_interests_10, 
          file="compare_interests_10.csv", row.names = FALSE)



### Repeat Kakumiro because of Bagala Susan


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_10 = read_csv("compare_interests_10.csv")


## **********************************************
## VULNERABLE PAPs
## **********************************************

Vulnerable_Kakumiro = read_excel("Vulnerable PAPs ALL Edited.xlsx", 
                              sheet = "KAKUMIRO")


## checking for unaccounted ...

Vulnerable_Unaccounted_1a = anti_join(x = compare_interests_10,
                                      y = Vulnerable_Kakumiro,
                                      by = c("PAP_NAME" = "Name of PAP"))


Vulnerable_Unaccounted_1b = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Kakumiro b
             where a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_1c = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Kakumiro b
             where a.PAP_NAME like '%'||b.'Name of PAP'||'%' );", 
                                  method = "raw")

Vulnerable_Unaccounted_1b$PAP_NAME[
  which(!(Vulnerable_Unaccounted_1b$PAP_NAME %in% Vulnerable_Unaccounted_1c$PAP_NAME))]


Vulnerable_Unaccounted_2a = anti_join(x = Vulnerable_Kakumiro,
                                      y = compare_interests_10,
                                      by = c("Name of PAP" = "PAP_NAME") )


Vulnerable_Unaccounted_2b = sqldf("select * 
             from Vulnerable_Kakumiro b
             where  not exists (
             select null from compare_interests_10 a
             where  a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_2c = sqldf("select * 
             from Vulnerable_Kakumiro b
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

Vulnerable_Kakumiro$Name_of_PAP_2 = Vulnerable_Kakumiro$`Name of PAP`


## rename

Vulnerable_Kakumiro$`Name of PAP`[which(Vulnerable_Kakumiro$`Name of PAP` ==
                                        "BUSIGYE JOY")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "BUSINGYE JOY") ]


Vulnerable_Kakumiro$`Name of PAP`[which(Vulnerable_Kakumiro$`Name of PAP` ==
                                          "KYALIKUNDA PRISCA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "KYALIKUNDA PRISKA") ]


Vulnerable_Kakumiro$`Name of PAP`[which(Vulnerable_Kakumiro$`Name of PAP` ==
                                          "MPISO YOSIFA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "MPISO YOFISA") ]


## special case ...

write.csv(Vulnerable_Unaccounted_2c, 
          file="Vulnerable_Unaccounted_2c.csv", row.names = FALSE)



### Finally joining ...

Kakumiro_joined_11 = left_join(x = compare_interests_10,
                            y = Vulnerable_Kakumiro,
                            by = c("PAP_NAME" = 
                                     "Name of PAP"),
                            keep = TRUE)


Kakumiro_joined_11 = sqldf("select *
                         from compare_interests_10 a
                         left join Vulnerable_Kakumiro b
                         on a.PAP_NAME like '%'||b.'Name of PAP'||'%';", 
                        method = "raw")


## With Duplicates Removed From Vulnerable
Kakumiro_joined_11 = Kakumiro_joined_11 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kakumiro_joined_11$VALUATION_ASSESSMENT[which(
  duplicated(Kakumiro_joined_11$VALUATION_ASSESSMENT)
)]


check1 = subset(Kakumiro_joined_11, 
                select = c("PAP_NAME", 
                           "Name_of_PAP_2",
                           "EACOP_PAP_REF",
                           "EACOP Ref.NUMBER"))

colnames(Kakumiro_joined_11)[
  which(startsWith(colnames(Kakumiro_joined_11), 
                   prefix = "Description of") )] = "Vulnerability"

colnames(Kakumiro_joined_11)[ 
  which(colnames(Kakumiro_joined_11) == "Comment")] = 
  "Comment_Vulnerability"


compare_interests_11 = 
  subset(Kakumiro_joined_11, 
         select = colnames(Kakumiro_joined_11)[c(1:29,36,37)] )

write.csv(compare_interests_11, 
          file="compare_interests_11.csv", row.names = FALSE)

write.csv(Kakumiro_joined_11, 
          file="Kakumiro_joined_11.csv", row.names = FALSE)




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
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "KAK/KIS/KIS-B/L002-00-00/5945")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KIS/KIS-B/L002-00-00/5945")]




### Update 2 ***************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-11")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-11")]




### Update 3 ***************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-15")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-15")]





### Update 4 ***************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]





### Update 5 ***************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY1/LO-02/M/LC-01")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-02/M/LC-01")]



### Update 6 **************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$`PAP REF NO` ==  "KAK/KYE/205")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


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

R_H_Kakumiro = subset(Replacement_Housing,
                       subset = (Replacement_Housing$DISTRICT ==
                                   "KAKUMIRO"))

colnames(R_H_Kakumiro)
colnames(compare_interests_11)



## checking for unaccounted ...

R_H_Unaccounted_1a = anti_join(x = compare_interests_11,
                               y = R_H_Kakumiro,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "PAP VALUATION ASSESSMENT REF"))


R_H_Unaccounted_1b = sqldf("select * 
             from compare_interests_11 a
             where  not exists (
             select null from R_H_Kakumiro b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%' );", 
                           method = "raw")


R_H_Unaccounted_1a$PAP_NAME[
  which(!(R_H_Unaccounted_1a$PAP_NAME %in% 
            R_H_Unaccounted_1b$PAP_NAME))]


R_H_Unaccounted_2a = anti_join(x = R_H_Kakumiro,
                               y = compare_interests_11,
                               by = c("PAP VALUATION ASSESSMENT REF" = 
                                        "VALUATION_ASSESSMENT") )


R_H_Unaccounted_2b = sqldf("select * 
             from R_H_Kakumiro b
             where  not exists (
             select null from compare_interests_11 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%');", 
                           method = "raw")


R_H_Unaccounted_2b$`PAP REF NO`[
  which(!(R_H_Unaccounted_2b$`PAP REF NO` %in% 
            R_H_Unaccounted_2a$`PAP REF NO` ))]



### Finally joining ...

Kakumiro_joined_12 = left_join(x = compare_interests_11,
                                y = R_H_Kakumiro,
                                by = c("VALUATION_ASSESSMENT" = 
                                         "PAP VALUATION ASSESSMENT REF"),
                                keep = TRUE)


## With Duplicates Removed
Kakumiro_joined_12 = Kakumiro_joined_12 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kakumiro_joined_12$VALUATION_ASSESSMENT[which(
  duplicated(Kakumiro_joined_12$VALUATION_ASSESSMENT)
)]


check1 = subset(Kakumiro_joined_12, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "EACOP_PAP_REF",
                           "PAP REF NO"))


colnames(Kakumiro_joined_12)[
  which(colnames(Kakumiro_joined_12) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Kakumiro_joined_12)[
  which(colnames(Kakumiro_joined_12) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_12 = 
  subset(Kakumiro_joined_12, 
         select = colnames(Kakumiro_joined_12)[c(1:31,37:38)] )

write.csv(compare_interests_12, 
          file="compare_interests_12.csv", row.names = FALSE)

write.csv(Kakumiro_joined_12, 
          file="Kakumiro_joined_12.csv", row.names = FALSE)




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

Unmarked_Graves_Kakumiro = subset(Unmarked_Graves,
                                subset = (Unmarked_Graves$DISTRICT ==
                                            "KAKUMIRO"))

Marked_Graves_Kakumiro = subset(Marked_Graves,
                              subset = (Marked_Graves$DISTRICT ==
                                          "KAKUMIRO"))


colnames(Unmarked_Graves_Kakumiro)
colnames(compare_interests_12)


## checking for unaccounted ...

U_G_Unaccounted_1a = anti_join(x = compare_interests_12,
                               y = Unmarked_Graves_Kakumiro,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


U_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_12 a
             where  not exists (
             select null from Unmarked_Graves_Kakumiro b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


U_G_Unaccounted_1a$PAP_NAME[
  which(!(U_G_Unaccounted_1a$PAP_NAME %in% 
            U_G_Unaccounted_1b$PAP_NAME))]


U_G_Unaccounted_2a = anti_join(x = Unmarked_Graves_Kakumiro,
                               y = compare_interests_12,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


U_G_Unaccounted_2b = sqldf("select * 
             from Unmarked_Graves_Kakumiro b
             where  not exists (
             select null from compare_interests_12 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


U_G_Unaccounted_2b$`PAP NAME`[
  which(!(U_G_Unaccounted_2b$`PAP NAME` %in% 
            U_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Kakumiro_joined_13a = left_join(x = compare_interests_12,
                              y = Unmarked_Graves_Kakumiro,
                              by = c("VALUATION_ASSESSMENT" = 
                                       "ASSESSMENT REFERENCE NUMBER"),
                              keep = TRUE)


## With Duplicates Removed
Kakumiro_joined_13a = Kakumiro_joined_13a %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kakumiro_joined_13a$VALUATION_ASSESSMENT[which(
  duplicated(Kakumiro_joined_13a$VALUATION_ASSESSMENT)
)]


check1 = subset(Kakumiro_joined_13a, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Kakumiro_joined_13a)[
  which(colnames(Kakumiro_joined_13a) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Kakumiro_joined_13a)[
  which(colnames(Kakumiro_joined_13a) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13a = 
  subset(Kakumiro_joined_13a, 
         select = colnames(Kakumiro_joined_13a)[c(1:33,37)] )



## checking for unaccounted ...

colnames(Marked_Graves_Kakumiro)

M_G_Unaccounted_1a = anti_join(x = compare_interests_13a,
                               y = Marked_Graves_Kakumiro,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


M_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_13a a
             where  not exists (
             select null from Marked_Graves_Kakumiro b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


M_G_Unaccounted_1a$PAP_NAME[
  which(!(M_G_Unaccounted_1a$PAP_NAME %in% 
            M_G_Unaccounted_1b$PAP_NAME))]


M_G_Unaccounted_2a = anti_join(x = Marked_Graves_Kakumiro,
                               y = compare_interests_13a,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


M_G_Unaccounted_2b = sqldf("select * 
             from Marked_Graves_Kakumiro b
             where  not exists (
             select null from compare_interests_13a a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


M_G_Unaccounted_2b$`PAP NAME`[
  which(!(M_G_Unaccounted_2b$`PAP NAME` %in% 
            M_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Kakumiro_joined_13b = left_join(x = compare_interests_13a,
                              y = Marked_Graves_Kakumiro,
                              by = c("VALUATION_ASSESSMENT" = 
                                       "ASSESSMENT REFERENCE NUMBER"),
                              keep = TRUE)


## With Duplicates Removed
Kakumiro_joined_13b = Kakumiro_joined_13b %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Kakumiro_joined_13b$VALUATION_ASSESSMENT[which(
  duplicated(Kakumiro_joined_13b$VALUATION_ASSESSMENT)
)]


check1 = subset(Kakumiro_joined_13b, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Kakumiro_joined_13b)[
  which(colnames(Kakumiro_joined_13b) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Kakumiro_joined_13b)[
  which(colnames(Kakumiro_joined_13b) == "SUBCOUNTY.x")] =
  "SUBCOUNTY"

colnames(Kakumiro_joined_13b)[
  which(colnames(Kakumiro_joined_13b) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13b = 
  subset(Kakumiro_joined_13b, 
         select = colnames(Kakumiro_joined_13b)[c(1:34,38)] )


compare_interests_13 = compare_interests_13b

Kakumiro_joined_13 = Kakumiro_joined_13b


write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)

write.csv(Kakumiro_joined_13, 
          file="Kakumiro_joined_13.csv", row.names = FALSE)





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
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "KAK/KAT/KAS/L001-00-03/1016")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "KAK/KAT/KAS/L001-00-03/1016")]



### Update 2 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$`PAP REF NO` ==  "KAK/KYE/205")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/KYE/205")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$`PAP REF NO` == "KAK/KYE/205")]




### Update 3 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$`PAP REF NO` ==  "KAK/WAB/326")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$EACOP_PAP_REF == "KAK/WAB/326")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$`PAP REF NO` == "KAK/WAB/326")]




### Update 4 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "MCPY1/LO-01/M/LC-18")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY1/LO-01/M/LC-18")]


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



