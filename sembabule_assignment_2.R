

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Sembabule

library(readxl)
library(dplyr)
library(readr)
library(sqldf)


## data of Sembabule Only from NewPlan
Sembabule_Control = read_excel("sembabule_assignment_2.xlsx", 
                                sheet = "Sembabule_NewPlan")

## Number of Interests Control
length(unique(Sembabule_Control$PAP_VALUATION_ASSESSMENT_REF))



## data from Total and Bank
Sembabule_From_Bank = read_excel("sembabule_assignment_2.xlsx", 
                                     sheet = "Sembabule_From_Bank")

## Remove empty rows
Sembabule_From_Bank = 
  Sembabule_From_Bank[!is.na(Sembabule_From_Bank$`PAP NAME`), ]


## Number of Interests From Bank Without Duplicates
length(unique(Sembabule_From_Bank$`PAP VALUATION ASSESSMENT REF`))

#### --- This is an issue of concern. Why those few interests ?
#### --- This is perhaps because many interests were captured in the same cell



## names
colnames(Sembabule_Control)
colnames(Sembabule_From_Bank)



# Joining


## left join
Sembabule_joined = left_join(x = Sembabule_Control, 
                           y = Sembabule_From_Bank, 
                           by = c("PAP_NAME" = "PAP NAME",
                                  "VILLAGE" = "VILLAGE"))

Unaccounted_1 = anti_join(x = Sembabule_Control, 
                          y = Sembabule_From_Bank, 
                          by = c("PAP_NAME" = "PAP NAME",
                                   "VILLAGE" = "VILLAGE"))


Unaccounted_2 = anti_join(x = Sembabule_From_Bank, 
                          y = Sembabule_Control, 
                          by = c("PAP NAME" = "PAP_NAME",
                               "VILLAGE" = "VILLAGE"))


#### --- The following lines only apply if there is "Unaccounted_2"


unique(Unaccounted_2$VILLAGE)

unique(Sembabule_Control$VILLAGE)


which( (Sembabule_From_Bank$VILLAGE == "VILLAGE") &
       (Sembabule_From_Bank$`PAP NAME` == "KIZITO ALIFAILI") )

Sembabule_From_Bank$VILLAGE[which( 
  (Sembabule_From_Bank$VILLAGE == "VILLAGE") &
  (Sembabule_From_Bank$`PAP NAME` == "KIZITO ALIFAILI") )] = "KAGANGO"


Sembabule_From_Bank$VILLAGE[which( 
  Sembabule_From_Bank$`PAP NAME` == "YIGA ROBERT") ] = "NJAZA-HELIPAD"


Sembabule_From_Bank$VILLAGE[which( 
  (Sembabule_From_Bank$VILLAGE == "NJAZA") &
    (Sembabule_From_Bank$`PAP NAME` == "KUTEESA FRED") )] = "NJAZA-HELIPAD"

Sembabule_From_Bank$`PAP VALUATION ASSESSMENT REF`[which( 
  (Sembabule_From_Bank$VILLAGE == "NJAZA-HELIPAD") &
    (Sembabule_From_Bank$`PAP NAME` == "KUTEESA FRED") )] = 
  "SSE/MIT/NJA/L008B-00-01/5158"


which( ( is.na(Sembabule_From_Bank$VILLAGE) )  &
         (Sembabule_From_Bank$`PAP NAME` == "KUTEESA FRED") )

Sembabule_From_Bank =
  Sembabule_From_Bank[ - which( ( is.na(Sembabule_From_Bank$VILLAGE) )  &
          (Sembabule_From_Bank$`PAP NAME` == "KUTEESA FRED") ), ]


Sembabule_From_Bank$`PAP NAME`[which( 
  Sembabule_From_Bank$`PAP NAME` == "KABIIBI KELLEN ANDRWOMUSHANA STEPHEN R48")] = 
  "KABIIBI KELLEN(ID)/KELENI KABIBI(LT)"

Sembabule_From_Bank$`ID Number`[which( 
  Sembabule_From_Bank$`PAP NAME` == "KABUGO PAUL")] = "CM840451014RUF"


Sembabule_From_Bank$VILLAGE[which( 
  (Sembabule_From_Bank$`PAP NAME` == "KABUGO PAUL") &
    (Sembabule_From_Bank$VILLAGE == "NJAZA-HELIPAD") )] = "NJAZA-ACCESS ROAD"

Sembabule_From_Bank$VILLAGE[which( 
  (Sembabule_From_Bank$`PAP NAME` == "KABUGO PAUL") &
    (Sembabule_From_Bank$VILLAGE == "NJAZA") )] = "NJAZA-PS-2"

Sembabule_From_Bank =
  Sembabule_From_Bank[ - which( ( is.na(Sembabule_From_Bank$VILLAGE) )  &
                  (Sembabule_From_Bank$`PAP NAME` == "KABUGO PAUL") ), ]


### -- details of VILLAGE not matching for SSERWADDA EXPEDIT
### -- (many interests)
### -- same for KIGGUNDU JOHN(ID)/KIGUNDU JOHN(LT)
### -- same for WALAKIRA PETER(DECEASED) C/O WALAKIRA GODFREY


### -- Repeat join

## left join
Sembabule_joined = left_join(x = Sembabule_Control, 
                             y = Sembabule_From_Bank, 
                             by = c("PAP_NAME" = "PAP NAME",
                                    "VILLAGE" = "VILLAGE"))

Unaccounted_1 = anti_join(x = Sembabule_Control, 
                          y = Sembabule_From_Bank, 
                          by = c("PAP_NAME" = "PAP NAME",
                                 "VILLAGE" = "VILLAGE"))


Unaccounted_2 = anti_join(x = Sembabule_From_Bank, 
                          y = Sembabule_Control, 
                          by = c("PAP NAME" = "PAP_NAME",
                                 "VILLAGE" = "VILLAGE"))


# Compare Interests
colnames(Sembabule_joined)

colnames(Sembabule_joined)[
  which( colnames(Sembabule_joined) == 
   "CENTENARY BANK ACCOUNT NAME(s):  \r\n(PAP and Spouse or Joint PAPs)" )
] =   "CENTENARY BANK ACCOUNT NAME(s):"



compare_interests_1 = subset(Sembabule_joined,
                             select = c("PAP_NAME",
                                        "PAP_VALUATION_ASSESSMENT_REF",
                                        "PAP VALUATION ASSESSMENT REF",
                                        "PAP REF NO",
                                        "EACOP PAP REF NO.",
                                        "DISTRICT.x",
                                        "SUBCOUNTY",
                                        "VILLAGE")) 


compare_interests_2 = subset(Sembabule_joined,
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
Sembabule_BAO = read_excel("Sembabule Analyzed.xlsx", 
                                     sheet = "BAO")

## Remove empty rows
Sembabule_BAO = 
  Sembabule_BAO[!is.na(Sembabule_BAO$PAP_name_No_Dash), ]



# Joining with Mathias
colnames(compare_interests_2)
colnames(Sembabule_BAO)


Sembabule_joined_3 = left_join(x = compare_interests_2, 
                              y = Sembabule_BAO, 
                              by = c("PAP_NAME" = "PAP_name_No_Dash",
                                     "VILLAGE" = "village"))

Unaccounted_BAO_mathias_2 = anti_join(x = Sembabule_BAO, 
                                      y = compare_interests_2, 
                                      by = c("PAP_name_No_Dash" = "PAP_NAME",
                                             "village" ="VILLAGE"))

which(compare_interests_2$`ID Number` %in% Unaccounted_BAO_mathias_2$ID_NO)


## -- Below the IDs from the bank field exercise are matched with the IDs from Mathias.
Sembabule_joined_3.2 = left_join(x = compare_interests_2, 
                               y = Sembabule_BAO, 
                               by = c("PAP_NAME" = "PAP_name_No_Dash",
                                      "ID Number" = "ID_NO"))

Unaccounted_BAO_mathias_2.2 = anti_join(x = Sembabule_BAO, 
                                      y = compare_interests_2, 
                                      by = c("PAP_name_No_Dash" = "PAP_NAME",
                                            "ID_NO" = "ID Number"))

### -- Here there is room to resolve many unaccounted


colnames(Sembabule_joined_3.2)


compare_interests_3 = subset(Sembabule_joined_3.2,
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


write.csv(compare_interests_3, 
          file="compare_interests_3.csv", row.names = FALSE)

compare_interests_3 = read_csv("compare_interests_3.csv")



# Joining with Centenary Core Banking System

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


Sembabule_joined_4_inner_join_with_sql = 
  read_csv("Sembabule_joined_4_inner_join_with_sql.csv")

Sembabule_joined_4_left_join_with_sql = 
  read_csv("Sembabule_joined_4_left_join_with_sql.csv")

Sembabule_joined_4_left_join_with_sql = 
  Sembabule_joined_4_left_join_with_sql %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Sembabule_joined_4_inner_join_with_sql = 
  Sembabule_joined_4_inner_join_with_sql %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

write.csv(Sembabule_joined_4_inner_join_with_sql, 
          file="Sembabule_joined_4_inner_join_with_sql.csv", row.names = FALSE)


length(unique(Sembabule_joined_4_inner_join_with_sql$PAP_NAME))

length(unique(setdiff(Sembabule_joined_4_inner_join_with_sql$PAP_NAME,
  Sembabule_joined_4_left_join_with_sql$PAP_NAME )))

length(unique(setdiff(Sembabule_joined_4_left_join_with_sql$PAP_NAME,
                      Sembabule_joined_4_inner_join_with_sql$PAP_NAME )))


Interests_Without_Accounts = sqldf("select * 
             from Sembabule_joined_4_left_join_with_sql a
             where  not exists (
             select null from Sembabule_joined_4_inner_join_with_sql b
             where  a.PAP_NAME = b.PAP_NAME
             and  a.PAP_VALUATION_ASSESSMENT_REF = 
            b.PAP_VALUATION_ASSESSMENT_REF );", 
            method = "raw")

Interests_Without_Accounts = Interests_Without_Accounts %>% 
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

Lost_and_Found = Interests_Without_Accounts[
  which(Interests_Without_Accounts$CENTENARY_BANK_ACCOUNT_NUMBER %in%
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

Lost_and_Found_new = read_excel("Sembabule_Feb_2_2022.xlsx", 
                              sheet = "Lost_and_Found_2")

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
  -which(Interests_Without_Accounts$CENTENARY_BANK_ACCOUNT_NUMBER %in%
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



Sembabule_joined_4_left_join_with_sql = 
  Sembabule_joined_4_left_join_with_sql %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


Sembabule_joined_4_left_join_with_sql$PAP_VALUATION_ASSESSMENT_REF %in%
  Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF

which(Sembabule_joined_4_left_join_with_sql$PAP_VALUATION_ASSESSMENT_REF %in%
  Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)


check =
  Sembabule_joined_4_left_join_with_sql[
    (which(Sembabule_joined_4_left_join_with_sql$PAP_VALUATION_ASSESSMENT_REF %in%
            Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]


Sembabule_joined_4_left_join_with_sql =
  Sembabule_joined_4_left_join_with_sql[
    -(which(Sembabule_joined_4_left_join_with_sql$PAP_VALUATION_ASSESSMENT_REF %in%
             Lost_and_Found_joined$PAP_VALUATION_ASSESSMENT_REF)), ]

  
colnames(Lost_and_Found_joined) =
  colnames(Sembabule_joined_4_left_join_with_sql)

Sembabule_joined_4_left_join_with_sql = 
  rbind(Sembabule_joined_4_left_join_with_sql,
        Lost_and_Found_joined)

write.csv(Sembabule_joined_4_left_join_with_sql, 
          file="Sembabule_joined_4_inner_join_with_sql_2.csv", row.names = FALSE)


### -- ignore this part below

compare_interests_3$PAP_name = c()

Sembabule_joined_4 = sqldf("select *
                      from compare_interests_3 a
                      inner join centenary_core_needed b
                      on a.PAP_NAME like CONCAT('%', b.DESC_2, '%')
                      WHERE 1;", 
                            method = "raw")


Sembabule_joined_4 = left_join(x = compare_interests_3, 
                            y = centenary_core_needed, 
                            by = c("PAP_NAME" = "DESC_2"))

Sembabule_joined_4.2 = inner_join(x = compare_interests_3, 
                               y = centenary_core_needed, 
                               by = c("PAP_NAME" = "DESC_2"))

Sembabule_joined_4.3 = inner_join(x = compare_interests_3, 
                                  y = centenary_core_needed, 
                                  by = c("PAP_NAME" = "DESC_1"))

Sembabule_joined_4.4 = inner_join(x = compare_interests_3, 
                                  y = centenary_core_needed, 
                                  by = c("PAP_NAME" = "ACCOUNT_DESCRIPTION"))

Unaccounted_accounts_1 = subset(compare_interests_3,
              !((compare_interests_3$`CENTENARY BANK ACCOUNT NUMBER:` %in%
                    Sembabule_joined_4.2$ACCOUNT_NUMBER  ) |
                                  
              (compare_interests_3$`CENTENARY BANK ACCOUNT NUMBER:` %in%
                  Sembabule_joined_4.3$ACCOUNT_NUMBER ) |
                   
                (compare_interests_3$`CENTENARY BANK ACCOUNT NUMBER:` %in%
                  Sembabule_joined_4.4$ACCOUNT_NUMBER ) ))



Sembabule_MFB_1 = anti_join(x = compare_interests_3, 
                            y = centenary_core_needed, 
                            by = c("PAP_NAME" = "ACCOUNT_DESCRIPTION"))

Sembabule_MFB_2 = anti_join(x = compare_interests_3, 
                            y = centenary_core_needed, 
                            by = c("PAP_NAME" = "DESC_1"))

Sembabule_MFB_3 = anti_join(x = compare_interests_3, 
                            y = centenary_core_needed, 
                            by = c("PAP_NAME" = "DESC_2"))

colnames(compare_interests_3)[
  which(colnames(compare_interests_3) == "PAP_name")] = "PAP_name2"


Sembabule_MFB_4 = anti_join(x = compare_interests_3, 
                            y = centenary_core_needed, 
                            by = c("ID Number" = "ID_NO"))

Sembabule_MFB_5 = anti_join(x = compare_interests_3, 
                            y = centenary_core_needed, 
                            by = c("PAP_NAME" = "DESC_2",
                                   "ID Number" = "ID_NO"))


## ignoring earlier data manipulations

Sembabule_joined_4_left_join_with_sql = 
  Sembabule_joined_4_left_join_with_sql %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)

Sembabule_joined_4 = Sembabule_joined_4_left_join_with_sql


colnames(Sembabule_joined_4)[
  which(colnames(Sembabule_joined_4) == 
          "CENTENARY_BANK_ACCOUNT_NAME")] = "CENTENARY BANK ACCOUNT NAME(s):"

colnames(Sembabule_joined_4)[
  which(colnames(Sembabule_joined_4) == 
          "CENTENARY_BANK_ACCOUNT_NUMBER")] = "CENTENARY BANK ACCOUNT NUMBER:"


missed_bank_account_names = subset(Sembabule_joined_4,
        subset = ( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
                (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
                (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") ))


missed_bank_account_numbers = subset(Sembabule_joined_4,
          subset = ( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`)) |
          (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "N/A") |
          (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "NA") ))



#### ---- checking missing bank account names

missed_bank_account_names = subset(Sembabule_joined_4,
    subset = ( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
    (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
    (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") ))


Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`[
  which( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
         (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] =
  
Sembabule_joined_4$ACCOUNT_DESCRIPTION[
  which( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):`)) |
           (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "N/A") |
           (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NAME(s):` == "NA") )] 


### ---- checking bank account numbers

cross_check_bank_accounts = Sembabule_joined_4[
  which(nchar(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`


#### check account numbers in centenary bank system
Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in bank system
Sembabule_joined_4$PAP_NAME[which(
  Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account numbers in Mathias survey data
Sembabule_joined_4$`centenary-bankaccount`[which(
  Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )]

#### check account names in centenary bank system name description
Sembabule_joined_4$PAP_NAME[which(
  Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` %in%
    cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:` )] %in%
  centenary_core_needed$ACCOUNT_DESCRIPTION


### --- Correct Bank Accounts

Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[which(
  Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` ==
    "320306633")] = "3203066331"


### --- Repeat check for bank account numbers

cross_check_bank_accounts = Sembabule_joined_4[
  which(nchar(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) != 10 &
          nchar(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) > 1 &
          nchar(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`) < 15 ), ]

cross_check_bank_accounts$`CENTENARY BANK ACCOUNT NUMBER:`


### --- Be careful with this transfer of accounts from Mathias' data

missed_bank_account_numbers = subset(Sembabule_joined_4,
          subset = ( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`)) |
                   (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "N/A") |
                   (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "NA") ))


Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`[
  which( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`)) |
           (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "N/A") |
           (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "NA") )] =
  
  Sembabule_joined_4$`centenary-bankaccount`[
    which( (is.na(Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:`)) |
             (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "N/A") |
             (Sembabule_joined_4$`CENTENARY BANK ACCOUNT NUMBER:` == "NA") )] 



#  some data from server
data_extra_server = read_excel("EACOP PAP NATIONAL ID.xlsx", 
                               sheet = "Sheet2")


colnames(Sembabule_joined_4)
colnames(data_extra_server)


# Join Again
Sembabule_joined_5 = left_join(x = Sembabule_joined_4, 
                            y = data_extra_server, 
                            by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                                     "ASSESSMENT REFERENCE NUMBER"))


colnames(Sembabule_joined_5)

colnames(Sembabule_joined_5)[
  which(colnames(Sembabule_joined_5) == 
          "PAP_name2")] = "PAP_name"

colnames(Sembabule_joined_5)[
  which(colnames(Sembabule_joined_5) == 
          "PAP_SPOUSE_NAME")] = "PAP'S SPOUSE NAME(s):"

compare_interests_4 = subset(Sembabule_joined_5,
                             select = c("PAP REF NO.x",
                                        "PAP REF NO.y",
                                        "EACOP PAP REF NO.", 
                                        "PAP_NAME",
                                        "PAP_name", 
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



### --- copy IDs found on server

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



## -- Actually in this case, these are extra IDs found in the core banking system

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



### --- copy IDs found with mathias

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



## -- crosscheck ids from bank field list

length(which(nchar(compare_interests_4$`ID Number`) != 14))

length(which(nchar(Sembabule_joined_5$`ID Number`) != 14))

cross_check_ids_from_bank = compare_interests_4[
  which( (nchar(compare_interests_4$`ID Number`) != 14) &
           ((startsWith(toupper(compare_interests_4$`ID Number`), 
                        prefix = "CF") |
               startsWith(toupper(compare_interests_4$`ID Number`), 
                          prefix = "CM") )) ), ]

cross_check_ids_from_bank$`ID Number`

nchar(cross_check_ids_from_bank$`ID Number`)

#### check IDs in centenary bank from field
compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` %in%
    cross_check_ids_from_bank$`ID Number` )]

#### check PAP names in bank from field
compare_interests_4$PAP_NAME[which(
  compare_interests_4$`ID Number` %in%
    cross_check_ids_from_bank$`ID Number` )]

#### check PAP names in centenary bank name description from field data
compare_interests_4$PAP_NAME[which(
  compare_interests_4$`ID Number` %in%
    cross_check_ids_from_bank$`ID Number` )] %in%
  compare_interests_4$ACCOUNT_DESCRIPTION



### -- correct IDs from bank field list

compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CF69045102K5")] = "CF69045102K5UL"

compare_interests_4$`ID Number`[which(
  compare_interests_4$`ID Number` ==
    "CM6103410CQ7K")] = "CM6103410C4Q7K"



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

unique_names[["3203130291"]]

unique_names[["3203073218"]]

unique_names[["3203067528"]]

unique_names[["3203075448"]]



Disputed_Accounts = subset(compare_interests_5,
                compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
                             "3203130291" |
                compare_interests_5$CENTENARY_BANK_ACCOUNT_NUMBER ==  
                             "3203075448" )

write.csv(Disputed_Accounts, 
          file="Disputed_Accounts.csv", row.names = FALSE)



# Unaccounted for accounts in Centenary Core Banking System

Unaccounted_bank_account_numbers = anti_join(x = compare_interests_5, 
                                             y = centenary_core_needed, 
                                             by = c("CENTENARY_BANK_ACCOUNT_NUMBER" = 
                                                      "ACCOUNT_NUMBER"))


wrong_account_numbers = subset(Unaccounted_bank_account_numbers,
      (startsWith(Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER, 
                                           prefix = "3") |
      startsWith(Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER, 
                                             prefix = "32") ))


### add them only if they are mutually exclusive
Disputed_Accounts$CENTENARY_BANK_ACCOUNT_NUMBER %in%
  Unaccounted_bank_account_numbers$CENTENARY_BANK_ACCOUNT_NUMBER

Unaccounted_bank_account_numbers = rbind(Disputed_Accounts,
                                         Unaccounted_bank_account_numbers)

write.csv(Unaccounted_bank_account_numbers, 
          file="Unaccounted_bank_account_numbers.csv", row.names = FALSE)


write.csv(compare_interests_4, 
          file="compare_interests_4.csv", row.names = FALSE)

write.csv(compare_interests_5, 
          file="compare_interests_5.csv", row.names = FALSE)



########### ---------------------------------------- ################


# SECTION TWO ... NOT FROM SCRATCH ...


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_4 = read_csv("compare_interests_4.csv")
compare_interests_5 = read_csv("compare_interests_5.csv")
Disputed_Accounts = read_csv("Disputed_Accounts.csv")
Unaccounted_bank_account_numbers = read_csv("Unaccounted_bank_account_numbers.csv")


# SUB-SECTION ....


# SPOUSES ....SUB-SECTION


## Spouses data from Mathias BAO
Spouses_BAO = read_excel("Sembabule Analyzed.xlsx", 
                         sheet = "BAO")


Spouses_BAO$village_No_Dash = gsub(pattern = "-", 
                                   replacement = " ",
                                   x = Spouses_BAO$village,
                                   ignore.case = TRUE)

Spouses_BAO$PAP_name_No_Dash = toupper(Spouses_BAO$PAP_name_No_Dash)
Spouses_BAO$subcounty = toupper(Spouses_BAO$subcounty)
Spouses_BAO$village_No_Dash = toupper(Spouses_BAO$village_No_Dash)


## Spouses data from Control
Spouses_Control = read_excel("Sembabule Analyzed.xlsx", 
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


Sembabule_joined_6 = Spouses_joined_3

colnames(Sembabule_joined_6)

compare_interests_6 = subset(Sembabule_joined_6,
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


write.csv(Sembabule_joined_6, 
          file="Sembabule_joined_6.csv", row.names = FALSE)

write.csv(compare_interests_6, 
          file="compare_interests_6.csv", row.names = FALSE)



## Fill in Spouse ID column ....


missed_PAP_names = subset(compare_interests_6,
              subset = ( (is.na(compare_interests_6$PAP_name.x)) |
                       (compare_interests_6$PAP_name.x == "N/A") |
                       (compare_interests_6$PAP_name.x == "N A") |
                       (compare_interests_6$PAP_name.x == "NA") ))


Sembabule_joined_6$PAP_name.x[
  which( (is.na(Sembabule_joined_6$PAP_name.x)) |
           (Sembabule_joined_6$PAP_name.x == "N/A") |
           (Sembabule_joined_6$PAP_name.x == "N A") |
           (Sembabule_joined_6$PAP_name.x == "NA") )] =
  
  Sembabule_joined_6$PAP_name.y[
    which( (is.na(Sembabule_joined_6$PAP_name.x)) |
             (Sembabule_joined_6$PAP_name.x == "N/A") |
             (Sembabule_joined_6$PAP_name.x == "N A") |
             (Sembabule_joined_6$PAP_name.x == "NA") )] 



missed_spouse_names = subset(compare_interests_6,
      subset = ( (is.na(compare_interests_6$spouse_name_No_Dash.x)) |
               (compare_interests_6$spouse_name_No_Dash.x == "N/A") |
               (compare_interests_6$spouse_name_No_Dash.x == "N A") |
               (compare_interests_6$spouse_name_No_Dash.x == "NA") ))


Sembabule_joined_6$spouse_name_No_Dash.x[
  which( (is.na(Sembabule_joined_6$spouse_name_No_Dash.x)) |
           (Sembabule_joined_6$spouse_name_No_Dash.x == "N/A") |
           (Sembabule_joined_6$spouse_name_No_Dash.x == "N A") |
           (Sembabule_joined_6$spouse_name_No_Dash.x == "NA") )] =
  
  Sembabule_joined_6$spouse_name_No_Dash.y[
    which( (is.na(Sembabule_joined_6$spouse_name_No_Dash.x)) |
             (Sembabule_joined_6$spouse_name_No_Dash.x == "N/A") |
             (Sembabule_joined_6$spouse_name_No_Dash.x == "N A") |
             (Sembabule_joined_6$spouse_name_No_Dash.x == "NA") )] 



missed_spouse_names_main = subset(compare_interests_6,
      subset = ( (is.na(compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
      (compare_interests_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") ))


Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`[
  which( (is.na(Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
           (Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
           (Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
           (Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )] =
  
  Sembabule_joined_6$spouse_name_No_Dash.x[
    which( (is.na(Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)`)) |
             (Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N/A") |
             (Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "N A") |
             (Sembabule_joined_6$`PAP SPOUSE NAME:  (SPOUSE 1)` == "NA") )]



missed_spouse_IDs = subset(compare_interests_6,
                subset = ( (is.na(compare_interests_6$`spouse-IDno.x`)) |
                (compare_interests_6$`spouse-IDno.x` == "N/A") |
                (compare_interests_6$`spouse-IDno.x` == "N A") |
                (compare_interests_6$`spouse-IDno.x` == "NA") ))


Sembabule_joined_6$`spouse-IDno.x`[
  which( (is.na(Sembabule_joined_6$`spouse-IDno.x`)) |
           (Sembabule_joined_6$`spouse-IDno.x` == "N/A") |
           (Sembabule_joined_6$`spouse-IDno.x` == "N A") |
           (Sembabule_joined_6$`spouse-IDno.x` == "NA") )] =
  
  Sembabule_joined_6$`spouse-IDno.y`[
    which( (is.na(Sembabule_joined_6$`spouse-IDno.x`)) |
             (Sembabule_joined_6$`spouse-IDno.x` == "N/A") |
             (Sembabule_joined_6$`spouse-IDno.x` == "N A") |
             (Sembabule_joined_6$`spouse-IDno.x` == "NA") )] 



Sembabule_joined_6$NO_spouse.x[
  which( (is.na(Sembabule_joined_6$NO_spouse.x)) |
           (Sembabule_joined_6$NO_spouse.x == "N/A") |
           (Sembabule_joined_6$NO_spouse.x == "N A") |
           (Sembabule_joined_6$NO_spouse.x == "NA") )] =
  
  Sembabule_joined_6$NO_spouse.y[
    which( (is.na(Sembabule_joined_6$NO_spouse.x)) |
             (Sembabule_joined_6$NO_spouse.x == "N/A") |
             (Sembabule_joined_6$NO_spouse.x == "N A") |
             (Sembabule_joined_6$NO_spouse.x == "NA") )] 



Sembabule_joined_6$PAPhve_spouse.x[
  which( (is.na(Sembabule_joined_6$PAPhve_spouse.x)) |
           (Sembabule_joined_6$PAPhve_spouse.x == "N/A") |
           (Sembabule_joined_6$PAPhve_spouse.x == "N A") |
           (Sembabule_joined_6$PAPhve_spouse.x == "NA") )] =
  
  Sembabule_joined_6$PAPhve_spouse.y[
    which( (is.na(Sembabule_joined_6$PAPhve_spouse.x)) |
             (Sembabule_joined_6$PAPhve_spouse.x == "N/A") |
             (Sembabule_joined_6$PAPhve_spouse.x == "N A") |
             (Sembabule_joined_6$PAPhve_spouse.x == "NA") )] 



### --- Room to correct some spouse names here




# Check spouse IDs ....


nchar(Sembabule_joined_6$`spouse-IDno.x`)

length(which(nchar(compare_interests_6$`spouse-IDno.x`) == 14))

length(which(nchar(compare_interests_6$`spouse-IDno.x`) != 14))

cross_check_ids_spouses = compare_interests_6[
  which(  (nchar(compare_interests_6$`spouse-IDno.x`) != 14) &
            ((startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                         prefix = "CF") |
                startsWith(toupper(compare_interests_6$`spouse-IDno.x`), 
                           prefix = "CM") ))), ]



### --- Correct IDs

### --- Later we shall correct some IDs

# Sembabule_joined_6$`spouse-IDno.x`[which(
#   Sembabule_joined_6$`spouse-IDno.x` ==
#     "CM710110279YA")] = "CM6710110279YA"



compare_interests_6 = subset(Sembabule_joined_6,
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



compare_interests_7 = subset(Sembabule_joined_6,
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
                                        "PAP'S SPOUSE NAME(s):",
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
    "SPOUSE_NAME",
    "SPOUSE_ID")



write.csv(cross_check_ids_spouses, 
          file="cross_check_ids_spouses.csv", row.names = FALSE)

write.csv(Sembabule_joined_6, 
          file="Sembabule_joined_6.csv", row.names = FALSE)

write.csv(compare_interests_6, 
          file="compare_interests_6.csv", row.names = FALSE)

write.csv(compare_interests_7, 
          file="compare_interests_7.csv", row.names = FALSE)



# Reloading ... NOT FROM SCRATCH ... edit the spouses 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_4 = read_csv("compare_interests_4.csv")
compare_interests_5 = read_csv("compare_interests_5.csv")
compare_interests_6 = read_csv("compare_interests_6.csv")
compare_interests_7 = read_csv("compare_interests_7.csv")
Sembabule_joined_6 = read_csv("Sembabule_joined_6.csv")


length(unique(compare_interests_7$PAP_NAME))

length(unique(compare_interests_7$PAP_VALUATION_ASSESSMENT_REF))




# Joining with Centenary Core Banking System

centenary_core2 = read_excel("EACOP Accounts 25-01-2022.xlsx", 
                               sheet = "Qualified accts ")

colnames(centenary_core2)


## -- checking if CUST_ID is unique --
length(unique(centenary_core2$CUST_ID))

## -- checking if ID_NO is unique --
length(unique(centenary_core2$`NATIONAL ID`))

## -- checking if ACCOUNT_NUMBER is unique --
length(unique(centenary_core2$ACCOUNT_NUMBER))



centenary_core = read.csv("ECOPS.csV")

colnames(centenary_core)

centenary_core$ID_NO = gsub(" ", "", centenary_core$ID_NO, fixed = TRUE)

centenary_core$ID_NO = gsub("[[:space:]]", "", centenary_core$ID_NO)


## -- checking if CUST_ID is unique -- found not unique
length(unique(centenary_core$CUST_ID))

## -- checking if ACCOUNT_NUMBER is unique -- found not unique
length(unique(centenary_core$ACCOUNT_NUMBER))

## -- checking if ID_NO is unique -- found not unique
length(unique(centenary_core$ID_NO))




# ***********************************************************
## GRIEVANCES
# ***********************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_7 = read_csv("compare_interests_7.csv")
Sembabule_joined_6 = read_csv("Sembabule_joined_6.csv")


Grievances_Sembabule = 
  read_excel("Grievances_Sembabule.xlsx", sheet = "Sembabule (2)",
             col_types = c("text", "text", "date", 
                           "text", "text", "text", "text", "text", 
                           "text", "text", "text", "text", "text", 
                           "text", "text", "text", "text", "text", 
                           "text", "text", "text", "date", "text", 
                           "text", "text"))


Grievances_Sembabule[,c(4:8)] = 
  apply(Grievances_Sembabule[,c(4:8)], 2, toupper)


compare_interests_8_left = left_join(x = Grievances_Sembabule,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Sembabule a
                              left join compare_interests_7 b
                              on a.'Name of grievant' like '%'||b.PAP_NAME||'%'
                              WHERE 1;", 
                                     method = "raw")


check = subset(compare_interests_8_left, select = c("Name of grievant",
                                                    "PAP_NAME",
                                                    "Village",
                                                    "VILLAGE",
                                                    "VALUATION_ASSESSMENT"))


check2 = subset(compare_interests_8_left_sql, 
                select = c("Name of grievant",
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



### Correct Village Names

Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "KIZZA YUSUFU")
] = "KYEMANDWA A"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "LWAMBONERA YOSEFU")
] = "KYEMANDWA A"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "NASUUNA ASIAT")
] = "KYEMANDWA A"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "KATO AFANE BWANIKA")
] = "KASEKERA"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "LUBEGA CHRISTOPHER")
] = "KITEMBO A"


Grievances_Sembabule$Village[
  which((Grievances_Sembabule$`Name of grievant` == 
          "KASOMA MOSES") & 
          (Grievances_Sembabule$Village == 
             "KIBUUBU"))
] = "KIBUBBU"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "KYEBITONDO JOVIA")
] = "MUCHWA"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "NAMUKASA ALICE")
] = "KABAYOOLA"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "NALUGO MARY")
] = "KABAYOOLA"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "SSENYONDO FRANK")
] = "KABAYOOLA"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "KIYIMBA DENIS")
] = "KABAYOOLA"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "NYINDO SAMUEL")
] = "KIBUBBU"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "NAMATOVU EVA (SPOUSE TO PAP MISIINDE JOSEPH)")
] = "LUTIRWANJUKI"


Grievances_Sembabule$Village[
  which(Grievances_Sembabule$`Name of grievant` == 
          "NDYAREBA EZRA")
] = "KITEMBO B"



### Correct PAP Names

Grievances_Sembabule$`Name of grievant`[
  which(Grievances_Sembabule$`Name of grievant` == 
          "KIZZA YUSUFU")
] = "KIZZA YUSUF"


Grievances_Sembabule$`Name of grievant`[
  which(Grievances_Sembabule$`Name of grievant` == 
          "NAMATOVU EVA (SPOUSE TO PAP MISIINDE JOSEPH)")
] = "NAMATOVU EVA"


Grievances_Sembabule$`Name of grievant`[
  which(Grievances_Sembabule$`Name of grievant` == 
          "MIRIMO MICHEAL MATOVU (FIRST GRIEVANCE)")
] = "MIRIMO MICHEAL MATOVU"



Grievances_Sembabule$`Name of grievant`[
  which(Grievances_Sembabule$`Name of grievant` == 
          "MIRIMO MICHEAL MATOVU (SECOND GRIEVANCE)")
] = "MIRIMO MICHEAL MATOVU"


Grievances_Sembabule$`Name of grievant`[
  which(Grievances_Sembabule$`Name of grievant` == 
          "LUBEGA BEN THE LC1 CHAIRMAN")
] = "LUBEGA BEN"



Grievances_Sembabule$`Name of grievant`[
  which(Grievances_Sembabule$`Name of grievant` %in% 
          check3$`Name of grievant`)] ==
  
  check3$PAP_NAME[
    which( Grievances_Sembabule$`Name of grievant`[
      which(Grievances_Sembabule$`Name of grievant` %in% 
              check3$`Name of grievant`)] %in% 
        check3$`Name of grievant`)]



## Repeat ....

compare_interests_8_left = left_join(x = Grievances_Sembabule,
                                     y = compare_interests_7,
                                     by = c("Name of grievant" = "PAP_NAME",
                                            "Village" = "VILLAGE"),
                                     keep = TRUE)


compare_interests_8_left_sql = sqldf("select *
                              from Grievances_Sembabule a
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

Sembabule_joined_8 = left_join(x = Sembabule_joined_6,
                            y = Grievances_Sembabule,
                            by = c("PAP_NAME" = "Name of grievant",
                                   "VILLAGE" = "Village"),
                            keep = TRUE)


colnames(Sembabule_joined_6)[
  which(colnames(Sembabule_joined_6) == "ID NUMBER")] = "ID_NUMBER_2"

Sembabule_joined_8_sql = sqldf("select *
                           from Sembabule_joined_6 a
                           left join Grievances_Sembabule b
                           on a.PAP_NAME like '%'||b.'Name of grievant'||'%'
                           and a.'VILLAGE' like '%'||b.Village||'%'
                           WHERE 1;", 
                            method = "raw")

length(unique(Sembabule_joined_8$PAP_VALUATION_ASSESSMENT_REF))


## The lines below require independent judgement
Duplicate_Interests = Sembabule_joined_8 %>%
  filter(duplicated(cbind(PAP_NAME, 
                          PAP_VALUATION_ASSESSMENT_REF)))


Sembabule_joined_8 = Sembabule_joined_8 %>%
  distinct(PAP_NAME, PAP_VALUATION_ASSESSMENT_REF, .keep_all = TRUE)


compare_interests_8 = subset(Sembabule_joined_8,
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

write.csv(Sembabule_joined_8, 
          file="Sembabule_joined_8.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_8 = read_csv("compare_interests_8.csv")
Sembabule_joined_8 = read_csv("Sembabule_joined_8.csv")



# DISCLOSURE ...

Disclosure_Sembabule = 
  read_excel("08.12. 21 - Sembabule Disclosed to PAPs and their spouses.xlsx", 
             sheet = "COMBINED",
             col_types = c("numeric","date", "text", "numeric", "text", 
                            "text", "text", "text", "text", "text", 
                            "text", "text", "text", "text"))


Unaccounted_disclosure_1 = 
  anti_join(x = compare_interests_8, 
            y = Disclosure_Sembabule, 
            by = c("VALUATION_ASSESSMENT" = 
                   "PAP Reference Number in approved VR"))


Unaccounted_disclosure_2 = 
  anti_join(x = Disclosure_Sembabule,
            y = compare_interests_8,
            by = c("PAP Reference Number in approved VR" = 
                   "VALUATION_ASSESSMENT"))


###  Corrections ...

Disclosure_Sembabule$`PAP Reference Number in approved VR`[
  which(Disclosure_Sembabule$`PAP Reference Number in approved VR` == 
          "MCPY3/L0-01/F/LC-06")
] = "MCPY3/LO-01/F/LC-06"


Disclosure_Sembabule$`PAP Reference Number in approved VR`[
  which(Disclosure_Sembabule$`PAP Reference Number in approved VR` == 
          "MCPY3/LO-02/M/KO-04/LC-01")
] = "MCPY 3 / LO-02 / M / KO-04 / LC-01"


Disclosure_Sembabule$`PAP Reference Number in approved VR`[
  which(Disclosure_Sembabule$`PAP Reference Number in approved VR` == 
          "SSE/KAT/KIT-B-L003B-00-00/5265")
] = "SSE/KAT/KIT-B/L003B-00-00/5265"


Unaccounted_disclosure_1 = 
  anti_join(x = compare_interests_8, 
            y = Disclosure_Sembabule, 
            by = c("VALUATION_ASSESSMENT" = 
                     "PAP Reference Number in approved VR"))


Unaccounted_disclosure_2 = 
  anti_join(x = Disclosure_Sembabule,
            y = compare_interests_8,
            by = c("PAP Reference Number in approved VR" = 
                     "VALUATION_ASSESSMENT"))



Sembabule_joined_9 = 
  left_join(x = Sembabule_joined_8, 
            y = Disclosure_Sembabule, 
            by = c("PAP_VALUATION_ASSESSMENT_REF" = 
                    "PAP Reference Number in approved VR"),
            keep = TRUE)


compare_interests_9 = subset(Sembabule_joined_9,
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
                                        "PAP Dislosed to their values",
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

write.csv(Sembabule_joined_9, 
          file="Sembabule_joined_9.csv", row.names = FALSE)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_9 = read_csv("compare_interests_9.csv")
Sembabule_joined_9 = read_csv("Sembabule_joined_9.csv")



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

SNAGS_Sembabule = subset(SNAGS_All, SNAGS_All$DISTRICT == "SSEMBABULE")


## checking for unaccounted ...

SNAGS_Unaccounted_1a = anti_join(x = compare_interests_9,
                                 y = SNAGS_Sembabule,
                                 by = c("PAP_NAME" = "NAME OF PAP"))


SNAGS_Unaccounted_1b = sqldf("select * 
             from compare_interests_9 a
             where  not exists (
             select null from SNAGS_Sembabule b
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_1c = sqldf("select * 
             from compare_interests_9 a
             where  not exists (
             select null from SNAGS_Sembabule b
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")

SNAGS_Unaccounted_1b$PAP_NAME[
  which(!(SNAGS_Unaccounted_1b$PAP_NAME %in% SNAGS_Unaccounted_1c$PAP_NAME))]


SNAGS_Unaccounted_2a = anti_join(x = SNAGS_Sembabule,
                                 y = compare_interests_9,
                                 by = c("NAME OF PAP" =
                                          "PAP_NAME" ) )


SNAGS_Unaccounted_2b = sqldf("select * 
             from SNAGS_Sembabule b
             where  not exists (
             select null from compare_interests_9 a
             where  a.PAP_NAME = b.'NAME OF PAP');", 
                             method = "raw")


SNAGS_Unaccounted_2c = sqldf("select * 
             from SNAGS_Sembabule b
             where  not exists (
             select null from compare_interests_9 a
             where a.PAP_NAME like '%'||b.'NAME OF PAP'||'%');", 
                             method = "raw")


SNAGS_Unaccounted_2b$`NAME OF PAP`[
  which(!(SNAGS_Unaccounted_2b$`NAME OF PAP` %in% 
            SNAGS_Unaccounted_2c$`NAME OF PAP` ))]


### Replace names ...

SNAGS_Sembabule$`NAME OF PAP`[which(SNAGS_Sembabule$`PAP REF.NO.` ==
                                     "SSE/KIM/374")] =
  
  compare_interests_9$PAP_NAME[
    which( (compare_interests_9$EACOP_PAP_REF == "SSE/KIM/374") ) ]


### Repeat check ...



### Finally joining ...

SNAGS_Sembabule_joined = left_join(x = compare_interests_9,
                                  y = SNAGS_Sembabule,
                                  by = c("PAP_NAME" = "NAME OF PAP"),
                                  keep = TRUE)


## With Duplicates Removed From SNAGS
SNAGS_Sembabule_joined = SNAGS_Sembabule_joined %>%
  distinct(VALUATION_ASSESSMENT, `SNAG ISSUE`, REMARKS,
           .keep_all = TRUE)


SNAGS_Sembabule_joined$VALUATION_ASSESSMENT[which(
  duplicated(SNAGS_Sembabule_joined$VALUATION_ASSESSMENT)
)]




library(sqldf)
check1 = sqldf("select PAP_NAME, VALUATION_ASSESSMENT, 
          group_concat(REMARKS, '; ') REMARKS, 
          group_concat(`SNAG ISSUE`, '; ') `SNAG ISSUE`
          from SNAGS_Sembabule_joined group by VALUATION_ASSESSMENT", 
               method = "raw")


check2 =
  SNAGS_Sembabule_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarise(REMARKS = toString(REMARKS)) %>%
  ungroup()



library(stringr)
check3 =
  SNAGS_Sembabule_joined %>%
  group_by(VALUATION_ASSESSMENT) %>%
  summarize(REMARKS = str_c(REMARKS, collapse = "; "))




### Rename columns

colnames(SNAGS_Sembabule_joined)[
  which(colnames(SNAGS_Sembabule_joined) == 
          "Date when snag was handled (If fieldwork was needed)")] = 
  "DATE_SNAG_Handled"


colnames(SNAGS_Sembabule_joined)[
  which(colnames(SNAGS_Sembabule_joined) == 
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
    from SNAGS_Sembabule_joined group by VALUATION_ASSESSMENT", 
                             method = "raw")


colnames(compare_interests_10)[
  which(colnames(compare_interests_10) == 
          "DISTRICT.x")] = "DISTRICT"


write.csv(compare_interests_10, 
          file="compare_interests_10.csv", row.names = FALSE)


### Note: Sembabule has pending issue on snags


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(readxl)
library(sqldf)

compare_interests_10 = read_csv("compare_interests_10.csv")


## **********************************************
## VULNERABLE PAPs
## **********************************************

Vulnerable_Sembabule = read_excel("Vulnerable PAPs ALL Edited.xlsx", 
                               sheet = "SEMBABULE")


## checking for unaccounted ...

Vulnerable_Unaccounted_1a = anti_join(x = compare_interests_10,
                                      y = Vulnerable_Sembabule,
                                      by = c("PAP_NAME" = "Name of PAP"))


Vulnerable_Unaccounted_1b = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Sembabule b
             where a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_1c = sqldf("select * 
             from compare_interests_10 a
             where  not exists (
             select null from Vulnerable_Sembabule b
             where a.PAP_NAME like '%'||b.'Name of PAP'||'%' );", 
                                  method = "raw")

Vulnerable_Unaccounted_1b$PAP_NAME[
  which(!(Vulnerable_Unaccounted_1b$PAP_NAME %in% Vulnerable_Unaccounted_1c$PAP_NAME))]


Vulnerable_Unaccounted_2a = anti_join(x = Vulnerable_Sembabule,
                                      y = compare_interests_10,
                                      by = c("Name of PAP" = "PAP_NAME") )


Vulnerable_Unaccounted_2b = sqldf("select * 
             from Vulnerable_Sembabule b
             where  not exists (
             select null from compare_interests_10 a
             where  a.PAP_NAME = b.'Name of PAP');", 
                                  method = "raw")


Vulnerable_Unaccounted_2c = sqldf("select * 
             from Vulnerable_Sembabule b
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

Vulnerable_Sembabule$Name_of_PAP_2 = Vulnerable_Sembabule$`Name of PAP`


## rename

Vulnerable_Sembabule$`Name of PAP`[which(Vulnerable_Sembabule$`Name of PAP` ==
                                        "MUGUME DAWSON & AIDAH MUGUME")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "MUGUME AIDAH & MUGUME DAWSON") ]


Vulnerable_Sembabule$`Name of PAP`[which(Vulnerable_Sembabule$`Name of PAP` ==
                                        "NABUUKERA ANNA MARIA")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "NABUKERA ANNA MARIA") ]


Vulnerable_Sembabule$`Name of PAP`[which(Vulnerable_Sembabule$`Name of PAP` ==
                                        "KANYANKOLE SEBASTAIN")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "KANYANKOLE SEBASTIAN") ]


Vulnerable_Sembabule$`Name of PAP`[which(Vulnerable_Sembabule$`Name of PAP` ==
                                        "KATEEBA ZEPHANIAH")] =
  
  compare_interests_10$PAP_NAME[
    which(compare_interests_10$PAP_NAME == "KATEEBA ZEPHANIA") ]



### Finally joining ...

Sembabule_joined_11 = left_join(x = compare_interests_10,
                             y = Vulnerable_Sembabule,
                             by = c("PAP_NAME" = 
                                      "Name of PAP"),
                             keep = TRUE)


## With Duplicates Removed From Vulnerable
Sembabule_joined_11 = Sembabule_joined_11 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Sembabule_joined_11$VALUATION_ASSESSMENT[which(
  duplicated(Sembabule_joined_11$VALUATION_ASSESSMENT)
)]


check1 = subset(Sembabule_joined_11, 
                select = c("PAP_NAME", 
                           "Name_of_PAP_2",
                           "EACOP_PAP_REF",
                           "EACOP Ref.NUMBER"))

colnames(Sembabule_joined_11)[
  which(startsWith(colnames(Sembabule_joined_11), 
                   prefix = "Description of") )] = "Vulnerability"

colnames(Sembabule_joined_11)[ 
  which(colnames(Sembabule_joined_11) == "Comment")] = 
  "Comment_Vulnerability"


compare_interests_11 = 
  subset(Sembabule_joined_11, 
         select = colnames(Sembabule_joined_11)[c(1:28,35,36)] )

write.csv(compare_interests_11, 
          file="compare_interests_11.csv", row.names = FALSE)

write.csv(Sembabule_joined_11, 
          file="Sembabule_joined_11.csv", row.names = FALSE)



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
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "MCPY3/LO-01/F/LC-05")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "MCPY3/LO-01/F/LC-05")]




### Update 2 ***************************************************

compare_interests_11$PAP_FIRST_NAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$PAP_SURNAME[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$NATIONAL_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$MOBILE_TEL[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$DATE_OF_BIRTH[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$CUST_ID[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$ACCOUNT_TYPE[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$ACCOUNT_DESCRIPTION[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


compare_interests_11$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_11$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L002A-06-01/1449")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L002A-06-01/1449")]


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

R_H_Sembabule = subset(Replacement_Housing,
                        subset = (Replacement_Housing$DISTRICT ==
                                    "SEMBABULE"))

colnames(R_H_Sembabule)
colnames(compare_interests_11)



## checking for unaccounted ...

R_H_Unaccounted_1a = anti_join(x = compare_interests_11,
                               y = R_H_Sembabule,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "PAP VALUATION ASSESSMENT REF"))


R_H_Unaccounted_1b = sqldf("select * 
             from compare_interests_11 a
             where  not exists (
             select null from R_H_Sembabule b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%' );", 
                           method = "raw")


R_H_Unaccounted_1a$PAP_NAME[
  which(!(R_H_Unaccounted_1a$PAP_NAME %in% 
            R_H_Unaccounted_1b$PAP_NAME))]


R_H_Unaccounted_2a = anti_join(x = R_H_Sembabule,
                               y = compare_interests_11,
                               by = c("PAP VALUATION ASSESSMENT REF" = 
                                        "VALUATION_ASSESSMENT") )


R_H_Unaccounted_2b = sqldf("select * 
             from R_H_Sembabule b
             where  not exists (
             select null from compare_interests_11 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'PAP VALUATION ASSESSMENT REF'||'%');", 
                           method = "raw")


R_H_Unaccounted_2b$`PAP REF NO`[
  which(!(R_H_Unaccounted_2b$`PAP REF NO` %in% 
            R_H_Unaccounted_2a$`PAP REF NO` ))]



### Finally joining ...

Sembabule_joined_12 = left_join(x = compare_interests_11,
                                 y = R_H_Sembabule,
                                 by = c("VALUATION_ASSESSMENT" = 
                                          "PAP VALUATION ASSESSMENT REF"),
                                 keep = TRUE)


## With Duplicates Removed
Sembabule_joined_12 = Sembabule_joined_12 %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Sembabule_joined_12$VALUATION_ASSESSMENT[which(
  duplicated(Sembabule_joined_12$VALUATION_ASSESSMENT)
)]


check1 = subset(Sembabule_joined_12, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "EACOP_PAP_REF",
                           "PAP REF NO"))


colnames(Sembabule_joined_12)[
  which(colnames(Sembabule_joined_12) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Sembabule_joined_12)[
  which(colnames(Sembabule_joined_12) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_12 = 
  subset(Sembabule_joined_12, 
         select = colnames(Sembabule_joined_12)[c(1:30,36:37)] )

write.csv(compare_interests_12, 
          file="compare_interests_12.csv", row.names = FALSE)

write.csv(Sembabule_joined_12, 
          file="Sembabule_joined_12.csv", row.names = FALSE)




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

Unmarked_Graves_Sembabule = subset(Unmarked_Graves,
                                    subset = (Unmarked_Graves$DISTRICT ==
                                                "SEMBABULE"))

Marked_Graves_Sembabule = subset(Marked_Graves,
                                  subset = (Marked_Graves$DISTRICT ==
                                              "SEMBABULE"))


colnames(Unmarked_Graves_Sembabule)
colnames(compare_interests_12)


## checking for unaccounted ...

U_G_Unaccounted_1a = anti_join(x = compare_interests_12,
                               y = Unmarked_Graves_Sembabule,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


U_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_12 a
             where  not exists (
             select null from Unmarked_Graves_Sembabule b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


U_G_Unaccounted_1a$PAP_NAME[
  which(!(U_G_Unaccounted_1a$PAP_NAME %in% 
            U_G_Unaccounted_1b$PAP_NAME))]


U_G_Unaccounted_2a = anti_join(x = Unmarked_Graves_Sembabule,
                               y = compare_interests_12,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


U_G_Unaccounted_2b = sqldf("select * 
             from Unmarked_Graves_Sembabule b
             where  not exists (
             select null from compare_interests_12 a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


U_G_Unaccounted_2b$`PAP NAME`[
  which(!(U_G_Unaccounted_2b$`PAP NAME` %in% 
            U_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Sembabule_joined_13a = left_join(x = compare_interests_12,
                                  y = Unmarked_Graves_Sembabule,
                                  by = c("VALUATION_ASSESSMENT" = 
                                           "ASSESSMENT REFERENCE NUMBER"),
                                  keep = TRUE)


## With Duplicates Removed
Sembabule_joined_13a = Sembabule_joined_13a %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Sembabule_joined_13a$VALUATION_ASSESSMENT[which(
  duplicated(Sembabule_joined_13a$VALUATION_ASSESSMENT)
)]


check1 = subset(Sembabule_joined_13a, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Sembabule_joined_13a)[
  which(colnames(Sembabule_joined_13a) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Sembabule_joined_13a)[
  which(colnames(Sembabule_joined_13a) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13a = 
  subset(Sembabule_joined_13a, 
         select = colnames(Sembabule_joined_13a)[c(1:32,36)] )



## checking for unaccounted ...

colnames(Marked_Graves_Sembabule)

M_G_Unaccounted_1a = anti_join(x = compare_interests_13a,
                               y = Marked_Graves_Sembabule,
                               by = c("VALUATION_ASSESSMENT" = 
                                        "ASSESSMENT REFERENCE NUMBER"))


M_G_Unaccounted_1b = sqldf("select * 
             from compare_interests_13a a
             where  not exists (
             select null from Marked_Graves_Sembabule b
             where a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%' );", 
                           method = "raw")


M_G_Unaccounted_1a$PAP_NAME[
  which(!(M_G_Unaccounted_1a$PAP_NAME %in% 
            M_G_Unaccounted_1b$PAP_NAME))]


M_G_Unaccounted_2a = anti_join(x = Marked_Graves_Sembabule,
                               y = compare_interests_13a,
                               by = c("ASSESSMENT REFERENCE NUMBER" = 
                                        "VALUATION_ASSESSMENT") )


M_G_Unaccounted_2b = sqldf("select * 
             from Marked_Graves_Sembabule b
             where  not exists (
             select null from compare_interests_13a a
             where  a.VALUATION_ASSESSMENT like 
                           '%'||b.'ASSESSMENT REFERENCE NUMBER'||'%');", 
                           method = "raw")


M_G_Unaccounted_2b$`PAP NAME`[
  which(!(M_G_Unaccounted_2b$`PAP NAME` %in% 
            M_G_Unaccounted_2a$`PAP NAME` ))]



### Finally joining ...

Sembabule_joined_13b = left_join(x = compare_interests_13a,
                                  y = Marked_Graves_Sembabule,
                                  by = c("VALUATION_ASSESSMENT" = 
                                           "ASSESSMENT REFERENCE NUMBER"),
                                  keep = TRUE)


## With Duplicates Removed
Sembabule_joined_13b = Sembabule_joined_13b %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME,
           .keep_all = TRUE)


Sembabule_joined_13b$VALUATION_ASSESSMENT[which(
  duplicated(Sembabule_joined_13b$VALUATION_ASSESSMENT)
)]


check1 = subset(Sembabule_joined_13b, 
                select = c("PAP_NAME", 
                           "PAP NAME",
                           "VALUATION_ASSESSMENT",
                           "ASSESSMENT REFERENCE NUMBER"))


colnames(Sembabule_joined_13b)[
  which(colnames(Sembabule_joined_13b) == "DISTRICT.x")] =
  "DISTRICT"

colnames(Sembabule_joined_13b)[
  which(colnames(Sembabule_joined_13b) == "SUBCOUNTY.x")] =
  "SUBCOUNTY"

colnames(Sembabule_joined_13b)[
  which(colnames(Sembabule_joined_13b) == "VILLAGE.x")] =
  "VILLAGE"

compare_interests_13b = 
  subset(Sembabule_joined_13b, 
         select = colnames(Sembabule_joined_13b)[c(1:33,37)] )


compare_interests_13 = compare_interests_13b

Sembabule_joined_13 = Sembabule_joined_13b


write.csv(compare_interests_13, 
          file="compare_interests_13.csv", row.names = FALSE)

write.csv(Sembabule_joined_13, 
          file="Sembabule_joined_13.csv", row.names = FALSE)




### *** May 19th bank information update ***

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
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]



### Update 2 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/MIT/L001A-15-00/1472")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/MIT/L001A-15-00/1472")]



### Update 3 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-31-01/3509")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-31-01/3509")]



### Update 4 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/LWE/KIB/L001-05-01/1635")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/LWE/KIB/L001-05-01/1635")]



### Update 5 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KYE/L002-15-01/3756")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KYE/L002-15-01/3756")]



### Update 6 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]



### Update 7 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-04/1978")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-04/1978")]



### Update 8 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-03/1978")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-03/1978")]



### Update 9 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KAS/L002A-05-00/5221")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KAS/L002A-05-00/5221")]




### Update 10 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/KAW/NAK/L002A-06-00/944")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/KAW/NAK/L002A-06-00/944")]



### Update 11 **************************************************

compare_interests_13$PAP_FIRST_NAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$FIRST_NAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$PAP_SURNAME[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$SURNAME[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$NATIONAL_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$ID_NO[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$MOBILE_TEL[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$MOBILE_TEL[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$DATE_OF_BIRTH[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$DATE_OF_BIRTH[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$CUST_ID[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$CUST_ID[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$ACCOUNT_TYPE[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$ACCT_TYPE[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$ACCOUNT_DESCRIPTION[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$ACCOUNT_DESCRIPTION[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


compare_interests_13$CENTENARY_BANK_ACCOUNT_NUMBER[
  which(compare_interests_13$VALUATION_ASSESSMENT == 
          "SSE/MIJ/KIS/L001-00-02/1978")] =
  All_IWA_inner$ACCOUNT_NUMBER[
    which(All_IWA_inner$PAP_VALUATION_ASSESSMENT_REF ==
            "SSE/MIJ/KIS/L001-00-02/1978")]


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


### experimental ....

compare_interests_13[,"POA"] = 
  ifelse(str_detect(compare_interests_13$`SNAG ISSUE`, 
                    regex('POA', ignore_case = TRUE)),"YES",NA)


compare_interests_13$POA = 
  ifelse(str_detect(compare_interests_13$`SNAG ISSUE`, 
                    regex('POA', ignore_case = TRUE)) &
           (nchar(compare_interests_13$`SNAG ISSUE`) == 
              nchar('POA')),"YES",NA)


### implementation ....

compare_interests_13 = read_csv("compare_interests_13.csv")

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
