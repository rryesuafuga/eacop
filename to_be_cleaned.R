

options(error = rlang::entrace)
utils:::make.packages.html(temp = TRUE)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)


# TO BE CLEANED


## data of Control
Control = read_excel("EACOP PAP REF NO DETAILS-011221.xlsx", 
                           sheet = "Sheet1")

## Number of Interests Control
length(unique(Control$PAP_VALUATION_ASSESSMENT_REF))




## Joining with Centenary Core Banking System

to_be_cleaned_up_all = read_excel("EACOP Accounts 25-01-2022.xlsx", 
                                        sheet = "To be cleaned up")


colnames(to_be_cleaned_up_all)[
  which(colnames(to_be_cleaned_up_all) == "NATIONAL ID")] = "ID_NO"

to_be_cleaned_up_all$ACCOUNT_NUMBER =
  as.character(to_be_cleaned_up_all$ACCOUNT_NUMBER)

to_be_cleaned_up_all$ACCOUNT_DESCRIPTION =
  as.character(to_be_cleaned_up_all$ACCOUNT_DESCRIPTION)

to_be_cleaned_up = subset(to_be_cleaned_up_all,
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
                                     "ADDRESS_1",
                                     "OPENING_DATE"))

str(to_be_cleaned_up)
to_be_cleaned_up$ACCOUNT_NUMBER =
  as.character(to_be_cleaned_up$ACCOUNT_NUMBER)

to_be_cleaned_up$ACCOUNT_DESCRIPTION =
  as.character(to_be_cleaned_up$ACCOUNT_DESCRIPTION)


to_be_cleaned_up$DESC_1 = 
  paste(to_be_cleaned_up$FIRST_NAME,
        to_be_cleaned_up$SURNAME)

to_be_cleaned_up$DESC_2 = 
  paste(to_be_cleaned_up$SURNAME,
        to_be_cleaned_up$FIRST_NAME)




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
                                          "ADDRESS_1",
                                          "OPENING_DATE"))


str(centenary_core_needed)
centenary_core_needed$ACCOUNT_NUMBER =
  as.character(centenary_core_needed$ACCOUNT_NUMBER)

centenary_core_needed$ACCOUNT_DESCRIPTION =
  as.character(centenary_core_needed$ACCOUNT_DESCRIPTION)


centenary_core_needed$DESC_1 = 
  paste(centenary_core_needed$FIRST_NAME,
        centenary_core_needed$SURNAME)

centenary_core_needed$DESC_2 = 
  paste(centenary_core_needed$SURNAME,
        centenary_core_needed$FIRST_NAME)




##### ************ another check


bad_mixture = rbind(centenary_core_needed, to_be_cleaned_up)

bad_mixture_2 = rbind(centenary_core_needed, to_be_cleaned_up)

cust_id_not_unique = bad_mixture[
  which((bad_mixture$CUST_ID != bad_mixture_2$CUST_ID) &
          (bad_mixture$ACCOUNT_DESCRIPTION == bad_mixture_2$ACCOUNT_DESCRIPTION)),]


cust_id_not_unique = bad_mixture[
  which((bad_mixture$CUST_ID != bad_mixture_2$CUST_ID) &
          (bad_mixture$DESC_2 == bad_mixture_2$DESC_2)),]



### ********************* Method 1

All_joined_4_left = sqldf("select *
                      from to_be_cleaned_up b
                      left join Control a
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                           method = "raw")


All_joined_4_inner = sqldf("select *
                      from to_be_cleaned_up b
                      inner join Control a
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                          method = "raw")


colnames(All_joined_4_inner)


To_Be_Cleaned_No_Duplicates = All_joined_4_inner %>%
                  filter(!duplicated(cbind(ACCOUNT_DESCRIPTION, 
                                           ID_NO,
                                           ACCOUNT_NUMBER,
                                           ACCT_TYPE,
                                           CUST_ID,
                                           ADDRESS_1)))


To_Be_Cleaned_No_Duplicates_2 <- All_joined_4_inner[
                   !duplicated(All_joined_4_inner[c('ACCOUNT_DESCRIPTION', 
                                                    'ID_NO',
                                                    'ACCOUNT_NUMBER',
                                                    'CUST_ID',
                                                    'ACCT_TYPE',
                                                    'ADDRESS_1')]),] 


write.csv(To_Be_Cleaned_No_Duplicates, 
          file="To_Be_Cleaned_No_Duplicates.csv", row.names = FALSE)



### **************** Method 2

length(to_be_cleaned_up$CUST_ID %in% centenary_core_needed$CUST_ID)

to_be_cleaned_in_cleaned = to_be_cleaned_up[
  which(to_be_cleaned_up$CUST_ID %in% centenary_core_needed$CUST_ID),
]


length(centenary_core_needed$CUST_ID %in% to_be_cleaned_up$CUST_ID)

cleaned_in_to_be_cleaned = centenary_core_needed[
  which(centenary_core_needed$CUST_ID %in% to_be_cleaned_up$CUST_ID),
]


truly_missing = to_be_cleaned_up[
  which(!(to_be_cleaned_up$CUST_ID %in% centenary_core_needed$CUST_ID)),
]


All_joined_4_left = sqldf("select *
                      from truly_missing b
                      left join Control a
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                          method = "raw")


All_joined_4_inner = sqldf("select *
                      from truly_missing b
                      inner join Control a
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                           method = "raw")


colnames(All_joined_4_inner)


Truly_Missing_No_Duplicates = All_joined_4_inner %>%
  filter(!duplicated(cbind(ACCOUNT_DESCRIPTION, 
                           ID_NO,
                           ACCOUNT_NUMBER,
                           ACCT_TYPE,
                           CUST_ID,
                           ADDRESS_1)))


Truly_Missing_No_Duplicates_2 <- All_joined_4_inner[
  !duplicated(All_joined_4_inner[c('ACCOUNT_DESCRIPTION', 
                                   'ID_NO',
                                   'ACCOUNT_NUMBER',
                                   'CUST_ID',
                                   'ACCT_TYPE',
                                   'ADDRESS_1')]),] 


write.csv(Truly_Missing_No_Duplicates, 
          file="Truly_Missing_No_Duplicates.csv", row.names = FALSE)



### ******* Duplicate Truly Missing

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)


Truly_Missing_Corrected = read_excel("Truly_Missing_Corrected.xlsx", 
                                  sheet = "Truly_Missing_Corrected")


D_T_M_C = sqldf("select c.DESC_2, d.DESC_2,
                      c.CUST_ID, d.CUST_ID,
                      c.ACCOUNT_NUMBER, d.ACCOUNT_NUMBER,
                      c.ACCOUNT_DESCRIPTION, d.ACCOUNT_DESCRIPTION,
                      c.ID_NO, d.ID_NO,
                      c.OPENING_DATE, d.OPENING_DATE,
                      c.ADDRESS_1, d.ADDRESS_1,
                      c.MOBILE_TEL, d.MOBILE_TEL,
                      c.TELEPHONE_1, d.TELEPHONE_1
                      from Truly_Missing_Corrected c 
                      inner join Truly_Missing_Corrected d
                      on c.DESC_2 LIKE '%'||d.DESC_2||'%'
                      WHERE c.CUST_ID <> d.CUST_ID ;", 
                           method = "raw")



### ********* Duplicate Customer IDs

Duplicate_CUST_IDs = sqldf("select c.DESC_2, d.DESC_2,
                      c.CUST_ID, d.CUST_ID,
                      c.ACCOUNT_NUMBER, d.ACCOUNT_NUMBER,
                      c.ACCOUNT_DESCRIPTION, d.ACCOUNT_DESCRIPTION,
                      c.ID_NO, d.ID_NO,
                      c.OPENING_DATE, d.OPENING_DATE,
                      c.ADDRESS_1, d.ADDRESS_1,
                      c.MOBILE_TEL, d.MOBILE_TEL,
                      c.TELEPHONE_1, d.TELEPHONE_1
                      from centenary_core_needed c 
                      inner join centenary_core_needed d
                      on c.DESC_2 LIKE '%'||d.DESC_2||'%'
                      WHERE c.CUST_ID <> d.CUST_ID ;", 
                          method = "raw")


write.csv(Duplicate_CUST_IDs, 
          file="Duplicate_CUST_IDs.csv", row.names = FALSE)




Duplicate_CUST_IDs_2 = sqldf("select c.DESC_2, d.DESC_2,
                      c.CUST_ID, d.CUST_ID,
                      c.ACCOUNT_NUMBER, d.ACCOUNT_NUMBER,
                      c.ACCOUNT_DESCRIPTION, d.ACCOUNT_DESCRIPTION,
                      c.ID_NO, d.ID_NO,
                      c.OPENING_DATE, d.OPENING_DATE,
                      c.ADDRESS_1, d.ADDRESS_1,
                      c.MOBILE_TEL, d.MOBILE_TEL,
                      c.TELEPHONE_1, d.TELEPHONE_1
                      from centenary_core_needed c 
                      inner join centenary_core_needed d
                      on c.ACCOUNT_DESCRIPTION LIKE '%'||d.ACCOUNT_DESCRIPTION||'%'
                      WHERE c.CUST_ID <> d.CUST_ID ;", 
                           method = "raw")




Duplicate_CUST_IDs_3 = sqldf("select c.DESC_2, d.DESC_2,
                      c.CUST_ID, d.CUST_ID,
                      c.ACCOUNT_NUMBER, d.ACCOUNT_NUMBER,
                      c.ACCOUNT_DESCRIPTION, d.ACCOUNT_DESCRIPTION,
                      c.ID_NO, d.ID_NO,
                      c.ADDRESS_1, d.ADDRESS_1,
                      c.MOBILE_TEL, d.MOBILE_TEL,
                      c.TELEPHONE_1, d.TELEPHONE_1
                      from centenary_core_needed c 
                      inner join centenary_core_needed d
                      on c.DESC_2 LIKE '%'||d.DESC_2||'%'
                      WHERE c.CUST_ID <> d.CUST_ID 
                        AND c.ID_NO = d.ID_NO ;", 
                             method = "raw")



Duplicate_CUST_IDs_4 = sqldf("select c.DESC_2, d.DESC_2,
                      c.CUST_ID, d.CUST_ID,
                      c.ACCOUNT_NUMBER, d.ACCOUNT_NUMBER,
                      c.ACCOUNT_DESCRIPTION, d.ACCOUNT_DESCRIPTION,
                      c.ID_NO, d.ID_NO,
                      c.ADDRESS_1, d.ADDRESS_1,
                      c.MOBILE_TEL, d.MOBILE_TEL,
                      c.TELEPHONE_1, d.TELEPHONE_1
                      from centenary_core_needed c 
                      inner join centenary_core_needed d
                      on c.DESC_2 LIKE '%'||d.DESC_2||'%'
                      WHERE c.CUST_ID <> d.CUST_ID 
                        AND c.ID_NO LIKE '%'||d.ID_NO||'%' ;", 
                           method = "raw")




### ********** Investigate CUST_IDs more ...

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)

Duplicate_CUST_IDs_5 = read_csv("Duplicate_CUST_IDs.csv")
colnames(Duplicate_CUST_IDs_5)

Duplicate_CUST_IDs_6 = Duplicate_CUST_IDs_5 %>%
  filter(!duplicated(cbind(DESC_2...1)))

Duplicate_CUST_IDs_6 = Duplicate_CUST_IDs_5 %>%
  distinct(DESC_2...1, .keep_all = TRUE)

write.csv(Duplicate_CUST_IDs_6, 
          file="Duplicate_CUST_IDs_6.csv", row.names = FALSE)


## data of Control
Control = read_excel("EACOP PAP REF NO DETAILS-011221.xlsx", 
                     sheet = "Sheet1")

## Number of Interests Control
length(unique(Control$PAP_VALUATION_ASSESSMENT_REF))


colnames(Duplicate_CUST_IDs_6)[1] = "DESC_2"


All_joined_6_left = sqldf("select *
                      from Duplicate_CUST_IDs_6 b
                      left join Control a 
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                          method = "raw")


All_joined_6_inner = sqldf("select *
                      from Duplicate_CUST_IDs_6 b
                      inner join Control a
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                           method = "raw")

colnames(All_joined_6_left)


Duplicate_CUST_IDs_7 = All_joined_6_left %>%
  filter(!duplicated(cbind(DESC_2,
                           DESC_2...2,
                           PAP_NAME,
                           DISTRICT,
                           SUBCOUNTY,
                           VILLAGE)))


write.csv(Duplicate_CUST_IDs_7, 
          file="Duplicate_CUST_IDs_7.csv", row.names = FALSE)


colnames(Duplicate_CUST_IDs_7)

Verify_Addresses = subset(Duplicate_CUST_IDs_7,
                          select = c("DESC_2",
                                     "PAP_NAME",
                                     "ADDRESS_1...13",
                                     "ADDRESS_1...14",
                                     "DISTRICT",
                                     "SUBCOUNTY",
                                     "VILLAGE",
                                     "PAP REF NO",
                                     "ID_NO...9",
                                     "ID_NO...10"))



### ********** Investigate Duplicate CUST IDs more ...

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)

Duplicate_CUST_IDs_Cases = read_csv("Duplicate_CUST_IDs_Cases.csv")

dd = Duplicate_CUST_IDs_Cases[,1:18]

## data of Control
Control = read_excel("EACOP PAP REF NO DETAILS-011221.xlsx", 
                     sheet = "Sheet1")


dd_left_join = left_join(x = dd, 
                         y = Control, 
                         by = c("DESC_2...1" = "PAP_NAME"))



### ***** New accounts ...

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)

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
                                          "ADDRESS_1",
                                          "OPENING_DATE"))


str(centenary_core_needed)
centenary_core_needed$ACCOUNT_NUMBER =
  as.character(centenary_core_needed$ACCOUNT_NUMBER)

centenary_core_needed$ACCOUNT_DESCRIPTION =
  as.character(centenary_core_needed$ACCOUNT_DESCRIPTION)


centenary_core_needed$DESC_1 = 
  paste(centenary_core_needed$FIRST_NAME,
        centenary_core_needed$SURNAME)

centenary_core_needed$DESC_2 = 
  paste(centenary_core_needed$SURNAME,
        centenary_core_needed$FIRST_NAME)



EACOPs_26_Jan_to_15_Mar_2022 = read_excel("EACOPs 26 Jan to 15 Mar 2022.xlsx", 
                                          sheet = "Grid Results")


str(EACOPs_26_Jan_to_15_Mar_2022)
EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_NUMBER =
  as.character(EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_NUMBER)

EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_DESCRIPTION =
  as.character(EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_DESCRIPTION)


EACOPs_26_Jan_to_15_Mar_2022$DESC_1 = 
  paste(EACOPs_26_Jan_to_15_Mar_2022$FIRST_NAME,
        EACOPs_26_Jan_to_15_Mar_2022$SURNAME)

EACOPs_26_Jan_to_15_Mar_2022$DESC_2 = 
  paste(EACOPs_26_Jan_to_15_Mar_2022$SURNAME,
        EACOPs_26_Jan_to_15_Mar_2022$FIRST_NAME)


colnames(EACOPs_26_Jan_to_15_Mar_2022)

EACOPs_26_Jan_to_15_Mar_2022a = EACOPs_26_Jan_to_15_Mar_2022


## With Duplicates Removed
EACOPs_26_Jan_to_15_Mar_2022 = EACOPs_26_Jan_to_15_Mar_2022 %>%
  distinct(ACCOUNT_NUMBER, ID_NO, CUST_ID, 
           ACCOUNT_DESCRIPTION, ACCT_TYPE, DESC_2, 
           .keep_all = TRUE)


which(EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_NUMBER %in% centenary_core_needed$ACCOUNT_NUMBER)

which(EACOPs_26_Jan_to_15_Mar_2022$CUST_ID %in% centenary_core_needed$CUST_ID)

which(EACOPs_26_Jan_to_15_Mar_2022$DESC_2 %in% centenary_core_needed$DESC_2)


IWA_Gomba = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                                          sheet = "Gomba")


IWA_Kyankwanzi = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                       sheet = "Kyankwanzi")


IWA_Kakumiro = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                       sheet = "Kakumiro")


IWA_Lwengo = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                       sheet = "Lwengo")


IWA_Sembabule = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                       sheet = "Sembabule")


colnames(IWA_Kyankwanzi)[which(colnames(IWA_Kyankwanzi) == "VILLAGE.x")] =
  "VILLAGE"


yy = colnames(IWA_Gomba)[
          which( (colnames(IWA_Gomba) %in% colnames(IWA_Kakumiro)) &
         
         (colnames(IWA_Gomba) %in% colnames(IWA_Kyankwanzi)) &
         
         (colnames(IWA_Gomba) %in% colnames(IWA_Lwengo)) &
         
         (colnames(IWA_Gomba) %in% colnames(IWA_Sembabule)) )]


IWA_Combined = rbind(IWA_Gomba[,yy],
                     IWA_Kakumiro[,yy],
                     IWA_Kyankwanzi[,yy],
                     IWA_Lwengo[,yy],
                     IWA_Sembabule[,yy])  

colnames(IWA_Kyankwanzi)[which(colnames(IWA_Kyankwanzi) == "VILLAGE.x")] =
  "VILLAGE"

# repeat refresh

IWA_Combined = IWA_Combined[,1:which(colnames(IWA_Combined)=="COMMENTS:")]


which(EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_NUMBER %in% IWA_Combined$`centenary-bankaccount`)

which(EACOPs_26_Jan_to_15_Mar_2022$ID_NO %in% IWA_Combined$`ID Number`)

EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_DESCRIPTION[
which(EACOPs_26_Jan_to_15_Mar_2022$ID_NO %in% IWA_Combined$`ID Number`)]


All_IWA_inner = sqldf("select *
                      from IWA_Combined a
                      inner join EACOPs_26_Jan_to_15_Mar_2022 b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                           method = "raw")

colnames(All_IWA_inner)

## With Duplicates Removed
All_IWA_inner = All_IWA_inner %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, PAP_NAME, `PAP REF NO`, 
           DISTRICT.x, SUBCOUNTY, VILLAGE, 
           CUST_ID, ACCOUNT_NUMBER,
           .keep_all = TRUE)


check1 = All_IWA_inner[,c("PAP_NAME","DESC_2",
                          "DISTRICT.x","SUBCOUNTY","VILLAGE","ADDRESS_1",
                          "ID Number","ID_NO",
                          "centenary-bankaccount","ACCOUNT_NUMBER", "CUST_ID",
                          "PAP REF NO", "PAP_VALUATION_ASSESSMENT_REF")]

unique(All_IWA_inner$PAP_NAME)

unique(EACOPs_26_Jan_to_15_Mar_2022$DESC_2)


setdiff(unique(EACOPs_26_Jan_to_15_Mar_2022$DESC_2), 
        unique(All_IWA_inner$DESC_2))


EACOPs_26_Jan_to_15_Mar_2022$DESC_2[ which(
  EACOPs_26_Jan_to_15_Mar_2022$DESC_2 == "MUSISI SIMONPETER")  ] =
  "MUSISI SIMON"

EACOPs_26_Jan_to_15_Mar_2022$DESC_2[ which(
  EACOPs_26_Jan_to_15_Mar_2022$DESC_2 == "KAMANYIRE JOSEPH")  ] =
  "KARAMAGI KAMANYIRE JOSEPH"


## data of Control
Control = read_excel("EACOP PAP REF NO DETAILS-011221.xlsx", 
                     sheet = "Sheet1")


EACOPs_26_Jan_to_15_Mar_2022$DESC_2[ which(
  EACOPs_26_Jan_to_15_Mar_2022$DESC_2 == "SSEMATA ACHILLES")  ] =
  
  Control$PAP_NAME[which(Control$`PAP REF NO` == "LWE/NKO/145")]


EACOPs_26_Jan_to_15_Mar_2022$DESC_2[ which(
  EACOPs_26_Jan_to_15_Mar_2022$DESC_2 == "AKATUHURIRA GIDON")  ] =
  
  Control$PAP_NAME[which(Control$`PAP REF NO` == "KAK/KIS-B/171")]


### LWE/NKO/145
### KAK/KIS-B/171

## refresh ...


check1 = All_IWA_inner[,c("PAP_NAME","DESC_2",
                          "DISTRICT.x","SUBCOUNTY","VILLAGE","ADDRESS_1",
                          "ID Number","ID_NO",
                          "centenary-bankaccount","ACCOUNT_NUMBER", "CUST_ID",
                          "PAP REF NO", "PAP_VALUATION_ASSESSMENT_REF")]


write.csv(All_IWA_inner, 
          file="All_IWA_inner.csv", row.names = FALSE)




# **** Testing with PAPS WITHOUT BANK ACCOUNTS-FINAL

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)

PAPS_WITHOUT_BANK_ACCOUNTS = read_excel("PAPS WITHOUT BANK ACCOUNTS-FINAL.xlsx", 
                                          sheet = "Sheet1")

colnames(PAPS_WITHOUT_BANK_ACCOUNTS)


Plus_IWA_inner = sqldf("select *
                      from PAPS_WITHOUT_BANK_ACCOUNTS a
                      inner join EACOPs_26_Jan_to_15_Mar_2022 b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                      method = "raw")


## With Duplicates Removed
Plus_IWA_inner = Plus_IWA_inner %>%
  distinct(VALUATION_ASSESSMENT, PAP_NAME, EACOP_PAP_REF, 
           DISTRICT, SUBCOUNTY, VILLAGE, 
           .keep_all = TRUE)


EACOPs_26_Jan_to_15_Mar_2022$DESC_2[ which(
  EACOPs_26_Jan_to_15_Mar_2022$DESC_2 == "KAMANYIRE JOSEPH")  ] =
  "KARAMAGI KAMANYIRE JOSEPH"

### refresh ...



## anti-join Where do these other accounts come from?

Unclear_accounts = sqldf("select * 
             from EACOPs_26_Jan_to_15_Mar_2022 b
             where  not exists (
             select null from PAPS_WITHOUT_BANK_ACCOUNTS a
             where a.PAP_NAME like '%'||b.DESC_2||'%');", 
                                  method = "raw")

colnames(Unclear_accounts)

Unclear_accounts = Unclear_accounts %>%
  distinct(ACCOUNT_NUMBER, ID_NO, CUST_ID, ACCOUNT_DESCRIPTION, DESC_2,
           .keep_all = TRUE)


check3 = Unclear_accounts[,c("DESC_2", "ADDRESS_1",  "ID_NO", "CUST_ID", "ACCOUNT_NUMBER" )]

Not_Expected = sqldf("select *
                      from Unclear_accounts a
                      inner join centenary_core_needed b
                      on a.DESC_2 like '%'||b.DESC_2||'%';", 
                       method = "raw")

colnames(Not_Expected)


check_not_expected = Not_Expected[,c(27,42,3,35,1,32,2,31,14,28,13,33)]


Truly_Missing_No_Duplicates = read_csv("Truly_Missing_No_Duplicates.csv")


Ghost = Unclear_accounts[which(Unclear_accounts$DESC_2 == "SSEMATA ACHILLES" |
                                 Unclear_accounts$DESC_2 == "AKATUHURIRA GIDON"),]


## data of Control
Control = read_excel("EACOP PAP REF NO DETAILS-011221.xlsx", 
                     sheet = "Sheet1")


check_5 = sqldf("select *
                 from Ghost a
                 inner join Control b
                 on a.DESC_2 like '%'||b.PAP_NAME||'%';", 
                     method = "raw")


write.csv(check_4, 
          file="Already_Have_Account_Duplicates.csv", row.names = FALSE)

### LWE/NKO/145
### KAK/KIS-B/171


write.csv(Ghost, 
          file="Ghost_accounts.csv", row.names = FALSE)





### ***** May 18th 2022 *****

### ***** New accounts ....

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(dplyr)
library(readr)
library(sqldf)

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
                                          "ADDRESS_1",
                                          "OPENING_DATE"))


centenary_core_needed$ACCOUNT_NUMBER =
  as.character(centenary_core_needed$ACCOUNT_NUMBER)

centenary_core_needed$ACCOUNT_DESCRIPTION =
  as.character(centenary_core_needed$ACCOUNT_DESCRIPTION)


centenary_core_needed$DESC_1 = 
  paste(centenary_core_needed$FIRST_NAME,
        centenary_core_needed$SURNAME)

centenary_core_needed$DESC_2 = 
  paste(centenary_core_needed$SURNAME,
        centenary_core_needed$FIRST_NAME)



EACOPs_26_Jan_to_15_Mar_2022 = read_excel("EACOPs 26 Jan to 15 Mar 2022.xlsx", 
                                          sheet = "Grid Results")


str(EACOPs_26_Jan_to_15_Mar_2022)
EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_NUMBER =
  as.character(EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_NUMBER)

EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_DESCRIPTION =
  as.character(EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_DESCRIPTION)


EACOPs_26_Jan_to_15_Mar_2022$DESC_1 = 
  paste(EACOPs_26_Jan_to_15_Mar_2022$FIRST_NAME,
        EACOPs_26_Jan_to_15_Mar_2022$SURNAME)

EACOPs_26_Jan_to_15_Mar_2022$DESC_2 = 
  paste(EACOPs_26_Jan_to_15_Mar_2022$SURNAME,
        EACOPs_26_Jan_to_15_Mar_2022$FIRST_NAME)


colnames(EACOPs_26_Jan_to_15_Mar_2022)

EACOPs_26_Jan_to_15_Mar_2022a = EACOPs_26_Jan_to_15_Mar_2022


## With Duplicates Removed
EACOPs_26_Jan_to_15_Mar_2022 = EACOPs_26_Jan_to_15_Mar_2022 %>%
  distinct(ACCOUNT_NUMBER, ID_NO, CUST_ID, 
           ACCOUNT_DESCRIPTION, ACCT_TYPE, DESC_2, 
           .keep_all = TRUE)


#### new data ...

EACOPs_16_Mar_to_04_May_2022 = 
        read_excel("EACOP Accounts 16-03-2022 to 04-05-2022.xlsx", 
                    sheet = "Grid Results")


str(EACOPs_16_Mar_to_04_May_2022)
EACOPs_16_Mar_to_04_May_2022$ACCOUNT_NUMBER =
  as.character(EACOPs_16_Mar_to_04_May_2022$ACCOUNT_NUMBER)

EACOPs_16_Mar_to_04_May_2022$ACCOUNT_DESCRIPTION =
  as.character(EACOPs_16_Mar_to_04_May_2022$ACCOUNT_DESCRIPTION)


EACOPs_16_Mar_to_04_May_2022$DESC_1 = 
  paste(EACOPs_16_Mar_to_04_May_2022$FIRST_NAME,
        EACOPs_16_Mar_to_04_May_2022$SURNAME)

EACOPs_16_Mar_to_04_May_2022$DESC_2 = 
  paste(EACOPs_16_Mar_to_04_May_2022$SURNAME,
        EACOPs_16_Mar_to_04_May_2022$FIRST_NAME)

colnames(EACOPs_16_Mar_to_04_May_2022)[
  which(colnames(EACOPs_16_Mar_to_04_May_2022) == "ACCOUNT_TYPE")] =
  "ACCT_TYPE"

colnames(EACOPs_16_Mar_to_04_May_2022)

EACOPs_16_Mar_to_04_May_2022a = EACOPs_16_Mar_to_04_May_2022


## With Duplicates Removed
EACOPs_16_Mar_to_04_May_2022 = EACOPs_16_Mar_to_04_May_2022 %>%
  distinct(ACCOUNT_NUMBER, ID_NO, CUST_ID, 
           ACCOUNT_DESCRIPTION, ACCT_TYPE, DESC_2, 
           .keep_all = TRUE)


### comparing with bigger bank list

which(EACOPs_16_Mar_to_04_May_2022$ACCOUNT_NUMBER %in% 
        centenary_core_needed$ACCOUNT_NUMBER)

which(EACOPs_16_Mar_to_04_May_2022$CUST_ID %in% 
        centenary_core_needed$CUST_ID)

which(EACOPs_16_Mar_to_04_May_2022$DESC_2 %in% 
        centenary_core_needed$DESC_2)



length(which(EACOPs_16_Mar_to_04_May_2022$ACCOUNT_NUMBER %in% 
        centenary_core_needed$ACCOUNT_NUMBER))

length(which(EACOPs_16_Mar_to_04_May_2022$CUST_ID %in% 
        centenary_core_needed$CUST_ID))

length(which(EACOPs_16_Mar_to_04_May_2022$DESC_2 %in% 
        centenary_core_needed$DESC_2))


### comparing with smaller bank list

which(EACOPs_16_Mar_to_04_May_2022$ACCOUNT_NUMBER %in% 
        EACOPs_26_Jan_to_15_Mar_2022$ACCOUNT_NUMBER)

which(EACOPs_16_Mar_to_04_May_2022$CUST_ID %in% 
        EACOPs_26_Jan_to_15_Mar_2022$CUST_ID)

which(EACOPs_16_Mar_to_04_May_2022$DESC_2 %in% 
        EACOPs_26_Jan_to_15_Mar_2022$DESC_2)




IWA_Gomba = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                       sheet = "Gomba")


IWA_Kyankwanzi = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                            sheet = "Kyankwanzi")


IWA_Kakumiro = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                          sheet = "Kakumiro")


IWA_Lwengo = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                        sheet = "Lwengo")


IWA_Sembabule = read_excel("Interests_Without_Accounts_Combined.xlsx", 
                           sheet = "Sembabule")


colnames(IWA_Kyankwanzi)[which(colnames(IWA_Kyankwanzi) == "VILLAGE.x")] =
  "VILLAGE"


yy = colnames(IWA_Gomba)[
  which( (colnames(IWA_Gomba) %in% colnames(IWA_Kakumiro)) &
           
           (colnames(IWA_Gomba) %in% colnames(IWA_Kyankwanzi)) &
           
           (colnames(IWA_Gomba) %in% colnames(IWA_Lwengo)) &
           
           (colnames(IWA_Gomba) %in% colnames(IWA_Sembabule)) )]


IWA_Combined = rbind(IWA_Gomba[,yy],
                     IWA_Kakumiro[,yy],
                     IWA_Kyankwanzi[,yy],
                     IWA_Lwengo[,yy],
                     IWA_Sembabule[,yy])  

colnames(IWA_Kyankwanzi)[which(colnames(IWA_Kyankwanzi) == "VILLAGE.x")] =
  "VILLAGE"

# repeat refresh

IWA_Combined = IWA_Combined[,1:which(colnames(IWA_Combined)=="COMMENTS:")]


which(EACOPs_16_Mar_to_04_May_2022$ACCOUNT_NUMBER %in% 
        IWA_Combined$`centenary-bankaccount`)

which(EACOPs_16_Mar_to_04_May_2022$ID_NO %in% IWA_Combined$`ID Number`)

EACOPs_16_Mar_to_04_May_2022$ACCOUNT_DESCRIPTION[
  which(EACOPs_16_Mar_to_04_May_2022$ID_NO %in% IWA_Combined$`ID Number`)]


All_IWA_inner = sqldf("select *
                      from IWA_Combined a
                      inner join EACOPs_16_Mar_to_04_May_2022 b
                      on a.PAP_NAME like '%'||b.DESC_2||'%'
                      WHERE 1;", 
                      method = "raw")

colnames(All_IWA_inner)

## With Duplicates Removed
All_IWA_inner = All_IWA_inner %>%
  distinct(PAP_VALUATION_ASSESSMENT_REF, PAP_NAME, `PAP REF NO`, 
           DISTRICT.x, SUBCOUNTY, VILLAGE, 
           CUST_ID, ACCOUNT_NUMBER,
           .keep_all = TRUE)


check1 = All_IWA_inner[,c("PAP_NAME","DESC_2",
                          "DISTRICT.x","SUBCOUNTY","VILLAGE","ADDRESS_1",
                          "ID Number","ID_NO",
                          "centenary-bankaccount","ACCOUNT_NUMBER", "CUST_ID",
                          "PAP REF NO", "PAP_VALUATION_ASSESSMENT_REF")]

unique(All_IWA_inner$PAP_NAME)

unique(EACOPs_16_Mar_to_04_May_2022$DESC_2)

setdiff(unique(EACOPs_16_Mar_to_04_May_2022$DESC_2), 
        unique(All_IWA_inner$DESC_2))


write.csv(All_IWA_inner, 
          file="All_IWA_inner_May_18.csv", row.names = FALSE)


