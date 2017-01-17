library(RForcecom)
library(dplyr)
library(purrr)

project <- 'kahuna-bq-access'
csm_list <- c("005j000000C9N4HAAV" = "Mila", "005j0000000EEdQAAW" = "Katrina", "005j000000Co3hyAAB" = "Christina", "005j000000DSsIqAAL" = "Thomas")



sf_username <- "blake@kahuna.com"
sf_password <- "Simd838483843WrBRaYQdTBKTnf0PbCNhrML"
sf_url <- "https://kahuna.my.salesforce.com/"
sf_api <- "34.0"
sf_session <- rforcecom.login(sf_username,sf_password,sf_url,sf_api)

# we are going to pull in ex-customers because we want to include their message stats in previous quarters
tmp <- rforcecom.query(sf_session, "SELECT Namespace__c, Account.Name, Account.Industry, Account.World_Sales_Region__c, Owner__c, Status FROM Asset WHERE (Account.Type = 'Customer' OR Account.Type = 'Ex-Customer') OR (Status = 'Success' or Status = 'Onboarding' or Status = 'Integration complete' or Status = 'Integration Deferred' or Status = 'Integration' or Status = 'Pre-Integration')")
tmp <- splitstackshape::cSplit(tmp, "Namespace__c", ",", direction = "long")

tmp$Namespace__c <- sapply(tmp$Namespace__c, tolower)
tmp$Owner__c <- csm_list[tmp$Owner__c]
names(tmp) <- c("Namespace", "Owner", "Status", "Account", "Industry", "Region")
tmp[] <- lapply(tmp, as.character)


#store data
write.csv(tmp, '/home/rstudio/scripts/benchmarks/output/salesforce_data.csv', row.names = FALSE)
