library(RForcecom)
library(dplyr)


# read in latest adoption file for namespaces
# filter to namespaces where status = Success
sf_username <- "blake@kahuna.com"
sf_password <- "Simd838483843WrBRaYQdTBKTnf0PbCNhrML"
sf_url <- "https://kahuna.my.salesforce.com/"
sf_api <- "34.0"
sf_session <- rforcecom.login(sf_username,sf_password,sf_url,sf_api)



# get Salesforce asset ID's
asset_ids <- rforcecom.query(sf_session, "SELECT Namespace__c, Asset_ID_Long__c FROM Asset")
# remove last 3 characters because they aren't part of asset ID
asset_ids$Asset_ID_Long__c <- substr(asset_ids$Asset_ID_Long__c, 1, (length(asset_ids$Asset_ID_Long__c) - 3))

# read in latest Customer360 report
namespaces <- read.csv('/Users/blakeschiafone/Desktop/namespaces.csv', stringsAsFactors = FALSE, header = TRUE)
namespaces$Namespace <- purrr::map(namespaces$Namespace, tolower)

# match asset ID with customer360
namespaces$asset_id <- asset_ids$Asset_ID_Long__c[match(namespaces$Namespace, asset_ids$Namespace__c)]

# filter null asset_id's
namespaces <- namespaces[!is.na(namespaces$asset_id),]


# bulk update records
job_info <- rforcecom.createBulkJob(sf_session, operation = 'update', object = 'Asset')
namespaces <- namespaces %>% select(asset_id, VERTICAL)
names(namespaces) <- c('Id', 'Asset_Vertical__c')

rforcecom.createBulkBatch(sf_session, jobId = job_info$id, data = namespaces)