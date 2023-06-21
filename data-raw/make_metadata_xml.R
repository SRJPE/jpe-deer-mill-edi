library(EMLaide)
library(tidyverse)
library(readxl)
library(EML)

datatable_metadata <-
  dplyr::tibble(filepath = c("data/deer_mill_catch_edi.csv",
                             "data/deer_mill_recapture_edi.csv",
                             "data/deer_mill_release_edi.csv",
                             "data/deer_mill_trap_edi.csv"),
                attribute_info = c("data-raw/metadata/deer_mill_catch_metadata.xlsx",
                                   "data-raw/metadata/deer_mill_recapture_metadata.xlsx",
                                   "data-raw/metadata/deer_mill_release_metadata.xlsx",
                                   "data-raw/metadata/deer_mill_trap_metadata.xlsx"),
                datatable_description = c("Daily catch",
                                          "Recaptured catch",
                                          "Release trial summary",
                                          "Daily trap operations"),
                datatable_url = paste0("https://raw.githubusercontent.com/SRJPE/jpe-deer-mill-edi/main/data/",
                                       c("deer_mill_catch_edi.csv",
                                         "deer_mill_recaptures_edi.csv",
                                         "deer_mill_releases_edi.csv",
                                         "deer_mill_trap_edi.csv")))
# save cleaned data to `data/`
excel_path <- "data-raw/metadata/deer_mill_metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.docx"
#methods_docx <- "data-raw/metadata/methods.md"

dataset <- list() %>%
  add_pub_date() %>%
  add_title(metadata$title) %>%
  add_personnel(metadata$personnel) %>%
  add_keyword_set(metadata$keyword_set) %>%
  add_abstract(abstract_docx) %>%
  add_license(metadata$license) %>%
  add_method(methods_docx) %>%
  add_maintenance(metadata$maintenance) %>%
  add_project(metadata$funding) %>%
  add_coverage(metadata$coverage, metadata$taxonomic_coverage) %>%
  add_datatable(datatable_metadata)

# GO through and check on all units
custom_units <- data.frame(id = c("NTU", "revolutions per minute", "number of fish"),
                           unitType = c("dimensionless", "dimensionless", "dimensionless"),
                           parentSI = c(NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA),
                           description = c("nephelometric turbidity units, common unit for measuring turbidity",
                                           "number of revolutions per minute",
                                           "number of fish counted"))

unitList <- EML::set_unitList(custom_units)

#edi_number <- reserve_edi_id(user_id = Sys.getenv("EDI_USER_ID"), password = Sys.getenv("EDI_PASSWORD"))
edi_number <- "edi.1445.1"

eml <- list(packageId = edi_number,
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList))
)

EML::write_eml(eml, paste0(edi_number, ".xml"))
EML::eml_validate("edi.1445.1.xml")

# evaluate <- EMLaide::evaluate_edi_package(user_id = Sys.getenv("EDI_USER_ID"),
#                                           password = Sys.getenv("EDI_PASSWORD"),
#                                           eml_file_path = "edi.1239.1.xml",
#                                           environment = "staging")
#
# EMLaide::upload_edi_package(user_id = Sys.getenv("EDI_USER_ID"),
#                             password = Sys.getenv("EDI_PASSWORD"),
#                             eml_file_path = "edi.1239.1.xml",
#                             environment = "staging")
