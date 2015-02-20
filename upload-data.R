# Upload geospatial files from a directory to a DataONE node
#
# Iterate over the subdirectories, each of which should contain one or more subdirectories
# that contain a data layer in a directory with its ancillary files.  Each top-level directory
# will be saved as a data package, and will contain a data entity for each of the second-level
# subdirectores, which will be uploaded as zip archvies.  In each top-level directory, we will
# search for a text file with metadata (title, creator(s), spatial extent, temporal extent,
# abstract, keywords, license), and use that to create overall dataset metadata.
#
# Matt Jones 2015-02-09

library(dataone)
library(datapackage)
library(uuid)
library(XML)
library(EML)
library(digest)

upload_datasets <- function(d, assignDOI=FALSE) {
    savewd <- getwd()
    print(paste("Processing ", d))
    setwd(d)

    # Create a DataPackage to track all of the data objects that are created
    dp <- DataPackage()
    format <- "application/zip"
    node <- "urn:node:mnTestKNB"
    cm <- CertificateManager()
    user <- showClientSubject(cm)

    # List all of the directories, each should represent one data object
    do_list <- list.dirs(".", full.names=FALSE, recursive=FALSE)
    for (do in do_list) {
        # zip up the directory
        zipfile <- paste0(do, ".zip")
        zip(zipfile, do)
        fq_zipfile <- normalizePath(zipfile)

        # Generate a unique identifier for the object
        identifier <- paste0("urn:uuid:", UUIDgenerate())

        # upload data zip to the data repository
        identifier <- upload_object(zipfile, identifier, format)

        # Create a DataObject and add it to the DataPackage for tracking
        data_object <- new("DataObject", id=identifier, format=format, user=user, mnNodeId=node, filename=fq_zipfile)
        addData(dp, data_object)

        # TODO: add identifier to list of uploaded objects
        message(paste("Uploaded: ", identifier))

        # clean up
        unlink(zipfile)
    }

    # create metadata for the directory
    mdfile <- "metadata.R"
    success <- source(mdfile, local=FALSE)

    # Generate a unique identifier for the object
    if (assignDOI) {
        metadata_id <- generateIdentifier(mn, "DOI")
        # TODO: check if we actually got one, if not then error
        system <- "doi"
    } else {
        metadata_id <- paste0("urn:uuid:", UUIDgenerate())
        system <- "uuid"
    }

    eml <- make_eml(metadata_id, system, title, creators)
    eml_xml <- as(eml, "XMLInternalElementNode")
    #print(eml_xml)
    eml_file <- tempfile()
    saveXML(eml_xml, file = eml_file)

    # upload metadata to the repository
    return_id <- upload_object(eml_file, metadata_id, "eml://ecoinformatics.org/eml-2.1.1")
    message(paste0("Uploaded metadata with id: ", return_id))

    # create and upload package linking the data files and metadata
    data_id_list <- getIdentifiers(dp)
    mdo <- new("DataObject", id=metadata_id, filename=eml_file, format="eml://ecoinformatics.org/eml-2.1.1", user=user, mnNodeId=node)
    addData(dp, mdo)
    unlink(eml_file)
    insertRelationship(dp, subjectID=metadata_id, objectIDs=data_id_list)
    tf <- tempfile()
    serialization_id <- paste0("urn:uuid:", UUIDgenerate())
    status <- serializePackage(dp, tf, id=serialization_id)
    return_id <- upload_object(tf, serialization_id, "http://www.openarchives.org/ore/terms")
    message(paste0("Uploaded data package with id: ", return_id))
    unlink(tf)

    # Revert back to our calling directory
    setwd(savewd)
}

make_eml <- function(id, system, title, creators) {
    #dt <- eml_dataTable(dat, description=description)
    creator <- new("ListOfcreator", lapply(as.list(with(creators, paste(given, " ", surname, " ", "<", email, ">", sep=""))), as, "creator"))
    ds <- new("dataset",
              title = title,
              creator = creator,
              contact = as(creator[[1]], "contact"))
              #coverage = new("coverage"),
              #dataTable = c(dt),
              #methods = new("methods"))

    eml <- new("eml",
              packageId = id,
              system = system,
              dataset = ds)
    return(eml)
}

upload_object <- function(filename, newid, format, public=TRUE) {
    cn <- CNode("STAGING2")
    mn <- getMNode(cn, "urn:node:mnTestKNB")

    # Ensure the user is logged in before the upload
    cm <- CertificateManager()
    user <- showClientSubject(cm)
    isExpired <- isCertExpired(cm)

    # Create SystemMetadata for the object
    size <- file.info(filename)$size
    sha1 <- digest(filename, algo="sha1", serialize=FALSE, file=TRUE)
    sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, submitter=user, rightsHolder=user, checksum=sha1, originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier)
    if (public) {
        sysmeta <- addAccessRule(sysmeta, "public", "read")
    }

    # Upload the data to the MN using create(), checking for success and a returned identifier
    created_id <- create(mn, newid, filename, sysmeta)
    if (is.null(created_id) | !grepl(newid, xmlValue(xmlRoot(created_id)))) {
        # TODO: Process the error
        message(paste0("Error on returned identifier: ", created_id))
    } else {
        return(newid)
    }
}

main <- function() {
    setwd("/Users/jones/datasets")
    ds <- list.dirs(".", full.names=FALSE, recursive=FALSE)
    for(d in ds) {
        upload_datasets(d)
    }
}
