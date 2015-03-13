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

#' Iterate over the data sets in a directory.
#' This function is passed a directory \code{'d'}, which contains a set of subdirectories,
#' each of which represents a data set. For each subdirectory, zip up the contents, generate
#' a new identifier, upload the zipfile to a repository, and add it to a DataPackage for
#' later inclusion in the dataset. Once all of the datasets are uploaded, source the associated
#' "metadata.R" file in each directory, use this to populate an EML metadata description, and
#' upload the EML and the associated DataPackage to the repository.
upload_datasets <- function(d, mn, assignDOI=FALSE) {
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
        identifier <- upload_object(mn, zipfile, identifier, format)

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

    eml <- make_eml(metadata_id, system, title, creators, methodDescription, geo_coverage, temp_coverage, dp, mn@endpoint)
    eml_xml <- as(eml, "XMLInternalElementNode")
    #print(eml_xml)
    eml_file <- tempfile()
    saveXML(eml_xml, file = eml_file)

    # upload metadata to the repository
    return_id <- upload_object(mn, eml_file, metadata_id, "eml://ecoinformatics.org/eml-2.1.1")
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
    return_id <- upload_object(mn, tf, serialization_id, "http://www.openarchives.org/ore/terms")
    message(paste0("Uploaded data package with id: ", return_id))
    unlink(tf)

    # Revert back to our calling directory
    setwd(savewd)
}

#' Create a geographic coverage element from a description and bounding coordinates
geo_cov <- function(geoDescription, west, east, north, south) {
    bc <- new("boundingCoordinates", westBoundingCoordinate=west, eastBoundingCoordinate=east, northBoundingCoordinate=north, southBoundingCoordinate=south)
    geoDescription="Southeast Alaska"
    gc <- new("geographicCoverage", geographicDescription=geoDescription, boundingCoordinates=bc)
    return(gc)
}

temp_cov <- function(begin, end) {
    bsd <- new("singleDateTime", calendarDate=begin)
    b <- new("beginDate", bsd)
    esd <- new("singleDateTime", calendarDate=end)
    e <- new("endDate", esd)
    rod <- new("rangeOfDates", beginDate=b, endDate=e)
    temp_coverage <- new("temporalCoverage", rangeOfDates=rod)
    return(temp_coverage)
}

cov <- function(gc, tempc) {
    coverage <- new("coverage", geographicCoverage=gc, temporalCoverage=tempc)
    return(coverage)
}

#' Create a minimal EML document.
#' Creating EML should be more complete, but this minimal example will suffice to create a valid document.
make_eml <- function(id, system, title, creators, methodDescription=NA, geo_coverage=NA, temp_coverage=NA, datapackage=NA, endpoint=NA) {
    #dt <- eml_dataTable(dat, description=description)
    oe_list <- as(list(), "ListOfotherEntity")
    if (!is.na(datapackage)) {
        for (id in getIdentifiers(datapackage)) {
            print(paste("Creating entity for ", id, sep=" "))
            current_do <- getMember(datapackage, id)
            oe <- new("otherEntity", entityName=basename(current_do@filename), entityType="application/zip")
            oe@physical@objectName <- basename(current_do@filename)
            oe@physical@size <- current_do@sysmeta@size
            if (!is.na(endpoint)) {
                oe@physical@distribution@online@url <- paste(endpoint, id, sep="/")
            }
            f <- new("externallyDefinedFormat", formatName="ESRI Arc/View ShapeFile")
            df <- new("dataFormat", externallyDefinedFormat=f)
            oe@physical@dataFormat <- df
            oe_list <- c(oe_list, oe)
        }
    }

    creator <- new("ListOfcreator", lapply(as.list(with(creators, paste(given, " ", surname, " ", "<", email, ">", sep=""))), as, "creator"))
    ds <- new("dataset",
              title = title,
              abstract = abstract,
              creator = creator,
              contact = as(creator[[1]], "contact"),
              #coverage = new("coverage"),
              pubDate = as.character(Sys.Date()),
              #dataTable = c(dt),
              otherEntity = as(oe_list, "ListOfotherEntity")
              #methods = new("methods"))
             )

    if (!is.na(methodDescription)) {
        ms <- new("methodStep", description=methodDescription)
        listms <- new("ListOfmethodStep", list(ms))
        ds@methods <- new("methods", methodStep=listms)
    }
    ds@coverage <- cov(geo_coverage, temp_coverage)
    eml <- new("eml",
              packageId = id,
              system = system,
              dataset = ds)
    return(eml)
}

#' Upload an object to a DataONE repository
upload_object <- function(mn, filename, newid, format, public=TRUE, replicate=FALSE) {

    # Ensure the user is logged in before the upload
    cm <- CertificateManager()
    user <- showClientSubject(cm)
    isExpired <- isCertExpired(cm)

    # Create SystemMetadata for the object
    size <- file.info(filename)$size
    sha1 <- digest(filename, algo="sha1", serialize=FALSE, file=TRUE)
    sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, submitter=user, rightsHolder=user, checksum=sha1, originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier)
    sysmeta@replicationAllowed <- replicate
    sysmeta@numberReplicas <- 2
    sysmeta@preferredNodes <- list("urn:node:mnUCSB1", "urn:node:mnUNM1", "urn:node:mnORC1")
    if (public) {
        sysmeta <- addAccessRule(sysmeta, "public", "read")
    }

    # Upload the data to the MN using create(), checking for success and a returned identifier
    created_id <- create(mn, newid, filename, sysmeta)
    if (is.null(created_id) || !grepl(newid, xmlValue(xmlRoot(created_id)))) {
        # TODO: Process the error
        message(paste0("Error on returned identifier: ", created_id))
        return(newid)
    } else {
        return(newid)
    }
}

#' main method to iterate across directories, uploading each data set
main <- function() {
    cn <- CNode("STAGING2")                     # Use Testing repository
    mn <- getMNode(cn, "urn:node:mnTestKNB")    # Use Testing repository
    #cn <- CNode()                               # Use Production repository
    #mn <- getMNode(cn, "urn:node:KNB")          # Use Production repository
    setwd("datasets")
    ds <- list.dirs(".", full.names=FALSE, recursive=FALSE)
    for(d in ds) {
        upload_datasets(d, mn, assignDOI=FALSE)
    }
    setwd("..")
}
