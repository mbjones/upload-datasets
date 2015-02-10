# Archive geospatial files from a directory
#
# Iterate over the subdirectories, each of which should contain one or more subdirectories
# that contain a data layer in a directory with its ancillary files.  Each top-level directory
# will be saved as a data package, and will contain a data entity for each of the second-level
# subdirectores, which will be uploaded as zip archvies.  In each top-level directory, we will
# search for a text file with metadata (title, creator(s), spatial extent, temporal extent,
# abstract, keywords, license).
#
# Matt Jones 2015-02-09

library(dataone)
library(uuid)
library(XML)
library(EML)
library(digest)

archive <- function(d) {
    savewd <- getwd()
    print(paste("Processing ", d))
    setwd(d)

    # List all of the directories, each should represent one data object
    do_list <- list.dirs(".", full.names=FALSE, recursive=FALSE)
    for (do in do_list) {
        # zip up the directory
        zipfile <- paste0(do, ".zip")
        zip(zipfile, do)

        # upload data zip to the KNB
        identifier <- upload_object(zipfile, "application/zip", assignDOI=FALSE)
        # TODO: add identifier to list of uploaded objects
        print(paste("Uploaded: ", identifier))

        # clean up
        unlink(zipfile)
    }

    # create metadata for the directory
    mdfile <- "metadata.R"
    success <- source(mdfile, local=TRUE)


    # upload metadata with DOI
    # create and upload resource map

    # Revert back to our calling directory
    setwd(savewd)
}

upload_object <- function(filename, format, assignDOI=FALSE, public=TRUE) {
    cn <- CNode("STAGING2")
    mn <- getMNode(cn, "urn:node:mnTestKNB")

    # Generate a unique identifier for the object
    if (assignDOI) {
        newid <- generateIdentifier(mn, "DOI")
        # TODO: check if we actually got one, if not then error
    } else {
        newid <- paste0("urn:uuid:", UUIDgenerate())
    }

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
    if (!grepl(newid, xmlValue(xmlRoot(created_id)))) {
        # TODO: Process the error
    } else {
        return(newid)
    }
}

main <- function() {
    setwd("/Users/jones/datasets")
    ds <- list.dirs(".", full.names=FALSE, recursive=FALSE)
    for(d in ds) {
        archive(d)
    }
}
