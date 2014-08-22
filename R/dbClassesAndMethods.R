#############
# Classes and methods
#############

#' @title Database class
#' @aliases MirnaDb columns keys keytypes select
#' @description object of \code{MirnaDb} class holds the sqlite database 
#' connection, and extends \code{AnnotationDb} class from AnnotationDbi 
#' package. \code{columns}, \code{keys}, \code{keytypes} and \code{select} 
#' methods allow access to database tables and retrieval of miRNA target 
#' information.
#'
#' \code{select} is the most important method, allows querying the 
#' database for predictions from a specific source and species for a 
#' given miRNA
#' @usage columns(x)
#' keytypes(x)
#' keys(x, keytype, ...)
#' select(x, keys, columns, keytype, ...)
#' @param x the \code{MirnaDb} object
#' @param keytype the keytype that matches the keys used; the table in which 
#' the search should be performed.
#' @param keys the key to select records for from the database - miRNA name; 
#' all possible keys (miRNAs) are returned by using the \code{keys} method.
#' @param columns in this case same as \code{keytype}, the table in which the 
#' search should be performed, this value specifies the source of predictions 
#' as well as species; as with \code{keys}, all possible columns are returned 
#' by using the \code{columns} method.
#' @param ... any optional arguments
#' @return string vectors, for \code{select} a data.frame with target 
#' genes and scores
#' @author Maciej Pajak \email{m.pajak@@sms.ed.ac.uk}
#' @exportClass MirnaDb
#' @examples 
#' #first load the annotations
#' require(miRNAtap.db)
#' #see all available tables
#' keytypes(miRNAtap.db)
.MirnaDb <- setRefClass('MirnaDb', contains='AnnotationDb')



.getLCcolnames <- function(x) {
    con <- AnnotationDbi:::dbConn(x)
    list <- dbListTables(con)
    list <- list[!list %in% c('metadata')]
    return(list)
}


.cols <- function(x) {
    list <- .getLCcolnames(x)
    return(toupper(list))
}

.getTableNames <- function(x) {
    LC <- .getLCcolnames(x)
    UC <- .cols(x)
    names(UC) <- LC
    return(UC)
}


.keys <- function(x, keytype) {
    tabNames <- .getTableNames(x)
    lckeytype <- names(tabNames[tabNames %in% keytype])
    
    con <- AnnotationDbi:::dbConn(x)
    
    if (keytype %in% c('HOMOLOGENE_RAW')) {
        sql <- paste("SELECT DISTINCT entrez FROM ", lckeytype , sep='')
    } else {
        sql <- paste("SELECT DISTINCT mirna FROM ", lckeytype , sep='')
    }
    res <- dbGetQuery(con, sql)
    res <- as.vector(t(res))
    
    return(res)
}

.select <- function(x, keys, columns, keytype) {
    
    tabNames <- .getTableNames(x)
    lckeytype <- names(tabNames[tabNames %in% keytype])
    
    con <- AnnotationDbi:::dbConn(x)
    
    if (keytype %in% c('HOMOLOGENE_RAW')) {
        one_string <- paste('"', paste(keys,collapse='","'), '"', sep='')
        sql <- paste('SELECT * FROM ', lckeytype, 
                ' WHERE entrez IN (',one_string,')', sep='')
    } else {
        if (length(keys)>1) {
            keys <- keys[1]
        } #only searching by single mirna allowed
        sql <- paste('SELECT * FROM ', lckeytype, 
                ' WHERE mirna LIKE "%', keys, '%"', sep='')
    }
    res <- dbGetQuery(con, sql)
    return(res)        
}


#' @rdname MirnaDb-class
#' @exportMethod columns
setMethod('columns','MirnaDb', function(x) .cols(x))

#' @rdname MirnaDb-class
#' @exportMethod keytypes
setMethod('keytypes','MirnaDb', function(x) .cols(x))

#' @rdname MirnaDb-class
#' @exportMethod keys
setMethod('keys', 'MirnaDb', function(x, keytype, ...) .keys(x,keytype))

#' @rdname MirnaDb-class
#' @exportMethod select
setMethod('select', 'MirnaDb', 
    function(x, keys, columns, keytype, ...) .select(x,keys, columns, keytype))


