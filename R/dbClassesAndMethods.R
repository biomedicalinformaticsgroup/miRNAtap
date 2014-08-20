#############
# Classes and methods
#############



#' @exportClass MirnaDb
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



#' @exportMethod columns
setMethod('columns','MirnaDb', function(x) .cols(x))

#' @exportMethod keytypes
setMethod('keytypes','MirnaDb', function(x) .cols(x))

#' @exportMethod keys
setMethod('keys', 'MirnaDb', function(x, keytype) .keys(x,keytype))

#' @exportMethod select
setMethod('select', 'MirnaDb', 
        function(x, keys, columns, keytype) .select(x,keys, columns, keytype))


