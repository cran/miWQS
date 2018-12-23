# #' @inherit Hmisc::format.pval
# #' @family format
# #' @keywords format
# #'
# #' @note See \code{\link[Hmisc]{format.pval}}. Thank you Frank Harell Jr for this code. I copied
# #'     \code{format.pval} to \pkg{makeJournalTables} as P-values are also used in comparing
# #'     demographic characteristics between two groups. \code{format.pval} also included for
# #'     convenience in selecting the other format... functions in \pkg{makeJournalTables}.
# #'
# #' @importFrom Hmisc format.pval

format.pval <- Hmisc::format.pval
