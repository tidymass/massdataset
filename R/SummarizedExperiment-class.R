# counts <- matrix(1:15, 5, 3, dimnames=list(LETTERS[1:5], LETTERS[1:3]))
# 
# dates <- SummarizedExperiment(assays=list(counts=counts),
#                               rowData=DataFrame(month=month.name[1:5], day=1:5))
# dates@elementMetadata
# dates@colData
# counts
