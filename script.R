library(xml2)
library(pubmedR)
library(bibliometrix)
library(tidyverse)
library(RCurl)


# retrieve all articles from NAR database issue 2020
# https://academic.oup.com/nar/article/48/D1/D1/5695332
query <- "0305-1048[TA] AND D1[IP] AND 48[VI]"
res <- pmQueryTotalCount(query = query)
response <- pmApiRequest(query = query, 
                         limit = res$total_count, 
                         api_key = NULL) %>% 
  convert2df(dbsource = "pubmed", format = "api")

# get pmids
pmids <- response$PMID


edge_list <- data.frame(paper = as.character(), 
                        cites = as.character())

# get pmids of articles that the NAR articles cite
get_references <- function(pmid) {
  # query url
  query_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id="
  query <- paste(query_url, pmid, sep = "")
  
  # get xml file and extract the pmids 
  temp <- getURL(query) %>% xmlParse(asText = TRUE)
  pmids <- xpathSApply(temp,'//Id', xmlValue) 
  
  # add to the edge list
  paper <- rep(pmid, length(pmids))
  return(data.frame(paper = paper, cites = pmids))
}

# get an edge list of papers and their references
edge_list <- lapply(pmids, get_references) %>% bind_rows

edge_list <- apply(edge_list, 2, as.numeric)

require(igraph)

net <- graph_from_adj_list(edge_list, mode = 'in')
plot(net)
