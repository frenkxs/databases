library(xml2)
library(XML)
# need to consolidate XML and xml2 packages....
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

# get pmids of articles that the NAR articles cite
get_references <- function(pmid) {
  # query url
  query_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id="
  query <- paste(query_url, pmid, sep = "")
  
  # get xml file and extract the pmids 
  temp <- getURL(query) %>% XML::xmlParse(asText = TRUE)
  pmids <- XML::xpathSApply(temp,'//Id', xmlValue)[-1] 
  
  # TO DO: need to map xpathSApply and xmlParse to functions in xml2 !!
  # https://gist.github.com/nuest/3ed3b0057713eb4f4d75d11bb62f2d66
  
  # add to the edge list
  paper <- rep(pmid, length(pmids))
  return(data.frame(paper = as.numeric(paper), 
                    cites = as.numeric(pmids)))
}

# get an edge list of papers and their references
edge_list <- lapply(pmids, get_references) %>% 
  bind_rows 

# check if there are no selflinks
sum(edge_list$paper == edge_list$cites)

sort(table(edge_list$cites), decreasing = TRUE)[1:20]

require(igraph)

net <- graph_from_data_frame(edge_list, directed = TRUE)
hist(log(degree(net, mode = 'all')))

# plot graph with only vertices with degree greater than 1
net2 <- delete.vertices(net, degree(net) < 2)
net2 <- delete.vertices(net, degree(net2) < 1)

igraph.options(vertex.size = degree(net2, mode = 'in') * 0.4, 
               edge.color = "grey50", 
               edge.size = 0.1,
               vertex.color = 'black',
               edge.curved = 0.3,
               vertex.label = NA,
               edge.arrow.size = 0.09,
               asp = 0)

layout <- layout_with_dh(net2)
layout <- ayout_with_gem(net2)	
layout <- layout_with_graphopt(net2)
layout <- layout_with_kk(net2)
layout <- layout_with_lgl(net2)
layout <- layout_with_mds(net2)
layout <- layout_with_sugiyama(net2)

plot(net2, layout = layout)


# 10 most cited papers
top <- sort(degree(net, mode = 'in'), decreasing = TRUE)[1:20] %>% 
  names


query_top <- paste(c(top, '[UID]'), collapse = " ")
res_top <- pmQueryTotalCount(query = query_top)

top_papers <- pmApiRequest(query = query_top, 
                           limit = res_top$total_count, 
                           api_key = NULL) %>% 
  convert2df(dbsource = "pubmed", format = "api")
