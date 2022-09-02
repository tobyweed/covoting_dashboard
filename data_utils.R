# Utils for acquiring Snapshot data from the Snapshot API
# Toby Weed - 9/2/22

library(ghql)
library(jsonlite)

snapshot_endpt <- 'https://hub.snapshot.org/graphql'
conn <- GraphqlClient$new(url = snapshot_endpt)

# returns list of dataframes of votes from different proposals in a Snapshot space
# params:
#   - space: the name of the space, e.g. "poh.eth"
#   - nprops: max number of proposals to take votes from
#   - nvotes: max number of votes to return, total
getVotesFromSpace <- function(space, nprops, nvotes) {
  proposal_df <- getProps(prop_query, nprops, space)
  prop_ids <- proposal_df$data.proposals.id  # get list of proposal IDs
  
  # store all the votes from every proposal in a big list
  votes_list <- list() # DF to store all the votes gathered
  for(i in 1:length(prop_ids)) {
    votes <- getVotes(vote_query, nvotes, prop_ids[i])
    votes_list[[i]] <- votes
  }
  votes_list
}

## --- Query Functions ---

# get the proposals from a Snapshot space
# params:
#   - query: GraphQL query string
#   - nprops: max number of props to return
#   - space: the name of the space to be queried
# returns: DF of proposals
getProps <- function(query, nprops, space) {
  new <- Query$new()$query('q', query) # init query object
  
  params <- list( # init params list
    nprops = nprops,
    space = space
  )
  
  # execute the request & convert from JSON to list
  result <- conn$exec(new$q, variables = params) %>%
    fromJSON(flatten = F)
  
  result_df <- as.data.frame(result) # convert list to DF
  result_df
}


# get the votes for a particular proposal
# params:
#   - query: GraphQL query string
#   - nvotes: max number of votes to return
#   - proposal: the ID/address of the proposal to be gotten. Can be found at the end of the snapshot URL
# returns: DF of votes
getVotes <- function(query, nvotes, proposal) {
  new <- Query$new()$query('q', query) # init query object
  
  params <- list( # init params list
    nvotes = nvotes,
    proposal = proposal
  )
  
  # execute the request & convert from JSON to list
  result <- conn$exec(new$q, variables = params) %>%
    fromJSON(flatten = F)
  
  result_df <- as.data.frame(result) # convert list to DF
  result_df
}



## --- Query Strings ----
# query for finding votes in a proposal
vote_query <- '
query ($nvotes: Int!, $proposal: String!) {
  votes (
    first: $nvotes
    skip: 0
    where: {
      proposal: $proposal
    }
    orderBy: "created",
    orderDirection: desc
  ) {
    id
    voter
    vp
    created
    proposal {
      id
    }
    choice
    space {
      id
    }
  }
}'

# query for finding proposals in a space
prop_query <- 'query ($nprops: Int!, $space: String!)  {
  proposals (
    first: $nprops,
    skip: 0,
    where: {
      space_in: [$space],
      state: "closed"
    },
    orderBy: "created",
    orderDirection: desc
  ) {
    id
    title
    body
    choices
    start
    end
    snapshot
    state
    author
    space {
      id
      name
    }
  }
}'