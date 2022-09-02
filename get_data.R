# Acquire Snapshot data from the Snapshot API, put in desired form, and write to CSV
# Toby Weed - 9/2/22

library(dplyr)

votes_list <- getVotesFromSpace("poh.eth", 1000, 100000)
votes_df <- votesListToDF(votes_list)
votes_df <- filterVotes(votes_df, 0.99, 20)
write.csv(votes_df, "poh_votes.csv", row.names = FALSE)

# filter out votes from all but nprops of the most recent proposals that have less than agree_thresh percentage consensus
# Also adds features Tot.Votes, N.Votes, Prop.Votes, Max.Prop
# params:
#   - votes_df
#   - agree_thresh: threshold proportion of intraproposal vote agreement to filter out, e.g. 0.99
#   - nprops: number of proposals to take votes from, after filtering out consensus
filterVotes <- function(votes_df, agree_thresh, nprops) {
  # label votes by the total votes in the prop, number of votes w that choice, and proportion of votes with that choice
  votes_df <- votes_df %>%
    group_by(Proposal.ID) %>%
    mutate(Tot.Votes = sum(Weight)) %>%
    group_by(Proposal.ID, Choice) %>%
    mutate(N.Votes = sum(Weight)) %>%
    mutate(Prop.Votes = N.Votes/Tot.Votes) %>%
    ungroup()
  
  # check maximum proportion and filter out ones with too much agreement
  votes_df <- votes_df %>%
    group_by(Proposal.ID) %>%
    mutate(Max.Prop = max(Prop.Votes)) %>%
    filter(Max.Prop < agree_thresh)
  
  # get 20 most recent proposals' start times
  props_df <- votes_df %>%
    mutate(Time.Started = min(Time.Created)) %>%
    summarise(Time.Started = unique(Time.Started)) %>%
    arrange(-Time.Started) %>%
    head(nprops)
  
  # earliest vote in the 20th-oldest proposal
  min_age <- min(props_df$Time.Started)
  
  # filter out votes that happened earlier than this
  votes_df %>%
    filter(Time.Created >= min_age)
}




# turn a list of votes dfs into a df of votes. 
# Removes all but single-choice votes (i.e. filters out approval votes)
# params:
#   - votes_list: list of dfs of votes
votesListToDF <- function(votes_list) {
  # use only elements with single-choice voting (causes problems with dplyr::bind_row() and don't know how to process later anyways)
  votes_list_singlechoice <- list()
  for(df in votes_list) {
    if(typeof(df$data.votes.choice) == "integer") {
      votes_list_singlechoice[[length(votes_list_singlechoice) + 1]] <- df
    }
  }
  
  # gather the data into one big dataframe
  votes_df <- bind_rows(votes_list_singlechoice)
  
  # flatten problematic nested object cols
  votes_df$Proposal.ID <- votes_df$`data.votes.proposal`$id
  votes_df$Space.ID <- votes_df$`data.votes.space`$id
  votes_df <- votes_df %>% select(-c(data.votes.proposal, data.votes.space))
  
  # rename cols
  votes_df <- votes_df %>%
    rename(ID = data.votes.id,
           Voter.ID = data.votes.voter,
           Weight = data.votes.vp,
           Time.Created = data.votes.created,
           Choice = data.votes.choice)
  
  votes_df
}
