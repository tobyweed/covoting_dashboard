library(dplyr)
library(DescTools)


# make a table with rows=voters and cols=proposals, with cell values=how they voted.
getVotTab <- function(votes) {
  proposals <- unique(votes$Proposal.ID)
  voters <- unique(votes$Voter.ID)
  vot_tab <- data.frame(matrix(nrow = length(voters), ncol= length(proposals)))
  colnames(vot_tab) <- proposals
  rownames(vot_tab) <- voters
  
  votes_remaining <- votes
  for(i in 1:ncol(vot_tab)) {
    prop_votes <- filter(votes_remaining, Proposal.ID == proposals[i]) # the votes cast on this proposal
    for(j in 1:nrow(prop_votes)) {
      v <- prop_votes[j,]
      vot_tab[toString(v$Voter.ID), toString(v$Proposal.ID)] <- v$Choice
    }
    votes_remaining <- anti_join(votes_remaining, prop_votes)
  }
  
  vot_tab
}

## participation

```{r}
getParticipation <- function(vot_tab) {
  na_counts <- rowSums(is.na(vot_tab))
  # table(na_counts)
  sum(na_counts)/(nrow(vot_tab)*ncol(vot_tab)) # portion of missing values
}
```

## mean gini coeff of token dist

```{r}
token_dists <- list()

for(p in 1:ncol(vot_tab)) {
  prop_votes <- filter(votes, Proposal.ID == proposals[p])
  token_dists[[p]] <- prop_votes$Weight
}

# barplot(sort(token_dists[[4]]))
ginis <- lapply(token_dists, gini)
mean(unlist(ginis))
```

```{r gini, include=FALSE}
# compute the gini coefficient of a given distribution
gini <- function(dist) {
  
  # area between Lorenz curve and line of equality
  num <- 0
  for(x in dist) {
    for(y in dist) {
      num <- num + abs(x - y)
    }
  }
  
  # area below line of equality
  denom <- 2*(length(dist))^2*mean(dist)
  
  num/denom
}
```
