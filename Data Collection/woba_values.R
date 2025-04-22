## Liam Jennings
## Sport Management Independent Study


# Getting Started ---------------------------------------------------------

## libraries
library(tidyverse)
library(Lahman)

## read in wOBA constants
woba_constants <- read_csv("wOBA_constants.csv")

## data
### filter for players who debuted after 2000
people_2000 <- People |> 
  mutate(
    # convert debute to a date
    debut_date = as.Date(debut)
  ) |> 
  # filter for players after 2000
  filter(
    debut_date >= "2000-01-01"
  )


### find positions
positions <- Fielding |> 
  # group by player and positions
  group_by(
    playerID, 
    POS
  ) |>
  # summarize number of games
  summarize(
    Games = sum(G)
  ) |> 
  # order by player
  arrange(
    playerID, 
    desc(Games)
  ) |> 
  # most played position
  filter(
    POS == first(POS)
  ) |> 
  # ungroup
  ungroup()

## join with People
batting_2000 <- Batting |>
  # left join with positions
  left_join(positions) |> 
  # filter out pitchers
  filter(
    POS != "P"
  ) |> 
  # replace NAs with 0s
  replace_na(list(SF = 0, HBP = 0)) |> 
  # inner join with People
  inner_join(people_2000) |> 
  # mutate
  mutate(
    # calculate birth year
    birthyear = if_else(
      birthMonth >= 7, birthYear + 1, birthYear
    ),
    
    # calculate age
    age = yearID - birthyear,
    
    # calculate unintentional walks
    uBB = BB - IBB,
    
    # calculate singles
    X1B = H - X2B - X3B - HR
  ) |> 
  # join wOBA constants
  left_join(
    woba_constants,
    # by
    by = c("yearID" = "Season")
  ) |> 
  # calculate wOBA for each season using wOBA constans
  mutate(
    wOBA = (
      # unintentional walks
      (wBB * uBB) + 
        # hit by pitches
        (wHBP * HBP) + 
        # singles
        (w1B * X1B) +   
        # doubles
        (w2B * X2B) +
        # triples
        (w3B * X3B) + 
        # home runs
        (wHR * HR)
    ) / 
      # denominator
      (AB + BB - IBB + SF + HBP),
    
    # name
    name = paste(nameFirst, nameLast)
  ) |> 
  select(
    playerID,
    name, 
    yearID,
    position = "POS",
    teamID,
    lgID,
    G,
    wOBA,
    height,
    weight,
    bats,
    throws,
    age
  )

## calculate wOBA for each player and season and join with other info
batting_woba <- batting_2000 |> 
  # group by player and year
  group_by(
    playerID,
    yearID
  ) |> 
  # summarize
  summarize(
    wOBA = mean(wOBA, na.rm = TRUE),
    name = first(name),
    position = first(position),
    G = sum(G, na.rm = TRUE),
    height = first(height),
    weight = first(weight),
    bats = first(bats),
    throws = first(throws),
    age = first(age)
  ) |> 
  ungroup()


## glimpse
glimpse(woba_constants)


## test
# test <- batting_woba |> filter(wOBA == "NaN")
