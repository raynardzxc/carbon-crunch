# Initialise all functions related to communicating with database --------------------
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student099",
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student099",
    password = getOption("AWSPassword"))
  conn
}

# Push player's score into the Leaderboard table
publishScore <- function(playerid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderScore (playerid,asoftime,score) VALUES (?id1,NOW(),?id2)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=score)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

# Retrieve leaderboard from database
getLeaderBoard <- function(){
  conn <- getAWSConnection()
  
  # Assemble the query
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp"
  query <- paste0(query," ON (ls.playerid=lp.playerid) WHERE ls.gamevariantid =")
  query <- paste0(query,gamevariantid)
  
  # Sort in descending order
  query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# Retrieve initial game conditions
getInitialCond <- function(){
  conn <- getAWSConnection()
  query <- "SELECT * FROM InitialCond"
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# Retrieve battery information given current level[int] (can be used to get next level info and cost as well)
getBatteryInfo <- function(){
  conn <- getAWSConnection()
  # Crafting query
  query <- "SELECT * FROM BatteryUpgrade"
  # Retrieve results
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# Retrieve battery information given current level[int] and line type[binary] (can be used to get next level info and cost as well)
getLineInfo <- function(){
  conn <- getAWSConnection()
  # Crafting query
  query <- "SELECT * FROM LineUpgrade"
  # Retrieve results
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}
# ------------------------------------------------------------------------------------