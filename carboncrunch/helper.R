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

## Function for login
#Generate, or re-generate, HTML to create modal dialog for Password creation
registerModal <- function(ns, failed = FALSE) {
  
  modalDialog(
    title = "Create a new password",
    textInput(inputId = ns("playername1"), "Enter your desired username:", placeholder = "Username"),
    passwordInput(inputId = ns("password1"), "Enter a new password:", placeholder = "Password"),
    passwordInput(inputId = ns("password2"), "Confirm by re-entering the new password:", placeholder = "Repeated Password"),
    
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(inputId = ns("registerok"), "OK")
    )
  )
}

loginModal <- function(ns, failed = FALSE) {
  
  modalDialog(
    title = "Login",
    textInput(inputId = ns("playername2"), "Enter your username", placeholder = "Username"),
    passwordInput(inputId = ns("password3"), "Enter your password:", placeholder = "Password"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(inputId = ns("loginok"), "OK")
    )
  )
}

getPlayerID <- function(playername,password){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE playername=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  #print(query) #for debug
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  #print(result)
  #print(playerid)
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}

createNewPlayerQuery <- function(playername,password){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderPlayer (playername,password) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  playername
}

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
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp ON (ls.playerid=lp.playerid)"
  # Sort in descending order
  query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  #print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  colnames(result) <- c("Player", "Score", "Date Achieved")
  result
}