### ORIGINAL FILE, NEEDS TO BE MODIFIED

getMatches = function(excerpt, regex) {
  matches <- gregexpr(pattern = regex, text = excerpt, perl=TRUE, ignore.case=TRUE)
  if(matches[[1]] != -1){
    return (length(matches[[1]]))
  }else{
    return (0);
  }
}

grepMatcher = function(excerpt, matches, excludes, codeID){
  grepResults = list(total = 0, codeID = codeID, matches = 0, excludes = 0)
  matchesResults = list()
  excludesResults = list()
  numExcludes <- 0
  for(n in 1:length(excludes)){
    if(length(excludes) != 0){
      count <- getMatches(excerpt, excludes[[n]][[2]])
      numExcludes <- numExcludes + count
      if(count > 0){
        excludesResults[[length(excludesResults) + 1]] = list(count = count, regex = excludes[[n]][[1]])
      }
    }
  }
  numMatches <- 0
  for(n in 1:length(matches)){
    if(length(matches) != 0){
      count <- getMatches(excerpt, matches[[n]][[2]])
      numMatches <- numMatches + count
      if(count > 0){
        matchesResults[[length(matchesResults) + 1]] <- list(count = count, regex = matches[[n]][[1]])
      }
    }
  }
  if(numExcludes > 0){
    grepResults[["total"]] <- -1
  }else{
    grepResults[["total"]] <- numMatches
  }
  grepResults[["matches"]] <- matchesResults
  grepResults[["excludes"]] <- excludesResults
  return (grepResults)
}

# MAKE SO LISTID IS A LIST() OF IDS AND A CROSSCODE VARIABLE AS WELL AND CROSS CODE IF TRUE
processList = function(listID, excerpt, crossCode){
  library('rjson')
  if(crossCode){
    
  }else{
    results <- list()
    for(m in 1:length(codeListContainer)){
      if(codeListContainer[[m]][["ListID"]] == listID){
        if(codeListContainer[[m]][["CodeType"]] == "org.egr.monster.autocoder.coder.GrepCoder"){
          for(n in 1:length(codeListContainer[[m]][["Codes"]])){
            holder = grepMatcher(excerpt, codeListContainer[[m]][["Codes"]][[n]][["Matches"]], codeListContainer[[m]][["Codes"]][[n]][["Excludes"]], codeListContainer[[m]][["Codes"]][[n]][["codeID"]])
            results[[length(results) + 1]] <- holder
          }
        }else if(codeListContainer[[m]][["CodeType"]] == "org.egr.monster.autocoder.coder.SkillsCoder"){
          nouns = 0
          skills = 0
          adj = 0
          persons = 0
          punct = 0
          for(n in 1:length(codeListContainer[[m]][["Codes"]])){
            if(codeListContainer[[m]][["Codes"]][[n]][["codeID"]] == "nouns"){
              nouns = n
            }else if(codeListContainer[[m]][["Codes"]][[n]][["codeID"]] == "skills"){
              skills = n
            }else if(codeListContainer[[m]][["Codes"]][[n]][["codeID"]] == "persons"){
              pesrons = n
            }else if(codeListContainer[[m]][["Codes"]][[n]][["codeID"]] == "punct"){
              punct = n
            }else if(codeListContainer[[m]][["Codes"]][[n]][["codeID"]] == "adj"){
              adj = n
            }
          }
          if(nouns != 0 && skills != 0 && adj != 0 && persons != 0 && punct != 0){
            results = skill.code(excerpt, codeListContainer[[m]][["Codes"]][[skills]][["Matches"]],codeListContainer[[m]][["Codes"]][[nouns]][["Matches"]],codeListContainer[[m]][["Codes"]][[persons]][["Matches"]],codeListContainer[[m]][["Codes"]][[punct]][["Matches"]],codeListContainer[[m]][["Codes"]][[adh]][["Matches"]], "notraw")
            print(results)
          }else{
            print("Proper codes not imported")
          }
        }
      }
    }
    return (toJSON(results))
  }
}

#isCustom just means that matches and excludes are already in their list
#rather than needing to be transfered into their list form from stirngs
importCode = function(listID, codeID, matches, excludes, codeType, isCustom){
  if(!isCustom){
    matches = eval(parse(text=matches))
    excludes = eval(parse(text=excludes))
  }
  code = list(codeID = codeID, Matches = matches, Excludes = excludes)
  assigned = 0
  if(exists("codeListContainer")){
    for(n in 1:length(codeListContainer)){
      if(codeListContainer[[n]][["ListID"]] == listID){
        for(o in 1: length(codeListContainer[[n]][["Codes"]])){
          if(codeListContainer[[n]][["Codes"]][[o]][["codeID"]] == codeID){
            #add to the specific code
            if(length(matches) != 0){
              for(p in 1:length(matches)){
                codeListContainer[[n]][["Codes"]][[o]][["Matches"]][[length(codeListContainer[[n]][["Codes"]][[o]][["Matches"]]) + 1]] <- matches[[p]]
              }
            }
            if(length(excludes) != 0){
              for(p in 1:length(excludes)){
                codeListContainer[[n]][["Codes"]][[o]][["Excludes"]][[length(codeListContainer[[n]][["Codes"]][[o]][["Excludes"]]) + 1]] <- excludes[[p]]
              }
            }
            assigned = 1
          }
        }
        if(assigned == 0){
          # if list matches but the not a code that exists
          codeListContainer[[n]][["Codes"]][[length(codeListContainer[[n]][["Codes"]]) + 1]] <- code
          assigned = 1
        }
      }
    }
    if(assigned == 0){
      # if list does not exist
      listShell = list(ListID = listID, CodeType = codeType, Codes = list(code))
      codeListContainer[[length(codeListContainer) + 1]] <- listShell
    }
  }else{
    # if codeListContainer does not exist
    listShell = list(ListID = listID, CodeType = codeType, Codes = list(code))
    shell = list(listShell)
    assign("codeListContainer", shell, envir = .GlobalEnv)
  }
  assign("codeListContainer",codeListContainer, envir = .GlobalEnv)
}

resetCodeContainer = function(){
  if(exists("codeListContainer")){
    rm("codeListContainer", envir = .GlobalEnv)
  }
}

#file name is name of the csv file eg. 'codeList-1.csv'
#all codes are by default imported into codeList 1 with
#arbitrary codeIDs and grepIDs.
#Codes start at 1 and count up for every code and greps
#start at 1 and count up with every grep 
#NOTE: greps do not restart for each code
importCSV = function(fileName, listID){
  importHolder = read.csv(fileName, header = FALSE, stringsAsFactors = FALSE)
  grepCounter = 1
  for(m in 1:length(importHolder)){
    matches = list()
    excludes = list()
    for(n in 3: length(importHolder[[m]])){
      if(importHolder[[m]][[n]] != ""){
        if(importHolder[[m]][[2]] == "matches"){
          matches[[(length(matches) + 1)]] <- c(toString(grepCounter), importHolder[[m]][[n]])
          grepCounter = grepCounter + 1
        }else if(importHolder[[m]][[2]] == "excludes"){
          excludes[[(length(excludes) + 1)]] <- c(toString(grepCounter), importHolder[[m]][[n]])
          grepCounter = grepCounter + 1
        }
        
      }
    }
    importCode(toString(listID), toString(importHolder[[m]][[1]]), matches, excludes, "org.egr.monster.autocoder.coder.GrepCoder", TRUE)
  }
}

skill.code=function(x,skills,nouns,persons,punct,adj,raw="notraw"){
  y=skill.markup(x,skills,nouns,persons,punct,adj)
  
  #print("skills marked up")
  len=1:length(y)
  out=vector("numeric",length(y))
  
  z=strsplit(y," ")
  
  #print("coding skills")
  for (p in len){
    #if(p/100 == as.integer(p/100)) print(p)
    q=z[[p]]
    
    l2=1:length(q)
    
    out[p]=0
    s=1
    
    for (r in l2){   
      t=q[r]
      
      if (t=="XXpunctXX") {
        s=1
      } else {
        if (t=="XXadjXX") { 
          s=0
        } else {
          if (t=="XXpersonXX") {
            s=1
          } else {
            if (t=="XXnounXX") {
              s=0
            } else {
              if (t=="XXverbXX") {
                out[p]=out[p]+s
                s=1
              }
            }
          }
        }
      }        
    }     
  }
  
  if (raw=="raw") {return (y)}
  else {return(out)} 
}

skill.markup=function(set,verb,noun,person,punct,adj) {    
  ss=" xsepepistsepx "    #arbitrary seperator
  
  ret=paste(" ",set," ",collapse=ss)
  
  #ret=rsub(adj[,1],"XXadjXX",ret)
  #print("adjectives marked")
  ret=rsub(person[,2],"XXpersonXX",ret)
  #print("persons marked")
  
  ret=rsub(noun[,2],"XXnounXX",ret)
  #print("nouns marked")
  ret=rsub(verb[,2],"XXverbXX",ret)
  #print("verbs marked")
  
  ret=rsub(punct[,2]," XXpunctXX ",ret)
  #print("punctuation marked")
  ret=unlist(strsplit(ret,ss))
  
  return(ret)    
}

rsub=function(pattern,replace,set) {
  pats=1:length(pattern)
  
  for (p in pats){
    #if(p/100==as.integer(p/100)) print(p)
    set=gsub(pattern[p],replace,set,ignore.case=TRUE,perl=TRUE)
  }  
  
  return(set)    
}