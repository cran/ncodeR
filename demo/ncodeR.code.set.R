# Load some data
data(RS.data)
rs = RS.data

###
# Create the Data code
###
code.data = create.code(name = "Data", expressions = c("number","data"), excerpts = rs$text)

# Handcode 30 excerpts for Data code
code.data = handcode(code = code.data, n=5)

# Run test to see rho/kappa of current test set
code.data = test(code = code.data, kappaThreshold = 0.65)

# View the summary, with the calcuated statistics
summary(code.data)



# Create the People code
code.people = create.code(name = "People", expressions = c("people","he", "she", "they"), excerpts = rs$text)

# Handcode 30 excerpts for People code
code.people = handcode(code = code.people, n=5)

# Run test
code.people = test(code = code.people, kappaThreshold = 0.65)

summary(code.people)

###
# Generate a CodeSet for all Codes
###
code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", codes = c(code.data, code.people))

# Autocode the full set of excerpts, returning a data.frame
allcoded = autocode(x = code.set)

# Autocode, returning the Code.Set with codes containing updated $computerSets
allcoded = autocode(x = code.set, simplify = F)

# Convert the CodeSet directly to a data.frame using each Codes $computerSet
allcoded.data = as.data.frame(allcoded)

