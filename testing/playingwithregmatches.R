areacodes <-read.csv("https://raw.githubusercontent.com/ravisorg/Area-Code-Geolocation-Database/master/us-area-code-cities.csv", header=F)
test <- head(areacodes, 10)
test <- as.character(test$V2)

test

results <- data.frame(text = test) 
results$matches <-  regmatches(test , m = gregexpr(test, pattern = "e" ))
results <- as.data.frame(results)
results %>% mutate(length = unlist(matches))
class(results$text)

length(regmatches(test , m = regexpr(test, pattern = "e")))
length(regmatches(test , m = gregexpr(test, pattern = "e")))
class(regmatches(test , m = gregexpr(test, pattern = "e")))
test

foo <- c("RddzAlejandra: RT @NiallOfficial: What a day for @johnJoeNevin ! Sooo proud t have been there to see him at #London2012 and here in mgar #MullingarShuffle","BPOInsight: RT @atos: Atos completes delivery of key IT systems for London 2012 Olympic Games http://t.co/Modkyo2R #london2012","BloombergWest: The #Olympics sets a ratings record for #NBC, with 219M viewers tuning in. http://t.co/scGzIXBp #london2012 #tech")

ms <- regmatches(foo, gregexpr("#(\\d|\\w)+", foo))  # extract hashtags from tweet (from other post)

foo$foo <- foo
foo$ms <- ms

foo <- data.frame(text = foo)
foo$ms <- ms
foo

class(foo$ms)
as.data.frame(foo)

library(arules)
im <- as(ms, "itemMatrix")

#you can retrieve the rows like this
as(im,"matrix")


for (i in 1:length((test))) {
  match[i] <- regexpr(text = test[i], pattern = "e")
}


# NOT RUN {
x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
pattern <- "[[:space:]]*(,|and)[[:space:]]"
## Match data from regexpr()
m <- regexpr(pattern, x)
regmatches(x, m)
regmatches(x, m, invert = TRUE)
## Match data from gregexpr()
m <- gregexpr(pattern, x)
regmatches(x, m)
regmatches(x, m, invert = TRUE)

## Consider
x <- "John (fishing, hunting), Paul (hiking, biking)"
## Suppose we want to split at the comma (plus spaces) between the
## persons, but not at the commas in the parenthesized hobby lists.
## One idea is to "blank out" the parenthesized parts to match the
## parts to be used for splitting, and extract the persons as the
## non-matched parts.
## First, match the parenthesized hobby lists.
m <- gregexpr("\\([^)]*\\)", x)
## Create blank strings with given numbers of characters.
blanks <- function(n) strrep(" ", n)
## Create a copy of x with the parenthesized parts blanked out.
s <- x
regmatches(s, m) <- Map(blanks, lapply(regmatches(s, m), nchar))
s
## Compute the positions of the split matches (note that we cannot call
## strsplit() on x with match data from s).
m <- gregexpr(", *", s)
## And finally extract the non-matched parts.
regmatches(x, m, invert = TRUE)
# }
