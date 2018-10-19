# Setup ----
library(cbn)
library(reshape2)

# Let R know where the GloVe file is located
cbn_set_vectorfile_location("/Users/dell/Google Drive/Documents/Coding/R-AI/glove.840B.300d.txt", persist = TRUE)

# Replicate WEAT1 csv method ----
# Load your data from csv
custom <- as.data.frame(read.csv("/Users/dell/Desktop/yup.csv"))
custom # check to see if files loaded properly

# playing with the cbn_make_items function
words <- custom$Word
mat <- cbn_extract_word_vectors(custom$Word)
weat_perm(custom, mat, x_name = "Flowers", y_name = "Insects",
          a_name = "Pleasant", b_name = "Unpleasant", 1000)
#        S_xyab          d p_value
# 1 -0.09316188 -0.1579272   0.702
# The numbesr do not look right. I think it is because each column dimension is
# stored as factors

custom$Study <- as.character(custom$Study)
custom$Condition <- as.character(custom$Condition)
custom$Word  <- as.character(custom$Word)
custom$Role <- as.character(custom$Role)
# Here, we are resaving each of the columns as characaters as opposed to factors
# This is how the computer stores information. It can be confusing for
# new coders, but you'll see that this is important once you understand it.
# There are nice tutorials explaining the differences in different types of vectors
# over the internet. Check out this tutorial here LINK HERE
vecs_copycat <- cbn_extract_word_vectors(custom$Word) # because the cbn_get_item_vectors function grabs its$Word
weat_perm(custom, vecs_copycat, x_name = "Flowers", y_name = "Insects",
          a_name = "Pleasant", b_name = "Unpleasant", 1000) # Now, let's test it!

#     S_xyab        d p_value
# 1 2.238165 1.504315       0

# Yell, "Eureka!"



# Now, we manipulate the data to have no effect size
# Same attribute words for both different sets of target words
custom$Word[custom$Condition=="Pleasant"] <- with(custom, Word[Condition=="Unpleasant"])
vecs_copycat <- cbn_extract_word_vectors(custom$Word)
weat_perm(custom, vecs_copycat, x_name = "Flowers", y_name = "Insects",
          a_name = "Pleasant", b_name = "Unpleasant", 1000)
#   S_xyab   d p_value
# 1      0 NaN       0
# I'll have to think about this

# Same target words for both different sets of attributes
custom$Word[custom$Condition=="Flowers"] <- with(custom, Word[Condition=="Insects"])
vecs_copycat <- cbn_extract_word_vectors(custom$Word)
weat_perm(custom, vecs_copycat, x_name = "Flowers", y_name = "Insects",
          a_name = "Pleasant", b_name = "Unpleasant", 1000)
#   S_xyab d p_value
# 1      0 0     0.5



# Replicate WEAT1 R method ----
Flowers <- c("aster", "clover", "hyacinth", "marigold", "poppy", "azalea",
             "crocus", "iris", "orchid", "rose", "bluebell", "daffodil",
             "lilac", "pansy", "tulip", "buttercup", "daisy", "lily",
             "peony", "violet", "carnation", "gladiola", "magnolia",
             "petunia", "zinnia")
Insects <- c("ant", "caterpillar", "flea", "locust", "spider", "bedbug",
             "centipede", "fly", "maggot", "tarantula", "bee", "cockroach",
             "gnat", "mosquito", "termite", "beetle", "cricket", "hornet",
             "moth", "wasp", "blackfly", "dragonfly", "horsefly", "roach",
             "weevil")
Pleasant <- c("caress", "freedom", "health", "love", "peace", "cheer",
              "friend", "heaven", "loyal", "pleasure", "diamond", "gentle",
              "honest", "lucky", "rainbow", "diploma", "gift", "honor",
              "miracle", "sunrise", "family", "happy", "laughter",
              "paradise", "vacation")
Unpleasant <- c("abuse", "crash", "filth", "murder", "sickness",
                "accident", "death", "grief", "poison", "stink", "assault",
                "disaster", "hatred", "pollute", "tragedy", "divorce",
                "jail", "poverty", "ugly", "cancer", "kill", "rotten",
                "vomit", "agony", "prison")

manual <- melt(cbind(Flowers, Insects, Pleasant, Unpleasant),
            value.name="Word",
            varnames=c("", "Condition")
            )[ , -1]
manual['Role'] <- ifelse((manual$Condition=="Flowers" | manual$Condition=="Insects"), # if Condition is Flowers or Insects
                         "target", # then, name it target
                         "attribute") # else, name it attribute
# Convert factors to characters
manual$Condition <- as.character(manual$Condition)
manual$Word  <- as.character(manual$Word)
manual$Role <- as.character(manual$Role)
vecs_copycat <- cbn_extract_word_vectors(manual$Word) # because the cbn_get_item_vectors function grabs its$Word
weat_perm(manual, vecs_copycat, x_name = "Flowers", y_name = "Insects",
          a_name = "Pleasant", b_name = "Unpleasant", 1000) # Now, let's test it!
#     S_xyab        d p_value
# 1 2.238165 1.504315       0



# We know how to replciate using both csv and R now, so let's use our own words ----
# comparing police against insects
police <- c("police", "enforcement", "detective", "agent", "informer", "officer",
             "deputy", "sheriff", "cop", "policeman", "policewoman", "officer",
             "marshal", "trooper", "paratrooper", "agent", "SA", "inspector",
             "investigator", "deputy", "patrolman", "highway", "force",
             "military", "law")
Insects <- c("ant", "caterpillar", "flea", "locust", "spider", "bedbug",
             "centipede", "fly", "maggot", "tarantula", "bee", "cockroach",
             "gnat", "mosquito", "termite", "beetle", "cricket", "hornet",
             "moth", "wasp", "blackfly", "dragonfly", "horsefly", "roach",
             "weevil")
Pleasant <- c("caress", "freedom", "health", "love", "peace", "cheer",
              "friend", "heaven", "loyal", "pleasure", "diamond", "gentle",
              "honest", "lucky", "rainbow", "diploma", "gift", "honor",
              "miracle", "sunrise", "family", "happy", "laughter",
              "paradise", "vacation")
Unpleasant <- c("abuse", "crash", "filth", "murder", "sickness",
                "accident", "death", "grief", "poison", "stink", "assault",
                "disaster", "hatred", "pollute", "tragedy", "divorce",
                "jail", "poverty", "ugly", "cancer", "kill", "rotten",
                "vomit", "agony", "prison")
manual <- melt(cbind(police, Insects, Pleasant, Unpleasant),
               value.name="Word",
               varnames=c("", "Condition")
               )[ , -1]
manual['Role'] <- ifelse((manual$Condition=="police" | manual$Condition=="Insects"), # if Condition is Flowers or Insects
                         "target", # then, name it target
                         "attribute") # else, name it attribute
# Convert factors to characters
manual$Condition <- as.character(manual$Condition)
manual$Word  <- as.character(manual$Word)
manual$Role <- as.character(manual$Role)
vecs_copycat <- cbn_extract_word_vectors(manual$Word) # because the cbn_get_item_vectors function grabs its$Word
weat_perm(manual, vecs_copycat, x_name = "police", y_name = "Insects",
          a_name = "Pleasant", b_name = "Unpleasant", 1000) # Now, let's test it!
# Police and Insects are similar, but they are not statistically significant



# Replicating WEFAT ----
# Same as before to get items and vectors
# We do this process manually because we deleted the pre-installed data.
Careers <- c("technician", "accountant", "supervisor", "engineer",
                    "worker", "educator", "clerk", "counselor", "inspector",
                    "mechanic", "manager", "therapist", "administrator",
                    "salesperson", "receptionist", "librarian", "advisor",
                    "pharmacist", "janitor", "psychologist", "physician",
                    "carpenter", "nurse", "investigator", "bartender",
                    "specialist", "electrician", "officer", "pathologist",
                    "teacher", "lawyer", "planner", "practitioner", "plumber",
                    "instructor", "surgeon", "veterinarian", "paramedic",
                    "examiner", "chemist", "machinist", "appraiser",
                    "nutritionist", "architect", "hairdresser", "baker",
                    "programmer", "paralegal", "hygienist", "scientist")
FemaleAttributes <- c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter")
MaleAttributes <- c("male", "man", "boy", "brother", "he", "him", "his", "son")

its <- melt(cbind(Careers, FemaleAttributes, MaleAttributes),
               value.name="Word",
               varnames=c("", "Condition"))[ , -1]
# WEFAT also has to have a role column because there is a specified role of target or
# attribute depending on the function

# I have to look at the original construction of WEFATS as stored in cbn_items to 
# reverse the original design. 

# Recommendaiton to self:
# 1. Reinstall the cbn package with all of its contents 
# 2. Install the cbn package without pre-installed data and name it as something else


its <- cbn_get_items("WEFAT", 1)
its_vecs <- cbn_get_item_vectors("WEFAT", 1)
# Now to get bootstrapped differences of cosines. Note that there is no y_name this time and we will get a statistic for each x_name.
res <- wefat_boot(its, its_vecs, x_name = "Careers",
                  a_name = "MaleAttributes", b_name = "FemaleAttributes",
                  se.calc = "quantile")
head(res)
#>        Careers       diff        lwr         upr     median
#> 1        nurse -0.1766406 -0.2347793 -0.11659004 -0.1713707
#> 2 receptionist -0.1369632 -0.1873617 -0.08162812 -0.1331052
#> 3    hygienist -0.1182356 -0.1514190 -0.07916550 -0.1158925
#> 4    librarian -0.1158335 -0.1425213 -0.08330271 -0.1131277
#> 5 nutritionist -0.1115838 -0.1616845 -0.06140949 -0.1099395
#> 6    therapist -0.1042064 -0.1469076 -0.06003664 -0.1040616

# Above values are the result of cosine of a names - cosine of b names


res <- wefat_boot(its, its_vecs, x_name = "Careers",
                  a_name = "FemaleAttributes", b_name = "MaleAttributes",
                  se.calc = "quantile")
# > head(res)
# Careers        diff        lwr         upr      median
# 1    engineer -0.12325063 -0.1809730 -0.06716184 -0.11842031
# 2   architect -0.12196313 -0.1829440 -0.04531105 -0.12284758
# 3   carpenter -0.11699415 -0.1689593 -0.04850602 -0.11488846
# 4   machinist -0.09463988 -0.1516643 -0.02350410 -0.09633851
# 5    mechanic -0.09247058 -0.1334022 -0.04511853 -0.08779599
# 6 electrician -0.09104800 -0.1189226 -0.05512391 -0.08965860

# This is a bit hard to interpret, so we’ll make a picture
library(ggplot2)
ggplot(res, aes(x = median, y = 1:nrow(res))) +
  geom_point(col = "grey") +
  geom_point(aes(x = diff)) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0) +
  geom_text(aes(x = upr, label = Careers), hjust = "left", nudge_x = 0.005) +
  xlim(-0.25, 0.25) +
  ylab("Careers") +
  xlab("Cosine difference (male - female)")

# This will work quite generally for WEFATs, but remember to mention the right condition name in geom_text.
# I don’t have the male / female proportions for different jobs, so we can’t compare them right now.





# Our experiment using WEAT 8 ----
Career <- c("executive", "management", "professional", "corporation", "salary", "office", "business", "career")
Family <- c("home", "parents", "children", "family", "cousins", "marriage", "wedding", "relatives")
Male <- c("brother", "father", "uncle", "grandfather", "son", "he", "his", "him")
Female <- c("sister", "mother", "aunt", "grandmother", "daughter", "she", "hers", "her")
my_study <- melt(cbind(Career, Family, Male, Female),
               value.name="Word",
               varnames=c("", "Condition"))[ , -1]
my_study['Role'] <- ifelse((my_study$Condition=="Career" | my_study$Condition=="Family"),
                         "target",
                         "attribute")
my_study$Condition <- as.character(my_study$Condition)
my_study$Word  <- as.character(my_study$Word)
my_study$Role <- as.character(my_study$Role)
vectors <- cbn_extract_word_vectors(my_study$Word)
weat_perm(my_study, 
          vectors, 
          x_name = "Career", y_name = "Family", # target
          a_name = "Male", b_name = "Female", # attribute
          1000)
