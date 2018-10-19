library(httr)
library(cbn)

# Microsoft Project Oxford
Oxford = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceAttributes=gender"
csAPI = "7d8c715138f545baacb62db3c1f22adb" # You need to grab your own API

# Gender detection powered by Microsoft Cognitive Services ----
# This is the function for face detection
gender_recognition <- function (g) {
  candidate = list(url = g)
  faceRecognition = POST(Oxford,
                         content_type('application/json'), 
                         encode = 'json',
                         body = candidate,
                         add_headers(.headers = c('Ocp-Apim-Subscription-Key' = csAPI))
  )
  do.call(rbind,
          content(faceRecognition)[[1]]
          $faceAttributes['gender'])[1]
}

# Test it
Obama <- "https://upload.wikimedia.org/wikipedia/commons/e/e9/Official_portrait_of_Barack_Obama.jpg"
gender_recognition(Obama)

Rice <- "http://www.femstory.com/images/condoleezza-rice.jpg"
gender_recognition(Rice)


# Now, let's loop through 105 photos ----
gender <- NULL
photos <- c("https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f1.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f2.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f3.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f4.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f5.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f6.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f7.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f8.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f9.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f10.jpg?raw=true"   
            )

photos <- c(
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f11.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f12.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f13.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f14.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f15.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f16.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f17.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f18.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f19.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f20.jpg?raw=true" 
            )

for (url in 1:length(photos)) {
  
  candidate = list(url = photos[url])
  faceRecognition = POST(Oxford,
                         content_type('application/json'), 
                         encode = 'json',
                         body = candidate,
                         add_headers(.headers = c('Ocp-Apim-Subscription-Key' = csAPI))
  )
  
  genderID <- (do.call(rbind,
                       content(faceRecognition)[[1]]
                       $faceAttributes['gender'])[1])
  gender   <- c(gender, genderID)
  
}

dataGender <- cbind(photos, gender)
View(dataGender)

          
# Comparing two candidates by photo
compare_candidates <- function(x, y) {
  
  require(cbn)
  
  # gender recognition for the first candidate
  candidate_1 = list(url = x)
  faceResponse = POST(
    url = Oxford, 
    content_type('application/json'), 
    add_headers(.headers = c('Ocp-Apim-Subscription-Key' = csAPI)),
    body = candidate_1,
    encode = 'json'
  )
  candidate_1 <- content(faceResponse)[[1]]
  candidate_1_gender <- do.call(rbind, candidate_1$faceAttributes['gender'])[1]
  
  # gender recognition for the second candidate
  candidate_2 = list(url = y)
  faceResponse = POST(
    url = Oxford, 
    content_type('application/json'), 
    add_headers(.headers = c('Ocp-Apim-Subscription-Key' = csAPI)),
    body = candidate_2,
    encode = 'json'
  )
  candidate_2 <- content(faceResponse)[[1]]
  candidate_2_gender <- do.call(rbind, candidate_2$faceAttributes['gender'])[1]
  
  if(candidate_1_gender == "female" & candidate_2_gender == "female") 
  {
    print("both females")
    
  } else if (candidate_1_gender == "male" & candidate_2_gender == "female"
             |candidate_1_gender == "female" & candidate_2_gender == "male" ) {
    # REPLACE THIS WITH FUNCTION
    print(weat_perm(custom, vecs_copycat, x_name = "Flowers", y_name = "Insects",
                    a_name = "Pleasant", b_name = "Unpleasant", 1000))
    
    print("One candidate is more associated with certain attributes than the other")
    
  } else if (candidate_1_gender == "male" & candidate_2_gender == "male") {
    
    print("both males")
    
  }
}

compare_candidates(Obama, Rice)


