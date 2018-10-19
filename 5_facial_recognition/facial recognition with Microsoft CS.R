library(httr)
library(cbn)

# Microsoft Project Oxford
Oxford = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceAttributes=gender"
csAPI = "7d8c715138f545baacb62db3c1f22adb" # You need to grab your own API

# Pictures of politicians ----
Obama = "https://upload.wikimedia.org/wikipedia/commons/e/e9/Official_portrait_of_Barack_Obama.jpg"
Rice = "http://www.femstory.com/images/condoleezza-rice.jpg"
Lincoln = "https://psmag.com/.image/t_share/MTI3NTgxNjM1NzcxMTU2NDkw/lincoln-portrait.jpg"
Farrar = "https://static.independent.co.uk/s3fs-public/thumbnails/image/2017/03/13/07/state-representative-jessica-farrar-texas.jpg?w968h681"
senator = "https://github.com/peoplecure/Sophie/blob/master/115th%20Congress/WebsiteBioPhoto.jpg?raw=true"

# Live API method (This will not work if Microsoft discontinues Face API services) ----
# Gender recognition function (Microsoft has a guide for creating this in python)
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
          obama_gender = gender_recognition(Obama)
          obama_gender
          
          rice_gender = gender_recognition(Rice)
          rice_gender
          
          gender_recognition(senator)
          
# alternatively, you can directly add the image url to the function
          gender_recognition("https://upload.wikimedia.org/wikipedia/commons/e/e9/Official_portrait_of_Barack_Obama.jpg")
          
          
          
          
          
# Compare candidates function via live distance calculation and facial recognition
# The process takes a longer time
# Choose whichever candidate from the list or create a new variable
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
compare_candidates(Farrar, Obama)
compare_candidates(Obama, Lincoln)
compare_candidates(Rice, Farrar)
compare_candidates(Obama, senator)

# pre-saved method ----


