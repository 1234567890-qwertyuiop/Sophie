library(httr)
library(cbn)

# Microsoft Project Oxford
Oxford = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceAttributes=gender"
csAPI = "7d8c715138f545baacb62db3c1f22adb" # You need to grab your own API

# 1. Gender detection powered by Microsoft Cognitive Services ----
# This is the function for the face detection
gender_recognition <- function (x) {

  y = POST(Oxford,
      content_type('application/json'), 
      encode = 'json',
      body = list(url = x),
      add_headers(.headers = c('Ocp-Apim-Subscription-Key' = csAPI))
  )
  do.call(rbind,
          content(y)[[1]]
          $faceAttributes['gender'])[1]
}

# Test the function
Obama <- "https://upload.wikimedia.org/wikipedia/commons/e/e9/Official_portrait_of_Barack_Obama.jpg"
gender_recognition(Obama)

Rice <- "http://www.femstory.com/images/condoleezza-rice.jpg"
gender_recognition(Rice)


# 2. Now, let's loop through 105 photos ----
# For our experiment, we ran all 102 photos at once. But, you might have to run the photos
# in batches of 20s because Microsoft limits 20 calls per minute for free accounts.
# I will provide code in the tutorial, but here I want to show you exactly what I've done. 
# It costs about 3 cents USD to run 100 codes at once
gender <- NULL
match <- NULL
photos <- c("https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_1.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_2.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_3.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_4.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_5.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_6.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_7.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_8.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_9.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_10.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_11.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_12.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_13.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_14.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_15.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_16.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_17.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_18.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_19.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_20.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_21.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/f_22.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m1.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m2.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m3.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m4.jpg?raw=true",
            
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m6.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m7.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m8.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m9.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m10.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m11.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m12.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m13.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m14.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m15.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m16.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m17.jpg?raw=true",
           
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m19.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m20.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m21.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m22.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m23.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m24.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m25.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m26.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m27.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m28.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m29.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m30.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m31.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m32.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m33.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m34.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m35.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m36.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m37.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m38.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m39.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m40.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m41.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m42.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m43.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m44.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m45.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m46.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m47.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m48.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m49.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m50.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m51.jpg?raw=true",
            
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m53.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m54.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m55.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m56.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m57.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m58.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m59.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m60.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m61.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m62.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m63.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m64.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m65.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m66.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m67.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m68.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m69.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m70.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m71.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m72.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m73.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m74.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m75.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m76.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m77.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m78.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m79.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m80.jpg?raw=true",
            "https://github.com/1234567890-qwertyuiop/Sophie/blob/master/115th%20Congress/m81.jpg?raw=true"
             )

library(curl)
photos <- readLines(curl("https://raw.githubusercontent.com/1234567890-qwertyuiop/Sophie/master/115th%20Congress/list.csv"))
  
# See whether the face detection technology identifies gender correctly
for (url in 1:length(photos)) {
  
  y         <- POST(Oxford,
                    content_type('application/json'), 
                    encode = 'json',
                    body = list(url = photos[url]),
                    add_headers(.headers = c('Ocp-Apim-Subscription-Key' = csAPI))
  )
  genderID  <- (do.call(rbind, 
                        content(y)[[1]]
                        $faceAttributes['gender'])[1])
  gender    <- c(gender, genderID)
  
}
photos      <- gsub(".*[Congress/]([^.]+)[.].*", "\\1", photos)
dataGender  <- cbind(photos, gender)
View(dataGender)


# 3. Next, we prepare the vectorized texts using GloVe. There are two methods for this: ----
# Easy method: Download the pre-trained data


# Fun method: 


# 4. Then, we replicate the WEAT6 manually ----



# You can also use csv files if you're more comfortable with that


# 5. Next, we randomly select one candidate from each gender, then compare it against each other ----
random_compare 



# We also iterated it 10,000 times


# That's it! It's a straightforward process.

          
# Manually comparing two candidates by photo
compare_candidates <- function(x, y) {
  
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


