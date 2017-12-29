library(Roxford)
library(png)
library(jpeg)
library(raster)
# For image processing
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")
# sudo apt-get install libfftw3-3 libfftw3-dev libtiff5-dev
# devtools::install('/home/joebrew/Documents/EBImage/')
library(EBImage)
keys <- readLines('keys.txt')
facekey = keys[1]  #look it up on your subscription site

# Define location of photo
lloc_foto <- "head_shot_small.jpg"


# or providing a url to a remote local image
# getFaceResponseURL("http://www.databrew.cc/images/joe.png", facekey, region = 'westcentralus')

joe <- readImage(lloc_foto)

# Get info
## using local images
out <- getFaceResponse(lloc_foto, facekey, region = 'westcentralus')

library(dplyr)
point_df <- expand.grid(x = seq(0, 500, 50),
                       y = seq(0, 500, 50)) %>%
  mutate(text = paste0(x, ' ', y))
plot(joe)
text(x = point_df$x,
     y = point_df$y,
     label = point_df$text, cex = 0.3)
create_square <- function(out){
  rect_columns <- which(grepl('ectangle', names(out)))
  for(j in rect_columns){
    out[,j] <- as.numeric(as.character(out[,j]))
  }
  left <- out$faceRectangle.left
  right <- out$faceRectangle.left + out$faceRectangle.width
  top <- out$faceRectangle.top
  bottom <- out$faceRectangle.top + out$faceRectangle.height
  mid_x <- mean(c(left, right))
  mid_y <- mean(c(top, bottom))
  polygon(x = c(left, right, right, left), 
          y = c(top, top, bottom, bottom),
          col = adjustcolor('darkred', alpha.f = 0.1), lwd = 1,
          border = adjustcolor('black', alpha.f = 0.4))
  points(left, top, cex = 0.5)
  points(right, top, cex = 1.5)
  points(left, bottom, cex = 0.5)
  points(right, bottom, cex = 1.5)
  lines(x = rep(mid_x, 2),
        y = c(bottom, top),
        col = adjustcolor('black', alpha.f = 0.3))
  lines(x = c(left, right),
        y = rep(mid_y, 2),
        col = adjustcolor('black', alpha.f = 0.3))
  abline(h = mid_y)
  abline(v = mid_x)
  # lines(x = c(left, right),
  #       y = c(bottom, top),
  #       lty = 2)
  # lines(x = c(left, right),
  #       y = c(top, bottom),
  #       lty = 2)
  # points(mid_y, bottom, cex = 20)
}
plot(joe)
create_square(out)

# Create an image sequence from a video
setwd('video2/')
out_dir <- 'out'
# system('ffmpeg -i video.mp4 frame%d.jpg')
# system('ffmpeg -i video2.mp4 frame%d.jpg')

frames <- dir()
frames <- frames[grepl('jpg', frames)]
# Renumber the frames
the_numbers <- gsub('frame', '', frames)
the_numbers <- gsub('.jpg', '', the_numbers)
the_numbers <- lendable::add_zero(the_numbers, 5)
for(i in 1:length(frames)){
  file.copy(from = frames[i],
            to = paste0(the_numbers[i], '.jpg'))
}
frames <- dir()
frames <- frames[grepl('jpg', frames)]
for (i in 1:length(frames)){
  message(i, ' of ', length(frames))
  this_frame <- frames[i]
  this_photo <- readImage(this_frame)
  this_info <- getFaceResponse(this_frame, facekey, region = 'westcentralus')
  if(nrow(this_info) >= 1){
    jpeg(filename = paste0(out_dir, '/',lendable::add_zero(i, 3), '.jpeg'),
         height = dim(this_photo)[2],
         width = dim(this_photo)[1])
    plot(this_photo)
    for(i in 1:nrow(this_info)){
      create_square(this_info[i,])
    }
    
    dev.off()
  }
  
  Sys.sleep(5)
}

system('mencoder "mf://*.jpeg" -mf fps=30 -o out.avi -ovc lavc -lavcopts vcodec=msmpeg4v2:vbitrate=640')
