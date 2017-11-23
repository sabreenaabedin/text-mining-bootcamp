setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")

# install necessary packages
install.packages("devtools")
devtools::install_github("soodoku/tuber", build_vignettes = TRUE, force=TRUE)
install.packages("httpuv")

library(devtools)
library(tuber)
library(httpuv)

# learn about tuber package
vignette("tuber-ex", package="tuber")

# obtain authorization from https://console.developers.google.com/apis/credentials?project=_
yt_oauth("1047222969450-roggh7tje9knh7bq1h9gbhfgmaoepal1.apps.googleusercontent.com", "uSaCAVaU7ViKfuaf6MY-y1DR")

get_stats(video_id="N708P-A45D0")

#comments
res <- get_comment_threads(c(video_id="Ef129P268X0"))
head(res)

#captions
get_caption_id <- list_caption_tracks(part="snippet", video_id="dvPJJUbM8IY")
count <- nrow(get_caption_id)

for (i in 1:count){
  caption_id <- get_caption_id$id[i]
  captions_hex <- get_captions(id=caption_id)
  captions <- paste(captions, rawToChar(as.raw(strtoi(captions_hex, 16L))))
}

captions
