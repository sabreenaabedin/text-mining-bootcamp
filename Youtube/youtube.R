# install necessary packages
install.packages("devtools")
library(devtools)
devtools::install_github("soodoku/tuber", build_vignettes = TRUE, force=TRUE)
library(tuber)

# learn about tuber package
vignette("tuber-ex", package="tuber")

install.packages("httpuv")
library(httpuv)


# obtain authorization from https://console.developers.google.com/apis/credentials?project=_
yt_oauth("1047222969450-roggh7tje9knh7bq1h9gbhfgmaoepal1.apps.googleusercontent.com", "uSaCAVaU7ViKfuaf6MY-y1DR")

get_stats(video_id="N708P-A45D0")

#comments
res <- get_comment_threads(c(video_id="Ef129P268X0"))
head(res)

#captions
cap <- get_captions(video_id="Ef129P268X0")
