library(RCurl)

ftp.root <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/"
folder.root <- "~/Documents/Esther/Projects/Flu-forecasting-underdog/Data"

# Have to make sure to be logged in at https://www.ncdc.noaa.gov/atrac/index.html
# Username: esther.van.kleef@gmail.com
# Password: London11

#=====================================================================
#   Function that downloads files from URL
#=====================================================================

fdownload <- function(sourcelink) {

  targetlink <- paste(folder.root, substr(sourcelink, nchar(ftp.root)+1,
                                          nchar(sourcelink)), sep = '')

  # list of contents
  filenames <- getURL(sourcelink, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\n")
  filenames <- unlist(filenames)

  files <- filenames[grep('\\.', filenames)]
  dirs <- setdiff(filenames, files)
  if (length(dirs) != 0) {
    dirs <- paste(sourcelink, dirs, '/', sep = '')
  }

  # files
  for (filename in files) {

    sourcefile <- paste(sourcelink, filename, sep = '')
    targetfile <- paste(targetlink, filename, sep = '')

    download.file(sourcefile, targetfile)
  }

  # subfolders
  for (dirname in dirs) {

    fdownload(dirname)
  }
}

fdownload(ftp.root)
