import <- function(package)
{
  print(paste( 'Importing Package :', package))
  if (!require(package, character.only = T))
    install.packages(package, dep = T)
  library(package, character.only = T)
}

import('tidyverse')
import('ggthemes')
import('repr')
import('corrplot')
import('psych')
import('visNetwork')
import('reshape2')
import('showtext')
import('maptools')

font.add.google('Gloria Hallelujah', 'gloria')
