import <- function(package, devtools = F, version = NA)
{
    if(devtools == T)
    {
        uri <- package
        package <- tail(strsplit(package, '/')[[1]], 1)

        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : GHUB :', uri))
            devtools::install_github(uri)
        }
        else
        {
            if(packageVersion(package) < version)
            {
                print(paste( ' Upgrading Package : GHUB :', uri))
                devtools::install_github(uri)
            }
        }
    }
    else
    {
        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : CRAN :', package))

            install.packages(package, dep = T)
        }
    }

    library(package, character.only = T)
}

import('tidyverse')
import('ggthemes')
import('repr')
import('corrplot')
import('psych')
import('visNetwork')
import('reshape2')
import('mapproj')
import('ggpubr')
import('RColorBrewer')
import('car')
import('devtools')
import('datastorm-open/visNetwork', devtools = T, version = '2.0.5')

# import('showtext')
# font.add.google('Gloria Hallelujah', 'gloria')
# showtext.auto(enable = T)
# family <- 'gloria'

family <- 'sans'
