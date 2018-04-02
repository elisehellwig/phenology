installReload <- function(package='ScrapeClim',
                          directory='/Users/echellwig/Research',
                          check=FALSE) {
    require('devtools')


    packdir <- file.path(directory, package)

    #updating the documentation
    devtools::document(pkg = packdir)

    #checking to see if the code and everything is okay
    if (check) {
        devtools::check(pkg = packdir)
    }

    devtools::install(pkg = packdir)

    #restarting R
    .rs.restartR()

}
