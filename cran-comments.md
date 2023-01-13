## Release summary

This is a minor release in the 0.0.* series.  

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Build windows binary package 

The check was performed with `devtools::check_win_devel()` and returned only the
usual warning reminder concerning the package maintainer.

## CRAN Check

CRAN check was performed with `rhub::check_for_cran()`. Result status:

* OK on Windows Server 2022, R-devel, 64 bit
* PREPERROR on Fedora Linux, R-devel, clang, gfortran
* PREPERROR on Ubuntu Linux 20.04.1 LTS, R-release, GCC

Errors were caused due to ImageMagick++ dependency of the `magick` package.

#> * installing *source* package ‘magick’ ...
5265#> ** package ‘magick’ successfully unpacked and MD5 sums checked
5266#> ** using staged installation
5267#> Package Magick++ was not found in the pkg-config search path.
5268#> Perhaps you should add the directory containing `Magick++.pc'
5269#> to the PKG_CONFIG_PATH environment variable
5270#> Package 'Magick++', required by 'virtual:world', not found
5271#> Using PKG_CFLAGS=
5272#> Using PKG_LIBS=-lMagick++-6.Q16
5273#> --------------------------- [ANTICONF] --------------------------------
5274#> Configuration failed to find the Magick++ library. Try installing:
5275#> - deb: libmagick++-dev (Debian, Ubuntu)
5276#> - rpm: ImageMagick-c++-devel (Fedora, CentOS, RHEL)
5277#> - csw: imagemagick_dev (Solaris)
5278#> - brew imagemagick@6 (MacOS)
5279#> For Ubuntu versions Trusty (14.04) and Xenial (16.04) use our PPA:
5280#> sudo add-apt-repository -y ppa:cran/imagemagick
5281#> sudo apt-get update
5282#> sudo apt-get install -y libmagick++-dev
5283#> If Magick++ is already installed, check that 'pkg-config' is in your
5284#> PATH and PKG_CONFIG_PATH contains a Magick++.pc file. If pkg-config
5285#> is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
5286#> R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
5287#> -------------------------- [ERROR MESSAGE] ---------------------------
5288#> :1:10: fatal error: 'Magick++.h' file not found
5289#> #include
5290#> ^~~~~~~~~~~~
5291#> 1 error generated.
