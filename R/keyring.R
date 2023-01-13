#-------------------------------------------------------------------------------
# Manage keys and keyrings
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Manage keys and keyrings
#'
#' @description
#'
#' This set of functions handles keys that are needed to download products such
#' as satellite data.
#'
#' @param ringname character. The name of a keyring (a ringname).
#' @param username character. A username signed-up in a service provider.
#' @param password character. A password.
#' @param provider character. The name of a provider (e.g. \code{"modis"}).
#'
#' @return nothing.
#' @export
#' @importFrom keyring keyring_create keyring_unlock keyring_lock key_set_with_value key_list key_get key_delete keyring_is_locked
#'
#' @details This set of function are wrappers of the keyring package. A keyring
#' is an account that can hold keys (credentials) for various services. Once a
#' keyring is created, the user can start adding keys to it. Functions in the
#' `cronus` package that require credentials can automatically get them if
#' provided with the ringname. Users are reminded to log-in and log-out
#' in order to use their keyrings.
#'
#' @examples
#' \dontrun{
#' ringname <- "my_ringname"
#' password <- "my_password"
#'
#' create_keyring(ringname, password)
#' check_keyring(ringname)
#'
#' log_in(ringname, password)
#' check_keyring(ringname)
#'
#' add_key(ringname = ringname,
#' provider = "nass",
#' username = NULL,
#' password = "nass_key")
#'
#' add_key(ringname = ringname,
#'         provider = "usgs",
#'         username = "usgs_username",
#'         password = "usgs_password")
#'
#' get_username(ringname, "usgs")
#' get_password(ringname, "usgs")
#' delete_key(ringname, "usgs")
#'
#' log_out(ringname)
#' delete_keyring(ringname)
#' }
#' @describeIn create_keyring Create a keyring.
create_keyring <- function(ringname, password) {
  keyring::keyring_create(ringname, password)
}

#' @describeIn create_keyring Delete a keyring.
#' @export
delete_keyring <- function(ringname) {
  keyring::keyring_delete(ringname)
}

#' @describeIn create_keyring Log into your keyring.
#' @export
log_in <- function(ringname, password) {
  keyring::keyring_unlock(ringname, password)
}

#' @describeIn create_keyring Log out of your keyring.
#' @export
log_out <- function(ringname) {
  keyring::keyring_lock(ringname)
}

#' @describeIn create_keyring Add a key to your keyring.
#' @export
add_key <- function(ringname, provider, username, password) {
  keyring::key_set_with_value(provider, username, password, ringname)
}

#' @describeIn create_keyring Get a username.
#' @export
get_username <- function(ringname, provider) {
  keyring::key_list(provider, ringname)$username
}

#' @describeIn create_keyring Get a password.
#' @export
get_password <- function(ringname, provider) {
  keyring::key_get(provider, get_username(ringname, provider), ringname)
}

#' @describeIn create_keyring Delete a key.
#' @export
delete_key <- function(ringname, provider) {
  keyring::key_delete(provider, get_username(ringname, provider), ringname)
}

#' @describeIn create_keyring Check whether the user is logged-in.
#' @export
check_keyring <- function(ringname) {
  if (is.null(ringname)) {
    stop("Keyring not provided. See ?create_keyring for more information.")
  } else if (keyring::keyring_is_locked(ringname)) {
    stop("Keyring is locked. See ?log_in for more information.")
  }
}
