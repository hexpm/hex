## v0.6.1-dev

* Enhancements
  * Add PKIX hostname verification according to RFC6125
  * Improve error messages from HTTP error codes

## v0.6.1 (2014-12-11)

* Enhancements
  * Convert config file to erlang term file

## v0.6.0 (2014-10-12)

* Enhancements
  * Add support for packages with a different OTP application name than the package name
  * Add task `mix hex.docs` for uploading project documentation
  * Add email confirmation

* Bug fixes
  * Allow you to change your password with `mix hex.user update`
  * Correctly display dependencies in `mix hex.info PACKAGE VERSION`
  * Verify peer certificates when fetching tarball

## v0.5.0 (2014-09-19)

* Enhancements
  * Verify peer certificate for SSL (only available in OTP 17.3)
  * Reduce archive size with compiler option `debug_info: false`
  * Add support for config as an erlang term file
  * Warn if Hex was built against a different major.minor Elixir version

## v0.4.3 (2014-09-06)

## v0.4.2 (2014-08-31)

* Enhancements
  * Add task `hex.user whoami` that prints the locally authorized user
  * Add task `hex.user deauth` to deauthorize the local user
  * Rename environment variable `HEX_URL` to `HEX_API` to not confuse it with `HEX_CDN`

* Bug fixes
  * Print newline after progress bar

## v0.4.1 (2014-08-12)

* Enhancements
  * Add progress bar for uploading the tarball when publishing
  * Compare tarball checksum against checksum in registry
  * Bump tarball support to version 3
  * Rename task for authenticating on the local machine from `hex.key new` to `hex.user auth`
  * Remove the ability to pass password as a CLI parameter

* Bug fixes
  * Support lower-case proxy environment variables
  * Remove any timeouts when fetching package tarballs
