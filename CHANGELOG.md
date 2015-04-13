## v0.7.6-dev

## v0.7.5 (2015-04-12)

* Enhancements
  * Add task `hex.user test` for testing user authentication.
  * Add task `hex.outdated` for listing outdated packages compared to the registry.
  * Update CA store as of April 3.
  * Inform user if authentication failed because they did not confirm email.
  * Improve error message for unsupported tarball version.

* Bug fixes
  * Fix a bug where overriding a Hex dependency with a non-Hex dependency was ignored when the overriding at least two levels deep in the dependency tree

## v0.7.4 (2015-03-16)

* Bug fixes
  * Include all conflicting requirements in backtrack message
  * Fix a bug where backtrack message failed on optional requests

## v0.7.3 (2015-03-04)

* Bug fixes
  * Fix an error when merging locked and optional dependencies

## v0.7.2 (2015-03-04)

* Enhancements
  * Print messages on backtracks if dependency resolution failed, this is intended to help users resolve conflicts

* Bug fixes
  * Fix a bug where a dependency converged in mix did not consider all its requirements
  * Fix a bug where dependencies in the lock was considered even if they weren't requested

## v0.7.1 (2015-02-15)

* Bug fixes
  * Fix updating the registry

## v0.7.0 (2015-02-15)

* Enhancements
  * Print proxy options on startup
  * Add `mix hex.user password reset` and remove `mix hex.user update`
  * Create version 3 tarballs with erlang term encoded metadata

* Bug fixes
  * Verify peer certificate against CA certificate public key in `partial_chain`
  * Fix a bug where overriding a Hex dependency with a non-Hex dependency was ignored when the overriding happened in a sub-dependency
  * Create hex directory before writing registry

## v0.6.2 (2015-01-02)

* Enhancements
  * Add PKIX hostname verification according to RFC6125
  * Improve error messages from HTTP error codes
  * Improve HTTP performance
  * Add config options `api_url`, `cdn_url`, `http_proxy` and `https_proxy`
  * Support both doc/ and docs/ as documentation directory

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
