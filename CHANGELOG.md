# Changelog

## v2.2.0
### Added
- Channel/Socket/Push map functions

### Fixed
- Channel.onJoinError now works as expected (thanks @coryc5)

## v2.1.0
### Added
- Presence support (thanks @knewter)
	- see examples/Presence.elm

## v2.0.1
### Changed
- Upgrade to Elm 0.18

## v2.0.0

### Added
- Client sends a heartbeat every 30 seconds (by default) to ensure that the connection stays healthy
- The heartbeat can be turned off or set to a different interval
