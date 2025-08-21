# Changelog [![Elm package](https://img.shields.io/elm-package/v/curtissimo/elm-inertial-scroll-detection.svg)](https://package.elm-lang.org/packages/curtissimo/elm-inertial-scroll-detection/latest/)

All notable changes to
[the `curtissimo/elm-inertial-scroll-detection` elm package](http://package.elm-lang.org/packages/curtissimo/elm-inertial-scroll-detection/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.1.0/)
and this project adheres to
[Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 3.0.0

### Added 

- Fire synthetic scroll end to patch that Safari doesn't fire the event

### Fixed

- Bug on bouncy inertial direction that moves beyond scrollable bounds
- Bug on change of sticky direction when no touch* events were fired 
- Bug on not transitioning from moved state to inertial state when touchend
  doesn't fire because sometimes it doesn't

## 1.0.0

### Added

- The `Components.InertialScrollDetector` component
- Documentation for the component
- An example using [`elm.land`](https://elm.land)
