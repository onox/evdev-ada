[![Build status](https://github.com/onox/evdev-ada/actions/workflows/build.yaml/badge.svg)](https://github.com/onox/evdev-ada/actions/workflows/build.yaml)
[![Alire crate](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/evdev.json)](https://alire.ada.dev/crates/evdev.html)
[![License](https://img.shields.io/github/license/onox/evdev-ada.svg?color=blue)](https://github.com/onox/evdev-ada/blob/master/LICENSE)
[![GitHub release](https://img.shields.io/github/release/onox/evdev-ada.svg)](https://github.com/onox/evdev-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20libera.chat-orange.svg)](https://libera.chat)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/ada-lang/Lobby)

# evdev-ada

An Ada 2012 library to read input events and use force-feedback using Linux' evdev API.

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler

 * [Alire][url-alire] and (optionally) `make`

## Using the library

Use the library in your crates as follows:

```
alr with evdev
```

## Installing the tools

A tool to print information about an input event file can be build and run with:

```
$ alr run --args="/dev/input/event*"
```

Add `--read` as a second argument to read current state of axes and keys, or
add `--ff=rumble` or `--ff=periodic` to generate some force-feedback effects.

Alternatively, it can be build and installed with:

```
$ make
$ make PREFIX=~/.local install
```

Run `evdev-ada /dev/input/event*` to print information.

## Contributing

Please read the [contributing guidelines][url-contributing] before opening
issues or pull requests.

## License

This library is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-alire]: https://alire.ada.dev/
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-contributing]: /CONTRIBUTING.md
