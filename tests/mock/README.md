# Units Mock
These files mocks files such as those from the LCL and Trndi files.

The purpose is for the tests to run from the console, rather than using GUI tests. They are AI-generated!

## License
The general license is GPLv3, unless files contain parts from the original work of some project. In such case that license applies.

## About Trndi mock files
This folder contains a mock files, these are mocking Trndi functions:
### trndi.nativve.mock.pp
This is a native driver, that inherits the base native class - but mocks settings operations such as settings, dark mode, touch screen etc.
It also mocks GetURL which always uses fphttp client in this case.

## About slicke mock files
A few slicke library files are mocked here
### slicke.ux.alert.pp
Message dialogs are not displayed in tests
## slicke.ux.native.pp
Fonts etc are not used in CLI tests
## Razer chroma
These are mocked as there's no use for chaning colors in tests

## About Form Mocks
Some forms are mocked, such as the settings dialog, to allow umain to compile

## About component mocks
GUI components, such as the paintboxes and labels are mocked as we don't display them