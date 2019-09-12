# autocad-lib

Base functions used in my AutoCAD environment.

Language: Autolisp

It does not contain commercial data or processes.

## Getting Started

### Prerequisites

* AutoCAD 2017 or newer - prior version may work, just not tested.

### Installing and Deploying into Autocad

1) Navigate to the desired source folder ```<src-folder>``` and run following command.  To create inside a different folder name than autocad-lib, use ```<lib-folder>```, else "autocad-lib" is used for ```<lib-folder>```.
```
cd <src-folder>
git clone git@github.com:trobbie/autocad-lib <lib-folder>
```
2) In AutoCAD, add the following path to your Trusted Locations.  Note the "...", which trusts all sub-folders as well.
```
<src-folder>\...
```
3) To load the library, run the following from AutoCAD console:
```
(load "<src-folder>/<lib-folder>/_all.lsp")
```
4) Consider auto-loading it with each new drawing:
  * In AutoCAD, open Customize User Interface.
  * Right-click LISP Files, and select Load LISP
  * Navigate to the file ```_all.lsp```.
  * Apply and OK.
5) FYI, for development purposes, I add it as a macro command in the Tool Pallette.

## Running the tests

To run tests, run the following from AutoCAD console.
```
(load "<src-folder>/<lib-folder>/lsp/test/_test-all.lsp")
```

FYI, for development purposes, I add it as a macro command in the Tool Pallette.

## Authors

* **Trevor Robbie** - *Initial work* - [Github account](https://github.com/trobbie)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

