# citeproc-el

[![Build Status](https://travis-ci.org/andras-simonyi/citeproc-el.svg?branch=master)](https://travis-ci.org/andras-simonyi/citeproc-el) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A CSL 1.01 Citation Processor for Emacs.

**Table of Contents**

- [Installation](#installation)
- [Usage](#usage)
- [License](#license)

## Installation

The long-term plan is to make `citeproc-el` available as a [MELPA](https://melpa.org)
package, but until then the recommended method of installation is to download
the latest release as a package from this link, and install it using the
`package-install-file` Emacs command.
 
## Usage

The central use-case of `citeproc-el` is that of feeding all citations occurring
in a document into a citation processor and rendering the complete list of
references and bibliography with it. This requires the following steps:

  1. Creating a citation processor object,
  2. collecting the document's citations into a list of `citeproc-citation`
     structures,
  3. loading this list into the processor's citation queue, and 
  3. rendering the loaded citations and the corresponding bibliography with the
     processor.

### Creating a citation processor

Citation processor objects are created using the `citeproc-create` function.
Inspired by the [citeproc-js](https://github.com/Juris-M/citeproc-js) API, the
signature of this function is

#### citeproc-create `(style item-getter locale-getter &optional locale force-locale)`

where 

  * `style` is a CSL style file (e.g.,
    `"/usr/local/csl_styles/chicago-author-date.csl"`) to use for rendering the
    references;
  * `item-getter` is function that takes a list of bibliographic item id strings
    as its sole argument and returns an alist in which the given item ids are
    the keys and the values are the
    [CSL-JSON](https://github.com/citation-style-language/schema/blob/master/csl-data.json)
    descriptions of the corresponding bibliography items as parsed by Emacs's
    built in JSON parser (keys are symbols, arrays and hashes should be
    represented as lists and alists, respectively);
  * `locale-getter` is a function that takes a CSL locale tag (e.g., `"fr-FR"`)
	as an argument and returns a corresponding CSL locale as parsed by Emacs's
	`libxml-parse-xml-region`function or nil, with the exception of the default
	`"en-US"` argument for which it must return the corresponding parsed locale;
  * the optional `locale` is the CSL locale tag to use if the style doesn't
	specify a default one (defaults to `"en-US"`); and
  * if the optional `force-locale` is non-nil then the specified `locale` is
    used even if the given `style` specifies a different one as default.

The function returns a `citeproc-proc` structure.

`citeproc-el` integrators are free to implement their own special item-getter
and locale-getter functions (e.g., to provide item descriptions and locales from
a centralized source on a network) but `citeproc-el` provides some convenience
functions to create typical item- and locale-getters:

#### citeproc-itemgetter-from-csl-json `(file)`

#### citeproc-hash-itemgetter-from-csl-json `(file)`

#### citeproc-itemgetter-from-bibtex `(file)`

#### citeproc-locale-getter-from-dir `(dir)`

### Citation structures

## License

Copyright (C) 2017 András Simonyi

Authors: András Simonyi andras.simonyi@gmail.com

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see http://www.gnu.org/licenses/.
