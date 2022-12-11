# citeproc-el

[![Build Status](https://github.com/andras-simonyi/citeproc-el/actions/workflows/ci.yml/badge.svg)](https://github.com/andras-simonyi/citeproc-el/actions/workflows/ci.yml)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](http://melpa.org/packages/citeproc-badge.svg)](http://melpa.org/#/citeproc)
[![MELPA Stable](http://stable.melpa.org/packages/citeproc-badge.svg)](http://stable.melpa.org/#/citeproc)


A CSL 1.0.2 Citation Processor for Emacs.

**Table of Contents**

- [Introduction](#introduction)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
    - [Creating a citation processor](#creating-a-citation-processor)
    - [Creating citation structures](#creating-citation-structures)
    - [Managing a processor’s citation list and bibliography items](#managing-a-processors-citation-list-and-bibliography-items)
    - [Rendering citations and bibliographies](#rendering-citations-and-bibliographies)
	- [Rendering isolated references](#rendering-isolated-references)
	- [Supported output formats](#supported-output-formats)
	- [Hooks](#hooks)
- [Acknowledgements](#acknowledgements)
- [License](#license)

## Introduction

citeproc-el is an Emacs Lisp library for rendering citations and
bibliographies in styles described in the Citation Style Language (CSL), an
XML-based, open format to describe the formatting of bibliographic references
(see http://citationstyles.org/ for further information on CSL).

The library implements most of the [CSL 1.0.2
specification](http://docs.citationstyles.org/en/stable/specification.html),
including such features as citation disambiguation, cite collapsing and
subsequent author substitution, and passes more than 70% of the tests in the
[CSL Test Suite](https://github.com/citation-style-language/test-suite). In
addition to the standard
[CSL-JSON](https://github.com/citation-style-language/schema/blob/master/csl-data.json)
data format, citeproc-el has rudimentary support for reading bibliographic data
from BibTeX, biblatex and org-bibtex bibliographies and can produce output in several
formats including HTML and org-mode markup (see [Supported output
formats](#supported-output-formats) for the full list).

## Requirements

Emacs 26 or later compiled with libxml2 support. (The library is regularly
tested on Emacs 26.3, 27.2 and 28.1.)

## Installation

citeproc-el is available in the [MELPA package repository](https://melpa.org)
and can be installed using Emacs’s built-in package manager, `package.el`.

-------------------------------------------------------------------------------

## Usage

The central use-case of citeproc-el is that of feeding all citations occurring
in a document into a citation processor and rendering the complete list of
references and bibliography with it. This requires

  1. creating a citation processor object,
  2. collecting the document’s citations into a list of citation structures,
  3. loading this list into the processor, and
  3. rendering the loaded citations and the corresponding bibliography with the
     processor in one of the supported formats.

### Creating a citation processor

Citation processor objects are created using the `citeproc-create` function (the
signature of which was inspired by the
[citeproc-js](https://github.com/Juris-M/citeproc-js) API):

#### citeproc-create `(style item-getter locale-getter &optional locale force-locale)`

  * `style` is a CSL style file (e.g.,
    `"/usr/local/csl_styles/chicago-author-date.csl"`) to use for rendering the
    references;
  * `item-getter` is function that takes a list of bibliographic item id strings
    as its sole argument and returns an alist in which the given item ids are
    the keys and the values are the
    [CSL-JSON](https://github.com/citation-style-language/schema/blob/master/csl-data.json)
    descriptions of the corresponding bibliography items as parsed by Emacs’s
    built in JSON parser (keys are symbols, arrays and hashes should be
    represented as lists and alists, respectively);
  * `locale-getter` is a function that takes a CSL locale tag (e.g., `"fr-FR"`)
	as an argument and returns a corresponding CSL locale as parsed by Emacs’s
	`libxml-parse-xml-region`function or `nil`, with the exception of the
	default `"en-US"` argument for which it must return the corresponding parsed
	locale (`nil` is not allowed);
  * the optional `locale` is the CSL locale tag to use if the style doesn’t
	specify a default one (defaults to `"en-US"`); and
  * if the optional `force-locale` is non-nil then the specified `locale` is
    used even if the given `style` specifies a different one as default.
  * Returns a citation processor with an empty citation list.

citeproc-el integrators are free to implement their own special item-getter
and locale-getter functions (e.g., to provide item descriptions and locales from
a centralized source on a network) but citeproc-el provides some convenience
functions to create typical item- and locale-getters:

#### citeproc-itemgetter-from-csl-json `(file)`

#### citeproc-hash-itemgetter-from-csl-json `(file)`

Both functions return an item-getter function getting bibliography item
descriptions from a
[CSL-JSON](https://github.com/citation-style-language/schema/blob/master/csl-data.json)
file. The difference between them is that an item-getter produced by
`citeproc-itemgetter-from-csl-json` opens and reads directly from `file` each
time it is called, while `citeproc-hash-itemgetter-from-csl-json` reads the
content of `file` into a hash-table and the created function reads item
descriptions from this hash-table when called. As a consequence, functions
created with `citeproc-hash-itemgetter-from-csl-json` can perform better but
ignore changes in `file` between calls.

#### citeproc-itemgetter-from-bibtex `(file-or-files)`

#### citeproc-itemgetter-from-org-bibtex `(file-or-files)`

Return an item-getter function getting bibliography item descriptions from
BibTeX/org-bibtex files. Similarly to `citeproc-itemgetter-from-csl-json`, these
functions open and read directly from the specified files each time they are
called.

#### citeproc-hash-itemgetter-from-any `(file-or-files &optional no-sentcase-wo-langid)`
Return a getter for `file-or-files` in any supported format.
The format is determined on the basis of file extensions.
Supported formats:

- CSL-JSON (.json extension) the recommended native format;
- BibTeX/biblatex (.bib or .bibtex extension),
- org-bibtex (.org extension). 

If `no-sentcase-wo-langid` is non-nil then title fields in items without a
`langid' field are not converted to sentence-case.

#### citeproc-locale-getter-from-dir `(directory)`

Return a locale-getter function getting CSL locales from `directory`. The
directory must contain the CSL locale files under their canonical names (as
found at the [Official CSL locale
repository](https://github.com/citation-style-language/locales)), and must
contain at least the default `en-US` locale file.

### Creating citation structures

Citation structures are created with

#### citeproc-citation-create `(&key cites note-index mode suppress-affixes capitalize-first ignore-et-al) `


  * `cites` is a list of alists describing cites. Each alist must contain the
     `id` symbol as key coupled with an item id string as value, and can
     optionally contain additional information with the symbol keys `prefix`,
     `suffix`, `locator`, `label` (all with string values);
  * `note-index` is the note index of the citation if it occurs in a note and
     `nil` otherwise;
  * `mode` is either nil (for the default citation mode) or one
    of the symbols `suppress-author`, `textual`, `author-only`,
    `year-only`, `title-only`, `locator-only`, `bib-entry`;
  * `suppress-affixes` is non-nil if the prefix and the suffix of the citation
    (e.g., opening and closing brackets) have to be suppressed;
  * `capitalize-first` is non-nil if the first word of the citation has to be
    capitalized;
  * `ignore-et-al` is non-nil if et-al settings should be ignored for the first
    cite.
  
### Managing a processor’s citation list and bibliography items

Processor objects maintain lists of citations and bibliography items,
which can be manipulated with the following functions:

#### citeproc-append-citations `(citations proc)`

Append `citations`, a list of citation structures, to the citation list of
citation processor `proc`.

#### citeproc-add-uncited `(itemids proc)`

Add uncited bib items with `itemids` to `proc`. As an extension, an itemid can
be the string "*" which has the effect of adding all items available in the
itemgetter.

#### citeproc-add-subbib-filters `(filters proc)`

Add sub-bibliography `filters` to `proc`. `filters` 
should be a list of alists containing symbol keys and
string values, each pair describing an atomic condition to be
satisified by the printed entries. The following keys are
supported:

- `type`: print only entries of the given type. Type is the
  bib(la)tex entry type if available, otherwise the CSL type is
  used as fallback;
- `nottype`: print only entries not of the given type. Type is
  the bib(la)tex entry type if available, otherwise the CSL type
  is used as fallback;
- `csltype`, `notcsltype`: same as `type` and `nottype` but uses
  the entries' CSL type even if the bib(la)tex type is also
  available;
- `keyword`: print only entries with the given keyword;
- `notkeyword`: print only entries without the given keyword;
- `filter`: print only entries for which the function named by
  the key returns a non-nil value.

#### citeproc-clear `(proc)`

Clear the citation and bibliography lists of citation processor `proc`.

### Rendering citations and bibliographies

#### citeproc-render-citations `(proc format &optional internal-links)`

Render all citations in citation processor `proc` in the given `format`. Return
a list of formatted citations. `format` is one of the [supported output
formats](#supported-output-formats) as a symbol. 

If the optional `internal-links` is `bib-links` then link cites
to the bibliography regardless of the style type, if `no-links`
then don't add internal links, if nil or `auto` then add internal
links based on the style type (cite-cite links for note styles
and cite-bib links else). For legacy reasons, any other value is
treated as `no-links`.

#### citeproc-render-bib `(proc format &optional internal-links no-external-links bib-formatter-fun)`

Render a bibliography of the citations in citation processor `proc` in the
given`format`. `format` is one of the [supported output
formats](#supported-output-formats) as a symbol. 

For the optional `internal-links` argument see `citeproc-render-citations`. If
the optional `no-external-links` is non-nil then don't generate external links.
If the optional `bib-formatter-fun` is given then it will be used to join the
bibliography items instead of the content of the chosen formatter’s `bib` slot
(see the documentation of the `citeproc-formatter` structure type for details).

Returns a `(FORMATTED-BIBLIOGRAPHY . FORMATTING-PARAMETERS)` pair, in which
`FORMATTED-BIBLIOGRAPHY` is either a single bibliography or a list of
sub-bibliograhies if filters were added to the processor, and
`FORMATTING-PARAMETERS` is an alist containing the values of the following
formatting parameters keyed to the parameter names as symbols:

  * `max-offset` (integer): The width of the widest first field in the
    bibliography, measured in characters.
  * `line-spacing` (integer): Vertical line distance specified as a multiple of
    standard line height.
  * `entry-spacing` (integer): Vertical distance between bibliographic entries,
    specified as a multiple of standard line height.
  * `second-field-align` (`flush` or `margin`): The position of second-field
    alignment.
  * `hanging-indent` (boolean): Whether the bibliography items should
    be rendered with hanging-indents.

### Rendering isolated references

Reference rendering is typically context-dependent, as the rendered form can
depend on the position of the reference and the presence of other references may
make it necessary to add disambiguating information. Since computing the
context-dependent form might be too time-consuming or unnecessary for some
applications (e.g., for generating previews), citeproc-el provides functions
to render isolated references.

Isolated rendering requires only the creation of a `citeproc-style` object (as
opposed to a full-blown citation processor) with the function

#### citeproc-create-style `(style locale-getter &optional locale force-locale)`

Return a newly created `citeproc-style` object. See the documentation of
[citeproc-create](#citeproc-create-style-item-getter-locale-getter-optional-locale-force-locale)
for the description of the arguments.

After the creation of a style object references can be rendered by

#### citeproc-render-item `(item-data style mode format &optional no-external-links)`
Render an item described by `item-data` with `style`. `item-data` is the parsed
form of a bibliographic item description in
[CSL-JSON](https://github.com/citation-style-language/schema/blob/master/csl-data.json)
format, `style` is a `citeproc-style` style object, `mode` is one of the symbols
`bib` or `cite`, `format` is a supported output format (see next section) as a
symbol. If the optional `no-external-links` is non-nil then don't generate
external links in the item.

### Supported output formats

Currently `html`, `org`, `plain` (plain text), `latex`, `org-odt`, `org-latex`
(for Org ODT and LaTeX export), `csl-test` (for the CSL test suite) and `raw`
(internal rich-text format, for debugging) are supported as output formats. New
ones can easily be added — see `citeproc-formatters.el` for examples.

### Hooks

citeproc-el provides the following hook variables:

#### citeproc-citation-postprocess-functions
A list of functions to postprocess rendered citations. Each function takes a
single argument, a rich-text, and returns a post-processed rich-text value. The
functions are applied in the order they appear in the list.

#### citeproc-name-postprocess-functions
A list of functions to postprocess rendered names.
Each function takes three arguments:

- the rich-text rendering of a name to be postprocessed,
- the rendered name as an alist with CSL name-part
  keys (`family`, `given` etc.), and
- the rendering context, as a `citeproc-context` structure.

The output of each function should be the postprocessed rich-text, and
the functions are applied in the order they appear in the list.

-------------------------------------------------------------------------------

## Acknowledgements

### Code contributions

- [Marvin Gülker](https://github.com/Quintus)
- [Matt Price](https://github.com/titaniumbones)
- [Alexey Shiklomanov](https://github.com/ashiklom)

### Advice, discussion

- [Rudolf Adamkovič](https://github.com/salutis)
- [Bruce D'Arcus](https://github.com/bdarcus)
- [Marvin Gülker](https://github.com/Quintus)
- [Denis Maier](https://github.com/denismaier)
- [Matt Price](https://github.com/titaniumbones)

-------------------------------------------------------------------------------

## License

Copyright (C) 2018-2022 András Simonyi

Authors: András Simonyi

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see http://www.gnu.org/licenses/.
