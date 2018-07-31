# dtk.el

*Read Biblical texts in Emacs*

---

[diatheke](https://crosswire.org/wiki/Frontends:Diatheke) is a front-end to the [SWORD](www.crosswire.org/sword/) library. **dtk** facilitates reading a Biblical text, or other [diatheke](https://crosswire.org/wiki/Frontends:Diatheke)-accessible material, in Emacs. 


## Getting started

1. Ensure that [diatheke](https://crosswire.org/wiki/Frontends:Diatheke) is installed. In Debian, it is available as the `diatheke` package.

2. Ensure that at least one text accessible as a diatheke module is installed. One way to do this is by using the 'module manager' of [Xiphos](http://xiphos.org/). A number of texts are also available in Debian as packages (e.g., `sword-text-kjv`).


## Use

Invoke `dtk` (<kbd>M-x</kbd> `dtk`) to read a portion of a text. Use `C-c h` to obtain a list of keybindings specific to `dtk-mode`. Some functions which may be of interest are described below.

<kbd>M-x</kbd> `dtk-clear-dtk-buffer`
     - clear the dtk buffer 

<kbd>M-x</kbd> `dtk-go-to`
     - go to a specific location in a text

## Performing a dictionary lookup

Some texts have dictionary references embedded in the texts. For these texts, dictionary lookups may be available. By default, <kbd>S</kbd> will, by invoking `dtk-show-dict-entry`, display the related dictionary entry, if that entry is available.


### Searching within a text

Search the selected text with <kbd>M-x</kbd> `dtk-search`.


### Selecting a module

Modules fall into a number of categories. Select the category of module which is under consideration with <kbd>M-x</kbd> `dtk-select-module-category`. By default, this functionality is bound to <kbd>M</kbd>.

Select the module of interest (e.g., KJV, ESV, or RNKJV) with <kbd>M-x</kbd> `dtk-select-module`. By default, this functionality is bound to <kbd>m</kbd>.


## See also

[diatheke.el](https://github.com/JasonFruit/diatheke.el) is another interface to diatheke.

[sword-converter](https://github.com/alphapapa/sword-converter) converts SWORD module content to JSON and SQLite. Provides search functionality.

[sword-to-org](https://github.com/alphapapa/sword-to-org) converts SWORD modules content to org outlines.
