# dtk.el

*Read Biblical texts in Emacs*

---

[diatheke](https://crosswire.org/wiki/Frontends:Diatheke) is a front-end to the [SWORD](www.crosswire.org/sword/) library. **dtk** facilitates reading a Biblical text, or other [diatheke](https://crosswire.org/wiki/Frontends:Diatheke)-accessible material, in Emacs. 

## Getting started

1. Ensure that [diatheke](https://crosswire.org/wiki/Frontends:Diatheke) is installed. In Debian, it is available as the `diatheke` package.

2. Ensure that at least one text accessible as a diatheke module is installed. One way to do this is by using the 'module manager' of [Xiphos](http://xiphos.org/). A number of texts are also available in Debian as packages (e.g., `sword-text-kjv`).


## Use

<kbd>M-x</kbd> `dtk`
     - read a portion of a text

<kbd>M-x</kbd> `dtk-search`
     - search the selected text

<kbd>M-x</kbd> `dtk-select-module`
     - select a module (e.g., KJV, ESV, or RNKJV)




## See also


[diatheke.el](https://github.com/JasonFruit/diatheke.el) is another interface to diatheke.

[sword-converter](https://github.com/alphapapa/sword-converter) converts SWORD module content to JSON and SQLite. Provides search functionality.

[sword-to-org](https://github.com/alphapapa/sword-to-org) converts SWORD modules content to org outlines.

