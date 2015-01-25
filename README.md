# emacs-surround

Emacs version of the vim.surround.

## Installation

Use `cask` or `el-get` or clone form `https://github.com/ganmacs/emacs-surround.git`

```
(require 'emacs-surround)
(global-set-key (kbd "C-q") 'emacs-surround)
```

## Example

### change

Press `C-t "'`

```
"Hello| world!"
```

change it to

```
'Hello world!'
```

### insert

Press `C-t i"` or `C-t " <return>`

```
Hel|lo
```

change it to

```
"Hello"
```

### delete

Press `C-t d"`

```
"Hello \"world\""
```

change it to

```
Hel|lo \"world\"
```

## TODO

* 場所を覚えておく
