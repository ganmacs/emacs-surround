# emacs-surround

emacs-surround is like [vim.surround](https://github.com/tpope/vim-surround).
This plugins provides easily change, delete and insert such surrounding in pairs.

## Example

### change

Press `C-q "'`

```rb
"Hello| world!"
```

Change it to

```rb
'Hello world!'
```

### insert

Press `C-q i"` or `C-q " <return>`

```rb
Hel|lo
```

Change it to

```rb
"Hello"
```

### delete

Press `C-q d"`

```rb
"He|llo \"world\""
```

Change it to

```rb
Hello \"world\"
```

## Installation and Settings

Use `cask` or `el-get` or `clone` form `https://github.com/ganmacs/emacs-surround.git`


And add this script your `.init.el`

```lisp
(require 'emacs-surround)
(global-set-key (kbd "C-q") 'emacs-surround)
```

## Customize

You can use custimze pair.

For Example Add this line to your `.init.el`

```lisp
(add-to-list 'emacs-surround-alist '("}" . ("{ " . " }")))
```

Now press `C-q {}`

```rb
[1, 2, 3].each {p |1+i}
```

Change it  to

```rb
[1, 2, 3].each { p 1+i }
```
