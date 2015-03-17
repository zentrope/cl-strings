# cl-strings #

A java-api mimicking string library for Common Lisp.

## Abstract ##

There's not really a need for a string convenience library for Common Lisp, since so much of what you might want to do with strings is available with the usual list or sequence functions already provided. What's not available with those is neatly filled by Edi Weitz's [CL-PPCRE](http://www.cliki.net/CL-PPCRE).

Still, just for fun, here's a library mimicking the java String class, with a little StringTokenizer thrown in for good measure.

## Symbols ##

  * `*`whitespace-regex`*`
  * `*`whitespace-bag`*`

  * string-bytes
  * string-char-at
  * string-compare-to
  * string-concat
  * string-contains
  * string-content-equals
  * string-copy
  * string-ends-with
  * string-equals
  * string-format
  * string-index-of
  * string-last-index-of
  * string-length
  * string-matches
  * string-octets
  * string-region-matches
  * string-replace-all
  * string-replace-char
  * string-replace-first
  * string-replace-substring
  * string-split
  * string-starts-with
  * string-subsequence
  * string-substring
  * string-to-char-list
  * string-to-lower
  * string-to-upper
  * string-to-symbol
  * string-strip
  * string-value-of

  * string-make-tokenizer
  * string-tokenizer-next
  * string-tokenizer-count
  * string-tokenizer-peek
  * string-tokenizer-more-p
  * string-tokenizer-reset

## Dependencies ##

The cl-strings package depends on [CL-PPCRE](http://www.cliki.net/CL-PPCRE).

## Special Variables ##

**`*`whitespace-regex`*`**

> Used to determine what constitutes whitespace in the `string-split` and
> `string-make-tokenizer` functions. Defaults to \\S.

**`*`whitespace-bag`*`**

> Contains the white space characters used in trim operations.
> Defaults to #\Newline #\Space #\Return #\Tab.

## String Functions ##

**string-bytes** _string_ ⇒ _list_

> Returns a representation of `string` as a list of bytes (unsigned-byte 8), otherwise known as octets.

**string-char-at** _string index_ => _character_

> Returns the char value of the specified `index` in `string`.

**string-compare-to** _string1 string2_ &key _(ignore-case nil)_ => _integer_

> Compares to strings lexicographically, returning: _-1_, if string1 precedes
string2, _0_ if string1 and string2 are equal, and _1_ if string1 follows string2.

**string-concat** _string_ &rest _strings_ => _string_

> Returns a new string which is the concatenation of any number of `strings` to the end of the first `string`.


**string-contains** _string str-or-char-list_ => _boolean_

> Returns true if `str-or-char-list` is contained in `string`, otherwise false.

**string-content-equals** _string char-list_ => _boolean_

> Returns true if the `char-list` is equal to `string`.


**string-copy** _str-or-char-list_ => _string_

> Returns a new string representation of `str-or-char-list`.


**string-ends-with** _string sub-string_ => _boolean_

> Returns true if `string` ends with `substring`.

**string-equals** _string1 string2_ &key _(ignore-case nil)_ => _boolean_

> Returns true if `string1` has the same set of characters as `string2`, otherwise
> returns false. Will ignore case if `ignore-case` is t.


**string-format** _format_ &rest _arguments_ => _string_

> Returns a formatted string using the specificed `format` (cl printer) string applied to `arguments`.

**string-index-of** _string sub-str-or-char_ &optional _(from-index 0)_ => _integer_

> Returns the index of the first occurrance of `sub-str-or-char` in string, starting at `from-index`.


**string-last-index-of** _string sub-str-or-char_ &optional _from-index_ => _integer_

> Returns the index of the last occurance of `sub-str-or-char` in `string`, optionally starting at `from-index`.


**string-length** _string_ => _integer_

> Returns the length of `string`.


**string-matches** _string regex_ => _boolean_

> Returns true if `string` matches the regular expression `regex`.

**string-octets** _string_ => _list_

> Returns a representation of STRING as an array of octets (unsigned-byte 8)."

**string-region-matches** _string1 string1-offset string1-len string2 string2-offset string2-len_ &key _ignore-case_ => _boolean_

> Returns true if two string regions are equal, ignoring case if `ignore-case` is t.

**string-replace-all** _string regex replacement_ => _string_

> Returns a copy of `string` with all occurances of substrings matching
> `regex` in `string` replaced by the string
> `replacement`.

**string-replace-char** _string old-char new-char_ => _string_

> Returns a new `string` with all occurances of `old-char` replaced by `new-char`.

**string-replace-first** _string regex replacement_ => _string_

> Returns a copy of `string` in which the first substring matching `regex` is replaced by `replacemen`t.

**string-replace-subtstring** _string old-charlist new-charlist_ => _string_

> Returns a new `string` with all occurances of `old-charlist` replaced by
> `new-charlist`. A char list is a list of character
> objects.

**string-split** _string regex_ &key _limit omit-empty_ => _list_

> Returns a list of strings split from `string` based on the `regex` stopping
> after `limit` times if `limit` is non-zero, and
> removing empty strings if `omit-empty` is t.


**string-starts-with** _string token_ &optional _(offset 0)_ => _boolean_

> Returns true if `string` begins with `token` optionally starting the comparison at the `offset` into the `string`.

**string-subsequence** _string begin-index end-index_ => _list_

> Returns the list of characters found in `string` from
> `begin-index` to `end-index`.

**string-substring** _string begin-index_ &optional _end-index_ => _string_

> Returns a new string representing the characters in `string` from
> position `begin-index` to the end of `string`, or to
> the position indicated by `end-index` if provided.


**string-to-char-list** _string_ => _list_

> Returns a list of the characters making up `string`.


**string-to-lower** _string_ => _string_

> Returns an lowercase copy of `string`.


**string-to-upper** _string_ => _string_

> Returns an uppercase copy of `string`.

**string-to-symbol** _string_ ⇒ _symbol_

> Returns an interned symbol named `string`.

**string-strip** _string_ &key _(charbag `*`whitespace-bag`*`)_ => _string_

> Returns a copy of `string` with whitespace (defined as `charbag`) removed from both ends.

**string-value-of** _object_ &key _(lowercase nil)_ => _string_

> Returns the string representation of `object`, optionally forcing it to lowercase if `lowercase` is true.

## String Tokenizer ##

**string-make-tokenizer** _string_ &key _delimiter_ => _instance_

> Returns a `string-tokenize`r object for use with string-tokenizer-`*` methods.

**Example:**

```
CL> (defparameter *tokens* (string-make-tokenizer "one two three four"))
*TOKENS*

CL> (string-tokenizer-count *tokens*)
4
```

**Example:**

```
CL> (defun list-tokens (string &key (delimiter *whitespace-regex*))
      (let ((tokens (string-make-tokenizer string :delimiter delimiter)))
        (loop
           while (string-tokenizer-more-p tokens)
             do (format t "~a~%" (string-tokenizer-next tokens)))))

CL> (list-tokens "one two three four five")
one
two
three
four
five

CL> (list-tokens "one,two,three four,five")
one,two,three
four,five

CL> (list-tokens "one,two,three four,five" :delimiter ",")
one
two
three four
five

CL>
```


**string-tokenizer-count** _instance_ => _count_

> Returns the number of tokens in the `string-tokenizer` instance.

**string-tokenizer-next** _instance_ => _token_

> Returns the next token, advancing the token pointer, or nil if there are no more tokens.

**string-tokenizer-more-p** _instance_ => _token_

> Returns t if there are more tokens available, nil if not.

**string-tokenizer-peek** _instance_ => _token_

> Returns the next token without advancing the token pointer.


















