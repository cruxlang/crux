"..." is of type unicode text.

b"..." is of type binary data.

Both support the following escape codes:

\a  07  alarm
\b  08  backspace
\f  0c  formfeed
\n  0a  newline
\r  0d  cr
\t  09  tab
\v  0b  vertical tab
\\  5c  backslash
\'  27
\"  22
\?  3f

Both string types support \xhh escapes which inserts an 8-bit number in the sequence.
Unicode text string types also support \uhhhh and \Uhhhhhhhh for 16- and 32-bit numbers.

We support everything that C does but the \nnn octal escape sequence.

r"..." is of type unicode text, but does not support escape codes of any kind.
rb"..." is of type binary data.

Text will either be encoded in memory using platform-native encoding (e.g. UTF-16 in JavaScript) or UTF-8.
