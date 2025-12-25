changequote([,])dnl
dnl News system

dnl Internal state variables
define([_sec], [1])dnl
define([_oldcount], [0])dnl

dnl Section markers
define([BEGNEWS], [divert(1)])dnl
define([OLD],     [define([_sec], [2])divert(2)])dnl
define([ODD],     [define([_sec], [3])divert(3)])dnl

dnl Add a news item
define([ITEM], [dnl
ifelse(_sec, 1, , [define([_oldcount], incr(_oldcount))])dnl
<p>$1</p>
])dnl

dnl Finalize and define output macros
define([ENDNEWS], [dnl
divert(0)dnl
define([NEWS], [undivert(1)])dnl
define([ONEWS], [undivert(2)])dnl
define([ENEWS], [undivert(3)])dnl
define([ONUM], _oldcount)dnl
])dnl

dnl Helper macro to clean up all news buffers
define([CLEANUP_NEWS], [dnl
divert(-1)dnl
undivert(1)dnl
undivert(2)dnl
undivert(3)dnl
divert(0)dnl
])dnl
