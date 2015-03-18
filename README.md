#erlang-phpserializer
Takes a PHP [serialize()](http://php.net/manual/en/function.serialize.php) data and turns it into a
data structure usable by erlang back and forth.

There has been to many projects I've seen that uses PHP serialized strings in database or other place,
when clearly it shouldn't be used like that. To be able to either rectify or use the values,
I made this lib.

```Erlang
1> php_serializer:unserialize(<<"a:1:{s:3:\"foo\";a:2:{i:0;i:242;i:1;N;}}">>).
[{<<"foo">>, [{0, 242}, {1, null}]}]
2> php_serializer:serialize([{<<"foo">>, [{0, 242}, {1, null}]}]).
<"a:1:{s:3:\"foo\";a:2:{i:0;i:242;i:1;N;}}">>
```

This library is inspired by Richard Jones take on unserializing, but I needed a serialize option.  
Read more here: http://www.metabrew.com/article/reading-serialized-php-objects-from-erlang

##Todo
* Support PHP objects ```<<"O:">>```.

## Disclaimer
Please, do not store php serialized data in database. If there is a need for storing serialized data
use json or something similar that is more widely supported.
