Dcerl
=======

Dcerl is a Porting library of [DiskLruCache](https://github.com/JakeWharton/DiskLruCache) for erlalng.
Main goals of this product are described below.

 * `SSD-Aware`: Enable to specify separate paths for Data files(on SSD) and the journal file(on HDD).

 * `S3 Compatible`: Enable a key to use a sequence of Unicode characters(`UTF8`), Max Length are limited in `1024byte`.

 * `Concurrency`: Enable to execute multi application instances.

 * `Reduce complexities`: Get rid of rarely used functions like multi values per a key.

 * `Improve maintainability`: Reduce the number of files per a directory(also obsolete OS file systems friendly).

 * `Improve CPU AND Memory efficiency`: Replace the journal file representation `text` with `binary`.

 * `Expand Usecases`: Provide chunked(split) data interfaces to handle `chunked|multipart HTTP bodies`.

Dependencies
=======

    Erlang >= R14B

Build
========
```shell

    make

```

Usage
========
```shell

    erl -pa ebin

```

```erlang

    %% on erlang REPL
    %% -spec(start(string(), string(), integer()) -> ref()).
    Descriptor = dcerl:start(DataDir, JournalDir, MaxSize),
  
    %% -spec(put(ref(), binary(), binary()) -> boolean()).
    dcerl:put(Descriptor, Key, BinBody),
    %% -spec(put(ref(), binary(), string()) -> boolean()).
    dcerl:put(Descriptor, Key, UploadedFilePath),
    %% -spec(put4chunked(ref(), binary(), binary(), boolean()) -> boolean()).
    dcerl:put4chunked(Descriptor, Key, BinChunkedBody, Tail),

    %% -spec(remove(ref(), binary()) -> boolean()).
    dcerl:remove(Descriptor, Key),
  
    %% -spec(get(ref(), binary()) -> binary()).
    RespBody = dcerl:get(Descriptor, Key),
    %% -spec(getpath(ref(), binary()) -> string()).
    CachedPath = dcerl:getpath(Descriptor, Key),
    %% -spec(get4chunked(ref(), binary()) -> {binary(), boolean()}).
    {ChunkedBody, Tail} = dcerl:get4chunked(Descriptor, Key),
  
    %% -spec((ref()) -> boolean()).
    dcerl:flush(Descriptor),
    %% -spec(delete(ref()) -> boolean()).
    dcerl:delete(Descriptor),

```
