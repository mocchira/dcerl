Dcerl
=======

Dcerl is a Porting library of [DiskLruCache](https://github.com/JakeWharton/DiskLruCache) for Erlalng.
Main goals of this product are described below.

 * `SSD-Aware`: Enable to specify separate paths for Data files(on SSD) and the journal file(on HDD).

 * `S3 Compatible`: Enable a key to use a sequence of Unicode characters(`UTF8`), Max Length are limited in `1024byte`.

 * `Concurrency`: Enable to execute by a Erlang process.

 * `Reduce complexities`: Get rid of rarely used functions like multi values per a key.

 * `Expand Usecases`: Provide chunked(split) data interfaces to handle very large files.

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

```Erlang

    %% on Erlang REPL
    {ok, DS} = dcerl_api:start(DataDir, JournalDir, MaxSize, MaxChunkSize),
  
    {ok, DS2} = dcerl_api:put(DS, BinKey, BinBody),   %% normal put

    {ok, DS3} = dcerl_api:remove(DS2, BinKey),        %% remove

    {ok, DS4, FD} = dcerl_api:put_begin(DS3, BinKey), %% chunked put
    ok  = dcerl_api:put_chunk(DS4, FD, Chunk1),
    ok  = dcerl_api:put_chunk(DS4, FD, Chunk2),
    ok  = dcerl_api:put_chunk(DS4, FD, Chunk3),
    {ok, DS5} = dcerl_api:put_end(DS4, FD, true),     %% commit
    %%{ok, DS5} = dcerl_api:put_end(DS4, FD, false),  %% abort 
  
    case dcerl:get(DS5, BinKey) of
        {ok, DS6, #dcerl_fd{tmp_datafile_iodev = IoDev} = FD2} ->
            %% chunked get
            %% got the wrapped FD record
            %% call `get_chunk` until the last of returned parameter become `true`
            void;
        {ok, DS6, Bin} ->
            %% normal get
            %% got the whole Data 
            void;
        _ -> void
    end,
  
    dcerl_api:delete(DS6), %% delete all of cache files AND a journal file

```
