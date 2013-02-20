-author('yoshiyuki.kanno@stoic.co.jp').

-record(dcerl_cache_stats, {
    gets        = 0 :: non_neg_integer(),
    puts        = 0 :: non_neg_integer(),
    dels        = 0 :: non_neg_integer(),
    hits        = 0 :: non_neg_integer(),
    records     = 0 :: non_neg_integer(),
    cached_size = 0 :: non_neg_integer()
}).

%% For chunked read/write I/F
-record(dcerl_fd, {
    key                = <<>> :: binary(),
    datafile_path      = ""   :: string(),
    tmp_datafile_iodev        :: file:io_device(),
    has_error                 :: boolean()
}).

%% dcerl's inner state
-record(dcerl_state, {
    journaldir_path   = "" :: string(),
    journalfile_iodev      :: file:io_device(),
    datadir_path      = "" :: string(),
    max_cache_size    = 0  :: pos_integer(),
    cache_stats            :: #dcerl_cache_stats{},
    cache_entries          :: term() % NIF resource
}).

