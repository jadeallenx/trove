-record(trove_entry, { key, value, inserted_at, last_lookup_at }).

-define(LOG(Sev, Msg, Vars), lager:Sev(Msg, Vars)).
