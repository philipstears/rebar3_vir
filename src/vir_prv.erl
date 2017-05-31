-module(vir_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, vir).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar vir..."},
            {opts, [ {rerelease, $r, "rerelease", undefined, "Re-run the release and don't bump versions"},
                     {dirty, $d, "dirty", undefined, "Do a dirty release, no cleaning up"},
                     {zip, $z, "zip", undefined, "Use zip rather than tar for building packages"},
                     {version, $v, "version", undefined, "Specify  the version number to use"},
                     {label, $l, "label", undefined, "Specify the build label to use"}
                   ]},
            {short_desc, "Drop in replacement for vir"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  State1 = update_materials(State),
  State2 = do_releases(State1),
  State3 = build_packages(State2),
  {ok, State3}.

update_materials(State) ->
  MajorVer = read_version("major_ver"),
  MinorVer = read_version("minor_ver"),
  BuildNo = read_version("build_no"),
  NewBuildNo = BuildNo + 1,
  write_version("build_no", NewBuildNo),
  write_version("major_ver", MajorVer),
  write_version("minor_ver", MinorVer),
  BranchName = strip(os:cmd("git rev-parse --abbrev-ref HEAD")),
  BuildLabel = list_to_binary(io_lib:format("~B.~B.~B-~s", [ MajorVer, MinorVer, NewBuildNo, binary_to_list(BranchName)])),
  write_shared_hrl(BuildLabel),
  State1 = rebar_state:set(State, build_label, BuildLabel),
  State1.


read_version(File) ->
  case file:read_file(filename:join("deployment", File)) of
    { ok, Data } ->
      Clean = strip(Data),
      binary_to_integer(Clean);
    _ -> 0
  end.

write_version(File, Number) ->
  ok = filelib:ensure_dir("deployment/foo.txt"),
  ok = file:write_file(filename:join("deployment", File), << (integer_to_binary(Number))/binary, "\n" >>),
  ok.

strip(Data) ->
  re:replace(Data, "\\s+", "", [global,{return,binary}]).

write_shared_hrl(Label) ->
  case filelib:is_file("apps/shared/include/version.hrl") of
    true ->
      file:write_file("apps/shared/include/version.hrl", <<"-define(VERSION, <<\"", Label/binary, "\">>).">>);
    _ ->
      ok
  end.

do_releases(State) ->
  os:cmd("rm -rf _build/default/rel"),
  case rebar_state:get(State, relx, []) of
    [] ->
      io:format("No relx stuff configured - proceeding with default release strategy"),
      rebar_relx:do(vir_prv, "release", ?PROVIDER, State);
    Releases ->
      lists:foreach(fun (Release) ->
                        case element(2, Release) of
                          { Name, _V } ->
                            io:format("Doing release ~p ~n", [ Name ]),
                            rebar_relx:do(vir_prv, "release -n " ++ atom_to_list(Name), ?PROVIDER, State);
                          _ ->
                            ok
                        end
                    end, Releases)
  end,
  State.

build_packages(State) ->
  { ok, Releases } = file:list_dir("_build/default/rel"),
  BuildLabel = rebar_state:get(State, build_label),
  ok = filelib:ensure_dir("releases/foo.txt"),
  lists:foreach(fun (Release) ->
                    io:format("Packaging release ~p ~p~n", [ Release, BuildLabel ]),
                    Fullpath = filename:join("_build/default/rel/", Release),
                    Result = os:cmd(string:join(["_build/default/plugins/rebar3_vir/priv/build_tar.sh", Fullpath, filename:absname("releases"), binary_to_list(BuildLabel)], " ")),

                    io:format("Tar result: ~p~n", [  Result ])
                end, Releases),
  State.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).



%%has_release(Name, []) ->
%%  false;
%%has_release(Name, [ Release | Tail ]) ->
