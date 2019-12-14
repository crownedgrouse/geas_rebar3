# geas_rebar3

This is rebar3 plugin to use [geas](https://github.com/crownedgrouse/geas) .

Simply add in your global config file `~/.config/rebar3/rebar.config` :

```
{plugins, [
  {geas_rebar3, {git, "https://github.com/crownedgrouse/geas_rebar3.git", {branch, "master"}}}
]}.

```
or simply (using hex package [geas_rebar3](https://hex.pm/packages/geas_rebar3))

```
{plugins, [geas_rebar3]}.

```

then run 

```
rebar3 geas
```

Note : Windows users can set this in `%userprofile%\.config\rebar3\rebar.config`.

This avoid to add it in local `rebar.config` in all your projects.

## Branches

Using 'dev' branch of this project will also use the 'dev' branch of `geas` repository.

## Troubles 
Please open trouble ticket here only if it is related to plugin only.

Geas issue have to be opened [there](https://github.com/crownedgrouse/geas/issues).
