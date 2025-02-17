# cornelis

![Cornelis in Action](https://raw.githubusercontent.com/isovector/cornelis/master/cast.gif)


## Dedication

> I'll ask to stand up \
> With a show about a rooster, \
> Which was old and worn out, \
> Impotent and weathered. \
> The chickens complained and whined \
> Because he did not satisfy them.
>
> -- [Cornelis Vreeswijk](https://www.youtube.com/watch?v=oKUscEWPVAM)


## Overview

`cornelis` is [agda-mode], but for neovim. It's written in Haskell, which means
it's maintainable and significantly less likely to bit-rot like any
vimscript/lua implementations.

[agda-mode]: https://agda.readthedocs.io/en/latest/tools/emacs-mode.html

## Features

It supports highlighting, goal listing, type-context, refinement, auto, solving,
case splitting, go-to definition, normalization, and helper functions. These are
exposed via vim commands.  Most commands have an equivalent in [agda-mode].

### Global commands

| Vim command | Description | Equivalent agda-mode keybinding |
| :--- | :--- | :--- |
| `:CornelisLoad`             | Load and type-check buffer | <kbd>C-c</kbd><kbd>C-l</kbd> |
| `:CornelisGoals`            | Show all goals        | <kbd>C-c</kbd><kbd>C-?</kbd> |
| `:CornelisRestart`          | Kill and restart the `agda` process | <kbd>C-c</kbd><kbd>C-x</kbd><kbd>C-r</kbd> |
| `:CornelisAbort`            | Abort running command | <kbd>C-c</kbd><kbd>C-x</kbd><kbd>C-a</kbd> |
| `:CornelisSolve <RW>`       | Solve constraints     | <kbd>C-c</kbd><kbd>C-s</kbd> |
| `:CornelisGoToDefinition`   | Jump to definition of name at cursor | <kbd>M-.</kbd> or middle mouse button |
| `:CornelisPrevGoal`         | Jump to previous goal | <kbd>C-c</kbd><kbd>C-b</kbd> |
| `:CornelisNextGoal`         | Jump to next goal     | <kbd>C-c</kbd><kbd>C-f</kbd> |
| `:CornelisQuestionToMeta`   | Expand `?`-holes to `{! !}` | _(none)_ |
| `:CornelisInc`              | Like `<C-A>` but also targets sub- and superscripts | _(none)_ |
| `:CornelisDec`              | Like `<C-X>` but also targets sub- and superscripts | _(none)_ |
| `:CornelisCloseInfoWindows` | Close (all) info windows cornelis has opened | _(none)_ |

### Commands in context of a goal

These commands can be used in context of a hole:

| Vim command | Description | Equivalent agda-mode keybinding |
| :--- | :--- | :--- |
| `:CornelisGive`                  | Fill goal with hole contents | <kbd>C-c</kbd><kbd>C-SPC</kbd> |
| `:CornelisRefine`                | Refine goal                  | <kbd>C-c</kbd><kbd>C-r</kbd> |
| `:CornelisElaborate <RW>`        | Fill goal with normalized hole contents | <kbd>C-c</kbd><kbd>C-m</kbd> |
| `:CornelisAuto`                  | [Automatic proof search]   | <kbd>C-c</kbd><kbd>C-a</kbd> |
| `:CornelisMakeCase`              | Case split                 | <kbd>C-c</kbd><kbd>C-c</kbd> |
| `:CornelisTypeContext <RW>`      | Show goal type and context | <kbd>C-c</kbd><kbd>C-,</kbd> |
| `:CornelisTypeInfer <RW>`        | Show inferred type of hole contents | <kbd>C-c</kbd><kbd>C-d</kbd> |
| `:CornelisTypeContextInfer <RW>` | Show goal type, context, and inferred type of hole contents | <kbd>C-c</kbd><kbd>C-.</kbd> |
| `:CornelisNormalize <CM>`        | Compute normal of hole contents | <kbd>C-c</kbd><kbd>C-n</kbd> |
| `:CornelisWhyInScope`            | Show why given name is in scope | <kbd>C-c</kbd><kbd>C-w</kbd> |
| `:CornelisHelperFunc <RW>`       | Copy inferred type to register `"` | <kbd>C-c</kbd><kbd>C-h</kbd> |

[Automatic proof search]: https://agda.readthedocs.io/en/latest/tools/auto.html#auto

Commands with an `<RW>` argument take an optional normalization mode argument,
one of `AsIs`, `Instantiated`, `HeadNormal`, `Simplified` or `Normalised`. When
omitted, defaults to `Normalised`.
This default may be specified in vimrc as `g:cornelis_rewrite_mode`.

Commands with a `<CM>` argument take an optional compute mode argument:

| `<CM>` | Description | Equivalent agda-mode prefix |
| :---   | :---        | :---             |
| `DefaultCompute`  | default, used if `<CM>` is omitted                 | no prefix, default |
| `IgnoreAbstract`  | compute normal form, ignoring `abstract`s          | <kbd>C-u</kbd> |
| `UseShowInstance` | compute normal form of print using `show` instance | <kbd>C-u</kbd><kbd>C-u</kbd> |
| `HeadCompute`     | compute weak-head normal form                      | <kbd>C-u</kbd><kbd>C-u</kbd><kbd>C-u</kbd> |

If Agda is stuck executing a command (e.g. if normalization takes too long),
abort the command with `:CornelisAbort`.

If you need to restart the plugin (eg if Agda is stuck in a loop), you can
restart everything via `:CornelisRestart`.

### Agda Input

There is reasonably good support for agda-input via your `<LocalLeader>` in
insert mode. See
[agda-input.vim](https://github.com/isovector/cornelis/blob/master/agda-input.vim)
for available bindings.

If you'd like to use a prefix other than your `<LocalLeader>`, add the following
to your `.vimrc`:

```viml
let g:cornelis_agda_prefix = "<Tab>" " Replace with your desired prefix
```


#### Interactive Unicode Selection

If you'd like an interactive prompt for choosing unicode characters,
additionally install `vim-which-key`:

```viml
Plug 'liuchengxu/vim-which-key'
```

and map a call to `cornelis#prompt_input()` in insert mode:

```viml
inoremap <localleader> <C-O>:call cornelis#prompt_input()<CR>
```


#### Disabling Default Bindings

If you don't want any of the default bindings, add the following to your `.vimrc`:

```viml
let g:cornelis_no_agda_input = 1
```


#### Adding Bindings

Custom bindings can be added by calling the `cornelis#bind_input` function in
`.vimrc`. For example:

```viml
call cornelis#bind_input("nat", "ℕ")
```

will add `<LocalLeader>nat` as an input remapping for `ℕ`.


#### Custom Hooks

If you'd prefer to manage agda-input entirely on your own (perhaps in a snippet
system), you can set the following:

```viml
function! MyCustomHook(key, character)
  " do something
endfunction

let g:cornelis_bind_input_hook = "MyCustomHook"
```

You can invoke `cornelis#standard_bind_input` with the same arguments if you'd
like to run your hook in addition to the standard one.


### Text Objects

Use the `iz`/`az` text objects to operate on text between `⟨` and `⟩`. Somewhat
surprisingly for i/a text objects, `iz` targets the _spaces_ between these
brackets, and `az` targets the spaces. Neither textobj targets the brackets
themselves.

Also `ii`/`ai` will operate on `⦃` and `⦄`, but in the way you'd expect
text objects to behave.

`ih`/`ah` will operate on `{!` and `!}`.



## Installation

Make sure you have [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/) on your PATH!

```viml
Plug 'kana/vim-textobj-user'
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'isovector/cornelis', { 'do': 'stack build' }
```


### Agda Version

`cornelis` is tested only against `agda-2.6.3`. If you run into weird error
messages from vim, it's probably because you're running an old version of
`agda`. If possible, try upgrading, if not, file a bug and I'll see if I can
help.

In addition, there are some bugs in the most recent version of `agda` that
negatively affect `cornelis`. For best results, build from head, ensuring you
have the following patches:

- https://github.com/agda/agda/pull/5752
- https://github.com/agda/agda/pull/5776


### Installation with Nix

You can install both the vim plugin and the cornelis binary using nix flakes!
You can access the binary as `cornelis.packages.<my-system>.cornelis` and the
vim plugin as `cornelis.packages.<my-system>.cornelis-vim`. Below is a sample
configuration to help you understand where everything plugs in.

<details>
<summary>Nix details</summary>

```nix
# flake.nix
{
  description = "my-config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cornelis.url = "github:isovector/cornelis";
    cornelis.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { home-manager
    , nixpkgs
    , cornelis
    , ...
    }: {
    nixosConfigurations = {
      bellerophon = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          {
            nixpkgs.overlays = [cornelis.overlays.cornelis];
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.my-home = import ./my-home.nix;
          }
        ];
      };
    };
  };
}

# my-home.nix
{pkgs, ...}:
{
  home.packages = [ pkgs.agda ];
  programs.neovim = {
    enable = true;
    extraConfig = builtins.readFile ./init.vim;
    plugins = [
      {
        # plugin packages in required Vim plugin dependencies
        plugin = pkgs.vimPlugins.cornelis;
        config = "let g:cornelis_use_global_binary = 1";
      }
    ];
    extraPackages = [ pkgs.cornelis ];
  };
}
```
</details>

Make sure you enable the global binary option in your vim config. Since
`/nix/store` is immutable cornelis will fail when `nvim-hs` tries to run stack,
which it will do if the global binary option isn't enabled.


#### Use global binary instead of stack

Vimscript:

```viml
let g:cornelis_use_global_binary = 1
```

Lua:

```lua
vim.g.cornelis_use_global_binary = 1
```


## Example Configuration

Once you have `cornelis` installed, you'll probably want to add some keybindings
for it! This is enough to get you started:

```viml
au BufRead,BufNewFile *.agda call AgdaFiletype()
au QuitPre *.agda :CornelisCloseInfoWindows
function! AgdaFiletype()
    nnoremap <buffer> <leader>l :CornelisLoad<CR>
    nnoremap <buffer> <leader>r :CornelisRefine<CR>
    nnoremap <buffer> <leader>d :CornelisMakeCase<CR>
    nnoremap <buffer> <leader>, :CornelisTypeContext<CR>
    nnoremap <buffer> <leader>. :CornelisTypeContextInfer<CR>
    nnoremap <buffer> <leader>n :CornelisSolve<CR>
    nnoremap <buffer> <leader>a :CornelisAuto<CR>
    nnoremap <buffer> gd        :CornelisGoToDefinition<CR>
    nnoremap <buffer> [/        :CornelisPrevGoal<CR>
    nnoremap <buffer> ]/        :CornelisNextGoal<CR>
    nnoremap <buffer> <C-A>     :CornelisInc<CR>
    nnoremap <buffer> <C-X>     :CornelisDec<CR>
endfunction
```

Feeling spicy? Automatically run `CornelisLoad` every time you save the file.

```viml
au BufWritePost *.agda execute "normal! :CornelisLoad\<CR>"
```

If you'd like to automatically load files when you open them too, try this:

```viml
function! CornelisLoadWrapper()
  if exists(":CornelisLoad") ==# 2
    CornelisLoad
  endif
endfunction

au BufReadPre *.agda call CornelisLoadWrapper()
au BufReadPre *.lagda* call CornelisLoadWrapper()
```

This won't work on the first Agda file you open due to a bug, but it will
successfully load subsequent files.


### Configuring Cornelis' Behavior

The max height and width of the info window can be set via:

```viml
let g:cornelis_max_size = 30
```

and

```viml
let g:cornelis_max_width = 40
```

If you'd prefer your info window to appear somewhere else, you can set
`g:cornelis_split_location` (previously `g:cornelis_split_direction`), e.g.

```viml
let g:cornelis_split_location = 'vertical'
```

The following configuration options are available:

- `horizontal`: The default, opens in a horizontal split respecting `splitbelow`.
- `vertical`: Opens in a vertical split respecting `splitright`.
- `top`: Opens at the top of the window.
- `bottom`: Opens at the bottom of the window.
- `left`: Opens at the left of the window.
- `right`: Opens at the right of the window.

Set default rewrite mode to use in commands which take an optional
normalization mode argument

```viml
let g:cornelis_rewrite_mode = 'HeadNormal'
```

The following configuration options are available:
- `AsIs`
- `Instantiated`
- `HeadNormal`
- `Simplified`
- `Normalised`

### Aligning Reasoning Justification

If you're interested in automatically aligning your reasoning justifications,
also install the following plugin:

```viml
Plug 'junegunn/vim-easy-align'
```

and add the following configuration for it:

```viml
vmap <leader><space> <Plug>(EasyAlign)

let g:easy_align_delimiters = {
\ 'r': { 'pattern': '[≤≡≈∎]', 'left_margin': 2, 'right_margin': 0 },
\ }
```

You can now align justifications by visually selecting the proof, and then
typing `<leader><space>r`.


### Customizing Syntax Highlighting

Syntax highlighting is controlled by syntax groups named `Cornelis*`,
defined in [`syntax/agda.vim`](./syntax/agda.vim).
These groups are linked to default highlighting groups
([`:h group-name`](https://neovim.io/doc/user/syntax.html#group-name)),
and can be customized by overriding them in user configuration.

```viml
" Highlight holes with a yellow undercurl/underline:
highlight CornelisHole ctermfg=yellow ctermbg=NONE cterm=undercurl

" Highlight "generalizables" (declarations in `variable` blocks) like constants:
highlight link CornelisGeneralizable Constant
```


## Contributing

I'm a noob at Agda, and I don't know what I don't know. If this plugin doesn't
have some necessary feature for you to get work done, please file a bug,
including both what's missing, and how you use it in your workflow. I'd love to
learn how to use Agda better! I can move quickly on feature requests.

If you'd like to get involved, feel free to tackle an issue on the tracker and
send a PR. I'd love to have you on board!

## Architecture

Cornelis spins up a new `BufferStuff` for each Agda buffer it encounters.
`BufferStuff` contains a handle to a unique `agda` instance, which can be used
to send commands. It also tracks things like the information window buffer,
in-scope goals, and whatever the last `DisplayInfo` response from `agda` was.

For each `BufferStuff`, we also spin up a new thread, blocking on responses
from `agda`. These responses all get redirected to a global worker thread, which
is responsible for dispatching on each command. Commands are typesafe, parsed
from JSON, and associated with the buffer they came from.
