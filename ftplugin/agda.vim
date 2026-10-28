setlocal formatoptions-=t
setlocal formatoptions+=croql

setlocal comments=sfl:{-,mb1:--,ex:-},:--
setlocal commentstring=--\ %s

setlocal iskeyword=@,!-~,^.,_,^\(,^\),^{,^},^\",^\',192-255

if exists("b:cornelis_ftplugin")
  finish
endif
let b:cornelis_ftplugin = 1

let s:cornelis_channel_running = 0
if exists('g:cornelis_channel')
  try
    let s:cornelis_channel_running = jobwait([g:cornelis_channel], 0)[0] == -1
  catch
  endtry
endif

if !s:cornelis_channel_running
  if exists("g:cornelis_use_global_binary")
    let g:cornelis_channel = rpcstart('cornelis', [])
    call remote#host#Register('cornelis', '*', g:cornelis_channel)
  else
    let g:cornelis_channel = nvimhs#start(expand('<sfile>:p:h:h'), 'cornelis', ['-v', 'DEBUG', '-l', '/tmp/cornelis.log'])
  endif
  let g:cornelis_attached_buffers = 0
endif

nnoremap <F5> :call nvimhs#compileAndRestart('cornelis')<CR>

runtime agda-input.vim
runtime agda-matchpairs.vim

call textobj#user#plugin('cornelis', {
\   'zbrackets': {
\     'pattern': ['⟨', '⟩'],
\     'select-i': 'az',
\   },
\   'zspacebrackets': {
\     'pattern': ['⟨ ', ' ⟩'],
\     'select-i': 'iz',
\   },
\   'ibrackets': {
\     'pattern': ['⦃', '⦄'],
\     'select-a': 'ai',
\     'select-i': 'ii',
\   },
\   'hole': {
\     'pattern': ['{!', '!}'],
\     'select-a': 'ah',
\     'select-i': 'ih',
\   },
\ })

function InternalCornelisNotifyEditWrapper(bytes, buf, changedtick, srow, scol, boff, orow, ocol, olen, nrow, ncol, nlen)
  " Swallow errors when the plugin hasn't loaded yet.
  try
    call InternalCornelisNotifyEdit(a:bytes, a:buf, a:changedtick, a:srow, a:scol, a:boff, a:orow, a:ocol, a:olen, a:nrow, a:ncol, a:nlen)
  catch /^Vim\%((\a\+)\)\=:E117:/ " Function doesn't exist
  endtry
endfunction

lua << EOF
local channel = vim.g.cornelis_channel
local attached = vim.api.nvim_buf_attach(0, false, {
  on_bytes = function(...)
    vim.api.nvim_call_function('InternalCornelisNotifyEditWrapper', {...})
  end,
  -- on_detach runs for ordinary :bdelete as well as :bwipeout. Schedule the
  -- RPC call because buffer callbacks execute in a fast-event context.
  on_detach = function(_, buffer)
    vim.schedule(function()
      pcall(vim.api.nvim_buf_del_var, buffer, 'cornelis_ftplugin')
      pcall(vim.rpcrequest, channel, 'InternalCornelisDetach:function', {buffer})

      if vim.g.cornelis_channel == channel then
        local remaining = math.max((vim.g.cornelis_attached_buffers or 1) - 1, 0)
        vim.g.cornelis_attached_buffers = remaining
        if remaining == 0 then
          vim.g.cornelis_channel = nil
          vim.g.cornelis_attached_buffers = nil
          vim.fn.jobstop(channel)
        end
      end
    end)
  end,
})
if attached then
  vim.g.cornelis_attached_buffers = (vim.g.cornelis_attached_buffers or 0) + 1
end
EOF
