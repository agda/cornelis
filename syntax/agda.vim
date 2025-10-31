if !exists('main_syntax')
  if exists('b:current_syntax')
    finish
  endif
  let main_syntax = 'agda'
elseif exists('b:current_syntax') && b:current_syntax == 'agda'
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" The following sets up syntax highlighting by linking
" plugin highlight groups to default highlight groups.

" Highlight of a hole.
hi def link CornelisHole                 Todo

" Highlight in pop-up window
hi def link CornelisTitle Title

" Highlight for error messages and warnings
hi def link CornelisError                DiagnosticError
hi def link CornelisErrorWarning         CornelisError
hi def link CornelisWarn                 DiagnosticWarn
hi def link CornelisUnsolvedMeta         CornelisWarn
hi def link CornelisUnsolvedConstraint   CornelisWarn
hi def link CornelisMissingDefinition    CornelisWarn
hi def link CornelisTypeChecks           CornelisWarn

hi def link CornelisKeyword              Keyword
hi def link CornelisSymbol               Normal

hi def link CornelisType                 Type
hi def link CornelisRecord               CornelisType

hi def link CornelisModule               Constant

hi def link CornelisFunction             Function
hi def link CornelisMacro                Macro
hi def link CornelisOperator             Operator

" Different kind of identifiers
hi def link CornelisName                 Identifier
hi def link CornelisArgument             CornelisName
hi def link CornelisBound                CornelisName
hi def link CornelisGeneralizable        CornelisName
hi def link CornelisField                CornelisName

" Inductive and coinductive constructors
hi def link CornelisConstructor           Constant
hi def link CornelisInductiveConstructor  CornelisConstructor
hi def link CornelisConductiveConstructor CornelisConstructor

" Primitives
hi def link CornelisPragma               PreProc
hi def link CornelisPostulate            Define
hi def link CornelisPrimitive            Special
hi def link CornelisPrimitiveType        CornelisPrimitive

" Constants
hi def link CornelisNumber               Number
hi def link CornelisComment              Comment
hi def link CornelisString               String

hi def link CornelisCatchAllClause       Folded

let b:current_syntax = 'agda'
if main_syntax == 'agda'
  unlet main_syntax
endif
let &cpo = s:cpo_save
unlet s:cpo_save
